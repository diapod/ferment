(ns ferment.user-test
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [ferment.auth :as auth]
            [ferment.db :as db]
            [ferment.test-support.fake-auth-db :as fake-db]
            [ferment.user :as user]))

(deftest create-or-get-shared-suite-id-is-idempotent
  (let [fake (fake-db/make-db)]
    (with-redefs [jdbc/transact (fn [connectable f _opts] (f connectable))
                  jdbc/execute! (partial fake-db/execute! fake)
                  jdbc/execute-one! (partial fake-db/execute-one! fake)]
      (is (= 1 (user/create-or-get-shared-suite-id ::db "{\"suite\":1}")))
      (is (= 1 (user/create-or-get-shared-suite-id ::db "{\"suite\":1}")))
      (is (= 1 (count @(:suites* fake)))))))

(deftest create-change-and-delete-user-flow
  (let [fake (fake-db/make-db)
        password-calls (atom 0)]
    (with-redefs [jdbc/transact (fn [connectable f _opts] (f connectable))
                  jdbc/execute! (partial fake-db/execute! fake)
                  jdbc/execute-one! (partial fake-db/execute-one! fake)
                  auth/config (fn
                                ([_] {:id :ferment.auth/simple
                                      :account-types {:default-name "user"}})
                                ([_ _] {:id :ferment.auth/simple
                                        :account-types {:default-name "user"}}))
                  auth/db (fn
                            ([_] ::db)
                            ([_ _] ::db))
                  auth/make-password-json (fn [plain _]
                                            (swap! password-calls inc)
                                            {:shared "{\"algo\":\"pbkdf2\"}"
                                             :intrinsic (str "{\"password\":\"" plain "\"}")})]
      (testing "create-user! creates a new user"
        (let [result (user/create-user! ::auth "TeSt@example.com" "sekret" :user)]
          (is (:ok? result))
          (is (:created? result))
          (is (= "test@example.com" (get-in result [:user :user/email])))))

      (testing "create-user! blocks duplicate email"
        (let [result (user/create-user! ::auth "test@example.com" "inne" :user)]
          (is (false? (:ok? result)))
          (is (= :user/already-exists (:error result)))))

      (testing "change-password! updates password for existing user"
        (let [result (user/change-password! ::auth "test@example.com" "nowe-haslo")]
          (is (:ok? result))
          (is (:updated? result))
          (is (= "test@example.com" (get-in result [:user :user/email])))))

      (testing "delete-user! removes user"
        (let [result (user/delete-user! ::auth "test@example.com")]
          (is (:ok? result))
          (is (:deleted? result))
          (is (= :user/not-found (:error (user/delete-user! ::auth "test@example.com"))))))

      (is (= 2 @password-calls)))))

(deftest lock-reset-and-unlock-user-flow
  (let [fake (fake-db/make-db)]
    (with-redefs [jdbc/transact (fn [connectable f _opts] (f connectable))
                  jdbc/execute! (partial fake-db/execute! fake)
                  jdbc/execute-one! (partial fake-db/execute-one! fake)
                  auth/config (fn
                                ([_] {:id :ferment.auth/simple
                                      :account-types {:default-name "user"}})
                                ([_ _] {:id :ferment.auth/simple
                                        :account-types {:default-name "user"}}))
                  auth/db (fn
                            ([_] ::db)
                            ([_ _] ::db))
                  auth/make-password-json (fn [_plain _]
                                            {:shared "{\"algo\":\"pbkdf2\"}"
                                             :intrinsic "{\"password\":\"x\"}"})]
      (let [created (user/create-user! ::auth "lock@example.com" "sekret" :user)
            user-id (get-in created [:user :user/id])]
        (testing "soft lock can be set and then reset"
          (let [locked (user/lock-user! ::auth user-id :soft)]
            (is (:ok? locked))
            (is (= :soft (:auth/lock-kind locked)))
            (is (some? (:soft_locked (user/get-user-by-id ::db user-id)))))
          (let [reseted (user/reset-login-attempts! ::auth user-id)
                row (user/get-user-by-id ::db user-id)]
            (is (:ok? reseted))
            (is (:updated? reseted))
            (is (= 0 (:login_attempts row)))
            (is (nil? (:soft_locked row)))))

        (testing "hard lock can be set and then unlocked"
          (let [locked (user/lock-user! ::auth user-id :hard)]
            (is (:ok? locked))
            (is (= :hard (:auth/lock-kind locked)))
            (is (some? (:locked (user/get-user-by-id ::db user-id)))))
          (let [unlocked (user/unlock-user! ::auth user-id)
                row (user/get-user-by-id ::db user-id)]
            (is (:ok? unlocked))
            (is (:updated? unlocked))
            (is (false? (:auth/locked? unlocked)))
            (is (= 0 (:login_attempts row)))
            (is (nil? (:soft_locked row)))
            (is (nil? (:locked row)))))

        (testing "invalid lock kind is rejected"
          (let [result (user/lock-user! ::auth user-id :invalid)]
            (is (false? (:ok? result)))
            (is (= :user/invalid-lock-kind (:error result)))))))))

(deftest users-coercions-are-registered
  (testing "coercions for :users work for key columns"
    (is (= "test@example.com"
           ((db/get-in-coercer :users/email) "Test@EXAMPLE.com")))
    (is (= :user
           ((db/get-out-coercer :users/account-type) "user")))))

(deftest id-lookup-uses-identity-cache
  (let [fake (fake-db/make-db)
        db-calls (atom 0)]
    (swap! (:users* fake) assoc 1 {:id 1
                                   :email "cache@test.io"
                                   :account_type "user"
                                   :password_suite_id 1
                                   :password "{}"
                                   :login_attempts 0
                                   :soft_locked nil
                                   :locked nil})
    (reset! (var-get #'ferment.user/identity-cache) nil)
    (with-redefs [jdbc/execute-one! (fn [connectable query & opts]
                                      (swap! db-calls inc)
                                      (apply fake-db/execute-one! fake connectable query opts))]
      (is (= 1 (user/id ::db "Cache@test.io")))
      (is (= 1 (user/id ::db "Cache@test.io")))
      (is (= 1 @db-calls)))))

(deftest ids-cache-aliases-props-cache
  (testing "Configured :ferment.user/ids-cache points to cache used by props helpers."
    (is (identical? (var-get #'ferment.user/ids-cache)
                    (var-get #'ferment.user/props-cache)))))
