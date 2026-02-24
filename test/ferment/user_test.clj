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

(deftest grant-list-and-revoke-role-flow
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
      (let [created (user/create-user! ::auth "roles@example.com" "sekret" :user)
            user-id (get-in created [:user :user/id])]
        (is (:ok? created))
        (testing "grant-role! rejects roles outside dictionary"
          (let [unknown (user/grant-role! ::auth user-id :role/not-in-dictionary)]
            (is (false? (:ok? unknown)))
            (is (= :user/unknown-role (:error unknown)))
            (is (= :role/not-in-dictionary (:role unknown)))))
        (testing "grant-role! is idempotent"
          (let [grant-1 (user/grant-role! ::auth user-id :role/admin)
                grant-2 (user/grant-role! ::auth user-id :role/admin)]
            (is (:ok? grant-1))
            (is (true? (:granted? grant-1)))
            (is (= :role/admin (:role grant-1)))
            (is (:ok? grant-2))
            (is (false? (:granted? grant-2)))
            (is (true? (:already? grant-2)))))
        (testing "list-roles! returns explicit assignments"
          (let [listed (user/list-roles! ::auth "roles@example.com")]
            (is (:ok? listed))
            (is (= #{:role/admin} (:roles listed)))))
        (testing "revoke-role! is idempotent"
          (let [revoke-1 (user/revoke-role! ::auth user-id :role/admin)
                revoke-2 (user/revoke-role! ::auth user-id :role/admin)]
            (is (:ok? revoke-1))
            (is (true? (:revoked? revoke-1)))
            (is (empty? (:roles revoke-1)))
            (is (:ok? revoke-2))
            (is (false? (:revoked? revoke-2)))
            (is (true? (:missing? revoke-2)))))))))

(deftest role-dictionary-management-flow
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
      (testing "list-known-roles! exposes seeded dictionary"
        (let [listed (user/list-known-roles! ::auth)]
          (is (:ok? listed))
          (is (seq (:roles listed)))
          (is (contains? (set (map :role (:roles listed))) :role/admin))))

      (testing "create-role! is idempotent"
        (let [create-1 (user/create-role! ::auth :role/researcher)
              create-2 (user/create-role! ::auth :role/researcher)]
          (is (:ok? create-1))
          (is (true? (:created? create-1)))
          (is (= :role/researcher (:role create-1)))
          (is (:ok? create-2))
          (is (false? (:created? create-2)))
          (is (true? (:already? create-2)))))

      (testing "delete-role! blocks removal of assigned role and allows removing unassigned role"
        (let [created (user/create-user! ::auth "dictionary@example.com" "sekret" :user)
              user-id (get-in created [:user :user/id])]
          (is (:ok? created))
          (is (:ok? (user/grant-role! ::auth user-id :role/researcher)))
          (let [in-use (user/delete-role! ::auth :role/researcher)]
            (is (false? (:ok? in-use)))
            (is (= :role/in-use (:error in-use)))
            (is (= 1 (:assignments in-use))))
          (is (:ok? (user/revoke-role! ::auth user-id :role/researcher)))
          (let [deleted (user/delete-role! ::auth :role/researcher)
                missing (user/delete-role! ::auth :role/researcher)]
            (is (:ok? deleted))
            (is (true? (:deleted? deleted)))
            (is (false? (:ok? missing)))
            (is (= :role/not-found (:error missing)))))))))

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

(deftest props-cache-var-is-available
  (testing "User props cache var is available for DB cache bootstrap."
    (is (instance? clojure.lang.Atom
                   (var-get #'ferment.user/props-cache)))))
