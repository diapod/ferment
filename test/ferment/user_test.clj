(ns ferment.user-test
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [ferment.auth :as auth]
            [ferment.db :as db]
            [ferment.user :as user]))

(defn- make-fake-db
  []
  {:users* (atom {})
   :suites* (atom {})
   :last-id* (atom 0)})

(defn- next-id!
  [db]
  (swap! (:last-id* db) inc))

(defn- fake-execute-one!
  [db _connectable query & _opts]
  (let [sql (first query)]
    (cond
      (= sql "SELECT LAST_INSERT_ID() AS id")
      {:id @(:last-id* db)}

      (= sql "SELECT id, email, account_type, password_suite_id, password, login_attempts, soft_locked, locked FROM users WHERE email = ? LIMIT 1")
      (let [email (second query)]
        (some (fn [[_ row]]
                (when (= email (:email row))
                  row))
              @(:users* db)))

      (= sql "SELECT id, email, account_type, password_suite_id, password, login_attempts, soft_locked, locked FROM users WHERE id = ? LIMIT 1")
      (get @(:users* db) (long (second query)))

      (= sql "SELECT id FROM users WHERE email = ? LIMIT 1")
      (let [email (second query)]
        (some (fn [[id row]]
                (when (= email (:email row))
                  [id]))
              @(:users* db)))

      :else
      (throw (ex-info "Unexpected SQL in fake execute-one!."
                      {:query query})))))

(defn- fake-execute!
  [db _connectable query & _opts]
  (let [sql (first query)]
    (cond
      (= sql "INSERT INTO password_suites (suite) VALUES (?) ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)")
      (let [suite (second query)
            existing-id (get @(:suites* db) suite)
            id (or existing-id (next-id! db))]
        (swap! (:suites* db) assoc suite id)
        (reset! (:last-id* db) id)
        [{:next.jdbc/update-count 1}])

      (= sql "INSERT INTO users (email, account_type, password_suite_id, password, login_attempts, soft_locked, locked) VALUES (?, ?, ?, ?, 0, NULL, NULL)")
      (let [[_ email account-type suite-id password] query
            id (next-id! db)]
        (swap! (:users* db) assoc id {:id id
                                      :email email
                                      :account_type account-type
                                      :password_suite_id suite-id
                                      :password password
                                      :login_attempts 0
                                      :soft_locked nil
                                      :locked nil})
        (reset! (:last-id* db) id)
        [{:next.jdbc/update-count 1}])

      (= sql "UPDATE users SET password_suite_id = ?,     password = ?,     login_attempts = 0,     soft_locked = NULL,     locked = NULL WHERE id = ?")
      (let [[_ suite-id password id] query
            id (long id)]
        (if (contains? @(:users* db) id)
          (do
            (swap! (:users* db) update id assoc
                   :password_suite_id suite-id
                   :password password
                   :login_attempts 0
                   :soft_locked nil
                   :locked nil)
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      (= sql "DELETE FROM users WHERE id = ?")
      (let [id (long (second query))]
        (if (contains? @(:users* db) id)
          (do
            (swap! (:users* db) dissoc id)
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      :else
      (throw (ex-info "Unexpected SQL in fake execute!."
                      {:query query})))))

(deftest create-or-get-shared-suite-id-is-idempotent
  (let [fake (make-fake-db)]
    (with-redefs [jdbc/transact (fn [connectable f _opts] (f connectable))
                  jdbc/execute! (partial fake-execute! fake)
                  jdbc/execute-one! (partial fake-execute-one! fake)]
      (is (= 1 (user/create-or-get-shared-suite-id ::db "{\"suite\":1}")))
      (is (= 1 (user/create-or-get-shared-suite-id ::db "{\"suite\":1}")))
      (is (= 1 (count @(:suites* fake)))))))

(deftest create-change-and-delete-user-flow
  (let [fake (make-fake-db)
        password-calls (atom 0)]
    (with-redefs [jdbc/transact (fn [connectable f _opts] (f connectable))
                  jdbc/execute! (partial fake-execute! fake)
                  jdbc/execute-one! (partial fake-execute-one! fake)
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

(deftest users-coercions-are-registered
  (testing "coercions for :users work for key columns"
    (is (= "test@example.com"
           ((db/get-in-coercer :users/email) "Test@EXAMPLE.com")))
    (is (= :user
           ((db/get-out-coercer :users/account-type) "user")))))

(deftest id-lookup-uses-identity-cache
  (let [fake (make-fake-db)
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
                                      (apply fake-execute-one! fake connectable query opts))]
      (is (= 1 (user/id ::db "Cache@test.io")))
      (is (= 1 (user/id ::db "Cache@test.io")))
      (is (= 1 @db-calls)))))

(deftest ids-cache-aliases-props-cache
  (testing "Configured :ferment.user/ids-cache points to cache used by props helpers."
    (is (identical? (var-get #'ferment.user/ids-cache)
                    (var-get #'ferment.user/props-cache)))))
