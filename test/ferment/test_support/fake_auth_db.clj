(ns ferment.test-support.fake-auth-db
  "Shared fake JDBC backend for user/auth tests."
  (:require [tick.core :as t]))

(defn make-db
  []
  {:users* (atom {})
   :suites* (atom {})
   :suites-id* (atom {})
   :last-id* (atom 0)})

(defn- next-id!
  [db]
  (swap! (:last-id* db) inc))

(def ^:private select-last-id-sql
  "SELECT LAST_INSERT_ID() AS id")

(def ^:private select-user-by-email-sql
  "SELECT id, email, account_type, password_suite_id, password, login_attempts, soft_locked, locked FROM users WHERE email = ? LIMIT 1")

(def ^:private select-user-by-id-sql
  "SELECT id, email, account_type, password_suite_id, password, login_attempts, soft_locked, locked FROM users WHERE id = ? LIMIT 1")

(def ^:private select-id-by-email-sql
  "SELECT id FROM users WHERE email = ? LIMIT 1")

(def ^:private login-select-sql
  "SELECT users.id AS id, users.email AS email, users.account_type AS account_type, users.login_attempts AS login_attempts, users.locked AS locked, users.soft_locked AS soft_locked, users.password AS intrinsic, password_suites.suite AS shared FROM users JOIN password_suites ON password_suites.id = users.password_suite_id WHERE users.email = ?")

(def ^:private login-select-by-account-sql
  (str login-select-sql " AND users.account_type = ?"))

(def ^:private insert-suite-sql
  "INSERT INTO password_suites (suite) VALUES (?) ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)")

(def ^:private insert-user-sql
  "INSERT INTO users (email, account_type, password_suite_id, password, login_attempts, soft_locked, locked) VALUES (?, ?, ?, ?, 0, NULL, NULL)")

(def ^:private update-password-sql
  "UPDATE users SET password_suite_id = ?,     password = ?,     login_attempts = 0,     soft_locked = NULL,     locked = NULL WHERE id = ?")

(def ^:private delete-user-by-id-sql
  "DELETE FROM users WHERE id = ?")

(def ^:private update-login-success-sql
  "UPDATE users SET login_attempts = 0,     soft_locked = NULL WHERE id = ?")

(def ^:private update-login-failed-sql
  "UPDATE users SET login_attempts = login_attempts + 1,     soft_locked = CASE       WHEN login_attempts + 1 >= ? THEN CURRENT_TIMESTAMP(6)       ELSE soft_locked     END WHERE id = ?")

(def ^:private unlock-user-by-id-sql
  "UPDATE users SET login_attempts = 0,     soft_locked = NULL,     locked = NULL WHERE id = ?")

(def ^:private soft-lock-user-by-id-sql
  "UPDATE users SET soft_locked = CURRENT_TIMESTAMP(6) WHERE id = ?")

(def ^:private hard-lock-user-by-id-sql
  "UPDATE users SET locked = CURRENT_TIMESTAMP(6) WHERE id = ?")

(defn- find-user-by-email
  [db email]
  (some (fn [[_ row]]
          (when (= email (:email row))
            row))
        @(:users* db)))

(defn- login-row
  [db row]
  (when (map? row)
    {:id (:id row)
     :email (:email row)
     :account_type (:account_type row)
     :login_attempts (:login_attempts row)
     :locked (:locked row)
     :soft_locked (:soft_locked row)
     :intrinsic (:password row)
     :shared (get @(:suites-id* db) (:password_suite_id row))}))

(defn execute-one!
  [db _connectable query & _opts]
  (let [sql (first query)]
    (cond
      (= sql select-last-id-sql)
      {:id @(:last-id* db)}

      (= sql select-user-by-email-sql)
      (find-user-by-email db (second query))

      (= sql select-user-by-id-sql)
      (get @(:users* db) (long (second query)))

      (= sql select-id-by-email-sql)
      (when-some [row (find-user-by-email db (second query))]
        [(:id row)])

      (= sql login-select-sql)
      (some-> (find-user-by-email db (second query))
              (login-row db))

      (= sql login-select-by-account-sql)
      (let [email (second query)
            account-type (nth query 2)]
        (some (fn [[_ row]]
                (when (and (= email (:email row))
                           (= account-type (:account_type row)))
                  (login-row db row)))
              @(:users* db)))

      :else
      (throw (ex-info "Unexpected SQL in fake execute-one!."
                      {:query query})))))

(defn execute!
  [db _connectable query & _opts]
  (let [sql (first query)]
    (cond
      (= sql insert-suite-sql)
      (let [suite (second query)
            existing-id (get @(:suites* db) suite)
            id (or existing-id (next-id! db))]
        (swap! (:suites* db) assoc suite id)
        (swap! (:suites-id* db) assoc id suite)
        (reset! (:last-id* db) id)
        [{:next.jdbc/update-count 1}])

      (= sql insert-user-sql)
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

      (= sql update-password-sql)
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

      (= sql delete-user-by-id-sql)
      (let [id (long (second query))]
        (if (contains? @(:users* db) id)
          (do
            (swap! (:users* db) dissoc id)
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      (= sql update-login-success-sql)
      (let [id (long (second query))]
        (if (contains? @(:users* db) id)
          (do
            (swap! (:users* db) update id assoc
                   :login_attempts 0
                   :soft_locked nil)
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      (= sql update-login-failed-sql)
      (let [max-attempts (long (second query))
            id (long (nth query 2))]
        (if-let [row (get @(:users* db) id)]
          (let [attempts' (inc (long (or (:login_attempts row) 0)))
                soft-lock' (if (>= attempts' max-attempts)
                             (t/now)
                             (:soft_locked row))]
            (swap! (:users* db) assoc id
                   (assoc row
                          :login_attempts attempts'
                          :soft_locked soft-lock'))
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      (= sql unlock-user-by-id-sql)
      (let [id (long (second query))]
        (if-let [row (get @(:users* db) id)]
          (do
            (swap! (:users* db) assoc id
                   (assoc row
                          :login_attempts 0
                          :soft_locked nil
                          :locked nil))
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      (= sql soft-lock-user-by-id-sql)
      (let [id (long (second query))]
        (if-let [row (get @(:users* db) id)]
          (do
            (swap! (:users* db) assoc id
                   (assoc row :soft_locked (t/now)))
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      (= sql hard-lock-user-by-id-sql)
      (let [id (long (second query))]
        (if-let [row (get @(:users* db) id)]
          (do
            (swap! (:users* db) assoc id
                   (assoc row :locked (t/now)))
            [{:next.jdbc/update-count 1}])
          [{:next.jdbc/update-count 0}]))

      :else
      (throw (ex-info "Unexpected SQL in fake execute!."
                      {:query query})))))
