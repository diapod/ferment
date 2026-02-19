(ns

    ^{:doc    "User authentication helpers backed by Ferment auth configuration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.user

  (:require [clojure.string :as str]
            [next.jdbc :as jdbc]
            [tick.core :as t]
            [ferment.auth :as auth]
            [ferment.auth.locking :as locking]
            [ferment.db :as db]
            [ferment.user :as user])

  (:import (ferment AuthConfig)))

(def ^:private login-select-prefix
  (str "SELECT users.id AS id, users.email AS email, users.account_type AS account_type,"
       " users.locked AS locked, users.soft_locked AS soft_locked,"
       " users.password AS intrinsic, password_suites.suite AS shared"
       " FROM users JOIN password_suites ON password_suites.id = users.password_suite_id"
       " WHERE users.email = ?"))

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- account-type-k
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v trim-s keyword)
    :else nil))

(defn- wait-no-user!
  [auth-config]
  (when (instance? AuthConfig auth-config)
    (when-some [wait-fn (some-> ^AuthConfig auth-config :passwords :wait)]
      (try
        (wait-fn false)
        (catch Throwable _ nil)))))

(defn- login-row->data
  [row]
  (when (map? row)
    {:user/id                  (:id row)
     :user/email               (:email row)
     :user/account-type        (some-> (:account_type row) trim-s keyword)
     :user/login-attempts      (:login_attempts row)
     :auth/locked-at           (:locked row)
     :auth/soft-locked-at      (:soft_locked row)
     :auth/locked?             (boolean (or (:locked row) (:soft_locked row)))
     :auth/password-intrinsic  (:intrinsic row)
     :auth/password-shared     (:shared row)}))

(defn- lock-state
  [login-data auth-config now]
  (let [hard?      (locking/hard-locked? login-data)
        soft?      (locking/soft-locked? login-data auth-config now)
        legacy?    (true? (:auth/locked? login-data))
        locked?    (or hard? soft? legacy?)
        soft-left  (when soft?
                     (locking/soft-lock-remains login-data auth-config now))]
    {:locked? locked?
     :hard? hard?
     :soft? soft?
     :soft/remaining soft-left}))

(defn- resolve-db-source
  [auth-source auth-config account-type]
  (letfn [(try-db
            ([src]
             (when (some? src)
               (try
                 (auth/db src)
                 (catch Throwable _ nil))))
            ([src at]
             (when (and (some? src) (some? at))
               (try
                 (auth/db src at)
                 (catch Throwable _ nil)))))]
    (or (try-db auth-source account-type)
        (try-db auth-source)
        (try-db auth-config))))

(defn get-login-data
  "Returns login row for `email` and optional `account-type`.

  Accepts auth source compatible with `ferment.auth/config` and `ferment.auth/db`
  (`AuthSettings`, `AuthConfig`, or `DataSource`)."
  ([auth-source email]
   (get-login-data auth-source email nil))
  ([auth-source email account-type]
   (when-some [email' (trim-s email)]
     (let [account-type' (account-type-k account-type)
           query (if account-type'
                   (str login-select-prefix " AND users.account_type = ?")
                   login-select-prefix)
           params (cond-> [query email']
                    account-type' (conj (name account-type')))
           db-src (or (auth/db auth-source account-type')
                      (auth/db auth-source))]
       (when db-src
         (some-> (jdbc/execute-one! db-src params db/opts-simple-map)
                 login-row->data))))))

(defn authenticate-password
  "Checks plain password against credentials loaded from DB through auth config.

  Returns:
  - success: `{:ok? true :user {...}}`
  - failure: `{:ok? false :error <keyword>}`"
  ([auth-source email password]
   (authenticate-password auth-source email password nil))
  ([auth-source email password account-type]
   (let [email'       (trim-s email)
         password'    (trim-s password)
         account-type' (account-type-k account-type)
         auth-config  (or (auth/config auth-source account-type')
                          (auth/config auth-source))
         now          (t/now)]
     (cond
       (or (nil? email') (nil? password'))
       {:ok? false
        :error :input/invalid}

       (nil? auth-config)
       {:ok? false
        :error :auth/not-configured}

       :else
       (if-some [{password-shared    :auth/password-shared
                  password-intrinsic :auth/password-intrinsic
                  user-id            :user/id
                  :as                login-data}
                 (get-login-data auth-source email' account-type')]
         (let [db-src       (resolve-db-source auth-source auth-config account-type')
               max-attempts (or (some-> auth-config :locking :max-attempts)
                                3)
               {:keys [locked? hard? soft? soft/remaining]}
               (lock-state login-data auth-config now)]
           (if locked?
             (cond-> {:ok? false
                      :error :auth/account-locked
                      :auth/lock-kind (cond hard? :hard
                                            soft? :soft
                                            :else :unknown)}
               (some? remaining) (assoc :auth/lock-remaining remaining))
             (if (true? (auth/check-password-json password'
                                                  password-shared
                                                  password-intrinsic
                                                  auth-config))
               (do
                 (when (and db-src user-id)
                   (user/update-login-ok! db-src user-id))
                 {:ok? true
                  :user (dissoc login-data
                                :auth/password-shared
                                :auth/password-intrinsic
                                :auth/locked?
                                :auth/locked-at
                                :auth/soft-locked-at)})
               (do
                 (when (and db-src user-id)
                   (user/update-login-failed! db-src user-id max-attempts))
                 {:ok? false
                  :error :auth/invalid-credentials}))))
         (do
           (wait-no-user! auth-config)
           {:ok? false
            :error :auth/invalid-credentials}))))))
