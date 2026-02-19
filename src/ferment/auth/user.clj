(ns

    ^{:doc    "User authentication helpers backed by Ferment auth configuration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.user

  (:require [clojure.string :as str]
            [next.jdbc :as jdbc]
            [ferment.auth :as auth]
            [ferment.db :as db])

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
     :auth/locked?             (boolean (or (:locked row) (:soft_locked row)))
     :auth/password-intrinsic  (:intrinsic row)
     :auth/password-shared     (:shared row)}))

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
                          (auth/config auth-source))]
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
                  locked?            :auth/locked?
                  :as                login-data}
                 (get-login-data auth-source email' account-type')]
         (if locked?
           {:ok? false
            :error :auth/account-locked}
           (if (true? (auth/check-password-json password'
                                                password-shared
                                                password-intrinsic
                                                auth-config))
             {:ok? true
              :user (dissoc login-data
                            :auth/password-shared
                            :auth/password-intrinsic
                            :auth/locked?)}
             {:ok? false
              :error :auth/invalid-credentials}))
         (do
           (wait-no-user! auth-config)
           {:ok? false
            :error :auth/invalid-credentials}))))))
