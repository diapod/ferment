(ns

    ^{:doc    "User authentication helpers backed by Ferment auth configuration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.user

  (:require [clojure.string :as str]
            [tick.core :as t]
            [ferment.auth :as auth]
            [ferment.auth.locking :as locking]
            [ferment.db :as db]
            [ferment.session :as session]
            [ferment.user :as user])

  (:import (ferment AuthConfig)))

(def ^:private login-select-prefix
  (str "SELECT users.id AS id, users.email AS email, users.account_type AS account_type,"
       " users.login_attempts AS login_attempts,"
       " users.locked AS locked, users.soft_locked AS soft_locked,"
       " users.password AS intrinsic, password_suites.suite AS shared,"
       " COALESCE((SELECT GROUP_CONCAT(user_roles.role ORDER BY user_roles.role SEPARATOR ',')"
       "            FROM user_roles WHERE user_roles.user_id = users.id), '') AS roles"
       " FROM users JOIN password_suites ON password_suites.id = users.password_suite_id"
       " WHERE users.email = ?"))

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- account-type-k
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v trim-s keyword)
    :else nil))

(defn- role-k
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (trim-s v)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- parse-roles
  [v]
  (cond
    (nil? v) #{}
    (set? v) (into #{} (keep role-k) v)
    (sequential? v) (into #{} (keep role-k) v)
    (keyword? v) #{v}
    (string? v) (->> (str/split v #",")
                     (keep role-k)
                     set)
    :else #{}))

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
    (let [roles' (parse-roles (:roles row))]
      (cond-> {:user/id                  (:id row)
               :user/email               (:email row)
               :user/account-type        (some-> (:account_type row) trim-s keyword)
               :user/login-attempts      (:login_attempts row)
               :auth/locked-at           (:locked row)
               :auth/soft-locked-at      (:soft_locked row)
               :auth/locked?             (boolean (or (:locked row) (:soft_locked row)))
               :auth/password-intrinsic  (:intrinsic row)
               :auth/password-shared     (:shared row)}
        (seq roles') (assoc :user/roles roles')))))

(defn- lock-state
  [login-data auth-config now]
  (let [hard?      (locking/hard-locked? login-data)
        soft?      (locking/soft-locked? login-data auth-config now)
        locked?    (or hard? soft?)
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

(def ^:private session-public-keys
  [:session/id
   :session/version
   :session/state
   :session/frozen?
   :session/updated-at
   :session/last-access-at
   :session/frozen-at
   :session/thawed-at])

(defn- session-service
  [opts]
  (let [svc (some-> opts :session/service)]
    (when (and (map? svc) (fn? (:open! svc)))
      svc)))

(defn- session-id
  [opts]
  (or (some-> opts :session/id trim-s)
      (some-> opts :session-id trim-s)))

(defn- session-principal
  [user]
  (let [roles' (->> (parse-roles (:user/roles user))
                    sort
                    vec)]
    (cond-> {}
      (some? (:user/id user)) (assoc :user/id (:user/id user))
      (some? (:user/email user)) (assoc :user/email (:user/email user))
      (some? (:user/account-type user)) (assoc :user/account-type (:user/account-type user))
      (seq roles') (assoc :user/roles roles'))))

(defn- open-auth-session
  [opts user]
  (let [sid      (session-id opts)
        service  (session-service opts)
        principal (session-principal user)
        ts       (long (now-ms))
        meta'    (if (map? (:session/meta opts)) (:session/meta opts) {})
        open-raw (if (map? (:session/options opts)) (:session/options opts) {})
        open-opts (assoc open-raw
                         :session/meta
                         (merge (cond-> {:source :auth/authenticate-password
                                         :auth/principal principal
                                         :auth/principal-at ts
                                         :auth/principal-refreshed-at ts}
                                  (some? (:user/id principal)) (assoc :user/id (:user/id principal))
                                  (some? (:user/email principal)) (assoc :user/email (:user/email principal))
                                  (some? (:user/account-type principal)) (assoc :user/account-type (:user/account-type principal))
                                  (seq (:user/roles principal)) (assoc :user/roles (:user/roles principal)))
                                meta'))]
    (when (and sid service)
      (try
        (some-> (session/open! service sid open-opts)
                (select-keys session-public-keys))
        (catch Throwable _ nil)))))

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
           db-src (or (auth/db auth-source account-type')
                      (auth/db auth-source))]
       (when db-src
         (some-> (if account-type'
                   (db/<exec-one! db-src query
                                  [:users/email email']
                                  [:users/account-type (name account-type')])
                   (db/<exec-one! db-src query [:users/email email']))
                 login-row->data))))))

(defn authenticate-password
  "Checks plain password against credentials loaded from DB through auth config.

  Returns:
  - success: `{:ok? true :user {...}}`
  - failure: `{:ok? false :error <keyword>}`"
  ([auth-source email password]
   (authenticate-password auth-source email password nil nil))
  ([auth-source email password account-type-or-opts]
   (if (map? account-type-or-opts)
     (authenticate-password auth-source email password nil account-type-or-opts)
     (authenticate-password auth-source email password account-type-or-opts nil)))
  ([auth-source email password account-type opts]
   (let [email'       (trim-s email)
         password'    (trim-s password)
         account-type' (account-type-k account-type)
         opts'        (if (map? opts) opts {})
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
                 (let [user' (dissoc login-data
                                     :auth/password-shared
                                     :auth/password-intrinsic
                                     :auth/locked?
                                     :auth/locked-at
                                     :auth/soft-locked-at)
                       session' (open-auth-session opts' user')]
                   (cond-> {:ok? true
                            :user user'}
                     (map? session') (assoc :session session'))))
               (do
                 (when (and db-src user-id)
                   (user/update-login-failed! db-src user-id max-attempts))
                 {:ok? false
                  :error :auth/invalid-credentials}))))
         (do
           (wait-no-user! auth-config)
           {:ok? false
            :error :auth/invalid-credentials}))))))
