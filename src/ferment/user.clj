(ns

    ^{:doc    "Administrative user management helpers."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.user

  (:require [clojure.string :as str]
            [clojure.core.cache.wrapped :as cwr]
            [next.jdbc :as jdbc]
            [ferment.auth :as auth]
            [ferment.db :as db]
            [ferment.identity :as identity]
            [io.randomseed.utils :refer [safe-parse-long
                                         some-keyword
                                         some-str]]))

(defonce ^:redef props-cache    (atom nil))
(defonce ^:redef settings-cache (atom nil))
(defonce ^:redef identity-cache (atom nil))
;; Cache config currently uses :ferment.user/ids-cache. Keep alias to props-cache
;; so DB cache bootstrap wires into the same cache atom used by user props helpers.
(defonce ^:redef ids-cache      props-cache)

(def ^:private select-user-by-id-sql
  (str "SELECT id, email, account_type, password_suite_id, password,"
       " login_attempts, soft_locked, locked"
       " FROM users WHERE id = ? LIMIT 1"))

(def ^:private select-user-by-email-sql
  (str "SELECT id, email, account_type, password_suite_id, password,"
       " login_attempts, soft_locked, locked"
       " FROM users WHERE email = ? LIMIT 1"))

(def ^:private select-id-by-email-sql
  "SELECT id FROM users WHERE email = ? LIMIT 1")

(def ^:private insert-user-sql
  (str "INSERT INTO users"
       " (email, account_type, password_suite_id, password, login_attempts, soft_locked, locked)"
       " VALUES (?, ?, ?, CAST(? AS JSON), 0, NULL, NULL)"))

(def ^:private update-password-sql
  (str "UPDATE users"
       " SET password_suite_id = ?,"
       "     password = CAST(? AS JSON),"
       "     login_attempts = 0,"
       "     soft_locked = NULL,"
       "     locked = NULL"
       " WHERE id = ?"))

(def ^:private delete-user-by-id-sql
  "DELETE FROM users WHERE id = ?")

(def ^:private insert-suite-sql
  (str "INSERT INTO password_suites (suite)"
       " VALUES (CAST(? AS JSON))"
       " ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"))

(def ^:private select-last-id-sql
  "SELECT LAST_INSERT_ID() AS id")

(def ^:private update-login-success-sql
  (str "UPDATE users"
       " SET login_attempts = 0,"
       "     soft_locked = NULL"
       " WHERE id = ?"))

(def ^:private update-login-failed-sql
  (str "UPDATE users"
       " SET login_attempts = login_attempts + 1,"
       "     soft_locked = CASE"
       "       WHEN login_attempts + 1 >= ? THEN CURRENT_TIMESTAMP(6)"
       "       ELSE soft_locked"
       "     END"
       " WHERE id = ?"))

(defn- parse-email
  [email]
  (some-> email identity/preparse-email))

(defn- id-to-db
  [v]
  (when-some [id' (safe-parse-long v)]
    (when (pos-int? id')
      (long id'))))

(defn- email-to-db
  [v]
  (some-> v parse-email str/lower-case))

(defn- long-or-nil
  [n]
  (when (some? n)
    (long n)))

(defn- to-long-or-zero
  [n]
  (safe-parse-long n 0))

(db/defcoercions :users
  :id                id-to-db        long
  :email             email-to-db     some-str
  :account-type      some-str        some-keyword
  :login-attempts    to-long-or-zero long
  :soft-locked       clojure.core/identity clojure.core/identity
  :locked            clojure.core/identity clojure.core/identity
  :password-suite-id safe-parse-long long-or-nil
  :password          nil             nil)

(def ^:const prop-cols
  [:id :email :account_type
   :password_suite_id :password
   :login_attempts :soft_locked :locked])

(def ^{:private true
       :arglists '([db ids] [db _ ids])}
  props-getter-coll
  (db/make-getter-coll db/lazy-execute! db/opts-lazy-simple-map
                       :users :id prop-cols))

(def ^{:private true
       :arglists '([db id] [db _ id] [db _ id & ids])}
  props-getter
  (db/make-getter db/lazy-execute-one! db/opts-lazy-simple-map
                  :users :id prop-cols props-getter-coll))

(def ^{:private true
       :arglists '([db id keys-vals])}
  props-setter
  (db/make-setter :users :id))

(def ^{:private true
       :arglists '([db id])}
  props-deleter
  (db/make-deleter :users :id))

(defn- normalize-email
  [email]
  (email-to-db email))

(defn- normalize-account-type
  [account-type]
  (some-> account-type some-keyword name))

(defn- normalize-identity-type
  [identity-type]
  (some-> identity-type some-keyword name keyword))

(defn- normalize-id
  [user-id]
  (when-some [user-id' (safe-parse-long user-id)]
    (when (pos-int? user-id')
      (long user-id'))))

(defn- ensure-cache!
  [cache]
  (when (and (some? cache) (nil? @cache))
    (reset! cache @(cwr/basic-cache-factory {})))
  cache)

(defn- cache-evict-safe!
  [cache key]
  (when (and (some? cache) (some? @cache) (some? key))
    (db/cache-evict! cache key)))

(defn- email-cache-key
  [email]
  [:email email])

(defn- detect-identity-type
  [user-identity]
  (cond
    (some? (normalize-id user-identity)) :id
    (some? (normalize-email user-identity)) :email
    :else nil))

(defn- resolve-auth-config
  [auth-source account-type]
  (or (when account-type
        (auth/config auth-source account-type))
      (auth/config auth-source)))

(defn- resolve-db
  [auth-source account-type auth-config]
  (or (when account-type
        (auth/db auth-source account-type))
      (auth/db auth-source)
      (auth/db auth-config)))

(defn- default-account-type-name
  [auth-config]
  (or (some-> auth-config :account-types :default-name)
      (some-> auth-config :id name)
      "user"))

(defn- update-count
  [execute-result]
  (long (or (some-> execute-result first :next.jdbc/update-count)
            0)))

(defn- user-public
  [row]
  (when (map? row)
    {:user/id           (:id row)
     :user/email        (:email row)
     :user/account-type (some-> (:account_type row) some-keyword)
     :auth/locked?      (boolean (or (:locked row) (:soft_locked row)))}))

(defn- evict-user-caches!
  [user-id email]
  (when-some [user-id' (normalize-id user-id)]
    (cache-evict-safe! props-cache (db/<- :users/id user-id')))
  (when-some [email' (normalize-email email)]
    (cache-evict-safe! identity-cache (email-cache-key email')))
  nil)

(declare get-user-by-id)

(defn update-login-ok!
  "Marks successful login and resets soft lock + login attempts for a user."
  [connectable user-id]
  (when-some [user-id' (normalize-id user-id)]
    (let [row    (get-user-by-id connectable user-id')
          email' (:email row)
          result (jdbc/execute! connectable [update-login-success-sql user-id'])
          ok?    (pos? (update-count result))]
      (evict-user-caches! user-id' email')
      ok?)))

(defn update-login-failed!
  "Marks failed login attempt and applies soft lock when threshold is reached."
  ([connectable user-id]
   (update-login-failed! connectable user-id 3))
  ([connectable user-id max-attempts]
   (when-some [user-id' (normalize-id user-id)]
     (let [max-attempts' (long (max 1 (or (safe-parse-long max-attempts) 3)))
           row           (get-user-by-id connectable user-id')
           email'        (:email row)
           result        (jdbc/execute! connectable
                                        [update-login-failed-sql max-attempts' user-id'])
           ok?           (pos? (update-count result))]
       (evict-user-caches! user-id' email')
       ok?))))

(defn get-user-by-id
  "Returns user row by numeric user ID or `nil`."
  [connectable user-id]
  (when-some [user-id' (safe-parse-long user-id)]
    (when (pos-int? user-id')
      (jdbc/execute-one! connectable [select-user-by-id-sql (long user-id')] db/opts-simple-map))))

(defn get-user-by-email
  "Returns user row by e-mail (normalized) or `nil`."
  [connectable email]
  (when-some [email' (normalize-email email)]
    (jdbc/execute-one! connectable [select-user-by-email-sql email'] db/opts-simple-map)))

(defn get-user
  "Returns user row by selector (`id`, e-mail string, or map with `:id`/`:email`)."
  [connectable selector]
  (cond
    (map? selector)
    (or (get-user-by-id connectable (:id selector))
        (get-user-by-email connectable (:email selector)))

    (or (integer? selector)
        (and (number? selector)
             (= selector (long selector))))
    (get-user-by-id connectable selector)

    :else
    (or (get-user-by-email connectable selector)
        (get-user-by-id connectable selector))))

(defmulti query-id
  "Queries user ID for a normalized identity type.

  Extension point: add methods for new identity types (e.g. :phone, :uid)."
  (fn [_connectable identity-type _user-identity]
    (normalize-identity-type identity-type)))

(defmethod query-id :id
  [_connectable _identity-type user-identity]
  (normalize-id user-identity))

(defmethod query-id :email
  [connectable _identity-type user-identity]
  (when-some [email' (normalize-email user-identity)]
    (some-> (jdbc/execute-one! connectable
                               [select-id-by-email-sql email']
                               db/opts-simple-vec)
            first
            long)))

(defmethod query-id :default
  [_connectable _identity-type _user-identity]
  nil)

(defn props-by-id
  "Returns cached user properties for a numeric user ID."
  [connectable user-id]
  (when-some [user-id' (safe-parse-long user-id)]
    (ensure-cache! props-cache)
    (db/get-cached props-cache props-getter connectable (long user-id'))))

(defn prop-by-id
  "Returns cached single user property for a numeric user ID."
  [connectable prop-id user-id]
  (when (keyword? prop-id)
    (when-some [user-id' (safe-parse-long user-id)]
      (ensure-cache! props-cache)
      (db/get-cached-prop props-cache props-getter connectable prop-id (long user-id')))))

(defn props-set!
  "Updates one or more user columns by ID and evicts user-related caches."
  [connectable user-id keys-vals]
  (when-some [user-id' (safe-parse-long user-id)]
    (let [user-id' (long user-id')
          current  (get-user-by-id connectable user-id')
          result   (props-setter connectable user-id' keys-vals)
          email    (or (:email keys-vals) (:email current))]
      (evict-user-caches! user-id' email)
      result)))

(defn props-del!
  "Deletes user row by ID through generic deleter and evicts caches."
  [connectable user-id]
  (when-some [user-id' (safe-parse-long user-id)]
    (let [user-id' (long user-id')
          current  (get-user-by-id connectable user-id')
          result   (props-deleter connectable user-id')]
      (evict-user-caches! user-id' (:email current))
      result)))

(defn get-id
  "Returns numeric user ID for a given identity (email/id), not cached."
  ([connectable user-identity]
   (when connectable
     (cond
       (map? user-identity)
       (or (get-id connectable (:id user-identity))
           (get-id connectable (:email user-identity)))

       :else
       (when-some [identity-type (detect-identity-type user-identity)]
         (query-id connectable identity-type user-identity)))))
  ([connectable identity-type user-identity]
   (if-some [identity-type' (normalize-identity-type identity-type)]
     (query-id connectable identity-type' user-identity)
     (get-id connectable user-identity))))

(defn id
  "Returns cached numeric user ID for a given identity (email/id)."
  ([connectable user-identity]
   (when connectable
     (or (normalize-id user-identity)
         (when-some [email' (normalize-email user-identity)]
           (ensure-cache! identity-cache)
           (if (some? @identity-cache)
             (cwr/lookup-or-miss identity-cache
                                 (email-cache-key email')
                                 (fn [_] (get-id connectable :email email')))
             (get-id connectable :email email'))))))
  ([connectable identity-type user-identity]
   (case (some-> identity-type some-keyword name keyword)
     :id (normalize-id user-identity)
     :email (id connectable (normalize-email user-identity))
     (id connectable user-identity))))

(defn id-of
  "Like `id` but identity type is the first argument."
  [identity-type connectable user-identity]
  (id connectable identity-type user-identity))

(defn exists?
  "Returns `true` when user exists for the given identity."
  ([connectable user-identity]
   (some? (id connectable user-identity)))
  ([connectable identity-type user-identity]
   (some? (id connectable identity-type user-identity))))

(defn prop
  "Gets user property by identity (email/id), using identity cache + props cache."
  [connectable prop-id user-identity]
  (when-some [user-id' (id connectable user-identity)]
    (prop-by-id connectable prop-id user-id')))

(defn prop-of
  "Like `prop` but identity type is explicit."
  [identity-type connectable prop-id user-identity]
  (when-some [user-id' (id connectable identity-type user-identity)]
    (prop-by-id connectable prop-id user-id')))

(defn create-or-get-shared-suite-id
  "Creates a shared password suite if missing and returns its ID.

  Operation is atomic at SQL level via:
  `INSERT ... ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)`."
  [connectable suite]
  (cond
    (nil? suite) nil
    (pos-int? suite) (long suite)
    :else
    (jdbc/with-transaction [tx connectable]
      (jdbc/execute! tx [insert-suite-sql suite])
      (some-> (jdbc/execute-one! tx [select-last-id-sql] db/opts-simple-map)
              :id
              long))))

(defn- generate-password-data
  [connectable auth-config plain-password]
  (when-some [suites (auth/make-password-json plain-password auth-config)]
    (when-some [suite-id (create-or-get-shared-suite-id connectable (:shared suites))]
      {:password-suite-id suite-id
       :password          (:intrinsic suites)})))

(defn create-user!
  "Creates a new user with encrypted password.

  Returns:
  - success: `{:ok? true :created? true :user {...}}`
  - failure: `{:ok? false :error <keyword> ...}`."
  ([auth-source params]
   (create-user! auth-source
                 (:email        params)
                 (:password     params)
                 (:account-type params)))
  ([auth-source email password]
   (create-user! auth-source email password nil))
  ([auth-source email password account-type]
   (let [email'        (normalize-email email)
         password'     (some-str password)
         account-type' (normalize-account-type account-type)]
     (cond
       (nil? email')
       {:ok? false :error :user/invalid-email}

       (nil? password')
       {:ok? false :error :user/invalid-password}

       :else
       (let [auth-config (resolve-auth-config auth-source account-type')
             db-src      (resolve-db auth-source account-type' auth-config)
             at-name     (or account-type'
                             (default-account-type-name auth-config))]
         (cond
           (nil? auth-config)
           {:ok? false :error :auth/not-configured}

           (nil? db-src)
           {:ok? false :error :db/not-configured}

           :else
           (jdbc/with-transaction [tx db-src]
             (if (get-user-by-email tx email')
               {:ok?   false
                :error :user/already-exists
                :email email'}
               (if-some [{:keys [password-suite-id password]}
                         (generate-password-data tx auth-config password')]
                 (do
                   (jdbc/execute! tx [insert-user-sql
                                      email'
                                      at-name
                                      (long password-suite-id)
                                      password])
                   (let [id (some-> (jdbc/execute-one! tx [select-last-id-sql] db/opts-simple-map)
                                    :id
                                    long)]
                     (evict-user-caches! id email')
                     {:ok?      true
                      :created? true
                      :user     {:user/id           id
                                 :user/email        email'
                                 :user/account-type (some-keyword at-name)}}))
                 {:ok?   false
                  :error :auth/password-generation-failed})))))))))

(defn change-password!
  "Changes user password and resets lock/login-attempt fields.

  `selector` can be user ID, e-mail, or map with `:id`/`:email`.
  Returns map with `:ok?` and update metadata."
  ([auth-source selector new-password]
   (change-password! auth-source selector new-password nil))
  ([auth-source selector new-password account-type]
   (let [new-password' (some-str new-password)
         account-type' (normalize-account-type account-type)]
     (cond
       (nil? new-password')
       {:ok? false :error :user/invalid-password}

       :else
       (let [auth-config (resolve-auth-config auth-source account-type')
             db-src      (resolve-db auth-source account-type' auth-config)]
         (cond
           (nil? auth-config)
           {:ok? false :error :auth/not-configured}

           (nil? db-src)
           {:ok? false :error :db/not-configured}

           :else
           (jdbc/with-transaction [tx db-src]
             (if-some [user-row (get-user tx selector)]
               (let [at-name (or account-type'
                                 (some-> (:account_type user-row) some-keyword name)
                                 (default-account-type-name auth-config))
                     auth-config' (or (resolve-auth-config auth-source at-name)
                                      auth-config)]
                 (if-some [{:keys [password-suite-id password]}
                           (generate-password-data tx auth-config' new-password')]
                   (let [result (jdbc/execute! tx [update-password-sql
                                                   (long password-suite-id)
                                                   password
                                                   (long (:id user-row))])
                         n      (update-count result)]
                     (evict-user-caches! (:id user-row) (:email user-row))
                     {:ok? (pos? n)
                      :updated? (pos? n)
                      :user {:user/id (:id user-row)
                             :user/email (:email user-row)
                             :user/account-type (some-> at-name some-keyword)}})
                   {:ok? false
                    :error :auth/password-generation-failed}))
               {:ok? false
                :error :user/not-found}))))))))

(defn delete-user!
  "Deletes user identified by selector (ID, e-mail, or `{:id ...}`/`{:email ...}`
  map)."
  [auth-source selector]
  (let [auth-config (resolve-auth-config auth-source nil)
        db-src      (resolve-db auth-source nil auth-config)]
    (cond
      (nil? db-src)
      {:ok? false :error :db/not-configured}

      :else
      (jdbc/with-transaction [tx db-src]
        (if-some [user-row (get-user tx selector)]
          (let [result (jdbc/execute! tx [delete-user-by-id-sql (long (:id user-row))])
                n      (update-count result)]
            (evict-user-caches! (:id user-row) (:email user-row))
            {:ok?      (pos? n)
             :deleted? (pos? n)
             :user     {:user/id           (:id user-row)
                        :user/email        (:email user-row)
                        :user/account-type (some-> (:account_type user-row) some-keyword)}})
          {:ok?   false
           :error :user/not-found})))))

(def update-password! change-password!)
(def set-password!    change-password!)
(def remove-user!     delete-user!)
