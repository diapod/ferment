(ns

    ^{:doc    "Cross-category databases and generators for ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.db

  (:refer-clojure :exclude [memoize parse-long uuid random-uuid -> <-])

  (:require [clojure.set                   :as                    set]
            [clojure.core                  :as                      c]
            [clojure.core.cache            :as                  cache]
            [clojure.core.memoize          :as                    mem]
            [next.jdbc                     :as                   jdbc]
            [next.jdbc.connection          :as             connection]
            [next.jdbc.protocols           :as                    jpr]
            [ragtime.repl                  :as           ragtime-repl]
            [potemkin                      :as                      p]
            [io.randomseed.utils           :refer                :all]
            [io.randomseed.utils.ip        :as                     ip]
            [io.randomseed.utils.db        :as                     db]
            [io.randomseed.utils.db.coercion]
            [io.randomseed.utils.db.types  :as               db-types]
            [io.randomseed.utils.fs        :as                     fs]
            [io.randomseed.utils.var       :as                    var]
            [io.randomseed.utils.map       :as                    map]
            [taoensso.nippy                :as                  nippy]
            [ferment                       :refer                :all]
            [ferment.app                   :as                    app]
            [ferment.system                :as                 system]
            [ferment.logging               :as                    log]
            [ferment.types.db              :refer                :all]
            [io.randomseed.utils.identity  :as               identity])

  (:import (ferment         DBConfig)
           (clojure.lang           Keyword
                                   Fn)
           (com.zaxxer.hikari      HikariConfig
                                   HikariDataSource
                                   HikariPoolMXBean)
           (javax.sql              DataSource)
           (java.io                Closeable)
           (java.lang.reflect      Method)))

(set! *warn-on-reflection* true)

(alter-var-root #'nippy/*thaw-serializable-allowlist*
                conj "com.google.i18n.phonenumbers.Phonenumber$PhoneNumber")

(defonce ^:redef auth      nil)
(defonce ^:redef migrators nil)
(defonce ^:redef caches    nil)

;; Database column readers and result set setters

(db-types/add-all-readers)
(db-types/add-all-setters)

;; Type checks

(p/import-vars [io.randomseed.utils.db data-source?])

;; Memoization

(p/import-vars [io.randomseed.utils.db
                memoize memoize+ memoizer invalidate! invalidate+! invalidator])

;; Generic getters and setters

(p/import-vars [io.randomseed.utils.db
                make-setter make-deleter
                get-ids get-id not-found?])

;; Cached database access

(p/import-vars [io.randomseed.utils.db
                cache-prepare cache-create cache-evict! cache-lookup-coll cache-lookup
                get-cached-coll get-cached get-cached-coll-prop
                get-cached-prop get-cached-prop-or-default])

;; SQL helpers

(p/import-vars [io.randomseed.utils.db
                for-replace for-insert-or for-replace-multi for-insert-multi-or
                insert-or! insert-multi-or!
                insert-or-replace-multi! insert-or-ignore-multi!
                insert-or-replace! insert-or-ignore!
                replace! replace-multi!])

;; Database result processing helpers

(p/import-vars [io.randomseed.utils.db get-failed? id-from-db id-to-db])

;; Settings abstraction

(p/import-vars [io.randomseed.utils.db make-setting-getter make-setting-setter make-setting-deleter])

;; Cached settings handling

(p/import-vars [io.randomseed.utils.db cached-setting-get cached-setting-set cached-setting-del])

;; Single-point cache management

(p/import-vars [io.randomseed.utils.db init-cache init-caches remove-caches])

(defn print-caches
  ([]           (db/print-caches caches))
  ([caches-obj] (db/print-caches caches-obj)))

(defn list-caches [] (print-caches))

;; Memoization

(defn mem-assoc-existing!
  "Manual cache updater for functions memoized with `clojure.core.memoize`. Sets a key
  `k` to a value `v` in a map being a cached result of prior calling memoized
  function `f`. Will not associate any value if the caching key does not exist. The
  key should be passed as a vector in `key`."
  ([f key k v]
   (mem/memo-swap! f #(if-some [e (cache/lookup %1 %2)]
                        (cache/miss %1 %2 (delay (map/qassoc @e k v))) %1)
                   key))
  ([f key k v & kvs]
   (mem/memo-swap! f #(if-some [e (cache/lookup %1 %2)]
                        (cache/miss %1 %2 (delay (apply map/qassoc @e k v kvs))) %1)
                   key)))


;; Coercion, SQL parameter/query helpers, DB wrappers and builder options are provided
;; by io.randomseed.utils.db.coercion and re-exported here.

(p/import-vars
 [io.randomseed.utils.db.coercion
  in-coercer out-coercer
  get-in-coercer* get-in-coercer
  get-out-coercer* get-out-coercer
  literal-result
  coerce-in coerce-in* coerce-out coerce-out*
  coerce-seq-in coerce-seq-in* coerce-seq-out coerce-seq-out*
  <- -> <-seq seq->
  gen-qs-keyword bindable-sym bindable-sym?
  <<- <<-*
  simple-> map->
  defcoercions
  gen-builder gen-builder-delayed
  opts-map opts-simple-map opts-vec opts-simple-vec
  opts-slashed-map opts-slashed-vec
  opts-lazy-vec opts-lazy-simple-vec opts-lazy-slashed-vec
  opts-lazy-map opts-lazy-simple-map opts-lazy-slashed-map
  <q <dq <d-do! <d-exec! <d-exec-one! <do! <exec! <exec-one!
  lazy-execute-one! lazy-execute!
  execute-one! execute!
  lazy-do lazy-get-by-id
  make-getter make-getter-coll])

;; Configuration record

(defn db-config?
  "Returns true if a value of the given argument is an instance of DBConfig record
  type."
  [v]
  (instance? DBConfig v))

(defn ds
  "Gets the data source from the DBConfig record. If the given argument is not an
  instance of DBConfig, it simply returns it."
  ^DataSource [v]
  (if (instance? DBConfig v) (:datasource v) v))

;; Configuration helpers

(def ^{:arglists '([m] [v])}
  dbname-key-finder
  "Finds a database identifier in the given map `m` or by taking it from `v` if it is a
  string or ident."
  (some-fn (comp some-str :orig-key)
           #(if (or (string? %) (ident? %)) (some-str %))
           (comp some-str :dbkey)
           (comp some-str :dbkey :properties)
           (comp some-str :dbkey :datasource)
           (comp some-str :dbkey :datasource :datastore)
           (comp some-str :dbkey :datastore :datasource)
           (comp some-str :dbkey :datastore)
           (comp some-str :dbkey :db-spec :datastore)
           (comp some-str :dbkey :db-spec)
           (comp some-str :dbkey :properties :datasource)
           (comp some-str :dbkey :properties :datastore)))

(def ^{:arglists '([m] [v])}
  dbname-finder
  "Finds a database name in the given map `m` or by taking it from `v` if it is a
  string or ident."
  (some-fn #(if (or (string? %) (ident? %)) (some-str %))
           (comp some-str :dbname :properties)
           (comp some-str :dbname :datasource)
           (comp some-str :dbname)
           (comp some-str :dsname)
           (comp some-str :name :db)
           (comp some-str :dbname :db)
           (comp some-str :db-name)
           (comp some-str :dbname :datasource :datastore)
           (comp some-str :dbname :datastore :datasource)
           (comp some-str :dbname :datastore)
           (comp some-str :dbname :db-spec :datastore)
           (comp some-str :dbname :db-spec)
           (comp some-str :dbname :properties :datasource)
           (comp some-str :dbname :properties :datasource)
           (comp some-str :name)))

(defn db-name
  "Obtains the database (data source) name from the given configuration data structure
  by using known patterns."
  ([v]
   (if v
     (or (and (db-config? v) (some-str (get v :dbname)))
         (dbname-finder v)
         nil)))
  ([v & more]
   (or (db-name v)
       (some dbname-finder (filter identity (cons v more)))
       nil)))

(defn db-key-name
  "Obtains the database (data source) key name from the given configuration data
  structure by using known patterns."
  ([v]
   (when v
     (or (and (db-config? v) (some-str (get v :dbkey)))
         (dbname-key-finder v)
         nil)))
  ([v & more]
   (or (db-key-name v)
       (some dbname-key-finder (filter identity (cons v more)))
       nil)))

;; Migrations

(declare init-db)
(declare close-db)
(declare close-mig)

(defn migration
  "Calls `migrator-obj` function without any arguments."
  ([migrator-obj]
   (migrator-obj)))

(defn migrations
  "Takes a migrators vector (`migrators-vec`) or uses a default migrators vector (from
  a global variable `ferment.db/migrators`) and calls all of them (without passing
  any arguments) gathering returned results in a vector."
  ([]
   (migrations migrators))
  ([migrators-vec]
   ((apply juxt migrators-vec))))

(defn try-initialize-db
  "Tries to create a database described by `config` map if it does not exist yet."
  [config]
  (let [db-spec         (merge (:properties config) (:datasource (:datastore config)))
        db-create-extra (or (:create-extra config) (:create-extra db-spec))
        db-name         (or (db-name db-spec) (db-name config))]
    (when (and db-name db-spec)
      (jdbc/execute! (dissoc db-spec :dbname) [(str-spc "CREATE DATABASE IF NOT EXISTS" db-name db-create-extra)]))))

(defn migration-databases
  "Returns distinct identifiers of all migration databases found in `config` sequence
  of functions by calling each function and extracting a value under `:dbkey` key of
  a returned map."
  [config]
  (when (and config (sequential? config) (seq config))
    (->> (filter fn? config)
         (map #(:dbkey (%)))
         (filter identity)
         distinct seq)))

(defn- migrators-state
  ([mig-key]
   (migrators-state mig-key nil))
  ([mig-key app-state]
   (let [app-state (or app-state app/state)
         migrators (get app-state mig-key)
         mig-dbs   (set (migration-databases migrators))]
     {:migrators? (some? (seq migrators))
      :dbs-up     mig-dbs
      :props-up   (set (map #(get-in app/post-config [%1 :properties :key]) mig-dbs))})))

(defn- migrators-key
  [v]
  (or (if (map? v) (get v :migrators-key) (valuable v))
      ::migrators))

(defn migrate!
  "Migrates all databases (or a database specified by a migrator function passed as an
  argument) up to the latest migration. Optional map of options can be passed which
  will be merged with each migration options."
  ([]
   (migrate! nil))
  ([opts]
   (let [mig-key        (migrators-key opts)
         explicit-state (if (map? opts) (:app/state opts))
         initial-state  (or explicit-state app/state)
         state-pre      (migrators-state mig-key initial-state)
         start-admin!   (get opts :fn/start-admin app/start-admin!)]
     (when-not (:migrators? state-pre) (start-admin! mig-key))
     (let [app-state (if (:migrators? state-pre)
                       initial-state
                       app/state)]
       (if (fn? opts)
         (ragtime-repl/migrate (opts))
         (doseq [mconfig (get app-state mig-key)]
           (let [config (merge (mconfig) opts)
                 dbname (db-name config)
                 dbkey  (db-key-name config)]
             (when (pos-int? (::jdbc/update-count (first (try-initialize-db config))))
               (log/msg "Created empty database" dbname (str "(" dbkey ")")))
             (ragtime-repl/migrate config))))
       (when-not (:migrators? state-pre)
         (let [state-post (migrators-state mig-key app/state)
               stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                  (set/difference (:props-up state-post) (:props-up state-pre)))]
           (apply app/stop! mig-key (filter identity stop-keys)))))
     nil)))

(defn rollback!
  "Rolls back all databases or a database specified by a migrator function passed as an
  argument. Optional map of options can be passed which will be merged with each
  migration options. If a value is passed instead of a map or a function it will be
  used as an additional argument meaning a number of migrations or a migration ID."
  ([]
   (rollback! nil))
  ([opts]
   (let [mig-key        (migrators-key opts)
         explicit-state (if (map? opts) (:app/state opts))
         initial-state  (or explicit-state app/state)
         state-pre      (migrators-state mig-key initial-state)
         start-admin!   (get opts :fn/start-admin app/start-admin!)]
     (when-not (:migrators? state-pre) (start-admin! mig-key))
     (let [app-state (if (:migrators? state-pre)
                       initial-state
                       app/state)]
       (if (fn? opts)
         (ragtime-repl/rollback (opts))
         (if (or (not opts) (map? opts))
           (doseq [migrator (get app-state mig-key)] (ragtime-repl/rollback (merge (migrator) opts)))
           (doseq [migrator (get app-state mig-key)] (ragtime-repl/rollback (migrator) opts))))
       (when-not (:migrators? state-pre)
         (let [state-post (migrators-state mig-key app/state)
               stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                  (set/difference (:props-up state-post) (:props-up state-pre)))]
           (apply app/stop! mig-key (filter identity stop-keys)))))
     nil))
  ([opts amount-or-id]
   (let [mig-key        (migrators-key opts)
         explicit-state (if (map? opts) (:app/state opts))
         initial-state  (or explicit-state app/state)
         state-pre      (migrators-state mig-key initial-state)
         start-admin!   (get opts :fn/start-admin app/start-admin!)]
     (when-not (:migrators? state-pre) (start-admin! mig-key))
     (let [app-state (if (:migrators? state-pre)
                       initial-state
                       app/state)]
       (if (fn? opts)
         (ragtime-repl/rollback (opts) amount-or-id)
         (doseq [migrator (get app-state mig-key)] (ragtime-repl/rollback (merge (migrator) opts) amount-or-id)))
       (when-not (:migrators? state-pre)
         (let [state-post (migrators-state mig-key app/state)
               stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                  (set/difference (:props-up state-post) (:props-up state-pre)))]
           (apply app/stop! mig-key (filter identity stop-keys)))))
     nil)))

(defn migration-index
  "Gets a current value of ragtime-repl/migration-indexes."
  []
  (deref ragtime-repl/migration-index))

;; Generic close

(defn- unary-close-method
  ^Boolean [^Method met]
  (and (= "close" (.getName met)) (nil? (seq (.getParameterTypes met)))))

(defn close!
  "Calls `.close` on `obj` if it implements `java.io.Closeable` interface. Otherwise
  uses reflection to check if there is unary `.close` method, and if it is found,
  calls it passing `obj`."
  [obj]
  (if obj
    (if (isa? (class obj) Closeable)
      (.close ^Closeable obj)
      (some-> unary-close-method
              (filter (.getMethods ^Class (class obj)))
              first
              (^Method identity)
              (.invoke obj (object-array []))))))

;; Connection pool (HikariCP)

(defn pool-datasource
  "Returns connection pool (`HikariDataSource`) object obtained from `db-props`."
  ^HikariDataSource [db-props]
  (when-some [^HikariDataSource ds (connection/->pool HikariDataSource db-props)]
    (.setPoolName ^HikariDataSource ds (db-key-name db-props))
    (.setAllowPoolSuspension ^HikariDataSource ds true)
    (close! (jdbc/get-connection ^HikariDataSource ds))
    ds))

(defn close-pool
  "Closes connection pool `ds`."
  [^HikariDataSource ds]
  (.close ^HikariDataSource ds))

(defn suspend-pool
  "Suspends connection pool `ds`."
  [^HikariDataSource ds]
  (.suspendPool ^HikariPoolMXBean (.getHikariPoolMXBean ^HikariDataSource ds)))

(defn resume-pool
  "Resumes connection pool `ds`."
  [^HikariDataSource ds]
  (.resumePool ^HikariPoolMXBean (.getHikariPoolMXBean ^HikariDataSource ds))
  (close! (jdbc/get-connection ^HikariDataSource ds)))

;; Configuration initializers

(defn prep-db
  "Prepares database configuration."
  [config]
  (if-not (map? config)
    config
    (c/-> config
          (map/update-existing :dbname         fs/parse-java-properties)
          (map/update-existing :migrations-dir fs/parse-java-properties)
          (map/assoc-missing   :user           (get config :username))
          (map/assoc-missing   :username       (get config :user))
          (map/dissoc-if       :username       nil?)
          (map/dissoc-if       :user           nil?))))

(defn expand-db
  "Expands database configuration."
  [k config]
  {k (prep-db config)})

(defn init-db
  "Initializes database configuration `config` for the configuration key `k`."
  ([k config]
   (init-db k config
            (var/deref-symbol (:initializer config))
            (var/deref-symbol (:finalizer   config))
            (var/deref-symbol (:suspender   config))
            (var/deref-symbol (:resumer     config))))
  ([k config ds-getter]
   (init-db k config ds-getter nil nil nil))
  ([k config ds-getter ds-closer]
   (init-db k config ds-getter ds-closer nil nil))
  ([k config ds-getter ds-closer ds-suspender]
   (init-db k config ds-getter ds-closer ds-suspender nil))
  ([k config ds-getter ds-closer ds-suspender ds-resumer]
   (when config
     (let [db-props (c/-> :properties config (dissoc :logger :migrations-dir) prep-db)
           db-name  (db-name db-props config k)
           db-key   (db-key-name k db-props config)
           db-props (map/assoc-missing db-props :name db-name :dbkey db-key)]
       (log/msg "Configuring database" db-name (str "(" db-key ")"))
       (DBConfig. ^Fn      ds-getter
                  ^Fn      ds-closer
                  ^Fn      ds-suspender
                  ^Fn      ds-resumer
                  ^Keyword db-key
                  ^String  db-name
                  (ds-getter db-props))))))

(defn close-db
  "Closes the database connection. Calls configured finalized (`:finalizer` key)
  before."
  [k config]
  (when config
    (log/msg "Closing database connection to" (db-name config k) (str "(" (db-key-name k config) ")"))
    (let [ds-closer (or (:finalizer config) close!)]
      (if-some [ds (or (:datasource config) (:datastore config) (:database config))]
        (ds-closer ds))
      nil)))

(defn suspend-db
  "Suspends the database connection."
  [k config]
  (if-some [ds-suspender (:suspender config)]
    (when-some [ds (:datasource config)]
      (log/msg "Suspending database" (db-name config k) (str "(" (db-key-name k config) ")"))
      (ds-suspender ds))
    (system/halt-key! k config)))

(defn resume-db
  "Resumes the database connection."
  [k config old-config old-impl]
  (let [ds-resumer (or (:resumer old-impl) (:resumer config) (:resumer old-config))]
    (if (and ds-resumer (= (dissoc config :initializer :finalizer :suspender :resumer)
                           (dissoc config :initializer :finalizer :suspender :resumer)))
      (if-some [ds (:datasource old-impl)] (ds-resumer ds) old-impl)
      (do (system/halt-key! k old-impl)
          (system/init-key k config)))))

(defn default-reporter
  "Logs database migration event described by database identifier `db-k-name`, data
  source `ds`, operation (`:up` or `:down`) and migration identifier `id`."
  [db-k-name ds op id]
  (condp identical? op
    :up   (log/msg "Applying DB migration"      id "on" (db-key-name db-k-name ds))
    :down (log/msg "Rolling back DB migration"  id "on" (db-key-name db-k-name ds))
    (log/err "Unknown database operation" id "on" (db-key-name db-k-name ds))))

(defn migrator-config
  [config loader migration-dir]
  (let [db-key (db-key-name config)]
    (c/-> config
          (assoc :migrations (loader migration-dir))
          (map/assoc-missing  :initializer identity)
          (map/assoc-missing  :reporter  (partial default-reporter db-key))
          (map/update-missing :datastore (:initializer config)))))

(defn init-mig
  [k config]
  (let [ds     (ds (init-db k config))
        loader (var/deref (:loader config))
        migdir (fs/parse-java-properties (or (:migrations-dir config)
                                             (get-in config [:properties :migrations-dir])))
        config (c/-> config
                     (assoc :dbkey k :datastore ds)
                     (map/update-existing :reporter  var/deref-symbol)
                     (map/update-existing :strategy  keyword)
                     (dissoc :loader :logger :initializer :properties))]
    (fn []
      (migrator-config config loader migdir))))

(defn init-migrators
  "Initializes migrators given in a `config` sequence. Calls each function found or
  `init-mig` if it's not a function but migration configuration map."
  [config]
  (if (and config (sequential? config) (seq config))
    (mapv #(if (fn? %) % (init-mig nil %)) config)))

(defn close-mig
  "Closes database connection used for migration. Expects `config` to be an
  argument-less function returning database configuration."
  [k config]
  (if (and (ident? k) (fn? config))
    (when-some [config (config)]
      (close-db k config)
      nil)))

(system/add-expand   ::properties  [k config] (expand-db k config))
(system/add-init     ::properties  [_ config] config)
(system/add-halt!    ::properties  [_ config] nil)

(system/add-expand   ::initializer [k config] (expand-db k config))
(system/add-init     ::initializer [k config] (let [d (init-db k config)] (var/make k (ds d)) d))
(system/add-suspend! ::initializer [k config] (suspend-db k config))
(system/add-resume   ::initializer [k config old-config old-impl] (resume-db k config old-config old-impl))
(system/add-halt!    ::initializer [k config] (var/make k (close-db k config)))

(system/add-expand   ::migrator    [k config] (expand-db k config))
(system/add-init     ::migrator    [k config] (var/make k (init-mig  k config)))
(system/add-halt!    ::migrator    [k config] (var/make k (close-mig k config)))

(system/add-init     ::migrators   [k config] (var/make k (init-migrators config)))
(system/add-halt!    ::migrators   [k config] (var/make k nil))

(system/add-init     ::caches      [k config] (var/make k (init-caches  config)))
(system/add-halt!    ::caches      [k config] (var/make k (remove-caches config)))

(derive ::main                ::initializer)
(derive ::main.props          ::properties)
(derive ::main.migrator.props ::properties)
(derive ::main.migrator       ::migrator)

;; Generic coercers

(defn- email-to-db    ^String [v] (identity/->db :email v))
(defn- phone-to-db    ^String [v] (identity/->db :phone v))
(defn- long-or-nil    ^Long   [n] (when n (long n)))
(defn- identity-to-db         [v] (identity/->db v))
(defn- ip-v6-str      ^String [v] (some-> v ip/to-address ip/to-v6 ip/to-str-v6))

(defcoercions ::any
  :identity          identity-to-db               identity/of
  :email             email-to-db                  some-str
  :phone             phone-to-db                  identity/preparse-phone
  :account-type      some-str                     some-keyword
  :first-name        some-str                     some-str
  :middle-name       some-str                     some-str
  :last-name         some-str                     some-str
  :ip                ip-v6-str                    ip-v6-str
  :ip-address        ip-v6-str                    ip-v6-str
  :client-ip         ip-v6-str                    ip-v6-str
  :password-suite-id safe-parse-long              long-or-nil
  :password          nil                          nil)
