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
            [next.jdbc.sql                 :as               next-sql]
            [next.jdbc.connection          :as             connection]
            [next.jdbc.result-set          :as                     rs]
            [next.jdbc.prepare             :as                     jp]
            [next.jdbc.protocols           :as                    jpr]
            [ragtime.repl                  :as           ragtime-repl]
            [potemkin                      :as                      p]
            [io.randomseed.utils           :refer                :all]
            [io.randomseed.utils.ip        :as                     ip]
            [io.randomseed.utils.db        :as                     db]
            [io.randomseed.utils.db.types  :as               db-types]
            [io.randomseed.utils.fs        :as                     fs]
            [io.randomseed.utils.var       :as                    var]
            [io.randomseed.utils.map       :as                    map]
            [io.randomseed.utils.map       :refer    [qassoc qupdate]]
            [taoensso.nippy                :as                  nippy]
            [ferment                       :refer                :all]
            [ferment.app                   :as                    app]
            [ferment.system                :as                 system]
            [ferment.logging               :as                    log]
            [ferment.db.sql                :as                    sql]
            [ferment.types.db              :refer                :all]
            [ferment.types.identity        :refer                :all]
            [ferment.identity              :as               identity])

  (:import (ferment         DBConfig)
           (io.randomseed.lazy_map LazyMap)
           (clojure.lang           IPersistentMap
                                   PersistentVector
                                   Keyword
                                   Fn)
           (com.zaxxer.hikari      HikariConfig
                                   HikariDataSource
                                   HikariPoolMXBean)
           (javax.sql              DataSource)
           (java.io                Closeable)
           (java.lang.reflect      Method)
           (java.sql               Connection
                                   PreparedStatement
                                   DatabaseMetaData
                                   ParameterMetaData
                                   ResultSet
                                   ResultSetMetaData
                                   Statement)))

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

;; Settable parameters

(extend-protocol jp/SettableParameter

  ;; Adds generic coercion support for Identity objects
  ;; when they are being sent to a database using next-jdbc operations.
  ;; Uses ferment.identity/to-db*.

  ferment.Identity

  (set-parameter [^ferment.Identity v ^PreparedStatement ps ^long i]
    (jp/set-parameter (identity/to-db* v) ps i)))

;; Coercion

(defmulti in-coercer
  "Returns a coercer suitable for transforming the given argument `v` to a
  database-suitable value, assuming table and column specified by the given qualified
  keyword `table-column`."
  {:arglists '([^Keyword table-column v] [^Keyword column v])}
  identity)

(defmulti out-coercer
  "Returns a coercer suitable for transforming the given argument `v` read from a
  database, assuming table and column specified by the given qualified keyword
  `table-column`."
  {:arglists '([^Keyword table-column v] [^Keyword column v])}
  identity)

(defmethod in-coercer  :default [_] nil)
(defmethod out-coercer :default [_] nil)

(defn get-in-coercer*
  "Same as `get-in-coercer` but trusts that `table-column` is a fully-qualified
  keyword (already a result of `colspec-kw`)."
  {:see-also ["get-in-coercer"]}
  [table-column]
  (if-some [f (in-coercer table-column)] f
           (if (namespace table-column)
             (in-coercer (keyword (name table-column))))))

(defn get-in-coercer
  "Tries to obtain a database coercion function by calling `in-coercer` multimethod for
  column and table specified with `table` and `column`, or by a single
  `table-column`. Transforms arguments to a lisp-cased keyword. If there is no
  coercer for table and column, tries to getting one using the column alone.

  Returns coercer when it is found. Returns `false` when a coercer is found but
  explicitly set to undefined. Returns `nil` when there is no coercer."
  ([table column]
   (if-some [f (in-coercer (sql/colspec-kw table column))] f (in-coercer (sql/column-kw column))))
  ([table-column]
   (get-in-coercer* (sql/colspec-kw table-column))))

(defn get-out-coercer*
  "Same as `get-out-coercer` but trusts that `table-column` is a lisp-cased
  keyword (already a result of `colspec-kw`)."
  {:see-also ["get-out-coercer"]}
  [table-column]
  (if-some [f (out-coercer table-column)] f
           (when (namespace table-column)
             (out-coercer (keyword (name table-column))))))

(defn get-out-coercer
  "Tries to obtain a database coercion function by calling `out-coercer` multimethod
  for column and table specified with `table` and `column`, or by a single
  `table-column`. Transforms arguments to a lisp-cased keyword. If there is no
  coercer for table and column, falls back to getting one using the column alone.

  Returns coercer when it is found. Returns `false` when a coercer is found but
  explicitly set to undefined. Returns `nil` when there is no coercer."
  ([table column]
   (if-some [f (out-coercer (sql/colspec-kw table column))] f (out-coercer (sql/column-kw column))))
  ([table-column]
   (get-out-coercer* (sql/colspec-kw table-column))))

(defn- statically-convertable?
  "Returns `true` if `v` can be statically handled at compile-time, `false` otherwise."
  ([v]
   (or (nil? v) (keyword? v) (string? v) (number? v) (boolean? v)))
  ([v ts cs]
   (and (string? ts) (string? cs)
        (or (nil? v) (keyword? v) (string? v) (number? v) (boolean? v)))))

(defmacro literal-result
  "Returns a value of the given argument `v` if it is statically convertable value (and
  it is ok to be put into a source code in literal form) or a function. Otherwise
  returns `alt` or `nil` (if `alt` is not given)."
  {:no-doc true}
  ([v]     `(let [v# ~v] (if (or (fn? v#) (statically-convertable? v#)) v#)))
  ([v alt] `(let [v# ~v] (if (or (fn? v#) (statically-convertable? v#)) v# ~alt))))

(defn coerce-in
  "Coerces the given value `v` to a database type by calling a function returned by
  invoking `ferment.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function. If there is no coercer, returns unchanged `v`.

  Will immediately return the given value `v` if the coercer exists but it is set to
  `false`."
  ([table column v] (if-let [f (get-in-coercer table column)] (f v) v))
  ([table-column v] (if-let [f (get-in-coercer table-column)] (f v) v)))

(defn coerce-in*
  "Same as `coerce-in` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column v] (if-let [f (get-in-coercer* table-column)] (f v) v))

(defn coerce-out
  "Coerces the given value `v` from a database type by calling a function returned by
  invoking `ferment.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `v`.

  Will immediately return the given value `v` if the coercer exists but it is
  set to `false`."
  ([table column v] (if-let [f (get-out-coercer table column)] (f v) v))
  ([table-column v] (if-let [f (get-out-coercer table-column)] (f v) v)))

(defn coerce-out*
  "Same as `coerce-out` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column v] (if-let [f (get-out-coercer* table-column)] (f v) v))

(defn coerce-seq-in
  "Coerces a sequence of values `coll` to database types by calling a function returned
  by invoking `ferment.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `coll`."
  ([table column coll] (if-let [f (get-in-coercer table column)] (map f coll) coll))
  ([table-column coll] (if-let [f (get-in-coercer table-column)] (map f coll) coll)))

(defn coerce-seq-in*
  "Same as `coerce-seq-in` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column coll] (if-let [f (get-in-coercer* table-column)] (map f coll) coll))

(defn coerce-seq-out
  "Coerces a sequence of values `coll` from database types by calling a function
  returned by invoking `ferment.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `coll`."
  ([table column coll] (if-let [f (get-out-coercer table column)] (map f coll) coll))
  ([table-column coll] (if-let [f (get-out-coercer table-column)] (map f coll) coll)))

(defn coerce-seq-out*
  "Same as `coerce-seq-out` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column coll] (if-let [f (get-out-coercer* table-column)] (map f coll) coll))

(defmacro <-
  "Coerces value `v` to a database type by calling a function returned by invoking
  `ferment.db/in-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`.

  If a coercer can be obtained at compile-time, a coercion function-call form will be
  generated. If a coercer can be obtained at compile-time and the given value is
  statically convertable, value resulting from applying coercion function will be
  generated immediately."
  ([table column v]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-in* ~tc ~v)))
     `(coerce-in ~table ~column ~v)))
  ([table-column v]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-in* ~tc ~v)))
     `(coerce-in ~table-column ~v))))

(defn ->
  "Coerces value `v` from a database type by calling a function returned by invoking
  `ferment.db/out-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`.

  If a coercer can be obtained at compile-time, a coercion function-call form will be
  generated. If a coercer can be obtained at compile-time and the given value is
  statically convertable, value resulting from applying coercion function will be
  generated immediately."
  ([table column v]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-out* ~tc ~v)))
     `(coerce-out ~table ~column ~v)))
  ([table-column v]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-out* ~tc ~v)))
     `(coerce-out ~table-column ~v))))

(defmacro <-seq
  "Coerces a sequence of values `coll` to database types by calling a function returned
  by invoking `ferment.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`.

  If both, a table and a column can be used to establish coercion function at
  compile-time, a mapping form will be generated which uses that function."
  ([table column coll]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-in* ~tc ~coll)))
     `(coerce-seq-in ~table ~column ~coll)))
  ([table-column coll]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-in* ~tc ~coll)))
     `(coerce-seq-in ~table-column ~coll))))

(defmacro seq->
  "Coerces a sequence of values `coll` from database types by calling a function
  returned by invoking `ferment.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`.

  If both, a table and a column can be used to establish coercion function at
  compile-time, a mapping form will be generated which uses that function."
  ([table column coll]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-out* ~tc ~coll)))
     `(coerce-seq-out ~table ~column ~coll)))
  ([table-column coll]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-out* ~tc ~coll)))
     `(coerce-seq-out ~table-column ~coll))))

(defrecord QSlot [^String t ^String c v])

(defn- not-empty-qslot?
  "Returns `true` if the given `e` is of type `QSlot` and all of its essential fields
  are not empty."
  [e]
  (and (instance? QSlot e) (.t ^QSlot e) (.c ^QSlot e) (.v ^QSlot e)))

(defn- join-qslots
  "Joins consecutive `QSlot` records if their column and table fields are equal."
  [done qs]
  (let [prev (peek done)]
    (if (and (not-empty-qslot? qs) (not-empty-qslot? prev))
      (if (and (= (.t ^QSlot prev) (.t ^QSlot qs))
               (= (.c ^QSlot prev) (.c ^QSlot qs)))
        (conj (pop done) (qupdate prev :v into (.v ^QSlot qs)))
        (conj done qs))
      (conj done qs))))

(defn- c-t
  "Extracts column and table specs as a vector from the given control keyword and
  predefined hints for table and column spec."
  [ctrl table-spec column-spec]
  (when (keyword? ctrl)
    (let [[c t] (sql/column-table ctrl)]
      (if c
        (if t [c t] (if table-spec [c table-spec] [table-spec c]))
        [column-spec table-spec]))))

(defn- pp-conv-specs
  "Pre-parses table/column conversion specifications."
  ([coll]
   (->> (mapcat #(pp-conv-specs % nil nil) coll)
        (reduce join-qslots [])))
  ([e tspec cspec]
   (cond

     ;; control vector
     ;; (group of values followed by table or column spec)

     (vector? e)
     (let [ctrl (nth e 0 nil), coll (subvec e 1)]
       (if (keyword? ctrl)
         ;; static table/column name
         (let [[c t] (c-t ctrl tspec cspec)]
           (mapcat #(pp-conv-specs % t c) coll))
         ;; dynamic table/column name
         (let [[c t] (if tspec [ctrl tspec] [nil ctrl])]
           (mapcat #(pp-conv-specs % t c) coll))))

     ;; single value
     ;; (known table and column)

     (and tspec cspec (statically-convertable? e tspec cspec))
     (if-some [coercer-fn (literal-result (get-in-coercer tspec cspec))]
       (cons (if coercer-fn (literal-result (coercer-fn e) e) e) nil)
       (cons (QSlot. tspec cspec [e]) nil))

     ;; regular dynamically-convertable element
     ;; (known table and column)

     (and tspec cspec)
     (cons (QSlot. tspec cspec [e]) nil)

     ;; single value expressed with a simple symbol
     ;; (known table, missing column)

     (and tspec (simple-symbol? e))
     (cons (QSlot. tspec (name e) [e]) nil)

     ;; any value

     :else (cons e nil))))

(defn- parse-conv-spec
  "Parses value with optional table/column conversion specification to produce a source
  code. Expects values or `QSlot` records."
  [e]
  (if (or (not (instance? QSlot e)) (nil? (.t ^QSlot e)))
    (cons e nil)
    (let [t  (.t ^QSlot e)
          c  (.c ^QSlot e)
          v  (.v ^QSlot e)
          tc (when (and (or (keyword? t) (string? t))
                        (or (keyword? c) (string? c)))
               (literal-result (sql/colspec-kw t c)))]
      (if (= (count v) 1)
        (if tc
          (cons `(<- ~tc   ~(nth v 0)) nil)
          (cons `(<- ~t ~c ~(nth v 0)) nil))
        (if tc
          (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
            (if coercer-fn (map (fn [e] `(~coercer-fn ~e)) v) `~v)
            (map (fn [e] `(<- ~tc ~e)) v))
          (map (fn [e] `(<- ~t ~c ~e)) v))))))

(defn gen-qs-keyword
  "Generates unique but deterministic symbolic name for `t` (presumably table name),
  `c` (column name) and `v` (value, being an identifier). Returns a keyword named
  like `DB__[t]_[c]_[v]_[nnnnnnn]` where `[t]`, `[c]` and `[v]` are string
  representations of the given argument values, and `[nnnnnnn]` is a numeric
  representation of combined hash of all values given as arguments."
  ([^QSlot qs]   (gen-qs-keyword (.t qs) (.c qs) (.v qs)))
  ([^QSlot qs v] (gen-qs-keyword (.t qs) (.c qs) v))
  ([t c v]
   (let [^String h (c/-> (hash t) (hash-combine (hash c)) (hash-combine (hash v)) strb)
         ^String h (if (identical? \- (.charAt h 0)) (strb "0" (subs h 1)) h)]
     (keyword (strb "DB__" (some-str t) "_" (some-str c) "_" (some-str v) "_" h)))))

(defn- repeating-qslot-bindings
  "Returns a map with keys being keywords representing unique table/column/value names
  identifying repeated, single-valued `QSlot` elements from the given `coll`, and
  with associated values being expressions for performing output database coercion."
  [coll]
  (->> (filter #(instance? QSlot %) coll)
       (remove #(or (nil? (.t ^QSlot %)) (nil? (.c ^QSlot %)) (list? (.t ^QSlot %)) (list? (.c ^QSlot %))))
       (mapcat #(map (fn [v] (qassoc % :v v)) (.v ^QSlot %)))
       (frequencies) (seq)
       (filter #(> (val %) 1))
       (map (juxt #(gen-qs-keyword (key %)) #(qupdate (key %) :v vector)))
       (into {})
       (map/map-vals (comp first parse-conv-spec))))

(defn bindable-sym
  "Returns a bindable, auto-generated symbol for the table/column (from `qs`, which
  should be a `QSlot` record) and a value `v`. The unique identifier (obtained using
  `gen-qs-keyword`) must exists in `bindings` map. Otherwise, `nil` is returned."
  [bindings ^QSlot qs v]
  (let [h (gen-qs-keyword qs v)]
    (when (contains? bindings h) (symbol h))))

(defn bindable-sym?
  "Returns `true` if a bindable, auto-generated symbol for the table/column (from `qs`,
  which should be a `QSlot` record) and a value `v` exist in `bindings` map. Otherwise
  it returns `false`."
  [bindings ^QSlot qs v]
  (contains? bindings (gen-qs-keyword qs v)))

(defn- replace-bindable
  "Replaces bindable expressions from `:v` fields of `QSlot` records present in `coll`
  by unique symbols corresponding to them, with names created with `gen-qs-keyword`
  which exist as keys in `bindings` map."
  [bindings coll]
  (vec
   (mapcat
    (fn [qs]
      (if (instance? QSlot qs)
        (let [parts    (partition-by #(bindable-sym? bindings qs %) (.v ^QSlot qs))
              first-b? (bindable-sym? bindings qs (ffirst parts))]
          (mapcat (fn [qvals bindable?]
                    (if bindable?
                      (map #(bindable-sym bindings qs %) qvals)
                      (cons (qassoc qs :v (vec qvals)) nil)))
                  parts (iterate not first-b?)))
        (cons qs nil)))
    coll)))

(defn- prepend-qslot-bindings
  "Wraps the given `coll` in a `let` block with `bindings`."
  [bindings coll]
  (if-some [bindings (seq bindings)]
    (list `let (vec (mapcat #(update % 0 symbol) bindings)) coll)
    coll))

(defmacro <<-
  "Magical macro which converts a sequence of values with optional table and column
  specifications to a database-suitable formats. Pre-processing of arguments is
  executed at compile-time, further processing is performed at run-time.

  Any type of argument is accepted but literal vectors, including nested vectors, are
  **control structures**. Their first elements are table names (for the first one),
  column names (for nested vectors) or both (when expressed using fully-qualified
  keywords).

  Example: `(<<- 1 2 3 [:table-name [:column-name :val1 val2 (exp3) \"val4\"])`

  `1`, `2` and `3` are regular values, `:table-name` is literally expressed table
  name for coercer, `:column-name` is literally expressed column name for coercer,
  other expressions are values to be coerced. Table and column names can be dynamic,
  expressed with symbols or call forms.

  All macro arguments are sequentially transformed with the following rules:

  - If there is a **literal vector** at **1st level**, its first element should be a
  table name. All elements of that vector will inherit that table name during
  conversion to a database-suitable format.

  - If there is a **literal vector** nested within existing vector its first element
  will be considered a **column name**. All elements of that vector will inherit that
  column name during conversion to a database-suitable format.

  - If the given literal vector contains a **fully-qualified keyword** at its first
  position then both **table** and **column** will be memorized to be applied during
  conversion of elements contained in that vector (table taken from a namespace and
  column from a name of the keyword).

  - If there is a table name inherited but no column specified, a value is not
  converted but returned as is, with the exception: when the value is expressed as a
  **literal, simple symbol** then a column name will be derived from its name.

  - A sub-vector may begin with an unspecified value `nil`. In such case it will
  group values to be converted but column name will not be set. The values will be
  left unconverted unless they are simple symbol forms; in such case column names
  will be derived from their names.

  Values used to set column and table names at the beginnings of vectors can be
  expressed with any valid code. However, literal strings or literal keywords (or
  symbols in case of identifier-derived column names) will be pre-processed at
  compile time. So, if both column and table name are expressed that way, the
  conversion specifier will be generated ahead.

  Moreover, if apart from the above, a value to be coerced is expressed as a literal
  number, string, keyword, boolean, or a `nil`, it will be converted at compile-time.

  Conversion of repeating atomic expressions sharing the same table and column name
  identifiers (including symbolic identifiers) will be performed in ad-hoc created
  `let` block, and the results will be referenced using auto-generated symbols
  replacing the original expressions. Therefore, conversion for repeated symbols (and
  other atomic expressions) will be performed just once.

  Examples:

  `(<<- [:users [:id id] [:email email]])`

  The above will convert values expressed by `id` and `email` symbol forms using a
  variant of coercion multimethod registered for `:users/id` and `:users/email`
  keywords, accordingly.

  `(<<- [:users/id id [:email e]])`

  The above will convert values of `id` and `e` symbol forms using a variant of
  coercion multimethod registered for `:users/id` and `:users/email` keywords,
  accordingly.

  `(<<- [:users [:id id] [:confirmations/email email [:expires expires]])`

  The above will convert values of `id`, `email` and `expires` symbol forms using a
  variant of coercion multimethod registered for `:users/id`, `:confirmations/email`,
  and `:confirmations/expires` keywords, accordingly. We can see that second vector
  changes the table name to `confirmations` in its scope so later `expires` derives
  it and is converted with `:confirmations/expires` specification.

  This is synonymous to:

  `(<<- [:users id] [:confirmations email expires])`

  As we can see, the `id` symbolic identifier is used to set the column name, same as
  `email` and `expires` symbols. The above code will be expanded to:

  ```
  [(ferment.db/<- :users/id id)
   (ferment.db/<- :confirmations/email email)
   (ferment.db/<- :confirmations/expires expires)]
  ```

  And then to:

  ```
  [(#<Fn@4d7e49d ferment.model.user/id_to_db> id)
   (ferment.db/coerce-in* :confirmations/email email)
   (#<Fn@3bf6fdc6 ferment.model.confirmation/to_expiry> expires)]
  ```

  We can see that coercers for `id` and `expires` symbols were resolved and function
  call forms were created at compile-time. That's because `:users/id` and
  `:confirmations/expires` were recognized as existing dispatch values when calling
  `in-coercer` internally. A coercer for the `email` symbol (using
  `:confirmations/email` dispatch value) was not recognized at compile-time so
  the call to `ferment.db/coerce-in*` was generated instead.

  Let's have a quick look at some real-world example:

  ```
  (ferment.db/<<-  [:confirmations id code token reason id-type expires id id])
  ```

  And generated Clojure code (phase 1):

  ```
  (let [DB__confirmations_id_id_54377141 (ferment.db/<- :confirmations/id id)]
    [DB__confirmations_id_id_54377141
     (ferment.db/<- :confirmations/code       code)
     (ferment.db/<- :confirmations/token     token)
     (ferment.db/<- :confirmations/reason   reason)
     (ferment.db/<- :confirmations/id-type id-type)
     (ferment.db/<- :confirmations/expires expires)
     DB__confirmations_id_id_54377141
     DB__confirmations_id_id_54377141])
  ```

  And fully expanded:

  ```
  (let* [DB__confirmations_id_id_54377141 (#<Fn@5a424a5a ferment.identity/__GT_db> id)]
    [DB__confirmations_id_id_54377141
     (#<Fn@279d4dd9 io.randomseed.utils/safe_parse_long>       code)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>             token)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>            reason)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>           id-type)
     (#<Fn@4ac5d426 ferment.model.confirmation/to_expiry> expires)
     DB__confirmations_id_id_54377141
     DB__confirmations_id_id_54377141])
  ```

  A SQL query which uses the sequence of values presented above needs one of
  them (identified with the `id`) to be repeated. We can observe that the macro
  generated `let` binding for it to assign the result of calling
  `ferment.identity/->db` on `id` to auto-generated symbol named
  `DB__confirmations_id_id_54377141`. This symbol is then re-used in output vector
  multiple times so the calculation is performed just once.

  Also, other coercers were successfully resolved to function objects during macro
  expansion since we have static table and column specifiers given.

  Rule of a thumb is: if you can express certain values or specifications with
  literal strings or keywords, it may speed things up."
  [& specs]
  (let [pre-converted (pp-conv-specs specs)
        bindings      (repeating-qslot-bindings pre-converted)]
    (->> (replace-bindable bindings pre-converted)
         (mapcat parse-conv-spec) vec
         (prepend-qslot-bindings bindings))))

(defmacro <<-*
  "Same as `<<-` but its last argument should be a sequence to be concatenated with the
  results without any pre-processing."
  {:see-also ["<<-"]}
  ([spec]
   `(<<- ~spec))
  ([spec & specs]
   `(into (or (<<- ~@(cons spec (butlast specs))) []) ~(last specs))))

(defn simple->
  [table m]
  (let [table (some-str table)]
    (map/map-vals-by-kv #(-> (keyword table (name %1)) %2) m)))

(defn map->
  ([m]
   (map/map-vals-by-kv -> m))
  ([table m]
   (let [table (some-str table)]
     (map/map-vals-by-kv
      #(-> (if (namespace %1) %1 (keyword table (name %1))) %2)
      m))))

(defmacro defcoercions
  "Defines input and output coercions for a database table `table`. The `specs` should
  be an argument list consisting of triples in a form of `column-name`,
  `input-coercer`, `output-coercer`.

  For each definition 4 multimethod implementations will be emitted, identified by a
  keyword having a namespace the same as the given table, and a name the same as
  currently processed column name, both in 2 variants: one for snake, and one for
  lisp case. Two multimethod definitions will be created for
  `ferment.db/in-coercer` and two for `ferment.db/out-coercer`.

  If the given coercer is a literal `nil` or `false` value, it will be marked as
  undefined using `false` value. It is advised to use that approach instead of
  assigning `identity` function to express that coercion is not needed since it can
  cause compile-time calculations to short-circuit instead of generating fallback
  code.

  Example:

  `(defcoercions :users :some-identifier str keyword)`

  The above will expand the following code:

  ```
  (defmethod ferment.db/in-coercer  :users/some-identifier [_] str)
  (defmethod ferment.db/in-coercer  :users/some_identifier [_] str)
  (defmethod ferment.db/out-coercer :users/some-identifier [_] keyword)
  (defmethod ferment.db/out-coercer :users/some_identifier [_] keyword)
  ```

  This will allow specialized database coercion functions to transformed values which
  are exchanged with a database.

  Optionally coercions can be defined without a table name. In such case the table
  should be set to either `nil`, `false` or `:ferment.db/any`."
  [table & specs]
  (let [t# (if (identical? table ::any) nil (some-str table))]
    `(do
       ~@(mapcat
          (fn [[c# in# out#]]
            `((defmethod  in-coercer ~(sql/colspec-kw    t# c#) [~'_] (or ~in#  false))
              (defmethod  in-coercer ~(sql/make-kw-snake t# c#) [~'_] (or ~in#  false))
              (defmethod out-coercer ~(sql/colspec-kw    t# c#) [~'_] (or ~out# false))
              (defmethod out-coercer ~(sql/make-kw-snake t# c#) [~'_] (or ~out# false))))
          (partition 3 specs))
       nil)))

(defn- get-db-out-coercer
  "Gets database output coercer on a basis of table name and column label from a result
  set metadata object (`rsm`) and index number (`i`). If there is no coercer found
  and column label differs from column name, tries with table and column
  name. Returns a function (coercer found), `false` (coercer found but value should
  remain as-is) or `nil` (coercer not found)."
  [^ResultSetMetaData rsm ^Integer i]
  (let [tab-name  (.getTableName   rsm i)
        col-label (.getColumnLabel rsm i)]
    (if-some [coercer-fn (get-out-coercer* (sql/make-kw-simple tab-name col-label))]
      coercer-fn
      (let [col-name (.getColumnName rsm i)]
        (when (not= col-label col-name)
          (get-out-coercer* (sql/make-kw-simple tab-name col-name)))))))

(defn- delayed-column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set or is different from label but no coercer has been found), and
  calls output coercer obtained using `ferment.db/out-coercer`. Each result is
  wrapped in a Delay object unless it does not require coercion."
  [builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)
        v                      (.getObject rs i)
        coercer-fn             (get-db-out-coercer rsm i)]
    (rs/read-column-by-index (if coercer-fn (delay (coercer-fn v)) v) rsm i)))

(defn- column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set or is different from label but no coercer has been found), and
  calls output coercer using `ferment.db/->` passing to it the mentioned names and
  a value."
  [builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)
        v                      (.getObject rs i)
        coercer-fn             (get-db-out-coercer rsm i)]
    (rs/read-column-by-index (if coercer-fn (coercer-fn v) v) rsm i)))

(defn gen-builder
  "Generates result set builder on a basis of the given builder `rs-builder`. Uses
  `ferment.db/column-by-index-fn` to coerce the results."
  [rs-builder]
  (rs/builder-adapter rs-builder column-by-index-fn))

(defn gen-builder-delayed
  "Generates result set builder on a basis of the given builder `rs-builder`. Uses
  `ferment.db/delayed-column-by-index-fn` to coerce the results."
  [rs-builder]
  (rs/builder-adapter rs-builder delayed-column-by-index-fn))

;; Predefined database options

(def opts-map               (update db/opts-map         :builder-fn gen-builder))
(def opts-simple-map        (update db/opts-simple-map  :builder-fn gen-builder))
(def opts-vec               (update db/opts-vec         :builder-fn gen-builder))
(def opts-simple-vec        (update db/opts-simple-vec  :builder-fn gen-builder))
(def opts-slashed-map       (update db/opts-slashed-map :builder-fn gen-builder))
(def opts-slashed-vec       (update db/opts-slashed-vec :builder-fn gen-builder))
(def opts-lazy-vec          (update db/opts-vec         :builder-fn gen-builder-delayed))
(def opts-lazy-simple-vec   (update db/opts-simple-vec  :builder-fn gen-builder-delayed))
(def opts-lazy-slashed-vec  (update db/opts-slashed-vec :builder-fn gen-builder-delayed))
(def opts-lazy-map          (update db/opts-map         :builder-fn gen-builder-delayed))
(def opts-lazy-simple-map   (update db/opts-simple-map  :builder-fn gen-builder-delayed))
(def opts-lazy-slashed-map  (update db/opts-slashed-map :builder-fn gen-builder-delayed))

;; Query params

(defmacro <q
  "Simple wrapper around `ferment.db.sql/build-query` and `<<-` macros. First
  argument should be a query (possibly grouped with a vector, if multiple arguments
  need to be passed), all other arguments are passed to `<<-`.

  Produces a sequence suitable to be used with `execute-*` family of functions (a
  parameterized query as its first element and coerced query parameters as other
  elements).

  Example:

  ```
  (<q [\"select %(id) from %[u] where %(id) = ? AND %(t) = ?\"
       {:id :users/id, :u :users/id, :t :users/account-type}]
      [:users [:id \"42\" :account-type :user]])
  ```

  The above will return:

  ```
  (\"select `id` from `users` where `id` = ? AND `account_type` = ?\" 42 \"user\")
  ```

  Note that `\"42\"` and `:user` are values which are to be coerced (first with a
  coercer registered for `:users/id` and second with a coercer registered for
  `:users/account-type`). After the coercion resulting values (`42` and `\"user\"`)
  are placed in a sequence to become query parameters."
  [query & params]
  `(cons (sql/build-query ~query) (<<- ~@params)))

(defmacro <dq
  "Simple wrapper around `ferment.db.sql/build-query-dynamic` and `<<-` macros. First
  argument should be a query (possibly grouped with a vector, if multiple arguments
  need to be passed), all other arguments are passed to `<<-`.

  Produces a sequence suitable to be used with `execute-*` family of functions (a
  parameterized query as its first element and coerced query parameters as other
  elements).

  Intended to be used for dynamically generated database queries. Uses a bit slower
  but safer FIFO cache of default size (about 150k items).

  Example:

  ```
  (<q [\"select %(id) from %[u] where %(id) = ? AND %(t) = ?\"
       {:id :users/id, :u :users/id, :t :users/account-type}]
      [:users [:id \"42\"] [:account-type :user]])
  ```

  The above will return:

  ```
  (\"select `id` from `users` where `id` = ? AND `account_type` = ?\" 42 \"user\")
  ```

  Note that `\"42\"` and `:user` are values which are to be coerced (first with a
  coercer registered for `:users/id` and second with a coercer registered for
  `:users/account-type`). After the coercion resulting values (`42` and `\"user\"`)
  are placed in a sequence to become query parameters."
  [query & params]
  `(cons (sql/build-query-dynamic ~query) (<<- ~@params)))

(defmacro <d-do!
  "Calls function `f`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `ferment.db.sql/build-query-dynamic`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [f db query & params]
  `(~f ~db (cons (sql/build-query-dynamic ~query) (<<- ~@params))))

(defmacro <d-exec!
  "Calls `execute!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `ferment.db.sql/build-query-dynamic`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute! ~db (cons (sql/build-query-dynamic ~query) (<<- ~@params))))

(defmacro <d-exec-one!
  "Calls `execute-one!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `ferment.db.sql/build-query-dynamic`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute-one! ~db (cons (sql/build-query-dynamic ~query) (<<- ~@params))))

(defmacro <do!
  "Calls function `f` passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `ferment.db.sql/build-query`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [f db query & params]
  `(~f ~db (cons (sql/build-query ~query) (<<- ~@params))))

(defmacro <exec!
  "Calls `execute!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `ferment.db.sql/build-query`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute! ~db (cons (sql/build-query ~query) (<<- ~@params))))

(defmacro <exec-one!
  "Calls `execute-one!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `ferment.db.sql/build-query`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute-one! ~db (cons (sql/build-query ~query) (<<- ~@params))))

;; Main wrappers

(defn lazy-execute-one!
  ([connectable sql-params opts]
   (let [m (jdbc/execute-one! connectable sql-params (conj opts-lazy-simple-map opts))]
     (if m (map/to-lazy m) m)))
  ([connectable sql-params]
   (lazy-execute-one! connectable sql-params nil)))

(defn lazy-execute!
  ([connectable sql-params opts]
   (when-some [coll (jdbc/execute! connectable sql-params (conj opts-lazy-simple-map opts))]
     (mapv #(if % (map/to-lazy %) %) coll)))
  ([connectable sql-params]
   (lazy-execute! connectable sql-params nil)))

(defn execute-one!
  ([connectable sql-params opts]
   (jdbc/execute-one! connectable sql-params (conj opts-simple-map opts)))
  ([connectable sql-params]
   (execute-one! connectable sql-params nil)))

(defn execute!
  ([connectable sql-params opts]
   (jdbc/execute! connectable sql-params (conj opts-simple-map opts)))
  ([connectable sql-params]
   (execute! connectable sql-params nil)))

(defmacro lazy-do
  [& cmd]
  `(let [r# ~@cmd] (if r# (map/to-lazy r#) r#)))

(defn lazy-get-by-id
  "Like `next.jdbc/get-by-id` but supports lazy maps."
  ([connectable table pk]
   (lazy-get-by-id connectable table pk :id {}))
  ([connectable table pk opts]
   (lazy-get-by-id connectable table pk :id opts))
  ([connectable table pk pk-name opts]
   (let [opts (conj (or (:options connectable) {}) opts-lazy-simple-map opts)]
     (lazy-do (next-sql/get-by-id connectable table pk pk-name opts)))))

;; Abstract getters and setters

(defn make-getter
  ([f opts id-col cols]
   (make-getter f opts nil id-col cols nil))
  ([f opts table id-col cols]
   (make-getter f opts table id-col cols nil))
  ([f opts table id-col cols getter-coll-fn]
   (let [id-col (keyword id-col)
         cols   (if (map?  cols) (keys cols) cols)
         cols   (if (coll? cols) (seq  cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (sql/to-snake-simple table)
         q      (str-spc "SELECT" (sql/join-col-names cols)
                         "FROM"   (or table "?")
                         "WHERE"  (sql/to-snake-simple id-col) "= ?")]
     (if table
       (if getter-coll-fn
         (fn db-lazy-getter
           ([db id]          (db-lazy-getter db nil id))
           ([db _ id]        (f db [q id] opts))
           ([db _ id & more] (getter-coll-fn db (cons id more))))
         (fn [db _ id]
           (f db [q id] opts)))
       (if getter-coll-fn
         (fn
           ([db table id]        (f db [q (sql/to-snake-simple table) id] opts))
           ([db table id & more] (getter-coll-fn db table (cons id more))))
         (fn [db table id]
           (f db [q (sql/to-snake-simple table) id] opts)))))))

(defn make-getter-coll
  "Creates a database getter suitable for use with `get-cached-coll-` family of
  functions. The returned function should accept an argument containing multiple
  identifiers."
  ([f opts id-col]
   (make-getter-coll f opts nil id-col nil))
  ([f opts id-col cols]
   (make-getter-coll f opts nil id-col cols))
  ([f opts table id-col cols]
   (let [id-col (keyword id-col)
         cols   (if (map? cols) (keys cols) cols)
         cols   (if (coll? cols) (seq cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (sql/to-snake-simple table)
         q      (str-spc "SELECT" (sql/join-col-names cols)
                         "FROM"   (or table "?")
                         "WHERE"  (sql/to-snake-simple id-col)
                         "IN (")]
     (if table
       (fn db-getter-coll
         ([db ids]   (db-getter-coll db nil ids))
         ([db _ ids] (if-some [ids (seq ids)]
                       (let [query (str q (sql/join-? ids) ")")]
                         (->> (f db (cons query ids) opts)
                              (reduce #(qassoc %1 (get %2 id-col) %2) {}))))))
       (fn [db table ids]
         (if-some [ids (seq ids)]
           (let [ids   (map id-to-db ids)
                 table (sql/to-snake-simple table)
                 query (str q (sql/join-? ids) ")")]
             (->> (f db (cons query (cons table ids)) opts)
                  (reduce #(qassoc %1 (get %2 id-col) %2) {})))))))))

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
   (if v
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
  [mig-key]
  (let [migrators (get app/state mig-key)
        mig-dbs   (set (migration-databases migrators))]
    {:migrators? (some? (seq migrators))
     :dbs-up     mig-dbs
     :props-up   (set (map #(get-in app/post-config [%1 :properties :key]) mig-dbs))}))

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
   (let [mig-key      (migrators-key opts)
         state-pre    (migrators-state mig-key)
         start-admin! (get opts :fn/start-admin app/start-admin!)]
     (if-not (:migrators? state-pre) (start-admin! mig-key))
     (if (fn? opts)
       (ragtime-repl/migrate (opts))
       (doseq [mconfig (get app/state mig-key)]
         (let [config (merge (mconfig) opts)
               dbname (db-name config)
               dbkey  (db-key-name config)]
           (if (pos-int? (::jdbc/update-count (first (try-initialize-db config))))
             (log/msg "Created empty database" dbname (str "(" dbkey ")")))
           (ragtime-repl/migrate config))))
     (if-not (:migrators? state-pre)
       (let [state-post (migrators-state mig-key)
             stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                (set/difference (:props-up state-post) (:props-up state-pre)))]
         (apply app/stop! mig-key (filter identity stop-keys)))))
   nil))

(defn rollback!
  "Rolls back all databases or a database specified by a migrator function passed as an
  argument. Optional map of options can be passed which will be merged with each
  migration options. If a value is passed instead of a map or a function it will be
  used as an additional argument meaning a number of migrations or a migration ID."
  ([]
   (rollback! nil))
  ([opts]
   (let [mig-key      (migrators-key opts)
         state-pre    (migrators-state mig-key)
         start-admin! (get opts :fn/start-admin app/start-admin!)]
     (if-not (:migrators? state-pre) (start-admin! mig-key))
     (if (fn? opts)
       (ragtime-repl/rollback (opts))
       (if (or (not opts) (map? opts))
         (doseq [migrator (get app/state mig-key)] (ragtime-repl/rollback (merge (migrator) opts)))
         (doseq [migrator (get app/state mig-key)] (ragtime-repl/rollback (migrator) opts))))
     (if-not (:migrators? state-pre)
       (let [state-post (migrators-state mig-key)
             stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                (set/difference (:props-up state-post) (:props-up state-pre)))]
         (apply app/stop! mig-key (filter identity stop-keys))))))
  ([opts amount-or-id]
   (let [mig-key      (migrators-key opts)
         state-pre    (migrators-state mig-key)
         start-admin! (get opts :fn/start-admin app/start-admin!)]
     (if-not (:migrators? state-pre) (start-admin! mig-key))
     (if (fn? opts)
       (ragtime-repl/rollback (opts) amount-or-id)
       (doseq [migrator (get app/state mig-key)] (ragtime-repl/rollback (merge (migrator) opts) amount-or-id)))
     (if-not (:migrators? state-pre)
       (let [state-post (migrators-state mig-key)
             stop-keys  (concat (set/difference (:dbs-up   state-post) (:dbs-up   state-pre))
                                (set/difference (:props-up state-post) (:props-up state-pre)))]
         (apply app/stop! mig-key (filter identity stop-keys)))))
   nil))

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

(defcoercions ::any
  :identity          identity-to-db               identity/of
  :email             email-to-db                  some-str
  :phone             phone-to-db                  identity/preparse-phone
  :account-type      some-str                     some-keyword
  :first-name        some-str                     some-str
  :middle-name       some-str                     some-str
  :last-name         some-str                     some-str
  :ip                ip/to-address                ip/string-to-address
  :ip-address        ip/to-address                ip/string-to-address
  :password-suite-id safe-parse-long              long-or-nil
  :password          nil                          nil)
