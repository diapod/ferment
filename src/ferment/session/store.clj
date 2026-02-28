(ns

    ^{:doc    "Session store branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.session.store

  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [io.randomseed.utils.db :as udb]
            [ferment.system :as system])

  (:import (java.time Instant)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(def ^:private table-name-pattern
  #"^[A-Za-z0-9_]+$")

(def ^:private db-row-opts
  {:builder-fn rs/as-unqualified-lower-maps})

(defn now-iso
  []
  (str (Instant/now)))

(defn now-ms
  []
  (System/currentTimeMillis))

(defn normalize-session-id
  [sid]
  (or (cond
        (string? sid)  (some-> sid str/trim not-empty)
        (keyword? sid) (some-> sid name str/trim not-empty)
        (uuid? sid)    (str sid)
        (nil? sid)     nil
        :else          (some-> sid str str/trim not-empty))
      (str (UUID/randomUUID))))

(defn new-session
  [sid opts]
  (let [sid' (normalize-session-id sid)
        ts   (now-iso)]
    (merge {:session/id             sid'
            :session/version        0
            :session/state          :warm
            :session/frozen?        true
            :session/created-at     ts
            :session/updated-at     ts
            :session/last-access-at ts
            :session/turns          []
            :session/facts          {}}
           (if (map? opts) opts {}))))

(defn- revise-session
  [session]
  (let [ts (now-iso)]
    (-> session
        (update :session/version (fnil inc 0))
        (assoc :session/updated-at ts
               :session/last-access-at ts))))

(defn- backend-keyword
  [v]
  (some-> v name str/lower-case keyword))

(defn- normalize-table-name
  [v fallback]
  (let [t (cond
            (keyword? v) (name v)
            (string? v)  v
            :else        nil)
        t (some-> t str/trim not-empty)]
    (or t fallback)))

(defn- safe-table-name
  [v fallback]
  (let [t (normalize-table-name v fallback)]
    (when-not (and t (re-matches table-name-pattern t))
      (throw (ex-info "Invalid SQL table name in session store config."
                      {:table v
                       :fallback fallback
                       :table-name-pattern table-name-pattern})))
    t))

(defn- truthy-db-bool?
  [v]
  (cond
    (nil? v) false
    (boolean? v) v
    (number? v) (not (zero? (long v)))
    (string? v) (contains? #{"1" "true" "t" "yes" "y"}
                           (str/lower-case (str/trim v)))
    :else (boolean v)))

(defn- json-encode
  [v]
  (when (some? v)
    (json/generate-string v)))

(defn- json-decode
  [v default]
  (if (or (nil? v) (and (string? v) (str/blank? v)))
    default
    (try
      (json/parse-string (str v) true)
      (catch Throwable _
        default))))

(defn- state-keyword
  [v]
  (if-let [s (some-> v str str/trim not-empty)]
    (keyword s)
    :warm))

(declare ensure-session!
         db-store?
         get-session
         update-session!)

(def ^:private default-session-vars-contract
  {:keys/require-qualified?  false
   :keys/allowed-namespaces  #{}
   :ttl/default-ms           nil
   :ttl/max-ms               604800000
   :freeze/allow-write?      true
   :freeze/allow-delete?     true
   :limits/max-vars          256
   :limits/max-key-chars     191
   :limits/max-value-bytes   131072
   :policy/default           {}
   :policy/by-intent         {}
   :policy/by-operation      {}
   :class/default            :session.vars/default
   :class/by-namespace       {}
   :class/policy             {}
   :request/default-bindings {}})

(def ^:private session-var-value-key :session.var/value)
(def ^:private session-var-meta-key  :session.var/meta)

(defn- positive-int
  [v default]
  (let [n (cond
            (integer? v) v
            (number? v)  (long v)
            (string? v)  (try
                           (Long/parseLong (str/trim v))
                           (catch Throwable _ nil))
            :else nil)]
    (if (and (integer? n) (pos? n))
      n
      default)))

(defn- non-negative-int-or-nil
  [v]
  (when (some? v)
    (let [n (cond
              (integer? v) v
              (number? v)  (long v)
              (string? v)  (try
                             (Long/parseLong (str/trim v))
                             (catch Throwable _ nil))
              :else nil)]
      (when (and (integer? n) (not (neg? n)))
        n))))

(defn- truthy?
  [v]
  (cond
    (nil? v) false
    (boolean? v) v
    (number? v) (not (zero? (long v)))
    (string? v) (contains? #{"1" "true" "t" "yes" "y" "on"}
                           (-> v str/trim str/lower-case))
    :else (boolean v)))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- parse-var-keyword
  [k]
  (cond
    (keyword? k) k
    (string? k) (let [s (some-> k str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- normalize-key-namespace
  [v]
  (cond
    (keyword? v) (or (namespace v)
                     (some-> v name str/trim not-empty))
    (string? v)  (some-> v str/trim not-empty)
    :else nil))

(defn- normalize-key-namespace-set
  [v]
  (let [src (cond
              (set? v) v
              (sequential? v) v
              (nil? v) nil
              :else [v])]
    (if (seq src)
      (into #{} (keep normalize-key-namespace) src)
      #{})))

(defn- normalize-class-policy-entry
  [entry]
  (let [entry' (if (map? entry) entry {})
        default-ttl? (contains? entry' :ttl/default-ms)
        max-ttl? (contains? entry' :ttl/max-ms)
        write? (contains? entry' :freeze/allow-write?)
        delete? (contains? entry' :freeze/allow-delete?)
        default-ttl (when default-ttl?
                      (non-negative-int-or-nil (:ttl/default-ms entry')))
        max-ttl (when max-ttl?
                  (positive-int (:ttl/max-ms entry') nil))
        default-ttl' (when (some? default-ttl)
                       (if (some? max-ttl)
                         (min default-ttl max-ttl)
                         default-ttl))]
    (cond-> {}
      default-ttl? (assoc :ttl/default-ms default-ttl')
      max-ttl? (assoc :ttl/max-ms max-ttl)
      write? (assoc :freeze/allow-write? (truthy? (:freeze/allow-write? entry')))
      delete? (assoc :freeze/allow-delete? (truthy? (:freeze/allow-delete? entry'))))))

(defn- normalize-class-policy-map
  [m]
  (if (map? m)
    (reduce-kv (fn [acc k v]
                 (if-some [class-id (keywordish k)]
                   (assoc acc class-id (normalize-class-policy-entry v))
                   acc))
               {}
               m)
    {}))

(defn- normalize-class-by-namespace
  [m]
  (if (map? m)
    (reduce-kv (fn [acc ns' class-id]
                 (if-let [ns* (normalize-key-namespace ns')]
                   (if-some [class* (keywordish class-id)]
                     (assoc acc ns* class*)
                     acc)
                   acc))
               {}
               m)
    {}))

(defn- normalize-target-path
  [v]
  (let [src (cond
              (vector? v) v
              (sequential? v) (vec v)
              (keyword? v) [v]
              (string? v) (some-> (keywordish v) vector)
              :else nil)
        path (when (seq src)
               (->> src (keep keywordish) vec))]
    (when (seq path)
      path)))

(defn- normalize-request-default-binding-entry
  [entry]
  (let [entry' (cond
                 (map? entry) entry
                 :else {:target entry})
        target (normalize-target-path (:target entry'))
        coerce (some-> (:coerce entry') keywordish)]
    (when (seq target)
      (cond-> {:target target}
        (keyword? coerce) (assoc :coerce coerce)))))

(defn- normalize-request-default-bindings
  [m]
  (if (map? m)
    (reduce-kv (fn [acc k v]
                 (if-some [k' (parse-var-keyword k)]
                   (if-some [binding (normalize-request-default-binding-entry v)]
                     (assoc acc k' binding)
                     acc)
                   acc))
               {}
               m)
    {}))

(defn- normalize-vars-policy-entry
  [entry]
  (let [entry' (if (map? entry) entry {})
        read? (contains? entry' :read-namespaces)
        write? (contains? entry' :write-namespaces)
        delete? (contains? entry' :delete-namespaces)]
    (cond-> {}
      read? (assoc :read-namespaces
                   (normalize-key-namespace-set (:read-namespaces entry')))
      write? (assoc :write-namespaces
                    (normalize-key-namespace-set (:write-namespaces entry')))
      delete? (assoc :delete-namespaces
                     (normalize-key-namespace-set (:delete-namespaces entry'))))))

(defn- normalize-vars-policy-map
  [m]
  (if (map? m)
    (reduce-kv (fn [acc k v]
                 (if-some [k' (keywordish k)]
                   (assoc acc k' (normalize-vars-policy-entry v))
                   acc))
               {}
               m)
    {}))

(def ^:private contract-absent ::contract-absent)

(defn- resolve-contract-value
  [src k default parse-fn]
  (let [raw (if (contains? src k)
              (get src k)
              contract-absent)]
    (if (identical? raw contract-absent)
      default
      (parse-fn raw))))

(defn- normalize-session-vars-contract
  [cfg]
  (let [src (if (map? (:session-vars/contract cfg))
              (:session-vars/contract cfg)
              {})
        defaults default-session-vars-contract
        require-qualified? (resolve-contract-value src
                                                  :keys/require-qualified?
                                                  (:keys/require-qualified? defaults)
                                                  truthy?)
        allowed-namespaces (resolve-contract-value src
                                                  :keys/allowed-namespaces
                                                  (:keys/allowed-namespaces defaults)
                                                  normalize-key-namespace-set)
        default-ttl-ms (resolve-contract-value src
                                              :ttl/default-ms
                                              (:ttl/default-ms defaults)
                                              non-negative-int-or-nil)
        max-ttl-ms (resolve-contract-value src
                                          :ttl/max-ms
                                          (:ttl/max-ms defaults)
                                          #(positive-int % (:ttl/max-ms defaults)))
        allow-write? (resolve-contract-value src
                                            :freeze/allow-write?
                                            (:freeze/allow-write? defaults)
                                            truthy?)
        allow-delete? (resolve-contract-value src
                                             :freeze/allow-delete?
                                             (:freeze/allow-delete? defaults)
                                             truthy?)
        max-vars (resolve-contract-value src
                                        :limits/max-vars
                                        (:limits/max-vars defaults)
                                        #(positive-int % (:limits/max-vars defaults)))
        max-key-chars (resolve-contract-value src
                                             :limits/max-key-chars
                                             (:limits/max-key-chars defaults)
                                             #(positive-int % (:limits/max-key-chars defaults)))
        max-value-bytes (resolve-contract-value src
                                               :limits/max-value-bytes
                                               (:limits/max-value-bytes defaults)
                                               #(positive-int % (:limits/max-value-bytes defaults)))
        policy-default (resolve-contract-value src
                                              :policy/default
                                              (:policy/default defaults)
                                              normalize-vars-policy-entry)
        policy-by-intent (resolve-contract-value src
                                                :policy/by-intent
                                                (:policy/by-intent defaults)
                                                normalize-vars-policy-map)
        policy-by-operation (resolve-contract-value src
                                                   :policy/by-operation
                                                   (:policy/by-operation defaults)
                                                   normalize-vars-policy-map)
        class-default (resolve-contract-value src
                                             :class/default
                                             (:class/default defaults)
                                             #(or (keywordish %) (:class/default defaults)))
        class-by-namespace (resolve-contract-value src
                                                  :class/by-namespace
                                                  (:class/by-namespace defaults)
                                                  normalize-class-by-namespace)
        class-policy (resolve-contract-value src
                                            :class/policy
                                            (:class/policy defaults)
                                            normalize-class-policy-map)
        request-default-bindings (resolve-contract-value src
                                                        :request/default-bindings
                                                        (:request/default-bindings defaults)
                                                        normalize-request-default-bindings)
        default-ttl-ms' (when (some? default-ttl-ms)
                          (if (pos? max-ttl-ms)
                            (min default-ttl-ms max-ttl-ms)
                            default-ttl-ms))]
    {:keys/require-qualified? require-qualified?
     :keys/allowed-namespaces allowed-namespaces
     :ttl/default-ms default-ttl-ms'
     :ttl/max-ms max-ttl-ms
     :freeze/allow-write? allow-write?
     :freeze/allow-delete? allow-delete?
     :limits/max-vars max-vars
     :limits/max-key-chars max-key-chars
     :limits/max-value-bytes max-value-bytes
     :policy/default policy-default
     :policy/by-intent policy-by-intent
     :policy/by-operation policy-by-operation
     :class/default class-default
     :class/by-namespace class-by-namespace
     :class/policy class-policy
     :request/default-bindings request-default-bindings}))

(defn- parse-var-key
  [k]
  (parse-var-keyword k))

(defn- var-key-text
  [k]
  (if-let [ns' (namespace k)]
    (str ns' "/" (name k))
    (name k)))

(defn- validate-var-key!
  [store sid key]
  (let [contract (or (:session-vars/contract store) default-session-vars-contract)
        key' (or (parse-var-key key)
                 (throw (ex-info "Session var key must be a keyword or keyword-like string."
                                 {:error :session.vars/invalid-key
                                  :session/id sid
                                  :key key})))
        key-text (var-key-text key')
        key-ns (namespace key')
        allowed-namespaces (:keys/allowed-namespaces contract)
        max-key-chars (or (:limits/max-key-chars contract) 191)]
    (when (and (:keys/require-qualified? contract)
               (not key-ns))
      (throw (ex-info "Session var key must be namespace-qualified."
                      {:error :session.vars/key-namespace-required
                       :session/id sid
                       :key key'
                       :contract (select-keys contract
                                              [:keys/require-qualified?
                                               :keys/allowed-namespaces])})))
    (when (and (seq allowed-namespaces)
               (not (contains? allowed-namespaces
                               (or key-ns (name key')))))
      (throw (ex-info "Session var key namespace is not allowed by contract."
                      {:error :session.vars/key-namespace-forbidden
                       :session/id sid
                       :key key'
                       :namespace key-ns
                       :allowed-namespaces allowed-namespaces})))
    (when (> (count key-text) max-key-chars)
      (throw (ex-info "Session var key is too long."
                      {:error :session.vars/key-too-long
                       :session/id sid
                       :key key'
                       :key-length (count key-text)
                       :max-key-chars max-key-chars})))
    key'))

(defn- vars-policy-for
  [store opts]
  (let [contract (or (:session-vars/contract store) default-session-vars-contract)
        opts' (if (map? opts) opts {})
        intent (some-> (:intent opts') keywordish)
        operation (some-> (:operation opts') keywordish)]
    (merge (:policy/default contract)
           (when (keyword? operation)
             (get-in contract [:policy/by-operation operation]))
           (when (keyword? intent)
             (get-in contract [:policy/by-intent intent])))))

(defn- mode->policy-key
  [mode]
  (case mode
    :read :read-namespaces
    :put  :write-namespaces
    :del  :delete-namespaces
    nil))

(defn- mode->policy-error
  [mode]
  (case mode
    :read :session.vars/policy-read-forbidden
    :put  :session.vars/policy-write-forbidden
    :del  :session.vars/policy-delete-forbidden
    :session.vars/policy-forbidden))

(defn- ensure-policy-permitted!
  [store sid key mode opts]
  (let [k' (validate-var-key! store sid key)
        ns' (or (namespace k') (name k'))
        policy (vars-policy-for store opts)
        policy-key (mode->policy-key mode)
        has-policy? (contains? policy policy-key)
        allowed (get policy policy-key)]
    (when (and has-policy?
               (not (contains? allowed ns')))
      (throw (ex-info "Session var access is forbidden by policy."
                      {:error (mode->policy-error mode)
                       :session/id sid
                       :key k'
                       :namespace ns'
                       :mode mode
                       :intent (some-> opts :intent keywordish)
                       :operation (some-> opts :operation keywordish)
                       :allowed-namespaces allowed})))
    k'))

(defn- var-class-id
  [contract k]
  (let [ns' (or (namespace k) (name k))]
    (or (get-in contract [:class/by-namespace ns'])
        (:class/default contract))))

(defn- class-policy-for
  [contract k]
  (let [class-id (var-class-id contract k)
        class-policy (if (keyword? class-id)
                       (get-in contract [:class/policy class-id])
                       nil)]
    {:class/id class-id
     :policy (if (map? class-policy) class-policy {})}))

(defn- mode->freeze-policy-key
  [mode]
  (case mode
    :put :freeze/allow-write?
    :del :freeze/allow-delete?
    nil))

(defn- freeze-allow?
  [contract mode k]
  (let [class-info (class-policy-for contract k)
        class-id (:class/id class-info)
        policy (:policy class-info)
        policy-key (mode->freeze-policy-key mode)
        class-defined? (contains? policy policy-key)
        class-allow? (when class-defined?
                       (truthy? (get policy policy-key)))
        global-allow? (case mode
                        :put (:freeze/allow-write? contract)
                        :del (:freeze/allow-delete? contract)
                        false)]
    {:class/id class-id
     :allow? (if class-defined?
               class-allow?
               (truthy? global-allow?))}))

(defn- ensure-write-permitted!
  ([store sid mode]
   (ensure-write-permitted! store sid mode nil))
  ([store sid mode keys]
   (let [contract (or (:session-vars/contract store) default-session-vars-contract)
         keys' (when (sequential? keys)
                 (->> keys
                      (keep parse-var-key)
                      distinct
                      vec))
         session (ensure-session! store sid nil)]
     (when (true? (:session/frozen? session))
       (if (seq keys')
         (let [decisions (mapv #(assoc (freeze-allow? contract mode %) :key %) keys')
               denied (->> decisions (remove :allow?) vec)]
           (when (seq denied)
             (throw (ex-info "Session vars are read-only while session is frozen."
                             {:error :session.vars/session-frozen
                              :session/id sid
                              :mode mode
                              :session/state (:session/state session)
                              :session/frozen? (:session/frozen? session)
                              :denied-keys (mapv :key denied)
                              :denied-classes (->> denied (map :class/id) distinct vec)}))))
         (let [allow? (case mode
                        :put (:freeze/allow-write? contract)
                        :del (:freeze/allow-delete? contract)
                        false)]
           (when-not (truthy? allow?)
             (throw (ex-info "Session vars are read-only while session is frozen."
                             {:error :session.vars/session-frozen
                              :session/id sid
                              :mode mode
                              :session/state (:session/state session)
                              :session/frozen? (:session/frozen? session)})))))))))

(defn- resolve-var-ttl-ms
  [store key opts]
  (let [contract (or (:session-vars/contract store) default-session-vars-contract)
        key' (or (parse-var-key key)
                 (parse-var-keyword key))
        class-policy (if (keyword? key')
                       (:policy (class-policy-for contract key'))
                       {})
        requested (if (map? opts)
                    (non-negative-int-or-nil (:ttl-ms opts))
                    nil)
        class-default-ttl-ms (non-negative-int-or-nil (:ttl/default-ms class-policy))
        ttl-ms (or requested
                   class-default-ttl-ms
                   (:ttl/default-ms contract))
        class-max-ttl-ms (positive-int (:ttl/max-ms class-policy) nil)
        max-ttl-ms (or class-max-ttl-ms
                       (:ttl/max-ms contract))]
    (when (some? ttl-ms)
      (if (some? max-ttl-ms)
        (min ttl-ms max-ttl-ms)
        ttl-ms))))

(defn- value-size-bytes
  [v]
  (let [^String serialized (pr-str v)]
    (alength (.getBytes serialized StandardCharsets/UTF_8))))

(defn- enforce-value-limit!
  [store sid key value]
  (let [contract (or (:session-vars/contract store) default-session-vars-contract)
        limit (or (:limits/max-value-bytes contract) 131072)
        size-bytes (value-size-bytes value)]
    (when (> size-bytes limit)
      (throw (ex-info "Session var value exceeds configured size limit."
                      {:error :session.vars/value-too-large
                       :session/id sid
                       :key key
                       :value-size-bytes size-bytes
                       :max-value-bytes limit})))))

(defn- make-var-entry
  [store key value opts]
  (let [set-ms (now-ms)
        key' (or (parse-var-key key) key)
        contract (or (:session-vars/contract store) default-session-vars-contract)
        class-id (when (keyword? key')
                   (var-class-id contract key'))
        ttl-ms (resolve-var-ttl-ms store key' opts)
        expires-ms (when (some? ttl-ms)
                     (+ set-ms ttl-ms))]
    {session-var-value-key value
     session-var-meta-key
     (cond-> {:set/ms set-ms
              :set/at (str (Instant/ofEpochMilli set-ms))}
       (keyword? class-id) (assoc :class/id class-id)
       (some? ttl-ms) (assoc :ttl/ms ttl-ms)
       (some? expires-ms) (assoc :expires/ms expires-ms
                                 :expires/at (str (Instant/ofEpochMilli expires-ms))))}))

(defn- var-entry?
  [v]
  (and (map? v)
       (contains? v session-var-value-key)
       (map? (get v session-var-meta-key))))

(defn- normalize-var-entry
  [v]
  (if (var-entry? v)
    v
    {session-var-value-key v
     session-var-meta-key  {}}))

(defn- var-entry-expired?
  [entry now]
  (let [expires-ms (get-in entry [session-var-meta-key :expires/ms])]
    (and (some? expires-ms)
         (<= (long expires-ms) (long now)))))

(defn- var-entry-value
  [entry]
  (get entry session-var-value-key))

(defn- read-all-raw-vars
  [store sid]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      (let [result ((:var-getter store) (:db store) sid')]
        (if (map? result) result {}))
      (or (get-in (get-session store sid') [:session/facts]) {}))))

(defn- normalize-raw-vars-map
  [raw]
  (reduce-kv (fn [m k v]
               (if-let [k' (parse-var-key k)]
                 (assoc m k' v)
                 m))
             {}
             (if (map? raw) raw {})))

(defn- delete-raw-vars!
  [store sid ks]
  (let [sid' (normalize-session-id sid)
        ks'  (->> ks (keep identity) distinct vec)]
    (when (seq ks')
      (if (db-store? store)
        (apply (:var-deleter store) (:db store) sid' ks')
        (update-session! store sid'
                         (fn [session]
                           (update session :session/facts
                                   (fn [facts]
                                     (apply dissoc (or facts {}) ks')))))))
    nil))

(defn- expire-session-vars!
  [store sid]
  (let [raw (read-all-raw-vars store sid)
        now (now-ms)
        expired-keys
        (->> raw
             (keep (fn [[k v]]
                     (let [entry (normalize-var-entry v)]
                       (when (var-entry-expired? entry now)
                         k))))
             vec)]
    (when (seq expired-keys)
      (delete-raw-vars! store sid expired-keys))
    expired-keys))

(defn- enforce-vars-count-limit!
  [store sid existing-keys incoming-keys]
  (let [contract (or (:session-vars/contract store) default-session-vars-contract)
        max-vars (or (:limits/max-vars contract) 256)
        existing (set existing-keys)
        incoming (set incoming-keys)
        added-count (count (remove existing incoming))
        total (+ (count existing) added-count)]
    (when (> total max-vars)
      (throw (ex-info "Session vars count exceeds configured limit."
                      {:error :session.vars/limit-exceeded
                       :session/id sid
                       :max-vars max-vars
                       :existing-vars (count existing)
                       :incoming-vars (count incoming)
                       :result-vars total})))))

(defn- build-db-sql
  [sessions-table]
  {:select
   (str "SELECT `session_id`, `version`, `state`, `frozen`,"
        " `created_at`, `updated_at`, `last_access_at`, `frozen_at`, `thawed_at`,"
        " `summary`, `snapshot`, `meta`, `turns`, `facts`"
        " FROM `" sessions-table "`"
        " WHERE `session_id` = ? LIMIT 1")

   :list
   (str "SELECT `session_id`, `version`, `state`, `frozen`,"
        " `created_at`, `updated_at`, `last_access_at`, `frozen_at`, `thawed_at`,"
        " `summary`, `snapshot`, `meta`, `turns`, `facts`"
        " FROM `" sessions-table "`"
        " ORDER BY `updated_at` DESC")

   :delete
   (str "DELETE FROM `" sessions-table "` WHERE `session_id` = ?")

   :upsert
   (str "INSERT INTO `" sessions-table "`"
        " (`session_id`, `version`, `state`, `frozen`,"
        "  `created_at`, `updated_at`, `last_access_at`, `frozen_at`, `thawed_at`,"
        "  `summary`, `snapshot`, `meta`, `turns`, `facts`)"
        " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        " ON DUPLICATE KEY UPDATE"
        " `version` = VALUES(`version`),"
        " `state` = VALUES(`state`),"
        " `frozen` = VALUES(`frozen`),"
        " `created_at` = VALUES(`created_at`),"
        " `updated_at` = VALUES(`updated_at`),"
        " `last_access_at` = VALUES(`last_access_at`),"
        " `frozen_at` = VALUES(`frozen_at`),"
        " `thawed_at` = VALUES(`thawed_at`),"
        " `summary` = VALUES(`summary`),"
        " `snapshot` = VALUES(`snapshot`),"
        " `meta` = VALUES(`meta`),"
        " `turns` = VALUES(`turns`),"
        " `facts` = VALUES(`facts`)")})

(defn- session->db-values
  [session]
  (let [sid'           (normalize-session-id (:session/id session))
        ts             (now-iso)
        version        (long (or (:session/version session) 0))
        state          (or (some-> (:session/state session) name) "warm")
        frozen         (if (true? (:session/frozen? session)) 1 0)
        created-at     (or (:session/created-at session) ts)
        updated-at     (or (:session/updated-at session) ts)
        last-access-at (or (:session/last-access-at session) updated-at)
        frozen-at      (:session/frozen-at session)
        thawed-at      (:session/thawed-at session)
        summary        (json-encode (:session/summary session))
        snapshot       (json-encode (:session/snapshot session))
        meta           (json-encode (:session/meta session))
        turns          (json-encode (or (:session/turns session) []))
        facts          (json-encode (or (:session/facts session) {}))]
    [sid'
     version
     state
     frozen
     created-at
     updated-at
     last-access-at
     frozen-at
     thawed-at
     summary
     snapshot
     meta
     turns
     facts]))

(defn- row->session
  [row]
  (when-some [sid' (:session_id row)]
    (cond-> {:session/id             sid'
             :session/version        (long (or (:version row) 0))
             :session/state          (state-keyword (:state row))
             :session/frozen?        (truthy-db-bool? (:frozen row))
             :session/created-at     (:created_at row)
             :session/updated-at     (:updated_at row)
             :session/last-access-at (:last_access_at row)
             :session/turns          (json-decode (:turns row) [])
             :session/facts          (json-decode (:facts row) {})}
      (some? (:frozen_at row)) (assoc :session/frozen-at (:frozen_at row))
      (some? (:thawed_at row)) (assoc :session/thawed-at (:thawed_at row))
      (some? (:summary row))   (assoc :session/summary (json-decode (:summary row) nil))
      (some? (:snapshot row))  (assoc :session/snapshot (json-decode (:snapshot row) nil))
      (some? (:meta row))      (assoc :session/meta (json-decode (:meta row) nil)))))

(defn- memory-store?
  [store]
  (= :memory (:backend store)))

(defn- db-store?
  [store]
  (= :db (:backend store)))

(defn- datasource-of
  [v]
  (cond
    (nil? v) nil
    (instance? javax.sql.DataSource v) v
    (map? v) (or (:datasource v) (:datastore v) (:database v))
    :else nil))

(defn- db-select-session
  [store connectable sid']
  (some-> (jdbc/execute-one! connectable
                             [(:select (:sql store)) sid']
                             db-row-opts)
          row->session))

(defn- db-upsert-session!
  [store connectable session]
  (jdbc/execute-one! connectable
                     (into [(:upsert (:sql store))]
                           (session->db-values session)))
  nil)

(defn preconfigure-store
  [_k config]
  (let [cfg     (if (map? config) config {})
        backend (or (backend-keyword (:backend cfg)) :memory)
        cfg     (-> cfg
                    (assoc :backend backend)
                    (assoc :session-vars/contract (normalize-session-vars-contract cfg)))]
    (case backend
      :memory cfg
      :db     (-> cfg
                  (assoc :sessions-table (safe-table-name (:sessions-table cfg) "sessions"))
                  (assoc :vars-table (safe-table-name (:vars-table cfg) "session_vars")))
      (throw (ex-info "Unsupported session store backend."
                      {:backend backend
                       :supported #{:memory :db}})))))

(defn init-store
  [_k config]
  (let [cfg     (preconfigure-store _k config)
        backend (:backend cfg)]
    (case backend
      :memory (assoc cfg
                     :state (atom {})
                     :started-at (now-iso))
      :db     (let [datasource (datasource-of (:db cfg))]
                (when-not datasource
                  (throw (ex-info "Session store backend :db requires :db data source."
                                  {:backend backend
                                   :store-key _k
                                   :config cfg})))
                (assoc cfg
                       :db datasource
                       :var-getter (udb/make-setting-getter (:vars-table cfg) :session_id)
                       :var-setter (udb/make-setting-setter (:vars-table cfg) :session_id)
                       :var-deleter (udb/make-setting-deleter (:vars-table cfg) :session_id)
                       :sql (build-db-sql (:sessions-table cfg))
                       :started-at (now-iso)))
      (throw (ex-info "Unsupported session store backend."
                      {:backend backend
                       :supported #{:memory :db}})))))

(defn stop-store
  [_k _state]
  nil)

(defn request-default-bindings
  "Returns normalized request default bindings for session vars.

  Each entry maps a session var key to:
  - `:target` vector path in request map,
  - optional `:coerce` keyword."
  [store]
  (let [contract (or (:session-vars/contract store) default-session-vars-contract)
        bindings (:request/default-bindings contract)]
    (if (map? bindings)
      bindings
      {})))

(defn store-state
  [store]
  (:state store))

(defn get-session
  [store sid]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      (db-select-session store (:db store) sid')
      (get @(store-state store) sid'))))

(defn ensure-session!
  [store sid & [opts]]
  (let [sid'  (normalize-session-id sid)
        opts' (if (map? opts) opts {})]
    (if (db-store? store)
      (jdbc/with-transaction [tx (:db store)]
        (if-some [existing (db-select-session store tx sid')]
          existing
          (let [created (new-session sid' opts')]
            (db-upsert-session! store tx created)
            created)))
      (do
        (swap! (store-state store)
               (fn [sessions]
                 (if (contains? sessions sid')
                   sessions
                   (assoc sessions sid' (new-session sid' opts')))))
        (get-session store sid')))))

(defn put-session!
  [store session]
  (let [sid' (normalize-session-id (:session/id session))
        session' (-> (if (map? session)
                       session
                       (throw (ex-info "Session must be a map."
                                       {:session session})))
                     (assoc :session/id sid')
                     revise-session)]
    (if (db-store? store)
      (db-upsert-session! store (:db store) session')
      (swap! (store-state store) assoc sid' session'))
    session'))

(defn update-session!
  [store sid f & args]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      (jdbc/with-transaction [tx (:db store)]
        (let [current  (or (db-select-session store tx sid')
                           (new-session sid' {}))
              updated  (apply f current args)
              updated' (-> (if (map? updated)
                             updated
                             (throw (ex-info "Session update function must return a map."
                                             {:sid sid'
                                              :result updated})))
                           (assoc :session/id sid')
                           revise-session)]
          (db-upsert-session! store tx updated')
          updated'))
      (do
        (swap! (store-state store)
               (fn [sessions]
                 (let [current (or (get sessions sid')
                                   (new-session sid' {}))
                       updated (apply f current args)]
                   (assoc sessions sid' (revise-session updated)))))
        (get-session store sid')))))

(defn append-turn!
  [store sid turn]
  (let [turn' (cond-> {:turn/at (now-iso)}
                (map? turn) (merge turn))]
    (update-session! store sid
                     (fn [session]
                       (-> session
                           (update :session/turns (fnil conj []) turn')
                           (assoc :session/state :hot
                                  :session/frozen? false))))))

(defn- merge-session-meta
  [session opts]
  (let [meta' (when (map? (:session/meta opts))
                (:session/meta opts))]
    (if (map? meta')
      (update session :session/meta (fnil merge {}) meta')
      session)))

(defn freeze-session!
  [store sid opts]
  (let [opts' (if (map? opts) opts {})]
    (update-session! store sid
                     (fn [session]
                       (-> session
                           (merge-session-meta opts')
                           (merge (select-keys opts'
                                               [:session/summary
                                                :session/snapshot]))
                           (assoc :session/state :warm
                                  :session/frozen? true
                                  :session/frozen-at (now-iso)))))))

(defn thaw-session!
  [store sid opts]
  (let [opts' (if (map? opts) opts {})]
    (update-session! store sid
                     (fn [session]
                       (-> session
                           (merge-session-meta opts')
                           (merge (select-keys opts'
                                               [:session/summary
                                                :session/snapshot]))
                           (assoc :session/state :hot
                                  :session/frozen? false
                                  :session/thawed-at (now-iso)))))))

(defn delete-session!
  [store sid]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      (jdbc/execute-one! (:db store) [(:delete (:sql store)) sid'])
      (swap! (store-state store) dissoc sid'))
    nil))

(defn list-sessions
  [store]
  (if (db-store? store)
    (->> (jdbc/execute! (:db store) [(:list (:sql store))] db-row-opts)
         (map row->session)
         (filter some?)
         vec)
    (->> @(store-state store)
         vals
         (sort-by :session/updated-at)
         reverse
         vec)))

(defn get-var
  ([store sid k]
   (get-var store sid k nil))
  ([store sid k opts]
   (let [sid' (normalize-session-id sid)
         k'   (ensure-policy-permitted! store sid' k :read opts)]
     (expire-session-vars! store sid')
     (if-let [entry (some-> (get (normalize-raw-vars-map (read-all-raw-vars store sid')) k')
                            normalize-var-entry)]
       (if (var-entry-expired? entry (now-ms))
         (do
           (delete-raw-vars! store sid' [k'])
           nil)
         (var-entry-value entry))
       nil))))

(defn get-vars
  ([store sid ks]
   (get-vars store sid ks nil))
  ([store sid ks opts]
   (let [sid' (normalize-session-id sid)
         ks'  (when (sequential? ks)
                (->> ks
                     (map #(ensure-policy-permitted! store sid' % :read opts))
                     distinct
                     vec))]
     (if-not (seq ks')
       {}
       (do
         (expire-session-vars! store sid')
         (let [raw (normalize-raw-vars-map (read-all-raw-vars store sid'))
               now (now-ms)]
           (reduce (fn [m k]
                     (if-let [entry (some-> (get raw k) normalize-var-entry)]
                       (if (var-entry-expired? entry now)
                         (do
                           (delete-raw-vars! store sid' [k])
                           m)
                         (assoc m k (var-entry-value entry)))
                       m))
                   {}
                   ks')))))))

(defn put-var!
  ([store sid k v]
   (put-var! store sid k v nil))
  ([store sid k v opts]
   (let [sid' (normalize-session-id sid)
         opts' (if (map? opts) opts {})
         k' (ensure-policy-permitted! store sid' k :put opts')
         _ (ensure-write-permitted! store sid' :put [k'])
         _ (expire-session-vars! store sid')
         existing (keys (normalize-raw-vars-map (read-all-raw-vars store sid')))
         _ (enforce-vars-count-limit! store sid' existing [k'])
         _ (enforce-value-limit! store sid' k' v)
         entry (make-var-entry store k' v opts')]
     (if (db-store? store)
       ((:var-setter store) (:db store) sid' k' entry)
       (update-session! store sid'
                        (fn [session]
                          (update session :session/facts (fnil assoc {}) k' entry))))
     true)))

(defn put-vars!
  ([store sid kvs]
   (put-vars! store sid kvs nil))
  ([store sid kvs opts]
   (let [sid' (normalize-session-id sid)
         opts' (if (map? opts) opts {})
         kvs' (if (map? kvs) kvs {})
         normalized
         (reduce-kv (fn [m k v]
                      (let [k' (ensure-policy-permitted! store sid' k :put opts')]
                        (enforce-value-limit! store sid' k' v)
                        (assoc m k' v)))
                    {}
                    kvs')]
     (if-not (seq normalized)
       false
       (do
         (ensure-write-permitted! store sid' :put (keys normalized))
         (expire-session-vars! store sid')
         (let [existing (keys (normalize-raw-vars-map (read-all-raw-vars store sid')))
               incoming (keys normalized)
               _ (enforce-vars-count-limit! store sid' existing incoming)
               entries (reduce-kv (fn [m k v]
                                    (assoc m k (make-var-entry store k v opts')))
                                  {}
                                  normalized)]
           (if (db-store? store)
             (apply (:var-setter store) (:db store) sid' (mapcat identity entries))
             (update-session! store sid'
                              (fn [session]
                                (update session :session/facts (fnil merge {}) entries))))
           true))))))

(defn del-var!
  ([store sid k]
   (del-var! store sid k nil))
  ([store sid k opts]
   (let [sid' (normalize-session-id sid)
         k'   (ensure-policy-permitted! store sid' k :del opts)]
     (ensure-write-permitted! store sid' :del [k'])
     (expire-session-vars! store sid')
     (delete-raw-vars! store sid' [k'])
     true)))

(defn del-vars!
  ([store sid ks]
   (del-vars! store sid ks nil))
  ([store sid ks opts]
   (let [sid' (normalize-session-id sid)
         ks'  (when (sequential? ks)
                (->> ks
                     (map #(ensure-policy-permitted! store sid' % :del opts))
                     distinct
                     vec))]
     (if-not (seq ks')
       false
       (do
         (ensure-write-permitted! store sid' :del ks')
         (expire-session-vars! store sid')
         (delete-raw-vars! store sid' ks')
         true)))))

(defn del-all-vars!
  ([store sid]
   (del-all-vars! store sid nil))
  ([store sid opts]
   (let [sid' (normalize-session-id sid)]
     (expire-session-vars! store sid')
     (let [ks' (keys (normalize-raw-vars-map (read-all-raw-vars store sid')))]
       (doseq [k ks']
         (ensure-policy-permitted! store sid' k :del opts))
       (ensure-write-permitted! store sid' :del ks'))
     (if (db-store? store)
       ((:var-deleter store) (:db store) sid')
       (update-session! store sid'
                        (fn [session]
                          (assoc session :session/facts {}))))
     true)))

(derive ::store-value :ferment.system/value)
(derive :ferment.session.store/default ::store-value)

(system/add-expand ::store-value [k config] {k (preconfigure-store k config)})
(system/add-init   ::store-value [k config]    (init-store k config))
(system/add-halt!  ::store-value [k state]     (stop-store k state))
