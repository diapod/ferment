(ns

    ^{:doc    "Routing helpers for capability dispatch (role/model-key resolution)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.router

  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [ferment.system :as system]))

(def ^:private router-top-keys
  #{:routing
    :profiles
    :policy
    :defaults
    :intent->policy-profile
    :policy-profiles
    :artifact/version
    :versions
    :rollout})

(def ^:private routing-keys
  #{:intent->cap
    :cap->model-key
    :intent->default-model-key
    :cap->role
    :intent->role
    :intent->default-role
    :switch-on
    :retry
    :fallback
    :checks
    :gateway
    :policy})

(def ^:private retry-keys
  #{:same-cap-max :fallback-max})

(def ^:private gateway-strategies
  #{:latency-first :quality-first :cost-first})

(def ^:private gateway-keys
  #{:strategy
    :intent->strategy
    :ema-alpha
    :circuit-breaker
    :hedging})

(def ^:private gateway-breaker-keys
  #{:enabled?
    :min-samples
    :error-rate-open
    :cooldown-ms})

(def ^:private gateway-hedging-keys
  #{:enabled?
    :intent->enabled?
    :max-probes
    :delay-ms})

(def ^:private routing-default-keys
  #{:meta? :strict? :force? :on-error :policy/profile})

(def ^:private routing-on-error-modes
  #{:fail-open :fail-closed})

(defn- fail-router!
  [message data]
  (throw (ex-info message
                  (merge {:error :router/invalid-config}
                         (if (map? data) data {})))))

(defn- sorted-keys
  [xs]
  (vec (sort-by str xs)))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- deep-merge
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn- ensure-map!
  [v path]
  (when-not (map? v)
    (fail-router! "Router config value must be a map."
                  {:path path
                   :expected :map
                   :actual (some-> v class str)
                   :value v})))

(defn- ensure-keyword-map!
  [m path]
  (ensure-map! m path)
  (doseq [[k _] m]
    (when-not (keyword? k)
      (fail-router! "Router config map keys must be keywords."
                    {:path path
                     :expected :keyword-key
                     :key k}))))

(defn- ensure-only-keys!
  [m allowed path]
  (let [unknown (remove allowed (keys m))]
    (when (seq unknown)
      (fail-router! "Router config contains unsupported keys."
                    {:path path
                     :allowed (sorted-keys allowed)
                     :unknown (sorted-keys unknown)}))))

(defn- validate-keyword->keyword-map!
  [m path]
  (ensure-keyword-map! m path)
  (doseq [[k v] m]
    (when-not (keyword? v)
      (fail-router! "Router mapping values must be keywords."
                    {:path (conj path k)
                     :expected :keyword
                     :value v}))))

(defn- validate-keyword-coll!
  [v path]
  (when-not (or (set? v) (sequential? v))
    (fail-router! "Router config value must be a set or sequence of keywords."
                  {:path path
                   :expected :keyword-coll
                   :value v}))
  (doseq [entry v]
    (when-not (keyword? entry)
      (fail-router! "Router config collection entries must be keywords."
                    {:path path
                     :expected :keyword
                     :value entry}))))

(defn- validate-router-defaults!
  [defaults path]
  (ensure-keyword-map! defaults path)
  (ensure-only-keys! defaults routing-default-keys path)
  (doseq [k [:meta? :strict? :force?]]
    (when (contains? defaults k)
      (let [v (get defaults k)]
        (when-not (boolean? v)
          (fail-router! "Router defaults flags must be booleans."
                        {:path (conj path k)
                         :expected :boolean
                         :value v})))))
  (when (contains? defaults :on-error)
    (let [mode (:on-error defaults)]
      (when-not (contains? routing-on-error-modes mode)
        (fail-router! "Router defaults :on-error must be :fail-open or :fail-closed."
                      {:path (conj path :on-error)
                       :expected routing-on-error-modes
                       :value mode}))))
  (when (contains? defaults :policy/profile)
    (when-not (keyword? (:policy/profile defaults))
      (fail-router! "Router defaults :policy/profile must be a keyword."
                    {:path (conj path :policy/profile)
                     :expected :keyword
                     :value (:policy/profile defaults)})))
  defaults)

(def ^:private rollout-canary-keys
  #{:enabled? :version :percent})

(def ^:private rollout-keys
  #{:active :canary})

(defn- validate-rollout-config!
  [rollout path]
  (ensure-keyword-map! rollout path)
  (ensure-only-keys! rollout rollout-keys path)
  (when (contains? rollout :active)
    (when-not (keyword? (keywordish (:active rollout)))
      (fail-router! "Router rollout :active must be a keyword."
                    {:path (conj path :active)
                     :expected :keyword
                     :value (:active rollout)})))
  (when (contains? rollout :canary)
    (let [canary (:canary rollout)]
      (ensure-keyword-map! canary (conj path :canary))
      (ensure-only-keys! canary rollout-canary-keys (conj path :canary))
      (when (contains? canary :enabled?)
        (when-not (boolean? (:enabled? canary))
          (fail-router! "Router rollout canary :enabled? must be a boolean."
                        {:path (conj path :canary :enabled?)
                         :expected :boolean
                         :value (:enabled? canary)})))
      (when (contains? canary :version)
        (when-not (keyword? (keywordish (:version canary)))
          (fail-router! "Router rollout canary :version must be a keyword."
                        {:path (conj path :canary :version)
                         :expected :keyword
                         :value (:version canary)})))
      (when (contains? canary :percent)
        (let [p (:percent canary)]
          (when-not (and (integer? p) (<= 0 p 100))
            (fail-router! "Router rollout canary :percent must be integer in [0,100]."
                          {:path (conj path :canary :percent)
                           :expected :percent
                           :value p})))))))

(defn- validate-policy-profiles!
  [profiles path]
  (ensure-keyword-map! profiles path)
  (doseq [[profile profile-cfg] profiles]
    (let [profile-path (conj path profile)]
      (ensure-map! profile-cfg profile-path)
      (when (contains? profile-cfg :default)
        (ensure-map! (:default profile-cfg) (conj profile-path :default)))
      (when (contains? profile-cfg :intents)
        (ensure-keyword-map! (:intents profile-cfg) (conj profile-path :intents))
        (doseq [[intent intent-cfg] (:intents profile-cfg)]
          (ensure-map! intent-cfg (conj profile-path :intents intent))))
      (when (contains? profile-cfg :limits)
        (ensure-keyword-map! (:limits profile-cfg) (conj profile-path :limits))
        (doseq [[k v] (:limits profile-cfg)]
          (when-not (and (integer? v) (<= 0 v))
            (fail-router! "Router policy profile limits must be non-negative integers."
                          {:path (conj profile-path :limits k)
                           :expected :non-negative-int
                           :value v})))))))

(defn- validate-gateway-breaker!
  [breaker path]
  (ensure-keyword-map! breaker path)
  (ensure-only-keys! breaker gateway-breaker-keys path)
  (when (contains? breaker :enabled?)
    (when-not (boolean? (:enabled? breaker))
      (fail-router! "Gateway breaker :enabled? must be a boolean."
                    {:path (conj path :enabled?)
                     :expected :boolean
                     :value (:enabled? breaker)})))
  (when (contains? breaker :min-samples)
    (let [n (:min-samples breaker)]
      (when-not (and (integer? n) (pos? n))
        (fail-router! "Gateway breaker :min-samples must be a positive integer."
                      {:path (conj path :min-samples)
                       :expected :positive-int
                       :value n}))))
  (when (contains? breaker :error-rate-open)
    (let [v (:error-rate-open breaker)]
      (when-not (and (number? v)
                     (<= 0.0 (double v))
                     (<= (double v) 1.0))
        (fail-router! "Gateway breaker :error-rate-open must be in range [0.0, 1.0]."
                      {:path (conj path :error-rate-open)
                       :expected :rate
                       :value v}))))
  (when (contains? breaker :cooldown-ms)
    (let [n (:cooldown-ms breaker)]
      (when-not (and (integer? n) (pos? n))
        (fail-router! "Gateway breaker :cooldown-ms must be a positive integer."
                      {:path (conj path :cooldown-ms)
                       :expected :positive-int
                       :value n}))))
  breaker)

(defn- validate-gateway-config!
  [gateway path]
  (ensure-keyword-map! gateway path)
  (ensure-only-keys! gateway gateway-keys path)
  (when (contains? gateway :strategy)
    (let [strategy (:strategy gateway)]
      (when-not (contains? gateway-strategies strategy)
        (fail-router! "Gateway :strategy must be one of :latency-first/:quality-first/:cost-first."
                      {:path (conj path :strategy)
                       :expected gateway-strategies
                       :value strategy}))))
  (when (contains? gateway :intent->strategy)
    (ensure-keyword-map! (:intent->strategy gateway) (conj path :intent->strategy))
    (doseq [[intent strategy] (:intent->strategy gateway)]
      (when-not (contains? gateway-strategies strategy)
        (fail-router! "Gateway :intent->strategy values must be known strategies."
                      {:path (conj path :intent->strategy intent)
                       :expected gateway-strategies
                       :value strategy}))))
  (when (contains? gateway :ema-alpha)
    (let [v (:ema-alpha gateway)]
      (when-not (and (number? v)
                     (<= 0.0 (double v))
                     (<= (double v) 1.0))
        (fail-router! "Gateway :ema-alpha must be in range [0.0, 1.0]."
                      {:path (conj path :ema-alpha)
                       :expected :rate
                       :value v}))))
  (when (contains? gateway :circuit-breaker)
    (validate-gateway-breaker! (:circuit-breaker gateway)
                               (conj path :circuit-breaker)))
  (when (contains? gateway :hedging)
    (let [hedging (:hedging gateway)
          hpath (conj path :hedging)]
      (ensure-keyword-map! hedging hpath)
      (ensure-only-keys! hedging gateway-hedging-keys hpath)
      (when (contains? hedging :enabled?)
        (when-not (boolean? (:enabled? hedging))
          (fail-router! "Gateway hedging :enabled? must be a boolean."
                        {:path (conj hpath :enabled?)
                         :expected :boolean
                         :value (:enabled? hedging)})))
      (when (contains? hedging :intent->enabled?)
        (let [intent-map (:intent->enabled? hedging)
              ipath (conj hpath :intent->enabled?)]
          (ensure-keyword-map! intent-map ipath)
          (doseq [[intent enabled?] intent-map]
            (when-not (boolean? enabled?)
              (fail-router! "Gateway hedging :intent->enabled? values must be booleans."
                            {:path (conj ipath intent)
                             :expected :boolean
                             :value enabled?})))))
      (when (contains? hedging :max-probes)
        (let [n (:max-probes hedging)]
          (when-not (and (integer? n) (>= (long n) 2))
            (fail-router! "Gateway hedging :max-probes must be an integer >= 2."
                          {:path (conj hpath :max-probes)
                           :expected :int>=2
                           :value n}))))
      (when (contains? hedging :delay-ms)
        (let [n (:delay-ms hedging)]
          (when-not (and (integer? n) (>= (long n) 0))
            (fail-router! "Gateway hedging :delay-ms must be a non-negative integer."
                          {:path (conj hpath :delay-ms)
                           :expected :non-negative-int
                           :value n}))))))
  gateway)

(defn validate-router-config!
  "Validates router configuration shape and throws `ex-info` when invalid."
  [config]
  (let [cfg (if (map? config) config {})]
    (ensure-map! cfg [])
    (ensure-only-keys! cfg router-top-keys [])
    (when-not (contains? cfg :routing)
      (fail-router! "Router config is missing required :routing map."
                    {:path [:routing]
                     :required true}))
    (let [routing (:routing cfg)]
      (when-not (ig/ref? routing)
        (ensure-keyword-map! routing [:routing])
        (ensure-only-keys! routing routing-keys [:routing])
        (when-not (contains? routing :intent->cap)
          (fail-router! "Router config is missing required :routing/:intent->cap mapping."
                        {:path [:routing :intent->cap]
                         :required true}))
        (validate-keyword->keyword-map! (:intent->cap routing) [:routing :intent->cap])
        (when (contains? routing :cap->model-key)
          (validate-keyword->keyword-map! (:cap->model-key routing)
                                          [:routing :cap->model-key]))
        (when (contains? routing :intent->default-model-key)
          (validate-keyword->keyword-map! (:intent->default-model-key routing)
                                          [:routing :intent->default-model-key]))
        (when (contains? routing :cap->role)
          (validate-keyword->keyword-map! (:cap->role routing)
                                          [:routing :cap->role]))
        (when (contains? routing :intent->role)
          (validate-keyword->keyword-map! (:intent->role routing)
                                          [:routing :intent->role]))
        (when (contains? routing :intent->default-role)
          (validate-keyword->keyword-map! (:intent->default-role routing)
                                          [:routing :intent->default-role]))
        (when (contains? routing :switch-on)
          (validate-keyword-coll! (:switch-on routing) [:routing :switch-on]))
        (when (contains? routing :fallback)
          (validate-keyword-coll! (:fallback routing) [:routing :fallback]))
        (when (contains? routing :checks)
          (validate-keyword-coll! (:checks routing) [:routing :checks]))
        (when (contains? routing :gateway)
          (validate-gateway-config! (:gateway routing) [:routing :gateway]))
        (when (contains? routing :policy)
          (when-not (keyword? (:policy routing))
            (fail-router! "Router config :routing/:policy must be a keyword."
                          {:path [:routing :policy]
                           :expected :keyword
                           :value (:policy routing)})))
        (when (contains? routing :retry)
          (let [retry (:retry routing)]
            (ensure-keyword-map! retry [:routing :retry])
            (ensure-only-keys! retry retry-keys [:routing :retry])
            (doseq [[k v] retry]
              (when-not (and (integer? v) (<= 0 v))
                (fail-router! "Router retry limits must be non-negative integers."
                              {:path [:routing :retry k]
                               :expected :non-negative-int
                               :value v})))))))
    (when (contains? cfg :profiles)
      (let [profiles (:profiles cfg)]
        (when-not (ig/ref? profiles)
          (ensure-keyword-map! profiles [:profiles])
          (doseq [[profile profile-cfg] profiles]
            (when-not (map? profile-cfg)
              (fail-router! "Router profile entries must be maps."
                            {:path [:profiles profile]
                             :expected :map
                             :value profile-cfg}))))))
    (when (contains? cfg :intent->policy-profile)
      (validate-keyword->keyword-map! (:intent->policy-profile cfg)
                                      [:intent->policy-profile]))
    (when (contains? cfg :policy-profiles)
      (validate-policy-profiles! (:policy-profiles cfg)
                                 [:policy-profiles]))
    (when (contains? cfg :policy)
      (when-not (keyword? (:policy cfg))
        (fail-router! "Router top-level :policy must be a keyword."
                      {:path [:policy]
                       :expected :keyword
                       :value (:policy cfg)})))
    (when (contains? cfg :artifact/version)
      (when-not (keyword? (keywordish (:artifact/version cfg)))
        (fail-router! "Router top-level :artifact/version must be a keyword."
                      {:path [:artifact/version]
                       :expected :keyword
                       :value (:artifact/version cfg)})))
    (when (contains? cfg :versions)
      (let [versions (:versions cfg)]
        (ensure-keyword-map! versions [:versions])
        (doseq [[version patch] versions]
          (when-not (map? patch)
            (fail-router! "Router version patch must be a map."
                          {:path [:versions version]
                           :expected :map
                           :value patch})))))
    (when (contains? cfg :rollout)
      (validate-rollout-config! (:rollout cfg) [:rollout]))
    (when (contains? cfg :defaults)
      (validate-router-defaults! (:defaults cfg) [:defaults]))
    cfg))

(defn preconfigure-router
  "Pre-configuration hook for router configuration branch."
  [_k config]
  (validate-router-config! (if (map? config) config {})))

(defn init-router
  "Initialization hook for router configuration branch."
  [_k config]
  (preconfigure-router _k config))

(defn stop-router
  "Stop hook for router configuration branch."
  [_k _state]
  nil)

(defn runtime-config
  "Returns normalized runtime config map.

  Accepts direct runtime config or wrapper maps containing `:runtime`/`:config`."
  [runtime]
  (cond
    (nil? runtime) nil
    (and (map? runtime) (map? (:runtime runtime))) (runtime-config (:runtime runtime))
    (and (map? runtime) (map? (:config runtime))) (:config runtime)
    (map? runtime) runtime
    :else nil))

(defn resolver-config
  "Returns resolver config from explicit `resolver` arg or runtime branch."
  [runtime resolver]
  (or resolver
      (some-> (runtime-config runtime) :resolver)))

(defn router-config
  "Returns router config from runtime branch."
  [runtime]
  (some-> (runtime-config runtime) :router))

(defn routing-defaults
  "Returns normalized routing defaults map from router config."
  [runtime]
  (let [defaults (some-> (router-config runtime) :defaults)]
    (if (map? defaults) defaults {})))

(defn resolver-routing
  "Returns routing map from dedicated router branch."
  [runtime resolver]
  (or (some-> (router-config runtime) :routing)
      (some-> (resolver-config runtime resolver) :routing)
      {}))

(defn resolver-capability
  "Finds capability metadata by `cap-id` in resolver config."
  [runtime resolver cap-id]
  (let [resolver' (resolver-config runtime resolver)]
    (or (some-> resolver' :caps/by-id (get cap-id))
        (some (fn [cap]
                (when (= cap-id (:cap/id cap))
                  cap))
              (:caps resolver')))))

(def ^:private fallback-intent->default-model-key
  {:text/respond :ferment.model/voice
   :code/generate :ferment.model/coding
   :code/patch :ferment.model/coding
   :code/explain :ferment.model/coding
   :code/review :ferment.model/coding
   :route/decide :ferment.model/meta
   :context/summarize :ferment.model/meta
   :eval/grade :ferment.model/meta
   :problem/solve :ferment.model/solver})

(def ^:private fallback-intent->default-role
  {:problem/solve :solver
   :code/generate :coder
   :code/patch :coder
   :code/explain :coder
   :code/review :coder
   :route/decide :router
   :context/summarize :router
   :eval/grade :router
   :text/respond :voice})

(defn- normalize-version-catalog
  [versions]
  (if-not (map? versions)
    {}
    (into {}
          (keep (fn [[version patch]]
                  (let [v' (keywordish version)]
                    (when (and (keyword? v')
                               (map? patch))
                      [v' patch]))))
          versions)))

(defn- normalize-rollout
  [rollout]
  (if-not (map? rollout)
    {}
    (let [canary (if (map? (:canary rollout))
                   (:canary rollout)
                   {})
          active (keywordish (:active rollout))
          canary-version (keywordish (:version canary))
          percent (let [p (:percent canary)]
                    (when (and (integer? p) (<= 0 p 100))
                      p))]
      {:active active
       :canary (cond-> {}
                 (contains? canary :enabled?) (assoc :enabled? (boolean (:enabled? canary)))
                 (keyword? canary-version) (assoc :version canary-version)
                 (integer? percent) (assoc :percent percent))})))

(defn- trace-bucket
  [trace-id]
  (if (and (string? trace-id) (not (str/blank? trace-id)))
    (mod (Math/abs (long (hash trace-id))) 100)
    0))

(defn select-router-artifact
  "Selects router artifact/version for a request.

  Input:
  - `router`: router config map, optionally with `:versions` and `:rollout`,
  - opts map:
    - `:trace-id` (string) for deterministic canary bucketing,
    - `:requested-version` (keyword|string) for explicit request override.

  Output map:
  - `:router`: selected router config map,
  - `:artifact/version`: selected version keyword when present,
  - `:artifact/source`: one of `:request`, `:canary`, `:active`, `:default`."
  [router {:keys [trace-id requested-version]}]
  (let [base-router (if (map? router) router {})
        versions (normalize-version-catalog (:versions base-router))
        rollout (normalize-rollout (:rollout base-router))
        requested-version' (keywordish requested-version)
        active-version (or (keywordish (:active rollout))
                           (keywordish (:artifact/version base-router)))
        canary-cfg (if (map? (:canary rollout))
                     (:canary rollout)
                     {})
        canary-enabled? (true? (:enabled? canary-cfg))
        canary-version (keywordish (:version canary-cfg))
        canary-percent (let [p (:percent canary-cfg)]
                         (if (and (integer? p) (<= 0 p 100))
                           p
                           0))
        canary-hit? (and canary-enabled?
                         (keyword? canary-version)
                         (contains? versions canary-version)
                         (> canary-percent 0)
                         (< (trace-bucket trace-id) canary-percent))
        [selected-version selected-source]
        (cond
          (and (keyword? requested-version')
               (contains? versions requested-version'))
          [requested-version' :request]

          canary-hit?
          [canary-version :canary]

          (and (keyword? active-version)
               (contains? versions active-version))
          [active-version :active]

          :else
          [(or (keywordish (:artifact/version base-router))
               (first (keys versions)))
           :default])
        selected-patch (when (keyword? selected-version)
                         (get versions selected-version))
        selected-router (if (map? selected-patch)
                          (-> (dissoc base-router :versions :rollout :artifact/version)
                              (deep-merge selected-patch)
                              (assoc :artifact/version selected-version))
                          (cond-> (dissoc base-router :versions :rollout)
                            (keyword? selected-version)
                            (assoc :artifact/version selected-version)))]
    {:router selected-router
     :artifact/version selected-version
     :artifact/source selected-source}))

(defn default-model-key-by-intent
  "Returns default model selector key for capability intent from routing config."
  [runtime resolver intent]
  (or (some-> (resolver-routing runtime resolver) :intent->default-model-key (get intent))
      (some-> fallback-intent->default-model-key (get intent))
      :ferment.model/solver))

(defn resolve-model-key
  "Resolves model selector key from capability metadata, resolver routing, or default intent mapping."
  [runtime resolver cap-id intent]
  (or (some-> (resolver-capability runtime resolver cap-id) :dispatch/model-key)
      (some-> (resolver-routing runtime resolver) :cap->model-key (get cap-id))
      (default-model-key-by-intent runtime resolver intent)))

(defn default-role-by-intent
  "Returns default execution role for capability intent from routing config."
  [runtime resolver intent]
  (or (some-> (resolver-routing runtime resolver) :intent->default-role (get intent))
      (some-> fallback-intent->default-role (get intent))
      :voice))

(defn resolve-role
  "Resolves execution role from capability metadata, resolver routing, or default intent mapping."
  [runtime resolver cap-id intent]
  (or (some-> (resolver-capability runtime resolver cap-id) :dispatch/role)
      (some-> (resolver-routing runtime resolver) :cap->role (get cap-id))
      (some-> (resolver-routing runtime resolver) :intent->role (get intent))
      (default-role-by-intent runtime resolver intent)))

(derive ::service :ferment.system/value)
(derive :ferment.router/default ::service)

(system/add-expand ::service [k config] {k (preconfigure-router k config)})
(system/add-init   ::service [k config]    (init-router k config))
(system/add-halt!  ::service [k state]     (stop-router k state))
