(ns

    ^{:doc    "Routing helpers for capability dispatch (role/model-key resolution)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.router

  (:require [integrant.core :as ig]
            [ferment.system :as system]))

(def ^:private router-top-keys
  #{:routing :profiles :policy :defaults})

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
    :policy})

(def ^:private retry-keys
  #{:same-cap-max :fallback-max})

(def ^:private routing-default-keys
  #{:meta? :strict? :force? :on-error})

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
  defaults)

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
    (when (contains? cfg :policy)
      (when-not (keyword? (:policy cfg))
        (fail-router! "Router top-level :policy must be a keyword."
                      {:path [:policy]
                       :expected :keyword
                       :value (:policy cfg)})))
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
