(ns

    ^{:doc    "Capabilities configuration branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.caps

  (:require [ferment.caps.registry]
            [clojure.string :as str]
            [ferment.system :as system]))

(def ^:private default-can-produce
  #{:value})

(def ^:private default-effects-allowed
  #{:none})

(def ^:private default-cap-version
  "1.0.0")

(def ^:private default-cap-cost
  {})

(def ^:private default-cap-limits
  {})

(def ^:private default-cap-tags
  #{})

(defn- keyword-set
  [v]
  (cond
    (set? v)        (into #{} (filter keyword?) v)
    (sequential? v) (into #{} (filter keyword?) v)
    (keyword? v)    #{v}
    :else           #{}))

(defn- keyword-coll-of?
  [v]
  (or (keyword? v)
      (and (set? v) (every? keyword? v))
      (and (sequential? v)
           (not (map? v))
           (every? keyword? v))))

(defn- nonblank-string?
  [v]
  (and (string? v) (not (str/blank? v))))

(defn- normalize-kv-map
  [m]
  (if (map? m) m {}))

(defn- nonneg-number-map?
  [m]
  (and (map? m)
       (every? (fn [[k v]]
                 (and (keyword? k)
                      (number? v)
                      (not (neg? (double v)))))
               m)))

(defn normalize-capability-metadata
  "Normalizes capability metadata fields to sets of keywords."
  [cap-config]
  (let [intents     (keyword-set (:cap/intents cap-config))
        can-produce (let [v (keyword-set (:cap/can-produce cap-config))]
                      (if (seq v) v default-can-produce))
        effects     (let [v (keyword-set (:cap/effects-allowed cap-config))]
                      (if (seq v) v default-effects-allowed))
        tags        (let [v (keyword-set (:cap/tags cap-config))]
                      (if (seq v) v default-cap-tags))
        version     (if (nonblank-string? (:cap/version cap-config))
                      (:cap/version cap-config)
                      default-cap-version)
        cost        (let [v (normalize-kv-map (:cap/cost cap-config))]
                      (if (seq v) v default-cap-cost))
        limits      (let [v (normalize-kv-map (:cap/limits cap-config))]
                      (if (seq v) v default-cap-limits))]
    (assoc cap-config
           :cap/intents intents
           :cap/can-produce can-produce
           :cap/effects-allowed effects
           :cap/tags tags
           :cap/version version
           :cap/cost cost
           :cap/limits limits)))

(defn- ensure-required-capability-metadata!
  [cap-key cap-config]
  (let [missing (cond-> []
                  (not (contains? cap-config :cap/intents))     (conj :cap/intents)
                  (not (contains? cap-config :cap/can-produce)) (conj :cap/can-produce)
                  (not (contains? cap-config :io/in-schema))    (conj :io/in-schema)
                  (not (contains? cap-config :io/out-schema))   (conj :io/out-schema))]
    (when (seq missing)
      (throw (ex-info "Capability metadata is incomplete."
                      {:cap/key  cap-key
                       :error    :capability/invalid-metadata
                       :missing  missing
                       :required [:cap/intents :cap/can-produce :io/in-schema :io/out-schema]
                       :config   cap-config})))))

(defn- validate-extended-capability-metadata!
  [cap-key cap-config]
  (let [errors (cond-> []
                 (and (contains? cap-config :cap/version)
                      (not (nonblank-string? (:cap/version cap-config))))
                 (conj :cap/version)
                 (and (contains? cap-config :cap/tags)
                      (not (keyword-coll-of? (:cap/tags cap-config))))
                 (conj :cap/tags)
                 (and (contains? cap-config :cap/cost)
                      (not (nonneg-number-map? (:cap/cost cap-config))))
                 (conj :cap/cost)
                 (and (contains? cap-config :cap/limits)
                      (not (nonneg-number-map? (:cap/limits cap-config))))
                 (conj :cap/limits)
                 (not (keyword? (:io/in-schema cap-config)))
                 (conj :io/in-schema)
                 (not (keyword? (:io/out-schema cap-config)))
                 (conj :io/out-schema))]
    (when (seq errors)
      (throw (ex-info "Capability metadata failed extended validation."
                      {:cap/key cap-key
                       :error :capability/invalid-metadata
                       :invalid-fields (vec errors)
                       :config cap-config})))))

(defn preconfigure-caps
  "Pre-configuration hook for generic `:ferment.caps/...` branch keys.

  Default behavior is pass-through."
  [_k config]
  config)

(defn init-caps
  "Initialization hook for generic `:ferment.caps/...` branch keys.

  Default behavior is pass-through."
  [_k config]
  config)

(defn stop-caps
  "Stop hook for generic `:ferment.caps/...` branch keys.

  Branch is configuration-oriented, so default stop is no-op."
  [_k _state]
  nil)

;; Per-capability entry hooks (`:ferment.caps.registry/...` keys).
(defn preconfigure-capability-value
  "Pre-configuration hook for a single capability entry."
  [_cap-key cap-config]
  cap-config)

(defn init-capability-value
  "Initialization hook for a single capability entry.

  Useful when one capability needs custom parsing before runtime usage."
  [cap-key cap-config]
  (if (map? cap-config)
    (do
      (ensure-required-capability-metadata! cap-key cap-config)
      (validate-extended-capability-metadata! cap-key cap-config)
      (normalize-capability-metadata cap-config))
    cap-config))

(defn stop-capability-value
  "Stop hook for a single capability entry."
  [_cap-key _state]
  nil)

;; Shared parents for capabilities config keys.
;; Add new branch keys with: (derive :ferment.caps/foo ::config)
;; Add new capability entries with: (derive :ferment.caps.registry/bar ::entry)
(derive ::entry    ::config)
(derive ::registry ::config)
(derive ::routing  ::config)
(derive ::profiles ::config)

;; Generic config branch lifecycle.
(system/add-expand ::config [k config] {k (preconfigure-caps k config)})
(system/add-init   ::config [k config]    (init-caps k config))
(system/add-halt!  ::config [k state]     (stop-caps k state))

;; Per-capability lifecycle (for flattened `:ferment.caps.registry/*` keys).
(system/add-expand ::entry [k config] {k (preconfigure-capability-value k config)})
(system/add-init   ::entry [k config]    (init-capability-value k config))
(system/add-halt!  ::entry [k state]     (stop-capability-value k state))
