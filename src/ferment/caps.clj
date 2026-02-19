(ns

    ^{:doc    "Capabilities configuration branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.caps

  (:require [ferment.caps.registry]
            [ferment.system :as system]))

(def ^:private default-can-produce
  #{:value})

(def ^:private default-effects-allowed
  #{:none})

(defn- keyword-set
  [v]
  (cond
    (set? v) (into #{} (filter keyword?) v)
    (sequential? v) (into #{} (filter keyword?) v)
    (keyword? v) #{v}
    :else #{}))

(defn normalize-capability-metadata
  "Normalizes capability metadata fields to sets of keywords."
  [cap-config]
  (let [intents (keyword-set (:cap/intents cap-config))
        can-produce (let [v (keyword-set (:cap/can-produce cap-config))]
                      (if (seq v) v default-can-produce))
        effects (let [v (keyword-set (:cap/effects-allowed cap-config))]
                  (if (seq v) v default-effects-allowed))]
    (assoc cap-config
           :cap/intents intents
           :cap/can-produce can-produce
           :cap/effects-allowed effects)))

(defn- ensure-required-capability-metadata!
  [cap-key cap-config]
  (let [missing (cond-> []
                  (not (contains? cap-config :cap/intents))
                  (conj :cap/intents)
                  (not (contains? cap-config :cap/can-produce))
                  (conj :cap/can-produce))]
    (when (seq missing)
      (throw (ex-info "Capability metadata is incomplete."
                      {:cap/key cap-key
                       :error :capability/invalid-metadata
                       :missing missing
                       :required [:cap/intents :cap/can-produce]
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
