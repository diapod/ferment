(ns

    ^{:doc    "Capabilities configuration branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.caps

  (:require [ferment.system :as system]))

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
  [_cap-key cap-config]
  cap-config)

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
