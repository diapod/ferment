(ns

    ^{:doc    "Runtime configuration branch for Ferment core orchestration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.runtime

  (:require [ferment.system :as system]))

(defn preconfigure-runtime
  "Pre-configuration hook for runtime config branch."
  [_k config]
  config)

(defn init-runtime
  "Initialization hook for runtime config branch.

  Runtime branch is configuration-oriented and passed through unchanged."
  [_k config]
  config)

(defn stop-runtime
  "Stop hook for runtime config branch."
  [_k _state]
  nil)

(derive ::default :ferment.system/value)

(system/add-expand ::default [k config] {k (preconfigure-runtime k config)})
(system/add-init   ::default [k config]    (init-runtime k config))
(system/add-halt!  ::default [k state]     (stop-runtime k state))
