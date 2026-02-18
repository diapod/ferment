(ns

    ^{:doc    "Runtime configuration branch for Ferment core orchestration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.runtime

  (:require [ferment.system :as system]))

(defn preconfigure-runtime
  "Pre-configuration hook for runtime config branch."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (-> cfg
        (update :ferment.model.session/enabled? #(if (nil? %) true (boolean %)))
        (update :ferment.model.session/idle-ttl-ms #(or % 900000))
        (update :ferment.model.session/max-per-model #(or % 4)))))

(defn init-runtime
  "Initialization hook for runtime config branch.

  Runtime branch is configuration-oriented and passed through unchanged."
  [_k config]
  (let [cfg (preconfigure-runtime _k config)]
    (assoc cfg
           :ferment.model.session/workers (atom {})
           :ferment.model.session/lock (Object.))))

(defn stop-runtime
  "Stop hook for runtime config branch."
  [_k _state]
  nil)

(derive ::default :ferment.system/value)

(system/add-expand ::default [k config] {k (preconfigure-runtime k config)})
(system/add-init   ::default [k config]    (init-runtime k config))
(system/add-halt!  ::default [k state]     (stop-runtime k state))
