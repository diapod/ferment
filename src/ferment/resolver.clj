(ns

    ^{:doc    "Resolver configuration branch for Ferment capabilities dispatch."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.resolver

  (:require [ferment.system :as system]))

(defn preconfigure-resolver
  "Pre-configuration hook for resolver config keys."
  [_k config]
  config)

(defn- caps->by-id
  "Builds `:cap/id -> capability-config` index from a sequential `caps` collection."
  [caps]
  (into {}
        (keep (fn [cap]
                (when (and (map? cap) (keyword? (:cap/id cap)))
                  [(:cap/id cap) cap])))
        caps))

(defn init-resolver
  "Initialization hook for resolver config keys.

  Default behavior is pass-through plus optional `:caps/by-id` derivation when
  `:caps` is a sequence of capability maps."
  [_k config]
  (if (and (map? config) (sequential? (:caps config)))
    (assoc config :caps/by-id (caps->by-id (:caps config)))
    config))

(defn stop-resolver
  "Stop hook for resolver config keys.

  Resolver branch is configuration-oriented, so stop is a no-op."
  [_k _state]
  nil)

(derive ::default ::config)

(system/add-expand ::config [k config] {k (preconfigure-resolver k config)})
(system/add-init   ::config [k config]    (init-resolver k config))
(system/add-halt!  ::config [k state]     (stop-resolver k state))
