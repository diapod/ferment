(ns

    ^{:doc    "Runtime configuration branch for Ferment core orchestration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.runtime

  (:require [ferment.system :as system]))

(defn- resolver-cap-ids
  [resolver]
  (let [by-id (if (map? (:caps/by-id resolver))
                (keys (:caps/by-id resolver))
                nil)
        listed (if (sequential? (:caps resolver))
                 (keep :cap/id (:caps resolver))
                 nil)]
    (into #{} (concat by-id listed))))

(defn- fail-runtime-router!
  [path cap-id known-caps]
  (throw (ex-info "Router references unknown capability id."
                  {:error :router/unknown-capability
                   :path path
                   :cap/id cap-id
                   :known-cap-ids (vec (sort-by str known-caps))})))

(defn- validate-router-capability-ref!
  [known-caps path cap-id]
  (when-not (contains? known-caps cap-id)
    (fail-runtime-router! path cap-id known-caps)))

(defn- validate-router-capabilities!
  [cfg]
  (let [router-cfg (if (map? (:router cfg)) (:router cfg) {})
        routing    (if (map? (:routing router-cfg)) (:routing router-cfg) {})
        resolver   (if (map? (:resolver cfg)) (:resolver cfg) {})
        known-caps (resolver-cap-ids resolver)]
    (when (seq known-caps)
      (doseq [[intent cap-id] (or (:intent->cap routing) {})]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :intent->cap intent]
                                         cap-id))
      (doseq [[cap-id _] (or (:cap->model-key routing) {})]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :cap->model-key cap-id]
                                         cap-id))
      (doseq [[cap-id _] (or (:cap->role routing) {})]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :cap->role cap-id]
                                         cap-id))
      (doseq [[idx cap-id] (map-indexed vector (or (:fallback routing) []))]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :fallback idx]
                                         cap-id))))
  cfg)

(defn- attach-router-to-resolver
  [cfg]
  (if-not (map? cfg)
    {}
    (let [router   (if (map? (:router cfg)) (:router cfg) {})
          resolver (if (map? (:resolver cfg)) (:resolver cfg) {})
          resolver' (cond-> (dissoc resolver :routing :profiles :policy)
                      (map? (:routing router))
                      (assoc :routing (:routing router))
                      (contains? router :profiles)
                      (assoc :profiles (:profiles router))
                      (contains? router :policy)
                      (assoc :policy (:policy router)))]
      (if (map? resolver')
        (assoc cfg :resolver resolver')
        cfg))))

(defn preconfigure-runtime
  "Pre-configuration hook for runtime config branch."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (-> cfg
        (cond-> (not (contains? cfg :router))
          (assoc :router (system/ref :ferment.router/default)))
        (cond-> (not (contains? cfg :roles))
          (assoc :roles (system/ref :ferment.roles/default)))
        (cond-> (not (contains? cfg :oplog))
          (assoc :oplog (system/ref :ferment.logging/oplog)))
        (update :ferment.model.session/enabled? #(if (nil? %) true (boolean %)))
        (update :ferment.model.session/idle-ttl-ms #(or % 900000))
        (update :ferment.model.session/max-per-model #(or % 4)))))

(defn init-runtime
  "Initialization hook for runtime config branch.

  Runtime branch is configuration-oriented and passed through unchanged."
  [_k config]
  (let [cfg (-> (preconfigure-runtime _k config)
                validate-router-capabilities!
                attach-router-to-resolver)]
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
