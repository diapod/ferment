(ns

    ^{:doc    "Session manager branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.session

  (:require [ferment.system :as system]
            [ferment.session.store :as store])

  (:import (java.time Instant)
           (java.util UUID)))

(defn now-iso
  []
  (str (Instant/now)))

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- normalize-int
  [v default minv]
  (let [n (cond
            (integer? v) v
            (number? v) (long v)
            :else nil)
        n' (or n default)]
    (max minv n')))

(defn preconfigure-context
  [_k config]
  (let [cfg (if (map? config) config {})]
    (-> cfg
        (update :context/version      #(normalize-int % 1 1))
        (update :window/turns         #(normalize-int % 16 1))
        (update :window/max-chars     #(normalize-int % 20000 1024))
        (update :summary/target-chars #(normalize-int % 1200 256)))))

(defn init-context
  [_k config]
  (preconfigure-context _k config))

(defn stop-context
  [_k _state]
  nil)

(defn preconfigure-manager
  [_k config]
  (let [cfg (if (map? config) config {})]
    (-> cfg
        (assoc :store (or (:store cfg)
                          (system/ref :ferment.session.store/default)))
        (assoc :context (or (:context cfg)
                            (system/ref :ferment.session.context/default)))
        (update :max-hot-sessions #(normalize-int % 32 1))
        (update :idle-ttl-ms #(normalize-int % 900000 0)))))

(defn init-manager
  [_k {:keys [store context] :as config}]
  (let [cfg (preconfigure-manager _k config)]
    (assoc cfg
           :store store
           :context context
           :hot (atom {})
           :started-at (now-iso))))

(defn stop-manager
  [_k _state]
  nil)

(defn hot-sessions
  [manager]
  @(or (:hot manager) (atom {})))

(defn- session-id
  [sid]
  (or (store/normalize-session-id sid)
      (str (UUID/randomUUID))))

(defn- mark-hot!
  [manager sid]
  (let [sid' (session-id sid)]
    (swap! (:hot manager)
           assoc
           sid'
           {:session/id sid'
            :hot/last-used-ms (now-ms)})
    sid'))

(defn- unmark-hot!
  [manager sid]
  (swap! (:hot manager) dissoc (session-id sid))
  nil)

(defn- freeze-victim!
  [manager sid reason]
  (unmark-hot! manager sid)
  (store/freeze-session! (:store manager)
                         sid
                         {:session/meta {:freeze/reason reason}})
  nil)

(defn- enforce-hot-limit!
  [manager keep-sid]
  (let [keep-sid' (some-> keep-sid session-id)
        limit     (or (:max-hot-sessions manager) 32)]
    (loop []
      (let [hot-map (hot-sessions manager)]
        (when (> (count hot-map) limit)
          (let [victim (->> hot-map
                            vals
                            (remove #(= keep-sid' (:session/id %)))
                            (sort-by :hot/last-used-ms)
                            first
                            :session/id)]
            (if victim
              (do
                (freeze-victim! manager victim :manager/hot-limit)
                (recur))
              ;; There is only keep-sid in hot map, but map still exceeds limit;
              ;; this can only happen with limit <= 0 (already normalized out),
              ;; so we defensively break.
              nil)))))))

(defn- expire-hot-sessions!
  [manager]
  (let [ttl-ms (or (:idle-ttl-ms manager) 0)]
    (when (pos? ttl-ms)
      (let [now (now-ms)]
        (doseq [{:keys [session/id hot/last-used-ms]} (vals (hot-sessions manager))]
          (when (> (- now (long (or last-used-ms 0)))
                   ttl-ms)
            (freeze-victim! manager id :manager/idle-ttl)))))))

(defn open-session!
  [manager sid opts]
  (let [sid'  (session-id sid)
        opts' (if (map? opts) opts {})]
    (expire-hot-sessions! manager)
    (store/ensure-session! (:store manager) sid' opts')
    (let [meta' (merge {:context/version
                        (get-in manager [:context :context/version])}
                       (if (map? (:session/meta opts'))
                         (:session/meta opts')
                         {}))
          thaw-opts (assoc (dissoc opts' :session/meta) :session/meta meta')
          session (store/thaw-session!
                   (:store manager)
                   sid'
                   thaw-opts)]
      (mark-hot! manager sid')
      (enforce-hot-limit! manager sid')
      session)))

(defn thaw-session!
  [manager sid opts]
  (open-session! manager sid opts))

(defn freeze-session!
  [manager sid opts]
  (let [sid' (session-id sid)]
    (unmark-hot! manager sid')
    (store/freeze-session! (:store manager) sid' opts)))

(defn append-session-turn!
  [manager sid turn]
  (let [sid' (session-id sid)]
    (open-session! manager sid' nil)
    (mark-hot! manager sid')
    (store/append-turn! (:store manager) sid' turn)))

(defn get-session
  [manager sid]
  (let [sid' (session-id sid)]
    (expire-hot-sessions! manager)
    (when-let [session (store/get-session (:store manager) sid')]
      (when (contains? (hot-sessions manager) sid')
        (mark-hot! manager sid'))
      session)))

(defn list-sessions
  [manager]
  (expire-hot-sessions! manager)
  (store/list-sessions (:store manager)))

(defn preconfigure-service
  [_k config]
  (let [cfg (if (map? config) config {})]
    (-> cfg
        (assoc :store   (or (:store cfg)
                            (system/ref :ferment.session.store/default)))
        (assoc :context (or (:context cfg)
                            (system/ref :ferment.session.context/default)))
        (assoc :manager (or (:manager cfg)
                            (system/ref :ferment.session.manager/default))))))

(defn init-service
  [_k {:keys [store context manager] :as config}]
  (let [cfg (preconfigure-service _k config)]
    (assoc cfg
           :store         store
           :context       context
           :manager       manager
           :open!         (fn
                            ([sid]       (open-session!        manager sid nil))
                            ([sid opts]  (open-session!        manager sid opts)))
           :get!          (fn [sid]      (get-session          manager sid))
           :list!         (fn []         (list-sessions        manager))
           :append-turn!  (fn [sid turn] (append-session-turn! manager sid turn))
           :get-var!      (fn [sid k]    (store/get-var       store sid k))
           :get-vars!     (fn [sid ks]   (store/get-vars      store sid ks))
           :put-var!      (fn
                            ([sid k v]       (store/put-var!      store sid k v))
                            ([sid k v opts]  (store/put-var!      store sid k v opts)))
           :put-vars!     (fn
                            ([sid kvs]       (store/put-vars!     store sid kvs))
                            ([sid kvs opts]  (store/put-vars!     store sid kvs opts)))
           :del-var!      (fn [sid k]    (store/del-var!      store sid k))
           :del-vars!     (fn [sid ks]   (store/del-vars!     store sid ks))
           :del-all-vars! (fn [sid]      (store/del-all-vars! store sid))
           :freeze!       (fn
                            ([sid]       (freeze-session! manager sid nil))
                            ([sid opts]  (freeze-session! manager sid opts)))
           :thaw!         (fn
                            ([sid]       (thaw-session!   manager sid nil))
                            ([sid opts]  (thaw-session!   manager sid opts))))))

(defn stop-service
  [_k _state]
  nil)

(defn open!
  [session-service sid & [opts]]
  ((:open! session-service) sid (if (map? opts) opts nil)))

(defn get!
  [session-service sid]
  ((:get! session-service) sid))

(defn list!
  [session-service]
  ((:list! session-service)))

(defn append-turn!
  [session-service sid turn]
  ((:append-turn! session-service) sid turn))

(defn get-var!
  [session-service sid k]
  ((:get-var! session-service) sid k))

(defn get-vars!
  [session-service sid ks]
  ((:get-vars! session-service) sid ks))

(defn put-var!
  ([session-service sid k v]
   ((:put-var! session-service) sid k v))
  ([session-service sid k v opts]
   ((:put-var! session-service) sid k v opts)))

(defn put-vars!
  ([session-service sid kvs]
   ((:put-vars! session-service) sid kvs))
  ([session-service sid kvs opts]
   ((:put-vars! session-service) sid kvs opts)))

(defn del-var!
  [session-service sid k]
  ((:del-var! session-service) sid k))

(defn del-vars!
  [session-service sid ks]
  ((:del-vars! session-service) sid ks))

(defn del-all-vars!
  [session-service sid]
  ((:del-all-vars! session-service) sid))

(defn freeze!
  [session-service sid & [opts]]
  ((:freeze! session-service) sid (if (map? opts) opts nil)))

(defn thaw!
  [session-service sid & [opts]]
  ((:thaw! session-service) sid (if (map? opts) opts nil)))

(derive ::context :ferment.system/value)
(derive ::manager :ferment.system/value)
(derive ::service :ferment.system/value)

(derive :ferment.session.context/default ::context)
(derive :ferment.session.manager/default ::manager)
(derive :ferment.session/default ::service)

(system/add-expand ::context [k config] {k (preconfigure-context k config)})
(system/add-init   ::context [k config]    (init-context k config))
(system/add-halt!  ::context [k state]     (stop-context k state))

(system/add-expand ::manager [k config] {k (preconfigure-manager k config)})
(system/add-init   ::manager [k config]    (init-manager k config))
(system/add-halt!  ::manager [k state]     (stop-manager k state))

(system/add-expand ::service [k config] {k (preconfigure-service k config)})
(system/add-init   ::service [k config]    (init-service k config))
(system/add-halt!  ::service [k state]     (stop-service k state))
