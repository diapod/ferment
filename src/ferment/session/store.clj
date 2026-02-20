(ns

    ^{:doc    "Session store branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.session.store

  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [io.randomseed.utils.db :as udb]
            [ferment.system :as system])

  (:import (java.time Instant)
           (java.util UUID)))

(def ^:private table-name-pattern
  #"^[A-Za-z0-9_]+$")

(def ^:private db-row-opts
  {:builder-fn rs/as-unqualified-lower-maps})

(defn now-iso
  []
  (str (Instant/now)))

(defn normalize-session-id
  [sid]
  (or (cond
        (string? sid)  (some-> sid str/trim not-empty)
        (keyword? sid) (some-> sid name str/trim not-empty)
        (uuid? sid)    (str sid)
        (nil? sid)     nil
        :else          (some-> sid str str/trim not-empty))
      (str (UUID/randomUUID))))

(defn new-session
  [sid opts]
  (let [sid' (normalize-session-id sid)
        ts   (now-iso)]
    (merge {:session/id             sid'
            :session/version        0
            :session/state          :warm
            :session/frozen?        true
            :session/created-at     ts
            :session/updated-at     ts
            :session/last-access-at ts
            :session/turns          []
            :session/facts          {}}
           (if (map? opts) opts {}))))

(defn- revise-session
  [session]
  (let [ts (now-iso)]
    (-> session
        (update :session/version (fnil inc 0))
        (assoc :session/updated-at ts
               :session/last-access-at ts))))

(defn- backend-keyword
  [v]
  (some-> v name str/lower-case keyword))

(defn- normalize-table-name
  [v fallback]
  (let [t (cond
            (keyword? v) (name v)
            (string? v)  v
            :else        nil)
        t (some-> t str/trim not-empty)]
    (or t fallback)))

(defn- safe-table-name
  [v fallback]
  (let [t (normalize-table-name v fallback)]
    (when-not (and t (re-matches table-name-pattern t))
      (throw (ex-info "Invalid SQL table name in session store config."
                      {:table v
                       :fallback fallback
                       :table-name-pattern table-name-pattern})))
    t))

(defn- truthy-db-bool?
  [v]
  (cond
    (nil? v) false
    (boolean? v) v
    (number? v) (not (zero? (long v)))
    (string? v) (contains? #{"1" "true" "t" "yes" "y"}
                           (str/lower-case (str/trim v)))
    :else (boolean v)))

(defn- json-encode
  [v]
  (when (some? v)
    (json/generate-string v)))

(defn- json-decode
  [v default]
  (if (or (nil? v) (and (string? v) (str/blank? v)))
    default
    (try
      (json/parse-string (str v) true)
      (catch Throwable _
        default))))

(defn- state-keyword
  [v]
  (if-let [s (some-> v str str/trim not-empty)]
    (keyword s)
    :warm))

(defn- build-db-sql
  [sessions-table]
  {:select
   (str "SELECT `session_id`, `version`, `state`, `frozen`,"
        " `created_at`, `updated_at`, `last_access_at`, `frozen_at`, `thawed_at`,"
        " `summary`, `snapshot`, `meta`, `turns`, `facts`"
        " FROM `" sessions-table "`"
        " WHERE `session_id` = ? LIMIT 1")

   :list
   (str "SELECT `session_id`, `version`, `state`, `frozen`,"
        " `created_at`, `updated_at`, `last_access_at`, `frozen_at`, `thawed_at`,"
        " `summary`, `snapshot`, `meta`, `turns`, `facts`"
        " FROM `" sessions-table "`"
        " ORDER BY `updated_at` DESC")

   :delete
   (str "DELETE FROM `" sessions-table "` WHERE `session_id` = ?")

   :upsert
   (str "INSERT INTO `" sessions-table "`"
        " (`session_id`, `version`, `state`, `frozen`,"
        "  `created_at`, `updated_at`, `last_access_at`, `frozen_at`, `thawed_at`,"
        "  `summary`, `snapshot`, `meta`, `turns`, `facts`)"
        " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        " ON DUPLICATE KEY UPDATE"
        " `version` = VALUES(`version`),"
        " `state` = VALUES(`state`),"
        " `frozen` = VALUES(`frozen`),"
        " `created_at` = VALUES(`created_at`),"
        " `updated_at` = VALUES(`updated_at`),"
        " `last_access_at` = VALUES(`last_access_at`),"
        " `frozen_at` = VALUES(`frozen_at`),"
        " `thawed_at` = VALUES(`thawed_at`),"
        " `summary` = VALUES(`summary`),"
        " `snapshot` = VALUES(`snapshot`),"
        " `meta` = VALUES(`meta`),"
        " `turns` = VALUES(`turns`),"
        " `facts` = VALUES(`facts`)")})

(defn- session->db-values
  [session]
  (let [sid'           (normalize-session-id (:session/id session))
        ts             (now-iso)
        version        (long (or (:session/version session) 0))
        state          (or (some-> (:session/state session) name) "warm")
        frozen         (if (true? (:session/frozen? session)) 1 0)
        created-at     (or (:session/created-at session) ts)
        updated-at     (or (:session/updated-at session) ts)
        last-access-at (or (:session/last-access-at session) updated-at)
        frozen-at      (:session/frozen-at session)
        thawed-at      (:session/thawed-at session)
        summary        (json-encode (:session/summary session))
        snapshot       (json-encode (:session/snapshot session))
        meta           (json-encode (:session/meta session))
        turns          (json-encode (or (:session/turns session) []))
        facts          (json-encode (or (:session/facts session) {}))]
    [sid'
     version
     state
     frozen
     created-at
     updated-at
     last-access-at
     frozen-at
     thawed-at
     summary
     snapshot
     meta
     turns
     facts]))

(defn- row->session
  [row]
  (when-some [sid' (:session_id row)]
    (cond-> {:session/id             sid'
             :session/version        (long (or (:version row) 0))
             :session/state          (state-keyword (:state row))
             :session/frozen?        (truthy-db-bool? (:frozen row))
             :session/created-at     (:created_at row)
             :session/updated-at     (:updated_at row)
             :session/last-access-at (:last_access_at row)
             :session/turns          (json-decode (:turns row) [])
             :session/facts          (json-decode (:facts row) {})}
      (some? (:frozen_at row)) (assoc :session/frozen-at (:frozen_at row))
      (some? (:thawed_at row)) (assoc :session/thawed-at (:thawed_at row))
      (some? (:summary row))   (assoc :session/summary (json-decode (:summary row) nil))
      (some? (:snapshot row))  (assoc :session/snapshot (json-decode (:snapshot row) nil))
      (some? (:meta row))      (assoc :session/meta (json-decode (:meta row) nil)))))

(defn- memory-store?
  [store]
  (= :memory (:backend store)))

(defn- db-store?
  [store]
  (= :db (:backend store)))

(defn- datasource-of
  [v]
  (cond
    (nil? v) nil
    (instance? javax.sql.DataSource v) v
    (map? v) (or (:datasource v) (:datastore v) (:database v))
    :else nil))

(defn- db-select-session
  [store connectable sid']
  (some-> (jdbc/execute-one! connectable
                             [(:select (:sql store)) sid']
                             db-row-opts)
          row->session))

(defn- db-upsert-session!
  [store connectable session]
  (jdbc/execute-one! connectable
                     (into [(:upsert (:sql store))]
                           (session->db-values session)))
  nil)

(defn preconfigure-store
  [_k config]
  (let [cfg     (if (map? config) config {})
        backend (or (backend-keyword (:backend cfg)) :memory)
        cfg     (assoc cfg :backend backend)]
    (case backend
      :memory cfg
      :db     (-> cfg
                  (assoc :sessions-table (safe-table-name (:sessions-table cfg) "sessions"))
                  (assoc :vars-table (safe-table-name (:vars-table cfg) "session_vars")))
      (throw (ex-info "Unsupported session store backend."
                      {:backend backend
                       :supported #{:memory :db}})))))

(defn init-store
  [_k config]
  (let [cfg     (preconfigure-store _k config)
        backend (:backend cfg)]
    (case backend
      :memory (assoc cfg
                     :state (atom {})
                     :started-at (now-iso))
      :db     (let [datasource (datasource-of (:db cfg))]
                (when-not datasource
                  (throw (ex-info "Session store backend :db requires :db data source."
                                  {:backend backend
                                   :store-key _k
                                   :config cfg})))
                (assoc cfg
                       :db datasource
                       :var-getter (udb/make-setting-getter (:vars-table cfg) :session_id)
                       :var-setter (udb/make-setting-setter (:vars-table cfg) :session_id)
                       :var-deleter (udb/make-setting-deleter (:vars-table cfg) :session_id)
                       :sql (build-db-sql (:sessions-table cfg))
                       :started-at (now-iso)))
      (throw (ex-info "Unsupported session store backend."
                      {:backend backend
                       :supported #{:memory :db}})))))

(defn stop-store
  [_k _state]
  nil)

(defn store-state
  [store]
  (:state store))

(defn get-session
  [store sid]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      (db-select-session store (:db store) sid')
      (get @(store-state store) sid'))))

(defn ensure-session!
  [store sid & [opts]]
  (let [sid'  (normalize-session-id sid)
        opts' (if (map? opts) opts {})]
    (if (db-store? store)
      (jdbc/with-transaction [tx (:db store)]
        (if-some [existing (db-select-session store tx sid')]
          existing
          (let [created (new-session sid' opts')]
            (db-upsert-session! store tx created)
            created)))
      (do
        (swap! (store-state store)
               (fn [sessions]
                 (if (contains? sessions sid')
                   sessions
                   (assoc sessions sid' (new-session sid' opts')))))
        (get-session store sid')))))

(defn put-session!
  [store session]
  (let [sid' (normalize-session-id (:session/id session))
        session' (-> (if (map? session)
                       session
                       (throw (ex-info "Session must be a map."
                                       {:session session})))
                     (assoc :session/id sid')
                     revise-session)]
    (if (db-store? store)
      (db-upsert-session! store (:db store) session')
      (swap! (store-state store) assoc sid' session'))
    session'))

(defn update-session!
  [store sid f & args]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      (jdbc/with-transaction [tx (:db store)]
        (let [current  (or (db-select-session store tx sid')
                           (new-session sid' {}))
              updated  (apply f current args)
              updated' (-> (if (map? updated)
                             updated
                             (throw (ex-info "Session update function must return a map."
                                             {:sid sid'
                                              :result updated})))
                           (assoc :session/id sid')
                           revise-session)]
          (db-upsert-session! store tx updated')
          updated'))
      (do
        (swap! (store-state store)
               (fn [sessions]
                 (let [current (or (get sessions sid')
                                   (new-session sid' {}))
                       updated (apply f current args)]
                   (assoc sessions sid' (revise-session updated)))))
        (get-session store sid')))))

(defn append-turn!
  [store sid turn]
  (let [turn' (cond-> {:turn/at (now-iso)}
                (map? turn) (merge turn))]
    (update-session! store sid
                     (fn [session]
                       (-> session
                           (update :session/turns (fnil conj []) turn')
                           (assoc :session/state :hot
                                  :session/frozen? false))))))

(defn- merge-session-meta
  [session opts]
  (let [meta' (when (map? (:session/meta opts))
                (:session/meta opts))]
    (if (map? meta')
      (update session :session/meta (fnil merge {}) meta')
      session)))

(defn freeze-session!
  [store sid opts]
  (let [opts' (if (map? opts) opts {})]
    (update-session! store sid
                     (fn [session]
                       (-> session
                           (merge-session-meta opts')
                           (merge (select-keys opts'
                                               [:session/summary
                                                :session/snapshot]))
                           (assoc :session/state :warm
                                  :session/frozen? true
                                  :session/frozen-at (now-iso)))))))

(defn thaw-session!
  [store sid opts]
  (let [opts' (if (map? opts) opts {})]
    (update-session! store sid
                     (fn [session]
                       (-> session
                           (merge-session-meta opts')
                           (merge (select-keys opts'
                                               [:session/summary
                                                :session/snapshot]))
                           (assoc :session/state :hot
                                  :session/frozen? false
                                  :session/thawed-at (now-iso)))))))

(defn delete-session!
  [store sid]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      (jdbc/execute-one! (:db store) [(:delete (:sql store)) sid'])
      (swap! (store-state store) dissoc sid'))
    nil))

(defn list-sessions
  [store]
  (if (db-store? store)
    (->> (jdbc/execute! (:db store) [(:list (:sql store))] db-row-opts)
         (map row->session)
         (filter some?)
         vec)
    (->> @(store-state store)
         vals
         (sort-by :session/updated-at)
         reverse
         vec)))

(defn get-var
  [store sid k]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      ((:var-getter store) (:db store) sid' k)
      (get-in (get-session store sid') [:session/facts k]))))

(defn get-vars
  [store sid ks]
  (let [sid' (normalize-session-id sid)
        ks'  (if (sequential? ks) (seq ks) nil)]
    (if-not (seq ks')
      {}
      (if (db-store? store)
        (let [result (apply (:var-getter store) (:db store) sid' ks')]
          (if (= 1 (count ks'))
            (let [k (first ks')]
              (if (some? result) {k result} {}))
            (if (map? result) result {})))
        (let [facts (or (:session/facts (get-session store sid')) {})]
          (reduce (fn [m k]
                    (if (contains? facts k)
                      (assoc m k (get facts k))
                      m))
                  {}
                  ks'))))))

(defn put-var!
  [store sid k v]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      ((:var-setter store) (:db store) sid' k v)
      (do
        (update-session! store sid'
                         (fn [session]
                           (update session :session/facts (fnil assoc {}) k v)))
        true))))

(defn put-vars!
  [store sid kvs]
  (let [sid' (normalize-session-id sid)
        kvs' (if (map? kvs) kvs {})]
    (if-not (seq kvs')
      false
      (if (db-store? store)
        (apply (:var-setter store) (:db store) sid' (mapcat identity kvs'))
        (do
          (update-session! store sid'
                           (fn [session]
                             (update session :session/facts (fnil merge {}) kvs')))
          true)))))

(defn del-var!
  [store sid k]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      ((:var-deleter store) (:db store) sid' k)
      (do
        (update-session! store sid'
                         (fn [session]
                           (update session :session/facts (fnil dissoc {}) k)))
        true))))

(defn del-vars!
  [store sid ks]
  (let [sid' (normalize-session-id sid)
        ks'  (if (sequential? ks) (seq ks) nil)]
    (if-not (seq ks')
      false
      (if (db-store? store)
        (apply (:var-deleter store) (:db store) sid' ks')
        (do
          (update-session! store sid'
                           (fn [session]
                             (update session :session/facts
                                     (fn [facts]
                                       (apply dissoc (or facts {}) ks')))))
          true)))))

(defn del-all-vars!
  [store sid]
  (let [sid' (normalize-session-id sid)]
    (if (db-store? store)
      ((:var-deleter store) (:db store) sid')
      (do
        (update-session! store sid'
                         (fn [session]
                           (assoc session :session/facts {})))
        true))))

(derive ::store-value :ferment.system/value)
(derive :ferment.session.store/default ::store-value)

(system/add-expand ::store-value [k config] {k (preconfigure-store k config)})
(system/add-init   ::store-value [k config]    (init-store k config))
(system/add-halt!  ::store-value [k state]     (stop-store k state))
