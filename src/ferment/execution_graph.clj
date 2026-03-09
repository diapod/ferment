(ns

    ^{:doc    "Durable execution graph storage for queued orchestration jobs."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.execution-graph

  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])

  (:import (java.time Instant)
           (java.util UUID)))

(def ^:private default-store-path
  "target/runtime/execution-graph/events.ednl")

(def ^:private inflight-statuses
  #{:submitted :queued :running})

(def ^:private terminal-statuses
  #{:completed :failed :canceled :expired})

(def ^:private max-events-default
  200000)

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v trim-s keyword)
    :else nil))

(defn- now-iso
  []
  (str (Instant/now)))

(defn- line->event
  [line]
  (let [line' (trim-s line)]
    (when line'
      (try
        (let [v (edn/read-string line')]
          (when (map? v) v))
        (catch Throwable _ nil)))))

(defn- read-events-from-file
  [path]
  (let [f (io/file path)]
    (if (.exists f)
      (with-open [r (io/reader f)]
        (->> (line-seq r)
             (keep line->event)
             vec))
      [])))

(defn- write-event-line!
  [path event]
  (io/make-parents (io/file path))
  (spit path (str (pr-str event) "\n") :append true))

(defn- empty-state
  []
  {:events/count 0
   :jobs {}})

(defn normalize-config
  "Normalizes execution graph config."
  [cfg]
  (let [cfg' (if (map? cfg) cfg {})
        enabled? (if (contains? cfg' :enabled?)
                   (boolean (:enabled? cfg'))
                   false)
        store-path (or (trim-s (:store/path cfg'))
                       default-store-path)
        store-type (or (keywordish (:store/type cfg'))
                       :fs-ednl)
        max-events (let [n (:max-events cfg')]
                     (if (and (integer? n) (pos? n))
                       n
                       max-events-default))]
    {:enabled? enabled?
     :store/type store-type
     :store/path store-path
     :max-events max-events}))

(defn- normalize-node-checkpoint
  [v]
  (when (map? v)
    (let [next-index (when (integer? (:next-index v))
                       (:next-index v))
          env (when (map? (:env v))
                (:env v))
          emitted (when (contains? v :emitted)
                    (:emitted v))]
      (cond-> {}
        (integer? next-index) (assoc :next-index next-index)
        (map? env) (assoc :env env)
        (contains? v :emitted) (assoc :emitted emitted)))))

(defn- apply-job-status
  [job event status]
  (let [job' (if (map? job) job {})
        updated-at (or (trim-s (:at event)) (now-iso))]
    (cond-> (assoc job'
                   :job/status status
                   :updated-at updated-at)
      (contains? event :request) (assoc :request (:request event))
      (contains? event :queue/class) (assoc :queue/class (keywordish (:queue/class event)))
      (contains? event :deadline-at) (assoc :deadline-at (trim-s (:deadline-at event)))
      (contains? event :attempt) (assoc :attempt (:attempt event))
      (contains? event :trace/id) (assoc :trace/id (trim-s (:trace/id event)))
      (contains? event :run/id) (assoc :run/id (trim-s (:run/id event)))
      (contains? event :session/id) (assoc :session/id (trim-s (:session/id event)))
      (terminal-statuses status) (assoc :completed-at updated-at))))

(defn- apply-event
  [state event]
  (let [state' (if (map? state) state (empty-state))
        type' (keywordish (:event/type event))
        job-id (trim-s (:job/id event))]
    (if-not (string? job-id)
      (update state' :events/count (fnil inc 0))
      (let [job0 (get-in state' [:jobs job-id])
            job1 (case type'
                   :job/submitted (apply-job-status job0 event :submitted)
                   :job/queued (apply-job-status job0 event :queued)
                   :job/running (apply-job-status job0 event :running)
                   :job/completed (apply-job-status job0 event :completed)
                   :job/failed (apply-job-status job0 event :failed)
                   :job/canceled (apply-job-status job0 event :canceled)
                   :job/expired (apply-job-status job0 event :expired)
                   :node/succeeded (let [base (apply-job-status job0 event (or (keywordish (:job/status job0))
                                                                                :running))
                                         checkpoint (normalize-node-checkpoint (:checkpoint event))]
                                     (cond-> base
                                       (map? checkpoint) (assoc :checkpoint checkpoint)))
                   :node/failed (apply-job-status job0 event :running)
                   :node/running (apply-job-status job0 event :running)
                   job0)]
        (-> state'
            (update :events/count (fnil inc 0))
            (assoc-in [:jobs job-id] job1))))))

(defn init-service
  "Creates execution graph service and rebuilds in-memory state from durable log."
  [cfg]
  (let [cfg' (normalize-config cfg)
        events (if (and (:enabled? cfg')
                        (= :fs-ednl (:store/type cfg')))
                 (read-events-from-file (:store/path cfg'))
                 [])
        rebuilt (reduce apply-event (empty-state) events)]
    {:execution-graph/config cfg'
     :execution-graph/state (atom rebuilt)}))

(defn service?
  [v]
  (and (map? v)
       (instance? clojure.lang.IAtom (:execution-graph/state v))
       (map? (:execution-graph/config v))))

(defn config
  [service]
  (if (service? service)
    (:execution-graph/config service)
    (normalize-config nil)))

(defn append-event!
  "Appends execution graph event to durable store and in-memory index."
  [service event]
  (let [cfg (config service)
        enabled? (true? (:enabled? cfg))]
    (cond
      (not (service? service))
      {:ok? false
       :error :execution-graph/not-initialized}

      (not enabled?)
      {:ok? false
       :error :execution-graph/disabled}

      (not (map? event))
      {:ok? false
       :error :execution-graph/invalid-event}

      :else
      (let [event' (cond-> {:execution.graph/version 1
                            :event/id (str (UUID/randomUUID))
                            :at (now-iso)}
                     (keyword? (keywordish (:event/type event)))
                     (assoc :event/type (keywordish (:event/type event)))
                     (string? (trim-s (:job/id event)))
                     (assoc :job/id (trim-s (:job/id event)))
                     (string? (trim-s (:run/id event)))
                     (assoc :run/id (trim-s (:run/id event)))
                     (string? (trim-s (:trace/id event)))
                     (assoc :trace/id (trim-s (:trace/id event))
                            :run/id (or (trim-s (:run/id event))
                                        (trim-s (:trace/id event))))
                     (string? (trim-s (:session/id event)))
                     (assoc :session/id (trim-s (:session/id event)))
                     (contains? event :request)
                     (assoc :request (:request event))
                     (contains? event :queue/class)
                     (assoc :queue/class (keywordish (:queue/class event)))
                     (contains? event :deadline-at)
                     (assoc :deadline-at (trim-s (:deadline-at event)))
                     (contains? event :attempt)
                     (assoc :attempt (:attempt event))
                     (contains? event :checkpoint)
                     (assoc :checkpoint (:checkpoint event))
                     (contains? event :details)
                     (assoc :details (:details event)))]
        (try
          (when (and (= :fs-ednl (:store/type cfg))
                     (string? (trim-s (:store/path cfg))))
            (write-event-line! (:store/path cfg) event'))
          (swap! (:execution-graph/state service) apply-event event')
          {:ok? true
           :event event'}
          (catch Throwable t
            {:ok? false
             :error :execution-graph/append-failed
             :message (.getMessage t)}))))))

(defn snapshot
  [service]
  (if-not (service? service)
    (empty-state)
    @(:execution-graph/state service)))

(defn inflight-jobs
  "Returns recoverable queued jobs from durable execution graph."
  [service]
  (if-not (service? service)
    []
    (->> (get (snapshot service) :jobs)
         (keep (fn [[job-id job]]
                 (let [status (keywordish (:job/status job))
                       request (when (map? job) (:request job))
                       class-k (keywordish (:queue/class job))]
                   (when (and (contains? inflight-statuses status)
                              (string? (trim-s job-id))
                              (map? request))
                     (cond-> {:job/id (trim-s job-id)
                              :job/status :queued
                              :request request
                              :attempt (if (integer? (:attempt job))
                                         (:attempt job)
                                         0)}
                       (keyword? class-k) (assoc :queue/class class-k)
                       (string? (trim-s (:deadline-at job))) (assoc :deadline-at (trim-s (:deadline-at job)))
                       (map? (:checkpoint job)) (assoc :checkpoint (:checkpoint job))
                       (string? (trim-s (:trace/id job))) (assoc :trace/id (trim-s (:trace/id job)))
                       (string? (trim-s (:run/id job))) (assoc :run/id (trim-s (:run/id job)))
                       (string? (trim-s (:session/id job))) (assoc :session/id (trim-s (:session/id job))))))))
         (sort-by :job/id)
         vec)))

(defn stop-service
  "No-op stop hook for execution graph service."
  [_service]
  nil)
