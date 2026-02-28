(ns

    ^{:doc    "In-memory job queue service for asynchronous orchestration flows."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.queue

  (:require [clojure.string :as str]
            [ferment.telemetry :as telemetry])

  (:import (java.time Instant)))

(def job-statuses
  #{:queued :running :completed :failed :canceled :expired})

(def terminal-statuses
  #{:completed :failed :canceled :expired})

(def ^:private active-statuses
  #{:queued :running})

(def ^:private default-max-size
  256)

(def ^:private default-max-events
  256)

(def ^:private default-poll-interval-ms
  25)

(def ^:private default-timeout-ms
  15000)

(def ^:private default-workers
  1)

(def ^:private default-classes
  [:interactive :batch])

(def ^:private transition-map
  {nil #{:queued}
   :queued #{:running :canceled :expired}
   :running #{:completed :failed :canceled :expired}
   :completed #{}
   :failed #{}
   :canceled #{}
   :expired #{}})

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v trim-s keyword)
    :else nil))

(defn- pos-int
  [v]
  (when (and (integer? v) (pos? v))
    (int v)))

(defn- nonneg-long
  [v]
  (let [n (cond
            (integer? v) v
            (number? v) (long (Math/floor (double v)))
            (string? v) (try
                          (Long/parseLong (str/trim v))
                          (catch Throwable _ nil))
            :else nil)]
    (when (and (integer? n) (<= 0 n))
      (long n))))

(defn- classes*
  [cfg]
  (let [provided (if (sequential? (:classes cfg))
                   (into [] (keep keywordish) (:classes cfg))
                   [])
        classes  (if (seq provided) (vec (distinct provided)) default-classes)]
    (if (seq classes) classes default-classes)))

(defn- priority-order*
  [cfg classes]
  (let [provided (if (sequential? (:priority-order cfg))
                   (into [] (keep keywordish) (:priority-order cfg))
                   [])
        normalized (->> provided
                        (filter (set classes))
                        distinct
                        vec)
        rest-classes (->> classes
                          (remove (set normalized))
                          vec)]
    (vec (concat normalized rest-classes))))

(defn normalize-config
  "Normalizes queue configuration into canonical shape."
  [cfg]
  (let [cfg'          (if (map? cfg) cfg {})
        enabled?      (if (contains? cfg' :enabled?)
                        (boolean (:enabled? cfg'))
                        false)
        classes       (classes* cfg')
        classes-set   (set classes)
        priority      (priority-order* cfg' classes)
        default-class (or (keywordish (:default-class cfg'))
                          (first priority)
                          (first classes)
                          :interactive)
        retry-cfg     (if (map? (:retry cfg'))
                        (:retry cfg')
                        {})
        workers       (or (pos-int (:workers cfg'))
                          (when enabled? default-workers)
                          0)
        clock         (if (ifn? (:clock cfg'))
                        (:clock cfg')
                        #(System/currentTimeMillis))]
    {:enabled?          enabled?
     :max-size          (or (pos-int (:max-size cfg')) default-max-size)
     :classes           classes
     :classes/set       classes-set
     :priority-order    priority
     :default-class     (if (contains? classes-set default-class)
                          default-class
                          (first classes))
     :max-events        (or (pos-int (:max-events cfg')) default-max-events)
     :workers           workers
     :poll-interval-ms  (or (pos-int (:poll-interval-ms cfg')) default-poll-interval-ms)
     :default-timeout-ms (or (pos-int (:default-timeout-ms cfg')) default-timeout-ms)
     :retry             {:max-attempts    (or (pos-int (:max-attempts retry-cfg)) 1)
                         :base-backoff-ms (or (pos-int (:base-backoff-ms retry-cfg)) 100)
                         :jitter-ms       (or (nonneg-long (:jitter-ms retry-cfg)) 0)}
     :default-deadline-ms (nonneg-long (:default-deadline-ms cfg'))
     :max-deadline-ms   (nonneg-long (:max-deadline-ms cfg'))
     :clock             clock}))

(defn default-config
  "Returns default queue config merged with optional overrides."
  ([] (normalize-config {}))
  ([overrides]
   (normalize-config (merge {} (if (map? overrides) overrides {})))))

(defn- iso-now
  [epoch-ms]
  (str (Instant/ofEpochMilli (long epoch-ms))))

(defn- iso->ms
  [v]
  (try
    (.toEpochMilli (Instant/parse (str v)))
    (catch Throwable _ nil)))

(defn- queue-empty?
  [q]
  (or (nil? q) (zero? (count q))))

(defn- empty-queues
  [classes]
  (zipmap classes (repeat clojure.lang.PersistentQueue/EMPTY)))

(defn- init-state
  [cfg]
  {:jobs {}
   :jobs/active 0
   :queues (empty-queues (:classes cfg))
   :events []})

(defn init-service
  "Creates in-memory queue service."
  [cfg]
  (let [normalized (normalize-config cfg)]
    {:queue/config normalized
     :queue/state (atom (init-state normalized))
     :queue/seq   (atom 0)}))

(defn service?
  "Returns true when `v` looks like queue service map."
  [v]
  (and (map? v)
       (instance? clojure.lang.IAtom (:queue/state v))
       (instance? clojure.lang.IAtom (:queue/seq v))
       (map? (:queue/config v))))

(defn config
  "Returns normalized queue config from service."
  [service]
  (if (service? service)
    (:queue/config service)
    (normalize-config nil)))

(defn snapshot
  "Returns queue state snapshot (for diagnostics/tests)."
  [service]
  (if-not (service? service)
    {:jobs {}
     :queues {}
     :events []}
    (let [state @(:queue/state service)
          jobs  (if (map? (:jobs state)) (:jobs state) {})
          by-status (reduce (fn [acc {:keys [job/status]}]
                              (if (keyword? status)
                                (update acc status (fnil inc 0))
                                acc))
                            {}
                            (vals jobs))]
      {:jobs jobs
       :jobs/by-status by-status
       :jobs/active (long (or (:jobs/active state) 0))
       :queues (:queues state)
       :events (vec (:events state))})))

(defn- now-ms
  [service]
  (long ((get-in service [:queue/config :clock]))))

(defn- append-event
  [state cfg event]
  (let [max-events (or (pos-int (:max-events cfg)) default-max-events)
        events0    (conj (vec (or (:events state) [])) event)
        overflow   (max 0 (- (count events0) max-events))
        events'    (if (pos? overflow)
                     (subvec events0 overflow)
                     events0)]
    (assoc state :events events')))

(defn- queued-depth
  [state]
  (reduce-kv (fn [acc _ job]
               (if (= :queued (:job/status job))
                 (inc acc)
                 acc))
             0
             (or (:jobs state) {})))

(defn- queue-depth
  [service]
  (if-not (service? service)
    0
    (queued-depth @(:queue/state service))))

(defn- record-transition!
  [service transition details]
  (let [details' (if (map? details) details {})
        with-depth (if (contains? details' :queue/depth)
                     details'
                     (assoc details' :queue/depth (queue-depth service)))]
    (telemetry/record-queue-transition! transition with-depth)))

(defn- active-job?
  [job]
  (contains? active-statuses (:job/status job)))

(defn- active-count
  [jobs]
  (reduce-kv (fn [acc _ job]
               (if (active-job? job) (inc acc) acc))
             0
             jobs))

(defn- state-active-count
  [state]
  (let [n (:jobs/active state)]
    (if (and (integer? n) (<= 0 n))
      n
      (active-count (:jobs state)))))

(defn- active-delta
  [from to]
  (let [from-active? (contains? active-statuses from)
        to-active?   (contains? active-statuses to)]
    (cond
      (= from-active? to-active?) 0
      to-active?                  1
      :else                      -1)))

(defn- next-job-id!
  [service]
  (str "job/" (swap! (:queue/seq service) inc)))

(defn- resolve-deadline-ms
  [cfg opts]
  (let [raw-deadline (or (nonneg-long (:deadline-ms opts))
                         (nonneg-long (:job/deadline-ms opts))
                         (:default-deadline-ms cfg))
        max-deadline (:max-deadline-ms cfg)]
    (cond
      (nil? raw-deadline) nil
      (nil? max-deadline) raw-deadline
      :else (min raw-deadline max-deadline))))

(defn- resolve-queue-class
  [cfg opts]
  (let [requested (or (keywordish (:queue/class opts))
                      (keywordish (:job/class opts))
                      (:default-class cfg))
        classes-set (or (:classes/set cfg)
                        (set (:classes cfg)))]
    (if (contains? classes-set requested)
      requested
      (:default-class cfg))))

(defn- queue-conj
  [state class job-id]
  (update-in state [:queues class]
             (fn [q]
               (conj (or q clojure.lang.PersistentQueue/EMPTY) job-id))))

(defn- transition-allowed?
  [from to]
  (contains? (get transition-map from #{}) to))

(defn- apply-transition
  [state cfg job-id to now-iso now-ms details]
  (let [job  (get-in state [:jobs job-id])
        from (:job/status job)
        delta (active-delta from to)
        job' (cond-> (assoc job
                            :job/status to
                            :updated-at now-iso)
               (= to :running)
               (-> (assoc :started-at now-iso)
                   (update :attempt (fnil inc 0)))
               (= to :completed)
               (assoc :completed-at now-iso
                      :result (:result details)
                      :error nil)
               (= to :failed)
               (assoc :completed-at now-iso
                      :error (:error details))
               (= to :canceled)
               (assoc :completed-at now-iso
                      :cancel/reason (or (:reason details) :request))
               (= to :expired)
               (assoc :completed-at now-iso
                      :error {:type :queue/deadline-exceeded
                              :message "Job deadline exceeded."
                              :deadline-at (:deadline-at job)}))
        state' (cond-> (assoc-in state [:jobs job-id] job')
                 (not (zero? delta))
                 (update :jobs/active
                         (fn [n]
                           (max 0 (+ (long (or n 0)) delta)))))
        event  {:at now-iso
                :at-ms now-ms
                :job/id job-id
                :from from
                :to to}]
    [(append-event state' cfg event) job']))

(defn- expire-queued-jobs
  [state cfg now-ms]
  (let [now-iso (iso-now now-ms)]
    (reduce-kv
     (fn [{:keys [state expired]} job-id job]
       (if (and (= :queued (:job/status job))
                (string? (:deadline-at job))
                (let [deadline-ms (iso->ms (:deadline-at job))]
                  (and (number? deadline-ms)
                       (> now-ms deadline-ms))))
         (let [[state' _] (apply-transition state cfg job-id :expired now-iso now-ms nil)]
           {:state state'
            :expired (inc expired)})
         {:state state
          :expired expired}))
     {:state state
      :expired 0}
     (:jobs state))))

(defn submit!
  "Submits new job into queue.

  Returns:
  - `{:ok? true :job <job-map>}`
  - `{:ok? false :error ...}`"
  ([service request]
   (submit! service request nil))
  ([service request opts]
   (let [cfg (config service)]
     (if-not (service? service)
       {:ok? false
        :error :queue/not-initialized}
       (if-not (:enabled? cfg)
         {:ok? false
          :error :queue/disabled}
         (let [opts'    (if (map? opts) opts {})
               class    (resolve-queue-class cfg opts')
               now-ms'  (now-ms service)
               now-iso  (iso-now now-ms')
               deadline-ms (resolve-deadline-ms cfg opts')
               deadline-at (when (number? deadline-ms)
                             (iso-now (+ now-ms' deadline-ms)))
               job-id   (next-job-id! service)
               out      (volatile! nil)]
           (swap! (:queue/state service)
                  (fn [state]
                    (let [current-active (state-active-count state)]
                      (if (>= current-active (:max-size cfg))
                        (do
                          (vreset! out {:ok? false
                                        :error :queue/full
                                        :max-size (:max-size cfg)})
                          state)
                        (let [job {:job/id job-id
                                   :job/status :queued
                                   :queue/class class
                                   :request request
                                   :attempt 0
                                   :submitted-at now-iso
                                   :updated-at now-iso
                                   :deadline-at deadline-at}
                              state' (-> state
                                         (assoc-in [:jobs job-id] job)
                                         (update :jobs/active
                                                 (fn [n]
                                                   (inc (long (or n 0)))))
                                         (queue-conj class job-id)
                                         (append-event cfg {:at now-iso
                                                            :at-ms now-ms'
                                                            :job/id job-id
                                                            :from nil
                                                            :to :queued}))]
                          (vreset! out {:ok? true :job job})
                          state')))))
           (let [result @out]
             (when (:ok? result)
               (record-transition! service :queued {:job/id (get-in result [:job :job/id])}))
             result)))))))

(defn get-job
  "Returns job by id."
  [service job-id]
  (if-not (service? service)
    {:ok? false
     :error :queue/not-initialized}
    (let [job-id' (trim-s job-id)
          job (get-in @(:queue/state service) [:jobs job-id'])]
      (if (map? job)
        {:ok? true :job job}
        {:ok? false
         :error :queue/job-not-found
         :job/id job-id'}))))

(defn poll!
  "Alias for `get-job`."
  [service job-id]
  (get-job service job-id))

(defn- pop-next-for-class
  [state cfg class now-ms]
  (loop [s state]
    (let [q (get-in s [:queues class] clojure.lang.PersistentQueue/EMPTY)]
      (if (queue-empty? q)
        [s nil]
        (let [job-id (peek q)
              q' (pop q)
              s1 (assoc-in s [:queues class] q')
              job (get-in s1 [:jobs job-id])]
          (cond
            (not (map? job))
            (recur s1)

            (not= :queued (:job/status job))
            (recur s1)

            (and (string? (:deadline-at job))
                 (let [deadline-ms (iso->ms (:deadline-at job))]
                   (and (number? deadline-ms)
                        (> now-ms deadline-ms))))
            (let [now-iso (iso-now now-ms)
                  [s2 _] (apply-transition s1 cfg job-id :expired now-iso now-ms nil)]
              (recur s2))

            :else
            (let [now-iso (iso-now now-ms)
                  [s2 running-job] (apply-transition s1 cfg job-id :running now-iso now-ms nil)]
              [s2 running-job])))))))

(defn start-next!
  "Starts next queued job according to configured priority order."
  [service]
  (if-not (service? service)
    {:ok? false
     :error :queue/not-initialized}
    (let [cfg (config service)
          now-ms' (now-ms service)
          out (volatile! nil)]
      (swap! (:queue/state service)
             (fn [state]
               (let [{:keys [state expired]} (expire-queued-jobs state cfg now-ms')
                     state0 state]
                 (loop [classes (:priority-order cfg)
                        s state0]
                   (if (empty? classes)
                     (do
                       (vreset! out {:ok? false
                                     :error :queue/empty})
                       (vswap! out assoc :expired expired)
                       s)
                     (let [class (first classes)
                           [s' job] (pop-next-for-class s cfg class now-ms')]
                       (if (map? job)
                         (do
                           (vreset! out {:ok? true
                                         :job job
                                         :expired expired})
                           s')
                         (recur (rest classes) s'))))))))
      (let [result @out]
        (when (pos-int (:expired result))
          (record-transition! service :expired {:count (:expired result)}))
        (when (:ok? result)
          (let [job       (:job result)
                submitted (iso->ms (:submitted-at job))
                started   (iso->ms (:started-at job))
                wait-ms   (when (and (number? submitted) (number? started))
                            (max 0.0 (- (double started) (double submitted))))]
            (record-transition! service
                                :running
                                (cond-> {:job/id (:job/id job)}
                                  (number? wait-ms) (assoc :wait-time-ms wait-ms)))))
        result))))

(defn- transition-by-id!
  [service job-id to details]
  (let [cfg (config service)]
    (if-not (service? service)
      {:ok? false
       :error :queue/not-initialized}
      (let [job-id' (trim-s job-id)
            now-ms' (now-ms service)
            now-iso (iso-now now-ms')
            out (volatile! nil)]
        (swap! (:queue/state service)
               (fn [state]
                 (if-not (contains? (:jobs state) job-id')
                   (do
                     (vreset! out {:ok? false
                                   :error :queue/job-not-found
                                   :job/id job-id'})
                     state)
                   (let [from (get-in state [:jobs job-id' :job/status])]
                     (if-not (transition-allowed? from to)
                       (do
                         (vreset! out {:ok? false
                                       :error :queue/invalid-transition
                                       :job/id job-id'
                                       :from from
                                       :to to})
                         state)
                       (let [[state' job'] (apply-transition state cfg job-id' to now-iso now-ms' details)]
                         (vreset! out {:ok? true
                                       :job job'})
                         state'))))))
        (let [result @out]
          (when (:ok? result)
            (record-transition! service to {:job/id (get-in result [:job :job/id])}))
          result)))))

(defn complete!
  "Marks running job as completed."
  [service job-id result]
  (transition-by-id! service job-id :completed {:result result}))

(defn fail!
  "Marks running job as failed."
  [service job-id error]
  (transition-by-id! service job-id :failed {:error error}))

(defn cancel!
  "Cancels queued or running job."
  ([service job-id]
   (cancel! service job-id nil))
  ([service job-id reason]
   (transition-by-id! service job-id :canceled {:reason reason})))
