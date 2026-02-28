(ns

    ^{:doc    "Shared telemetry helpers (counters, merges, simple rates)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.telemetry

  (:require [clojure.string :as str])

  (:import (java.time Instant)))

(def default-workflow-counters
  {:nodes/total 0
   :nodes/by-op {}
   :calls/total 0
   :calls/succeeded 0
   :calls/failed 0
   :calls/retries 0
   :calls/fallback-hops 0
   :calls/failure-types {}
   :quality/judge-used 0
   :quality/judge-pass 0
   :quality/judge-fail 0
   :quality/must-failed 0})

(defn merge-counters
  "Recursively merges telemetry maps by summing numeric leaves."
  [a b]
  (merge-with
   (fn [x y]
     (cond
       (and (map? x) (map? y)) (merge-counters x y)
       (and (number? x) (number? y)) (+ x y)
       :else y))
   (or a {})
   (or b {})))

(defn ensure-atom
  "Returns telemetry atom. Creates one with `initial` when `telemetry` is not an atom."
  ([telemetry]
   (ensure-atom telemetry {}))
  ([telemetry initial]
   (if (instance? clojure.lang.IAtom telemetry)
     telemetry
     (atom (if (map? initial) initial {})))))

(defn inc!
  [telemetry k]
  (swap! telemetry update k (fnil inc 0)))

(defn inc-in!
  [telemetry ks]
  (swap! telemetry update-in ks (fnil inc 0)))

(def ^:private default-lifecycle-max-events
  256)

(defonce ^:private lifecycle-state
  (atom {:seq 0
         :events []
         :totals {:total 0
                  :errors 0}
         :transitions {}
         :components {}
         :max-events default-lifecycle-max-events}))

(defn- lifecycle-component
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str str/trim not-empty)]
                  (when s (keyword s)))
    :else nil))

(defn- lifecycle-transition
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str str/trim not-empty)]
                  (when s (keyword s)))
    :else nil))

(defn- lifecycle-error?
  [transition details]
  (or (= :error transition)
      (true? (:error? details))
      (some? (:error details))))

(defn record-lifecycle!
  "Records lifecycle transition event in global telemetry registry.

  Event shape:
  - `:component` keyword (`:app`, `:runtime`, `:http`, `:model`, `:session`, ...)
  - `:transition` keyword (`:start`, `:stop`, `:error`, ...)
  - optional `details` map."
  ([component transition]
   (record-lifecycle! component transition nil))
  ([component transition details]
   (let [component' (or (lifecycle-component component) :unknown)
         transition' (or (lifecycle-transition transition) :unknown)
         details' (if (map? details) details nil)
         max-events (long (or (:max-events @lifecycle-state)
                              default-lifecycle-max-events))]
     (swap! lifecycle-state
            (fn [state]
              (let [seq' (inc (long (or (:seq state) 0)))
                    event (cond-> {:seq seq'
                                   :at (str (Instant/now))
                                   :component component'
                                   :transition transition'}
                            (map? details') (assoc :details details'))
                    events' (let [events0 (conj (vec (or (:events state) [])) event)
                                  overflow (max 0 (- (count events0) (int max-events)))]
                              (if (pos? overflow)
                                (subvec events0 overflow)
                                events0))
                    error? (lifecycle-error? transition' details')]
                (-> state
                    (assoc :seq seq'
                           :events events'
                           :max-events max-events)
                    (update-in [:totals :total] (fnil inc 0))
                    (cond-> error?
                      (update-in [:totals :errors] (fnil inc 0)))
                    (update-in [:transitions transition'] (fnil inc 0))
                    (update-in [:components component' transition'] (fnil inc 0)))))))
   nil))

(defn lifecycle-snapshot
  "Returns current lifecycle telemetry snapshot."
  []
  (let [state @lifecycle-state]
    {:total (long (or (get-in state [:totals :total]) 0))
     :errors (long (or (get-in state [:totals :errors]) 0))
     :transitions (or (:transitions state) {})
     :components (or (:components state) {})
     :events (vec (or (:events state) []))
     :max-events (long (or (:max-events state) default-lifecycle-max-events))}))

(defn clear-lifecycle!
  "Clears global lifecycle telemetry registry."
  []
  (reset! lifecycle-state
          {:seq 0
           :events []
           :totals {:total 0
                    :errors 0}
           :transitions {}
           :components {}
           :max-events default-lifecycle-max-events})
  nil)

(def ^:private default-queue-max-events
  512)

(def ^:private queue-wait-buckets
  [{:limit-ms 100 :bucket :<=100ms}
   {:limit-ms 500 :bucket :<=500ms}
   {:limit-ms 1000 :bucket :<=1s}
   {:limit-ms 5000 :bucket :<=5s}
   {:limit-ms 15000 :bucket :<=15s}])

(defonce ^:private queue-state
  (atom {:seq 0
         :events []
         :max-events default-queue-max-events
         :counters {:jobs/submitted 0
                    :jobs/started 0
                    :jobs/completed 0
                    :jobs/failed 0
                    :jobs/canceled 0
                    :jobs/expired 0}
         :wait-time-ms {:count 0
                        :sum 0.0
                        :max 0.0
                        :buckets {}}
         :depth {:last 0
                 :max 0}}))

(defn- queue-transition
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v str str/trim not-empty keyword)
    :else nil))

(defn- queue-depth
  [v]
  (when (number? v)
    (max 0 (long (Math/floor (double v))))))

(defn- queue-wait-ms
  [v]
  (when (number? v)
    (max 0.0 (double v))))

(defn- queue-wait-bucket
  [wait-ms]
  (or (some (fn [{:keys [limit-ms bucket]}]
              (when (<= wait-ms (double limit-ms))
                bucket))
            queue-wait-buckets)
      :>15s))

(defn- queue-counter-key
  [transition]
  (case transition
    :queued :jobs/submitted
    :running :jobs/started
    :completed :jobs/completed
    :failed :jobs/failed
    :canceled :jobs/canceled
    :expired :jobs/expired
    nil))

(defn record-queue-transition!
  "Records queue lifecycle transition and updates queue counters.

  Supported details keys:
  - `:job/id` (string)
  - `:queue/depth` (non-negative number)
  - `:wait-time-ms` (non-negative number, usually for `:running`)
  - `:count` (positive integer, batched transitions)"
  ([transition]
   (record-queue-transition! transition nil))
  ([transition details]
   (let [transition' (queue-transition transition)
         details' (if (map? details) details {})
         count' (let [n (:count details')]
                  (if (and (integer? n) (pos? n))
                    (long n)
                    1))
         depth' (queue-depth (:queue/depth details'))
         wait-ms' (queue-wait-ms (:wait-time-ms details'))
         counter-k (queue-counter-key transition')
         max-events (long (or (:max-events @queue-state)
                              default-queue-max-events))]
     (swap! queue-state
            (fn [state]
              (let [seq' (inc (long (or (:seq state) 0)))
                    event (cond-> {:seq seq'
                                   :at (str (Instant/now))
                                   :transition (or transition' :unknown)
                                   :count count'}
                            (some? (:job/id details')) (assoc :job/id (:job/id details'))
                            (number? depth') (assoc :queue/depth depth')
                            (number? wait-ms') (assoc :wait-time-ms wait-ms'))
                    events' (let [events0 (conj (vec (or (:events state) [])) event)
                                  overflow (max 0 (- (count events0) (int max-events)))]
                              (if (pos? overflow)
                                (subvec events0 overflow)
                                events0))
                    state' (cond-> (assoc state
                                          :seq seq'
                                          :events events'
                                          :max-events max-events)
                             (keyword? counter-k)
                             (update-in [:counters counter-k] (fnil + 0) count')

                             (number? depth')
                             (assoc-in [:depth :last] depth')

                             (number? depth')
                             (update-in [:depth :max] (fnil max 0) depth')

                             (number? wait-ms')
                             (update-in [:wait-time-ms :count] (fnil + 0) count')

                             (number? wait-ms')
                             (update-in [:wait-time-ms :sum] (fnil + 0.0) (* wait-ms' count'))

                             (number? wait-ms')
                             (update-in [:wait-time-ms :max] (fnil max 0.0) wait-ms')

                             (number? wait-ms')
                             (update-in [:wait-time-ms :buckets (queue-wait-bucket wait-ms')] (fnil + 0) count'))]
                state'))))
   nil))

(defn queue-snapshot
  "Returns queue telemetry snapshot."
  []
  (let [state @queue-state
        wait-count (long (or (get-in state [:wait-time-ms :count]) 0))
        wait-sum (double (or (get-in state [:wait-time-ms :sum]) 0.0))
        wait-avg (if (pos? wait-count) (/ wait-sum wait-count) 0.0)]
    {:counters (or (:counters state) {})
     :depth (or (:depth state) {:last 0 :max 0})
     :wait-time-ms (assoc (or (:wait-time-ms state) {})
                          :avg wait-avg)
     :events (vec (or (:events state) []))
     :max-events (long (or (:max-events state) default-queue-max-events))}))

(defn clear-queue!
  "Clears global queue telemetry registry."
  []
  (reset! queue-state
          {:seq 0
           :events []
           :max-events default-queue-max-events
           :counters {:jobs/submitted 0
                      :jobs/started 0
                      :jobs/completed 0
                      :jobs/failed 0
                      :jobs/canceled 0
                      :jobs/expired 0}
           :wait-time-ms {:count 0
                          :sum 0.0
                          :max 0.0
                          :buckets {}}
           :depth {:last 0
                   :max 0}})
  nil)
