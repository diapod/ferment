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
