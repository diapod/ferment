(ns

    ^{:doc    "Shared telemetry helpers (counters, merges, simple rates)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.telemetry)

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
