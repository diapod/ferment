(ns

    ^{:doc    "Domain contracts for Ferment protocol messages and invoke/retry flow."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.contracts

  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

;;
;; Protocol envelope (canonical, low-entropy)
;;

(def ^:const result-types
  #{:value :plan :stream :error})

(s/def :ferment.contracts/proto pos-int?)
(s/def :ferment.contracts/id string?)
(s/def :ferment.contracts/turn nat-int?)
(s/def :ferment.contracts/intent keyword?)
(s/def :ferment.contracts/trace
  (s/keys :req-un [:ferment.contracts/id]
          :opt-un [:ferment.contracts/turn]))
(s/def :ferment.contracts/role keyword?)
(s/def :ferment.contracts/input map?)
(s/def :ferment.contracts/requires map?)
(s/def :ferment.contracts/task
  (s/keys :req-un [:ferment.contracts/intent]
          :opt-un [:ferment.contracts/requires]))

(s/def :ferment.contracts/request-id string?)
(s/def :ferment.contracts/session-id string?)
(s/def :ferment.contracts/cap-id keyword?)
(s/def :ferment.contracts/ctx map?)
(s/def :ferment.contracts/constraints map?)
(s/def :ferment.contracts/done map?)
(s/def :ferment.contracts/context map?)
(s/def :ferment.contracts/budget map?)
(s/def :ferment.contracts/effects map?)

(s/def :ferment.contracts/request
  (s/keys :req-un [:ferment.contracts/proto
                   :ferment.contracts/trace
                   :ferment.contracts/task
                   :ferment.contracts/input]
          :opt-un [:ferment.contracts/request-id
                   :ferment.contracts/session-id
                   :ferment.contracts/cap-id
                   :ferment.contracts/ctx
                   :ferment.contracts/role
                   :ferment.contracts/constraints
                   :ferment.contracts/done
                   :ferment.contracts/context
                   :ferment.contracts/budget
                   :ferment.contracts/effects]))

(s/def :ferment.contracts/result-type result-types)
(s/def :ferment.contracts/result map?)

(defn validate-request
  "Validates request envelope against protocol v0 request contract."
  [request]
  (if (s/valid? :ferment.contracts/request request)
    {:ok? true}
    {:ok? false
     :error :invalid-request
     :explain (s/explain-data :ferment.contracts/request request)}))

(defn result-type-of
  "Returns normalized result type for canonical envelopes."
  [result]
  (cond
    (contains? result :result) (some-> result :result :type)
    (contains? result :error)  :error))

(defn result-out-of
  "Returns normalized `:out` map from result."
  [result]
  (get-in result [:result :out]))

(defn result-plan-of
  "Returns normalized plan value from result."
  [result]
  (get-in result [:result :plan]))

(defn result-bindings-of
  "Returns normalized optional bindings map for plan materialization."
  [result]
  (get-in result [:result :bindings]))

(defn slot-node?
  "Returns true when `v` is a plan injection slot."
  [v]
  (and (map? v)
       (or (keyword? (:slot/id v))
           (vector? (:slot/id v)))
       (= 1 (count v))))

(defn- binding-for-slot
  [bindings slot-id]
  (cond
    (keyword? slot-id) (get bindings slot-id)
    (vector? slot-id)  (get-in bindings slot-id)
    :else nil))

(defn materialize-plan
  "Injects bindings into plan placeholders represented as `{:slot/id <kw-or-path>}`."
  [plan bindings]
  (if-not (map? bindings)
    plan
    (walk/postwalk
     (fn [node]
       (if (slot-node? node)
         (let [value (binding-for-slot bindings (:slot/id node))]
           (if (nil? value) node value))
         node))
     plan)))

(defn materialize-plan-result
  "Returns a materialized plan from result (if result type is `:plan`)."
  [result]
  (when (= :plan (result-type-of result))
    (materialize-plan (result-plan-of result)
                      (result-bindings-of result))))

(defn- canonical-result-shape-valid?
  "Validates response envelope with exactly one of `:result` or `:error`."
  [result]
  (let [has-result? (contains? result :result)
        has-error?  (contains? result :error)]
    (cond
      (and has-result? has-error?) false
      has-result?
      (let [r (:result result)
            t (:type r)]
        (and (map? r)
             (contains? result-types t)
             (case t
               :value  (contains? r :out)
               :plan   (contains? r :plan)
               :stream (contains? r :stream)
               :error  (contains? r :error)
               false)))
      has-error?  (and (map? (:error result))
                       (keyword? (get-in result [:error :type])))
      :else false)))

(defn validate-result
  "Validates result envelope and semantic branch shape.

  Canonical envelope only: `{:result {...}}` or `{:error {...}}`."
  [result]
  (cond
    (not (map? result))
    {:ok? false
     :error :invalid-result
     :reason :not-a-map}

    (not (canonical-result-shape-valid? result))
    {:ok? false
     :error :invalid-result
     :reason :invalid-envelope-shape}

    :else
    {:ok? true}))

(defn invoke-with-contract
  "Runs `invoke-fn` under protocol validation with bounded retries.

  `invoke-fn` gets `(invoke-fn request attempt-no)` and should return a result map.
  Returns either:
  - `{:ok? true  :attempt n :result ...}`
  - `{:ok? false :error ... :attempts n ...}`."
  ([invoke-fn request]
   (invoke-with-contract invoke-fn request {}))
  ([invoke-fn request {:keys [max-attempts]
                       :or   {max-attempts 3}}]
   (let [request-check (validate-request request)
         max-attempts  (long (max 1 max-attempts))]
     (if-not (:ok? request-check)
       request-check
       (loop [attempt 1]
         (let [result (invoke-fn request attempt)
               check  (validate-result result)]
           (if (:ok? check)
             {:ok? true
              :attempt attempt
              :result result}
             (if (< attempt max-attempts)
               (recur (inc attempt))
               {:ok? false
                :error :invalid-result-after-retries
                :attempts attempt
                :last-result result
                :last-check  check}))))))))
