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

(defn- keyword-coll?
  [v]
  (or (set? v)
      (and (sequential? v)
           (not (map? v))))
  )

(defn- validate-done-shape
  [done]
  (cond
    (nil? done) {:ok? true}
    (not (map? done))
    {:ok? false :reason :done/not-map}
    (and (contains? done :must)
         (not (keyword-coll? (:must done))))
    {:ok? false :reason :done/must-not-keyword-coll}
    (and (contains? done :must)
         (not (every? keyword? (:must done))))
    {:ok? false :reason :done/must-not-keywords}
    (and (contains? done :should)
         (not (keyword-coll? (:should done))))
    {:ok? false :reason :done/should-not-keyword-coll}
    (and (contains? done :should)
         (not (every? keyword? (:should done))))
    {:ok? false :reason :done/should-not-keywords}
    (and (contains? done :score-min)
         (not (number? (:score-min done))))
    {:ok? false :reason :done/score-min-not-number}
    (and (contains? done :score-min)
         (let [score-min (double (:score-min done))]
           (or (< score-min 0.0)
               (> score-min 1.0))))
    {:ok? false :reason :done/score-min-out-of-range}
    :else {:ok? true}))

(defn- validate-effects-shape
  [effects]
  (cond
    (nil? effects) {:ok? true}
    (not (map? effects))
    {:ok? false :reason :effects/not-map}
    (and (contains? effects :allowed)
         (not (keyword-coll? (:allowed effects))))
    {:ok? false :reason :effects/allowed-not-keyword-coll}
    (and (contains? effects :allowed)
         (not (every? keyword? (:allowed effects))))
    {:ok? false :reason :effects/allowed-not-keywords}
    :else {:ok? true}))

(defn- validate-intent-vs-protocol
  [request protocol]
  (let [intent (get-in request [:task :intent])
        intents (get protocol :intents)]
    (if (and (map? intents) (keyword? intent))
      (if (contains? intents intent)
        {:ok? true}
        {:ok? false :reason :intent/not-supported :intent intent})
      {:ok? true})))

(defn validate-request
  "Validates request envelope against protocol v0 request contract."
  ([request]
   (validate-request nil request))
  ([protocol request]
   (if-not (s/valid? :ferment.contracts/request request)
     {:ok? false
      :error :invalid-request
      :explain (s/explain-data :ferment.contracts/request request)}
     (let [intent-check (validate-intent-vs-protocol request protocol)]
       (if-not (:ok? intent-check)
         {:ok? false
          :error :invalid-request
          :reason (:reason intent-check)
          :intent (:intent intent-check)}
         (let [done-check (validate-done-shape (:done request))]
           (if-not (:ok? done-check)
             {:ok? false
              :error :invalid-request
              :reason (:reason done-check)}
             (let [effects-check (validate-effects-shape (:effects request))]
               (if-not (:ok? effects-check)
                 {:ok? false
                  :error :invalid-request
                  :reason (:reason effects-check)}
                 {:ok? true})))))))))

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

(defn- score-in-range?
  [v min-score max-score]
  (and (number? v)
       (<= (double min-score) (double v) (double max-score))))

(defn- validate-eval-grade-out
  [out contract]
  (let [required (set (or (:required contract) [:score]))
        [min-score max-score] (or (:score/range contract) [0.0 1.0])]
    (cond
    (not (map? out))
    {:ok? false
     :reason :eval/grade-out-not-map}

    (and (contains? required :score)
         (not (contains? out :score)))
    {:ok? false
     :reason :eval/grade-score-missing}

    (not (number? (:score out)))
    {:ok? false
     :reason :eval/grade-score-not-number}

    (not (score-in-range? (:score out) min-score max-score))
    {:ok? false
     :reason :eval/grade-score-out-of-range}

    (and (contains? out :retry?)
         (not (boolean? (:retry? out))))
    {:ok? false
     :reason :eval/grade-retry-not-boolean}

    (and (contains? out :violations)
         (not (or (set? (:violations out))
                  (sequential? (:violations out)))))
    {:ok? false
     :reason :eval/grade-violations-not-coll}

    :else
    {:ok? true})))

(defn- keyword-coll-of?
  [v]
  (and (keyword-coll? v)
       (every? keyword? v)))

(defn- nonneg-int?
  [v]
  (and (integer? v) (<= 0 v)))

(defn- validate-route-decide-out
  [out]
  (let [allowed-top-keys #{:cap/id :dispatch :constraints :done :budget :effects}
        unknown-top-keys (seq (remove allowed-top-keys (keys out)))
        dispatch-map (if (map? (:dispatch out)) (:dispatch out) nil)
        retry-map   (some-> dispatch-map :retry)
        allowed-dispatch-keys #{:candidates :checks :switch-on :retry}
        unknown-dispatch-keys (when dispatch-map
                                (seq (remove allowed-dispatch-keys (keys dispatch-map))))]
    (cond
      (not (map? out))
      {:ok? false
       :reason :route/decide-out-not-map}

      (seq unknown-top-keys)
      {:ok? false
       :reason :route/decide-unknown-keys}

      (not (contains? out :cap/id))
      {:ok? false
       :reason :route/decide-target-missing}

      (not (keyword? (:cap/id out)))
      {:ok? false
       :reason :route/decide-cap-not-keyword}

      (and (contains? out :dispatch)
           (not (map? (:dispatch out))))
      {:ok? false
       :reason :route/decide-dispatch-not-map}

      (seq unknown-dispatch-keys)
      {:ok? false
       :reason :route/decide-dispatch-unknown-keys}

      (and dispatch-map
           (contains? dispatch-map :candidates)
           (not (keyword-coll-of? (:candidates dispatch-map))))
      {:ok? false
       :reason :route/decide-candidates-not-keywords}

      (and dispatch-map
           (contains? dispatch-map :checks)
           (not (keyword-coll-of? (:checks dispatch-map))))
      {:ok? false
       :reason :route/decide-checks-not-keywords}

      (and dispatch-map
           (contains? dispatch-map :switch-on)
           (not (keyword-coll-of? (:switch-on dispatch-map))))
      {:ok? false
       :reason :route/decide-switch-on-not-keywords}

      (and (some? retry-map) (not (map? retry-map)))
      {:ok? false
       :reason :route/decide-retry-not-map}

      (and (map? retry-map)
           (contains? retry-map :same-cap-max)
           (not (nonneg-int? (:same-cap-max retry-map))))
      {:ok? false
       :reason :route/decide-retry-same-cap-max-not-nonneg-int}

      (and (map? retry-map)
           (contains? retry-map :fallback-max)
           (not (nonneg-int? (:fallback-max retry-map))))
      {:ok? false
       :reason :route/decide-retry-fallback-max-not-nonneg-int}

      :else
      {:ok? true})))

(defn- validate-intent-result-shape
  [protocol intent result]
  (let [judge-intent (or (get-in protocol [:quality/judge :intent])
                         :eval/grade)
         contract     (or (get-in protocol [:intents intent :result/contract])
                          {:type :value
                           :required [:score]
                           :score/range [0.0 1.0]})
         contract-kind (:contract/kind contract)]
    (cond
      (= intent judge-intent)
      (cond
        (contains? result :error)
        {:ok? false
         :reason :eval/grade-error-not-allowed}

        (not= (:type contract) (result-type-of result))
        {:ok? false
         :reason :eval/grade-result-type-not-value
         :result/type (result-type-of result)}

        :else
        (validate-eval-grade-out (result-out-of result) contract))

      (and (= :route/decide intent)
           (= :route/decide contract-kind))
      (cond
        (contains? result :error)
        {:ok? false
         :reason :route/decide-error-not-allowed}

        (and (keyword? (:type contract))
             (not= (:type contract) (result-type-of result)))
        {:ok? false
         :reason :route/decide-result-type-not-value
         :result/type (result-type-of result)}

        :else
        (validate-route-decide-out (result-out-of result)))

      :else
      {:ok? true})))

(defn validate-result
  "Validates result envelope and semantic branch shape.

  Canonical envelope only: `{:result {...}}` or `{:error {...}}`."
  ([result]
   (validate-result nil result))
  ([protocol result]
   (validate-result protocol nil result))
  ([protocol intent result]
   (cond
     (not (map? result))
     {:ok? false
      :error :invalid-result
      :reason :not-a-map}

     (not (canonical-result-shape-valid? result))
     {:ok? false
      :error :invalid-result
      :reason :invalid-envelope-shape}

     (contains? result :result)
     (let [allowed-types (set (or (:result/types protocol) result-types))
           rtype (get-in result [:result :type])]
       (if-not (contains? allowed-types rtype)
         {:ok? false
          :error :invalid-result
          :reason :result/type-not-allowed
          :result/type rtype}
         (let [intent-check (validate-intent-result-shape protocol intent result)]
           (if (:ok? intent-check)
             {:ok? true}
             {:ok? false
              :error :invalid-result
              :reason (:reason intent-check)
              :intent intent
              :result/type rtype}))))

     :else
     (let [intent-check (validate-intent-result-shape protocol intent result)]
       (if (:ok? intent-check)
         {:ok? true}
         {:ok? false
          :error :invalid-result
          :reason (:reason intent-check)
          :intent intent})))))

(defn invoke-with-contract
  "Runs `invoke-fn` under protocol validation with bounded retries.

  `invoke-fn` gets `(invoke-fn request attempt-no)` and should return a result map.
  Returns either:
  - `{:ok? true  :attempt n :result ...}`
  - `{:ok? false :error ... :attempts n ...}`."
  ([invoke-fn request]
   (invoke-with-contract invoke-fn request {}))
  ([invoke-fn request {:keys [max-attempts protocol]
                       :or   {max-attempts 3}}]
   (let [request-check (validate-request protocol request)
         max-attempts  (long (max 1 max-attempts))]
     (if-not (:ok? request-check)
       request-check
       (loop [attempt 1]
         (let [result (invoke-fn request attempt)
               check  (validate-result protocol
                                       (get-in request [:task :intent])
                                       result)]
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
