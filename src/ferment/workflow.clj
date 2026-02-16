(ns

    ^{:doc    "Minimal plan evaluator for stratified capability execution."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.workflow

  (:require [clojure.walk :as walk]
            [ferment.contracts :as contracts]))

(def ^:private default-retry-policy
  {:same-cap-max 0
   :fallback-max 0})

(defn resolve-capability-id
  "Resolves capability id for a call node.

  Priority:
  1. explicit `:cap/id` in node
  2. first candidate from node `:dispatch`
  3. resolver routing by `:intent`"
  [resolver node]
  (or (:cap/id node)
      (first (get-in node [:dispatch :candidates]))
      (get-in resolver [:routing :intent->cap (:intent node)])))

(defn call-failed?
  "Best-effort failure check for values stored in plan environment."
  [v]
  (or (and (map? v) (some? (:error v)))
      (and (map? v) (= :error (contracts/result-type-of (:result v))))
      (and (map? v) (= :error (contracts/result-type-of v)))))

(defn should-run-node?
  "Evaluates optional `:when` predicate on a node."
  [node env]
  (let [w (:when node)]
    (cond
      (nil? w) true
      (boolean? w) w
      (keyword? w) (boolean (get env w))
      (map? w) (cond
                 (contains? w :failed?) (call-failed? (get env (:failed? w)))
                 (contains? w :present?) (contains? env (:present? w))
                 :else true)
      :else true)))

(defn normalize-call-result
  "Normalizes call result into env slot payload."
  [cap-id result]
  {:cap/id cap-id
   :result result
   :out (contracts/result-out-of result)
   :error (:error result)})

(defn- nonneg-int
  [v default]
  (if (and (int? v) (<= 0 v))
    v
    default))

(defn- resolve-retry-policy
  [resolver node]
  (let [routing-retry (get-in resolver [:routing :retry])
        node-retry    (get-in node [:dispatch :retry])]
    {:same-cap-max (nonneg-int (or (:same-cap-max node-retry)
                                   (:same-cap-max routing-retry)
                                   (:same-cap-max default-retry-policy))
                               0)
     :fallback-max (nonneg-int (or (:fallback-max node-retry)
                                   (:fallback-max routing-retry)
                                   (:fallback-max default-retry-policy))
                               0)}))

(defn- resolve-switch-on
  [resolver node]
  (let [routing (set (or (get-in resolver [:routing :switch-on]) #{}))
        local   (set (or (get-in node [:dispatch :switch-on]) #{}))]
    (into routing local)))

(defn- resolve-candidates
  [resolver node]
  (let [explicit (:cap/id node)
        listed   (vec (or (get-in node [:dispatch :candidates]) []))
        routed   (some-> (get-in resolver [:routing :intent->cap (:intent node)]) vector)]
    (vec
     (cond
       (keyword? explicit) [explicit]
       (seq listed) listed
       (seq routed) routed
       :else []))))

(defn- default-schema-check
  [result]
  (:ok? (contracts/validate-result result)))

(defn- invoke-check-fn
  [f call-node env result]
  (try
    (f call-node env result)
    (catch clojure.lang.ArityException _
      (f {:call-node call-node
          :env env
          :result result}))))

(defn- normalize-check
  [v]
  (cond
    (map? v) (assoc v :ok? (boolean (:ok? v)))
    (boolean? v) {:ok? v}
    :else {:ok? false}))

(defn- run-check
  [check-key call-node env result check-fns]
  (let [check-fn (or (get check-fns check-key)
                     (when (= :schema-valid check-key)
                       (fn [_ _ r] (default-schema-check r))))]
    (if (fn? check-fn)
      (let [raw (invoke-check-fn check-fn call-node env result)
            out (normalize-check raw)]
        (assoc out :check check-key))
      {:ok? false
       :check check-key
       :error :missing-check-fn})))

(defn- extract-judge-score
  [judge-out]
  (cond
    (number? judge-out) (double judge-out)
    (map? judge-out)    (some-> (:score judge-out) double)
    :else nil))

(defn- evaluate-done
  [call-node env result check-fns judge-fn]
  (let [done         (or (:done call-node) {})
        must-keys    (set (or (:must done) #{}))
        should-keys  (set (or (:should done) #{}))
        score-min    (double (or (:score-min done) 0.0))
        must-results (mapv #(run-check % call-node env result check-fns) must-keys)
        should-results (mapv #(run-check % call-node env result check-fns) should-keys)
        must-failed  (->> must-results (remove :ok?) (mapv :check))
        should-failed (->> should-results (remove :ok?) (mapv :check))
        should-score (if (seq should-results)
                       (/ (count (filter :ok? should-results))
                          (double (count should-results)))
                       1.0)
        judge-score  (when (fn? judge-fn)
                       (-> (invoke-check-fn judge-fn call-node env result)
                           extract-judge-score))
        score        (double (if (number? judge-score)
                               (/ (+ should-score judge-score) 2.0)
                               should-score))
        ok?          (and (empty? must-failed)
                          (>= score score-min))
        failure-type (cond
                       (some #{:schema-valid} must-failed) :schema/invalid
                       (not ok?) :eval/low-score
                       :else nil)]
    {:ok? ok?
     :failure/type failure-type
     :score score
     :must-failed must-failed
     :should-failed should-failed
     :judge/score judge-score
     :score-min score-min}))

(defn- call-failure-type
  [result done-eval]
  (or (get-in result [:error :type])
      (when-not (:ok? (contracts/validate-result result))
        :schema/invalid)
      (:failure/type done-eval)))

(defn- recoverable-failure?
  [failure-type switch-on]
  (and (keyword? failure-type)
       (contains? switch-on failure-type)))

(defn- materialize-emit-input
  [input env]
  (walk/postwalk
   (fn [node]
     (if (and (keyword? node) (contains? env node))
       (get env node)
       node))
   (contracts/materialize-plan input env)))

(defn execute-plan
  "Executes minimal plan AST with ops:
  - `:let`
  - `:call`
  - `:emit`

  Input map:
  - `:plan`       plan map with `:nodes`
  - `:resolver`   routing map
  - `:invoke-call` fn of `[call-node env] -> canonical result envelope`
  - `:env`        optional initial environment map

  Returns:
  - `{:ok? true, :env ..., :emitted ...}`"
  [{:keys [plan resolver invoke-call check-fns judge-fn env]
    :or   {env {}}}]
  (let [nodes (vec (:nodes plan))]
    (loop [idx 0
           env env
           emitted nil]
      (if (>= idx (count nodes))
        {:ok? true
         :env env
         :emitted emitted}
        (let [node (nth nodes idx)]
          (if-not (should-run-node? node env)
            (recur (inc idx) env emitted)
            (case (:op node)
              :let
              (let [value (contracts/materialize-plan (:value node) env)
                    env'  (if (keyword? (:as node))
                            (assoc env (:as node) value)
                            env)]
                (recur (inc idx) env' emitted))

              :call
              (let [base-node     (update node :input contracts/materialize-plan env)
                    retry-policy  (resolve-retry-policy resolver base-node)
                    switch-on     (resolve-switch-on resolver base-node)
                    candidates0   (resolve-candidates resolver base-node)
                    candidates    (vec (take (inc (:fallback-max retry-policy))
                                             candidates0))]
                (when-not (seq candidates)
                  (throw (ex-info "Unable to resolve capability candidates for call node"
                                  {:node node :resolver resolver})))
                (let [same-cap-attempts (inc (:same-cap-max retry-policy))
                      call-outcome
                      (loop [candidate-idx 0
                             last-outcome nil]
                        (if (>= candidate-idx (count candidates))
                          (or last-outcome
                              {:ok? false
                               :failure/type :unsupported/intent
                               :failure/recover? false})
                          (let [cap-id (nth candidates candidate-idx)
                                candidate-node (assoc base-node :cap/id cap-id)
                                candidate-outcome
                                (loop [attempt 1
                                       last-attempt nil]
                                  (if (> attempt same-cap-attempts)
                                    (or last-attempt
                                        {:ok? false
                                         :failure/type :schema/invalid
                                         :failure/recover? false
                                         :cap/id cap-id})
                                    (let [result (invoke-call candidate-node env)
                                          rtype  (contracts/result-type-of result)
                                          run*   (when (= :plan rtype)
                                                   (let [sub-plan (or (contracts/materialize-plan-result result)
                                                                      (contracts/result-plan-of result))]
                                                     (execute-plan {:plan sub-plan
                                                                    :resolver resolver
                                                                    :invoke-call invoke-call
                                                                    :check-fns check-fns
                                                                    :judge-fn judge-fn
                                                                    :env env})))
                                          slot-val (if run*
                                                     (assoc (normalize-call-result cap-id result)
                                                            :out (:emitted run*)
                                                            :plan/run run*)
                                                     (normalize-call-result cap-id result))
                                          verify-result (if run*
                                                          {:result {:type :value
                                                                    :out (:emitted run*)}}
                                                          result)
                                          done-eval (evaluate-done candidate-node env verify-result check-fns judge-fn)
                                          failure-type (call-failure-type result done-eval)
                                          recover? (recoverable-failure? failure-type switch-on)
                                          accepted? (not recover?)
                                          outcome {:ok? accepted?
                                                   :cap/id cap-id
                                                   :result result
                                                   :slot-val slot-val
                                                   :emitted (when run* (:emitted run*))
                                                   :plan/run run*
                                                   :attempt attempt
                                                   :done/eval done-eval
                                                   :failure/type failure-type
                                                   :failure/recover? recover?}]
                                      (if accepted?
                                        outcome
                                        (if (< attempt same-cap-attempts)
                                          (recur (inc attempt) outcome)
                                          outcome)))))]
                            (if (:ok? candidate-outcome)
                              candidate-outcome
                              (recur (inc candidate-idx) candidate-outcome)))))]
                  (if (:ok? call-outcome)
                    (let [env' (if (keyword? (:as node))
                                 (assoc env (:as node) (:slot-val call-outcome))
                                 env)
                          emitted' (or (:emitted call-outcome) emitted)]
                      (recur (inc idx) env' emitted'))
                    (throw (ex-info "Call node failed quality/dispatch policy"
                                    {:node node
                                     :outcome call-outcome
                                     :switch-on switch-on
                                     :retry-policy retry-policy
                                     :candidates candidates})))))

              :emit
              (let [output (materialize-emit-input (:input node) env)]
                (recur (inc idx) env output))

              (throw (ex-info "Unsupported plan node operation"
                              {:op (:op node)
                               :node node})))))))))
