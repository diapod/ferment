(ns

    ^{:doc    "Minimal plan evaluator for stratified capability execution."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.workflow

  (:require [clojure.walk :as walk]
            [ferment.contracts :as contracts]
            [ferment.roles :as roles]))

(def ^:private default-retry-policy
  {:same-cap-max 0
   :fallback-max 0})

(defn- keyword-set
  [v]
  (cond
    (set? v) (into #{} (filter keyword?) v)
    (sequential? v) (into #{} (filter keyword?) v)
    (keyword? v) #{v}
    :else #{}))

(defn- call-requires
  [node]
  (let [requires0 (if (map? (:requires node))
                    (:requires node)
                    {})
        requires1 (cond-> requires0
                    (keyword? (get-in node [:output :schema]))
                    (assoc :out-schema (get-in node [:output :schema]))
                    (keyword? (or (:result/type node)
                                  (get-in node [:expect :result/type])))
                    (assoc :result/type (or (:result/type node)
                                            (get-in node [:expect :result/type]))))
        requires2 (contracts/normalize-requires requires1)]
    (if (map? requires2) requires2 {})))

(defn- normalize-call-node
  [node]
  (let [requires (call-requires node)]
    (if (seq requires)
      (assoc node :requires requires)
      node)))

(defn- requested-result-type
  [node]
  (or (:result/type (call-requires node))
      (:result/type node)
      (get-in node [:expect :result/type])))

(defn- requested-effects
  [node]
  (let [declared (keyword-set (or (:effects/allowed node)
                                  (get-in node [:effects :allowed])))
        required (keyword-set (get-in (call-requires node) [:effects/allowed]))]
    (into declared required)))

(defn- auth-user
  [env]
  (let [env' (if (map? env) env {})]
    (or (when (map? (:auth/user env'))
          (:auth/user env'))
        (when (map? (get-in env' [:workflow/auth :user]))
          (get-in env' [:workflow/auth :user])))))

(defn- public-auth-user
  [user]
  (when (map? user)
    (let [roles' (->> (keyword-set (or (:user/roles user)
                                       (:roles user)))
                      sort
                      vec)]
      (cond-> {}
        (some? (:user/id user)) (assoc :user/id (:user/id user))
        (some? (:user/email user)) (assoc :user/email (:user/email user))
        (some? (:user/account-type user)) (assoc :user/account-type (:user/account-type user))
        (seq roles') (assoc :user/roles roles')))))

(defn- roles-config
  [resolver env]
  (let [env'      (if (map? env) env {})
        resolver' (if (map? resolver) resolver {})]
    (or (when (map? (:roles/config env'))
          (:roles/config env'))
        (when (map? (:roles env'))
          (:roles env'))
        (when (map? (:roles resolver'))
          (:roles resolver')))))

(defn- effect-authorization-context
  [resolver node env]
  (let [req-effects (requested-effects node)
        user        (public-auth-user (auth-user env))
        roles-cfg   (roles-config resolver env)
        authz       (when (and (seq req-effects)
                               (map? roles-cfg))
                      (roles/authorize-effects roles-cfg req-effects user))]
    {:requested-effects req-effects
     :auth/user user
     :roles/config roles-cfg
     :auth/effects authz}))

(defn- enforce-effects-authorization!
  [resolver call-node env]
  (let [{:keys [requested-effects] :as ctx}
        (effect-authorization-context resolver call-node env)
        user         (:auth/user ctx)
        auth-effects (:auth/effects ctx)]
    (when (and (seq requested-effects)
               (map? auth-effects)
               (not (:ok? auth-effects)))
      (throw (ex-info "Call node requested effects forbidden for authenticated principal."
                      {:error :auth/forbidden-effect
                       :failure/type :auth/forbidden-effect
                       :retryable? false
                       :node call-node
                       :requested-effects requested-effects
                       :denied-effects (:denied auth-effects)
                       :user (select-keys user [:user/id
                                                :user/email
                                                :user/account-type
                                                :user/roles])})))
    ctx))

(defn- cap-supports-intent?
  [cap intent]
  (let [intents (keyword-set (:cap/intents cap))]
    (or (empty? intents)
        (contains? intents intent))))

(defn- cap-supports-result-type?
  [cap result-type]
  (if (keyword? result-type)
    (let [types (keyword-set (:cap/can-produce cap))]
      (or (empty? types)
          (contains? types result-type)))
    true))

(defn- cap-allows-effects?
  [cap req-effects]
  (if (seq req-effects)
    (let [allowed (keyword-set (:cap/effects-allowed cap))]
      (or (empty? allowed)
          (every? allowed req-effects)))
    true))

(defn- cap-matches-required-schema?
  [cap requires]
  (let [in-schema  (:in-schema requires)
        out-schema (:out-schema requires)]
    (and (or (not (keyword? in-schema))
             (= in-schema (:io/in-schema cap)))
         (or (not (keyword? out-schema))
             (= out-schema (:io/out-schema cap))))))

(defn- cap-matches-required-kind?
  [cap requires]
  (let [required-kind (:cap/kind requires)]
    (or (not (keyword? required-kind))
        (= required-kind (:cap/kind cap)))))

(defn- cap-matches-required-tags?
  [cap requires]
  (let [required-tags (keyword-set (:cap/tags requires))]
    (if (seq required-tags)
      (let [cap-tags (keyword-set (:cap/tags cap))]
        (every? cap-tags required-tags))
      true)))

(defn- candidate-verdict
  [resolver node cap-id]
  (if-not (map? (:caps/by-id resolver))
    {:ok? true
     :cap/id cap-id}
    (let [cap         (get-in resolver [:caps/by-id cap-id])
          requires    (call-requires node)
          intent      (:intent node)
          result-type (requested-result-type node)
          req-effects (requested-effects node)]
      (cond
        (nil? cap)
        {:ok? false
         :cap/id cap-id
         :reason :cap/not-found}

        (not (cap-supports-intent? cap intent))
        {:ok? false
         :cap/id cap-id
         :reason :intent/not-supported
         :intent intent}

        (not (cap-supports-result-type? cap result-type))
        {:ok? false
         :cap/id cap-id
         :reason :result-type/not-supported
         :result/type result-type}

        (not (cap-allows-effects? cap req-effects))
        {:ok? false
         :cap/id cap-id
         :reason :effects/not-allowed
         :effects req-effects}

        (not (cap-matches-required-schema? cap requires))
        {:ok? false
         :cap/id cap-id
         :reason :requires/schema-mismatch
         :requires (select-keys requires [:in-schema :out-schema])
         :cap-schemas (select-keys cap [:io/in-schema :io/out-schema])}

        (not (cap-matches-required-kind? cap requires))
        {:ok? false
         :cap/id cap-id
         :reason :requires/cap-kind-mismatch
         :required-kind (:cap/kind requires)
         :cap-kind (:cap/kind cap)}

        (not (cap-matches-required-tags? cap requires))
        {:ok? false
         :cap/id cap-id
         :reason :requires/cap-tags-mismatch
         :required-tags (keyword-set (:cap/tags requires))
         :cap-tags (keyword-set (:cap/tags cap))}

        :else
        {:ok? true
         :cap/id cap-id}))))

(defn- resolve-candidates
  [resolver node]
  (let [protocol (if (map? (:protocol resolver)) (:protocol resolver) {})
        policy (contracts/intent-policy protocol (:intent node))
        explicit (:cap/id node)
        listed   (vec (or (get-in node [:dispatch :candidates]) []))
        routed   (some-> (get-in resolver [:routing :intent->cap (:intent node)]) vector)
        policy-fallback (vec (or (:fallback policy) []))
        routing-fallback (vec (or (get-in resolver [:routing :fallback]) []))
        base-candidates
        (cond
          (keyword? explicit) [explicit]
          (seq listed) listed
          (seq routed) routed
          :else [])
        candidates
        (->> (concat base-candidates policy-fallback routing-fallback)
             (filter keyword?)
             distinct
             vec)]
    (if (map? (:caps/by-id resolver))
      (let [verdicts (mapv #(candidate-verdict resolver node %) candidates)
            accepted (->> verdicts (filter :ok?) (mapv :cap/id))
            rejected (->> verdicts
                          (remove :ok?)
                          (mapv #(dissoc % :ok?)))]
        (with-meta accepted {:routing/rejected rejected}))
      candidates)))

(defn resolve-capability-id
  "Resolves capability id for a call node.

  Priority:
  1. explicit `:cap/id` in node
  2. first candidate from node `:dispatch`
  3. resolver routing by `:intent`"
  [resolver node]
  (first (resolve-candidates resolver node)))

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

(defn- normalize-tool-result
  [tool-id result]
  {:tool/id tool-id
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
  (let [protocol      (if (map? (:protocol resolver)) (:protocol resolver) {})
        policy-retry  (get-in (contracts/intent-policy protocol (:intent node))
                              [:retry])
        routing-retry (get-in resolver [:routing :retry])
        node-retry    (get-in node [:dispatch :retry])]
    {:same-cap-max (nonneg-int (or (:same-cap-max node-retry)
                                   (:same-cap-max routing-retry)
                                   (:same-cap-max policy-retry)
                                   (:same-cap-max default-retry-policy))
                               0)
     :fallback-max (nonneg-int (or (:fallback-max node-retry)
                                   (:fallback-max routing-retry)
                                   (:fallback-max policy-retry)
                                   (:fallback-max default-retry-policy))
                               0)}))

(defn- resolve-switch-on
  [resolver node]
  (let [protocol (if (map? (:protocol resolver)) (:protocol resolver) {})
        policy  (set (or (get-in (contracts/intent-policy protocol (:intent node))
                                 [:switch-on])
                         #{}))
        routing (set (or (get-in resolver [:routing :switch-on]) #{}))
        local   (set (or (get-in node [:dispatch :switch-on]) #{}))]
    (into (into policy routing) local)))

(defn- default-schema-check
  [protocol call-node result]
  (:ok? (contracts/validate-result protocol
                                   (:intent call-node)
                                   result
                                   (:requires call-node))))

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
  [protocol check-key call-node env result check-fns]
  (let [check-fn (or (get check-fns check-key)
                     (when (= :schema-valid check-key)
                       (fn [n _ r] (default-schema-check protocol n r))))]
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

(defn- done-score-min
  [v]
  (if (number? v) (double v) 0.0))

(defn- merge-done-overrides
  [base override]
  (let [base' (if (map? base) base {})
        over' (if (map? override) override {})
        merged (merge base' over')]
    (cond-> merged
      (or (contains? base' :must) (contains? over' :must))
      (assoc :must (into (keyword-set (:must base'))
                         (keyword-set (:must over'))))
      (or (contains? base' :should) (contains? over' :should))
      (assoc :should (into (keyword-set (:should base'))
                           (keyword-set (:should over')))))))

(defn- effective-done
  [protocol call-node]
  (let [intent         (:intent call-node)
        policy         (contracts/intent-policy protocol intent)
        policy-done    (if (map? (:done policy)) (:done policy) {})
        node-done      (if (map? (:done call-node)) (:done call-node) {})
        merged-done    (merge-done-overrides policy-done node-done)
        policy-checks  (keyword-set (:checks policy))
        dispatch-checks (keyword-set (get-in call-node [:dispatch :checks]))
        must-keys      (into (keyword-set (:must merged-done))
                             (concat policy-checks dispatch-checks))
        should-keys    (keyword-set (:should merged-done))]
    (cond-> merged-done
      true (assoc :must must-keys
                  :should should-keys))))

(defn- evaluate-done
  [protocol call-node env result check-fns judge-fn]
  (let [done         (effective-done protocol call-node)
        must-keys    (set (or (:must done) #{}))
        should-keys  (set (or (:should done) #{}))
        score-min    (done-score-min (:score-min done))
        must-results (mapv #(run-check protocol % call-node env result check-fns) must-keys)
        should-results (mapv #(run-check protocol % call-node env result check-fns) should-keys)
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
  [protocol call-node result done-eval]
  (or (get-in result [:error :type])
      (when-not (:ok? (contracts/validate-result protocol
                                                 (:intent call-node)
                                                 result
                                                 (:requires call-node)))
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

(defn- telemetry-atom
  [telemetry]
  (if (instance? clojure.lang.IAtom telemetry)
    telemetry
    (atom {:nodes/total 0
           :nodes/by-op {}
           :calls/total 0
           :calls/succeeded 0
           :calls/failed 0
           :calls/retries 0
           :calls/fallback-hops 0
           :calls/failure-types {}
           :quality/judge-used 0})))

(defn- telemetry-inc!
  [telemetry k]
  (swap! telemetry update k (fnil inc 0)))

(defn- telemetry-inc-in!
  [telemetry ks]
  (swap! telemetry update-in ks (fnil inc 0)))

(defn execute-plan
  "Executes minimal plan AST with ops:
  - `:let`
  - `:call`
  - `:tool`
  - `:emit`

  Input map:
  - `:plan`       plan map with `:nodes`
  - `:resolver`   routing map
  - `:invoke-call` fn of `[call-node env] -> canonical result envelope`
  - `:invoke-tool` fn of `[tool-node env] -> canonical result envelope`
  - `:env`        optional initial environment map

  Returns:
  - `{:ok? true, :env ..., :emitted ...}`"
  [{:keys [plan resolver invoke-call invoke-tool check-fns judge-fn env telemetry]
    :or   {env {}}}]
  (let [nodes      (vec (:nodes plan))
        telemetry* (telemetry-atom telemetry)
        protocol   (or (:protocol resolver) {})]
    (loop [idx 0
           env env
           emitted nil]
      (if (>= idx (count nodes))
        {:ok? true
         :env env
         :emitted emitted
         :telemetry @telemetry*}
        (let [node (nth nodes idx)]
          (telemetry-inc! telemetry* :nodes/total)
          (when (keyword? (:op node))
            (telemetry-inc-in! telemetry* [:nodes/by-op (:op node)]))
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
              (let [base-node    (-> node
                                     (update :input contracts/materialize-plan env)
                                     normalize-call-node)
                    _            (enforce-effects-authorization! resolver base-node env)
                    retry-policy (resolve-retry-policy resolver base-node)
                    switch-on    (resolve-switch-on resolver base-node)
                    candidates0  (resolve-candidates resolver base-node)
                    rejected     (-> candidates0 meta :routing/rejected)
                    candidates   (vec (take (inc (:fallback-max retry-policy))
                                            candidates0))]
                (telemetry-inc! telemetry* :calls/total)
                (when-not (seq candidates)
                  (throw (ex-info "Unable to resolve capability candidates for call node"
                                  {:node node
                                   :resolver resolver
                                   :rejected-candidates rejected})))
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
                                _ (when (pos? candidate-idx)
                                    (telemetry-inc! telemetry* :calls/fallback-hops))
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
                                          _ (when (> attempt 1)
                                              (telemetry-inc! telemetry* :calls/retries))
                                          rtype (contracts/result-type-of result)
                                          run* (when (= :plan rtype)
                                                 (let [sub-plan (or (contracts/materialize-plan-result result)
                                                                    (contracts/result-plan-of result))]
                                                   (execute-plan {:plan sub-plan
                                                                  :resolver resolver
                                                                  :invoke-call invoke-call
                                                                  :invoke-tool invoke-tool
                                                                  :check-fns check-fns
                                                                  :judge-fn judge-fn
                                                                  :env env
                                                                  :telemetry telemetry*})))
                                          slot-val (if run*
                                                     (assoc (normalize-call-result cap-id result)
                                                            :out (:emitted run*)
                                                            :plan/run run*)
                                                     (normalize-call-result cap-id result))
                                          verify-result (if run*
                                                          {:result {:type :value
                                                                    :out (:emitted run*)}}
                                                          result)
                                          done-eval (evaluate-done protocol candidate-node env verify-result check-fns judge-fn)
                                          _ (when (number? (:judge/score done-eval))
                                              (telemetry-inc! telemetry* :quality/judge-used))
                                          failure-type (call-failure-type protocol candidate-node result done-eval)
                                          recover? (recoverable-failure? failure-type switch-on)
                                          failed? (keyword? failure-type)
                                          accepted? (not failed?)
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
                                        (if (and recover?
                                                 (< attempt same-cap-attempts))
                                          (recur (inc attempt) outcome)
                                          outcome)))))]
                            (if (:ok? candidate-outcome)
                              candidate-outcome
                              (if (and (:failure/recover? candidate-outcome)
                                       (< candidate-idx (dec (count candidates))))
                                (recur (inc candidate-idx) candidate-outcome)
                                candidate-outcome)))))]
                  (if (:ok? call-outcome)
                    (let [_ (telemetry-inc! telemetry* :calls/succeeded)
                          env' (if (keyword? (:as node))
                                 (assoc env (:as node) (:slot-val call-outcome))
                                 env)
                          emitted' (or (:emitted call-outcome) emitted)]
                      (recur (inc idx) env' emitted'))
                    (let [allow-failure? (true? (get-in base-node [:dispatch :allow-failure?]))
                          _ (telemetry-inc! telemetry* :calls/failed)
                          _ (when (keyword? (:failure/type call-outcome))
                              (telemetry-inc-in! telemetry* [:calls/failure-types (:failure/type call-outcome)]))]
                      (if allow-failure?
                        (let [env' (if (keyword? (:as node))
                                     (assoc env (:as node) (:slot-val call-outcome))
                                     env)]
                          (recur (inc idx) env' emitted))
                        (throw (ex-info "Call node failed quality/dispatch policy"
                                        {:node node
                                         :outcome call-outcome
                                         :switch-on switch-on
                                         :retry-policy retry-policy
                                         :candidates candidates
                                         :rejected-candidates rejected})))))))

              :tool
              (let [base-node      (-> node
                                       (update :input contracts/materialize-plan env)
                                       normalize-call-node)
                    tool-id        (:tool/id base-node)
                    req-effects    (requested-effects base-node)
                    allow-failure? (true? (get-in base-node [:dispatch :allow-failure?]))]
                (when-not (keyword? tool-id)
                  (throw (ex-info "Tool node requires :tool/id keyword."
                                  {:node node
                                   :error :effects/invalid-input
                                   :failure/type :effects/invalid-input})))
                (when-not (seq req-effects)
                  (throw (ex-info "Tool node must declare requested effects in :effects/:allowed."
                                  {:node node
                                   :tool/id tool-id
                                   :error :effects/not-declared
                                   :failure/type :effects/not-declared
                                   :retryable? false})))
                (when-not (fn? invoke-tool)
                  (throw (ex-info "Workflow runtime is missing :invoke-tool handler."
                                  {:node node
                                   :tool/id tool-id
                                   :error :effects/runtime-missing
                                   :failure/type :effects/runtime-missing
                                   :retryable? false})))
                (let [authz-ctx (enforce-effects-authorization! resolver base-node env)
                      tool-node (cond-> base-node
                                  (map? (:auth/user authz-ctx))
                                  (assoc :auth/user (:auth/user authz-ctx))
                                  (map? (:roles/config authz-ctx))
                                  (assoc :roles/config (:roles/config authz-ctx))
                                  (map? (:auth/effects authz-ctx))
                                  (assoc :auth/effects (:auth/effects authz-ctx)))]
                (telemetry-inc! telemetry* :calls/total)
                (let [outcome
                      (try
                        (let [raw-result (invoke-tool tool-node env)
                              result (if (and (map? raw-result)
                                              (or (contains? raw-result :result)
                                                  (contains? raw-result :error)))
                                       raw-result
                                       {:result {:type :value
                                                 :out (cond
                                                        (map? raw-result) raw-result
                                                        (string? raw-result) {:text raw-result}
                                                        (nil? raw-result) {}
                                                        :else {:value raw-result})}})
                              done-eval (evaluate-done protocol base-node env result check-fns judge-fn)
                              _ (when (number? (:judge/score done-eval))
                                  (telemetry-inc! telemetry* :quality/judge-used))
                              failure-type (call-failure-type protocol base-node result done-eval)
                              failed? (keyword? failure-type)
                              slot-val (normalize-tool-result tool-id result)]
                          {:ok? (not failed?)
                           :tool/id tool-id
                           :result result
                           :slot-val slot-val
                           :done/eval done-eval
                           :failure/type failure-type
                           :failure/recover? false})
                        (catch clojure.lang.ExceptionInfo e
                          (let [data (or (ex-data e) {})
                                failure-type (or (:failure/type data)
                                                 (:error data)
                                                 :effects/runtime-failed)
                                slot-val {:tool/id tool-id
                                          :error (:error data)
                                          :details data}]
                            {:ok? false
                             :tool/id tool-id
                             :slot-val slot-val
                             :failure/type failure-type
                             :failure/recover? false
                             :details data}))
                        (catch Throwable t
                          {:ok? false
                           :tool/id tool-id
                           :slot-val {:tool/id tool-id
                                      :error :effects/runtime-failed
                                      :message (.getMessage t)}
                           :failure/type :effects/runtime-failed
                           :failure/recover? false
                           :details {:message (.getMessage t)}}))]
                  (if (:ok? outcome)
                    (do
                      (telemetry-inc! telemetry* :calls/succeeded)
                      (let [env' (if (keyword? (:as node))
                                   (assoc env (:as node) (:slot-val outcome))
                                   env)]
                        (recur (inc idx) env' emitted)))
                    (do
                      (telemetry-inc! telemetry* :calls/failed)
                      (when (keyword? (:failure/type outcome))
                        (telemetry-inc-in! telemetry* [:calls/failure-types (:failure/type outcome)]))
                      (if allow-failure?
                        (let [env' (if (keyword? (:as node))
                                     (assoc env (:as node) (:slot-val outcome))
                                     env)]
                          (recur (inc idx) env' emitted))
                        (throw (ex-info "Tool node execution failed"
                                        (merge {:node node
                                                :outcome outcome}
                                                (when (map? (:details outcome))
                                                  (:details outcome)))))))))))

              :emit
              (let [output (materialize-emit-input (:input node) env)]
                (recur (inc idx) env output))

              (throw (ex-info "Unsupported plan node operation"
                              {:op (:op node)
                               :node node})))))))))
