(ns

    ^{:doc    "Minimal plan evaluator for stratified capability execution."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.workflow

  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [ferment.contracts :as contracts]
            [ferment.roles :as roles]
            [ferment.telemetry :as telemetry]))

(def ^:private default-retry-policy
  {:same-cap-max 0
   :fallback-max 0})

(declare merge-done-overrides)

(defn- now-nanos
  []
  (System/nanoTime))

(defn- nanos->millis
  [start-nanos]
  (/ (double (- (System/nanoTime) start-nanos)) 1000000.0))

(defn- keyword-set
  [v]
  (cond
    (set? v) (into #{} (filter keyword?) v)
    (sequential? v) (into #{} (filter keyword?) v)
    (keyword? v) #{v}
    :else #{}))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

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

(defn- routing-requires
  [node]
  (let [requires (call-requires node)]
    (if (keyword? (keywordish (:input/schema node)))
      (dissoc requires :in-schema)
      requires)))

(defn- normalize-call-node
  [node]
  (let [requires (call-requires node)]
    (if (seq requires)
      (assoc node :requires requires)
      node)))

(defn- policy-profiles-map
  [resolver]
  (let [profiles (or (when (map? (:policy/profiles resolver))
                       (:policy/profiles resolver))
                     (when (map? (get-in resolver [:routing :policy-profiles]))
                       (get-in resolver [:routing :policy-profiles])))]
    (if (map? profiles) profiles {})))

(defn- policy-profile-key
  [resolver]
  (or (keywordish (:policy/profile resolver))
      (keywordish (get-in resolver [:routing :policy/profile]))
      :balanced))

(defn- merge-policy-overrides
  [base override]
  (let [base' (if (map? base) base {})
        over' (if (map? override) override {})
        done' (merge-done-overrides (:done base') (:done over'))
        checks' (into (keyword-set (:checks base'))
                      (keyword-set (:checks over')))
        switch-on' (into (keyword-set (:switch-on base'))
                         (keyword-set (:switch-on over')))
        fallback' (vec (distinct (concat (or (:fallback base') [])
                                         (or (:fallback over') []))))
        retry' (merge (if (map? (:retry base')) (:retry base') {})
                      (if (map? (:retry over')) (:retry over') {}))
        judge' (merge (if (map? (:judge base')) (:judge base') {})
                      (if (map? (:judge over')) (:judge over') {}))]
    (cond-> {}
      (seq done') (assoc :done done')
      (seq checks') (assoc :checks checks')
      (seq switch-on') (assoc :switch-on switch-on')
      (seq fallback') (assoc :fallback fallback')
      (seq retry') (assoc :retry retry')
      (seq judge') (assoc :judge judge'))))

(defn- profile-intent-policy
  [resolver intent]
  (let [profiles (policy-profiles-map resolver)
        profile-k (policy-profile-key resolver)
        profile-cfg (if (map? (get profiles profile-k))
                      (get profiles profile-k)
                      {})
        default' (if (map? (:default profile-cfg))
                   (:default profile-cfg)
                   {})
        intent' (if (map? (get-in profile-cfg [:intents intent]))
                  (get-in profile-cfg [:intents intent])
                  {})]
    (merge-policy-overrides default' intent')))

(defn- effective-intent-policy
  [resolver protocol intent]
  (merge-policy-overrides
   (contracts/intent-policy protocol intent)
   (profile-intent-policy resolver intent)))

(defn- validate-call-input!
  [ctx call-node]
  (let [schema-k (some-> (:input/schema call-node) keywordish)]
    (when (keyword? schema-k)
      (let [check (contracts/validate-schema (:protocol ctx)
                                             schema-k
                                             (:input call-node))]
        (when-not (:ok? check)
          (throw (ex-info "Call node input failed schema contract."
                          {:error :input/schema-invalid
                           :failure/type :input/schema-invalid
                           :schema schema-k
                           :details (select-keys check [:reason :schema :explain])
                           :node call-node})))))))

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
          requires    (routing-requires node)
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

(def ^:private gateway-strategies
  #{:latency-first :quality-first :cost-first})

(defn- gateway-config
  [resolver]
  (let [cfg (or (when (map? (:gateway resolver))
                  (:gateway resolver))
                (when (map? (get-in resolver [:routing :gateway]))
                  (get-in resolver [:routing :gateway])))]
    (if (map? cfg) cfg {})))

(defn- gateway-strategy
  [resolver intent]
  (let [cfg (gateway-config resolver)
        by-intent (if (map? (:intent->strategy cfg))
                    (:intent->strategy cfg)
                    {})
        strategy (or (keywordish (get by-intent intent))
                     (keywordish (:strategy cfg)))]
    (when (contains? gateway-strategies strategy)
      strategy)))

(defn- gateway-health-registry
  [resolver]
  (let [registry (:gateway/model-health resolver)]
    (when (instance? clojure.lang.IAtom registry)
      registry)))

(defn- gateway-cap-model-key
  [resolver cap-id]
  (or (some-> (get-in resolver [:caps/by-id cap-id]) :dispatch/model-key keywordish)
      (some-> (get-in resolver [:routing :cap->model-key cap-id]) keywordish)
      cap-id))

(defn- gateway-cap-latency-ms
  [resolver cap-id]
  (let [v (or (get-in resolver [:caps/by-id cap-id :cap/cost :latency-ms])
              (get-in resolver [:caps/by-id cap-id :cap/limits :timeout-ms]))]
    (if (number? v)
      (double v)
      10000.0)))

(defn- gateway-cap-cost
  [resolver cap-id]
  (let [cost (or (get-in resolver [:caps/by-id cap-id :cap/cost :usd])
                 (get-in resolver [:caps/by-id cap-id :cap/cost :tokens])
                 (get-in resolver [:caps/by-id cap-id :cap/cost :latency-ms]))]
    (if (number? cost)
      (double cost)
      10000.0)))

(defn- gateway-breaker-config
  [resolver]
  (let [cfg (gateway-config resolver)
        raw (if (map? (:circuit-breaker cfg))
              (:circuit-breaker cfg)
              {})
        enabled? (boolean (:enabled? raw))
        min-samples (let [v (:min-samples raw)]
                      (if (and (integer? v) (pos? (long v)))
                        (int v)
                        5))
        open-rate (let [v (:error-rate-open raw)]
                    (if (number? v)
                      (min 1.0 (max 0.0 (double v)))
                      0.6))
        cooldown-ms (let [v (:cooldown-ms raw)]
                      (if (and (integer? v) (pos? (long v)))
                        (int v)
                        30000))]
    {:enabled? enabled?
     :min-samples min-samples
     :error-rate-open open-rate
     :cooldown-ms cooldown-ms}))

(defn- gateway-breaker-open?
  [stats now-ms]
  (let [open-until (let [v (:breaker/open-until-ms stats)]
                     (if (integer? v) (long v) 0))]
    (> open-until (long now-ms))))

(defn- gateway-candidate-score
  [strategy idx latency-ms error-rate quality-score cost]
  (case strategy
    :quality-first [(- 1.0 quality-score) error-rate latency-ms idx]
    :cost-first [cost latency-ms error-rate idx]
    :latency-first [latency-ms error-rate idx]
    [idx]))

(defn- gateway-order-candidates
  [resolver node candidates]
  (let [strategy (gateway-strategy resolver (:intent node))
        health* (gateway-health-registry resolver)
        now-ms (System/currentTimeMillis)
        breaker-cfg (gateway-breaker-config resolver)
        breaker-enabled? (true? (:enabled? breaker-cfg))]
    (if-not (and (keyword? strategy) (seq candidates))
      [candidates []]
      (let [ranked (map-indexed
                    (fn [idx cap-id]
                      (let [model-k (gateway-cap-model-key resolver cap-id)
                            stats   (if (instance? clojure.lang.IAtom health*)
                                      (get @health* model-k)
                                      nil)
                            calls   (if (integer? (:calls stats)) (long (:calls stats)) 0)
                            errors  (if (integer? (:errors stats)) (long (:errors stats)) 0)
                            error-rate (if (pos? calls)
                                         (/ (double errors) (double calls))
                                         0.0)
                            latency-ms (if (number? (:latency/ema-ms stats))
                                         (double (:latency/ema-ms stats))
                                         (gateway-cap-latency-ms resolver cap-id))
                            quality-score (if (number? (:quality/ema stats))
                                            (double (:quality/ema stats))
                                            0.5)
                            cost (gateway-cap-cost resolver cap-id)
                            breaker-open? (and breaker-enabled?
                                               (>= calls (long (:min-samples breaker-cfg)))
                                               (gateway-breaker-open? stats now-ms))]
                        {:idx idx
                         :cap/id cap-id
                         :model-key model-k
                         :calls calls
                         :error-rate error-rate
                         :latency-ms latency-ms
                         :quality-score quality-score
                         :cost cost
                         :breaker/open? breaker-open?}))
                    candidates)
            active (vec (remove :breaker/open? ranked))
            use-active? (and breaker-enabled? (seq active))
            selected (if use-active? active (vec ranked))
            ordered (->> selected
                         (sort-by (fn [{:keys [idx latency-ms error-rate quality-score cost]}]
                                    (gateway-candidate-score strategy
                                                             idx
                                                             latency-ms
                                                             error-rate
                                                             quality-score
                                                             cost)))
                         (mapv :cap/id))
            rejected (if use-active?
                       (->> ranked
                            (filter :breaker/open?)
                            (mapv (fn [entry]
                                    (let [cap-id (:cap/id entry)]
                                      {:cap/id cap-id
                                       :reason :gateway/circuit-open
                                       :model-key (:model-key entry)}))))
                       [])]
        [ordered rejected]))))

(defn- resolve-candidates
  [resolver node]
  (let [protocol (if (map? (:protocol resolver)) (:protocol resolver) {})
        policy (effective-intent-policy resolver protocol (:intent node))
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
            accepted0 (->> verdicts (filter :ok?) (mapv :cap/id))
            rejected0 (->> verdicts
                           (remove :ok?)
                           (mapv #(dissoc % :ok?)))
            [accepted1 gateway-rejected] (gateway-order-candidates resolver node accepted0)
            rejected (into (vec rejected0) (vec gateway-rejected))]
        (with-meta accepted1 {:routing/rejected rejected}))
      candidates)))

(defn resolve-capability-decision
  "Resolves capability candidates for a call node and returns diagnostics map.

  Returned map keys:
  - `:cap/id`                selected capability id (or nil)
  - `:candidates`            accepted candidates in deterministic order
  - `:rejected-candidates`   rejected candidates with reasons"
  [resolver node]
  (let [candidates0 (resolve-candidates resolver node)
        rejected    (-> candidates0 meta :routing/rejected)]
    {:cap/id (first candidates0)
     :candidates (vec candidates0)
     :rejected-candidates (if (sequential? rejected) (vec rejected) [])}))

(defn resolve-capability
  "Resolves capability id for a call node.

  Priority:
  1. explicit `:cap/id` in node
  2. first candidate from node `:dispatch`
  3. resolver routing by `:intent`"
  [resolver node]
  (:cap/id (resolve-capability-decision resolver node)))

(defn call-failed?
  "Best-effort failure check for values stored in plan environment."
  [v]
  (or (and (map? v) (some? (:error v)))
      (and (map? v) (keyword? (:failure/type v)))
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
  (let [invocation (when (map? result) (:invoke/meta result))]
    (cond-> {:cap/id cap-id
             :result result
             :out (contracts/result-out-of result)
             :error (:error result)}
      (map? invocation) (assoc :invoke/meta invocation))))

(defn- call-transcript-entry
  [call-node result attempt candidate-idx failure-type run* latency-ms]
  (let [invocation (when (map? result) (:invoke/meta result))
        run-ok?    (when (map? run*) (boolean (:ok? run*)))
        run-telemetry (when (map? run*) (:telemetry run*))
        out        (if (map? run*)
                     (:emitted run*)
                     (contracts/result-out-of result))
        result-type (contracts/result-type-of result)]
    (cond-> {:op :call
             :intent (:intent call-node)
             :cap/id (:cap/id call-node)
             :as (:as call-node)
             :attempt attempt
             :candidate-index candidate-idx
             :input (:input call-node)
             :result/type result-type
             :out out}
      (number? latency-ms) (assoc :latency-ms latency-ms)
      (map? invocation) (assoc :invoke/meta invocation)
      (and (map? result) (some? (:error result))) (assoc :error (:error result))
      (keyword? failure-type) (assoc :failure/type failure-type)
      (map? run*) (assoc :plan/run (cond-> {:ok? run-ok?}
                                      (map? run-telemetry)
                                      (assoc :telemetry run-telemetry))))))

(defn- call-timing-entry
  [call-node result attempt candidate-idx failure-type latency-ms]
  (let [invocation (when (map? result) (:invoke/meta result))]
    (cond-> {:op :call
             :intent (:intent call-node)
             :cap/id (:cap/id call-node)
             :as (:as call-node)
             :attempt attempt
             :candidate-index candidate-idx
             :latency-ms latency-ms}
      (map? invocation) (assoc :invoke/meta invocation)
      (keyword? failure-type) (assoc :failure/type failure-type))))

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

(defn- nonneg-double
  [v default]
  (if (number? v)
    (max 0.0 (double v))
    (double default)))

(defn- positive-int-or-nil
  [v]
  (let [n (cond
            (int? v) v
            (integer? v) (int v)
            :else nil)]
    (when (and (int? n) (pos? n))
      n)))

(defn- gateway-ema-alpha
  [resolver]
  (let [alpha (get-in (gateway-config resolver) [:ema-alpha])]
    (if (number? alpha)
      (min 1.0 (max 0.0 (double alpha)))
      0.2)))

(defn- gateway-result-model-key
  [resolver call-node result]
  (or (some-> result :invoke/meta :model-key keywordish)
      (gateway-cap-model-key resolver (:cap/id call-node))
      (:cap/id call-node)))

(defn- gateway-quality-score
  [failed? done-eval]
  (if (and (map? done-eval)
           (number? (:score done-eval)))
    (double (:score done-eval))
    (if failed? 0.0 1.0)))

(defn- update-model-health
  [entry failed? latency-ms quality-score now-ms resolver]
  (let [entry0      (if (map? entry) entry {})
        alpha       (gateway-ema-alpha resolver)
        calls       (inc (long (or (:calls entry0) 0)))
        errors      (+ (long (or (:errors entry0) 0))
                       (if failed? 1 0))
        latency-ema (let [sample (nonneg-double latency-ms 0.0)
                          prev   (if (number? (:latency/ema-ms entry0))
                                   (double (:latency/ema-ms entry0))
                                   sample)]
                      (+ (* alpha sample)
                         (* (- 1.0 alpha) prev)))
        quality-ema (let [sample (nonneg-double quality-score 0.0)
                          prev   (if (number? (:quality/ema entry0))
                                   (double (:quality/ema entry0))
                                   sample)]
                      (+ (* alpha sample)
                         (* (- 1.0 alpha) prev)))
        err-rate    (if (pos? calls)
                      (/ (double errors) (double calls))
                      0.0)
        breaker-cfg (gateway-breaker-config resolver)
        breaker-enabled? (true? (:enabled? breaker-cfg))
        min-samples (long (:min-samples breaker-cfg))
        open-rate   (double (:error-rate-open breaker-cfg))
        cooldown-ms (long (:cooldown-ms breaker-cfg))
        should-open? (and breaker-enabled?
                          (>= calls min-samples)
                          (>= err-rate open-rate))
        open-until' (if should-open?
                      (+ (long now-ms) cooldown-ms)
                      (let [current (long (or (:breaker/open-until-ms entry0) 0))]
                        (if (> current (long now-ms))
                          current
                          0)))]
    (cond-> (assoc entry0
                   :calls calls
                   :errors errors
                   :error-rate err-rate
                   :latency/ema-ms latency-ema
                   :quality/ema quality-ema
                   :updated-at-ms (long now-ms))
      (> open-until' 0) (assoc :breaker/open-until-ms open-until')
      (and should-open?
           (<= (long (or (:breaker/open-until-ms entry0) 0)) (long now-ms)))
      (update :breaker/open-count (fnil inc 0)))))

(defn- record-model-health!
  [resolver call-node result failure-type latency-ms done-eval]
  (let [health* (gateway-health-registry resolver)
        model-k (gateway-result-model-key resolver call-node result)]
    (when (and (instance? clojure.lang.IAtom health*)
               (keyword? model-k))
      (let [failed? (keyword? failure-type)
            quality-score (gateway-quality-score failed? done-eval)
            now-ms (System/currentTimeMillis)]
        (swap! health* update model-k
               (fn [entry]
                 (update-model-health entry
                                      failed?
                                      latency-ms
                                      quality-score
                                      now-ms
                                      resolver)))))))

(defn- ensure-call-attempt-budget!
  [ctx]
  (let [attempts* (:call-attempts* ctx)
        max-attempts (positive-int-or-nil (:max-call-attempts ctx))]
    (when (and (instance? clojure.lang.Atom attempts*)
               (int? max-attempts))
      (let [n (swap! attempts* inc)]
        (when (> n max-attempts)
          (throw (ex-info "Call tree exceeded configured attempt limit."
                          {:error :policy/call-tree-limit
                           :failure/type :policy/call-tree-limit
                           :attempts n
                           :max-attempts max-attempts})))))))

(defn- ensure-fallback-hop-budget!
  [ctx]
  (let [max-hops (positive-int-or-nil (:max-fallback-hops ctx))
        telemetry* (:telemetry* ctx)
        current-hops (if (instance? clojure.lang.Atom telemetry*)
                       (long (or (:calls/fallback-hops @telemetry*) 0))
                       0)]
    (when (and (int? max-hops)
               (>= current-hops max-hops))
      (throw (ex-info "Call tree exceeded configured fallback-hop limit."
                      {:error :policy/fallback-limit
                       :failure/type :policy/fallback-limit
                       :fallback-hops current-hops
                       :max-fallback-hops max-hops})))))

(defn- resolve-retry-policy
  [resolver node]
  (let [protocol      (if (map? (:protocol resolver)) (:protocol resolver) {})
        policy-retry  (get-in (effective-intent-policy resolver protocol (:intent node))
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
        policy  (set (or (get-in (effective-intent-policy resolver protocol (:intent node))
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
  [resolver protocol call-node]
  (let [intent         (:intent call-node)
        policy         (effective-intent-policy resolver protocol intent)
        dispatch-map   (if (map? (:dispatch call-node)) (:dispatch call-node) {})
        policy-done    (if (map? (:done policy)) (:done policy) {})
        node-done      (if (map? (:done call-node)) (:done call-node) {})
        merged-done    (merge-done-overrides policy-done node-done)
        policy-hard-checks (into (keyword-set (:checks policy))
                                 (keyword-set (:checks/hard policy)))
        policy-soft-checks (keyword-set (:checks/soft policy))
        legacy-hard-checks (keyword-set (:checks dispatch-map))
        explicit-hard? (contains? dispatch-map :checks/hard)
        dispatch-hard-checks (keyword-set (:checks/hard dispatch-map))
        dispatch-soft-checks (keyword-set (:checks/soft dispatch-map))
        hard-checks (if explicit-hard?
                      dispatch-hard-checks
                      (into policy-hard-checks legacy-hard-checks))
        must-keys      (into (keyword-set (:must merged-done))
                             hard-checks)
        should-keys    (into (keyword-set (:should merged-done))
                             (into policy-soft-checks dispatch-soft-checks))]
    (cond-> merged-done
      true (assoc :must must-keys
                  :should should-keys))))

(defn- evaluate-done
  [resolver protocol call-node env result check-fns judge-fn]
  (let [done         (effective-done resolver protocol call-node)
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
        judge-pass?  (when (number? judge-score)
                       (>= judge-score score-min))
        score        (double (if (number? judge-score)
                               (/ (+ should-score judge-score) 2.0)
                               should-score))
        ok?          (and (empty? must-failed)
                          (>= score score-min))
        failure-type (cond
                       (some #{:schema-valid} must-failed) :schema/invalid
                       (seq must-failed) :eval/must-failed
                       (not ok?) :eval/low-score
                       :else nil)]
    {:ok? ok?
     :failure/type failure-type
     :score score
     :must-failed must-failed
     :should-failed should-failed
     :judge/score judge-score
     :judge/pass? judge-pass?
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
  (telemetry/ensure-atom telemetry telemetry/default-workflow-counters))

(defn- telemetry-inc!
  [telemetry k]
  (telemetry/inc! telemetry k))

(defn- telemetry-inc-in!
  [telemetry ks]
  (telemetry/inc-in! telemetry ks))

(defn- telemetry-record-quality!
  [telemetry done-eval]
  (when (seq (:must-failed done-eval))
    (telemetry-inc! telemetry :quality/must-failed))
  (when (number? (:judge/score done-eval))
    (telemetry-inc! telemetry :quality/judge-used)
    (if (true? (:judge/pass? done-eval))
      (telemetry-inc! telemetry :quality/judge-pass)
      (telemetry-inc! telemetry :quality/judge-fail))))

(declare execute-plan)

(defn- execute-sub-plan
  [ctx env result]
  (let [rtype (contracts/result-type-of result)]
    (when (= :plan rtype)
      (let [sub-plan (or (contracts/materialize-plan-result result)
                         (contracts/result-plan-of result))]
        (execute-plan {:plan sub-plan
                       :resolver (:resolver ctx)
                       :invoke-call (:invoke-call ctx)
                       :invoke-tool (:invoke-tool ctx)
                       :check-fns (:check-fns ctx)
                       :judge-fn (:judge-fn ctx)
                       :env env
                       :telemetry (:telemetry* ctx)
                       :timings (:timings* ctx)
                       :debug/transcript? (:debug-transcript? ctx)
                       :transcript (:transcript* ctx)})))))

(defn- record-call-artifacts!
  [ctx candidate-node result attempt candidate-idx failure-type run* latency-ms]
  (when (instance? clojure.lang.Atom (:timings* ctx))
    (swap! (:timings* ctx) conj
           (call-timing-entry candidate-node
                              result
                              attempt
                              candidate-idx
                              failure-type
                              latency-ms)))
  (when (instance? clojure.lang.Atom (:transcript* ctx))
    (swap! (:transcript* ctx) conj
           (call-transcript-entry candidate-node
                                  result
                                  attempt
                                  candidate-idx
                                  failure-type
                                  run*
                                  latency-ms))))

(defn- evaluate-call-attempt
  [ctx env switch-on candidate-node cap-id attempt candidate-idx]
  (let [_ (ensure-call-attempt-budget! ctx)
        call-start (now-nanos)
        result (let [invoke-call (:invoke-call ctx)]
                 (invoke-call candidate-node env))
        latency-ms (nanos->millis call-start)
        _ (when (> attempt 1)
            (telemetry-inc! (:telemetry* ctx) :calls/retries))
        run* (execute-sub-plan ctx env result)
        slot-val (if run*
                   (assoc (normalize-call-result cap-id result)
                          :out (:emitted run*)
                          :plan/run run*)
                   (normalize-call-result cap-id result))
        verify-result (if run*
                        {:result {:type :value
                                  :out (:emitted run*)}}
                        result)
        done-eval (evaluate-done (:resolver ctx)
                                 (:protocol ctx)
                                 candidate-node
                                 env
                                 verify-result
                                 (:check-fns ctx)
                                 (:judge-fn ctx))
        _ (telemetry-record-quality! (:telemetry* ctx) done-eval)
        failure-type (call-failure-type (:protocol ctx) candidate-node result done-eval)
        recover? (recoverable-failure? failure-type switch-on)
        failed? (keyword? failure-type)
        slot-val' (cond-> slot-val
                    failed? (assoc :failure/type failure-type
                                   :failure/recover? recover?))
        accepted? (not failed?)
        outcome {:ok? accepted?
                 :cap/id cap-id
                 :result result
                 :slot-val slot-val'
                 :emitted (when run* (:emitted run*))
                 :plan/run run*
                 :attempt attempt
                 :latency-ms latency-ms
                 :done/eval done-eval
                 :failure/type failure-type
                 :failure/recover? recover?}]
    (record-model-health! (:resolver ctx)
                          candidate-node
                          result
                          failure-type
                          latency-ms
                          done-eval)
    (record-call-artifacts! ctx
                            candidate-node
                            result
                            attempt
                            candidate-idx
                            failure-type
                            run*
                            latency-ms)
    outcome))

(defn- attempt-candidate
  [ctx env switch-on candidate-node cap-id candidate-idx same-cap-attempts]
  (loop [attempt 1
         last-attempt nil]
    (if (> attempt same-cap-attempts)
      (or last-attempt
          {:ok? false
           :failure/type :schema/invalid
           :failure/recover? false
           :cap/id cap-id})
      (let [outcome (evaluate-call-attempt ctx
                                           env
                                           switch-on
                                           candidate-node
                                           cap-id
                                           attempt
                                           candidate-idx)]
        (if (and (:failure/recover? outcome)
                 (< attempt same-cap-attempts))
          (recur (inc attempt) outcome)
          outcome)))))

(defn- resolve-call-outcome
  [ctx env base-node switch-on candidates same-cap-attempts]
  (loop [candidate-idx 0
         last-outcome nil]
    (if (>= candidate-idx (count candidates))
      (or last-outcome
          {:ok? false
           :failure/type :unsupported/intent
           :failure/recover? false})
      (let [cap-id (nth candidates candidate-idx)
            _ (when (pos? candidate-idx)
                (ensure-fallback-hop-budget! ctx)
                (telemetry-inc! (:telemetry* ctx) :calls/fallback-hops))
            candidate-node (assoc base-node :cap/id cap-id)
            candidate-outcome (attempt-candidate ctx
                                                 env
                                                 switch-on
                                                 candidate-node
                                                 cap-id
                                                 candidate-idx
                                                 same-cap-attempts)]
        (if (and (:failure/recover? candidate-outcome)
                 (< candidate-idx (dec (count candidates))))
          (recur (inc candidate-idx) candidate-outcome)
          candidate-outcome)))))

(defn- run-let-node
  [node env emitted]
  (let [value (contracts/materialize-plan (:value node) env)
        env'  (if (keyword? (:as node))
                (assoc env (:as node) value)
                env)]
    {:env env'
     :emitted emitted}))

(defn- run-call-node
  [ctx node env emitted]
  (let [base-node    (-> node
                         (update :input contracts/materialize-plan env)
                         normalize-call-node)
        resolver     (:resolver ctx)
        _            (validate-call-input! ctx base-node)
        _            (enforce-effects-authorization! resolver base-node env)
        retry-policy (resolve-retry-policy resolver base-node)
        switch-on    (resolve-switch-on resolver base-node)
        candidates0  (resolve-candidates resolver base-node)
        rejected     (-> candidates0 meta :routing/rejected)
        candidates   (vec (take (inc (:fallback-max retry-policy))
                                candidates0))
        telemetry*   (:telemetry* ctx)]
    (telemetry-inc! telemetry* :calls/total)
    (when-not (seq candidates)
      (throw (ex-info "Unable to resolve capability candidates for call node"
                      {:node node
                       :resolver resolver
                       :rejected-candidates rejected})))
    (let [same-cap-attempts (inc (:same-cap-max retry-policy))
          call-outcome (try
                         (resolve-call-outcome ctx
                                               env
                                               base-node
                                               switch-on
                                               candidates
                                               same-cap-attempts)
                         (catch clojure.lang.ExceptionInfo e
                           (let [data (or (ex-data e) {})
                                 failure-type (or (:failure/type data)
                                                  (:error data)
                                                  :policy/call-tree-limit)]
                             {:ok? false
                              :cap/id (first candidates)
                              :failure/type failure-type
                              :failure/recover? false
                              :details data}))
                         (catch Throwable t
                           {:ok? false
                            :cap/id (first candidates)
                            :failure/type :policy/call-tree-limit
                            :failure/recover? false
                            :details {:message (.getMessage t)}}))]
      (if (:ok? call-outcome)
        (do
          (telemetry-inc! telemetry* :calls/succeeded)
          {:env (if (keyword? (:as node))
                  (assoc env (:as node) (:slot-val call-outcome))
                  env)
           :emitted (or (:emitted call-outcome) emitted)})
        (let [allow-failure? (true? (get-in base-node [:dispatch :allow-failure?]))
              _ (telemetry-inc! telemetry* :calls/failed)
              _ (when (keyword? (:failure/type call-outcome))
                  (telemetry-inc-in! telemetry* [:calls/failure-types (:failure/type call-outcome)]))]
          (if allow-failure?
            {:env (if (keyword? (:as node))
                    (assoc env (:as node) (:slot-val call-outcome))
                    env)
             :emitted emitted}
              (throw (ex-info "Call node failed quality/dispatch policy"
                              {:node node
                               :outcome call-outcome
                               :switch-on switch-on
                               :retry-policy retry-policy
                               :candidates candidates
                               :rejected-candidates rejected
                               :details (:details call-outcome)}))))))))

(defn- run-tool-node
  [ctx node env emitted]
  (let [base-node      (-> node
                           (update :input contracts/materialize-plan env)
                           normalize-call-node)
        tool-id        (:tool/id base-node)
        req-effects    (requested-effects base-node)
        allow-failure? (true? (get-in base-node [:dispatch :allow-failure?]))
        resolver       (:resolver ctx)
        invoke-tool    (:invoke-tool ctx)
        telemetry*     (:telemetry* ctx)]
    (when-not (keyword? tool-id)
      (throw (ex-info "Tool node requires :tool/id keyword."
                      {:node node
                       :error :effects/invalid-input
                       :failure/type :effects/invalid-input})))
    (when-not (seq req-effects)
      (throw (ex-info "Tool node must declare requested effects in :effects/:allowed."
                      {:node node
                       :tool/id tool-id
                       :error :effects/invalid-input
                       :failure/type :effects/invalid-input
                       :reason :effects/not-declared
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
                    done-eval (evaluate-done (:resolver ctx)
                                             (:protocol ctx)
                                             base-node
                                             env
                                             result
                                             (:check-fns ctx)
                                             (:judge-fn ctx))
                    _ (telemetry-record-quality! telemetry* done-eval)
                    failure-type (call-failure-type (:protocol ctx) base-node result done-eval)
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
            {:env (if (keyword? (:as node))
                    (assoc env (:as node) (:slot-val outcome))
                    env)
             :emitted emitted})
          (do
            (telemetry-inc! telemetry* :calls/failed)
            (when (keyword? (:failure/type outcome))
              (telemetry-inc-in! telemetry* [:calls/failure-types (:failure/type outcome)]))
            (if allow-failure?
              {:env (if (keyword? (:as node))
                      (assoc env (:as node) (:slot-val outcome))
                      env)
               :emitted emitted}
              (throw (ex-info "Tool node execution failed"
                              (merge {:node node
                                      :outcome outcome}
                                     (when (map? (:details outcome))
                                       (:details outcome))))))))))))

(defn- run-emit-node
  [node env]
  {:env env
   :emitted (materialize-emit-input (:input node) env)})

(defn- run-node
  [ctx node env emitted]
  (case (:op node)
    :let (run-let-node node env emitted)
    :call (run-call-node ctx node env emitted)
    :tool (run-tool-node ctx node env emitted)
    :emit (run-emit-node node env)
    (throw (ex-info "Unsupported plan node operation"
                    {:op (:op node)
                     :node node}))))

(defn- finalize-run
  [telemetry* timings* transcript* env emitted]
  (cond-> {:ok? true
           :env env
           :emitted emitted
           :telemetry @telemetry*}
    (seq @timings*)
    (assoc :timings @timings*)
    (instance? clojure.lang.Atom transcript*)
    (assoc :transcript @transcript*)))

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
  - `:max-call-attempts` optional hard limit for total call attempts in one run
  - `:max-fallback-hops` optional hard limit for fallback hops in one run
  - `:debug/transcript?` include per-call transcript in run output
  - `:env`        optional initial environment map

  Returns:
  - `{:ok? true, :env ..., :emitted ...}`"
  [{:keys [plan resolver invoke-call invoke-tool check-fns judge-fn env telemetry transcript]
    :or   {env {}}
    :as opts}]
  (let [nodes      (vec (:nodes plan))
        telemetry* (telemetry-atom telemetry)
        protocol   (or (:protocol resolver) {})
        debug-transcript? (true? (:debug/transcript? opts))
        max-call-attempts (positive-int-or-nil (:max-call-attempts opts))
        max-fallback-hops (positive-int-or-nil (:max-fallback-hops opts))
        call-attempts* (if (instance? clojure.lang.Atom (:call-attempts opts))
                         (:call-attempts opts)
                         (atom 0))
        timings*   (if (instance? clojure.lang.Atom (:timings opts))
                     (:timings opts)
                     (atom []))
        transcript* (when debug-transcript?
                      (if (instance? clojure.lang.Atom transcript)
                        transcript
                        (atom [])))
        ctx {:resolver resolver
             :invoke-call invoke-call
             :invoke-tool invoke-tool
             :check-fns check-fns
             :judge-fn judge-fn
             :protocol protocol
             :telemetry* telemetry*
             :call-attempts* call-attempts*
             :max-call-attempts max-call-attempts
             :max-fallback-hops max-fallback-hops
             :timings* timings*
             :transcript* transcript*
             :debug-transcript? debug-transcript?}]
    (loop [idx 0
           env env
           emitted nil]
      (if (>= idx (count nodes))
        (finalize-run telemetry* timings* transcript* env emitted)
        (let [node (nth nodes idx)]
          (telemetry-inc! telemetry* :nodes/total)
          (when (keyword? (:op node))
            (telemetry-inc-in! telemetry* [:nodes/by-op (:op node)]))
          (if-not (should-run-node? node env)
            (recur (inc idx) env emitted)
            (let [{env' :env
                   emitted' :emitted}
                  (run-node ctx node env emitted)]
              (recur (inc idx) env' emitted'))))))))
