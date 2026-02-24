(ns

    ^{:doc    "Domain contracts for Ferment protocol messages and invoke/retry flow."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.contracts

  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]))

;;
;; Protocol envelope (canonical, low-entropy)
;;

(def ^:const result-types
  #{:value :plan :stream :error})

(def ^:private requires-keys
  #{:in-schema
    :out-schema
    :result/type
    :cap/kind
    :cap/tags
    :effects/allowed})

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (trim-s v)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- keyword-coll?
  [v]
  (or (set? v)
      (and (sequential? v)
           (not (map? v)))))

(defn- keyword-set
  [v]
  (cond
    (keyword? v) #{v}
    (set? v) (into #{} (keep keywordish) v)
    (sequential? v) (into #{} (keep keywordish) v)
    :else #{}))

(defn- keyword-coll-input?
  [v]
  (or (keyword? v)
      (and (set? v)
           (every? #(or (keyword? %) (string? %)) v))
      (and (sequential? v)
           (not (map? v))
           (every? #(or (keyword? %) (string? %)) v))))

(defn- nonblank-string?
  [v]
  (boolean (trim-s v)))

(defn- contains-nonblank-string?
  [m ks]
  (boolean
   (some (fn [k]
           (nonblank-string? (get m k)))
         ks)))

(defn- map-has-any?
  [m ks]
  (boolean (some #(contains? m %) ks)))

(defn- req-route?
  [v]
  (and (map? v)
       (map? (:request v))))

(defn- req-context?
  [v]
  (and (map? v)
       (or (empty? v)
           (contains-nonblank-string? v [:text :prompt :summary])
           (map? (:context v)))))

(defn- req-text?
  [v]
  (and (map? v)
       (contains-nonblank-string? v [:prompt :text :content])))

(defn- req-problem?
  [v]
  (and (map? v)
       (contains-nonblank-string? v [:prompt :problem :question :task])))

(defn- req-code?
  [v]
  (and (map? v)
       (contains-nonblank-string? v [:prompt :task :spec :code])))

(defn- req-eval?
  [v]
  (and (map? v)
       (or (map? (:task/result v))
           (map? (:result v))
           (contains? v :score))))

(defn- req-meta?
  [v]
  (map? v))

(defn- res-route?
  [v]
  (and (map? v)
       (keyword? (:cap/id v))))

(defn- res-context-summary?
  [v]
  (and (map? v)
       (contains-nonblank-string? v [:summary :text :content])))

(defn- res-text?
  [v]
  (and (map? v)
       (contains-nonblank-string? v [:text :content])))

(defn- res-problem?
  [v]
  (and (map? v)
       (or (contains-nonblank-string? v [:answer :text :summary])
           (map? (:plan v))
           (map-has-any? v [:steps :open_questions]))))

(defn- res-code?
  [v]
  (and (map? v)
       (or (contains-nonblank-string? v [:code :patch :text])
           (sequential? (:files v)))))

(defn- res-patch+tests?
  [v]
  (and (map? v)
       (or (contains-nonblank-string? v [:patch :text])
           (sequential? (:files v))
           (sequential? (:tests v)))))

(defn- res-explanation?
  [v]
  (and (map? v)
       (contains-nonblank-string? v [:text :explanation :summary])))

(defn- res-review?
  [v]
  (and (map? v)
       (or (contains-nonblank-string? v [:text :summary])
           (sequential? (:findings v))
           (set? (:violations v)))))

(defn- score-in-unit-range?
  [n]
  (and (number? n)
       (<= 0.0 (double n) 1.0)))

(defn- res-eval?
  [v]
  (and (map? v)
       (score-in-unit-range? (:score v))
       (or (not (contains? v :retry?))
           (boolean? (:retry? v)))
       (or (not (contains? v :violations))
           (or (set? (:violations v))
               (sequential? (:violations v))))))

(defn- res-meta?
  [v]
  (map? v))

(s/def :req/any map?)
(s/def :req/route req-route?)
(s/def :req/context req-context?)
(s/def :req/text req-text?)
(s/def :req/problem req-problem?)
(s/def :req/code req-code?)
(s/def :req/eval req-eval?)
(s/def :req/meta req-meta?)

(s/def :res/any map?)
(s/def :res/route res-route?)
(s/def :res/context-summary res-context-summary?)
(s/def :res/text res-text?)
(s/def :res/problem res-problem?)
(s/def :res/code res-code?)
(s/def :res/patch+tests res-patch+tests?)
(s/def :res/explanation res-explanation?)
(s/def :res/review res-review?)
(s/def :res/eval res-eval?)
(s/def :res/meta res-meta?)

(defn normalize-requires
  "Normalizes `:task/:requires` map into canonical keyword-based shape."
  [requires]
  (when (map? requires)
    (let [in-schema (keywordish (:in-schema requires))
          out-schema (keywordish (:out-schema requires))
          result-type (keywordish (:result/type requires))
          cap-kind (keywordish (:cap/kind requires))
          cap-tags (keyword-set (:cap/tags requires))
          effects-allowed (keyword-set (:effects/allowed requires))]
      (cond-> {}
        (keyword? in-schema) (assoc :in-schema in-schema)
        (keyword? out-schema) (assoc :out-schema out-schema)
        (keyword? result-type) (assoc :result/type result-type)
        (keyword? cap-kind) (assoc :cap/kind cap-kind)
        (seq cap-tags) (assoc :cap/tags cap-tags)
        (seq effects-allowed) (assoc :effects/allowed effects-allowed)))))

(defn effective-in-schema
  "Returns effective input schema key for request intent, optionally overridden by `:requires`."
  [protocol intent requires]
  (or (:in-schema (normalize-requires requires))
      (get-in protocol [:intents intent :in-schema])))

(defn effective-out-schema
  "Returns effective output schema key for request intent, optionally overridden by `:requires`."
  [protocol intent requires]
  (or (:out-schema (normalize-requires requires))
      (get-in protocol [:intents intent :out-schema])))

(defn- validate-requires-shape
  [requires]
  (let [unknown (when (map? requires)
                  (seq (remove requires-keys (keys requires))))
        normalized (normalize-requires requires)]
    (cond
      (nil? requires) {:ok? true}
      (not (map? requires))
      {:ok? false :reason :requires/not-map}
      (seq unknown)
      {:ok? false
       :reason :requires/unknown-keys
       :unknown (vec (sort-by str unknown))}
      (and (contains? requires :in-schema)
           (not (keyword? (:in-schema normalized))))
      {:ok? false :reason :requires/in-schema-not-keyword}
      (and (contains? requires :out-schema)
           (not (keyword? (:out-schema normalized))))
      {:ok? false :reason :requires/out-schema-not-keyword}
      (and (contains? requires :result/type)
           (not (contains? result-types (:result/type normalized))))
      {:ok? false :reason :requires/result-type-not-supported}
      (and (contains? requires :cap/kind)
           (not (keyword? (:cap/kind normalized))))
      {:ok? false :reason :requires/cap-kind-not-keyword}
      (and (contains? requires :cap/tags)
           (not (keyword-coll-input? (:cap/tags requires))))
      {:ok? false :reason :requires/cap-tags-not-keywords}
      (and (contains? requires :effects/allowed)
           (not (keyword-coll-input? (:effects/allowed requires))))
      {:ok? false :reason :requires/effects-not-keywords}
      :else
      {:ok? true})))

(s/def :ferment.contracts/proto pos-int?)
(s/def :ferment.contracts/id string?)
(s/def :ferment.contracts/turn nat-int?)
(s/def :ferment.contracts/intent keyword?)
(s/def :ferment.contracts/trace
  (s/keys :req-un [:ferment.contracts/id]
          :opt-un [:ferment.contracts/turn]))
(s/def :ferment.contracts/role keyword?)
(s/def :ferment.contracts/input map?)
(s/def :ferment.contracts/requires
  (s/and map?
         #(-> (validate-requires-shape %) :ok?)))
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

(def ^:private builtin-schema-validators
  {:req/any             :req/any
   :req/route           :req/route
   :req/context         :req/context
   :req/text            :req/text
   :req/problem         :req/problem
   :req/code            :req/code
   :req/eval            :req/eval
   :req/meta            :req/meta
   :res/any             :res/any
   :res/route           :res/route
   :res/context-summary :res/context-summary
   :res/text            :res/text
   :res/problem         :res/problem
   :res/code            :res/code
   :res/patch+tests     :res/patch+tests
   :res/explanation     :res/explanation
   :res/review          :res/review
   :res/eval            :res/eval
   :res/meta            :res/meta})

(defn- validate-schema-by-spec
  [spec-kw value]
  (if (s/valid? spec-kw value)
    {:ok? true}
    {:ok? false
     :reason :schema/invalid
     :schema spec-kw
     :explain (s/explain-data spec-kw value)}))

(defn- validate-schema-by-fn
  [validator value schema-key]
  (let [raw (validator value)]
    (cond
      (map? raw)
      (assoc raw :ok? (boolean (:ok? raw)))

      (boolean? raw)
      {:ok? raw}

      :else
      {:ok? false
       :reason :schema/validator-invalid-return
       :schema schema-key})))

(defn validate-schema
  "Validates `value` against a configured schema key.

  Validator lookup order:
  1. `protocol` `:schema/validators`
  2. built-in validators for `:req/*` and `:res/*` schemas."
  ([schema-key value]
   (validate-schema nil schema-key value))
  ([protocol schema-key value]
   (if-not (keyword? schema-key)
     {:ok? false
      :reason :schema/not-keyword
      :schema schema-key}
     (let [registry (merge builtin-schema-validators
                           (if (map? (:schema/validators protocol))
                             (:schema/validators protocol)
                             {}))
           validator (get registry schema-key)]
       (cond
         (nil? validator)
         {:ok? false
          :reason :schema/not-found
          :schema schema-key}

         (keyword? validator)
         (validate-schema-by-spec validator value)

         (fn? validator)
         (validate-schema-by-fn validator value schema-key)

         (and (map? validator) (keyword? (:spec validator)))
         (validate-schema-by-spec (:spec validator) value)

         (and (map? validator) (fn? (:fn validator)))
         (validate-schema-by-fn (:fn validator) value schema-key)

         :else
         {:ok? false
          :reason :schema/validator-invalid
          :schema schema-key
          :validator validator})))))

(defn- merge-done
  [base override]
  (let [base' (if (map? base) base {})
        over' (if (map? override) override {})
        merged (merge base' over')]
    (cond-> merged
      (or (contains? base' :must) (contains? over' :must))
      (assoc :must (set/union (keyword-set (:must base'))
                              (keyword-set (:must over'))))

      (or (contains? base' :should) (contains? over' :should))
      (assoc :should (set/union (keyword-set (:should base'))
                                (keyword-set (:should over')))))))

(defn intent-policy
  "Returns normalized policy map for `intent`, with defaults inherited from protocol."
  [protocol intent]
  (let [default-policy
        (if (map? (:policy/default protocol))
          (:policy/default protocol)
          {})
        intent-policy*
        (if (map? (get-in protocol [:policy/intents intent]))
          (get-in protocol [:policy/intents intent])
          {})
        done* (merge-done (:done default-policy) (:done intent-policy*))
        checks* (set/union (keyword-set (:checks default-policy))
                           (keyword-set (:checks intent-policy*)))
        switch-on* (set/union (keyword-set (:switch-on default-policy))
                              (keyword-set (:switch-on intent-policy*)))
        fallback* (vec (distinct (concat (or (:fallback default-policy) [])
                                         (or (:fallback intent-policy*) []))))
        retry* (merge (if (map? (:retry default-policy)) (:retry default-policy) {})
                      (if (map? (:retry intent-policy*)) (:retry intent-policy*) {}))
        judge* (merge (if (map? (:judge default-policy)) (:judge default-policy) {})
                      (if (map? (:judge intent-policy*)) (:judge intent-policy*) {}))]
    (cond-> {}
      (seq done*) (assoc :done done*)
      (seq checks*) (assoc :checks checks*)
      (seq switch-on*) (assoc :switch-on switch-on*)
      (seq fallback*) (assoc :fallback fallback*)
      (seq retry*) (assoc :retry retry*)
      (seq judge*) (assoc :judge judge*))))

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
     (let [intent   (get-in request [:task :intent])
           requires (get-in request [:task :requires])
           intent-check (validate-intent-vs-protocol request protocol)]
       (if-not (:ok? intent-check)
         {:ok? false
          :error :invalid-request
          :reason (:reason intent-check)
          :intent (:intent intent-check)}
         (let [requires-check (validate-requires-shape requires)]
           (if-not (:ok? requires-check)
             {:ok? false
              :error :invalid-request
              :reason (:reason requires-check)
              :details (select-keys requires-check [:unknown])}
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
                     (let [schema-k (effective-in-schema protocol intent requires)]
                       (if (keyword? schema-k)
                         (let [input-check (validate-schema protocol schema-k (:input request))]
                           (if (:ok? input-check)
                             {:ok? true}
                             {:ok? false
                              :error :invalid-request
                              :reason :input/schema-invalid
                              :schema schema-k
                              :details (select-keys input-check [:reason :explain :schema])}))
                         {:ok? true})))))))))
       ))))

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

(defn- validate-route-solver->voice-plan
  [plan]
  (let [nodes (when (map? plan) (:nodes plan))
        node0 (when (vector? nodes) (nth nodes 0 nil))
        node1 (when (vector? nodes) (nth nodes 1 nil))
        node2 (when (vector? nodes) (nth nodes 2 nil))
        voice-prompt-slot (get-in node1 [:input :prompt :slot/id])
        emit-slot (get-in node2 [:input :slot/id])]
    (cond
      (not (map? plan))
      {:ok? false
       :reason :route/plan-not-map}

      (not (vector? nodes))
      {:ok? false
       :reason :route/plan-nodes-not-vector}

      (< (count nodes) 3)
      {:ok? false
       :reason :route/plan-too-short}

      (not= :call (:op node0))
      {:ok? false
       :reason :route/plan-first-op-not-call}

      (not= :problem/solve (:intent node0))
      {:ok? false
       :reason :route/plan-first-intent-not-problem-solve}

      (not= :solver (:as node0))
      {:ok? false
       :reason :route/plan-first-as-not-solver}

      (not (nonblank-string? (get-in node0 [:input :prompt])))
      {:ok? false
       :reason :route/plan-solver-prompt-not-string}

      (not= :call (:op node1))
      {:ok? false
       :reason :route/plan-second-op-not-call}

      (not= :text/respond (:intent node1))
      {:ok? false
       :reason :route/plan-second-intent-not-text-respond}

      (not= :voice (:as node1))
      {:ok? false
       :reason :route/plan-second-as-not-voice}

      (not= [:solver :out :text] voice-prompt-slot)
      {:ok? false
       :reason :route/plan-voice-prompt-not-solver-slot}

      (not= :emit (:op node2))
      {:ok? false
       :reason :route/plan-third-op-not-emit}

      (not= [:voice :out] emit-slot)
      {:ok? false
       :reason :route/plan-emit-not-voice-out}

      :else
      {:ok? true})))

(defn- validate-intent-result-shape
  [protocol intent result]
  (let [judge-intent (or (get-in (intent-policy protocol intent) [:judge :intent])
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
           (= :route/solver->voice contract-kind))
      (cond
        (contains? result :error)
        {:ok? false
         :reason :route/plan-error-not-allowed}

        (and (keyword? (:type contract))
             (not= (:type contract) (result-type-of result)))
        {:ok? false
         :reason :route/plan-result-type-not-plan
         :result/type (result-type-of result)}

        :else
        (validate-route-solver->voice-plan (result-plan-of result)))

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

(defn- validate-requires-result-shape
  [protocol intent requires result]
  (let [requires' (normalize-requires requires)
        expected-type (:result/type requires')
        actual-type   (result-type-of result)
        out-schema    (effective-out-schema protocol intent requires')]
    (cond
      (and (keyword? expected-type)
           (not= expected-type actual-type))
      {:ok? false
       :reason :requires/result-type-mismatch
       :expected expected-type
       :actual actual-type}

      (and (= :value actual-type)
           (keyword? out-schema))
      (let [out-check (validate-schema protocol out-schema (result-out-of result))]
        (if (:ok? out-check)
          {:ok? true}
          {:ok? false
           :reason :output/schema-invalid
           :schema out-schema
           :details (select-keys out-check [:reason :schema :explain])}))

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
   (validate-result protocol intent result nil))
  ([protocol intent result requires]
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
           (if-not (:ok? intent-check)
             {:ok? false
              :error :invalid-result
              :reason (:reason intent-check)
              :intent intent
              :result/type rtype}
             (let [requires-check (validate-requires-result-shape protocol intent requires result)]
               (if (:ok? requires-check)
                 {:ok? true}
                 {:ok? false
                  :error :invalid-result
                  :reason (:reason requires-check)
                  :intent intent
                  :result/type rtype
                  :details (select-keys requires-check [:expected :actual :schema :details])}))))))

     :else
     (let [intent-check (validate-intent-result-shape protocol intent result)]
       (if-not (:ok? intent-check)
         {:ok? false
          :error :invalid-result
          :reason (:reason intent-check)
          :intent intent}
         (let [requires-check (validate-requires-result-shape protocol intent requires result)]
           (if (:ok? requires-check)
             {:ok? true}
             {:ok? false
              :error :invalid-result
              :reason (:reason requires-check)
              :intent intent
              :details (select-keys requires-check [:expected :actual :schema :details])})))))))

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
         max-attempts  (long (max 1 max-attempts))
         intent        (get-in request [:task :intent])
         requires      (get-in request [:task :requires])]
     (if-not (:ok? request-check)
       request-check
       (loop [attempt 1]
         (let [result (invoke-fn request attempt)
               check  (validate-result protocol intent result requires)]
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
