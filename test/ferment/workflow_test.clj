(ns

    ^{:doc    "Workflow evaluator tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.workflow-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.workflow :as workflow]))

(deftest execute-plan-runs-call-and-emits-materialized-out
  (testing "Evaluator resolves capability, executes call and emits slot-path output."
    (let [plan {:nodes [{:op :call
                         :intent :text/respond
                         :input {:prompt {:slot/id :summary}}
                         :as :answer}
                        {:op :emit
                         :input {:slot/id [:answer :out]}}]}
          run  (workflow/execute-plan
                {:plan plan
                 :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}
                 :env {:summary "hej"}
                 :invoke-call (fn [call-node _env]
                                {:result {:type :value
                                          :out {:text (str "ECHO:" (get-in call-node [:input :prompt]))}}})})]
      (is (:ok? run))
      (is (= {:text "ECHO:hej"} (:emitted run)))
      (is (= :llm/voice (get-in run [:env :answer :cap/id])))
      (is (= 1 (count (:timings run))))
      (is (number? (get-in run [:timings 0 :latency-ms]))))))

(deftest execute-plan-resumes-from-checkpoint
  (testing "Evaluator resumes from persisted checkpoint without re-executing finished nodes."
    (let [calls (atom 0)
          plan {:nodes [{:op :let
                         :value {:text "warmup"}
                         :as :seed}
                        {:op :call
                         :intent :text/respond
                         :input {:prompt {:slot/id [:seed :text]}}
                         :as :answer}
                        {:op :emit
                         :input {:slot/id [:answer :out]}}]}
          run (workflow/execute-plan
               {:plan plan
                :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}
                :resume/checkpoint {:next-index 1
                                    :env {:seed {:text "checkpoint-seed"}}
                                    :emitted nil}
                :invoke-call (fn [call-node _env]
                               (swap! calls inc)
                               {:result {:type :value
                                         :out {:text (str "ECHO:" (get-in call-node [:input :prompt]))}}})})]
      (is (:ok? run))
      (is (= 1 @calls))
      (is (= {:text "ECHO:checkpoint-seed"} (:emitted run))))))

(deftest execute-plan-tool-node-precommits-checkpoint-before-outer-loop
  (testing "Tool node persists checkpoint before outer-loop hook, so resume does not rerun side effects."
    (let [tool-calls (atom 0)
          events (atom [])
          plan {:nodes [{:op :tool
                         :tool/id :tool/demo
                         :effects {:allowed #{:none}}
                         :as :tool-out}
                        {:op :emit
                         :input {:slot/id [:tool-out :out]}}]}
          crash (try
                  (workflow/execute-plan
                   {:plan plan
                    :resolver {}
                    :on-node-state (fn [event] (swap! events conj event))
                    :after-node (fn [{node-index :node/index}]
                                  (when (zero? (long node-index))
                                    (throw (ex-info "synthetic-crash-after-tool" {:node/index node-index}))))
                    :invoke-tool (fn [_ _]
                                   (swap! tool-calls inc)
                                   {:result {:type :value
                                             :out {:text "tool-ok"}}})})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    {:message (ex-message e)
                     :data (ex-data e)}))
          checkpoint (some->> @events
                              (filter #(= :node/succeeded (:event/type %)))
                              last
                              :checkpoint)
          resumed (workflow/execute-plan
                   {:plan plan
                    :resolver {}
                    :resume/checkpoint checkpoint
                    :invoke-tool (fn [_ _]
                                   (swap! tool-calls inc)
                                   {:result {:type :value
                                             :out {:text "tool-ok"}}})})]
      (is (= "synthetic-crash-after-tool" (:message crash)))
      (is (map? checkpoint))
      (is (= 1 (:next-index checkpoint)))
      (is (= 1 @tool-calls))
      (is (:ok? resumed))
      (is (= {:text "tool-ok"} (:emitted resumed))))))

(deftest execute-plan-notifies-node-state-callback
  (testing "Evaluator reports node lifecycle transitions to callback."
    (let [events (atom [])
          plan {:nodes [{:op :call
                         :intent :text/respond
                         :as :answer}
                        {:op :emit
                         :input {:slot/id [:answer :out]}}]}
          run (workflow/execute-plan
               {:plan plan
                :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}
                :on-node-state (fn [event] (swap! events conj event))
                :invoke-call (fn [_ _]
                               {:result {:type :value
                                         :out {:text "ok"}}})})]
      (is (:ok? run))
      (is (= [:node/pending :node/running :node/succeeded
              :node/pending :node/running :node/succeeded]
             (mapv :event/type @events)))
      (is (= 2 (count (filter #(contains? % :checkpoint) @events)))))))

(deftest execute-plan-recurses-when-call-returns-plan
  (testing "Evaluator executes nested plan returned by a call and propagates emitted output."
    (let [calls (atom [])
          plan  {:nodes [{:op :call
                          :intent :route/decide
                          :as :route}
                         {:op :emit
                          :input {:slot/id [:route :out]}}]}
          run   (workflow/execute-plan
                 {:plan plan
                  :resolver {:routing {:intent->cap {:route/decide :llm/meta
                                                     :text/respond :llm/voice}}}
                  :invoke-call
                  (fn [call-node _env]
                    (swap! calls conj (:intent call-node))
                    (case (:intent call-node)
                      :route/decide
                      {:result {:type :plan
                                :plan {:nodes [{:op :call
                                                :intent :text/respond
                                                :input {:prompt {:slot/id :summary}}
                                                :as :answer}
                                               {:op :emit
                                                :input {:slot/id [:answer :out]}}]}
                                :bindings {:summary "z planu"}}}
                      :text/respond
                      {:result {:type :value
                                :out {:text (str "VOICE:" (get-in call-node [:input :prompt]))}}}))})]
      (is (:ok? run))
      (is (= {:text "VOICE:z planu"} (:emitted run)))
      (is (= [:route/decide :text/respond] @calls))
      (is (= 2 (count (:timings run))))
      (is (= #{:route/decide :text/respond}
             (->> (:timings run) (map :intent) set))))))

(deftest execute-plan-respects-failure-conditions
  (testing "Node guarded by `:when {:failed? ...}` runs only after failed call."
    (let [called (atom [])
          plan   {:nodes [{:op :call
                           :intent :demo/fail
                           :dispatch {:allow-failure? true}
                           :as :failed-call}
                          {:op :call :intent :demo/repair :when {:failed? :failed-call} :as :repair}
                          {:op :call :intent :demo/skip :when {:failed? :repair} :as :skip}
                          {:op :emit :input {:slot/id [:repair :out]}}]}
          run    (workflow/execute-plan
                  {:plan plan
                   :resolver {:routing {:intent->cap {:demo/fail :llm/mock
                                                      :demo/repair :llm/meta
                                                      :demo/skip :llm/voice}}}
                   :invoke-call
                   (fn [call-node _env]
                     (swap! called conj (:intent call-node))
                     (case (:intent call-node)
                       :demo/fail {:error {:type :schema/invalid}}
                       :demo/repair {:result {:type :value
                                              :out {:text "fixed"}}}
                       :demo/skip {:result {:type :value
                                            :out {:text "should-not-run"}}}))})]
      (is (:ok? run))
      (is (= {:text "fixed"} (:emitted run)))
      (is (= [:demo/fail :demo/repair] @called)))))

(deftest execute-plan-emits-env-keyword-reference
  (testing "Emit can reference env value directly by keyword."
    (let [run (workflow/execute-plan
               {:plan {:nodes [{:op :let :value {:text "z let"} :as :answer}
                               {:op :emit :input :answer}]}
                :resolver {}
                :invoke-call (fn [_ _]
                               {:result {:type :value
                                         :out {:text "unused"}}})})]
      (is (:ok? run))
      (is (= {:text "z let"} (:emitted run))))))

(deftest execute-plan-retries-same-capability-when-switch-on-matches
  (testing "Invalid output can be retried on the same capability when policy enables switch-on."
    (let [calls (atom 0)
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :code/patch
                                  :dispatch {:candidates [:llm/code]
                                             :retry {:same-cap-max 1}
                                             :switch-on #{:schema/invalid}}
                                  :done {:must #{:schema-valid}}
                                  :as :answer}
                                 {:op :emit :input {:slot/id [:answer :out]}}]}
                  :resolver {}
                  :invoke-call
                  (fn [_ _]
                    (if (= 1 (swap! calls inc))
                      {:result {:type :value}}
                      {:result {:type :value
                                :out {:text "ok-po-retry"}}}))})]
      (is (:ok? run))
      (is (= 2 @calls))
      (is (= {:text "ok-po-retry"} (:emitted run))))))

(deftest execute-plan-fallbacks-to-next-candidate-on-low-score
  (testing "Low score triggers fallback candidate when `:eval/low-score` is switch-on."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice-a :llm/voice-b]
                                             :retry {:fallback-max 1}
                                             :switch-on #{:eval/low-score}}
                                  :done {:should #{:tests-pass}
                                         :score-min 1.0}
                                  :as :answer}
                                 {:op :emit :input {:slot/id [:answer :out]}}]}
                  :resolver {}
                  :check-fns {:tests-pass
                              (fn [call-node _env _result]
                                (= :llm/voice-b (:cap/id call-node)))}
                  :invoke-call
                  (fn [call-node _env]
                    (swap! calls conj (:cap/id call-node))
                    {:result {:type :value
                              :out {:text (name (:cap/id call-node))}}})})]
      (is (:ok? run))
      (is (= [:llm/voice-a :llm/voice-b] @calls))
      (is (= {:text "voice-b"} (:emitted run))))))

(deftest execute-plan-applies-per-intent-quality-checks
  (testing "Per-intent :policy/intents :checks are enforced as hard gates even when node does not provide :done."
    (let [err (try
                (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice]}
                                  :as :answer}
                                 {:op :emit :input {:slot/id [:answer :out]}}]}
                  :resolver {:protocol {:policy/intents {:text/respond
                                                         {:checks [:tests-pass]
                                                          :done {:score-min 1.0}}}
                                       :result/types [:value]}}
                  :check-fns {:tests-pass (fn [_ _ _] false)}
                  :invoke-call (fn [_ _]
                                 {:result {:type :value
                                           :out {:text "x"}}})})
                nil
                (catch clojure.lang.ExceptionInfo e
                  (ex-data e)))]
      (is (= :eval/must-failed (get-in err [:outcome :failure/type])))
      (is (= [:tests-pass] (get-in err [:outcome :done/eval :must-failed])))
      (is (false? (get-in err [:outcome :failure/recover?]))))))

(deftest execute-plan-supports-per-node-hard-soft-checks
  (testing "Node dispatch may override hard checks and move selected checks to soft scoring."
    (let [run (workflow/execute-plan
               {:plan {:nodes [{:op :call
                                :intent :text/respond
                                :dispatch {:candidates [:llm/voice]
                                           :checks/hard [:schema-valid]
                                           :checks/soft [:no-list-expansion]}
                                :done {:score-min 0.0}
                                :as :answer}
                               {:op :emit :input {:slot/id [:answer :out]}}]}
                :resolver {:protocol {:policy/intents {:text/respond
                                                       {:checks [:no-list-expansion]
                                                        :done {:must #{:schema-valid}
                                                               :score-min 0.85}}}
                                     :result/types [:value]}}
                :check-fns {:schema-valid (fn [_ _ _] true)
                            :no-list-expansion (fn [_ _ _] false)}
                :invoke-call (fn [_ _]
                               {:result {:type :value
                                         :out {:text "- item"}}})})]
      (is (:ok? run))
      (is (= {:text "- item"} (:emitted run)))
      (is (= 0 (get-in run [:telemetry :quality/must-failed]))))))

(deftest execute-plan-fails-on-invalid-result-without-switch-policy
  (testing "Invalid result fails fast when failure type is not declared in :switch-on."
    (let [err (try
                (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice]}
                                  :as :answer}
                                 {:op :emit :input {:slot/id [:answer :out]}}]}
                  :resolver {}
                  :invoke-call
                  (fn [_ _]
                    {:result {:type :value}})})
                nil
                (catch clojure.lang.ExceptionInfo e
                  (ex-data e)))]
      (is (= :schema/invalid (get-in err [:outcome :failure/type])))
      (is (false? (get-in err [:outcome :failure/recover?]))))))

(deftest execute-plan-does-not-fallback-on-terminal-failure
  (testing "Terminal failure does not hop to next candidate unless :switch-on allows it."
    (let [calls (atom [])
          err   (try
                  (workflow/execute-plan
                   {:plan {:nodes [{:op :call
                                    :intent :text/respond
                                    :dispatch {:candidates [:llm/voice-a :llm/voice-b]
                                               :retry {:fallback-max 1}
                                               :switch-on #{:eval/low-score}}
                                    :as :answer}
                                   {:op :emit :input {:slot/id [:answer :out]}}]}
                    :resolver {}
                    :invoke-call
                    (fn [call-node _env]
                      (swap! calls conj (:cap/id call-node))
                      (if (= :llm/voice-a (:cap/id call-node))
                        {:error {:type :timeout/hard}}
                        {:result {:type :value
                                  :out {:text "should-not-run"}}}))})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))]
      (is (= :timeout/hard (get-in err [:outcome :failure/type])))
      (is (false? (get-in err [:outcome :failure/recover?])))
      (is (= [:llm/voice-a] @calls)))))

(deftest execute-plan-filters-candidates-by-capability-intent
  (testing "Routing rejects a candidate that does not declare intent support."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :route/decide
                                  :dispatch {:candidates [:llm/voice :llm/meta]}
                                  :as :answer}
                                 {:op :emit :input {:slot/id [:answer :out]}}]}
                  :resolver {:caps/by-id
                             {:llm/voice {:cap/id :llm/voice
                                          :cap/intents #{:text/respond}
                                          :cap/can-produce #{:value}
                                          :cap/effects-allowed #{:none}}
                              :llm/meta {:cap/id :llm/meta
                                         :cap/intents #{:route/decide}
                                         :cap/can-produce #{:value :plan}
                                         :cap/effects-allowed #{:none}}}}
                  :invoke-call
                  (fn [call-node _env]
                    (swap! calls conj (:cap/id call-node))
                    {:result {:type :value
                              :out {:text (name (:cap/id call-node))}}})})]
      (is (:ok? run))
      (is (= [:llm/meta] @calls))
      (is (= {:text "meta"} (:emitted run))))))

(deftest resolve-capability-decision-golden-intent-cap-matrix
  (testing "Golden matrix: resolver chooses canonical cap/id per intent."
    (let [resolver {:routing {:intent->cap {:route/decide :llm/meta
                                            :context/summarize :llm/meta
                                            :text/respond :llm/voice
                                            :problem/solve :llm/solver
                                            :code/generate :llm/code
                                            :code/patch :llm/code
                                            :code/explain :llm/code
                                            :code/review :llm/code
                                            :eval/grade :llm/judge}}
                    :caps/by-id {:llm/meta {:cap/id :llm/meta
                                            :cap/intents #{:route/decide :context/summarize}
                                            :cap/can-produce #{:value :plan}
                                            :cap/effects-allowed #{:none}}
                                 :llm/voice {:cap/id :llm/voice
                                             :cap/intents #{:text/respond}
                                             :cap/can-produce #{:value}
                                             :cap/effects-allowed #{:none}}
                                 :llm/solver {:cap/id :llm/solver
                                              :cap/intents #{:problem/solve}
                                              :cap/can-produce #{:value :plan}
                                              :cap/effects-allowed #{:none}}
                                 :llm/code {:cap/id :llm/code
                                            :cap/intents #{:code/generate :code/patch :code/explain :code/review}
                                            :cap/can-produce #{:value :plan}
                                            :cap/effects-allowed #{:none}}
                                 :llm/judge {:cap/id :llm/judge
                                             :cap/intents #{:eval/grade}
                                             :cap/can-produce #{:value}
                                             :cap/effects-allowed #{:none}}}}
          cases [[:route/decide :llm/meta]
                 [:context/summarize :llm/meta]
                 [:text/respond :llm/voice]
                 [:problem/solve :llm/solver]
                 [:code/generate :llm/code]
                 [:code/patch :llm/code]
                 [:code/explain :llm/code]
                 [:code/review :llm/code]
                 [:eval/grade :llm/judge]]]
      (doseq [[intent expected] cases]
        (let [decision (workflow/resolve-capability-decision resolver {:intent intent})]
          (is (= expected (:cap/id decision)))
          (is (empty? (:rejected-candidates decision))))))))

(deftest resolve-capability-decision-near-miss-reasons
  (testing "Near-miss suite reports deterministic rejection reasons for schema/effects/result-type mismatches."
    (let [resolver {:routing {:intent->cap {:text/respond :llm/voice}}
                    :caps/by-id {:llm/voice {:cap/id :llm/voice
                                             :cap/intents #{:text/respond}
                                             :cap/can-produce #{:value}
                                             :cap/effects-allowed #{:none}
                                             :io/in-schema :req/text
                                             :io/out-schema :res/text}}}
          cases [{:name :schema-mismatch
                  :node {:intent :text/respond
                         :dispatch {:candidates [:llm/voice]}
                         :requires {:out-schema :res/problem}}
                  :reason :requires/schema-mismatch}
                 {:name :effects-not-allowed
                  :node {:intent :text/respond
                         :dispatch {:candidates [:llm/voice]}
                         :effects {:allowed #{:fs/write}}}
                  :reason :effects/not-allowed}
                 {:name :result-type-not-supported
                  :node {:intent :text/respond
                         :dispatch {:candidates [:llm/voice]}
                         :requires {:result/type :plan}}
                  :reason :result-type/not-supported}]]
      (doseq [{:keys [name node reason]} cases]
        (let [decision (workflow/resolve-capability-decision resolver node)]
          (is (nil? (:cap/id decision)) (str "expected nil cap for " name))
          (is (= reason
                 (get-in decision [:rejected-candidates 0 :reason]))))))))

(deftest resolve-capability-decision-applies-gateway-strategy-ranking
  (testing "Gateway strategy can reorder valid candidates using model health and cost signals."
    (let [health* (atom {:ferment.model/voice {:calls 12
                                               :errors 7
                                               :latency/ema-ms 800.0
                                               :quality/ema 0.45}
                         :ferment.model/solver {:calls 12
                                                :errors 1
                                                :latency/ema-ms 1400.0
                                                :quality/ema 0.92}})
          resolver {:routing {:gateway {:strategy :quality-first}
                              :intent->cap {:text/respond :llm/voice}}
                    :gateway/model-health health*
                    :caps/by-id {:llm/voice {:cap/id :llm/voice
                                             :cap/intents #{:text/respond}
                                             :cap/can-produce #{:value}
                                             :cap/effects-allowed #{:none}
                                             :cap/cost {:latency-ms 1200}
                                             :dispatch/model-key :ferment.model/voice}
                                 :llm/solver-text {:cap/id :llm/solver-text
                                                   :cap/intents #{:text/respond}
                                                   :cap/can-produce #{:value}
                                                   :cap/effects-allowed #{:none}
                                                   :cap/cost {:latency-ms 1700}
                                                   :dispatch/model-key :ferment.model/solver}}}
          decision (workflow/resolve-capability-decision resolver
                                                         {:intent :text/respond
                                                          :dispatch {:candidates [:llm/voice
                                                                                  :llm/solver-text]}})]
      (is (= :llm/solver-text (:cap/id decision)))
      (is (= [:llm/solver-text :llm/voice] (:candidates decision)))
      (is (empty? (:rejected-candidates decision))))))

(deftest resolve-capability-decision-quarantines-open-circuit-candidate
  (testing "Gateway breaker filters out candidates in open-circuit state when alternatives exist."
    (let [future-ms (+ (System/currentTimeMillis) 60000)
          health* (atom {:ferment.model/voice {:calls 15
                                               :errors 12
                                               :latency/ema-ms 600.0
                                               :quality/ema 0.30
                                               :breaker/open-until-ms future-ms}})
          resolver {:routing {:gateway {:strategy :latency-first
                                        :circuit-breaker {:enabled? true
                                                          :min-samples 5
                                                          :error-rate-open 0.6
                                                          :cooldown-ms 30000}}
                              :intent->cap {:text/respond :llm/voice}}
                    :gateway/model-health health*
                    :caps/by-id {:llm/voice {:cap/id :llm/voice
                                             :cap/intents #{:text/respond}
                                             :cap/can-produce #{:value}
                                             :cap/effects-allowed #{:none}
                                             :dispatch/model-key :ferment.model/voice}
                                 :llm/solver-text {:cap/id :llm/solver-text
                                                   :cap/intents #{:text/respond}
                                                   :cap/can-produce #{:value}
                                                   :cap/effects-allowed #{:none}
                                                   :dispatch/model-key :ferment.model/solver}}}
          decision (workflow/resolve-capability-decision resolver
                                                         {:intent :text/respond
                                                          :dispatch {:candidates [:llm/voice
                                                                                  :llm/solver-text]}})]
      (is (= :llm/solver-text (:cap/id decision)))
      (is (= [:llm/solver-text] (:candidates decision)))
      (is (= :gateway/circuit-open
             (get-in decision [:rejected-candidates 0 :reason]))))))

(deftest execute-plan-filters-candidates-by-requires-out-schema
  (testing "CallNode :requires/:out-schema is a hard routing contract for candidate capabilities."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :requires {:out-schema :res/text}
                                  :dispatch {:candidates [:llm/solver :llm/voice]}
                                  :as :answer}
                                 {:op :emit
                                  :input {:slot/id [:answer :out]}}]}
                  :resolver {:caps/by-id
                             {:llm/solver {:cap/id :llm/solver
                                           :cap/intents #{:text/respond}
                                           :cap/can-produce #{:value}
                                           :cap/effects-allowed #{:none}
                                           :io/in-schema :req/problem
                                           :io/out-schema :res/problem}
                              :llm/voice  {:cap/id :llm/voice
                                           :cap/intents #{:text/respond}
                                           :cap/can-produce #{:value}
                                           :cap/effects-allowed #{:none}
                                           :io/in-schema :req/text
                                           :io/out-schema :res/text}}}
                  :invoke-call
                  (fn [call-node _env]
                    (swap! calls conj (:cap/id call-node))
                    {:result {:type :value
                              :out {:text (name (:cap/id call-node))}}})})]
      (is (:ok? run))
      (is (= [:llm/voice] @calls))
      (is (= {:text "voice"} (:emitted run))))))

(deftest execute-plan-fails-when-handoff-text-missing
  (testing "Voice handoff input contract requires :handoff/text and fails with controlled schema error."
    (let [calls (atom [])
          err (try
                (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice]
                                             :allow-failure? true}
                                  :as :voice-primary}
                                 {:op :call
                                  :intent :problem/solve
                                  :dispatch {:candidates [:llm/solver]}
                                  :as :solver
                                  :when {:failed? :voice-primary}}
                                 {:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice]}
                                  :input/schema :req/handoff
                                  :input {:handoff/text {:slot/id [:solver :out :text]}}
                                  :as :voice-final
                                  :when {:failed? :voice-primary}}
                                 {:op :emit
                                  :when {:failed? :voice-primary}
                                  :input {:slot/id [:voice-final :out]}}]}
                  :resolver {}
                  :invoke-call (fn [call-node _env]
                                 (swap! calls conj (:as call-node))
                                 (case (:as call-node)
                                   :voice-primary {:error {:type :eval/low-score}}
                                   :solver {:result {:type :value
                                                     :out {}}}
                                   :voice-final {:result {:type :value
                                                          :out {:text "should-not-run"}}}))})
                nil
                (catch clojure.lang.ExceptionInfo e
                  (ex-data e)))]
      (is (= :input/schema-invalid (:error err)))
      (is (= :req/handoff (:schema err)))
      (is (= [:voice-primary :solver] @calls)))))

(deftest execute-plan-uses-policy-registry-for-retry-and-fallback
  (testing "Per-intent policy registry drives retry/switch-on/fallback even without node dispatch overrides."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :as :answer}
                                 {:op :emit
                                  :input {:slot/id [:answer :out]}}]}
                  :resolver {:routing {:intent->cap {:text/respond :llm/voice-a}}
                             :protocol {:policy/default {:retry {:same-cap-max 1
                                                                 :fallback-max 1}
                                                         :switch-on #{:eval/low-score}
                                                         :fallback [:llm/voice-b]}
                                        :policy/intents {:text/respond {:done {:must #{:schema-valid}
                                                                               :score-min 1.0}
                                                                        :checks [:schema-valid]}}
                                        :result/types [:value]}}
                  :invoke-call
                  (fn [call-node _env]
                    (swap! calls conj (:cap/id call-node))
                    (if (= :llm/voice-a (:cap/id call-node))
                      {:error {:type :eval/low-score}}
                      {:result {:type :value
                                :out {:text "ok"}}}))})]
      (is (:ok? run))
      (is (= [:llm/voice-a :llm/voice-a :llm/voice-b] @calls))
      (is (= {:text "ok"} (:emitted run))))))

(deftest execute-plan-uses-policy-profile-overrides-per-intent
  (testing "Resolver policy profile overrides retry/fallback/switch-on for selected intent."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :as :answer}
                                 {:op :emit
                                  :input {:slot/id [:answer :out]}}]}
                  :resolver {:routing {:intent->cap {:text/respond :llm/voice-a}}
                             :policy/profile :high-quality
                             :policy/profiles {:high-quality {:default {:retry {:same-cap-max 1
                                                                                 :fallback-max 1}
                                                                        :switch-on #{:eval/low-score}
                                                                        :fallback [:llm/voice-b]
                                                                        :done {:must #{:schema-valid}
                                                                               :score-min 1.0}
                                                                        :checks/hard [:schema-valid]}}}
                             :protocol {:policy/default {:retry {:same-cap-max 0
                                                                 :fallback-max 0}
                                                         :switch-on #{:schema/invalid}
                                                         :fallback []}
                                        :policy/intents {:text/respond {:done {:must #{:schema-valid}
                                                                               :score-min 1.0}
                                                                        :checks [:schema-valid]}}
                                        :result/types [:value]}}
                  :invoke-call
                  (fn [call-node _env]
                    (swap! calls conj (:cap/id call-node))
                    (if (= :llm/voice-b (:cap/id call-node))
                      {:result {:type :value
                                :out {:text "ok"}}}
                      {:error {:type :eval/low-score}}))})]
      (is (:ok? run))
      (is (= [:llm/voice-a :llm/voice-a :llm/voice-b] @calls))
      (is (= {:text "ok"} (:emitted run))))))

(deftest execute-plan-enforces-call-tree-attempt-limit
  (testing "Workflow aborts with deterministic failure type when call attempt limit is exceeded."
    (let [calls (atom [])
          err   (try
                  (workflow/execute-plan
                   {:plan {:nodes [{:op :call
                                    :intent :text/respond
                                    :dispatch {:candidates [:llm/voice-a :llm/voice-b]
                                               :retry {:same-cap-max 2
                                                       :fallback-max 1}
                                               :switch-on #{:eval/low-score}}
                                    :as :answer}]}
                    :resolver {}
                    :max-call-attempts 2
                    :invoke-call (fn [call-node _env]
                                   (swap! calls conj (:cap/id call-node))
                                   {:error {:type :eval/low-score}})})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))]
      (is (= :policy/call-tree-limit (get-in err [:outcome :failure/type])))
      (is (= 2 (count @calls))))))

(deftest execute-plan-enforces-fallback-hop-limit
  (testing "Workflow aborts when fallback-hop budget is exhausted in one request."
    (let [calls (atom [])
          err   (try
                  (workflow/execute-plan
                   {:plan {:nodes [{:op :call
                                    :intent :text/respond
                                    :dispatch {:candidates [:llm/voice-a :llm/voice-b :llm/voice-c]
                                               :retry {:same-cap-max 0
                                                       :fallback-max 2}
                                               :switch-on #{:eval/low-score}}
                                    :as :answer}]}
                    :resolver {}
                    :max-fallback-hops 1
                    :invoke-call (fn [call-node _env]
                                   (swap! calls conj (:cap/id call-node))
                                   {:error {:type :eval/low-score}})})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))]
      (is (= :policy/fallback-limit (get-in err [:outcome :failure/type])))
      (is (= [:llm/voice-a :llm/voice-b] @calls)))))

(deftest execute-plan-rejects-capability-when-effects-not-allowed
  (testing "Routing rejects capability when node requires effects outside `:cap/effects-allowed`."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unable to resolve capability candidates"
         (workflow/execute-plan
          {:plan {:nodes [{:op :call
                           :intent :code/patch
                           :effects {:allowed #{:fs/write}}
                           :dispatch {:candidates [:llm/code]}
                           :as :answer}]}
           :resolver {:caps/by-id
                      {:llm/code {:cap/id :llm/code
                                  :cap/intents #{:code/patch}
                                  :cap/can-produce #{:value :plan}
                                  :cap/effects-allowed #{:none}}}}
           :invoke-call
           (fn [_ _]
             {:result {:type :value
                       :out {:text "should-not-run"}}})})))))

(deftest execute-plan-rejects-node-when-auth-forbids-effects
  (testing "Authenticated principal cannot execute call when effect policy denies requested effects."
    (let [err (try
                (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :code/patch
                                  :effects {:allowed #{:fs/write}}
                                  :dispatch {:candidates [:llm/code]}
                                  :as :answer}]}
                  :resolver {:caps/by-id
                             {:llm/code {:cap/id :llm/code
                                         :cap/intents #{:code/patch}
                                         :cap/can-produce #{:value :plan}
                                         :cap/effects-allowed #{:fs/write}}}}
                  :env {:auth/user {:user/id 7
                                    :user/account-type :user}
                        :roles/config {:enabled? true
                                       :authorize-default? false
                                       :account-type->roles {:user #{:role/user}
                                                             :manager #{:role/admin}}
                                       :effects {:fs/write {:any #{:role/admin}}}}}
                  :invoke-call
                  (fn [_ _]
                    {:result {:type :value
                              :out {:text "should-not-run"}}})})
                nil
                (catch clojure.lang.ExceptionInfo e
                  (ex-data e)))]
      (is (= :auth/forbidden-effect (:error err)))
      (is (= :auth/forbidden-effect (:failure/type err)))
      (is (= #{:fs/write} (:requested-effects err)))
      (is (= #{:fs/write} (:denied-effects err))))))

(deftest execute-plan-emits-telemetry-for-retry-and-fallback
  (testing "Telemetry counts retry/fallback and call statuses."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice-a :llm/voice-b]
                                             :retry {:same-cap-max 1
                                                     :fallback-max 1}
                                             :switch-on #{:eval/low-score}}
                                  :as :answer}
                                 {:op :emit :input {:slot/id [:answer :out]}}]}
                  :resolver {}
                  :invoke-call
                  (fn [call-node _env]
                    (swap! calls conj (:cap/id call-node))
                    (if (= :llm/voice-a (:cap/id call-node))
                      {:error {:type :eval/low-score}}
                      {:result {:type :value
                                :out {:text "ok"}}}))})]
      (is (:ok? run))
      (is (= [:llm/voice-a :llm/voice-a :llm/voice-b] @calls))
      (is (= 1 (get-in run [:telemetry :calls/total])))
      (is (= 1 (get-in run [:telemetry :calls/succeeded])))
      (is (= 0 (get-in run [:telemetry :calls/failed])))
      (is (= 1 (get-in run [:telemetry :calls/retries])))
      (is (= 1 (get-in run [:telemetry :calls/fallback-hops]))))))

(deftest execute-plan-emits-judge-pass-fail-telemetry
  (testing "Telemetry tracks judge usage and pass/fail split per evaluated call."
    (let [run (workflow/execute-plan
               {:plan {:nodes [{:op :call
                                :intent :text/respond
                                :dispatch {:candidates [:llm/voice-a :llm/voice-b]
                                           :retry {:fallback-max 1}
                                           :switch-on #{:eval/low-score}}
                                :done {:score-min 0.8}
                                :as :answer}
                               {:op :emit :input {:slot/id [:answer :out]}}]}
                :resolver {}
                :judge-fn (fn [call-node _env _result]
                            (if (= :llm/voice-a (:cap/id call-node))
                              {:score 0.2}
                              {:score 0.9}))
                :invoke-call (fn [call-node _env]
                               {:result {:type :value
                                         :out {:text (name (:cap/id call-node))}}})})]
      (is (:ok? run))
      (is (= {:text "voice-b"} (:emitted run)))
      (is (= 0 (get-in run [:telemetry :quality/must-failed])))
      (is (= 2 (get-in run [:telemetry :quality/judge-used])))
      (is (= 1 (get-in run [:telemetry :quality/judge-pass])))
      (is (= 1 (get-in run [:telemetry :quality/judge-fail]))))))

(deftest execute-plan-emits-must-failed-telemetry
  (testing "Telemetry tracks hard quality gate failures via :quality/must-failed."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice-a :llm/voice-b]
                                             :retry {:fallback-max 1}
                                             :switch-on #{:eval/must-failed}}
                                  :done {:must #{:tests-pass}
                                         :score-min 0.0}
                                  :as :answer}
                                 {:op :emit
                                  :input {:slot/id [:answer :out]}}]}
                  :resolver {}
                  :check-fns {:tests-pass (fn [call-node _env _result]
                                            (= :llm/voice-b (:cap/id call-node)))}
                  :invoke-call (fn [call-node _env]
                                 (swap! calls conj (:cap/id call-node))
                                 {:result {:type :value
                                           :out {:text (name (:cap/id call-node))}}})})]
      (is (:ok? run))
      (is (= [:llm/voice-a :llm/voice-b] @calls))
      (is (= {:text "voice-b"} (:emitted run)))
      (is (= 1 (get-in run [:telemetry :quality/must-failed]))))))

(deftest execute-plan-keeps-deterministic-candidate-order-across-base-policy-and-routing-fallback
  (testing "Candidate order is deterministic and deduplicated: base -> policy fallback -> routing fallback."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :as :answer}
                                 {:op :emit
                                  :input {:slot/id [:answer :out]}}]}
                  :resolver {:routing {:intent->cap {:text/respond :llm/voice-a}
                                       :fallback [:llm/voice-b :llm/voice-a :llm/voice-c]}
                             :protocol {:policy/default {:retry {:same-cap-max 0
                                                                 :fallback-max 3}
                                                         :switch-on #{:eval/low-score}
                                                         :fallback [:llm/voice-d]}
                                        :policy/intents {:text/respond {:done {:should #{:tests-pass}
                                                                               :score-min 1.0}
                                                                        :fallback [:llm/voice-b :llm/voice-c]}}
                                        :result/types [:value]}}
                  :check-fns {:tests-pass (fn [call-node _env _result]
                                            (= :llm/voice-c (:cap/id call-node)))}
                  :invoke-call (fn [call-node _env]
                                 (swap! calls conj (:cap/id call-node))
                                 {:result {:type :value
                                           :out {:text (name (:cap/id call-node))}}})})]
      (is (:ok? run))
      (is (= [:llm/voice-a :llm/voice-d :llm/voice-b :llm/voice-c] @calls))
      (is (= {:text "voice-c"} (:emitted run))))))

(deftest execute-plan-recovers-on-must-failed-only-when-switch-on-allows-it
  (testing "Failure class :eval/must-failed is recoverable only when explicitly listed in :switch-on."
    (let [calls (atom [])
          run   (workflow/execute-plan
                 {:plan {:nodes [{:op :call
                                  :intent :text/respond
                                  :dispatch {:candidates [:llm/voice-a :llm/voice-b]
                                             :retry {:fallback-max 1}
                                             :switch-on #{:eval/must-failed}}
                                  :done {:must #{:tests-pass}}
                                  :as :answer}
                                 {:op :emit
                                  :input {:slot/id [:answer :out]}}]}
                  :resolver {}
                  :check-fns {:tests-pass (fn [call-node _env _result]
                                            (= :llm/voice-b (:cap/id call-node)))}
                  :invoke-call (fn [call-node _env]
                                 (swap! calls conj (:cap/id call-node))
                                 {:result {:type :value
                                           :out {:text (name (:cap/id call-node))}}})})]
      (is (:ok? run))
      (is (= [:llm/voice-a :llm/voice-b] @calls))
      (is (= {:text "voice-b"} (:emitted run))))))

(deftest execute-plan-fails-tool-node-with-missing-effects-declaration
  (testing "Tool node without :effects/:allowed fails with canonical invalid-input error."
    (let [err (try
                (workflow/execute-plan
                 {:plan {:nodes [{:op :tool
                                  :tool/id :fs/write-file
                                  :input {:path "x.txt"}}]}
                  :resolver {}
                  :invoke-tool (fn [_ _]
                                 {:result {:type :value
                                           :out {:wrote? true}}})})
                nil
                (catch clojure.lang.ExceptionInfo e
                  (ex-data e)))]
      (is (= :effects/invalid-input (:error err)))
      (is (= :effects/invalid-input (:failure/type err)))
      (is (= :effects/not-declared (:reason err))))))

(deftest execute-plan-runs-tool-node-through-runtime-invoker
  (testing "Tool node executes through :invoke-tool handler and exposes normalized slot output."
    (let [called (atom nil)
          user   {:user/id 17
                  :user/account-type :admin
                  :user/roles #{:role/admin}}
          roles-cfg {:enabled? true
                     :authorize-default? false
                     :anonymous-role :role/anonymous
                     :logged-in-role :role/user
                     :account-type->roles {:admin #{:role/admin}}
                     :effects {:fs/write {:any #{:role/admin}}}}
          run (workflow/execute-plan
               {:plan {:nodes [{:op :tool
                                :tool/id :fs/write-file
                                :effects {:allowed #{:fs/write}}
                                :input {:path "x.txt"}
                                :as :tool-res}
                               {:op :emit
                                :input {:slot/id [:tool-res :out]}}]}
                :resolver {}
                :env {:auth/user user
                      :roles/config roles-cfg}
                :invoke-tool (fn [tool-node _env]
                               (reset! called tool-node)
                               {:result {:type :value
                                         :out {:path "x.txt"
                                               :wrote? true}}})})]
      (is (:ok? run))
      (is (= :fs/write-file (:tool/id @called)))
      (is (= 17 (get-in @called [:auth/user :user/id])))
      (is (= roles-cfg (:roles/config @called)))
      (is (= true (get-in @called [:auth/effects :ok?])))
      (is (= {:path "x.txt"
              :wrote? true}
             (:emitted run))))))

(deftest execute-plan-updates-gateway-model-health
  (testing "Successful and failed call attempts update per-model health registry."
    (let [health* (atom {})
          calls   (atom 0)
          resolver {:routing {:gateway {:strategy :latency-first
                                        :ema-alpha 0.5
                                        :circuit-breaker {:enabled? true
                                                          :min-samples 2
                                                          :error-rate-open 0.5
                                                          :cooldown-ms 2000}}}
                    :gateway/model-health health*
                    :caps/by-id {:llm/voice {:cap/id :llm/voice
                                             :cap/intents #{:text/respond}
                                             :cap/can-produce #{:value}
                                             :cap/effects-allowed #{:none}
                                             :dispatch/model-key :ferment.model/voice}
                                 :llm/solver-text {:cap/id :llm/solver-text
                                                   :cap/intents #{:text/respond}
                                                   :cap/can-produce #{:value}
                                                   :cap/effects-allowed #{:none}
                                                   :dispatch/model-key :ferment.model/solver}}}
          run (workflow/execute-plan
               {:plan {:nodes [{:op :call
                                :intent :text/respond
                                :dispatch {:candidates [:llm/voice :llm/solver-text]
                                           :retry {:fallback-max 1}
                                           :switch-on #{:eval/low-score}}
                                :done {:should #{:tests-pass}
                                       :score-min 1.0}
                                :as :answer}
                               {:op :emit
                                :input {:slot/id [:answer :out]}}]}
                :resolver resolver
                :invoke-call (fn [call-node _env]
                               (swap! calls inc)
                               {:invoke/meta {:model-key (if (= :llm/voice (:cap/id call-node))
                                                           :ferment.model/voice
                                                           :ferment.model/solver)}
                                :result {:type :value
                                         :out {:text (name (:cap/id call-node))}}})
                :check-fns {:tests-pass (fn [call-node _env _result]
                                          (= :llm/solver-text (:cap/id call-node)))} })]
      (is (:ok? run))
      (is (= 2 @calls))
      (is (pos? (get-in @health* [:ferment.model/voice :calls] 0)))
      (is (pos? (get-in @health* [:ferment.model/voice :errors] 0)))
      (is (pos? (get-in @health* [:ferment.model/solver :calls] 0)))
      (is (number? (get-in @health* [:ferment.model/solver :latency/ema-ms])))
      (is (number? (get-in @health* [:ferment.model/solver :quality/ema]))))))

(deftest execute-plan-hedging-selects-faster-successful-candidate
  (testing "Gateway hedging can probe candidates in parallel and pick the fastest successful result."
    (let [run (workflow/execute-plan
               {:plan {:nodes [{:op :call
                                :intent :text/respond
                                :dispatch {:candidates [:llm/voice :llm/solver-text]
                                           :retry {:same-cap-max 0
                                                   :fallback-max 1}
                                           :switch-on #{:eval/low-score}}
                                :as :answer}
                               {:op :emit
                                :input {:slot/id [:answer :out]}}]}
                :resolver {:routing {:gateway {:strategy :latency-first
                                               :hedging {:enabled? true
                                                         :intent->enabled? {:text/respond true}
                                                         :max-probes 2
                                                         :delay-ms 0}}
                                     :intent->cap {:text/respond :llm/voice}}
                           :caps/by-id {:llm/voice {:cap/id :llm/voice
                                                    :cap/intents #{:text/respond}
                                                    :cap/can-produce #{:value}
                                                    :cap/effects-allowed #{:none}
                                                    :dispatch/model-key :ferment.model/voice}
                                        :llm/solver-text {:cap/id :llm/solver-text
                                                          :cap/intents #{:text/respond}
                                                          :cap/can-produce #{:value}
                                                          :cap/effects-allowed #{:none}
                                                          :dispatch/model-key :ferment.model/solver}}}
                :invoke-call (fn [call-node _env]
                               (case (:cap/id call-node)
                                 :llm/voice
                                 (do
                                   (Thread/sleep 80)
                                   {:result {:type :value
                                             :out {:text "voice"}}})
                                 :llm/solver-text
                                 (do
                                   (Thread/sleep 5)
                                   {:result {:type :value
                                             :out {:text "solver"}}})
                                 {:error {:type :unsupported/capability}}))})]
      (is (:ok? run))
      (is (= {:text "solver"} (:emitted run))))))
