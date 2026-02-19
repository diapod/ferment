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
      (is (= :llm/voice (get-in run [:env :answer :cap/id]))))))

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
      (is (= [:route/decide :text/respond] @calls)))))

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
