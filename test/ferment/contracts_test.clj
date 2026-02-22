(ns

    ^{:doc    "Contract tests for Ferment invoke protocol."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.contracts-test

  (:require [clojure.edn       :as edn]
            [clojure.test      :refer [deftest is testing]]
            [ferment.contracts :as contracts]))

(defn- read-edn-with-integrant-readers
  [path]
  (edn/read-string {:readers {'ref identity 'refset identity}}
                   (slurp path)))

(deftest protocol-config-has-envelope-and-intent-contracts
  (testing "Protocol config defines canonical envelope and intent-level contracts."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/protocol.edn")
          protocol (get cfg :ferment.protocol/default)]
      (is (= 1 (:proto/version protocol)))
      (is (= :application/edn (:transport/content-type protocol)))
      (is (= 3 (:retry/max-attempts protocol)))
      (is (= [:proto :trace :task :input]
             (get-in protocol [:envelope/request :required])))
      (is (= [:result :error]
             (get-in protocol [:envelope/response :required-one-of])))
      (is (contains? (:intents protocol) :code/patch))
      (is (contains? (:intents protocol) :text/respond))
      (is (= :req/text
             (get-in protocol [:intents :text/respond :in-schema])))
      (is (= :res/text
             (get-in protocol [:intents :text/respond :out-schema])))
      (is (= :route/solver->voice
             (get-in protocol [:intents :route/decide :result/contract :contract/kind])))
      (is (= :plan
             (get-in protocol [:intents :route/decide :result/contract :type])))
      (is (= #{:schema-valid}
             (get-in protocol [:policy/intents :route/decide :done :must])))
      (is (= [:schema-valid :no-hallucinated-apis]
             (get-in protocol [:policy/intents :text/respond :checks])))
      (is (contains? (:error/catalog protocol) :schema/invalid))
      (is (= :builtin/schema-valid
             (get-in protocol [:policy/checks :schema-valid])))
      (is (= :eval/grade
             (get-in protocol [:policy/default :judge :intent])))
      (is (= :llm/judge
             (get-in protocol [:policy/default :judge :cap/id])))
      (is (= [:value :plan :stream :error]
             (:result/types protocol))))))

(deftest dev-protocol-enables-quality-judge
  (testing "Dev profile can override protocol branch and enable judge."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/dev/protocol.edn")
          protocol (get cfg :ferment.protocol/default)]
      (is (= true (get-in protocol [:policy/default :judge :enabled?])))
      (is (= :llm/judge (get-in protocol [:policy/default :judge :cap/id]))))))

(deftest request-validation-works
  (testing "Valid request passes, malformed request fails."
    (let [ok-request {:proto 1
                      :trace {:id "trace-1" :turn 1}
                      :role :coder
                      :task {:intent :code/generate}
                      :input {:spec "..."}}
          bad-request {:proto 1
                       :trace {:id "trace-2"}
                       :task {:input {:spec "..."}}}]
      (is (:ok? (contracts/validate-request ok-request)))
      (is (false? (:ok? (contracts/validate-request bad-request)))))))

(deftest request-validation-is-protocol-aware
  (testing "Request validation rejects unsupported intent and invalid done/effects shapes."
    (let [protocol {:intents {:text/respond {:in-schema :req/text}}
                    :result/types [:value]}
          bad-intent {:proto 1
                      :trace {:id "trace-intent"}
                      :task {:intent :code/patch}
                      :input {:prompt "x"}}
          bad-done {:proto 1
                    :trace {:id "trace-done"}
                    :task {:intent :text/respond}
                    :input {:prompt "x"}
                    :done {:must ["schema-valid"]}}
          bad-done-score {:proto 1
                          :trace {:id "trace-done-score"}
                          :task {:intent :text/respond}
                          :input {:prompt "x"}
                          :done {:must #{:schema-valid}
                                 :score-min 1.2}}
          bad-effects {:proto 1
                       :trace {:id "trace-effects"}
                       :task {:intent :text/respond}
                       :input {:prompt "x"}
                       :effects {:allowed ["fs/write"]}}]
      (is (= {:ok? false
              :error :invalid-request
              :reason :intent/not-supported
              :intent :code/patch}
             (contracts/validate-request protocol bad-intent)))
      (is (= {:ok? false
              :error :invalid-request
              :reason :done/must-not-keywords}
             (contracts/validate-request protocol bad-done)))
      (is (= {:ok? false
              :error :invalid-request
              :reason :done/score-min-out-of-range}
             (contracts/validate-request protocol bad-done-score)))
      (is (= {:ok? false
              :error :invalid-request
              :reason :effects/allowed-not-keywords}
             (contracts/validate-request protocol bad-effects))))))

(deftest request-validation-enforces-in-schema
  (testing "Input payload is validated against intent in-schema validators."
    (let [protocol {:intents {:text/respond {:in-schema :req/text}}}
          bad-request {:proto 1
                       :trace {:id "trace-schema-in"}
                       :task {:intent :text/respond}
                       :input {:x 1}}
          ok-request {:proto 1
                      :trace {:id "trace-schema-in-ok"}
                      :task {:intent :text/respond}
                      :input {:prompt "hej"}}]
      (is (= :input/schema-invalid
             (:reason (contracts/validate-request protocol bad-request))))
      (is (:ok? (contracts/validate-request protocol ok-request))))))

(deftest invoke-with-contract-retries-until-valid-result
  (testing "Invoker retries invalid result and accepts first valid one."
    (let [request {:proto 1
                   :trace {:id "trace-3"}
                   :role :coder
                   :task {:intent :code/generate}
                   :input {:spec "..."}}
          calls   (atom 0)
          invoke  (fn [_request _attempt]
                    (let [n (swap! calls inc)]
                      (if (= n 1)
                        {:result {:type :value}}
                        {:result {:type :value
                                  :out {:patch "...diff..."}}})))
          result (contracts/invoke-with-contract invoke request {:max-attempts 3})]
      (is (:ok? result))
      (is (= 2 (:attempt result)))
      (is (= 2 @calls)))))

(deftest invoke-with-contract-fails-after-max-retries
  (testing "Invoker returns explicit error after exhausting retries."
    (let [request {:proto 1
                   :trace {:id "trace-4"}
                   :role :coder
                   :task {:intent :code/generate}
                   :input {:spec "..."}}
          invoke  (fn [_request _attempt]
                    {:result {:type :value}})
          result (contracts/invoke-with-contract invoke request {:max-attempts 2})]
      (is (false? (:ok? result)))
      (is (= :invalid-result-after-retries (:error result)))
      (is (= 2 (:attempts result))))))

(deftest result-envelope-validation-supports-canonical-result-or-error
  (testing "Canonical response envelope accepts exactly one of :result or :error."
    (is (:ok? (contracts/validate-result {:result {:type :value
                                                   :out {:text "ok"}}})))
    (is (:ok? (contracts/validate-result {:error {:type :schema/invalid}})))
    (is (false? (:ok? (contracts/validate-result {:result {} :error {}}))))))

(deftest result-envelope-requires-type-specific-payload
  (testing "Canonical `:result` validates type-specific required keys."
    (is (false? (:ok? (contracts/validate-result {:result {:type :value}}))))
    (is (:ok? (contracts/validate-result {:result {:type :value
                                                   :out {:text "ok"}}})))
    (is (:ok? (contracts/validate-result {:result {:type :plan
                                                   :plan {:steps []}}})))))

(deftest result-validation-is-protocol-aware
  (testing "Result validation respects :result/types from protocol."
    (let [protocol {:result/types [:value :plan]}]
      (is (:ok? (contracts/validate-result protocol
                                           {:result {:type :value
                                                     :out {:text "ok"}}})))
      (is (false? (:ok? (contracts/validate-result protocol
                                                   {:result {:type :stream
                                                             :stream {:chunks []}}})))))))

(deftest eval-grade-result-contract-is-enforced
  (testing "Intent :eval/grade requires canonical value result with :out/:score in range 0..1."
    (let [protocol {:quality/judge {:intent :eval/grade}
                    :result/types [:value :plan :error]}
          ok-result {:result {:type :value
                              :out {:score 0.75
                                    :violations []
                                    :retry? false}}}
          missing-score {:result {:type :value
                                  :out {:text "no score"}}}
          out-of-range {:result {:type :value
                                 :out {:score 1.5}}}
          wrong-type {:result {:type :plan
                               :plan {:nodes []}}}
          error-result {:error {:type :schema/invalid}}]
      (is (:ok? (contracts/validate-result protocol :eval/grade ok-result)))
      (is (= :eval/grade-score-missing
             (:reason (contracts/validate-result protocol :eval/grade missing-score))))
      (is (= :eval/grade-score-out-of-range
             (:reason (contracts/validate-result protocol :eval/grade out-of-range))))
      (is (= :eval/grade-result-type-not-value
             (:reason (contracts/validate-result protocol :eval/grade wrong-type))))
      (is (= :eval/grade-error-not-allowed
             (:reason (contracts/validate-result protocol :eval/grade error-result))))
      (is (:ok? (contracts/validate-result protocol :text/respond
                                           {:result {:type :value
                                                     :out {:text "ok"}}}))))))

(deftest result-validation-enforces-out-schema
  (testing "Value responses are validated against intent out-schema validators."
    (let [protocol {:intents {:text/respond {:out-schema :res/text}}
                    :result/types [:value :error]}]
      (is (= :output/schema-invalid
             (:reason (contracts/validate-result protocol
                                                 :text/respond
                                                 {:result {:type :value
                                                           :out {:x 1}}}))))
      (is (:ok? (contracts/validate-result protocol
                                           :text/respond
                                           {:result {:type :value
                                                     :out {:text "ok"}}}))))))

(deftest requires-are-used-as-hard-result-contract
  (testing "Request-level :task/:requires enforces expected result type and schema."
    (let [protocol {:intents {:text/respond {:in-schema :req/text
                                             :out-schema :res/text}}
                    :result/types [:value :plan :error]}
          request {:proto 1
                   :trace {:id "trace-requires"}
                   :task {:intent :text/respond
                          :requires {:result/type :value
                                     :out-schema :res/text}}
                   :input {:prompt "hej"}}
          calls (atom 0)
          invoke (fn [_request _attempt]
                   (let [n (swap! calls inc)]
                     (if (= n 1)
                       {:result {:type :plan
                                 :plan {:nodes []}}}
                       {:result {:type :value
                                 :out {:text "ok"}}})))]
      (let [result (contracts/invoke-with-contract invoke request {:max-attempts 2
                                                                   :protocol protocol})]
        (is (:ok? result))
        (is (= 2 (:attempt result)))
        (is (= 2 @calls))))))

(deftest route-decide-result-contract-is-enforced
  (testing "Intent :route/decide requires canonical route decision payload with keyword target and valid dispatch policy."
    (let [protocol {:intents {:route/decide {:result/contract {:type :value
                                                               :contract/kind :route/decide}}}
                    :result/types [:value :error]}
          ok-flat {:result {:type :value
                            :out {:cap/id :llm/solver
                                  :dispatch {:checks [:schema-valid]
                                             :retry {:same-cap-max 1
                                                     :fallback-max 1}}}}}
          ok-nested {:result {:type :value
                              :out {:cap/id :llm/voice
                                    :dispatch {:candidates [:llm/voice :llm/solver]
                                               :switch-on #{:schema/invalid}}}}}
          missing-target {:result {:type :value
                                   :out {:dispatch {:checks [:schema-valid]}}}}
          bad-cap-type {:result {:type :value
                                 :out {:cap/id "llm/solver"}}}
          bad-candidates {:result {:type :value
                                   :out {:cap/id :llm/solver
                                         :dispatch {:candidates ["llm/voice"]}}}}
          bad-retry {:result {:type :value
                              :out {:cap/id :llm/solver
                                    :dispatch {:retry {:same-cap-max -1}}}}}
          bad-alias {:result {:type :value
                              :out {:cap-id :llm/solver}}}
          error-result {:error {:type :runtime/invoke-failed}}]
      (is (:ok? (contracts/validate-result protocol :route/decide ok-flat)))
      (is (:ok? (contracts/validate-result protocol :route/decide ok-nested)))
      (is (= :route/decide-target-missing
             (:reason (contracts/validate-result protocol :route/decide missing-target))))
      (is (= :route/decide-cap-not-keyword
             (:reason (contracts/validate-result protocol :route/decide bad-cap-type))))
      (is (= :route/decide-candidates-not-keywords
             (:reason (contracts/validate-result protocol :route/decide bad-candidates))))
      (is (= :route/decide-retry-same-cap-max-not-nonneg-int
             (:reason (contracts/validate-result protocol :route/decide bad-retry))))
      (is (= :route/decide-unknown-keys
             (:reason (contracts/validate-result protocol :route/decide bad-alias))))
      (is (= :route/decide-error-not-allowed
             (:reason (contracts/validate-result protocol :route/decide error-result)))))))

(deftest route-solver-voice-plan-contract-is-enforced
  (testing "Intent :route/decide can require canonical solver->voice plan shape."
    (let [protocol {:intents {:route/decide {:result/contract {:type :plan
                                                               :contract/kind :route/solver->voice}}}
                    :result/types [:value :plan :error]}
          ok-plan {:result {:type :plan
                            :plan {:nodes [{:op :call
                                            :intent :problem/solve
                                            :as :solver
                                            :input {:prompt "Q"}}
                                           {:op :call
                                            :intent :text/respond
                                            :as :voice
                                            :input {:prompt {:slot/id [:solver :out :text]}}}
                                           {:op :emit
                                            :input {:slot/id [:voice :out]}}]}}}
          bad-plan {:result {:type :plan
                             :plan {:nodes [{:op :call
                                             :intent :problem/solve
                                             :as :solver
                                             :input {:prompt "Q"}}
                                            {:op :call
                                             :intent :problem/solve
                                             :as :voice
                                             :input {:prompt {:slot/id [:solver :out :text]}}}
                                            {:op :emit
                                             :input {:slot/id [:voice :out]}}]}}}
          bad-type {:result {:type :value
                             :out {:cap/id :llm/solver}}}]
      (is (:ok? (contracts/validate-result protocol :route/decide ok-plan)))
      (is (= :route/plan-second-intent-not-text-respond
             (:reason (contracts/validate-result protocol :route/decide bad-plan))))
      (is (= :route/plan-result-type-not-plan
             (:reason (contracts/validate-result protocol :route/decide bad-type)))))))

(deftest invoke-with-contract-enforces-route-decide-shape
  (testing "invoke-with-contract retries :route/decide until decision payload is valid."
    (let [request {:proto 1
                   :trace {:id "trace-route"}
                   :task {:intent :route/decide}
                   :input {:prompt "route this"}}
          protocol {:intents {:route/decide {:result/contract {:type :value
                                                               :contract/kind :route/decide}}}
                    :result/types [:value :error]}
          calls (atom 0)
          invoke (fn [_request _attempt]
                   (let [n (swap! calls inc)]
                     (if (= n 1)
                       {:result {:type :value
                                 :out {:text "invalid"}}}
                       {:result {:type :value
                                 :out {:cap/id :llm/solver}}})))]
      (let [result (contracts/invoke-with-contract invoke request
                                                   {:max-attempts 3
                                                    :protocol protocol})]
        (is (:ok? result))
        (is (= 2 (:attempt result)))
        (is (= 2 @calls))))))

(deftest invoke-with-contract-enforces-eval-grade-shape
  (testing "invoke-with-contract retries :eval/grade when result does not satisfy score contract."
    (let [request {:proto 1
                   :trace {:id "trace-eval"}
                   :task {:intent :eval/grade}
                   :input {:prompt "grade this"}}
          protocol {:quality/judge {:intent :eval/grade}
                    :result/types [:value :error]}
          calls (atom 0)
          invoke (fn [_request _attempt]
                   (let [n (swap! calls inc)]
                     (if (= n 1)
                       {:result {:type :value
                                 :out {:text "{\"score\":0.4}"}}}
                       {:result {:type :value
                                 :out {:score 0.4}}})))]
      (let [result (contracts/invoke-with-contract invoke request
                                                   {:max-attempts 3
                                                    :protocol protocol})]
        (is (:ok? result))
        (is (= 2 (:attempt result)))
        (is (= 2 @calls))))))

(deftest plan-materialization-injects-slot-bindings
  (testing "Plan placeholders can be injected from model-provided bindings (HOF-like output)."
    (let [result {:result {:type :plan
                           :plan {:steps [{:op :call
                                           :intent :text/respond
                                           :input {:prompt {:slot/id :summary}}}]}
                           :bindings {:summary "Krótkie podsumowanie"}}}
          materialized (contracts/materialize-plan-result result)]
      (is (= {:steps [{:op :call
                       :intent :text/respond
                       :input {:prompt "Krótkie podsumowanie"}}]}
             materialized)))))

(deftest plan-materialization-supports-slot-paths
  (testing "Slot placeholders may reference nested value by path."
    (let [plan {:nodes [{:op :emit
                         :input {:slot/id [:summary :out :text]}}]}
          bindings {:summary {:out {:text "gotowe"}}}]
      (is (= {:nodes [{:op :emit
                       :input "gotowe"}]}
             (contracts/materialize-plan plan bindings))))))
