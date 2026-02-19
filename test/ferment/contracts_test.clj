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
      (is (contains? (:error/catalog protocol) :schema/invalid))
      (is (= :builtin/schema-valid
             (get-in protocol [:quality/checks :schema-valid])))
      (is (= :eval/grade
             (get-in protocol [:quality/judge :intent])))
      (is (= :llm/judge
             (get-in protocol [:quality/judge :cap/id])))
      (is (= [:value :plan :stream :error]
             (:result/types protocol))))))

(deftest dev-protocol-enables-quality-judge
  (testing "Dev profile can override protocol branch and enable judge."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/dev/protocol.edn")
          protocol (get cfg :ferment.protocol/default)]
      (is (= true (get-in protocol [:quality/judge :enabled?])))
      (is (= :llm/judge (get-in protocol [:quality/judge :cap/id]))))))

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
              :reason :effects/allowed-not-keywords}
             (contracts/validate-request protocol bad-effects))))))

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
