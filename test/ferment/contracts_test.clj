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
      (is (= [:value :plan :stream :error]
             (:result/types protocol))))))

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
  (testing "Walidacja requestu odrzuca nieobsługiwany intent i niepoprawne shape'y done/effects."
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
  (testing "Walidacja resultu respektuje :result/types z protokołu."
    (let [protocol {:result/types [:value :plan]}]
      (is (:ok? (contracts/validate-result protocol
                                           {:result {:type :value
                                                     :out {:text "ok"}}})))
      (is (false? (:ok? (contracts/validate-result protocol
                                                   {:result {:type :stream
                                                             :stream {:chunks []}}})))))))

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
