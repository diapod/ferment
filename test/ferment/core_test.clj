(ns

    ^{:doc    "Core runtime contract integration tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.core-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.contracts :as contracts]
            [ferment.core :as core]))

(deftest solver-retries-when-model-response-is-empty
  (testing "solver! retries when first model response cannot produce valid :value result."
    (let [calls (atom 0)]
      (with-redefs [core/ollama-generate!
                    (fn [_]
                      (let [n (swap! calls inc)]
                        (if (= n 1)
                          {:response ""}
                          {:response "{\"intent\":\"ok\"}"})))]
        (is (= "{\"intent\":\"ok\"}" (core/solver! "napisz patch")))
        (is (= 2 @calls))))))

(deftest voice-fails-after-max-retries-when-model-keeps-returning-empty
  (testing "voice! fails with ex-info when all retries return invalid content."
    (let [calls (atom 0)]
      (with-redefs [core/ollama-generate!
                    (fn [_]
                      (swap! calls inc)
                      {:response ""})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"LLM invocation failed after retries"
                              (core/voice! "{\"intent\":\"ok\"}" "hej")))
        (is (= 3 @calls))))))

(deftest solver-can-read-model-selection-from-runtime-config
  (testing "solver! accepts runtime config map and uses :ferment.model/solver."
    (let [called-with (atom nil)
          cfg {:ferment.model/solver "solver-selected"}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (is (= "{\"intent\":\"ok\"}" (core/solver! "napisz patch" {:config cfg})))
        (is (= "solver-selected" (:model @called-with)))))))

(deftest solver-prefers-ferment-model-key-when-present
  (testing "solver! falls back to :ferment.model/coding when :ferment.model/solver is missing."
    (let [called-with (atom nil)
          cfg {:ferment.model/coding "coding-from-model-branch"}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (is (= "{\"intent\":\"ok\"}" (core/solver! "napisz patch" {:config cfg})))
        (is (= "coding-from-model-branch" (:model @called-with)))))))

(deftest invoke-capability-can-return-plan-with-injected-slots
  (testing "invoke-capability! supports plan results with model-provided bindings (HOF-like plan output)."
    (with-redefs [core/ollama-generate! (fn [_] {:response "ignored"})]
      (let [result (core/invoke-capability!
                    nil
                    {:role :router
                     :intent :route/decide
                     :cap-id :llm/meta
                     :model "mock/meta"
                     :prompt "zbuduj plan"
                     :max-attempts 1
                     :result-parser
                     (fn [_ {:keys [request]}]
                       {:trace  (:trace request)
                        :result {:type :plan
                                 :plan {:steps [{:op :call
                                                 :intent :text/respond
                                                 :input {:prompt {:slot/id :summary}}}]}
                                 :bindings {:summary "Plan gotowy"}}})})
            plan (contracts/materialize-plan-result result)]
        (is (= :plan (contracts/result-type-of result)))
        (is (= {:steps [{:op :call
                         :intent :text/respond
                         :input {:prompt "Plan gotowy"}}]}
               plan))))))

(deftest execute-capability-evaluates-plan-in-runtime
  (testing "execute-capability! evaluates returned plan and normalizes final value output."
    (with-redefs [core/ollama-generate!
                  (fn [{:keys [model prompt]}]
                    (if (= "mock/meta" model)
                      {:response "PLAN"}
                      {:response (str "VOICE:" prompt)}))]
      (let [result (core/execute-capability!
                    nil
                    {:routing {:intent->cap {:text/respond :llm/voice}}}
                    {:role :router
                     :intent :route/decide
                     :cap-id :llm/meta
                     :model "mock/meta"
                     :prompt "zbuduj plan"
                     :max-attempts 1
                     :result-parser
                     (fn [_ {:keys [request]}]
                       {:trace (:trace request)
                        :result {:type :plan
                                 :plan {:nodes [{:op :call
                                                 :intent :text/respond
                                                 :input {:prompt {:slot/id :summary}}
                                                 :as :answer}
                                                {:op :emit
                                                 :input {:slot/id [:answer :out]}}]}
                                 :bindings {:summary "hej"}}})})]
        (is (= :value (contracts/result-type-of result)))
        (is (= "VOICE:hej" (get-in result [:result :out :text])))))))
