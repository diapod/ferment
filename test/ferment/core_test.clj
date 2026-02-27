(ns

    ^{:doc    "Core runtime contract integration tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.core-test

  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ferment.contracts :as contracts]
            [ferment.core :as core]
            [ferment.effects :as effects]
            [ferment.model :as model]
            [ferment.session :as session]
            [ferment.session.store :as session-store]
            [ferment.workflow :as workflow]))

(deftest solver-retries-when-model-response-is-empty
  (testing "solver! retries when first model response cannot produce valid :value result."
    (let [calls (atom 0)]
      (with-redefs [core/ollama-generate!
                    (fn [_]
                      (let [n (swap! calls inc)]
                        (if (= n 1)
                          {:response ""}
                          {:response "{\"intent\":\"ok\"}"})))]
        (is (= "{\"intent\":\"ok\"}" (core/solver! "rozwiąż problem")))
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

(deftest invoke-capability-rejects-tool-call-output-outside-route-decide
  (testing "tool_call style output is rejected for non-route intents and retried under contract."
    (let [calls (atom 0)]
      (with-redefs [core/ollama-generate!
                    (fn [_]
                      (swap! calls inc)
                      {:response "<tool_call>{\"name\":\"solve_question\",\"arguments\":{\"question\":\"X\"}}</tool_call>"})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"LLM invocation failed after retries"
                              (core/invoke-capability!
                               nil
                               {:role :voice
                                :intent :text/respond
                                :cap-id :llm/voice
                                :model "voice-model"
                                :input {:prompt "hej"}
                                :max-attempts 2})))
        (is (= 2 @calls))))))

(deftest solver-can-read-model-selection-from-runtime-config
  (testing "solver! accepts runtime config map and uses :ferment.model/solver."
    (let [called-with (atom nil)
          runtime {:models {:ferment.model/solver {:id "solver-selected"}}}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (is (= "{\"intent\":\"ok\"}" (core/solver! "rozwiąż problem" runtime)))
        (is (= "solver-selected" (:model @called-with)))))))

(deftest solver-does-not-fallback-to-coding-model
  (testing "solver! does not use coding model when :ferment.model/solver is missing."
    (let [called-with (atom nil)
          runtime {:models {:ferment.model/coding {:id "coding-from-model-branch"}}}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (is (= "{\"intent\":\"ok\"}" (core/solver! "rozwiąż problem" runtime)))
        (is (= "mlx-community/Qwen2.5-7B-Instruct-4bit" (:model @called-with)))))))

(deftest coder-uses-coding-model-selection
  (testing "coder! uses :ferment.model/coding selector for code capability."
    (let [called-with (atom nil)
          runtime {:models {:ferment.model/coding {:id "coding-selected"}}}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (is (= "{\"intent\":\"ok\"}" (core/coder! "napisz patch" runtime)))
        (is (= "coding-selected" (:model @called-with)))))))

(deftest model-generate-uses-runtime-command-config
  (testing "Generator uses runtime model command and injects prompt as configured."
    (let [called (atom nil)
          runtime {:models {:ferment.model/coding
                            {:runtime {:command ["mlx_lm.chat" "--model" "coding-x"]
                                       :prompt-via :arg
                                       :prompt-arg "--prompt"
                                       :env {"HF_HOME" "/tmp/hf"}}}}}]
      (with-redefs [core/sh!
                    (fn [command & opts]
                      (reset! called {:command command :opts opts})
                      {:exit 0 :out "OK" :err ""})]
        (is (= "OK"
               (:response (core/ollama-generate!
                           {:runtime runtime
                            :cap-id :llm/code
                            :intent :code/patch
                            :model "ignored"
                            :system "sys"
                            :prompt "hello"
                            :mode :live}))))
        (is (= ["mlx_lm.chat" "--model" "coding-x"
                "--prompt" "SYSTEM:\nsys\n\nUSER:\nhello\n\nASSISTANT:\n"]
               (:command @called)))
        (is (= {"HF_HOME" "/tmp/hf"}
               (get (apply hash-map (:opts @called)) :env)))))))

(deftest model-generate-prefers-runtime-invoke
  (testing "Generator prefers invoke on running runtime worker (process reuse)."
    (let [called (atom nil)]
      (with-redefs [model/invoke-model!
                    (fn [runtime model-k payload opts]
                      (reset! called {:runtime runtime
                                      :model-k model-k
                                      :payload payload
                                      :opts opts})
                      {:ok? true
                       :result {:text "RUNTIME-OK"}})
                    core/sh!
                    (fn [& _]
                      (throw (ex-info "one-shot path should not be called"
                                      {:error :unexpected-one-shot-path})))]
        (is (= "RUNTIME-OK"
               (:response (core/ollama-generate!
                           {:runtime {:models {}}
                            :cap-id :llm/solver
                            :intent :problem/solve
                            :model "ignored"
                            :prompt "hej"
                            :system "sys"
                            :max-tokens 420
                            :top-p 0.77
                            :session-id "s-42"
                            :mode :live}))))
        (is (= :ferment.model/solver (:model-k @called)))
        (is (= 420 (get-in @called [:payload :max-tokens])))
        (is (= 0.77 (get-in @called [:payload :top-p])))
        (is (= "s-42" (get-in @called [:opts :session/id])))))))

(deftest invoke-capability-forwards-budget-generation-overrides
  (testing "invoke-capability! forwards budget :max-tokens and :top-p into runtime invoke payload."
    (let [seen (atom nil)]
      (with-redefs [core/ollama-generate!
                    (fn [opts]
                      (reset! seen opts)
                      {:response "OK"})]
        (let [result (core/invoke-capability!
                      nil
                      {:role :voice
                       :intent :text/respond
                       :cap-id :llm/voice
                       :input {:prompt "hej"}
                       :budget {:max-tokens 321
                                :top-p 0.91}
                       :max-attempts 1})]
          (is (= :value (contracts/result-type-of result)))
          (is (= "OK" (get-in result [:result :out :text])))
          (is (= 321 (:max-tokens @seen)))
          (is (= 0.91 (:top-p @seen))))))))

(deftest model-generate-uses-capability-dispatch-model-key-from-resolver
  (testing "Generator resolves model runtime key from resolver capability metadata."
    (let [called (atom nil)
          runtime {:resolver {:caps/by-id {:llm/custom
                                           {:cap/id :llm/custom
                                            :dispatch/model-key :ferment.model/meta}}}
                   :models {:ferment.model/meta {:id "meta-selected"}}}]
      (with-redefs [model/invoke-model!
                    (fn [_runtime model-k _payload _opts]
                      (reset! called model-k)
                      {:ok? true
                       :result {:text "RUNTIME-OK"}})
                    core/sh!
                    (fn [& _]
                      (throw (ex-info "one-shot path should not be called"
                                      {:error :unexpected-one-shot-path})))]
        (is (= "RUNTIME-OK"
               (:response (core/ollama-generate!
                           {:runtime runtime
                            :cap-id :llm/custom
                            :intent :problem/solve
                            :model "ignored"
                            :prompt "hej"
                            :mode :live}))))
        (is (= :ferment.model/meta @called))))))

(deftest call-capability-plan-call-uses-dispatch-role-and-model-from-resolver
  (testing "Plan call takes role/model from capability dispatch metadata in resolver."
    (let [nested-call (atom nil)
          runtime {:resolver {:caps/by-id
                              {:llm/custom {:cap/id :llm/custom
                                            :dispatch/role :voice
                                            :dispatch/model-key :ferment.model/meta}}
                              :routing {:intent->cap {:custom/intent :llm/custom}}}
                   :models {:ferment.model/meta {:id "meta-selected"}}}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (case (:intent opts)
                        :route/decide
                        {:result {:type :plan
                                  :plan {:nodes [{:op :call
                                                  :intent :custom/intent
                                                  :as :answer}
                                                 {:op :emit
                                                  :input {:slot/id [:answer :out]}}]}}}

                        :custom/intent
                        (do
                          (reset! nested-call opts)
                          {:result {:type :value
                                    :out {:text "ok"}}})

                        {:result {:type :value
                                  :out {:text "noop"}}}))]
        (let [result (core/call-capability
                      runtime
                      (:resolver runtime)
                      {:role :router
                       :intent :route/decide
                       :cap-id :llm/meta
                       :input {:prompt "plan"}})]
          (is (= :value (contracts/result-type-of result)))
          (is (= :llm/custom (:cap-id @nested-call)))
          (is (= :voice (:role @nested-call)))
          (is (= "meta-selected" (:model @nested-call))))))))

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

(deftest invoke-capability-strips-think-blocks-from-model-output
  (testing "invoke-capability! removes `<think>...</think>` content before returning text."
    (with-redefs [core/ollama-generate!
                  (fn [_]
                    {:response "<think>internal reasoning</think>\n\nFerment is a multi-model orchestrator."})]
      (let [result (core/invoke-capability!
                    nil
                    {:role :voice
                     :intent :text/respond
                     :cap-id :llm/voice
                     :model "voice-model"
                     :prompt "what is Ferment?"
                     :max-attempts 1})
            text (get-in result [:result :out :text])]
        (is (string? text))
        (is (not (str/includes? text "<think")))
        (is (= "Ferment is a multi-model orchestrator." text))))))

(deftest invoke-capability-falls-back-to-think-body-for-non-voice-intents
  (testing "For non-user-facing intents, parser keeps non-empty text when output is think-only."
    (with-redefs [core/ollama-generate!
                  (fn [_]
                    {:response "<think>ACID to atomowość, spójność, izolacja, trwałość.</think>"})]
      (let [result (core/invoke-capability!
                    nil
                    {:role :solver
                     :intent :problem/solve
                     :cap-id :llm/solver
                     :model "solver-model"
                     :prompt "co to ACID?"
                     :max-attempts 1})
            text (get-in result [:result :out :text])]
        (is (string? text))
        (is (not (str/blank? text)))
        (is (str/includes? text "ACID"))))))

(deftest invoke-capability-non-voice-does-not-leak-unclosed-think
  (testing "For non-user-facing intents, unclosed think-only output falls back to prompt instead of leaking think content."
    (with-redefs [core/ollama-generate!
                  (fn [_]
                    {:response "<think>internal reasoning without closing tag"})]
      (let [result (core/invoke-capability!
                    nil
                    {:role :solver
                     :intent :problem/solve
                     :cap-id :llm/solver
                     :model "solver-model"
                     :input {:prompt "Pytanie testowe"}
                     :max-attempts 1})
            text (get-in result [:result :out :text])]
        (is (= "Pytanie testowe" text))
        (is (not (str/includes? text "<think")))))))

(deftest invoke-capability-voice-falls-back-to-request-prompt-when-think-only
  (testing "For text/respond, think-only output falls back to request prompt instead of empty text."
    (with-redefs [core/ollama-generate!
                  (fn [_]
                    {:response "<think>internal notes only</think>"})]
      (let [result (core/invoke-capability!
                    nil
                    {:role :voice
                     :intent :text/respond
                     :cap-id :llm/voice
                     :model "voice-model"
                     :input {:prompt "ACID to atomowość, spójność, izolacja, trwałość."}
                     :max-attempts 1})
            text (get-in result [:result :out :text])]
        (is (= "ACID to atomowość, spójność, izolacja, trwałość." text))
        (is (not (str/includes? text "<think")))))))

(deftest invoke-capability-text-respond-enforces-max-chars
  (testing "text/respond output is truncated to :constraints/:max-chars."
    (with-redefs [core/ollama-generate!
                  (fn [_]
                    {:response "abcdefghijklmnop"})]
      (let [result (core/invoke-capability!
                    nil
                    {:role :voice
                     :intent :text/respond
                     :cap-id :llm/voice
                     :model "voice-model"
                     :input {:prompt "hej"}
                     :constraints {:max-chars 10}
                     :max-attempts 1})
            text (get-in result [:result :out :text])]
        (is (= "abcdefghij" text))
        (is (= 10 (count text)))))))

(deftest invoke-capability-uses-intent-system-prompt-from-protocol
  (testing "invoke-capability! injects intent-level system prompt when request does not provide one."
    (let [seen (atom nil)
          runtime {:protocol {:intents {:text/respond {:in-schema :req/text
                                                       :out-schema :res/text
                                                       :system "SYS/VOICE"}}
                              :result/types [:value]}}]
      (with-redefs [core/ollama-generate!
                    (fn [params]
                      (reset! seen params)
                      {:response "ok"})]
        (let [result (core/invoke-capability!
                      runtime
                      {:role :voice
                       :intent :text/respond
                       :cap-id :llm/voice
                       :model "voice-model"
                       :prompt "hej"
                       :max-attempts 1})]
          (is (= :value (contracts/result-type-of result)))
          (is (= "SYS/VOICE" (:system @seen))))))))

(deftest invoke-capability-composes-system-prompt-from-role-packages
  (testing "When intent has no explicit system prompt, runtime composes prompt from default+role+intent packages."
    (let [seen (atom nil)
          runtime {:protocol {:prompts {:default "SYS/DEFAULT"
                                        :roles {:voice "SYS/ROLE/VOICE"}
                                        :intents {:text/respond "SYS/INTENT/TEXT"}}
                              :intents {:text/respond {:in-schema :req/text
                                                       :out-schema :res/text}}
                              :result/types [:value]}}]
      (with-redefs [core/ollama-generate!
                    (fn [params]
                      (reset! seen params)
                      {:response "ok"})]
        (let [result (core/invoke-capability!
                      runtime
                      {:role :voice
                       :intent :text/respond
                       :cap-id :llm/voice
                       :model "voice-model"
                       :prompt "hej"
                       :max-attempts 1})]
          (is (= :value (contracts/result-type-of result)))
          (is (= "SYS/DEFAULT\n\nSYS/ROLE/VOICE\n\nSYS/INTENT/TEXT"
                 (:system @seen))))))))

(deftest invoke-capability-composes-system-prompt-from-sequential-fragments
  (testing "Prompt composition accepts vectors/lists for default/role/intent/addendum fragments."
    (let [seen (atom nil)
          runtime {:protocol {:prompts {:default ["SYS/DEFAULT" "A"]
                                        :roles {:voice '("SYS/ROLE" "VOICE")}
                                        :intents {:text/respond ["SYS/INTENT" "TEXT"]}}
                              :intents {:text/respond {:in-schema :req/text
                                                       :out-schema :res/text
                                                       :system/addendum '("SYS/ADD" "END")}}
                              :result/types [:value]}}]
      (with-redefs [core/ollama-generate!
                    (fn [params]
                      (reset! seen params)
                      {:response "ok"})]
        (let [result (core/invoke-capability!
                      runtime
                      {:role :voice
                       :intent :text/respond
                       :cap-id :llm/voice
                       :model "voice-model"
                       :prompt "hej"
                       :max-attempts 1})]
          (is (= :value (contracts/result-type-of result)))
          (is (= "SYS/DEFAULT A\n\nSYS/ROLE VOICE\n\nSYS/INTENT TEXT\n\nSYS/ADD END"
                 (:system @seen))))))))

(deftest invoke-capability-intent-system-prompt-overrides-packages
  (testing "Explicit :system in intent config works as full override."
    (let [seen (atom nil)
          runtime {:protocol {:prompts {:default "SYS/DEFAULT"
                                        :roles {:voice "SYS/ROLE/VOICE"}
                                        :intents {:text/respond "SYS/INTENT/TEXT"}}
                              :intents {:text/respond {:in-schema :req/text
                                                       :out-schema :res/text
                                                       :system "SYS/OVERRIDE"}}
                              :result/types [:value]}}]
      (with-redefs [core/ollama-generate!
                    (fn [params]
                      (reset! seen params)
                      {:response "ok"})]
        (let [result (core/invoke-capability!
                      runtime
                      {:role :voice
                       :intent :text/respond
                       :cap-id :llm/voice
                       :model "voice-model"
                       :prompt "hej"
                       :max-attempts 1})]
          (is (= :value (contracts/result-type-of result)))
          (is (= "SYS/OVERRIDE" (:system @seen))))))))

(deftest call-capability-evaluates-plan-in-runtime
  (testing "call-capability! evaluates returned plan and normalizes final value output."
    (with-redefs [core/ollama-generate!
                  (fn [{:keys [model prompt]}]
                    (if (= "mock/meta" model)
                      {:response "PLAN"}
                      {:response (str "VOICE:" prompt)}))]
        (let [result (core/call-capability
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
        (is (= "VOICE:hej" (get-in result [:result :out :text])))
        (is (= true (get-in result [:result :plan/run :ok?])))
        (is (map? (get-in result [:result :plan/run :telemetry])))
        (is (nil? (get-in result [:result :plan/run :env])))
        (is (nil? (get-in result [:result :plan/run :transcript])))
        (is (vector? (get-in result [:result :plan/run :participants])))))))

(deftest call-capability-can-expose-plan-debug-when-enabled
  (testing "call-capability! may include pre-execution plan with slot refs when :debug/plan? is true."
    (with-redefs [core/invoke-capability!
                  (fn [_runtime _opts]
                    {:result {:type :plan
                              :plan {:nodes [{:op :let
                                              :as :solver
                                              :value {:out {:text "ACID"}}}
                                             {:op :emit
                                              :input {:slot/id [:solver :out :text]}}]}}})]
      (let [result (core/call-capability
                    nil
                    {}
                    {:role :router
                     :intent :route/decide
                     :cap-id :llm/meta
                     :debug/plan? true})]
        (is (= :value (contracts/result-type-of result)))
        (is (= {:slot/id [:solver :out :text]}
               (get-in result [:result :plan/debug :nodes 1 :input])))
        (is (= "ACID"
               (get-in result [:result :out :text])))
        (is (map? (get-in result [:result :plan/run :telemetry])))))))

(deftest call-capability-can-expose-call-transcript-when-enabled
  (testing "call-capability! may include per-call transcript with materialized inputs/outputs when :debug/transcript? is true."
    (with-redefs [core/invoke-capability!
                  (fn [_runtime {:keys [cap-id input]}]
                    (case cap-id
                      :llm/meta
                      {:result {:type :plan
                                :plan {:nodes [{:op :call
                                                :intent :problem/solve
                                                :input {:prompt "Wyjasnij ACID."}
                                                :as :solver}
                                               {:op :call
                                                :intent :text/respond
                                                :input {:prompt {:slot/id [:solver :out :text]}}
                                                :as :voice}
                                               {:op :emit
                                                :input {:slot/id [:voice :out]}}]}}}

                      :llm/solver
                      {:result {:type :value
                                :out {:text "ACID to atomowosc, spojnosc, izolacja i trwalosc."}}}

                      :llm/voice
                      {:result {:type :value
                                :out {:text (str "VOICE:" (:prompt input))}}}

                      {:result {:type :error
                                :error {:type :unsupported/capability}}}))]
      (let [result (core/call-capability
                    {}
                    {:routing {:intent->cap {:problem/solve :llm/solver
                                             :text/respond :llm/voice}}}
                    {:role :router
                     :intent :route/decide
                     :cap-id :llm/meta
                     :debug/transcript? true})
            transcript (get-in result [:result :plan/run :transcript])]
        (is (= :value (contracts/result-type-of result)))
        (is (= "VOICE:ACID to atomowosc, spojnosc, izolacja i trwalosc."
               (get-in result [:result :out :text])))
        (is (= 2 (count transcript)))
        (is (number? (get-in transcript [0 :latency-ms])))
        (is (number? (get-in transcript [1 :latency-ms])))
        (is (= :problem/solve (get-in transcript [0 :intent])))
        (is (= "Wyjasnij ACID." (get-in transcript [0 :input :prompt])))
        (is (= "ACID to atomowosc, spojnosc, izolacja i trwalosc."
               (get-in transcript [0 :out :text])))
        (is (= :text/respond (get-in transcript [1 :intent])))
        (is (= "ACID to atomowosc, spojnosc, izolacja i trwalosc."
               (get-in transcript [1 :input :prompt])))
        (is (= "VOICE:ACID to atomowosc, spojnosc, izolacja i trwalosc."
               (get-in transcript [1 :out :text])))
        (is (= 2 (count (get-in result [:result :plan/run :timings]))))
        (is (= :problem/solve
               (get-in result [:result :plan/run :timings 0 :intent])))
        (is (= :text/respond
               (get-in result [:result :plan/run :timings 1 :intent])))))))

(deftest call-capability-runs-tool-node-with-runtime-effects
  (testing "call-capability! runs :tool plan nodes through scoped runtime effect handler."
    (let [root (str (java.nio.file.Files/createTempDirectory
                     "ferment-core-tool"
                     (make-array java.nio.file.attribute.FileAttribute 0)))
          runtime {:effects {:fs/write {:enabled? true
                                        :root root
                                        :allow ["sandbox/"]}}}
          plan {:nodes [{:op :tool
                         :tool/id :fs/write-file
                         :effects {:allowed #{:fs/write}}
                         :input {:path "sandbox/tool.txt"
                                 :content "tool-ok"
                                 :mkdirs? true}
                         :as :write}
                        {:op :emit
                         :input {:slot/id [:write :out]}}]}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime _opts]
                      {:result {:type :plan
                                :plan plan}})]
        (let [result (core/call-capability
                      runtime
                      {}
                      {:role :router
                       :intent :route/decide
                       :cap-id :llm/meta
                       :input {:prompt "plan"}})]
          (is (= :value (contracts/result-type-of result)))
          (is (= true (get-in result [:result :out :wrote?])))
          (is (= "tool-ok"
                 (slurp (io/file root "sandbox/tool.txt")))))))))

(deftest call-capability-propagates-auth-context-into-tool-plan
  (testing "call-capability! passes auth user and role policy into tool node payload and workflow env."
    (let [seen (atom nil)
          runtime {}
          roles-cfg {:enabled? true
                     :authorize-default? false
                     :anonymous-role :role/anonymous
                     :logged-in-role :role/user
                     :account-type->roles {:admin #{:role/admin}}
                     :effects {:fs/write {:any #{:role/admin}}}}
          auth-user {:user/id 21
                     :user/account-type :admin
                     :user/roles #{:role/admin}}
          plan {:nodes [{:op :tool
                         :tool/id :fs/write-file
                         :effects {:allowed #{:fs/write}}
                         :input {:path "sandbox/out.txt"
                                 :content "tool-ok"}
                         :as :write}
                        {:op :emit
                         :input {:slot/id [:write :out]}}]}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime _opts]
                      {:result {:type :plan
                                :plan plan}})
                    effects/invoke-tool!
                    (fn [_cfg tool-node env]
                      (reset! seen {:tool-node tool-node
                                    :env env})
                      {:result {:type :value
                                :out {:ok? true}}})]
        (let [result (core/call-capability
                      runtime
                      {}
                      {:role :router
                       :intent :route/decide
                       :cap-id :llm/meta
                       :input {:prompt "plan"}
                       :auth/user auth-user
                       :roles roles-cfg})]
          (is (= :value (contracts/result-type-of result)))
          (is (= 21 (get-in @seen [:tool-node :auth/user :user/id])))
          (is (= roles-cfg (get-in @seen [:tool-node :roles/config])))
          (is (= true (get-in @seen [:tool-node :auth/effects :ok?])))
          (is (= 21 (get-in @seen [:env :auth/user :user/id])))
          (is (= roles-cfg (get-in @seen [:env :roles/config]))))))))

(deftest call-capability-uses-configured-checks-and-judge-capability
  (testing "call-capability! passes protocol check-fns and triggers judge capability (:eval/grade)."
    (let [calls (atom [])
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}
                                        :eval/grade {:in-schema :req/eval}}
                              :result/types [:value :plan :error]
                              :policy/checks {:tests-pass :builtin/tests-pass}
                              :policy/default {:judge {:enabled? true
                                                       :intent :eval/grade
                                                       :cap/id :llm/judge
                                                       :role :router
                                                       :max-attempts 1
                                                       :score-path [:score]}}}
                   :resolver {:routing {:intent->cap {:eval/grade :llm/judge}}}}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (swap! calls conj opts)
                      (case (:intent opts)
                        :problem/solve
                        {:result {:type :plan
                                  :plan {:nodes []}}}

                        :eval/grade
                        {:result {:type :value
                                  :out {:score 0.42}}}

                        {:result {:type :value
                                  :out {:text "ok"}}}))
                    workflow/execute-plan
                    (fn [{:keys [check-fns judge-fn]}]
                      (let [judge-out (judge-fn {:intent :text/respond
                                                 :cap/id :llm/voice
                                                 :done {:score-min 0.8}}
                                                {}
                                                {:result {:type :value
                                                          :out {:text "x"}}})
                            tests-check ((get check-fns :tests-pass)
                                         {:intent :text/respond}
                                         {}
                                         {:result {:type :value
                                                   :out {:tests/pass? true}}})]
                        {:ok? true
                         :emitted {:judge-score (:score judge-out)
                                   :tests-check tests-check}}))]
        (let [result (core/call-capability
                      runtime
                      (:resolver runtime)
                      {:role :solver
                       :intent :problem/solve
                       :cap-id :llm/solver
                       :input {:prompt "diag"}})]
          (is (= :value (contracts/result-type-of result)))
          (is (= {:judge-score 0.42
                  :tests-check true}
                 (get-in result [:result :out])))
          (is (= [:problem/solve :eval/grade]
                 (mapv :intent @calls))))))))

(deftest call-capability-judge-parses-score-from-json-text
  (testing "Judge may return score as JSON in :out/:text and the result is parsed."
    (let [runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}
                                        :eval/grade {:in-schema :req/eval}}
                              :result/types [:value :plan :error]
                              :policy/default {:judge {:enabled? true
                                                       :intent :eval/grade
                                                       :cap/id :llm/judge
                                                       :score-path [:score]
                                                       :max-attempts 1}}}
                   :resolver {:routing {:intent->cap {:eval/grade :llm/judge}}}}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (case (:intent opts)
                        :problem/solve {:result {:type :plan
                                                 :plan {:nodes []}}}
                        :eval/grade {:result {:type :value
                                              :out {:text "{\"score\":0.73}"}}}
                        {:result {:type :value
                                  :out {:text "ok"}}}))
                    workflow/execute-plan
                    (fn [{:keys [judge-fn]}]
                      {:ok? true
                       :emitted {:judge-score (:score (judge-fn {:intent :text/respond
                                                                 :cap/id :llm/voice
                                                                 :done {:score-min 0.8}}
                                                                {}
                                                                {:result {:type :value
                                                                          :out {:text "x"}}}))}})]
        (let [result (core/call-capability
                      runtime
                      (:resolver runtime)
                      {:role :solver
                       :intent :problem/solve
                       :cap-id :llm/solver
                       :input {:prompt "diag"}})]
          (is (= :value (contracts/result-type-of result)))
          (is (= 0.73 (get-in result [:result :out :judge-score]))))))))

(deftest call-capability-retries-problem-solve-on-truncated-ending
  (testing "Problem solve fails quality gate and retries when output ends mid-sentence."
    (let [calls (atom 0)
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}}
                              :result/types [:value :plan :error]
                              :policy/checks {:no-truncated-ending :builtin/no-truncated-ending}
                              :policy/intents {:problem/solve {:checks [:no-truncated-ending]}}
                              :policy/default {:retry {:same-cap-max 1
                                                       :fallback-max 0}
                                               :switch-on #{:eval/must-failed}}}
                   :resolver {:routing {:intent->cap {:problem/solve :llm/solver}}}}
          plan {:nodes [{:op :call
                         :intent :problem/solve
                         :cap/id :llm/solver
                         :input {:prompt "Explain ACID briefly."}
                         :as :answer}
                        {:op :emit
                         :input {:slot/id [:answer :out]}}]}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (when (= :problem/solve (:intent opts))
                        (if (= 1 (swap! calls inc))
                          {:result {:type :value
                                    :out {:text "ACID is a set of database transaction guarantees and"}}}
                          {:result {:type :value
                                    :out {:text "ACID is a set of database transaction guarantees."}}})))]
        (let [result (core/call-capability
                      runtime
                      (:resolver runtime)
                      {:role :router
                       :intent :route/decide
                       :cap-id :llm/meta
                       :plan plan})]
          (is (= 2 @calls))
          (is (= :value (contracts/result-type-of result)))
          (is (= "ACID is a set of database transaction guarantees."
                 (get-in result [:result :out :text]))))))))

(deftest call-capability-retries-text-respond-on-truncated-ending
  (testing "Text respond can enforce no-truncated-ending as hard check and retry same capability."
    (let [calls (atom 0)
          runtime {:protocol {:intents {:text/respond {:in-schema :req/text}}
                              :result/types [:value :plan :error]
                              :policy/checks {:no-truncated-ending :builtin/no-truncated-ending}
                              :policy/default {:retry {:same-cap-max 1
                                                       :fallback-max 0}
                                               :switch-on #{:eval/must-failed}}}
                   :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}}
          plan {:nodes [{:op :call
                         :intent :text/respond
                         :cap/id :llm/voice
                         :dispatch {:checks/hard [:no-truncated-ending]
                                    :retry {:same-cap-max 1
                                            :fallback-max 0}
                                    :switch-on #{:eval/must-failed}}
                         :input {:prompt "Explain briefly."}
                         :as :answer}
                        {:op :emit
                         :input {:slot/id [:answer :out]}}]}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (when (= :text/respond (:intent opts))
                        (if (= 1 (swap! calls inc))
                          {:result {:type :value
                                    :out {:text "ACID means Atomicity, Consistency, Isolation and Durability"}}}
                          {:result {:type :value
                                    :out {:text "ACID means Atomicity, Consistency, Isolation, and Durability."}}})))]
        (let [result (core/call-capability
                      runtime
                      (:resolver runtime)
                      {:role :router
                       :intent :route/decide
                       :cap-id :llm/meta
                       :plan plan})]
          (is (= 2 @calls))
          (is (= :value (contracts/result-type-of result)))
          (is (= "ACID means Atomicity, Consistency, Isolation, and Durability."
                 (get-in result [:result :out :text]))))))))

(deftest invoke-capability-applies-protocol-defaults
  (testing "invoke-capability! builds request from protocol default branches and retry from :retry/max-attempts."
    (let [captured (atom nil)
          protocol {:retry/max-attempts 5
                    :intents {:text/respond {:in-schema :req/text}}
                    :constraints/default {:no-web true :language :pl}
                    :policy/default {:done {:must #{:schema-valid}
                                             :should #{:tests-pass}
                                             :score-min 0.8}}
                    :budget/default {:max-tokens 1200}
                    :effects/default {:allowed #{:none}}
                    :result/types [:value]}
          runtime {:protocol protocol}]
      (with-redefs [contracts/invoke-with-contract
                    (fn [_invoke-fn request opts]
                      (reset! captured {:request request :opts opts})
                      {:ok? true
                       :result {:result {:type :value
                                         :out {:text "ok"}}}})]
        (let [response (core/invoke-capability!
                        runtime
                        {:role :voice
                         :intent :text/respond
                         :cap-id :llm/voice
                         :model "voice-model"
                         :prompt "hej"})]
          (is (= {:type :value
                  :out {:text "ok"}}
                 (:result response)))
          (is (= {:role :voice
                  :intent :text/respond
                  :cap/id :llm/voice
                  :model-key :ferment.model/voice
                  :model "voice-model"}
                 (:invoke/meta response))))
        (is (= {:no-web true :language :pl}
               (get-in @captured [:request :constraints])))
        (is (= {:must #{:schema-valid}
                :should #{:tests-pass}
                :score-min 0.8}
               (get-in @captured [:request :done])))
        (is (= {:max-tokens 1200}
               (get-in @captured [:request :budget])))
        (is (= {:allowed #{:none}}
               (get-in @captured [:request :effects])))
        (is (= 5 (get-in @captured [:opts :max-attempts]))))))

(deftest invoke-capability-merges-intent-budget-over-default-and-request
  (testing "Budget defaults are merged as: protocol default -> intent default -> request-local."
    (let [captured (atom nil)
          protocol {:retry/max-attempts 3
                    :intents {:text/respond {:in-schema :req/text
                                             :budget {:max-tokens 640
                                                      :temperature 0.0}}}
                    :budget/default {:max-tokens 1200
                                     :max-roundtrips 3
                                     :top-p 0.9}
                    :result/types [:value]}
          runtime {:protocol protocol}]
      (with-redefs [contracts/invoke-with-contract
                    (fn [_invoke-fn request _opts]
                      (reset! captured request)
                      {:ok? true
                       :result {:result {:type :value
                                         :out {:text "ok"}}}})]
        (core/invoke-capability!
         runtime
         {:role :voice
          :intent :text/respond
          :cap-id :llm/voice
          :model "voice-model"
          :prompt "hej"
          :budget {:top-p 0.75}})
        (is (= {:max-tokens 640
                :max-roundtrips 3
                :top-p 0.75
                :temperature 0.0}
               (get-in @captured [:budget])))))))

(deftest invoke-capability-uses-merged-request-budget-for-generation-params
  (testing "Generation max-tokens/top-p are derived from merged request budget (default -> intent -> request)."
    (let [seen-call (atom nil)
          protocol {:intents {:text/respond {:in-schema :req/text
                                             :budget {:max-tokens 640
                                                      :top-p 0.85}}}
                    :budget/default {:max-tokens 1200
                                     :top-p 0.9}
                    :result/types [:value]}
          runtime {:protocol protocol}]
      (with-redefs [core/ollama-generate!
                    (fn [opts]
                      (reset! seen-call opts)
                      {:response "ok"})
                    contracts/invoke-with-contract
                    (fn [invoke-fn request _opts]
                      {:ok? true
                       :result (invoke-fn request 1)})]
        (core/invoke-capability!
         runtime
         {:role :voice
          :intent :text/respond
          :cap-id :llm/voice
          :input {:prompt "hej"}
          :budget {:top-p 0.75}})
        (is (= 640 (:max-tokens @seen-call)))
        (is (= 0.75 (:top-p @seen-call)))))))

(deftest invoke-capability-prefers-intent-done-policy-over-global-default
  (testing "Intent-level quality/:done is used when explicit request :done is not provided."
    (let [captured (atom nil)
          protocol {:retry/max-attempts 3
                    :intents {:text/respond {:in-schema :req/text}}
                    :policy/default {:done {:must #{:schema-valid}
                                             :should #{:tests-pass}
                                             :score-min 0.8}}
                    :policy/intents {:text/respond {:done {:must #{:schema-valid}
                                                           :score-min 1.0}}}
                    :result/types [:value]}
          runtime {:protocol protocol}]
      (with-redefs [contracts/invoke-with-contract
                    (fn [_invoke-fn request _opts]
                      (reset! captured request)
                      {:ok? true
                       :result {:result {:type :value
                                         :out {:text "ok"}}}})]
        (core/invoke-capability!
         runtime
         {:role :voice
          :intent :text/respond
          :cap-id :llm/voice
          :model "voice-model"
          :prompt "hej"})
        (is (= {:must #{:schema-valid}
                :score-min 1.0}
               (:done @captured)))))))

(deftest call-capability-respects-per-intent-judge-policy
  (testing "Intent-level quality/:judge may disable judge even with global judge enabled."
    (let [calls (atom [])
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}
                                        :text/respond {:in-schema :req/text}
                                        :eval/grade {:in-schema :req/eval}}
                              :policy/intents {:text/respond {:judge {:enabled? false}}}
                              :result/types [:value :plan :error]
                              :policy/default {:judge {:enabled? true
                                                       :intent :eval/grade
                                                       :cap/id :llm/judge
                                                       :role :router
                                                       :max-attempts 1}}}
                   :resolver {:routing {:intent->cap {:eval/grade :llm/judge}}}}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (swap! calls conj (:intent opts))
                      (case (:intent opts)
                        :problem/solve {:result {:type :plan
                                                 :plan {:nodes []}}}
                        :eval/grade {:result {:type :value
                                              :out {:score 0.2}}}
                        {:result {:type :value
                                  :out {:text "ok"}}}))
                    workflow/execute-plan
                    (fn [{:keys [judge-fn]}]
                      {:ok? true
                       :emitted {:judge-out (judge-fn {:intent :text/respond
                                                       :cap/id :llm/voice}
                                                      {}
                                                      {:result {:type :value
                                                                :out {:text "x"}}})}})]
        (let [result (core/call-capability
                      runtime
                      (:resolver runtime)
                      {:role :solver
                       :intent :problem/solve
                       :cap-id :llm/solver
                       :input {:prompt "diag"}})]
          (is (= :value (contracts/result-type-of result)))
          (is (nil? (get-in result [:result :out :judge-out])))
          (is (= [:problem/solve] @calls)))))))

(deftest invoke-capability-supports-stream-response-mode
  (testing "invoke-capability! can return canonical :stream result when response mode requests streaming."
    (let [runtime {:protocol {:intents {:text/respond {:in-schema :req/text}}
                              :result/types [:value :stream]}}]
      (with-redefs [core/ollama-generate!
                    (fn [_]
                      {:response "chunk-1"})]
        (let [result (core/invoke-capability!
                      runtime
                      {:role :voice
                       :intent :text/respond
                       :cap-id :llm/voice
                       :model "voice-model"
                       :prompt "hej"
                       :response/type :stream
                       :max-attempts 1})]
          (is (= :stream (contracts/result-type-of result)))
          (is (= :delta (get-in result [:result :stream 0 :event])))
          (is (= "chunk-1" (get-in result [:result :stream 0 :text]))))))))

(deftest call-capability-passes-stream-result-through
  (testing "call-capability! returns :stream result unchanged (no plan materialization)."
    (with-redefs [core/invoke-capability!
                  (fn [_runtime _opts]
                    {:result {:type :stream
                              :stream [{:seq 0 :event :delta :text "s-1"}
                                       {:seq 1 :event :done}]}})]
      (let [result (core/call-capability nil {} {:intent :text/respond})]
        (is (= :stream (contracts/result-type-of result)))
        (is (= "s-1" (get-in result [:result :stream 0 :text])))))))

(deftest invoke-capability-uses-request-overrides-and-rejects-unsupported-intent
  (testing "Request override replaces defaults, and unsupported intent fails before model invocation."
    (let [captured (atom nil)
          model-calls (atom 0)
          protocol {:retry/max-attempts 3
                    :intents {:text/respond {:in-schema :req/text}}
                    :constraints/default {:no-web true}
                    :policy/default {:done {:must #{:schema-valid}}}
                    :effects/default {:allowed #{:none}}
                    :result/types [:value]}
          runtime {:protocol protocol}]
      (with-redefs [contracts/invoke-with-contract
                    (fn [invoke-fn request opts]
                      (reset! captured {:request request :opts opts})
                      (invoke-fn request 1))
                    core/ollama-generate!
                    (fn [_]
                      (swap! model-calls inc)
                      {:response "ok"})]
        (is (= {:result {:type :value
                         :out {:text "ok"}
                         :usage {:mode :live}}}
               (core/invoke-capability!
                runtime
                {:role :voice
                 :intent :text/respond
                 :cap-id :llm/voice
                 :model "voice-model"
                 :prompt "hej"
                 :constraints {:style :concise}
                 :done {:must #{:schema-valid}
                        :should #{:tests-pass}
                        :score-min 1.0}
                 :effects {:allowed #{:none}}
                 :max-attempts 1})))
        (is (= {:style :concise}
               (get-in @captured [:request :constraints])))
        (is (= 1 (get-in @captured [:opts :max-attempts])))
        (is (= 1 @model-calls)))
      (with-redefs [core/ollama-generate!
                    (fn [_]
                      (swap! model-calls inc)
                      {:response "should-not-run"})]
        (let [ex (try
                   (core/invoke-capability!
                    runtime
                    {:role :coder
                     :intent :code/patch
                     :cap-id :llm/code
                     :model "solver-model"
                     :prompt "napraw"})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo ex))
          (is (= :invalid-request (:error (ex-data ex))))
          (is (= :intent/not-supported (:reason (ex-data ex))))
          (is (= 1 @model-calls))))))))

(deftest invoke-capability-opens-session-and-appends-turns
  (testing "invoke-capability! uses session service and logs user/assistant turns."
    (let [store   (session-store/init-store :ferment.session.store/default {:backend :memory})
          context (session/init-context :ferment.session.context/default {:context/version 1})
          manager (session/init-manager :ferment.session.manager/default
                                        {:store store
                                         :context context})
          service (session/init-service :ferment.session/default
                                        {:store store
                                         :context context
                                         :manager manager})
          seen-request (atom nil)
          seen-ollama (atom nil)]
      (with-redefs [core/ollama-generate!
                    (fn [params]
                      (reset! seen-ollama params)
                      {:response "OK"})]
        (let [result (core/invoke-capability!
                      nil
                      {:role :solver
                       :intent :problem/solve
                       :cap-id :llm/solver
                       :model "mock/solver"
                       :input {:prompt "diag: hello"}
                       :session/service service
                       :session/id "diag-s1"
                       :max-attempts 1
                       :result-parser
                       (fn [text {:keys [request]}]
                         (reset! seen-request request)
                         {:trace (:trace request)
                          :result {:type :value
                                   :out {:text text}}})})
              session-state (session/get! service "diag-s1")
              turns (:session/turns session-state)]
          (is (= "OK" (get-in result [:result :out :text])))
          (is (= "diag-s1" (:session/id @seen-request)))
          (is (= "diag-s1" (get-in @seen-request [:context :session/id])))
          (is (= "diag-s1" (:session-id @seen-ollama)))
          (is (= "diag-s1" (:session/id result)))
          (is (integer? (:session/version result)))
          (is (contains? #{:hot :warm} (:session/state result)))
          (is (boolean? (:session/frozen? result)))
          (is (= 2 (count turns)))
          (is (= :user (get-in turns [0 :turn/role])))
          (is (= "diag: hello" (get-in turns [0 :turn/text])))
          (is (= :assistant (get-in turns [1 :turn/role])))
          (is (= "OK" (get-in turns [1 :turn/text]))))))))

(deftest core-service-exposes-session-bridge-ops
  (testing "init-service exposes session and session-worker operations without global state."
    (let [runtime {:id :runtime}
          session-service {:open! (fn [sid _opts] {:session/id sid})
                           :get! (fn [sid] {:session/id sid :session/version 3})
                           :freeze! (fn [sid _opts] {:session/id sid :session/frozen? true})
                           :thaw! (fn [sid _opts] {:session/id sid :session/frozen? false})}
          service (core/init-service :ferment.core/default
                                     {:runtime runtime
                                      :resolver {}
                                      :protocol {}
                                      :session session-service})]
      (is (= {:session/id "s-1"} ((:session-open! service) "s-1")))
      (is (= 3 (get-in ((:session-get! service) "s-1") [:session/version])))
      (is (= true (get-in ((:session-freeze! service) "s-1") [:session/frozen?])))
      (is (= false (get-in ((:session-thaw! service) "s-1") [:session/frozen?])))
      (with-redefs [model/session-workers-state (fn [_] {:m {}})
                    model/freeze-session-worker! (fn [_ model-id sid]
                                                   {:ok? true :model model-id :session/id sid})
                    model/thaw-session-worker! (fn [_ model-id sid]
                                                 {:ok? true :model model-id :session/id sid})]
        (is (= {:m {}} ((:session-workers service))))
        (is (= {:ok? true :model :meta :session/id "s-1"}
               ((:session-worker-freeze! service) :meta "s-1")))
        (is (= {:ok? true :model :meta :session/id "s-1"}
               ((:session-worker-thaw! service) :meta "s-1")))))))
