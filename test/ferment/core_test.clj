(ns

    ^{:doc    "Core runtime contract integration tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.core-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.contracts :as contracts]
            [ferment.core :as core]
            [ferment.model :as model]
            [ferment.session :as session]
            [ferment.session.store :as session-store]))

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
  (testing "solver! nie używa modelu coding, gdy brak :ferment.model/solver."
    (let [called-with (atom nil)
          runtime {:models {:ferment.model/coding {:id "coding-from-model-branch"}}}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (is (= "{\"intent\":\"ok\"}" (core/solver! "rozwiąż problem" runtime)))
        (is (= "mlx-community/Qwen2.5-7B-Instruct-4bit" (:model @called-with)))))))

(deftest coder-uses-coding-model-selection
  (testing "coder! używa selektora :ferment.model/coding dla capability kodu."
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
  (testing "Generator preferuje invoke na uruchomionym runtime workerze (reuse procesu)."
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
                            :session-id "s-42"
                            :mode :live}))))
        (is (= :ferment.model/solver (:model-k @called)))
        (is (= "s-42" (get-in @called [:opts :session/id])))))))

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

(deftest invoke-capability-applies-protocol-defaults
  (testing "invoke-capability! buduje request z domyślnych gałęzi protokołu i retry z :retry/max-attempts."
    (let [captured (atom nil)
          protocol {:retry/max-attempts 5
                    :intents {:text/respond {:in-schema :req/text}}
                    :constraints/default {:no-web true :language :pl}
                    :done/default {:must #{:schema-valid}
                                   :should #{:tests-pass}
                                   :score-min 0.8}
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
        (is (= {:result {:type :value
                         :out {:text "ok"}}}
               (core/invoke-capability!
                runtime
                {:role :voice
                 :intent :text/respond
                 :cap-id :llm/voice
                 :model "voice-model"
                 :prompt "hej"})))
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

(deftest invoke-capability-uses-request-overrides-and-rejects-unsupported-intent
  (testing "Request override przesłania defaulty, a nieobsługiwany intent failuje przed wywołaniem modelu."
    (let [captured (atom nil)
          model-calls (atom 0)
          protocol {:retry/max-attempts 3
                    :intents {:text/respond {:in-schema :req/text}}
                    :constraints/default {:no-web true}
                    :done/default {:must #{:schema-valid}}
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
  (testing "invoke-capability! korzysta z session service i loguje turn user/assistant."
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
          (is (= 2 (count turns)))
          (is (= :user (get-in turns [0 :turn/role])))
          (is (= "diag: hello" (get-in turns [0 :turn/text])))
          (is (= :assistant (get-in turns [1 :turn/role])))
          (is (= "OK" (get-in turns [1 :turn/text]))))))))
