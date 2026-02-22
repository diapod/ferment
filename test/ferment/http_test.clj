(ns ferment.http-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.core :as core]
            [ferment.http :as http]))

(deftest invoke-act-injects-auth-principal-into-core-options
  (testing "invoke-act passes authenticated user and role policy to core execution options."
    (let [seen (atom nil)
          runtime {:roles {:enabled? true
                           :authorize-default? false
                           :effects {:none {:any #{:role/user :role/operator :role/admin}}
                                     :fs/write {:any #{:role/admin}}}}
                   :protocol {}
                   :resolver {}}
          payload {:proto 1
                   :trace {:id "t-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :session/id "sess-1"
                   :input {:prompt "hej"}}
          auth {:user {:user/id 11
                       :user/email "u@example.com"
                       :user/account-type :operator
                       :user/roles #{:role/operator :role/reviewer}}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver opts]
                                   (reset! seen opts)
                                   {:result {:type :value
                                             :out {:text "ok"}}})]
                     (http/invoke-act runtime payload nil auth))]
      (is (= 200 (:status response)))
      (is (= :llm/voice (:cap-id @seen)))
      (is (= :text/respond (:intent @seen)))
      (is (= 11 (get-in @seen [:auth/user :user/id])))
      (is (= :operator (get-in @seen [:auth/user :user/account-type])))
      (is (= #{:role/operator :role/reviewer}
             (set (get-in @seen [:auth/user :user/roles]))))
      (is (= #{:role/operator :role/reviewer}
             (set (get-in @seen [:context :auth/user :user/roles]))))
      (is (= #{:role/operator :role/reviewer}
             (set (get-in @seen [:session/meta :user/roles]))))
      (is (= (:roles runtime) (:roles @seen))))))

(deftest invoke-act-applies-session-var-defaults-when-missing
  (testing "invoke-act fills missing context/constraints/input defaults from session vars."
    (let [seen (atom nil)]
      (letfn [(get-vars-fn
                ([sid ks]
                 (is (= "sess-ctx-1" sid))
                 (is (= [:session/language
                         :session/style
                         :session/system-prompt
                         :session/context-summary]
                        ks))
                 {:session/language :pl
                  :session/style :concise
                  :session/system-prompt "SYS-FROM-SESSION"
                  :session/context-summary "ctx-from-session"})
                ([sid ks opts]
                 (is (= :text/respond (:intent opts)))
                 (is (= :act/defaults (:operation opts)))
                 (get-vars-fn sid ks)))]
        (let [runtime {:protocol {}
                       :resolver {}
                       :session {:get-vars! get-vars-fn}}
              payload {:proto 1
                       :trace {:id "t-session-defaults-1"}
                       :session/id "sess-ctx-1"
                       :task {:intent :text/respond
                              :cap/id :llm/voice}
                       :input {:prompt "hej"}}
              response (with-redefs [core/execute-capability!
                                     (fn [_runtime _resolver opts]
                                       (reset! seen opts)
                                       {:result {:type :value
                                                 :out {:text "ok"}}})]
                         (http/invoke-act runtime payload nil nil))]
          (is (= 200 (:status response)))
          (is (= :pl (get-in @seen [:constraints :language])))
          (is (= :concise (get-in @seen [:constraints :style])))
          (is (= "SYS-FROM-SESSION" (get-in @seen [:input :system])))
          (is (= "ctx-from-session" (get-in @seen [:context :summary]))))))))

(deftest invoke-act-maps-forbidden-effect-to-403
  (testing "auth/forbidden-effect error from workflow/core is exposed as HTTP 403 envelope."
    (let [runtime {:roles {:enabled? true
                           :authorize-default? false
                           :effects {:fs/write {:any #{:role/admin}}}}
                   :protocol {}
                   :resolver {}}
          payload {:proto 1
                   :trace {:id "t-2"}
                   :task {:intent :code/patch
                          :cap/id :llm/code}
                   :input {:prompt "zrób patch"}
                   :effects {:allowed #{:fs/write}}}
          auth {:user {:user/id 12
                       :user/email "user@example.com"
                       :user/account-type :user
                       :user/roles #{:role/user}}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver _opts]
                                   (throw (ex-info "Forbidden effect"
                                                   {:error :auth/forbidden-effect
                                                    :failure/type :auth/forbidden-effect
                                                    :requested-effects #{:fs/write}
                                                    :denied-effects #{:fs/write}})))]
                     (http/invoke-act runtime payload nil auth))]
      (is (= 403 (:status response)))
      (is (= :auth/forbidden-effect (get-in response [:body :error :type])))
      (is (= #{:fs/write}
             (set (get-in response [:body :error :details :denied-effects])))))))

(deftest invoke-act-maps-effects-scope-denied-to-403
  (testing "Runtime scope denial for effects is exposed as HTTP 403 envelope."
    (let [runtime {:protocol {}
                   :resolver {}}
          payload {:proto 1
                   :trace {:id "t-3"}
                   :task {:intent :code/patch
                          :cap/id :llm/code}
                   :input {:prompt "run tool"}
                   :effects {:allowed #{:fs/write}}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver _opts]
                                   (throw (ex-info "Scope denied"
                                                   {:error :effects/scope-denied
                                                    :failure/type :effects/scope-denied
                                                    :effect :fs/write
                                                    :reason :path-not-allowed
                                                    :path "/tmp/out.txt"})))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 403 (:status response)))
      (is (= :effects/scope-denied (get-in response [:body :error :type])))
      (is (= :path-not-allowed
             (get-in response [:body :error :details :reason]))))))

(deftest invoke-act-supports-stream-response-mode
  (testing "invoke-act forwards stream mode to core and returns canonical :stream envelope."
    (let [seen (atom nil)
          runtime {:protocol {}
                   :resolver {}}
          payload {:proto 1
                   :trace {:id "t-4"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :input {:prompt "hej"}
                   :response/type "stream"}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver opts]
                                   (reset! seen opts)
                                   {:result {:type :stream
                                             :stream [{:seq 0 :event :delta :text "czesc"}
                                                      {:seq 1 :event :done}]}})]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= :stream (:response/type @seen)))
      (is (= :stream (get-in response [:body :result :type])))
      (is (= "czesc" (get-in response [:body :result :stream 0 :text]))))))

(deftest invoke-act-meta-routing-overrides-capability-when-decision-is-valid
  (testing "invoke-act runs route/decide first and uses decided :cap/id for the main call."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          seen (atom nil)
          calls (atom [])
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-5"}
                   :task {:intent :text/respond}
                   :input {:prompt "hej"}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver opts]
                                   (swap! calls conj (:intent opts))
                                   (case (:intent opts)
                                     :route/decide
                                     {:result {:type :value
                                               :out {:cap/id :llm/solver
                                                     :dispatch {:checks [:schema-valid]
                                                                :switch-on [:schema/invalid]
                                                                :retry {:same-cap-max 1}}}}}

                                     :text/respond
                                     (do
                                       (reset! seen opts)
                                       {:result {:type :value
                                                 :out {:text "ok"}}})

                                     {:result {:type :value
                                               :out {:text "unexpected"}}}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :text/respond] @calls))
      (is (= :llm/solver (:cap-id @seen))))))

(deftest invoke-act-meta-routing-adapts-tool-call-to-solver-voice-plan
  (testing "Meta route/decide output in tool_call format is adapted into canonical solver->voice plan and executed."
    (let [calls (atom [])
          routing {:intent->cap {:route/decide :llm/meta
                                 :problem/solve :llm/solver
                                 :text/respond :llm/voice}}
          protocol {:intents {:route/decide {:in-schema :req/route
                                             :result/contract {:type :plan
                                                               :contract/kind :route/solver->voice}}
                              :problem/solve {:in-schema :req/problem}
                              :text/respond {:in-schema :req/text}}
                    :result/types [:value :plan :error]
                    :retry/max-attempts 2}
          runtime {:protocol protocol
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-5b"}
                   :task {:intent :text/respond}
                   :input {:prompt "Kto stworzył Clojure?"}}
          response (with-redefs [core/ollama-generate!
                                 (fn [{:keys [intent prompt]}]
                                   (swap! calls conj intent)
                                   (case intent
                                     :route/decide
                                     {:response "<tool_call>{\"name\":\"solve_question\",\"arguments\":{\"question\":\"Kto stworzył Clojure?\"}}</tool_call>"}
                                     :problem/solve
                                     {:response "Clojure został stworzony przez Richa Hickeya."}
                                     :text/respond
                                     {:response (str "VOICE:" prompt)}
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :problem/solve :text/respond] @calls))
      (is (= "VOICE:Clojure został stworzony przez Richa Hickeya."
             (get-in response [:body :result :out :text])))
      (is (= #{:ferment.model/meta
               :ferment.model/solver
               :ferment.model/voice}
             (->> (get-in response [:body :models/used])
                  (map :model-key)
                  set)))
      (is (= #{:llm/meta :llm/solver :llm/voice}
             (->> (get-in response [:body :models/used])
                  (map :cap/id)
                  set))))))

(deftest invoke-act-meta-routing-synthesizes-plan-on-empty-route-output
  (testing "Meta route/decide may return empty text; parser still synthesizes canonical solver->voice plan from request prompt."
    (let [calls (atom [])
          routing {:intent->cap {:route/decide :llm/meta
                                 :problem/solve :llm/solver
                                 :text/respond :llm/voice}}
          protocol {:intents {:route/decide {:in-schema :req/route
                                             :result/contract {:type :plan
                                                               :contract/kind :route/solver->voice}}
                              :problem/solve {:in-schema :req/problem}
                              :text/respond {:in-schema :req/text}}
                    :result/types [:value :plan :error]
                    :retry/max-attempts 2}
          runtime {:protocol protocol
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-5c"}
                   :task {:intent :text/respond}
                   :routing {:meta? true
                             :strict? true}
                   :input {:prompt "Czy ryby piją?"}}
          response (with-redefs [core/ollama-generate!
                                 (fn [{:keys [intent prompt]}]
                                   (swap! calls conj intent)
                                   (case intent
                                     :route/decide {:response ""}
                                     :problem/solve {:response "Ryby nie piją jak ssaki; regulują gospodarkę wodną osmotycznie."}
                                     :text/respond {:response (str "VOICE:" prompt)}
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :problem/solve :text/respond] @calls))
      (is (= "VOICE:Ryby nie piją jak ssaki; regulują gospodarkę wodną osmotycznie."
             (get-in response [:body :result :out :text]))))))

(deftest invoke-act-meta-routing-ignores-invalid-model-plan-and-synthesizes-canonical-plan
  (testing "Meta route/decide ignores non-canonical model plan and falls back to synthesized solver->voice plan."
    (let [calls (atom [])
          routing {:intent->cap {:route/decide :llm/meta
                                 :problem/solve :llm/solver
                                 :text/respond :llm/voice}}
          protocol {:intents {:route/decide {:in-schema :req/route
                                             :result/contract {:type :plan
                                                               :contract/kind :route/solver->voice}}
                              :problem/solve {:in-schema :req/problem}
                              :text/respond {:in-schema :req/text}}
                    :result/types [:value :plan :error]
                    :retry/max-attempts 2}
          runtime {:protocol protocol
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-5d"}
                   :task {:intent :text/respond}
                   :routing {:meta? true
                             :strict? true}
                   :input {:prompt "Wyjaśnij ACID jednym zdaniem."}}
          response (with-redefs [core/ollama-generate!
                                 (fn [{:keys [intent prompt]}]
                                   (swap! calls conj intent)
                                   (case intent
                                     :route/decide
                                     {:response "{\"plan\":{\"nodes\":[{\"op\":\"call\",\"intent\":\"text/respond\",\"cap/id\":\"llm/voice\"}]}}"}
                                     :problem/solve
                                     {:response "ACID to zbiór gwarancji poprawności transakcji: atomowość, spójność, izolacja, trwałość."}
                                     :text/respond
                                     {:response (str "VOICE:" prompt)}
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :problem/solve :text/respond] @calls))
      (is (= "VOICE:ACID to zbiór gwarancji poprawności transakcji: atomowość, spójność, izolacja, trwałość."
             (get-in response [:body :result :out :text]))))))

(deftest invoke-act-meta-routing-can-expose-lazy-plan-in-debug-mode
  (testing "When routing debug is enabled, /v1/act includes pre-execution plan with lazy slot refs."
    (let [calls (atom [])
          routing {:intent->cap {:route/decide :llm/meta
                                 :problem/solve :llm/solver
                                 :text/respond :llm/voice}}
          protocol {:intents {:route/decide {:in-schema :req/route
                                             :result/contract {:type :plan
                                                               :contract/kind :route/solver->voice}}
                              :problem/solve {:in-schema :req/problem}
                              :text/respond {:in-schema :req/text}}
                    :result/types [:value :plan :error]
                    :retry/max-attempts 2}
          runtime {:protocol protocol
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-5dbg"}
                   :task {:intent :text/respond}
                   :routing {:meta? true
                             :strict? true
                             :debug/plan? true}
                   :input {:prompt "Wyjaśnij ACID jednym zdaniem."}}
          response (with-redefs [core/ollama-generate!
                                 (fn [{:keys [intent prompt]}]
                                   (swap! calls conj intent)
                                   (case intent
                                     :route/decide
                                     {:response "<tool_call>{\"name\":\"solve_question\",\"arguments\":{\"question\":\"Wyjaśnij ACID jednym zdaniem.\"}}</tool_call>"}
                                     :problem/solve
                                     {:response "ACID to zestaw gwarancji poprawności transakcji."}
                                     :text/respond
                                     {:response (str "VOICE:" prompt)}
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :problem/solve :text/respond] @calls))
      (is (= {:slot/id [:solver :out :text]}
             (get-in response [:body :result :plan/debug :nodes 1 :input :prompt])))
      (is (= {:slot/id [:voice :out]}
             (get-in response [:body :result :plan/debug :nodes 2 :input]))))))

(deftest invoke-act-meta-routing-fails-when-no-list-expansion-check-fails
  (testing "Strict meta routing fails closed when voice output violates :no-list-expansion quality policy."
    (let [calls (atom [])
          routing {:intent->cap {:route/decide :llm/meta
                                 :problem/solve :llm/solver
                                 :text/respond :llm/voice}}
          protocol {:intents {:route/decide {:in-schema :req/route
                                             :result/contract {:type :plan
                                                               :contract/kind :route/solver->voice}}
                              :problem/solve {:in-schema :req/problem
                                              :out-schema :res/problem}
                              :text/respond {:in-schema :req/text
                                             :out-schema :res/text}}
                    :result/types [:value :plan :error]
                    :retry/max-attempts 1
                    :policy/checks {:schema-valid :builtin/schema-valid
                                    :no-list-expansion :builtin/no-list-expansion}
                    :policy/intents {:text/respond {:done {:must #{:schema-valid}
                                                            :should #{:no-list-expansion}
                                                            :score-min 1.0}
                                                    :checks [:schema-valid]}}}
          runtime {:protocol protocol
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-5e"}
                   :task {:intent :text/respond}
                   :routing {:meta? true
                             :strict? true
                             :force? true}
                   :input {:prompt "Wyjaśnij ACID jednym zdaniem."}}
          response (with-redefs [core/ollama-generate!
                                 (fn [{:keys [intent]}]
                                   (swap! calls conj intent)
                                   (case intent
                                     :route/decide
                                     {:response "<tool_call>{\"name\":\"solve_question\",\"arguments\":{\"question\":\"Wyjaśnij ACID jednym zdaniem.\"}}</tool_call>"}
                                     :problem/solve
                                     {:response "ACID to zestaw gwarancji dla transakcji."}
                                     :text/respond
                                     {:response "- atomowość\n- spójność\n- izolacja\n- trwałość"}
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 502 (:status response)))
      (is (= :route/decide-failed
             (get-in response [:body :error :type])))
      (is (= [:route/decide :problem/solve :text/respond] @calls)))))

(deftest invoke-act-meta-routing-strict-mode-fails-closed
  (testing "strict meta routing returns 502 when the decider fails and does not execute main capability."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          main-called? (atom false)
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-6"}
                   :task {:intent :text/respond}
                   :routing {:strict? true}
                   :input {:prompt "hej"}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver opts]
                                   (if (= :route/decide (:intent opts))
                                     (throw (ex-info "route failed" {:error :runtime/invoke-failed}))
                                     (do
                                       (reset! main-called? true)
                                       {:result {:type :value
                                                 :out {:text "should-not-happen"}}})))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 502 (:status response)))
      (is (= :route/decide-failed (get-in response [:body :error :type])))
      (is (false? @main-called?)))))

(deftest invoke-act-meta-routing-falls-back-when-not-strict
  (testing "non-strict meta routing falls back to static resolver routing when decider fails."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          seen (atom nil)
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-7"}
                   :task {:intent :text/respond}
                   :input {:prompt "hej"}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver opts]
                                   (if (= :route/decide (:intent opts))
                                     (throw (ex-info "route failed" {:error :runtime/invoke-failed}))
                                     (do
                                       (reset! seen opts)
                                       {:result {:type :value
                                                 :out {:text "fallback-ok"}}})))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= :llm/voice (:cap-id @seen)))
      (is (= "fallback-ok" (get-in response [:body :result :out :text]))))))

(deftest invoke-act-meta-routing-can-return-final-response
  (testing "if route/decide does not emit a routing decision map, invoke-act returns that response directly."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          main-called? (atom false)
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-8"}
                   :task {:intent :text/respond}
                   :input {:prompt "hej"}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver opts]
                                   (if (= :route/decide (:intent opts))
                                     {:result {:type :value
                                               :out {:text "route-only"}}}
                                     (do
                                       (reset! main-called? true)
                                       {:result {:type :value
                                                 :out {:text "should-not-happen"}}})))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= "route-only" (get-in response [:body :result :out :text])))
      (is (false? @main-called?)))))

(deftest invoke-act-meta-routing-telemetry-counters
  (testing "invoke-act records route/decide telemetry counters for continue, fail-open, fail-closed and strict mode."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          telemetry (atom {})
          mode (atom :continue)
          payload {:proto 1
                   :trace {:id "t-9"}
                   :task {:intent :text/respond}
                   :input {:prompt "hej"}}]
      (with-redefs [core/execute-capability!
                    (fn [_runtime _resolver opts]
                      (if (= :route/decide (:intent opts))
                        (case @mode
                          :continue {:result {:type :value
                                              :out {:cap/id :llm/solver}}}
                          :fail-open (throw (ex-info "route failed" {:error :runtime/invoke-failed}))
                          :fail-closed (throw (ex-info "route failed" {:error :runtime/invoke-failed})))
                        {:result {:type :value
                                  :out {:text "ok"}}}))]
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil))))
        (reset! mode :fail-open)
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil))))
        (reset! mode :fail-closed)
        (is (= 502 (:status (http/invoke-act runtime
                                             (assoc payload :routing {:strict? true})
                                             telemetry
                                             nil)))))
      (is (= 3 (get-in @telemetry [:act :routing :route/decide-hit])))
      (is (= 1 (get-in @telemetry [:act :routing :route/decide-continue])))
      (is (= 1 (get-in @telemetry [:act :routing :route/fail-open])))
      (is (= 1 (get-in @telemetry [:act :routing :route/fail-closed])))
      (is (= 1 (get-in @telemetry [:act :routing :route/strict]))))))

(deftest invoke-act-writes-audit-trail-event
  (testing "invoke-act emits persistent audit event with trace/request/session/principal/intent/capability/outcome."
    (let [seen (atom nil)
          runtime {:protocol {}
                   :resolver {}}
          payload {:proto 1
                   :trace {:id "trace-audit-1"}
                   :request/id "req-audit-1"
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :session/id "sess-audit-1"
                   :input {:prompt "hej"}}
          auth {:source :http/basic
                :user {:user/id 42
                       :user/email "audit@example.com"
                       :user/account-type :operator
                       :user/roles #{:role/operator}}}
          response (with-redefs [core/execute-capability!
                                 (fn [_runtime _resolver _opts]
                                   {:result {:type :value
                                             :out {:text "ok"}}})
                                 ferment.oplog/logger
                                 (fn [_sub _cfg]
                                   (fn [& {:as msg}]
                                     (reset! seen msg)))]
                     (http/invoke-act runtime payload nil auth))]
      (is (= 200 (:status response)))
      (is (= "trace-audit-1" (:trace-id @seen)))
      (is (= "req-audit-1" (:request-id @seen)))
      (is (= "sess-audit-1" (:session-id @seen)))
      (is (= :text/respond (:intent @seen)))
      (is (= :llm/voice (:capability @seen)))
      (is (= :ok (:outcome @seen)))
      (is (= 200 (:status @seen)))
      (is (= 42 (:principal-id @seen)))
      (is (= "audit@example.com" (:principal-email @seen))))))
