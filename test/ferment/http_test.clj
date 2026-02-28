(ns ferment.http-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [ferment.core :as core]
            [ferment.http :as http]
            [ferment.queue :as queue]
            [ferment.telemetry :as telemetry]))

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
          response (with-redefs [core/call-capability
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

(deftest invoke-act-uses-configured-middleware-chain
  (testing "invoke-act may run through configured act middleware chain compiled from data modules."
    (let [called (atom 0)
          custom-module {:name :test/act-shortcut
                         :compile (fn [_runtime _opts]
                                    (fn [_next]
                                      (fn [ctx]
                                        (assoc ctx
                                               :request* {:trace {:id "mw-1"}}
                                               :response {:status 200
                                                          :body {:proto 1
                                                                 :trace {:id "mw-1"}
                                                                 :result {:type :value
                                                                          :out {:text "middleware-ok"}}}}))))}
          runtime {:protocol {}
                   :resolver {}
                   :act/middleware [custom-module]}
          payload {:proto 1
                   :trace {:id "mw-1"}
                   :task {:intent :text/respond}
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
                                 (fn [_runtime _resolver _opts]
                                   (swap! called inc)
                                   {:result {:type :value
                                             :out {:text "core"}}})]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= "middleware-ok" (get-in response [:body :result :out :text])))
      (is (zero? @called)))))

(deftest invoke-act-derives-role-from-resolver-config
  (testing "invoke-act derives execution role from resolver capability/routing maps (not hardcoded HTTP table)."
    (let [seen (atom nil)
          runtime {:protocol {}
                   :resolver {:caps/by-id {:llm/voice {:cap/id :llm/voice
                                                       :dispatch/role :router}}
                              :routing {:cap->role {:llm/voice :coder}}}}
          payload {:proto 1
                   :trace {:id "t-role-map-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
                                 (fn [_runtime _resolver opts]
                                   (reset! seen opts)
                                   {:result {:type :value
                                             :out {:text "ok"}}})]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      ;; resolver :caps/by-id dispatch role has precedence
      (is (= :router (:role @seen))))))

(deftest invoke-act-applies-session-var-defaults-when-missing
  (testing "invoke-act fills missing context/constraints/input defaults from session vars."
    (let [seen (atom nil)]
      (letfn [(get-vars-fn
                ([sid ks]
                 (is (= "sess-ctx-1" sid))
                 (is (= #{:session/language
                          :session/style
                          :session/system-prompt
                          :session/context-summary}
                        (set ks)))
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
              response (with-redefs [core/call-capability
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

(deftest invoke-act-applies-session-var-defaults-from-contract-bindings
  (testing "invoke-act reads request default bindings from session contract."
    (let [seen (atom nil)]
      (letfn [(get-vars-fn
                ([sid ks]
                 (is (= "sess-ctx-2" sid))
                 (is (= #{:request/topic
                          :runtime/language}
                        (set ks)))
                 {:request/topic "  acid  "
                  :runtime/language "pl"})
                ([sid ks opts]
                 (is (= :text/respond (:intent opts)))
                 (is (= :act/defaults (:operation opts)))
                 (get-vars-fn sid ks)))]
        (let [runtime {:protocol {}
                       :resolver {}
                       :session {:store {:session-vars/contract
                                         {:request/default-bindings
                                          {:request/topic {:target [:context :topic]
                                                           :coerce :trimmed-string}
                                           :runtime/language {:target [:constraints :language]
                                                              :coerce :keyword-or-string}}}}
                                 :get-vars! get-vars-fn}}
              payload {:proto 1
                       :trace {:id "t-session-defaults-2"}
                       :session/id "sess-ctx-2"
                       :task {:intent :text/respond
                              :cap/id :llm/voice}
                       :input {:prompt "hej"}}
              response (with-redefs [core/call-capability
                                     (fn [_runtime _resolver opts]
                                       (reset! seen opts)
                                       {:result {:type :value
                                                 :out {:text "ok"}}})]
                         (http/invoke-act runtime payload nil nil))]
          (is (= 200 (:status response)))
          (is (= :pl (get-in @seen [:constraints :language])))
          (is (= "acid" (get-in @seen [:context :topic]))))))))

(deftest invoke-act-session-defaults-arity-adapter-learns-once
  (testing "Session defaults adapter may fall back to 2-arity get-vars! once and reuse learned arity on next calls."
    (let [cache (var-get #'ferment.http/session-get-vars-invokers)
          counts (atom {:two 0 :three 0})
          seen (atom nil)]
      (reset! cache {:entries {} :order []})
      (let [get-vars-fn (fn
                          ([sid ks]
                           (swap! counts update :two inc)
                           (is (= "sess-ctx-arity-1" sid))
                           (is (= #{:request/topic} (set ks)))
                           {:request/topic "acid"})
                          ([sid ks _opts]
                           (swap! counts update :three inc)
                           (throw (clojure.lang.ArityException. 3 "get-vars-fn"))))
            runtime {:protocol {}
                     :resolver {}
                     :session {:store {:session-vars/contract
                                       {:request/default-bindings
                                        {:request/topic {:target [:context :topic]
                                                         :coerce :trimmed-string}}}}
                               :get-vars! get-vars-fn}}
            payload {:proto 1
                     :trace {:id "t-session-defaults-arity-1"}
                     :session/id "sess-ctx-arity-1"
                     :task {:intent :text/respond
                            :cap/id :llm/voice}
                     :input {:prompt "hej"}}]
        (with-redefs [core/call-capability
                      (fn [_runtime _resolver opts]
                        (reset! seen opts)
                        {:result {:type :value
                                  :out {:text "ok"}}})]
          (is (= 200 (:status (http/invoke-act runtime payload nil nil))))
          (is (= "acid" (get-in @seen [:context :topic])))
          (is (= 200 (:status (http/invoke-act runtime
                                               (assoc payload :trace {:id "t-session-defaults-arity-2"})
                                               nil
                                               nil)))))
        (is (= 1 (:three @counts)))
        (is (= 2 (:two @counts)))
        (is (= 1 (count (:entries @cache))))))))

(deftest invoke-act-session-defaults-invoker-cache-is-bounded
  (testing "Invoker cache uses bounded size and evicts oldest entries under high churn."
    (let [cache (var-get #'ferment.http/session-get-vars-invokers)
          max-size (var-get #'ferment.http/session-get-vars-invokers-max-size)]
      (reset! cache {:entries {} :order []})
      (dotimes [_ (+ max-size 32)]
        (let [service {:get-vars! (fn
                                    ([sid ks] {})
                                    ([sid ks opts] {}))}]
          (#'ferment.http/session-get-vars-invoker service)))
      (is (= (count (:entries @cache))
             (count (:order @cache))))
      (is (<= (count (:entries @cache))
              max-size)))))

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
          response (with-redefs [core/call-capability
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
          response (with-redefs [core/call-capability
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

(deftest invoke-act-maps-effects-invalid-input-to-400
  (testing "Invalid effect declaration is exposed as HTTP 400 envelope with canonical details."
    (let [runtime {:protocol {}
                   :resolver {}}
          payload {:proto 1
                   :trace {:id "t-3b"}
                   :task {:intent :code/patch
                          :cap/id :llm/code}
                   :input {:prompt "run tool"}}
          response (with-redefs [core/call-capability
                                 (fn [_runtime _resolver _opts]
                                   (throw (ex-info "Invalid tool input"
                                                   {:error :effects/invalid-input
                                                    :failure/type :effects/invalid-input
                                                    :reason :effects/not-declared
                                                    :tool/id :fs/write-file})))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 400 (:status response)))
      (is (= :effects/invalid-input (get-in response [:body :error :type])))
      (is (= :effects/not-declared
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
          response (with-redefs [core/call-capability
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

(deftest invoke-act-accepted-submits-async-job
  (testing "response/type=accepted enqueues job and returns 202 accepted envelope without invoking core runtime."
    (let [calls (atom 0)
          runtime {:protocol {}
                   :resolver {}
                   :queue/service (queue/init-service {:enabled? true
                                                       :max-size 8})}
          payload {:proto 1
                   :trace {:id "t-accepted-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :response/type :accepted
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
                                 (fn [& _]
                                   (swap! calls inc)
                                   {:result {:type :value
                                             :out {:text "nope"}}})]
                     (http/invoke-act runtime payload nil nil))
          out (get-in response [:body :result :out])]
      (is (= 202 (:status response)))
      (is (= :accepted (get-in response [:body :response/type])))
      (is (= :value (get-in response [:body :result :type])))
      (is (= :queued (:job/status out)))
      (is (string? (:job/id out)))
      (is (zero? @calls)))))

(deftest invoke-act-accepted-maps-queue-full-to-overload
  (testing "When async queue is full, invoke-act returns deterministic overload error."
    (let [runtime {:protocol {}
                   :resolver {}
                   :queue/service (queue/init-service {:enabled? true
                                                       :max-size 1})}
          payload {:proto 1
                   :trace {:id "t-accepted-full-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :response/type :accepted
                   :input {:prompt "hej"}}]
      (is (= 202 (:status (http/invoke-act runtime payload nil nil))))
      (let [response (http/invoke-act runtime
                                      (assoc payload :trace {:id "t-accepted-full-2"})
                                      nil
                                      nil)]
        (is (= 503 (:status response)))
        (is (= :runtime/overloaded (get-in response [:body :error :type])))
        (is (= :queue/full (get-in response [:body :error :details :error])))))))

(deftest queue-job-status-response-returns-canonical-payload
  (testing "Queue job status endpoint response returns canonical payload for existing and missing jobs."
    (let [service (queue/init-service {:enabled? true
                                       :max-size 8})
          runtime {:queue/service service}
          submit (queue/submit! service {:task {:intent :text/respond}
                                         :input {:prompt "hej"}})
          job-id (get-in submit [:job :job/id])
          existing (#'ferment.http/queue-job-status-response runtime {:trace {:id "q-status-1"}} job-id)
          missing (#'ferment.http/queue-job-status-response runtime {:trace {:id "q-status-2"}} "job/404")]
      (is (= 200 (:status existing)))
      (is (= job-id (get-in existing [:body :result :out :job/id])))
      (is (= :queued (get-in existing [:body :result :out :job/status])))
      (is (= 404 (:status missing)))
      (is (= :queue/job-not-found (get-in missing [:body :error :type]))))))

(deftest queue-job-cancel-response-handles-accepted-and-rejected-cancel
  (testing "Cancel endpoint returns accepted=true on first cancel and accepted=false on repeated cancel."
    (let [service (queue/init-service {:enabled? true
                                       :max-size 8})
          runtime {:queue/service service}
          submit (queue/submit! service {:task {:intent :text/respond}
                                         :input {:prompt "hej"}})
          job-id (get-in submit [:job :job/id])
          first-cancel (#'ferment.http/queue-job-cancel-response runtime {:trace {:id "q-cancel-1"}} job-id :user-request)
          second-cancel (#'ferment.http/queue-job-cancel-response runtime {:trace {:id "q-cancel-2"}} job-id :user-request)]
      (is (= 200 (:status first-cancel)))
      (is (= true (get-in first-cancel [:body :result :out :cancel/accepted?])))
      (is (= :canceled (get-in first-cancel [:body :result :out :job/status])))
      (is (= 200 (:status second-cancel)))
      (is (= false (get-in second-cancel [:body :result :out :cancel/accepted?])))
      (is (= :canceled (get-in second-cancel [:body :result :out :job/status]))))))

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
          response (with-redefs [core/call-capability
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
          voice-calls (atom 0)
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
                                     (if (= 1 (swap! voice-calls inc))
                                       {:response "Nie wiem"}
                                       {:response (str "VOICE:" prompt)})
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :text/respond :problem/solve :text/respond] @calls))
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
          voice-calls (atom 0)
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
                                     :text/respond (if (= 1 (swap! voice-calls inc))
                                                     {:response "Nie wiem"}
                                                     {:response (str "VOICE:" prompt)})
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :text/respond :problem/solve :text/respond] @calls))
      (is (= "VOICE:Ryby nie piją jak ssaki; regulują gospodarkę wodną osmotycznie."
             (get-in response [:body :result :out :text]))))))

(deftest invoke-act-meta-routing-ignores-invalid-model-plan-and-synthesizes-canonical-plan
  (testing "Meta route/decide ignores non-canonical model plan and falls back to synthesized solver->voice plan."
    (let [calls (atom [])
          voice-calls (atom 0)
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
                                     (if (= 1 (swap! voice-calls inc))
                                       {:response "Nie wiem"}
                                       {:response (str "VOICE:" prompt)})
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :text/respond :problem/solve :text/respond] @calls))
      (is (= "VOICE:ACID to zbiór gwarancji poprawności transakcji: atomowość, spójność, izolacja, trwałość."
             (get-in response [:body :result :out :text]))))))

(deftest invoke-act-meta-routing-can-expose-lazy-plan-in-debug-mode
  (testing "When routing debug is enabled, /v1/act includes pre-execution plan with lazy slot refs."
    (let [calls (atom [])
          voice-calls (atom 0)
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
                             :debug/plan? true
                             :debug-transcript? true}
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
                                     (if (= 1 (swap! voice-calls inc))
                                       {:response "Nie wiem"}
                                       {:response (str "VOICE:" prompt)})
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:route/decide :text/respond :problem/solve :text/respond] @calls))
      (is (= {:slot/id [:solver :out :text]}
             (get-in response [:body :result :plan/debug :nodes 2 :input :prompt])))
      (is (string? (get-in response [:body :result :plan/debug :nodes 2 :system])))
      (is (str/includes? (get-in response [:body :result :plan/debug :nodes 2 :system])
                         "Rewrite for tone/style only"))
      (is (= [:schema-valid :no-truncated-ending]
             (get-in response [:body :result :plan/debug :nodes 2 :dispatch :checks/hard])))
      (is (= {:same-cap-max 1 :fallback-max 0}
             (get-in response [:body :result :plan/debug :nodes 2 :dispatch :retry])))
      (is (= {:slot/id [:voice-primary :out]}
             (get-in response [:body :result :plan/debug :nodes 3 :input])))
      (is (= {:slot/id [:voice-final :out]}
             (get-in response [:body :result :plan/debug :nodes 4 :input])))
      (is (= 3 (count (get-in response [:body :result :plan/run :transcript]))))
      (is (= :text/respond
             (get-in response [:body :result :plan/run :transcript 0 :intent])))
      (is (= "Wyjaśnij ACID jednym zdaniem."
             (get-in response [:body :result :plan/run :transcript 0 :input :prompt])))
      (is (= :problem/solve
             (get-in response [:body :result :plan/run :transcript 1 :intent])))
      (is (= "Wyjaśnij ACID jednym zdaniem."
             (get-in response [:body :result :plan/run :transcript 1 :input :prompt])))
      (is (= :text/respond
             (get-in response [:body :result :plan/run :transcript 2 :intent])))
      (is (= "ACID to zestaw gwarancji poprawności transakcji."
             (get-in response [:body :result :plan/run :transcript 2 :input :prompt]))))))

(deftest invoke-act-meta-routing-softens-voice-final-list-expansion-check
  (testing "Strict meta routing may recover to solver->voice-final when primary voice fails hard list-expansion gate."
    (let [calls (atom [])
          text-respond-calls (atom 0)
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
                                     (case (swap! text-respond-calls inc)
                                       ;; voice-primary -> hard fail (list expansion)
                                       1 {:response "- atomowość\n- spójność\n- izolacja\n- trwałość"}
                                       ;; voice-final attempt #1 -> hard fail (truncated ending)
                                       2 {:response "ACID to zestaw gwarancji dla transakcji"}
                                       ;; voice-final retry #2 -> pass
                                       {:response "- atomowość.\n- spójność.\n- izolacja.\n- trwałość."})
                                     {:response "UNEXPECTED"}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= "- atomowość.\n- spójność.\n- izolacja.\n- trwałość."
             (get-in response [:body :result :out :text])))
      (is (= [:route/decide :text/respond :problem/solve :text/respond :text/respond] @calls)))))

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
          response (with-redefs [core/call-capability
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

(deftest invoke-act-routing-profile-low-latency-disables-meta-decider
  (testing "routing profile :low-latency disables meta decider and executes direct text/respond."
    (let [calls (atom [])
          routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing
                            :defaults {:meta? true
                                       :strict? true
                                       :on-error :fail-closed}
                            :profiles {:low-latency {:meta? false
                                                     :strict? false
                                                     :force? false
                                                     :on-error :fail-open}}}}
          payload {:proto 1
                   :trace {:id "t-low-latency"}
                   :task {:intent :text/respond}
                   :routing {:profile "low-latency"}
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
                                 (fn [_runtime _resolver opts]
                                   (swap! calls conj (:intent opts))
                                   (case (:intent opts)
                                     :text/respond {:result {:type :value
                                                             :out {:text "SZYBKO"}}}
                                     :route/decide (throw (ex-info "meta-decider should be disabled for low-latency profile"
                                                                   {:error :unexpected-route-decide}))
                                     {:result {:type :value
                                               :out {:text "UNEXPECTED"}}}))]
                     (http/invoke-act runtime payload nil nil))]
      (is (= 200 (:status response)))
      (is (= [:text/respond] @calls))
      (is (= "SZYBKO" (get-in response [:body :result :out :text]))))))

(deftest invoke-act-meta-routing-request-strict-overrides-default-fail-open
  (testing "request :routing/:strict? true enforces fail-closed even when router default :on-error is :fail-open."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          main-called? (atom false)
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing
                            :defaults {:meta? true
                                       :on-error :fail-open}}}
          payload {:proto 1
                   :trace {:id "t-6-request-strict-overrides-default"}
                   :task {:intent :text/respond}
                   :routing {:strict? true}
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
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

(deftest invoke-act-meta-routing-strict-fail-closed-includes-rich-details
  (testing "strict fail-closed includes compact routing failure context in error details."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing
                            :defaults {:meta? true
                                       :on-error :fail-open}}}
          payload {:proto 1
                   :trace {:id "t-6-rich-details"}
                   :task {:intent :text/respond}
                   :routing {:strict? true}
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
                                 (fn [_runtime _resolver opts]
                                  (if (= :route/decide (:intent opts))
                                     (throw (ex-info
                                             "Call node failed quality/dispatch policy"
                                             {:error :runtime/invoke-failed
                                              :attempts 3
                                              :last-check {:ok? false
                                                           :error :invalid-result
                                                           :reason :output/schema-invalid
                                                           :intent :route/decide
                                                           :result/type :value
                                                           :details {:reason :schema/invalid
                                                                     :schema :res/route}}
                                              :failure/type :schema/invalid
                                              :node {:op :call
                                                     :intent :route/decide
                                                     :cap/id :llm/meta
                                                     :as :router}
                                              :outcome {:ok? false
                                                        :cap/id :llm/meta
                                                        :attempt 2
                                                        :failure/type :schema/invalid
                                                        :failure/recover? true
                                                        :done/eval {:ok? false
                                                                    :must-failed [:no-list-expansion]
                                                                    :should-failed []
                                                                    :judge/pass? true
                                                                    :checks [{:check :schema-valid
                                                                              :ok? false
                                                                              :reason :schema/invalid}]}}
                                              :switch-on #{:schema/invalid}
                                              :retry-policy {:same-cap-max 1
                                                             :fallback-max 1}
                                              :candidates [:llm/meta :llm/solver]
                                              :rejected-candidates [{:cap/id :llm/solver
                                                                     :reason :intent/not-supported}]}))
                                     {:result {:type :value
                                               :out {:text "should-not-happen"}}}))]
                     (http/invoke-act runtime payload nil nil))
          details (get-in response [:body :error :details])]
      (is (= 502 (:status response)))
      (is (= :route/decide-failed (get-in response [:body :error :type])))
      (is (= :route/decide (:route/intent details)))
      (is (= :llm/meta (:route/cap-id details)))
      (is (= :schema/invalid (:failure/type details)))
      (is (= 3 (:attempts details)))
      (is (= :invalid-result (get-in details [:last-check :error])))
      (is (= :output/schema-invalid (get-in details [:last-check :reason])))
      (is (= :schema/invalid (get-in details [:last-check :details :reason])))
      (is (= :res/route (get-in details [:last-check :details :schema])))
      (is (= :schema/invalid (get-in details [:outcome :failure/type])))
      (is (= {:same-cap-max 1 :fallback-max 1} (:retry-policy details)))
      (is (= [:llm/meta :llm/solver] (:candidates details)))
      (is (= [:schema/invalid] (:switch-on details)))
      (is (= :call (get-in details [:node :op])))
      (is (= [:no-list-expansion] (get-in details [:outcome :done/eval :must-failed])))
      (is (= [] (get-in details [:outcome :done/eval :should-failed])))
      (is (= true (get-in details [:outcome :done/eval :judge/pass?])))
      (is (= :schema-valid (get-in details [:outcome :done/eval :checks 0 :check]))))))

(deftest invoke-act-meta-routing-fails-closed-by-router-default-on-error
  (testing "router :defaults/:on-error :fail-closed enforces fail-closed even without request :routing/:strict?."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          main-called? (atom false)
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing
                            :defaults {:meta? true
                                       :on-error :fail-closed}}}
          payload {:proto 1
                   :trace {:id "t-6-defaults"}
                   :task {:intent :text/respond}
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
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

(deftest invoke-act-meta-routing-request-on-error-overrides-defaults
  (testing "request :routing/:on-error may override router defaults and force fail-open behavior."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          seen (atom nil)
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing
                            :defaults {:meta? true
                                       :on-error :fail-closed}}}
          payload {:proto 1
                   :trace {:id "t-6-override"}
                   :task {:intent :text/respond}
                   :routing {:on-error :fail-open}
                   :input {:prompt "hej"}}
          response (with-redefs [core/call-capability
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
          response (with-redefs [core/call-capability
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

(deftest invoke-act-meta-routing-retries-decider-then-fails-open-deterministically
  (testing "With non-strict mode, route/decide retries on same cap then fail-opens to static capability."
    (let [calls (atom [])
          telemetry (atom {})
          routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          protocol {:intents {:route/decide {:in-schema :req/route
                                             :result/contract {:type :value
                                                               :contract/kind :route/decide}}
                              :text/respond {:in-schema :req/text}}
                    :result/types [:value :plan :error]
                    :retry/max-attempts 3
                    :policy/intents {:route/decide {:retry {:max-attempts 3}}}}
          runtime {:protocol protocol
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing}}
          payload {:proto 1
                   :trace {:id "t-7-retry-open"}
                   :task {:intent :text/respond}
                   :input {:prompt "hej"}}]
      (with-redefs [core/ollama-generate!
                    (fn [{:keys [intent]}]
                      (swap! calls conj intent)
                      (case intent
                        :route/decide {:response "not-a-route"}
                        :text/respond {:response "fallback-ok"}
                        {:response "unexpected"}))]
        (let [response (http/invoke-act runtime payload telemetry nil)
              snapshot (#'ferment.http/telemetry-snapshot telemetry)]
          (is (= 200 (:status response)))
          (is (= "fallback-ok" (get-in response [:body :result :out :text])))
          (is (= [:route/decide :route/decide :route/decide :text/respond] @calls))
          (is (= 1 (get-in snapshot [:act :routing :route/decide-hit])))
          (is (= 1 (get-in snapshot [:act :routing :route/fail-open])))
          (is (= 0 (get-in snapshot [:act :routing :route/fail-closed])))
          (is (= 0 (get-in snapshot [:act :routing :route/strict]))))))))

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
          response (with-redefs [core/call-capability
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
      (with-redefs [core/call-capability
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

(deftest telemetry-snapshot-exposes-kpi-and-failure-taxonomy
  (testing "Telemetry snapshot computes canonical KPI and normalized failure taxonomy."
    (let [runtime {:protocol {}
                   :resolver {}}
          telemetry (atom {})
          payload {:proto 1
                   :trace {:id "t-kpi-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :input {:prompt "hej"}}]
      (with-redefs [core/call-capability
                    (fn [_runtime _resolver _opts]
                      {:result {:type :value
                                :out {:text "ok"}
                                :plan/run {:telemetry {:calls/total 4
                                                       :calls/succeeded 2
                                                       :calls/failed 2
                                                       :calls/retries 2
                                                       :calls/fallback-hops 1
                                                       :calls/failure-types {:schema/invalid 1
                                                                             :runtime/invoke-failed 1}
                                                       :quality/must-failed 1
                                                       :quality/judge-used 2
                                                       :quality/judge-pass 1
                                                       :quality/judge-fail 1}}}})]
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil)))))
      (let [snapshot (#'ferment.http/telemetry-snapshot telemetry)]
        (is (= :workflow (get-in snapshot [:kpi :parse-rate :source])))
        (is (= 0.75 (get-in snapshot [:kpi :parse-rate :value])))
        (is (= 0.5 (get-in snapshot [:kpi :retry-rate :value])))
        (is (= 0.25 (get-in snapshot [:kpi :fallback-rate :value])))
        (is (= 0.25 (get-in snapshot [:kpi :must-failed-rate :value])))
        (is (= 1 (get-in snapshot [:kpi :must-failed-rate :must-failed])))
        (is (= 0.5 (get-in snapshot [:kpi :judge-pass-rate :value])))
        (is (= 1 (get-in snapshot [:kpi :failure-taxonomy :by-type :schema/invalid])))
        (is (= 1 (get-in snapshot [:kpi :failure-taxonomy :by-type :runtime/invoke-failed])))
        (is (= 1 (get-in snapshot [:kpi :failure-taxonomy :by-domain :schema])))
        (is (= 1 (get-in snapshot [:kpi :failure-taxonomy :by-domain :runtime])))))))

(deftest telemetry-snapshot-includes-must-failed-from-error-details
  (testing "Strict fail-closed errors contribute workflow must-failed telemetry."
    (let [routing {:intent->cap {:route/decide :llm/meta
                                 :text/respond :llm/voice}}
          runtime {:protocol {}
                   :resolver {:routing routing}
                   :router {:policy :meta-decider
                            :routing routing
                            :defaults {:meta? true
                                       :on-error :fail-open}}}
          telemetry (atom {})
          payload {:proto 1
                   :trace {:id "t-kpi-must-1"}
                   :task {:intent :text/respond}
                   :routing {:strict? true}
                   :input {:prompt "hej"}}]
      (with-redefs [core/call-capability
                    (fn [_runtime _resolver opts]
                      (if (= :route/decide (:intent opts))
                        (throw (ex-info
                                "Call node failed quality/dispatch policy"
                                {:error :runtime/invoke-failed
                                 :failure/type :eval/must-failed
                                 :node {:op :call
                                        :intent :text/respond
                                        :cap/id :llm/voice
                                        :as :voice}
                                 :outcome {:ok? false
                                           :cap/id :llm/voice
                                           :attempt 2
                                           :failure/type :eval/must-failed
                                           :failure/recover? true
                                           :done/eval {:ok? false
                                                       :score 1.0
                                                       :score-min 0.85
                                                       :must-failed [:no-list-expansion]
                                                       :should-failed []}}}))
                        {:result {:type :value
                                  :out {:text "should-not-happen"}}}))]
        (is (= 502 (:status (http/invoke-act runtime payload telemetry nil)))))
      (let [snapshot (#'ferment.http/telemetry-snapshot telemetry)]
        (is (= 1 (get-in snapshot [:workflow :quality/must-failed])))
        (is (= 1 (get-in snapshot [:workflow :calls/total])))
        (is (= 1 (get-in snapshot [:workflow :calls/failed])))
        (is (= 1 (get-in snapshot [:workflow :calls/failure-types :eval/must-failed])))
        (is (= 1.0 (get-in snapshot [:kpi :must-failed-rate :value])))))))

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
          response (with-redefs [core/call-capability
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

(deftest invoke-act-response-cache-hit-and-telemetry
  (testing "When response cache is enabled, identical requests hit cache and update cache telemetry."
    (let [calls (atom 0)
          runtime {:protocol {}
                   :resolver {}
                   :response-cache {:enabled? true
                                    :ttl-ms 120000
                                    :max-size 32
                                    :state (atom {:entries {}
                                                  :order []})}}
          telemetry (atom {})
          payload {:proto 1
                   :trace {:id "cache-hit-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :session/id "session/cache-hit-1"
                   :input {:prompt "hej"}}]
      (with-redefs [core/call-capability
                    (fn [_runtime _resolver _opts]
                      (swap! calls inc)
                      {:result {:type :value
                                :out {:text "ok"}}})]
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil))))
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil)))))
      (is (= 1 @calls))
      (let [snapshot (#'ferment.http/telemetry-snapshot telemetry)]
        (is (= 2 (get-in snapshot [:act :cache :lookups])))
        (is (= 1 (get-in snapshot [:act :cache :hits])))
        (is (= 1 (get-in snapshot [:act :cache :misses])))
        (is (= 1 (get-in snapshot [:act :cache :stores])))
        (is (= 0.5 (get-in snapshot [:kpi :cache-hit-rate :value])))))))

(deftest session-mutation-invalidates-act-response-cache
  (testing "Session mutation actions invalidate cached /v1/act responses for the same session id."
    (let [calls (atom 0)
          session-store (atom {})
          runtime {:protocol {}
                   :resolver {}
                   :session {:put-var! (fn [sid k v _opts]
                                         (swap! session-store assoc-in [sid k] v)
                                         true)}
                   :response-cache {:enabled? true
                                    :ttl-ms 120000
                                    :max-size 32
                                    :state (atom {:entries {}
                                                  :order []})}}
          telemetry (atom {})
          payload {:proto 1
                   :trace {:id "cache-inv-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice}
                   :session/id "session/cache-inv-1"
                   :input {:prompt "hej"}}]
      (with-redefs [core/call-capability
                    (fn [_runtime _resolver _opts]
                      (swap! calls inc)
                      {:result {:type :value
                                :out {:text "ok"}}})]
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil))))
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil))))
        (is (= 1 @calls))
        (is (= 200 (:status (#'ferment.http/session-action-response
                             runtime
                             {:action :session/put-var
                              :session/id "session/cache-inv-1"
                              :key :session/context-summary
                              :value "new-context"}
                             telemetry))))
        (is (= 200 (:status (http/invoke-act runtime payload telemetry nil)))))
      (is (= 2 @calls))
      (let [snapshot (#'ferment.http/telemetry-snapshot telemetry)]
        (is (pos? (long (or (get-in snapshot [:act :cache :invalidations]) 0))))
        (is (>= (long (or (get-in snapshot [:kpi :cache-hit-rate :lookups]) 0)) 3))))))

(deftest telemetry-snapshot-exposes-lifecycle-events
  (testing "Diagnostics snapshot includes lifecycle telemetry branch."
    (telemetry/clear-lifecycle!)
    (telemetry/record-lifecycle! :app :start {:profile :test})
    (telemetry/record-lifecycle! :http :error {:error "boom"})
    (let [snapshot (#'ferment.http/telemetry-snapshot (atom {}))]
      (is (= 2 (get-in snapshot [:lifecycle :total])))
      (is (= 1 (get-in snapshot [:lifecycle :errors])))
      (is (= 1 (get-in snapshot [:lifecycle :components :app :start])))
      (is (= 1 (get-in snapshot [:lifecycle :components :http :error]))))
    (telemetry/clear-lifecycle!)))
