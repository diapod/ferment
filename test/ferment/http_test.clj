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
