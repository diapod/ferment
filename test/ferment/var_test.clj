(ns

    ^{:doc    "ferment, var tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.var-test

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.edn                     :as             edn]
            [clojure.java.io                 :as              io]
            [clojure.test                    :refer [deftest
                                                     is
                                                     testing]]
            [clojure.spec.alpha              :as               s]
            [clojure.spec.gen.alpha          :as             gen]
            [cheshire.core                   :as            json]
            [orchestra.spec.test             :as              st]
            [ferment.caps                    :as            caps]
            [ferment.core                    :as            core]
            [ferment.http                    :as           fhttp]
            [ferment.model                   :as           model]
            [ferment.session                 :as         session]
            [ferment.session.store           :as   session-store]
            [ferment.system                  :as          system]
            [ferment.resolver                :as        resolver]
            [io.randomseed.utils.bot         :as             bot]
            [io.randomseed.utils.bus         :as             bus]
            [ferment                       :refer         :all]
            [expound.alpha                   :as         expound]))

(s/check-asserts true)

(defn- read-edn-with-integrant-readers
  [path]
  (edn/read-string {:readers {'ref identity 'refset identity}}
                   (slurp path)))

(defn- free-port
  []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defn- http-post-json
  [url payload]
  (let [conn
        (doto (.openConnection (java.net.URL. url))
          (.setRequestMethod "POST")
          (.setDoOutput true)
          (.setConnectTimeout 2000)
          (.setReadTimeout 5000)
          (.setRequestProperty "Content-Type" "application/json"))]
    (with-open [w (io/writer (.getOutputStream conn) :encoding "UTF-8")]
      (.write w (json/generate-string payload)))
    (let [status (.getResponseCode conn)
          stream (if (<= 200 status 399)
                   (.getInputStream conn)
                   (.getErrorStream conn))
          body (if stream
                 (with-open [in stream]
                   (slurp in :encoding "UTF-8"))
                 "")]
      {:status status
       :body body})))

(defn- http-post-edn
  [url payload]
  (let [conn
        (doto (.openConnection (java.net.URL. url))
          (.setRequestMethod "POST")
          (.setDoOutput true)
          (.setConnectTimeout 2000)
          (.setReadTimeout 5000)
          (.setRequestProperty "Content-Type" "application/edn"))]
    (with-open [w (io/writer (.getOutputStream conn) :encoding "UTF-8")]
      (.write w (pr-str payload)))
    (let [status (.getResponseCode conn)
          stream (if (<= 200 status 399)
                   (.getInputStream conn)
                   (.getErrorStream conn))
          body (if stream
                 (with-open [in stream]
                   (slurp in :encoding "UTF-8"))
                 "")]
      {:status status
       :body body})))

(defn- http-get
  [url]
  (let [conn
        (doto (.openConnection (java.net.URL. url))
          (.setRequestMethod "GET")
          (.setConnectTimeout 2000)
          (.setReadTimeout 5000))
        status (.getResponseCode conn)
        stream (if (<= 200 status 399)
                 (.getInputStream conn)
                 (.getErrorStream conn))
        body (if stream
               (with-open [in stream]
                 (slurp in :encoding "UTF-8"))
               "")]
    {:status status
     :body body}))

(deftest capabilities-config-is-flattened
  (testing "Each capability is a separate key, has contract metadata, and aggregate still exists."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/capabilities.edn")
          cap-keys #{:ferment.caps.registry/llm-voice
                     :ferment.caps.registry/llm-code
                     :ferment.caps.registry/llm-solver
                     :ferment.caps.registry/llm-meta
                     :ferment.caps.registry/llm-judge
                     :ferment.caps.registry/llm-mock}
          refs (get cfg :ferment.caps/registry)]
      (is (every? #(contains? cfg %) cap-keys))
      (is (every? (fn [k]
                    (let [cap (get cfg k)]
                      (and (contains? cap :cap/intents)
                           (contains? cap :cap/can-produce)
                           (contains? cap :cap/effects-allowed))))
                  cap-keys))
      (is (= 6 (count refs)))
      (is (= cap-keys (set refs))))))

(deftest resolver-config-references-flat-capabilities
  (testing "Resolver has a list of capability refs and derives :caps/by-id index."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/resolver.edn")
          default (get cfg :ferment.resolver/default)
          caps [{:cap/id :llm/voice :x 1}
                {:cap/id :llm/code :x 2}]
          initialized (resolver/init-resolver :ferment.resolver/default
                                              {:caps caps})]
      (is (map? default))
      (is (= #{:ferment.caps.registry/llm-voice
               :ferment.caps.registry/llm-code
               :ferment.caps.registry/llm-solver
               :ferment.caps.registry/llm-meta
               :ferment.caps.registry/llm-judge
               :ferment.caps.registry/llm-mock}
             (set (:caps default))))
      (is (= 2 (count (:caps/by-id initialized))))
      (is (= {:cap/id :llm/code :x 2}
             (get-in initialized [:caps/by-id :llm/code]))))))

(deftest caps-entry-hooks-normalize-capability-metadata
  (testing "Entry hooks normalize capability contract metadata."
    (let [entry {:cap/id :llm/meta
                 :dispatch/tag :meta
                 :cap/intents [:route/decide]
                 :cap/can-produce :plan}
          initialized (caps/init-capability-value
                       :ferment.caps.registry/llm-meta
                       entry)]
      (is (= entry
             (caps/preconfigure-capability-value
              :ferment.caps.registry/llm-meta
              entry)))
      (is (= #{:route/decide} (:cap/intents initialized)))
      (is (= #{:plan} (:cap/can-produce initialized)))
      (is (= #{:none} (:cap/effects-allowed initialized))))))

(deftest caps-entry-hooks-fail-fast-on-missing-required-metadata
  (testing "Capability init fails fast when required metadata keys are missing."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Capability metadata is incomplete."
         (caps/init-capability-value
          :ferment.caps.registry/llm-meta
          {:cap/id :llm/meta
           :dispatch/tag :meta
           :cap/intents #{:route/decide}})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Capability metadata is incomplete."
         (caps/init-capability-value
          :ferment.caps.registry/llm-meta
          {:cap/id :llm/meta
           :dispatch/tag :meta
           :cap/can-produce #{:plan}})))))

(deftest runtime-config-contains-core-runtime-branch
  (testing "Runtime branch has refs to resolver/protocol, session, and models aggregate."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/runtime.edn")
          runtime (get cfg :ferment.runtime/default)]
      (is (map? runtime))
      (is (= :ferment.resolver/default (:resolver runtime)))
      (is (= :ferment.protocol/default (:protocol runtime)))
      (is (= :ferment.session/default (:session runtime)))
      (is (= :ferment/models
             (:models runtime))))))

(deftest http-config-references-models-aggregate
  (testing "HTTP branch has :ferment.http/default key and reference to :ferment/models."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/http.edn")
          http (get cfg :ferment.http/default)]
      (is (map? http))
      (is (= "127.0.0.1" (:host http)))
      (is (= 12002 (:port http)))
      (is (= :ferment/models (:models http))))))

(deftest http-route-builder-collects-enabled-runtime-endpoints
  (testing "HTTP endpoints are built only for runtimes with :http {:enabled? true ...}."
    (let [worker-a {:worker-id :a}
          worker-b {:worker-id :b}
          routes (fhttp/model-http-routes
                  {:ferment.model/solver {:runtime {:id :ferment.model.runtime/solver
                                                    :worker worker-a
                                                    :config {:http {:enabled? true
                                                                    :endpoint "solver/responses"}}}}
                   :ferment.model/meta   {:runtime {:id :ferment.model.runtime/meta
                                                    :worker worker-b
                                                    :config {:http {:enabled? true
                                                                    :endpoint "/meta/responses"}}}}
                   :ferment.model/voice  {:runtime {:id :ferment.model.runtime/voice
                                                    :worker worker-b
                                                    :config {:http {:enabled? false
                                                                    :endpoint "voice/responses"}}}}})]
      (is (= #{"/solver/responses" "/meta/responses"} (set (keys routes))))
      (is (= :ferment.model/solver (get-in routes ["/solver/responses" :model])))
      (is (= :ferment.model.runtime/meta (get-in routes ["/meta/responses" :worker-id]))))))

(deftest http-v1-act-dispatches-through-core-contract-flow
  (testing "/v1/act validates request and calls core/invoke-capability! with cap-id from resolver."
    (let [captured (atom nil)
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}}
                              :result/types [:value]}
                   :resolver {:routing {:intent->cap {:problem/solve :llm/solver}}}}
          request {:proto 1
                   :trace {:id "trace-1" :turn 7}
                   :task {:intent "problem/solve"}
                   :input {:prompt "diag"}
                   :budget {:max-roundtrips 2
                            :temperature 0.0}
                   :session/id "s-1"}]
      (with-redefs [core/execute-capability!
                    (fn [rt resolver opts]
                      (reset! captured {:runtime rt :resolver resolver :opts opts})
                      {:proto 1
                       :trace (:trace opts)
                       :result {:type :value
                                :out {:text "OK"}}})]
        (let [resp (fhttp/invoke-act runtime request)]
          (is (= 200 (:status resp)))
          (is (= (:resolver runtime) (:resolver @captured)))
          (is (= :problem/solve (get-in @captured [:opts :intent])))
          (is (= :llm/solver (get-in @captured [:opts :cap-id])))
          (is (= :solver (get-in @captured [:opts :role])))
          (is (= 2 (get-in @captured [:opts :max-attempts])))
          (is (= "s-1" (get-in @captured [:opts :session-id])))
          (is (= {:type :value
                  :out {:text "OK"}}
                 (get-in resp [:body :result]))))))))

(deftest http-v1-act-reports-validation-and-routing-errors
  (testing "/v1/act returns contract errors and missing routing as envelope error."
    (let [runtime {:protocol {:intents {:code/patch {:in-schema :req/code}}
                              :result/types [:value]}
                   :resolver {:routing {:intent->cap {}}}}
          invalid-resp (fhttp/invoke-act runtime {:proto 1
                                                  :trace {:id "bad"}})
          unsupported-resp (fhttp/invoke-act runtime {:proto 1
                                                      :trace {:id "trace-2"}
                                                      :task {:intent :code/patch}
                                                      :input {:prompt "patch"}})]
      (is (= 400 (:status invalid-resp)))
      (is (= :input/invalid
             (get-in invalid-resp [:body :error :type])))
      (is (= 422 (:status unsupported-resp)))
      (is (= :unsupported/intent
             (get-in unsupported-resp [:body :error :type]))))))

(deftest http-v1-act-includes-session-metadata-when-available
  (testing "/v1/act returns session metadata when runtime has attached session service."
    (let [runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}}
                              :result/types [:value]}
                   :resolver {:routing {:intent->cap {:problem/solve :llm/solver}}}
                   :session {:get! (fn [sid]
                                     {:session/id sid
                                      :session/version 7
                                      :session/state :hot
                                      :session/frozen? false})}}
          request {:proto 1
                   :trace {:id "trace-sess-1"}
                   :task {:intent :problem/solve}
                   :input {:prompt "diag"}
                   :session/id "s-http-1"}]
      (with-redefs [core/execute-capability!
                    (fn [_rt _resolver opts]
                      {:proto 1
                       :trace (:trace opts)
                       :result {:type :value
                                :out {:text "OK"}}})]
        (let [resp (fhttp/invoke-act runtime request)]
          (is (= 200 (:status resp)))
          (is (= "s-http-1" (get-in resp [:body :session/id])))
          (is (= 7 (get-in resp [:body :session/version])))
          (is (= :hot (get-in resp [:body :session/state])))
          (is (= false (get-in resp [:body :session/frozen?]))))))))

(deftest http-v1-act-roundtrip-over-network
  (testing "Endpoint /v1/act works over real HTTP and returns canonical envelope."
    (let [port (free-port)
          called (atom nil)
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}}
                              :result/types [:value]}
                   :resolver {:routing {:intent->cap {:problem/solve :llm/solver}}}}
          server-state (fhttp/init-http
                        :ferment.http/default
                        {:host "127.0.0.1"
                         :port port
                         :runtime runtime
                         :models {}})
          url (str "http://127.0.0.1:" port "/v1/act")]
      (try
        (with-redefs [core/execute-capability!
                      (fn [rt resolver opts]
                        (reset! called {:runtime rt :resolver resolver :opts opts})
                        {:proto 1
                         :trace (:trace opts)
                         :result {:type :value
                                  :out {:text "E2E-OK"}}})]
          (let [ok-resp (http-post-json
                         url
                         {:proto 1
                          :trace {:id "http-trace-1"}
                          :task {:intent "problem/solve"}
                          :input {:prompt "diag e2e"}
                          :session/id "http-s-1"})
                ok-body (json/parse-string (:body ok-resp) true)
                bad-resp (http-post-json
                          url
                          {:proto 1
                           :trace {:id "http-trace-bad"}})
                bad-body (json/parse-string (:body bad-resp) true)]
            (is (= 200 (:status ok-resp)))
            (is (= :problem/solve (get-in @called [:opts :intent])))
            (is (= (:resolver runtime) (:resolver @called)))
            (is (= :llm/solver (get-in @called [:opts :cap-id])))
            (is (= "E2E-OK" (get-in ok-body [:result :out :text])))
            (is (= "http-trace-1" (get-in ok-body [:trace :id])))
            (is (= 400 (:status bad-resp)))
            (is (= "input/invalid" (get-in bad-body [:error :type])))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-act-collects-telemetry-and-exposes-diag-endpoint
  (testing "HTTP bridge aggregates telemetry for /v1/act and exposes it via /diag/telemetry."
    (let [port (free-port)
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}}
                              :result/types [:value]}
                   :resolver {:routing {:intent->cap {:problem/solve :llm/solver}}}}
          server-state (fhttp/init-http
                        :ferment.http/default
                        {:host "127.0.0.1"
                         :port port
                         :runtime runtime
                         :models {}})
          act-url (str "http://127.0.0.1:" port "/v1/act")
          diag-url (str "http://127.0.0.1:" port "/diag/telemetry")]
      (try
        (with-redefs [core/execute-capability!
                      (fn [_rt _resolver opts]
                        {:proto 1
                         :trace (:trace opts)
                         :result {:type :value
                                  :out {:text "OK"}
                                  :plan/run {:telemetry {:calls/total 1
                                                         :calls/succeeded 1}}}})]
          (let [ok-resp (http-post-json act-url {:proto 1
                                                 :trace {:id "telemetry-ok"}
                                                 :task {:intent :problem/solve}
                                                 :input {:prompt "diag"}})
                bad-resp (http-post-json act-url {:proto 1
                                                  :trace {:id "telemetry-bad"}})
                diag-resp (http-get diag-url)
                diag-body (json/parse-string (:body diag-resp) true)
                status-map (get-in diag-body [:telemetry :act :status])]
            (is (= 200 (:status ok-resp)))
            (is (= 400 (:status bad-resp)))
            (is (= 200 (:status diag-resp)))
            (is (= 2 (get-in diag-body [:telemetry :act :requests])))
            (is (= 1 (get-in diag-body [:telemetry :act :ok])))
            (is (= 1 (get-in diag-body [:telemetry :act :errors])))
            (is (= 1 (or (get status-map 200)
                         (get status-map "200")
                         (get status-map :200))))
            (is (= 1 (or (get status-map 400)
                         (get status-map "400")
                         (get status-map :400))))
            (is (= 1 (get-in diag-body [:telemetry :workflow :calls/total])))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-session-bridge-supports-runtime-and-store-actions
  (testing "Endpoint /v1/session handles worker/session bridge actions."
    (let [port (free-port)
          runtime {:session {:open! (fn [sid _opts]
                                      {:session/id sid
                                       :session/version 1
                                       :session/state :hot
                                       :session/frozen? false})
                             :get! (fn [sid]
                                     {:session/id sid
                                      :session/version 2
                                      :session/state :warm
                                      :session/frozen? true})}
                   :models {}}
          server-state (fhttp/init-http
                        :ferment.http/default
                        {:host "127.0.0.1"
                         :port port
                         :runtime runtime
                         :models {}})
          url (str "http://127.0.0.1:" port "/v1/session")]
      (try
        (with-redefs [model/session-workers-state
                      (fn [_runtime]
                        {:ferment.model/meta {"s-1" {:running? true}}})
                      model/thaw-session-worker!
                      (fn [_runtime model-id sid]
                        {:ok? true :model model-id :session/id sid :worker-id :w-1})
                      model/freeze-session-worker!
                      (fn [_runtime model-id sid]
                        {:ok? true :model model-id :session/id sid})]
          (let [state-resp (http-post-json url {:action "state"})
                state-body (json/parse-string (:body state-resp) true)
                thaw-resp  (http-post-json url {:action "worker/thaw"
                                                :model "meta"
                                                :session/id "s-1"})
                thaw-body  (json/parse-string (:body thaw-resp) true)
                get-resp   (http-post-json url {:action "session/get"
                                                :session/id "s-1"})
                get-body   (json/parse-string (:body get-resp) true)]
            (is (= 200 (:status state-resp)))
            (is (= true (get state-body :ok?)))
            (is (= 200 (:status thaw-resp)))
            (is (= true (get thaw-body :ok?)))
            (is (= "s-1" (get thaw-body :session/id)))
            (is (= 200 (:status get-resp)))
            (is (= "s-1" (get-in get-body [:session :session/id])))
            (is (= 2 (get-in get-body [:session :session/version])))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-act-plan-result-is-materialized-to-value
  (testing "/v1/act returns final :value when capability returns :plan."
    (let [calls (atom [])
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}
                                        :text/respond  {:in-schema :req/text}}
                              :result/types [:value :plan]}
                   :resolver {:routing {:intent->cap {:problem/solve :llm/solver
                                                      :text/respond  :llm/voice}}}}
          request {:proto 1
                   :trace {:id "plan-trace-1"}
                   :task {:intent "problem/solve"}
                   :input {:prompt "diag plan"}}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (swap! calls conj opts)
                      (if (= 1 (count @calls))
                        {:proto 1
                         :trace (:trace opts)
                         :result {:type :plan
                                  :plan {:nodes [{:op :call
                                                  :intent :text/respond
                                                  :input {:prompt {:slot/id :summary}}
                                                  :as :answer}
                                                 {:op :emit
                                                  :input {:slot/id [:answer :out]}}]}
                                  :bindings {:summary "PLAN->VOICE"}}}
                        {:proto 1
                         :trace (:trace opts)
                         :result {:type :value
                                  :out {:text "VOICE:PLAN->VOICE"}}}))]
        (let [resp (fhttp/invoke-act runtime request)]
          (is (= 200 (:status resp)))
          (is (= 2 (count @calls)))
          (is (= :problem/solve (get-in @calls [0 :intent])))
          (is (= :text/respond (get-in @calls [1 :intent])))
          (is (= :value (get-in resp [:body :result :type])))
          (is (= {:text "VOICE:PLAN->VOICE"}
                 (get-in resp [:body :result :out]))))))))

(deftest http-v1-act-plan-fallbacks-on-eval-low-score
  (testing "/v1/act performs plan fallback when first candidate returns :eval/low-score."
    (let [calls (atom [])
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}
                                        :text/respond  {:in-schema :req/text}}
                              :result/types [:value :plan :error]}
                   :resolver {:routing {:intent->cap {:problem/solve :llm/solver
                                                      :text/respond  :llm/solver}
                                        :switch-on #{:eval/low-score}
                                        :retry {:same-cap-max 0
                                                :fallback-max 1}}}}
          request {:proto 1
                   :trace {:id "fallback-trace-1"}
                   :task {:intent "problem/solve"}
                   :input {:prompt "diag fallback"}
                   :done {:must #{:schema-valid}
                          :should #{:tests-pass}
                          :score-min 0.8}}]
      (with-redefs [core/invoke-capability!
                    (fn [_runtime opts]
                      (swap! calls conj opts)
                      (let [n (count @calls)
                            cap-id (:cap-id opts)]
                        (cond
                          (= n 1)
                          {:proto 1
                           :trace (:trace opts)
                           :result {:type :plan
                                    :plan {:nodes [{:op :call
                                                    :intent :text/respond
                                                    :dispatch {:candidates [:llm/solver :llm/mock]
                                                               :switch-on #{:eval/low-score}
                                                               :retry {:same-cap-max 0
                                                                       :fallback-max 1}}
                                                    :done {:must #{:schema-valid}
                                                           :score-min 0.0}
                                                    :input {:prompt "route me"}
                                                    :as :answer}
                                                   {:op :emit
                                                    :input {:slot/id [:answer :out]}}]}}}

                          (= cap-id :llm/solver)
                          {:proto 1
                           :trace (:trace opts)
                           :error {:type :eval/low-score
                                   :retryable? true}}

                          (= cap-id :llm/mock)
                          {:proto 1
                           :trace (:trace opts)
                           :result {:type :value
                                    :out {:text "FALLBACK-OK"}}}

                          :else
                          {:proto 1
                           :trace (:trace opts)
                           :result {:type :value
                                    :out {:text "UNEXPECTED"}}})))]
        (let [resp (fhttp/invoke-act runtime request)
              cap-seq (mapv :cap-id @calls)]
          (is (= 200 (:status resp)))
          (is (= [:llm/solver :llm/solver :llm/mock] cap-seq))
          (is (= :value (get-in resp [:body :result :type])))
          (is (= {:text "FALLBACK-OK"}
                 (get-in resp [:body :result :out]))))))))

(deftest models-config-defines-selector-values-in-edn
  (testing "Model selectors are kept in models.edn."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/models.edn")]
      (is (= "default" (get cfg :ferment.model.defaults/profile)))
      (is (= "mlx-community/Qwen2.5-7B-Instruct-4bit"
             (get-in cfg [:ferment.model.id/solver :id/default])))
      (is (= "mlx-community/SmolLM3-3B-8bit"
             (get-in cfg [:ferment.model.id/solver :id/mini])))
      (is (= "speakleash/Bielik-4.5B-v3.0-Instruct-MLX-8bit"
             (get-in cfg [:ferment.model.id/voice :id/default])))
      (is (= {"HF_HOME" :ferment.env/hf.home
              "HF_HUB_CACHE" :ferment.env/hf.hub.cache
              "PATH" "${HOME}/.pyenv/shims:${PATH}"}
             (get-in cfg [:ferment.model.defaults/runtime :env])))
      (is (= :ferment.model.defaults/runtime
             (get-in cfg [:ferment.model.runtime/solver :defaults]))))))

(deftest model-runtime-config-wires-session-and-workers
  (testing "models.edn contains runtime defaults, runtime branches, and :ferment/models aggregate."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/models.edn")
          session (get cfg :ferment.model.defaults/bot-session)
          defaults (get cfg :ferment.model.defaults/runtime)
          solver-rt  (get cfg :ferment.model.runtime/solver)
          voice-rt   (get cfg :ferment.model.runtime/voice)
          models (get cfg :ferment/models)]
      (is (map? session))
      (is (= "ferment-model-runtime" (:sid session)))
      (is (= :ferment.model.defaults/bot-session (:session defaults)))
      (is (= :ferment.model.defaults/runtime (:defaults solver-rt)))
      (is (= :ferment.model.defaults/runtime (:defaults voice-rt)))
      (is (= :ferment.model/solver (get models :ferment.model/solver)))
      (is (= :ferment.model/voice  (get models :ferment.model/voice))))))

(deftest core-config-references-runtime-branches
  (testing "Core branch has refs to runtime/resolver/protocol/session."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/core.edn")
          corecfg (get cfg :ferment.core/default)]
      (is (map? corecfg))
      (is (= :ferment.runtime/default (:runtime corecfg)))
      (is (= :ferment.resolver/default (:resolver corecfg)))
      (is (= :ferment.protocol/default (:protocol corecfg)))
      (is (= :ferment.session/default (:session corecfg))))))

(deftest session-config-defines-store-manager-and-service
  (testing "Session config defines store/context/manager/service branches."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/session.edn")]
      (is (= :db
             (get-in cfg [:ferment.session.store/default :backend])))
      (is (= :ferment.db/main
             (get-in cfg [:ferment.session.store/default :db])))
      (is (= :sessions
             (get-in cfg [:ferment.session.store/default :sessions-table])))
      (is (= :session_vars
             (get-in cfg [:ferment.session.store/default :vars-table])))
      (is (= 1
             (get-in cfg [:ferment.session.context/default :context/version])))
      (is (= :ferment.session.store/default
             (get-in cfg [:ferment.session.manager/default :store])))
      (is (= :ferment.session.context/default
             (get-in cfg [:ferment.session.manager/default :context])))
      (is (= :ferment.session.manager/default
             (get-in cfg [:ferment.session/default :manager]))))))

(deftest session-manager-open-freeze-thaw-and-turns
  (testing "Session manager supports open/freeze/thaw and appending turns."
    (let [store   (session-store/init-store :ferment.session.store/default {:backend :memory})
          context (session/init-context :ferment.session.context/default {:context/version 1})
          manager (session/init-manager :ferment.session.manager/default
                                        {:store store
                                         :context context
                                         :max-hot-sessions 2
                                         :idle-ttl-ms 1000000})]
      (is (= :hot (:session/state (session/open-session! manager "s1" nil))))
      (is (= 1 (count (:session/turns
                       (session/append-session-turn! manager "s1" {:turn/role :user
                                                                   :turn/text "hej"})))))
      (is (true? (:session/frozen? (session/freeze-session! manager "s1" {:session/summary "sum"}))))
      (is (= :hot (:session/state (session/thaw-session! manager "s1" nil))))
      (is (= "s1" (:session/id (session/get-session manager "s1"))))
      (is (= 1 (count (session/list-sessions manager)))))))

(deftest core-service-initializer-builds-callable-map
  (testing "Core service init returns functions operating on runtime from config."
    (let [called-with (atom nil)
          runtime {:models {:ferment.model/solver {:id "solver-mini"}}}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (let [service (core/init-service :ferment.core/default
                                         {:runtime runtime
                                          :resolver {}
                                          :protocol {}})]
          (is (fn? (:respond! service)))
          (is (fn? (:solver! service)))
          (is (fn? (:voice! service)))
          (is (= "{\"intent\":\"ok\"}" ((:solver! service) "napisz patch")))
          (is (= "solver-mini" (:model @called-with))))))))

(deftest dev-config-overlays-prod-config
  (testing "Dev config loads prod and overrides profile."
    (let [cfg (system/read-configs nil
                                   "config/common/prod"
                                   "config/local/prod"
                                   "config/common/dev"
                                   "config/local/dev")]
      (is (contains? cfg :ferment.core/default))
      (is (contains? cfg :ferment.runtime/default))
      (is (contains? cfg :ferment.model/solver))
      (is (contains? cfg :ferment.model/voice))
      (is (contains? cfg :ferment.resolver/default))
      (is (contains? cfg :ferment.protocol/default))
      (is (contains? cfg :ferment.caps/registry))
      (is (contains? cfg :ferment.logging/unilog))
      (is (= :dev (get-in cfg [:ferment.app/properties :profile]))))))

(deftest test-config-overlays-prod-config
  (testing "Test config loads prod and overrides profile and database name."
    (let [cfg (system/read-configs nil
                                   "config/common/prod"
                                   "config/local/prod"
                                   "config/common/test"
                                   "config/local/test")]
      (is (contains? cfg :ferment.core/default))
      (is (= :test (get-in cfg [:ferment.app/properties :profile])))
      (is (= "ferment_test" (:ferment.env/db.main.name cfg))))))

(deftest dev-overlay-order-beats-local-prod
  (testing "Overlay order guarantees dev overrides prod (including local)."
    (let [prod-only (system/read-configs nil
                                         "config/test-merge/common/prod"
                                         "config/test-merge/local/prod")
          with-common-dev (system/read-configs nil
                                               "config/test-merge/common/prod"
                                               "config/test-merge/local/prod"
                                               "config/test-merge/common/dev")
          with-local-dev (system/read-configs nil
                                              "config/test-merge/common/prod"
                                              "config/test-merge/local/prod"
                                              "config/test-merge/common/dev"
                                              "config/test-merge/local/dev")]
      (is (= :local-prod (get prod-only :ferment.test/overlay)))
      (is (= :common-dev (get with-common-dev :ferment.test/overlay)))
      (is (= :local-dev (get with-local-dev :ferment.test/overlay))))))

(deftest profile-resource-dirs-include-profile-overlay-at-the-end
  (testing "Profile directories helper keeps local/<profile> as the last layer."
    (is (= ["translations/ferment"
            "config/common"
            "config/common/env"
            "config/common/prod"
            "config/local"
            "config/local/env"
            "config/local/prod"]
           (system/profile-resource-dirs :prod)))
    (is (= "config/common/test"
           (nth (system/profile-resource-dirs :test) 7)))
    (is (= "config/local/test"
           (peek (system/profile-resource-dirs :test))))))

(deftest model-selector-initialization-picks-profile-specific-value
  (testing "model/init-model-key chooses model according to profile."
    (is (= "solver-mini"
           (model/init-model-key
            :ferment.model.id/solver
            {:profile "mini" :id/default "solver-default" :id/mini "solver-mini"})))
    (is (= "voice-default"
           (model/init-model-key
            :ferment.model.id/voice
            {:profile "default" :id/default "voice-default" :id/mini "voice-mini"})))))

(deftest model-selector-runtime-functions-prefer-model-branch
  (testing "solver-id/voice-id read values from :ferment.model/* branch."
    (let [runtime {:models {:ferment.model/solver {:id "solver-from-model-branch"}
                            :ferment.model/voice  {:id "voice-from-model-branch"}
                            :ferment.model/coding {:id "coding-from-model-branch"}}}]
      (is (= "solver-from-model-branch" (model/solver-id runtime)))
      (is (= "voice-from-model-branch" (model/voice-id runtime)))
      (is (= "coding-from-model-branch"
             (model/coding-id {:models {:ferment.model/coding {:id "coding-from-model-branch"}}})))
      (is (= "mlx-community/Qwen2.5-7B-Instruct-4bit"
             (model/solver-id {:models {:ferment.model/coding {:id "coding-from-model-branch"}}}))))))

(deftest model-runtime-worker-lifecycle-uses-bot-start-stop
  (testing "Worker runtime uses bot/start and bot/stop through model.clj wrappers."
    (let [started (atom nil)
          stopped (atom nil)
          session {:sid "test-session"}]
      (with-redefs [model/start-bot-worker!
                    (fn [s cfg]
                      (reset! started {:session s :cfg cfg})
                      {:worker-id :mock-worker})
                    model/stop-bot-worker!
                    (fn [worker]
                      (reset! stopped worker)
                      true)]
        (let [state (model/init-runtime-worker
                     :ferment.model.runtime/solver
                     {:session session
                      :enabled? true
                      :name "solver runtime test"})]
          (is (= session (:session @started)))
          (is (= :ferment.model.runtime/solver
                 (get-in @started [:cfg :id])))
          (is (= :mock-worker (:worker-id (:worker state))))
          (is (true? (:enabled? state)))
          (is (nil? (model/stop-runtime-worker :ferment.model.runtime/solver state)))
          (is (= {:worker-id :mock-worker} @stopped)))))))

(deftest model-runtime-worker-default-invoke-function-uses-runtime-process
  (testing "Worker with :command gets default invoke-fn based on runtime process and responds via runtime-request-handler."
    (let [called (atom nil)]
      (with-redefs [model/invoke-runtime-process!
                    (fn [payload session worker-config]
                      (reset! called {:cfg worker-config
                                      :session session
                                      :payload payload})
                      {:text "meta-ok"})]
        (let [cfg (model/preconfigure-runtime-worker
                   :ferment.model.runtime/meta
                   {:command ["mlx_lm.chat" "--model" "meta-model"]})
              response (model/runtime-request-handler
                        {:stage :RUNNING}
                        {:id :ferment.model.runtime/meta}
                        {:body :invoke
                         :args [{:prompt "hej"}]}
                        [cfg])]
          (is (fn? (:invoke-fn cfg)))
          (is (= {:ok? true
                  :result {:text "meta-ok"}}
                 response))
          (is (= {:prompt "hej"} (:payload @called)))
          (is (= :RUNNING (get-in @called [:session :stage]))))))))

(deftest model-runtime-request-quit-writes-command-to-process-stdin
  (testing "Command :quit writes :cmd/quit to process stdin with trailing newline."
    (let [sink (java.io.ByteArrayOutputStream.)
          process (proxy [Process] []
                    (getOutputStream [] sink)
                    (getInputStream [] (java.io.ByteArrayInputStream. (byte-array 0)))
                    (getErrorStream [] (java.io.ByteArrayInputStream. (byte-array 0)))
                    (waitFor [] 0)
                    (exitValue [] 0)
                    (destroy [] nil)
                    (destroyForcibly [] this)
                    (isAlive [] true))
          response (model/runtime-request-handler
                    {:runtime/state {:process process}}
                    {:id :ferment.model.runtime/meta}
                    {:body :quit}
                    {:id :ferment.model.runtime/meta
                     :name "meta model runtime"
                     :cmd/quit "q"})]
      (is (= true (:ok? response)))
      (is (= "q\n" (.toString sink "UTF-8"))))))

(deftest model-runtime-request-runtime-returns-safe-snapshot
  (testing "Command :runtime returns operator snapshot without raw/secret fields."
    (let [response (model/runtime-request-handler
                    {:stage :RUNNING
                     :started-at "2026-02-16T15:00:00Z"
                     :runtime/error {:error :runtime-start-failed
                                     :class "java.io.IOException"
                                     :message "boom"
                                     :stacktrace "..."}
                     :runtime/state {:type :process
                                     :pid 1234
                                     :command ["mlx_lm.chat" "--model" "m"]
                                     :secret/token "hidden"}}
                    {:id :ferment.model.runtime/meta}
                    {:body :runtime}
                    {:id :ferment.model.runtime/meta
                     :name "meta model runtime"})]
      (is (= :ferment.model.runtime/meta (:worker/id response)))
      (is (= "meta model runtime" (:worker/name response)))
      (is (= :RUNNING (:stage response)))
      (is (= {:error :runtime-start-failed
              :class "java.io.IOException"
              :message "boom"}
             (:runtime/error response)))
      (is (= {:type :process
              :pid 1234
              :command ["mlx_lm.chat" "--model" "m"]}
             (:runtime/state response)))
      (is (not (contains? (:runtime/state response) :secret/token))))))

(deftest model-runtime-worker-run-sends-bot-responses
  (testing "runtime-worker-run! sends Outcome response back on bus channel."
    (let [sent (atom nil)]
      (with-redefs [model/start-command-process! (fn [_ _] nil)
                    bus/wait-for-request (fn [_] {:request :config})
                    bot/handle-request
                    (fn [_ _ _ wrk _ _]
                      {:response {:body {:ok true}
                                  :request {:id "req-1"}}
                       :data :QUIT
                       :wrk wrk})
                    bus/send-response
                    (fn [_wrk response]
                      (reset! sent response)
                      response)]
        (is (nil? (model/runtime-worker-run! {:id :worker}
                                             {:sid "sid"}
                                             {:name "test"})))
        (is (= {:body {:ok true}
                :request {:id "req-1"}}
               @sent))))))

(deftest stop-runtime-worker-sends-quit-before-stop
  (testing "stop-runtime-worker sends :quit before stopping worker when :cmd/quit is set."
    (let [calls (atom [])]
      (with-redefs [model/command-bot-worker!
                    (fn [worker cmd & _]
                      (swap! calls conj [:command worker cmd])
                      :ok)
                    model/stop-bot-worker!
                    (fn [worker]
                      (swap! calls conj [:stop worker])
                      true)]
        (is (nil? (model/stop-runtime-worker
                   :ferment.model.runtime/meta
                   {:worker :mock-worker
                    :config {:cmd/quit "q"}})))
        (is (= [[:command :mock-worker :quit]
                [:stop :mock-worker]]
               @calls))))))

(deftest model-runtime-aggregate-builds-workers-map
  (testing "Runtime aggregate keeps workers map and read helpers work."
    (let [runtime (model/init-model-runtime
                   :ferment.model/runtime
                   {:workers {:solver {:worker :solver-w}
                              :voice  {:worker :voice-w}}})]
      (is (= {:worker :solver-w}
             (model/runtime-worker-state runtime :solver)))
      (is (= :voice-w
             (model/runtime-worker runtime :voice))))))

(deftest model-diagnostic-invoke-uses-running-worker-from-system-map
  (testing "diagnostic-invoke! finds worker by model-id and sends :invoke with payload."
    (let [called (atom nil)
          system {:ferment.model/meta {:runtime {:worker :meta-worker}}}]
      (with-redefs [model/command-bot-worker!
                    (fn [worker cmd payload]
                      (reset! called {:worker worker :cmd cmd :payload payload})
                      {:ok? true :result {:text "meta-ok"}})]
        (is (= {:ok? true :result {:text "meta-ok"}}
               (model/diagnostic-invoke! system :meta {:prompt "hej"})))
        (is (= {:worker :meta-worker
                :cmd :invoke
                :payload {:prompt "hej"}}
               @called))))))

(deftest model-diagnostic-invoke-fails-when-worker-not-running
  (testing "diagnostic-invoke! returns readable exception when worker is unavailable."
    (let [ex (try
               (model/diagnostic-invoke! {:ferment.model/meta {:runtime {:enabled? true}}}
                                         :meta
                                         {:prompt "hej"})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo ex))
      (is (= :model/worker-not-found (:error (ex-data ex))))
      (is (= :ferment.model/meta (:model (ex-data ex)))))))

(deftest model-session-runtime-starts-on-demand-and-reuses-worker
  (testing "invoke-model! creates worker per (model, session), and subsequent calls reuse process."
    (let [starts (atom [])
          calls  (atom [])
          open-calls (atom [])
          runtime {:models {:ferment.model/meta
                            {:runtime {:config {:id :ferment.model.runtime/meta
                                                :name "meta runtime"
                                                :command ["mlx_lm.chat" "--model" "meta"]}}}}
                   :session {:open! (fn [sid opts]
                                      (swap! open-calls conj {:sid sid :opts opts})
                                      {:session/id sid})}
                   :ferment.model.session/workers (atom {})
                   :ferment.model.session/lock (Object.)
                   :ferment.model.session/enabled? true
                   :ferment.model.session/idle-ttl-ms 60000
                   :ferment.model.session/max-per-model 4}]
      (with-redefs [model/init-runtime-worker
                    (fn [_k cfg]
                      (let [worker (keyword (str "worker-" (inc (count @starts))))
                            state {:id (:id cfg)
                                   :worker worker
                                   :config cfg}]
                        (swap! starts conj state)
                        state))
                    model/command-bot-worker!
                    (fn [worker command payload]
                      (swap! calls conj {:worker worker :command command :payload payload})
                      {:ok? true
                       :result {:text (str "OK-" (name worker))}})]
        (is (= {:ok? true :result {:text "OK-worker-1"}}
               (model/invoke-model! runtime :meta {:prompt "hej"} {:session/id "s-1"})))
        (is (= {:ok? true :result {:text "OK-worker-1"}}
               (model/invoke-model! runtime :meta {:prompt "hej-2"} {:session/id "s-1"})))
        (is (= 1 (count @starts)))
        (is (= 2 (count @calls)))
        (is (= :worker-1 (:worker (first @calls))))
        (is (= :worker-1 (:worker (second @calls))))
        (is (<= 2 (count @open-calls)))
        (is (= "s-1" (:sid (first @open-calls))))
        (is (contains? (get (model/session-workers-state runtime) :ferment.model/meta) "s-1"))))))

(deftest model-session-runtime-expires-workers-by-ttl
  (testing "Session worker is stopped after TTL and restarted on next invoke."
    (let [starts (atom [])
          stops  (atom [])
          runtime {:models {:ferment.model/meta
                            {:runtime {:config {:id :ferment.model.runtime/meta
                                                :name "meta runtime"
                                                :command ["mlx_lm.chat" "--model" "meta"]}}}}
                   :session {:open! (fn [sid _opts] {:session/id sid})}
                   :ferment.model.session/workers (atom {})
                   :ferment.model.session/lock (Object.)
                   :ferment.model.session/enabled? true
                   :ferment.model.session/idle-ttl-ms 1
                   :ferment.model.session/max-per-model 4}]
      (with-redefs [model/init-runtime-worker
                    (fn [_k cfg]
                      (let [worker (keyword (str "worker-" (inc (count @starts))))
                            state {:id (:id cfg)
                                   :worker worker
                                   :config cfg}]
                        (swap! starts conj state)
                        state))
                    model/stop-runtime-worker
                    (fn [_k state]
                      (swap! stops conj (:worker state))
                      nil)
                    model/command-bot-worker!
                    (fn [_worker _command _payload]
                      {:ok? true :result {:text "OK"}})]
        (model/invoke-model! runtime :meta {:prompt "hej"} {:session/id "s-ttl"})
        (swap! (:ferment.model.session/workers runtime)
               assoc-in
               [:ferment.model/meta "s-ttl" :last-used-ms]
               0)
        (model/invoke-model! runtime :meta {:prompt "hej-2"} {:session/id "s-ttl"})
        (is (= 2 (count @starts)))
        (is (= 1 (count @stops)))
        (is (= :worker-1 (first @stops)))))))

(deftest model-session-runtime-freeze-and-thaw
  (testing "thaw-session-worker!/freeze-session-worker! manage worker and call session service."
    (let [starts (atom [])
          stops  (atom [])
          freezes (atom [])
          runtime {:models {:ferment.model/meta
                            {:runtime {:config {:id :ferment.model.runtime/meta
                                                :name "meta runtime"
                                                :command ["mlx_lm.chat" "--model" "meta"]}}}}
                   :session {:open! (fn [sid _opts] {:session/id sid})
                             :freeze! (fn [sid opts]
                                        (swap! freezes conj {:sid sid :opts opts})
                                        {:session/id sid :session/frozen? true})}
                   :ferment.model.session/workers (atom {})
                   :ferment.model.session/lock (Object.)
                   :ferment.model.session/enabled? true
                   :ferment.model.session/idle-ttl-ms 60000
                   :ferment.model.session/max-per-model 4}]
      (with-redefs [model/init-runtime-worker
                    (fn [_k cfg]
                      (let [worker (keyword (str "worker-" (inc (count @starts))))
                            state {:id (:id cfg)
                                   :worker worker
                                   :config cfg}]
                        (swap! starts conj state)
                        state))
                    model/stop-runtime-worker
                    (fn [_k state]
                      (swap! stops conj (:worker state))
                      nil)]
        (is (= {:ok? true
                :model :ferment.model/meta
                :session/id "s-freeze"
                :worker-id :ferment.model.runtime.session/meta--s-freeze}
               (model/thaw-session-worker! runtime :meta "s-freeze")))
        (is (= 1 (count @starts)))
        (is (= {:ok? true
                :model :ferment.model/meta
                :session/id "s-freeze"}
               (model/freeze-session-worker! runtime :meta "s-freeze")))
        (is (= 1 (count @stops)))
        (is (= 1 (count @freezes)))
        (is (= "s-freeze" (:sid (first @freezes))))
        (is (empty? (model/session-workers-state runtime)))))))
