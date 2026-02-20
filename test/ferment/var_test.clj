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
            [ferment.admin                   :as           admin]
            [ferment.caps                    :as            caps]
            [ferment.core                    :as            core]
            [ferment.http                    :as           fhttp]
            [ferment.model                   :as           model]
            [ferment.runtime                 :as        runtime]
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
  ([url payload]
   (http-post-json url payload nil))
  ([url payload headers]
   (let [conn
         (doto (.openConnection (java.net.URL. url))
           (.setRequestMethod "POST")
           (.setDoOutput true)
           (.setConnectTimeout 2000)
           (.setReadTimeout 5000)
           (.setRequestProperty "Content-Type" "application/json"))]
     (doseq [[k v] (or headers {})]
       (when (and (some? k) (some? v))
         (.setRequestProperty conn (str k) (str v))))
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
        :body body}))))

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

(deftest router-config-references-routing-policy
  (testing "Router config keeps routing/profiles/policy under dedicated branch."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/router.edn")
          router (get cfg :ferment.router/default)]
      (is (map? router))
      (is (= :ferment.caps/routing (:routing router)))
      (is (= :ferment.caps/profiles (:profiles router)))
      (is (= :quality-aware (:policy router))))))

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
  (testing "Runtime branch has refs to router/resolver/protocol/session/oplog and models aggregate."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/runtime.edn")
          runtime (get cfg :ferment.runtime/default)]
      (is (map? runtime))
      (is (= :ferment.router/default (:router runtime)))
      (is (= :ferment.resolver/default (:resolver runtime)))
      (is (= :ferment.protocol/default (:protocol runtime)))
      (is (= :ferment.session/default (:session runtime)))
      (is (= :ferment.logging/oplog (:oplog runtime)))
      (is (= :ferment/models
             (:models runtime))))))

(deftest runtime-branch-attaches-router-policy-to-resolver
  (testing "Runtime init injects router routing/policy into resolver for workflow dispatch."
    (let [state (runtime/init-runtime
                 :ferment.runtime/default
                 {:router {:routing {:intent->cap {:problem/solve :llm/solver}}
                           :profiles {:default [:llm/solver]}
                           :policy :quality-aware}
                  :resolver {:caps/by-id {:llm/solver {:cap/id :llm/solver}}}})]
      (is (= {:intent->cap {:problem/solve :llm/solver}}
             (get-in state [:resolver :routing])))
      (is (= {:default [:llm/solver]}
             (get-in state [:resolver :profiles])))
      (is (= :quality-aware
             (get-in state [:resolver :policy]))))))

(deftest runtime-branch-fails-on-router-capability-mismatch
  (testing "Runtime init fails fast when router references capability missing in resolver registry."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"unknown capability id"
         (runtime/init-runtime
          :ferment.runtime/default
          {:router {:routing {:intent->cap {:problem/solve :llm/missing}}}
           :resolver {:caps/by-id {:llm/solver {:cap/id :llm/solver}}}})))))

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
                   :router {:routing {:intent->cap {:problem/solve :llm/solver}}}
                   :resolver {}}
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
          (is (= {:intent->cap {:problem/solve :llm/solver}}
                 (get-in @captured [:resolver :routing])))
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
                   :router {:routing {:intent->cap {}}}
                   :resolver {}}
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
                   :router {:routing {:intent->cap {:problem/solve :llm/solver}}}
                   :resolver {}
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
                   :router {:routing {:intent->cap {:problem/solve :llm/solver}}}
                   :resolver {}}
          server-state (fhttp/init-http
                        :ferment.http/default
                        {:host "127.0.0.1"
                         :port port
                         :runtime runtime
                         :auth (:auth runtime)
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
            (is (= {:intent->cap {:problem/solve :llm/solver}}
                   (get-in @called [:resolver :routing])))
            (is (= :llm/solver (get-in @called [:opts :cap-id])))
            (is (= "E2E-OK" (get-in ok-body [:result :out :text])))
            (is (= "http-trace-1" (get-in ok-body [:trace :id])))
            (is (= 400 (:status bad-resp)))
            (is (= "input/invalid" (get-in bad-body [:error :type])))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-basic-auth-forwards-session-id-into-auth-flow
  (testing "authorize-request forwards session id (payload/header) and session service into auth-user/authenticate-password."
    (let [runtime {:session {:open! (fn [sid _opts] {:session/id sid})}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :operator}}
          auth-calls (atom [])]
      (with-redefs-fn
        {#'ferment.http/parse-basic-credentials
         (fn [_exchange]
           {:login "diag@example.com"
            :password "secret"})
         #'ferment.http/report-auth!
         (fn [& _] nil)
         #'ferment.auth.user/authenticate-password
         (fn [source login password account-type opts]
           (swap! auth-calls conj {:source source
                                   :login login
                                   :password password
                                   :account-type account-type
                                   :opts opts})
           {:ok? true
            :user {:user/id 91
                   :user/email login}})}
        (fn []
          (is (nil? (#'ferment.http/authorize-request runtime nil {:session/id "sid-payload"})))
          (with-redefs-fn
            {#'ferment.http/session-id-from-header
             (fn [_exchange] "sid-header")}
            (fn []
              (is (nil? (#'ferment.http/authorize-request runtime nil {})))))
          (is (= 2 (count @auth-calls)))
          (is (= :auth/source (:source (first @auth-calls))))
          (is (= :operator (:account-type (first @auth-calls))))
          (is (= "sid-payload" (get-in (first @auth-calls) [:opts :session/id])))
          (is (= "sid-header" (get-in (second @auth-calls) [:opts :session/id])))
          (is (= (:session runtime)
                 (get-in (first @auth-calls) [:opts :session/service]))))))))

(deftest http-basic-auth-can-be-rejected-by-role-policy
  (testing "authorize-request returns 403 when authenticated user has no required role."
    (let [runtime {:session {:open! (fn [sid _opts] {:session/id sid})}
                   :roles {:enabled? true
                           :authorize-default? false
                           :account-type->roles {:operator #{:role/operator}}
                           :operations {:http.v1/act {:any #{:role/admin}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :operator}}]
      (with-redefs-fn
        {#'ferment.http/parse-basic-credentials
         (fn [_exchange]
           {:login "diag@example.com"
            :password "secret"})
         #'ferment.http/report-auth!
         (fn [& _] nil)
         #'ferment.auth.user/authenticate-password
         (fn [_source login _password _account-type _opts]
           {:ok? true
            :user {:user/id 12
                   :user/email login
                   :user/account-type :operator}})}
        (fn []
          (let [response (#'ferment.http/authorize-request runtime nil {:session/id "sid-payload"} :http.v1/act)]
            (is (= 403 (:status response)))
            (is (= :auth/forbidden (get-in response [:body :error])))))))))

(deftest http-session-principal-allows-v1-act-without-basic-auth
  (testing "authenticate-request may resolve principal from session for /v1/act when session-principal mode is enabled."
    (let [opened*   (atom [])
          auth-calls (atom 0)
          now-ms    (System/currentTimeMillis)
          runtime   {:session {:get! (fn [_sid]
                                       {:session/id "sid-1"
                                        :session/version 3
                                        :session/state :hot
                                        :session/frozen? false
                                        :session/meta {:auth/principal {:user/id 51
                                                                        :user/email "session@example.com"
                                                                        :user/account-type :operator
                                                                        :user/roles [:role/operator]}
                                                       :auth/principal-refreshed-at (- now-ms 2000)}})
                               :open! (fn [sid opts]
                                        (swap! opened* conj {:sid sid :opts opts})
                                        {:session/id sid
                                         :session/version 4
                                         :session/state :hot
                                         :session/frozen? false})}
                     :roles {:enabled? true
                             :authorize-default? false
                             :operations {:http.v1/act {:any #{:role/operator}}}}
                     :auth {:enabled? true
                            :source :auth/source
                            :session-principal {:enabled? true
                                                :operations #{:http.v1/act}
                                                :ttl-ms 600000
                                                :refresh-ms 1}}}]
      (with-redefs-fn
        {#'ferment.http/parse-basic-credentials
         (fn [_exchange] nil)
         #'ferment.http/report-auth!
         (fn [& _] nil)
         #'ferment.auth.user/authenticate-password
         (fn [& _args]
           (swap! auth-calls inc)
           {:ok? false})}
        (fn []
          (let [authn (#'ferment.http/authenticate-request runtime nil {:session/id "sid-1"} :http.v1/act)]
            (is (:ok? authn))
            (is (= :http/session-principal (get-in authn [:auth :source])))
            (is (= 51 (get-in authn [:auth :user :user/id])))
            (is (= #{:role/operator}
                   (set (get-in authn [:auth :user :user/roles]))))
            (is (= "sid-1" (get-in authn [:auth :session :session/id])))
            (is (zero? @auth-calls))
            (is (= 1 (count @opened*)))
            (is (= 51 (get-in (first @opened*) [:opts :session/meta :auth/principal :user/id])))))))))

(deftest http-session-principal-respects-ttl
  (testing "authenticate-request rejects stale session principal when TTL is exceeded."
    (let [now-ms  (System/currentTimeMillis)
          runtime {:session {:get! (fn [_sid]
                                     {:session/id "sid-expired"
                                      :session/meta {:auth/principal {:user/id 71
                                                                      :user/email "expired@example.com"
                                                                      :user/account-type :operator
                                                                      :user/roles [:role/operator]}
                                                     :auth/principal-refreshed-at (- now-ms 10000)}})}
                   :roles {:enabled? true
                           :authorize-default? false
                           :operations {:http.v1/act {:any #{:role/operator}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :session-principal {:enabled? true
                                              :operations #{:http.v1/act}
                                              :ttl-ms 100
                                              :refresh-ms 1}}}]
      (with-redefs-fn
        {#'ferment.http/parse-basic-credentials
         (fn [_exchange] nil)
         #'ferment.http/report-auth!
         (fn [& _] nil)}
        (fn []
          (let [authn (#'ferment.http/authenticate-request runtime nil {:session/id "sid-expired"} :http.v1/act)]
            (is (false? (:ok? authn)))
            (is (= 401 (get-in authn [:response :status])))
            (is (= :auth/unauthorized
                   (get-in authn [:response :body :error])))))))))

(deftest http-v1-admin-rejects-operator-without-admin-role
  (testing "/v1/admin returns 403 when authenticated user lacks admin role."
    (let [port (free-port)
          runtime {:models {}
                   :roles {:enabled? true
                           :authorize-default? false
                           :account-type->roles {:operator #{:role/operator}}
                           :operations {:admin/create-user {:any #{:role/admin}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :operator}}
          server-state (fhttp/init-http :ferment.http/default
                                        {:host "127.0.0.1"
                                         :port port
                                         :runtime runtime})
          url (str "http://127.0.0.1:" port "/v1/admin")]
      (try
        (with-redefs-fn
          {#'ferment.http/parse-basic-credentials
           (fn [_exchange]
             {:login "diag@example.com"
              :password "secret"})
           #'ferment.http/report-auth!
           (fn [& _] nil)
           #'ferment.auth.user/authenticate-password
           (fn [_source login _password _account-type _opts]
             {:ok? true
              :user {:user/id 10
                     :user/email login
                     :user/account-type :operator}})}
          (fn []
            (let [resp (http-post-json url {:action "admin/create-user"
                                            :email "new@example.com"
                                            :password "secret"})
                  body (json/parse-string (:body resp) true)]
              (is (= 403 (:status resp)))
              (is (= "auth/forbidden" (:error body))))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-admin-allows-manager-to-create-user
  (testing "/v1/admin calls create-user when authenticated role policy allows operation."
    (let [port (free-port)
          called (atom nil)
          runtime {:models {}
                   :roles {:enabled? true
                           :authorize-default? false
                           :account-type->roles {:manager #{:role/admin}}
                           :operations {:admin/create-user {:any #{:role/admin}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :manager}}
          server-state (fhttp/init-http :ferment.http/default
                                        {:host "127.0.0.1"
                                         :port port
                                         :runtime runtime})
          url (str "http://127.0.0.1:" port "/v1/admin")]
      (try
        (with-redefs-fn
          {#'ferment.http/parse-basic-credentials
           (fn [_exchange]
             {:login "admin@example.com"
              :password "secret"})
           #'ferment.http/report-auth!
           (fn [& _] nil)
           #'ferment.auth.user/authenticate-password
           (fn [_source login _password _account-type _opts]
             {:ok? true
              :user {:user/id 11
                     :user/email login
                     :user/account-type :manager}})
           #'ferment.admin/create-user!
           (fn [email password account-type]
             (reset! called {:email email
                             :password password
                             :account-type account-type})
             {:ok? true
              :created? true
              :user {:user/id 77
                     :user/email email
                     :user/account-type (or account-type :user)}})}
          (fn []
            (let [resp (http-post-json url {:action :admin/create-user
                                            :email "new@example.com"
                                            :password "new-secret"
                                            :account-type :operator})
                  body (json/parse-string (:body resp) true)]
              (is (= 200 (:status resp)))
              (is (= true (:ok? body)))
              (is (= "admin/create-user" (:action body)))
              (is (= "new@example.com" (:email @called)))
              (is (= "new-secret" (:password @called)))
              (is (= :operator (:account-type @called))))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-admin-rejects-db-migration-without-role
  (testing "/v1/admin returns 403 for :admin/migrate-db when role policy denies operation."
    (let [port (free-port)
          runtime {:models {}
                   :roles {:enabled? true
                           :authorize-default? false
                           :account-type->roles {:manager #{:role/admin}}
                           :operations {:admin/create-user {:any #{:role/admin}}
                                        :admin/migrate-db  {:any #{:role/infra-admin}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :manager}}
          server-state (fhttp/init-http :ferment.http/default
                                        {:host "127.0.0.1"
                                         :port port
                                         :runtime runtime})
          url (str "http://127.0.0.1:" port "/v1/admin")]
      (try
        (with-redefs-fn
          {#'ferment.http/parse-basic-credentials
           (fn [_exchange]
             {:login "admin@example.com"
              :password "secret"})
           #'ferment.http/report-auth!
           (fn [& _] nil)
           #'ferment.auth.user/authenticate-password
           (fn [_source login _password _account-type _opts]
             {:ok? true
              :user {:user/id 13
                     :user/email login
                     :user/account-type :manager}})
           #'ferment.admin/migrate!
           (fn [& _]
             (throw (ex-info "Should not run migration on denied request." {})))}
          (fn []
            (let [resp (http-post-json url {:action "admin/migrate-db"})
                  body (json/parse-string (:body resp) true)]
              (is (= 403 (:status resp)))
              (is (= "auth/forbidden" (:error body))))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-admin-allows-db-migrate-and-rollback
  (testing "/v1/admin delegates migrate/rollback through admin wrappers when role policy allows operations."
    (let [port (free-port)
          migrated (atom [])
          rolled-back (atom [])
          runtime {:models {}
                   :roles {:enabled? true
                           :authorize-default? false
                           :account-type->roles {:manager #{:role/admin}}
                           :operations {:admin/migrate-db {:any #{:role/admin}}
                                        :admin/rollback-db {:any #{:role/admin}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :manager}}
          server-state (fhttp/init-http :ferment.http/default
                                        {:host "127.0.0.1"
                                         :port port
                                         :runtime runtime})
          url (str "http://127.0.0.1:" port "/v1/admin")]
      (try
        (with-redefs-fn
          {#'ferment.http/parse-basic-credentials
           (fn [_exchange]
             {:login "admin@example.com"
              :password "secret"})
           #'ferment.http/report-auth!
           (fn [& _] nil)
           #'ferment.auth.user/authenticate-password
           (fn [_source login _password _account-type _opts]
             {:ok? true
              :user {:user/id 14
                     :user/email login
                     :user/account-type :manager}})
           #'ferment.admin/migrate!
           (fn
             ([] (swap! migrated conj :no-opts) {:ok? true :migrated? true})
             ([opts] (swap! migrated conj opts) {:ok? true :migrated? true}))
           #'ferment.admin/rollback!
           (fn
             ([] (swap! rolled-back conj :no-opts) {:ok? true :rolled-back? true})
             ([opts] (swap! rolled-back conj [:opts opts]) {:ok? true :rolled-back? true})
             ([opts amount-or-id] (swap! rolled-back conj [:opts+amount opts amount-or-id]) {:ok? true :rolled-back? true}))}
          (fn []
            (let [migrate-resp (http-post-json url {:action "admin/migrate-db"
                                                    :opts {:migrators-key :ferment.db/migrators}})
                  migrate-body (json/parse-string (:body migrate-resp) true)
                  rollback-resp (http-post-json url {:action "admin/rollback-db"
                                                     :amount 2})
                  rollback-body (json/parse-string (:body rollback-resp) true)]
              (is (= 200 (:status migrate-resp)))
              (is (= true (:ok? migrate-body)))
              (is (= true (:migrated? migrate-body)))
              (is (= "admin/migrate-db" (:action migrate-body)))
              (is (= {:migrators-key "ferment.db/migrators"}
                     (first @migrated)))

              (is (= 200 (:status rollback-resp)))
              (is (= true (:ok? rollback-body)))
              (is (= true (:rolled-back? rollback-body)))
              (is (= "admin/rollback-db" (:action rollback-body)))
              (is (= [:opts+amount nil 2]
                     (first @rolled-back))))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-admin-allows-role-management-actions
  (testing "/v1/admin delegates grant/list/revoke role actions when role policy allows operations."
    (let [port (free-port)
          granted (atom [])
          revoked (atom [])
          listed  (atom [])
          runtime {:models {}
                   :roles {:enabled? true
                           :authorize-default? false
                           :account-type->roles {:manager #{:role/admin}}
                           :operations {:admin/grant-role {:any #{:role/admin}}
                                        :admin/list-roles {:any #{:role/admin}}
                                        :admin/revoke-role {:any #{:role/admin}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :manager}}
          server-state (fhttp/init-http :ferment.http/default
                                        {:host "127.0.0.1"
                                         :port port
                                         :runtime runtime})
          url (str "http://127.0.0.1:" port "/v1/admin")]
      (try
        (with-redefs-fn
          {#'ferment.http/parse-basic-credentials
           (fn [_exchange]
             {:login "admin@example.com"
              :password "secret"})
           #'ferment.http/report-auth!
           (fn [& _] nil)
           #'ferment.auth.user/authenticate-password
           (fn [_source login _password _account-type _opts]
             {:ok? true
              :user {:user/id 15
                     :user/email login
                     :user/account-type :manager}})
           #'ferment.admin/grant-role!
           (fn [selector role]
             (swap! granted conj [selector role])
             {:ok? true
              :granted? true
              :role role
              :user {:user/id 77}
              :roles #{role}})
           #'ferment.admin/list-roles!
           (fn [selector]
             (swap! listed conj selector)
             {:ok? true
              :user {:user/id 77}
              :roles #{:role/admin :role/operator}})
           #'ferment.admin/revoke-role!
           (fn [selector role]
             (swap! revoked conj [selector role])
             {:ok? true
              :revoked? true
              :role role
              :user {:user/id 77}
              :roles #{:role/operator}})}
          (fn []
            (let [grant-resp (http-post-json url {:action "admin/grant-role"
                                                  :email "u@example.com"
                                                  :role "role/admin"})
                  grant-body (json/parse-string (:body grant-resp) true)
                  list-resp (http-post-json url {:action "admin/list-roles"
                                                 :email "u@example.com"})
                  list-body (json/parse-string (:body list-resp) true)
                  revoke-resp (http-post-json url {:action "admin/revoke-role"
                                                   :email "u@example.com"
                                                   :role :role/admin})
                  revoke-body (json/parse-string (:body revoke-resp) true)]
              (is (= 200 (:status grant-resp)))
              (is (= true (:ok? grant-body)))
              (is (= "admin/grant-role" (:action grant-body)))
              (is (= ["u@example.com" :role/admin]
                     (first @granted)))

              (is (= 200 (:status list-resp)))
              (is (= true (:ok? list-body)))
              (is (= "admin/list-roles" (:action list-body)))
              (is (= "u@example.com" (first @listed)))

              (is (= 200 (:status revoke-resp)))
              (is (= true (:ok? revoke-body)))
              (is (= "admin/revoke-role" (:action revoke-body)))
              (is (= ["u@example.com" :role/admin]
                     (first @revoked))))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-admin-allows-role-dictionary-actions
  (testing "/v1/admin delegates create/list-known/delete role dictionary actions when role policy allows operations."
    (let [port (free-port)
          created (atom [])
          deleted (atom [])
          listed  (atom 0)
          runtime {:models {}
                   :roles {:enabled? true
                           :authorize-default? false
                           :account-type->roles {:manager #{:role/admin}}
                           :operations {:admin/create-role {:any #{:role/admin}}
                                        :admin/list-known-roles {:any #{:role/admin}}
                                        :admin/delete-role {:any #{:role/admin}}}}
                   :auth {:enabled? true
                          :source :auth/source
                          :account-type :manager}}
          server-state (fhttp/init-http :ferment.http/default
                                        {:host "127.0.0.1"
                                         :port port
                                         :runtime runtime})
          url (str "http://127.0.0.1:" port "/v1/admin")]
      (try
        (with-redefs-fn
          {#'ferment.http/parse-basic-credentials
           (fn [_exchange]
             {:login "admin@example.com"
              :password "secret"})
           #'ferment.http/report-auth!
           (fn [& _] nil)
           #'ferment.auth.user/authenticate-password
           (fn [_source login _password _account-type _opts]
             {:ok? true
              :user {:user/id 16
                     :user/email login
                     :user/account-type :manager}})
           #'ferment.admin/create-role!
           (fn
             ([role]
              (swap! created conj [role nil])
              {:ok? true :created? true :role role})
             ([role description]
              (swap! created conj [role description])
              {:ok? true :created? true :role role :description description}))
           #'ferment.admin/list-known-roles!
           (fn []
             (swap! listed inc)
             {:ok? true
              :roles [{:role :role/admin}
                      {:role :role/operator}]})
           #'ferment.admin/delete-role!
           (fn [role]
             (swap! deleted conj role)
             {:ok? true :deleted? true :role role})}
          (fn []
            (let [create-resp (http-post-json url {:action "admin/create-role"
                                                   :role "role/researcher"
                                                   :description "Diagnostic role"})
                  create-body (json/parse-string (:body create-resp) true)
                  list-resp (http-post-json url {:action "admin/list-known-roles"})
                  list-body (json/parse-string (:body list-resp) true)
                  delete-resp (http-post-json url {:action "admin/delete-role"
                                                   :role :role/researcher})
                  delete-body (json/parse-string (:body delete-resp) true)]
              (is (= 200 (:status create-resp)))
              (is (= true (:ok? create-body)))
              (is (= "admin/create-role" (:action create-body)))
              (is (= [:role/researcher "Diagnostic role"]
                     (first @created)))

              (is (= 200 (:status list-resp)))
              (is (= true (:ok? list-body)))
              (is (= "admin/list-known-roles" (:action list-body)))
              (is (= 1 @listed))

              (is (= 200 (:status delete-resp)))
              (is (= true (:ok? delete-body)))
              (is (= "admin/delete-role" (:action delete-body)))
              (is (= :role/researcher (first @deleted))))))
        (finally
          (fhttp/stop-http :ferment.http/default server-state))))))

(deftest http-v1-act-collects-telemetry-and-exposes-diag-endpoint
  (testing "HTTP bridge aggregates telemetry for /v1/act and exposes it via /diag/telemetry."
    (let [port (free-port)
          runtime {:protocol {:intents {:problem/solve {:in-schema :req/problem}}
                              :result/types [:value]}
                   :router {:routing {:intent->cap {:problem/solve :llm/solver}}}
                   :resolver {}}
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
                   :router {:routing {:intent->cap {:problem/solve :llm/solver
                                                    :text/respond  :llm/voice}}}
                   :resolver {}}
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
                   :router {:routing {:intent->cap {:problem/solve :llm/solver
                                                    :text/respond  :llm/solver}
                                      :switch-on #{:eval/low-score}
                                      :retry {:same-cap-max 0
                                              :fallback-max 1}}}
                   :resolver {}}
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
      (is (= "ferment_test" (:ferment.env/db.main.name cfg)))
      (is (= "mock" (:ferment.env/ferment.llm.mode cfg)))
      (is (false? (get-in cfg [:ferment.model.defaults/runtime :enabled?]))))))

(deftest test-live-config-is-lightweight-single-runtime
  (testing "Test-live config keeps one small shared runtime enabled for smoke checks."
    (let [cfg (system/read-configs nil
                                   "config/common/prod"
                                   "config/local/prod"
                                   "config/common/test-live"
                                   "config/local/test-live")]
      (is (= :test-live (get-in cfg [:ferment.app/properties :profile])))
      (is (= "live" (:ferment.env/ferment.llm.mode cfg)))
      (is (false? (get-in cfg [:ferment.model.runtime/solver :enabled?])))
      (is (false? (get-in cfg [:ferment.model.runtime/voice :enabled?])))
      (is (false? (get-in cfg [:ferment.model.runtime/coding :enabled?])))
      (is (true? (get-in cfg [:ferment.model.runtime/meta :enabled?])))
      (is (= :ferment.model.runtime/meta
             (:key (get-in cfg [:ferment.model/solver :runtime]))))
      (is (= :ferment.model.runtime/meta
             (:key (get-in cfg [:ferment.model/voice :runtime]))))
      (is (= :ferment.model.runtime/meta
             (:key (get-in cfg [:ferment.model/coding :runtime])))))))

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
           (peek (system/profile-resource-dirs :test))))
    (is (= "config/common/test-live"
           (nth (system/profile-resource-dirs :test-live) 7)))
    (is (= "config/local/test-live"
           (peek (system/profile-resource-dirs :test-live))))))

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
