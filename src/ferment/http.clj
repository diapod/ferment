(ns

    ^{:doc    "HTTP bridge for model runtimes exposed via bot command channel."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.http

  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [cheshire.core :as json]
            [ferment.auth.user :as auth-user]
            [ferment.contracts :as contracts]
            [ferment.core :as core]
            [ferment.middleware.remote-ip :as remote-ip]
            [ferment.model :as model]
            [ferment.oplog :as oplog]
            [ferment.session :as session]
            [ferment.system :as system]
            [ferment.workflow :as workflow]
            [io.randomseed.utils.ip :as ip])

  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
    (java.io OutputStream)
    (java.net InetSocketAddress)
    (java.nio.charset StandardCharsets)
    (java.util Base64 Base64$Decoder)))

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- parse-port
  [v]
  (let [port (cond
               (integer? v) v
               (string? v) (try
                             (Long/parseLong (str/trim v))
                             (catch Throwable _ nil))
               :else nil)]
    (if (and (integer? port) (<= 1 port 65535))
      (int port)
      12002)))

(defn- normalize-endpoint
  [v]
  (when-some [endpoint (trim-s v)]
    (if (str/starts-with? endpoint "/")
      endpoint
      (str "/" endpoint))))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (trim-s v)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- runtime-config
  [runtime]
  (cond
    (and (map? runtime) (map? (:config runtime))) (:config runtime)
    (map? runtime) runtime
    :else nil))

(defn- runtime-worker
  [runtime]
  (when (map? runtime)
    (:worker runtime)))

(defn- request-proto
  [request]
  (let [p (if (map? request) (:proto request) nil)]
    (if (pos-int? p) p 1)))

(defn- request-trace
  [request]
  (let [trace (if (map? request) (:trace request) nil)]
    (if (and (map? trace) (string? (:id trace)) (not (str/blank? (:id trace))))
      trace
      {:id (str (java.util.UUID/randomUUID))})))

(defn- error-envelope
  ([request error-type message]
   (error-envelope request error-type message nil nil))
  ([request error-type message details]
   (error-envelope request error-type message details nil))
  ([request error-type message details retryable?]
   {:proto (request-proto request)
    :trace (request-trace request)
    :error (cond-> {:type error-type
                    :message (or (trim-s message) "Request handling failed.")}
             (some? retryable?) (assoc :retryable? (boolean retryable?))
             (map? details) (assoc :details details))}))

(defn- coerce-act-request
  [payload]
  (letfn [(->keyword-coll
            [v]
            (cond
              (nil? v) nil
              (set? v) (into #{} (keep keywordish) v)
              (sequential? v) (into [] (keep keywordish) v)
              :else v))
          (->int
            [v default]
            (cond
              (integer? v) (int v)
              (number? v)  (int (Math/round (double v)))
              (string? v)  (try
                             (Integer/parseInt (str/trim v))
                             (catch Throwable _ default))
              :else default))
          (normalize-top
            [request]
            (let [intent (or (some-> request :task :intent keywordish)
                             (some-> request :intent keywordish))
                  cap-id (or (some-> request :task :cap/id keywordish)
                             (some-> request :cap/id keywordish))
                  role   (keywordish (:role request))]
              (cond-> request
                (keyword? intent) (assoc-in [:task :intent] intent)
                (keyword? cap-id) (assoc-in [:task :cap/id] cap-id)
                (keyword? role) (assoc :role role)
                (contains? request :proto) (update :proto ->int 1)
                (contains? request :done)
                (-> (update-in [:done :must] ->keyword-coll)
                    (update-in [:done :should] ->keyword-coll))
                (contains? request :effects)
                (update-in [:effects :allowed] ->keyword-coll)
                (contains? request :budget)
                (update-in [:budget :max-roundtrips] ->int nil)
                (contains? request :constraints)
                (update-in [:constraints :language] #(or (keywordish %) %)))))]
    (cond
      (and (map? payload) (map? (:task payload)))
      (normalize-top payload)

      (string? payload)
      {:proto 1
       :trace {:id (str (java.util.UUID/randomUUID))}
       :task {:intent :text/respond}
       :input {:prompt payload}}

      (map? payload)
      (let [trace (or (:trace payload)
                      {:id (str (java.util.UUID/randomUUID))})
            req (-> payload
                    (assoc :proto (or (:proto payload) 1))
                    (assoc :trace trace)
                    (assoc :task (or (:task payload) {:intent :text/respond}))
                    (assoc :input (or (:input payload)
                                      (if (contains? payload :prompt)
                                        {:prompt (:prompt payload)}
                                        {}))))]
        (normalize-top req))

      :else payload)))

(defn- cap-id->role
  [cap-id intent]
  (or (case cap-id
        :llm/code :coder
        :llm/solver :solver
        :llm/meta :router
        :llm/judge :router
        :llm/voice :voice
        :llm/mock :router
        nil)
      (case intent
        :route/decide :router
        :context/summarize :router
        :eval/grade :router
        :problem/solve :solver
        :code/generate :coder
        :code/patch :coder
        :code/explain :coder
        :code/review :coder
        :text/respond :voice
        :solver)))

(defn- now-nanos
  []
  (System/nanoTime))

(defn- nanos->millis
  [start-nanos]
  (/ (double (- (System/nanoTime) start-nanos)) 1000000.0))

(defn- merge-telemetry-counters
  [a b]
  (merge-with
   (fn [x y]
     (cond
       (and (map? x) (map? y)) (merge-telemetry-counters x y)
       (and (number? x) (number? y)) (+ x y)
       :else y))
   (or a {})
   (or b {})))

(defn- default-telemetry
  []
  {:act {:requests 0
         :ok 0
         :errors 0
         :status {}
         :error-types {}
         :latency-ms {:count 0
                      :sum 0.0
                      :max 0.0}}
   :workflow {}})

(defn- telemetry-error-type
  [body]
  (let [err-type (or (get-in body [:error :type])
                     (get-in body [:result :error :type]))]
    (when (keyword? err-type) err-type)))

(defn- record-act-telemetry!
  [telemetry response latency-ms]
  (when (instance? clojure.lang.IAtom telemetry)
    (let [status (or (:status response) 500)
          body   (:body response)
          ok?    (< (int status) 400)
          err-k  (telemetry-error-type body)
          wf-telemetry (get-in body [:result :plan/run :telemetry])]
      (swap! telemetry
             (fn [state]
               (-> (merge-telemetry-counters (default-telemetry) state)
                   (update-in [:act :requests] (fnil inc 0))
                   (update-in [:act :status status] (fnil inc 0))
                   (update-in [:act :latency-ms :count] (fnil inc 0))
                   (update-in [:act :latency-ms :sum] (fnil + 0.0) latency-ms)
                   (update-in [:act :latency-ms :max] (fnil max 0.0) latency-ms)
                   (update-in [:act (if ok? :ok :errors)] (fnil inc 0))
                   (cond-> (keyword? err-k)
                     (update-in [:act :error-types err-k] (fnil inc 0)))
                   (cond-> (map? wf-telemetry)
                     (update :workflow merge-telemetry-counters wf-telemetry))))))))

(defn- telemetry-snapshot
  [telemetry]
  (let [state (if (instance? clojure.lang.IAtom telemetry)
                @telemetry
                (default-telemetry))
        count (get-in state [:act :latency-ms :count] 0)
        sum   (double (get-in state [:act :latency-ms :sum] 0.0))
        avg   (if (pos? count) (/ sum count) 0.0)]
    (assoc-in state [:act :latency-ms :avg] avg)))

(defn- resolve-cap-id
  [resolver request]
  (or (:cap/id request)
      (get-in request [:task :cap/id])
      (workflow/resolve-capability-id
       resolver
       {:intent  (get-in request [:task :intent])
        :effects (:effects request)})))

(defn- positive-int
  [v]
  (when (and (integer? v) (pos? v))
    (int v)))

(defn- act-request->invoke-opts
  [request cap-id]
  (let [intent (get-in request [:task :intent])
        budget (:budget request)]
    (cond-> {:role        (or (:role request)
                              (cap-id->role cap-id intent))
             :intent      intent
             :cap-id      cap-id
             :input       (:input request)
             :context     (:context request)
             :constraints (:constraints request)
             :done        (:done request)
             :budget      budget
             :effects     (:effects request)
             :request-id  (:request/id request)
             :trace       (:trace request)
             :proto       (:proto request)
             :session-id  (:session/id request)
             :session-version (:session/version request)}
      (some? (:model request)) (assoc :model (:model request))
      (some? (get-in request [:task :model])) (assoc :model (get-in request [:task :model]))
      (some? (positive-int (:max-roundtrips budget)))
      (assoc :max-attempts (positive-int (:max-roundtrips budget)))
      (some? (:temperature budget))
      (assoc :temperature (:temperature budget)))))

(defn invoke-act
  "Runs canonical `/v1/act` request through contract validation and core capability flow.

  Returns:
  - `{:status <http-status> :body <canonical-response-envelope>}`"
  ([runtime payload]
   (invoke-act runtime payload nil))
  ([runtime payload telemetry]
   (let [started-at (now-nanos)
         request  (coerce-act-request payload)
         protocol (or (:protocol runtime) {})
         resolver (or (:resolver runtime) {})
         req-check (contracts/validate-request protocol request)
         response
         (cond
           (not (map? request))
           {:status 400
            :body   (error-envelope nil :input/invalid
                                    "Request payload must be a map (EDN/JSON object).")}

           (not (:ok? req-check))
           {:status 400
            :body   (error-envelope request
                                    :input/invalid
                                    "Request does not satisfy protocol contract."
                                    (select-keys req-check [:reason :intent]))}

           :else
           (let [cap-id (resolve-cap-id resolver request)]
             (if-not (keyword? cap-id)
               {:status 422
                :body   (error-envelope request
                                        :unsupported/intent
                                        "No capability can handle the requested intent."
                                        {:intent (get-in request [:task :intent])})}
               (try
                 {:status 200
                  :body   (core/execute-capability!
                           runtime
                           resolver
                           (act-request->invoke-opts request cap-id))}
                 (catch clojure.lang.ExceptionInfo e
                   (let [data (or (ex-data e) {})
                         reason (:error data)]
                     (case reason
                       :invalid-request
                       {:status 400
                        :body   (error-envelope request
                                                :input/invalid
                                                (.getMessage e)
                                                (select-keys data [:reason :intent]))}

                       :invalid-result-after-retries
                       {:status 502
                        :body   (error-envelope request
                                                :schema/invalid
                                                (.getMessage e)
                                                (select-keys data [:attempts :last-check])
                                                true)}

                       {:status 502
                        :body   (error-envelope request
                                                :runtime/invoke-failed
                                                (.getMessage e)
                                                (select-keys data [:error :reason]))})))
                 (catch Throwable t
                   {:status 500
                    :body   (error-envelope request
                                            :runtime/internal
                                            (.getMessage t))})))))
         elapsed-ms (nanos->millis started-at)
         sid        (or (some-> (:session/id request) trim-s)
                        (some-> (:session-id request) trim-s))
         session-state
         (when sid
           (let [service (when (map? runtime) (:session runtime))]
             (when (map? service)
               (try
                 (session/get! service sid)
                 (catch Throwable _ nil)))))
         session-view
         (when (map? session-state)
           (select-keys session-state
                        [:session/id
                         :session/version
                         :session/state
                         :session/frozen?
                         :session/updated-at
                         :session/last-access-at
                         :session/frozen-at
                         :session/thawed-at]))
         response'   (if (and (map? (:body response))
                              (map? session-view))
                       (update response :body merge session-view)
                       response)]
     (record-act-telemetry! telemetry response' elapsed-ms)
     response')))

(def ^:private session-public-keys
  [:session/id
   :session/version
   :session/state
   :session/frozen?
   :session/updated-at
   :session/last-access-at
   :session/frozen-at
   :session/thawed-at])

(defn- runtime-session-service
  [runtime]
  (let [svc (when (map? runtime) (:session runtime))]
    (when (map? svc) svc)))

(defn- session-id-from-payload
  [payload]
  (or (some-> (:session/id payload) trim-s)
      (some-> (:session-id payload) trim-s)))

(defn- session-public
  [session-state]
  (when (map? session-state)
    (select-keys session-state session-public-keys)))

(defn- session-action-response
  [runtime payload]
  (let [action      (keywordish (:action payload))
        session-id  (session-id-from-payload payload)
        model-id    (or (:model payload)
                        (:model-id payload)
                        (:model/id payload))
        opts        (if (map? (:opts payload)) (:opts payload) {})
        service     (runtime-session-service runtime)]
    (case action
      :state
      {:status 200
       :body {:ok? true
              :workers (model/session-workers-state runtime)}}

      :expire
      (do
        (model/expire-session-workers! runtime)
        {:status 200
         :body {:ok? true
                :workers (model/session-workers-state runtime)}})

      :worker/thaw
      (if (and session-id model-id)
        {:status 200
         :body (model/thaw-session-worker! runtime model-id session-id)}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing required keys: :session/id and :model."}})

      :worker/freeze
      (if (and session-id model-id)
        {:status 200
         :body (model/freeze-session-worker! runtime model-id session-id)}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing required keys: :session/id and :model."}})

      :session/open
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (session/open! service session-id opts))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/get
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (session/get! service session-id))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/thaw
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (session/thaw! service session-id opts))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/freeze
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (session/freeze! service session-id opts))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/list
      (if service
        {:status 200
         :body {:ok? true
                :sessions (mapv session-public (session/list! service))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service in runtime."}})

      {:status 400
       :body {:ok? false
              :error :input/invalid
              :message "Unsupported session action."
              :details {:action action
                        :supported #{:state :expire
                                     :worker/thaw :worker/freeze
                                     :session/open :session/get
                                     :session/thaw :session/freeze
                                     :session/list}}}})))

(defn model-http-routes
  "Builds endpoint routing table from initialized `:models` map.

  Output shape:
  {\"/solver/responses\" {:endpoint \"/solver/responses\"
                         :model    :ferment.model/solver
                         :worker   <bot-worker>
                         :worker-id :ferment.model.runtime/solver}}"
  [models]
  (reduce-kv
   (fn [acc model-k model-entry]
     (let [runtime      (when (map? model-entry) (:runtime model-entry))
           config       (runtime-config runtime)
           http-config  (when (map? config) (:http config))
           enabled?     (and (map? http-config) (:enabled? http-config))
           endpoint     (normalize-endpoint (:endpoint http-config))
           worker       (runtime-worker runtime)
           worker-id    (or (some-> runtime :id) model-k)]
       (if (and enabled? endpoint worker)
         (do
           (when (contains? acc endpoint)
             (throw (ex-info "Duplicate HTTP endpoint in model runtime config."
                             {:endpoint endpoint
                              :model model-k
                              :existing (get-in acc [endpoint :model])})))
           (assoc acc endpoint {:endpoint endpoint
                                :model model-k
                                :worker worker
                                :worker-id worker-id}))
         acc)))
   {}
   (or models {})))

(defn- read-body
  [^HttpExchange exchange]
  (with-open [in (.getRequestBody exchange)]
    (slurp in :encoding "UTF-8")))

(defn- content-type
  [^HttpExchange exchange]
  (some-> (.getRequestHeaders exchange)
          (.getFirst "Content-Type")
          trim-s
          str/lower-case))

(defn- decode-request-body
  [^String body ctype]
  (let [body (or body "")
        body' (str/trim body)]
    (cond
      (str/blank? body') {}
      (and ctype (str/includes? ctype "application/json"))
      (json/parse-string body' true)

      (and ctype (str/includes? ctype "application/edn"))
      (edn/read-string body')

      :else {:prompt body})))

(defn- safe-decode-request-body
  [^String body ctype]
  (try
    (decode-request-body body ctype)
    (catch Throwable _
      {})))

(defn- encode-response
  [data]
  (json/generate-string
   (cond
     (map? data) data
     (string? data) {:ok? true :result {:text data}}
     :else {:ok? true :result data})))

(defn- auth-enabled?
  [runtime]
  (true? (get-in runtime [:auth :enabled?])))

(defn- auth-account-type
  [runtime]
  (some-> (get-in runtime [:auth :account-type]) keywordish))

(defn- auth-realm
  [runtime]
  (or (trim-s (get-in runtime [:auth :realm]))
      "ferment"))

(defn- auth-source
  [runtime]
  (get-in runtime [:auth :source]))

(defn- auth-session-service
  [runtime]
  (let [svc (when (map? runtime) (:session runtime))]
    (when (and (map? svc) (fn? (:open! svc)))
      svc)))

(defn- session-id-from-header
  [^HttpExchange exchange]
  (let [headers (.getRequestHeaders exchange)]
    (or (some-> headers (.getFirst "X-Session-Id") trim-s)
        (some-> headers (.getFirst "Session-Id") trim-s))))

(defn- auth-session-id
  [^HttpExchange exchange payload]
  (or (session-id-from-payload payload)
      (session-id-from-header exchange)))

(defn- auth-options
  [runtime ^HttpExchange exchange payload]
  (let [sid      (auth-session-id exchange payload)
        service  (auth-session-service runtime)]
    (cond-> {}
      sid (assoc :session/id sid)
      service (assoc :session/service service))))

(defn- normalize-ipv6-str
  [v]
  (some-> v
          remote-ip/remote-addr-parse
          ip/to-address
          ip/to-v6
          ip/to-str-v6))

(defn- auth-client-ip
  [runtime ^HttpExchange exchange]
  (let [proxy-header (some-> (get-in runtime [:auth :proxy-header])
                             remote-ip/process-proxy)
        proxy-value  (when proxy-header
                       (some-> (.getRequestHeaders exchange)
                               (.getFirst proxy-header)))
        proxy-first  (some-> proxy-value (str/split #",") first trim-s)
        remote-addr  (some-> exchange .getRemoteAddress .getAddress .getHostAddress)]
    (or (normalize-ipv6-str proxy-first)
        (normalize-ipv6-str remote-addr))))

(defn- report-auth!
  [runtime exchange message]
  (let [logger (oplog/auth-logger runtime)]
    (when (fn? logger)
      (let [base {:client-ip (auth-client-ip runtime exchange)}
            data (if (map? message) (merge base message) base)]
        (apply logger (mapcat identity data))))))

(defn- parse-basic-credentials
  [^HttpExchange exchange]
  (when-some [header (some-> (.getRequestHeaders exchange)
                             (.getFirst "Authorization")
                             trim-s)]
    (let [[scheme token] (str/split header #"\s+" 2)]
      (when (and scheme token
                 (= "basic" (str/lower-case scheme)))
        (try
          (let [^String token' token
                ^Base64$Decoder decoder (Base64/getDecoder)
                ^bytes decoded-bytes (.decode decoder token')
                decoded (String. decoded-bytes StandardCharsets/UTF_8)
                idx (.indexOf ^String decoded ":")]
            (when (pos? idx)
              {:login    (subs decoded 0 idx)
               :password (subs decoded (inc idx))}))
          (catch Throwable _
            nil))))))

(defn- unauthorized-response
  [runtime message]
  {:status 401
   :headers {"WWW-Authenticate" (str "Basic realm=\"" (auth-realm runtime) "\"")}
   :body {:ok? false
          :error :auth/unauthorized
          :message (or (trim-s message)
                       "Authentication required.")}})

(defn- auth-config-error-response
  []
  {:status 500
   :body {:ok? false
          :error :auth/not-configured
          :message "HTTP authentication enabled, but auth source is missing."}})

(defn- authorize-request
  ([runtime exchange]
   (authorize-request runtime exchange nil))
  ([runtime exchange payload]
  (when (auth-enabled? runtime)
    (if-not (some? (auth-source runtime))
      (do
        (report-auth! runtime exchange
                      {:operation :auth/http-basic
                       :success false
                       :level :error
                       :message "HTTP auth enabled, but auth source is missing."})
        (auth-config-error-response))
      (if-some [{:keys [login password]} (parse-basic-credentials exchange)]
        (let [result (auth-user/authenticate-password
                      (auth-source runtime)
                      login
                      password
                      (auth-account-type runtime)
                      (auth-options runtime exchange payload))]
          (if (:ok? result)
            (do
              (report-auth! runtime exchange
                            {:operation :auth/http-basic
                             :success true
                             :user-id (get-in result [:user :user/id])
                             :message "HTTP basic auth accepted."})
              nil)
            (do
              (report-auth! runtime exchange
                            {:operation :auth/http-basic
                             :success false
                             :level :warning
                             :message (str "HTTP basic auth rejected: " (or (:error result) :unknown))})
              (unauthorized-response runtime "Invalid credentials."))))
        (do
          (report-auth! runtime exchange
                        {:operation :auth/http-basic
                         :success false
                         :level :notice
                         :message "Missing or invalid Authorization header."})
          (unauthorized-response runtime "Missing or invalid Authorization header.")))))))

(defn- write-response!
  ([^HttpExchange exchange status ^String body]
   (write-response! exchange status body nil))
  ([^HttpExchange exchange status ^String body extra-headers]
   (let [bytes (.getBytes body StandardCharsets/UTF_8)
         headers (.getResponseHeaders exchange)]
     (.set headers "Content-Type" "application/json; charset=utf-8")
     (when (map? extra-headers)
       (doseq [[k v] extra-headers]
         (when (and (some? k) (some? v))
           (.set headers (str k) (str v)))))
     (.sendResponseHeaders exchange (long status) (long (alength bytes)))
     (with-open [^OutputStream out (.getResponseBody exchange)]
       (.write out bytes)))))

(defn- safe-invoke!
  [route payload]
  (try
    (let [worker (:worker route)]
      (if-not worker
        {:ok? false
         :error :runtime-worker-missing
         :model (:model route)}
        (or (model/command-bot-worker! worker :invoke payload)
            {:ok? false
             :error :empty-response
             :model (:model route)})))
    (catch Throwable t
      {:ok? false
       :error :invoke-exception
       :message (.getMessage t)
       :class (str (class t))
       :model (:model route)})))

(defn- invoke-handler
  [route]
  (reify HttpHandler
    (handle [_ exchange]
      (let [method (some-> (.getRequestMethod exchange) str/upper-case)]
        (if (not= "POST" method)
          (write-response! exchange 405 (encode-response {:ok? false
                                                          :error :method-not-allowed
                                                          :allowed ["POST"]}))
          (let [ctype   (content-type exchange)
                body    (read-body exchange)
                payload (decode-request-body body ctype)
                result  (safe-invoke! route payload)
                status  (if (= false (:ok? result)) 502 200)]
            (write-response! exchange status (encode-response result))))))))

(defn- act-handler
  [runtime telemetry]
  (reify HttpHandler
    (handle [_ exchange]
      (let [method (some-> (.getRequestMethod exchange) str/upper-case)]
        (if (not= "POST" method)
          (write-response! exchange
                           405
                           (encode-response
                            (error-envelope nil
                                            :method-not-allowed
                                            "Only POST is supported for this endpoint."
                                            {:allowed ["POST"]})))
          (let [ctype         (content-type exchange)
                body-str      (read-body exchange)
                auth-payload  (safe-decode-request-body body-str ctype)]
            (if-some [{:keys [status body headers]} (authorize-request runtime exchange auth-payload)]
              (write-response! exchange status (encode-response body) headers)
              (try
                (let [payload (decode-request-body body-str ctype)
                      {:keys [status body]} (invoke-act runtime payload telemetry)]
                  (write-response! exchange status (encode-response body)))
                (catch Throwable t
                  (write-response! exchange
                                   400
                                   (encode-response
                                    (error-envelope nil
                                                    :input/invalid
                                                    (.getMessage t)))))))))))))

(defn- telemetry-handler
  [telemetry]
  (reify HttpHandler
    (handle [_ exchange]
      (let [method (some-> (.getRequestMethod exchange) str/upper-case)]
        (if (contains? #{"GET" "POST"} method)
          (write-response! exchange 200 (encode-response {:ok? true
                                                          :service :ferment.http
                                                          :telemetry (telemetry-snapshot telemetry)}))
          (write-response! exchange
                           405
                           (encode-response {:ok? false
                                             :error :method-not-allowed
                                             :allowed ["GET" "POST"]})))))))

(defn- session-handler
  [runtime]
  (reify HttpHandler
    (handle [_ exchange]
      (let [method (some-> (.getRequestMethod exchange) str/upper-case)]
        (if (not= "POST" method)
          (write-response! exchange
                           405
                           (encode-response {:ok? false
                                             :error :method-not-allowed
                                             :allowed ["POST"]}))
          (let [ctype         (content-type exchange)
                body-str      (read-body exchange)
                auth-payload  (safe-decode-request-body body-str ctype)]
            (if-some [{:keys [status body headers]} (authorize-request runtime exchange auth-payload)]
              (write-response! exchange status (encode-response body) headers)
              (try
                (let [payload (decode-request-body body-str ctype)
                      {:keys [status body]} (session-action-response runtime payload)]
                  (write-response! exchange status (encode-response body)))
                (catch Throwable t
                  (write-response! exchange
                                   500
                                   (encode-response {:ok? false
                                                     :error :runtime/internal
                                                     :message (.getMessage t)})))))))))))

(defn- health-handler
  [route-count]
  (reify HttpHandler
    (handle [_ exchange]
      (write-response! exchange 200 (encode-response {:ok? true
                                                      :service :ferment.http
                                                      :routes route-count})))))

(defn- routes-handler
  [routes]
  (reify HttpHandler
    (handle [_ exchange]
      (write-response! exchange 200 (encode-response {:ok? true
                                                      :routes routes})))))

(defn preconfigure-http
  "Pre-configuration hook for HTTP bridge."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (cond-> cfg
      (not (contains? cfg :models))
      (assoc :models (system/ref :ferment/models))

      (not (contains? cfg :runtime))
      (assoc :runtime (system/ref :ferment.runtime/default))

      (not (contains? cfg :auth))
      (assoc :auth {:enabled? false}))))

(defn init-http
  "Initializes HTTP bridge for model runtime workers."
  [_k config]
  (let [cfg      (preconfigure-http _k config)
        host     (or (trim-s (:host cfg)) "127.0.0.1")
        port     (parse-port (:port cfg))
        runtime  (let [r (if (map? (:runtime cfg)) (:runtime cfg) {})]
                   (cond-> (if (contains? r :models)
                             r
                             (assoc r :models (:models cfg)))
                     (contains? cfg :auth)
                     (assoc :auth (:auth cfg))))
        routes   (model-http-routes (:models runtime))
        public-model-routes
        (into {}
              (map (fn [[endpoint {:keys [model worker-id]}]]
                     [endpoint {:model model :worker-id worker-id
                                :type :model-runtime}]))
              routes)
        public-routes
        (assoc public-model-routes
               "/v1/act" {:type :protocol-act}
               "/v1/session" {:type :session-bridge}
               "/health" {:type :health}
               "/routes" {:type :routes}
               "/diag/telemetry" {:type :diag-telemetry})
        telemetry (atom (default-telemetry))
        server   (HttpServer/create (InetSocketAddress. ^String host (int port)) 0)]
    (doseq [[endpoint route] routes]
      (.createContext server endpoint (invoke-handler route)))
    (.createContext server "/v1/act" (act-handler runtime telemetry))
    (.createContext server "/v1/session" (session-handler runtime))
    (.createContext server "/health" (health-handler (count public-routes)))
    (.createContext server "/routes" (routes-handler public-routes))
    (.createContext server "/diag/telemetry" (telemetry-handler telemetry))
    (.setExecutor server nil)
    (.start server)
    {:host host
     :port port
     :server server
     :telemetry telemetry
     :routes public-routes}))

(defn stop-http
  "Stops HTTP bridge."
  [_k state]
  (when-some [^HttpServer server (:server state)]
    (.stop server 0))
  nil)

(derive ::service :ferment.system/value)
(derive :ferment.http/default ::service)

(system/add-expand ::service [k config] {k (preconfigure-http k config)})
(system/add-init   ::service [k config]    (init-http k config))
(system/add-halt!  ::service [k state]     (stop-http k state))
