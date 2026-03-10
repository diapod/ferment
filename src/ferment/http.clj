(ns

    ^{:doc    "HTTP bridge for model runtimes exposed via bot command channel."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.http

  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [cheshire.core :as json]
            [ferment.adapters.model :as model-adapter]
            [ferment.admin :as admin]
            [ferment.auth.user :as auth-user]
            [ferment.contracts :as contracts]
            [ferment.core :as core]
            [ferment.execution-graph :as execution-graph]
            [ferment.memory :as memory]
            [ferment.http.act.middleware.execute :as act-middleware-execute]
            [ferment.http.act.middleware.finalize :as act-middleware-finalize]
            [ferment.http.act.middleware.prepare :as act-middleware-prepare]
            [ferment.http.act.middleware.route :as act-middleware-route]
            [ferment.middleware.remote-ip :as remote-ip]
            [ferment.oplog :as oplog]
            [ferment.protocol :as protocol]
            [ferment.queue :as queue]
            [ferment.runtime :as runtime-svc]
            [ferment.roles :as roles]
            [ferment.router :as router]
            [ferment.system :as system]
            [ferment.telemetry :as telemetry]
            [ferment.tenancy :as tenancy]
            [ferment.training.collector :as training-collector]
            [ferment.training.events :as training-events]
            [ferment.workflow :as workflow]
            [io.randomseed.utils.ip :as ip])

  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
    (java.io OutputStream)
    (java.net InetSocketAddress URLDecoder)
    (java.nio.charset StandardCharsets)
    (java.util Base64 Base64$Decoder)
    (java.util.concurrent ExecutorService Executors ThreadFactory TimeUnit)
    (java.util.concurrent.atomic AtomicLong)))

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

(defn- parse-non-negative-long
  [v]
  (let [n (cond
            (integer? v) (long v)
            (number? v) (long v)
            (string? v) (try
                          (Long/parseLong (str/trim v))
                          (catch Throwable _ nil))
            :else nil)]
    (when (and (some? n) (not (neg? n)))
      n)))

(defn- parse-positive-int
  [v default]
  (let [n (cond
            (integer? v) (int v)
            (number? v) (int (Math/floor (double v)))
            (string? v) (try
                          (Integer/parseInt (str/trim v))
                          (catch Throwable _ nil))
            :else nil)]
    (if (and (integer? n) (pos? n))
      n
      default)))

(defn- http-worker-threads
  [cfg]
  (let [default-threads (max 4 (* 2 (.availableProcessors (Runtime/getRuntime))))
        configured      (or (:executor/threads cfg)
                            (get-in cfg [:executor :threads]))]
    (parse-positive-int configured default-threads)))

(defn- http-thread-factory
  []
  (let [counter (AtomicLong. 0)]
    (reify ThreadFactory
      (^Thread newThread [_ ^Runnable runnable]
        (let [^Thread t (Thread. runnable (str "ferment-http-" (.incrementAndGet counter)))]
          (.setDaemon t true)
          t)))))

(defn- create-http-executor
  [cfg]
  (Executors/newFixedThreadPool (int (http-worker-threads cfg))
                                ^ThreadFactory (http-thread-factory)))

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

(defn- keyword-set
  [v]
  (cond
    (set? v) (into #{} (keep keywordish) v)
    (sequential? v) (into #{} (keep keywordish) v)
    (some? v) (if-some [k (keywordish v)] #{k} #{})
    :else #{}))

(defn- auth-user-public
  [user]
  (when (map? user)
    (let [roles' (->> (keyword-set (or (:user/roles user)
                                       (:roles user)))
                      sort
                      vec)]
      (cond-> {}
        (some? (:user/id user)) (assoc :user/id (:user/id user))
        (some? (:user/email user)) (assoc :user/email (:user/email user))
        (some? (:user/tenant-id user)) (assoc :user/tenant-id (:user/tenant-id user))
        (some? (:user/account-type user)) (assoc :user/account-type (:user/account-type user))
        (seq roles') (assoc :user/roles roles')))))

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

(defn- coerce-keyword-coll
  [v]
  (cond
    (nil? v)        nil
    (set? v)        (into #{} (keep keywordish) v)
    (sequential? v) (into [] (keep keywordish) v)
    :else           v))

(defn- coerce-bool
  [v]
  (cond
    (boolean? v) v
    (number? v)  (not (zero? (long v)))
    (string? v)  (contains? #{"1" "true" "yes" "on"}
                            (-> v str/trim str/lower-case))
    (nil? v)     nil
    :else        (boolean v)))

(defn- coerce-int
  [v default]
  (cond
    (integer? v) (int v)
    (number? v)  (int (Math/round (double v)))
    (string? v)  (try
                   (Integer/parseInt (str/trim v))
                   (catch Throwable _ default))
    :else        default))

(defn- contains-path?
  [m path]
  (if-not (seq path)
    true
    (let [k (first path)
          tail (next path)]
      (and (map? m)
           (contains? m k)
           (if (seq tail)
             (contains-path? (get m k) tail)
             true)))))

(defn- map-bool-option
  [m key-candidates]
  (when (map? m)
    (reduce (fn [_ k]
              (if (contains? m k)
                (reduced (coerce-bool (get m k)))
                nil))
            nil
            key-candidates)))

(def ^:private training-enabled-nested-keys
  [:enabled?])

(def ^:private training-enabled-top-keys
  [:training/enabled?])

(def ^:private replay-enabled-nested-keys
  [:enabled? :enabled "enabled?" "enabled"])

(def ^:private replay-enabled-top-keys
  [:replay/enabled?
   :replay.enabled?
   "replay/enabled?"
   "replay.enabled?"])

(defn- normalize-act-training
  [request]
  (let [request' (if (map? request) request {})
        nested-training (if (map? (:training request'))
                          (:training request')
                          nil)
        enabled? (or (map-bool-option nested-training training-enabled-nested-keys)
                     (map-bool-option request' training-enabled-top-keys))]
    (cond-> request
      (some? enabled?) (assoc-in [:training :enabled?] enabled?))))

(defn- normalize-act-replay
  [request]
  (let [request' (if (map? request) request {})
        nested-replay (if (map? (:replay request'))
                        (:replay request')
                        nil)
        enabled? (or (map-bool-option nested-replay replay-enabled-nested-keys)
                     (map-bool-option request' replay-enabled-top-keys))]
    (cond-> request
      (some? enabled?) (assoc-in [:replay :enabled?] enabled?))))

(defn- normalize-act-routing
  [routing]
  (let [routing'    (if (map? routing) routing {})
        intent      (keywordish (:intent routing'))
        cap-id      (keywordish (:cap/id routing'))
        profile     (keywordish (:profile routing'))
        meta?       (coerce-bool (:meta? routing'))
        strict?     (coerce-bool (:strict? routing'))
        force?      (coerce-bool (:force? routing'))
        on-error    (keywordish (:on-error routing'))
        artifact-version (or (keywordish (:artifact/version routing'))
                             (keywordish (:policy/version routing'))
                             (keywordish (:policy-version routing'))
                             (keywordish (:protocol/version routing')))
        shadow-artifact-version (or (keywordish (:shadow/artifact-version routing'))
                                    (keywordish (:shadow/version routing'))
                                    (keywordish (:protocol/shadow-version routing'))
                                    (keywordish (:policy/shadow-version routing')))
        router-artifact-version (or (keywordish (:router/artifact-version routing'))
                                    (keywordish (:router/version routing'))
                                    (keywordish (:routing/version routing')))
        router-shadow-artifact-version (or (keywordish (:router/shadow-artifact-version routing'))
                                           (keywordish (:router/shadow-version routing'))
                                           (keywordish (:routing/shadow-version routing')))
        debug-plan? (coerce-bool (or (:debug/plan? routing')
                                     (:debug-plan? routing')
                                     (get-in routing' [:debug :plan?])))
        debug-transcript? (coerce-bool (or (:debug/transcript? routing')
                                           (:debug-transcript? routing')
                                           (get-in routing' [:debug :transcript?])))]
    (cond-> routing'
      (keyword? intent)                               (assoc :intent intent)
      (keyword? cap-id)                               (assoc :cap/id cap-id)
      (keyword? profile)                              (assoc :profile profile)
      (some? meta?)                                   (assoc :meta? meta?)
      (some? strict?)                                 (assoc :strict? strict?)
      (some? force?)                                  (assoc :force? force?)
      (contains? #{:fail-open :fail-closed} on-error) (assoc :on-error on-error)
      (keyword? artifact-version)                     (assoc :artifact/version artifact-version)
      (keyword? shadow-artifact-version)              (assoc :shadow/artifact-version shadow-artifact-version)
      (keyword? router-artifact-version)              (assoc :router/artifact-version router-artifact-version)
      (keyword? router-shadow-artifact-version)       (assoc :router/shadow-artifact-version router-shadow-artifact-version)
      (some? debug-plan?)                             (assoc :debug/plan? debug-plan?)
      (some? debug-transcript?)                       (assoc :debug/transcript? debug-transcript?))))

(defn- normalize-act-top
  [request]
  (let [intent        (or (some-> request :task :intent keywordish)
                          (some-> request :intent keywordish))
        cap-id        (or (some-> request :task :cap/id keywordish)
                          (some-> request :cap/id keywordish))
        requires      (contracts/normalize-requires
                       (or (get-in request [:task :requires])
                           (:requires request)))
        role          (keywordish (:role request))
        response-type (or (some-> request :response/type keywordish)
                          (some-> request :response :type keywordish))
        stream?       (or (coerce-bool (:stream? request))
                          (coerce-bool (get-in request [:response :stream?])))]
    (-> (cond-> request
          (keyword? intent)          (assoc-in [:task :intent] intent)
          (keyword? cap-id)          (assoc-in [:task :cap/id] cap-id)
          (map? requires)            (assoc-in [:task :requires] requires)
          (keyword? role)            (assoc :role role)
          (keyword? response-type)   (assoc :response/type response-type)
          (some? stream?)            (assoc :stream? stream?)
          (contains? request :proto) (update :proto coerce-int 1)
          (contains? request :done)
          (-> (update-in [:done :must] coerce-keyword-coll)
              (update-in [:done :should] coerce-keyword-coll))
          (contains? request :effects)
          (update-in [:effects :allowed] coerce-keyword-coll)
          (contains? request :budget)
          (update-in [:budget :max-roundtrips] coerce-int nil)
          (contains? request :constraints)
          (update-in [:constraints :language] #(or (keywordish %) %))
          (contains? request :routing)
          (update :routing normalize-act-routing))
        normalize-act-training
        normalize-act-replay)))

(defn- coerce-act-request
  [payload]
  (cond
    (and (map? payload) (map? (:task payload)))
    (normalize-act-top payload)

    (string? payload)
    {:proto 1
     :trace {:id (str (java.util.UUID/randomUUID))}
     :task  {:intent :text/respond}
     :input {:prompt payload}}

    (map? payload)
    (let [trace (or (:trace payload)
                    {:id (str (java.util.UUID/randomUUID))})
          req   (-> payload
                    (assoc :proto (or (:proto payload) 1))
                    (assoc :trace trace)
                    (assoc :task (or (:task payload) {:intent :text/respond}))
                    (assoc :input (or (:input payload)
                                      (if (contains? payload :prompt)
                                        {:prompt (:prompt payload)}
                                        {}))))]
      (normalize-act-top req))

    :else payload))

(defn- cap-id->role
  [runtime resolver cap-id intent]
  (router/resolve-role runtime resolver cap-id intent))

(defn- now-nanos
  []
  (System/nanoTime))

(defn- nanos->millis
  [start-nanos]
  (/ (double (- (System/nanoTime) start-nanos)) 1000000.0))

(defn- default-telemetry
  []
  {:act      {:requests    0
              :ok          0
              :errors      0
              :status      {}
              :error-types {}
              :cache       {:lookups       0
                            :hits          0
                            :misses        0
                            :stores        0
                            :evictions     0
                            :invalidations 0}
              :routing     {:route/decide-hit      0
                            :route/decide-continue 0
                            :route/decide-final    0
                            :route/fail-open       0
                            :route/fail-closed     0
                            :route/strict          0
                            :route/shadow-enabled  0
                            :route/shadow-attempt  0
                            :route/shadow-match    0
                            :route/shadow-mismatch 0
                            :route/shadow-error    0
                            :cap/resolve-attempt   0
                            :cap/resolve-hit       0
                            :cap/resolve-miss      0
                            :cap/reject-reasons    {}}
              :latency-ms  {:count 0
                            :sum   0.0
                            :max   0.0}}
   :workflow {:calls/total         0
              :calls/succeeded     0
              :calls/failed        0
              :calls/retries       0
              :calls/fallback-hops 0
              :calls/failure-types {}
              :quality/judge-used  0
              :quality/judge-pass  0
              :quality/judge-fail  0
              :quality/must-failed 0}
   :orchestration {:participants/requests 0
                   :participants/total 0
                   :participants/max 0
                   :context/default-lookups 0
                   :context/default-hits 0
                   :context/default-misses 0}
   :tenancy {:requests 0
             :errors 0
             :rejected 0
             :billed-tokens 0}})

(defn- workflow-telemetry-from-error
  [body]
  (let [outcome     (if (map? body) (get-in body [:error :details :outcome]) nil)
        failure-type (when (map? outcome) (:failure/type outcome))
        must-failed (when (map? outcome) (get-in outcome [:done/eval :must-failed]))]
    (if (map? outcome)
      (cond-> {:calls/total 1
               :calls/failed 1}
        (keyword? failure-type)
        (assoc :calls/failure-types {failure-type 1})

        (and (sequential? must-failed) (seq must-failed))
        (assoc :quality/must-failed 1))
      {})))

(defn- orchestration-telemetry-from-response
  [body]
  (let [participants (if (sequential? (:models/used body))
                       (->> (:models/used body)
                            (filter map?)
                            distinct
                            vec)
                       [])
        n (count participants)]
    (cond-> {}
      (pos? n)
      (assoc :participants/requests 1
             :participants/total n
             :participants/max n))))

(defn- telemetry-error-type
  [body]
  (let [err-type (or (get-in body [:error :type])
                     (get-in body [:result :error :type]))]
    (when (keyword? err-type) err-type)))

(def ^:private parse-failure-types
  #{:schema/invalid
    :format/drift})

(defn- counter-value
  [v]
  (long (or (parse-non-negative-long v) 0)))

(defn- normalize-counter-map
  [m]
  (if (map? m)
    (reduce-kv (fn [acc k v]
                 (let [k' (keywordish k)
                       n  (counter-value v)]
                   (if (and (keyword? k') (pos? n))
                     (assoc acc k' n)
                     acc)))
               {}
               m)
    {}))

(defn- sum-counter
  [counter ks]
  (reduce (fn [acc k]
            (+ acc (counter-value (get counter k))))
          0
          ks))

(defn- safe-rate
  [num den]
  (when (pos? den)
    (/ (double num) (double den))))

(defn- failure-domain
  [failure-type]
  (case (some-> failure-type namespace)
    "auth"        :auth
    "effects"     :effects
    "route"       :route
    "runtime"     :runtime
    "schema"      :schema
    "eval"        :eval
    "input"       :input
    "unsupported" :unsupported
    "timeout"     :timeout
    "policy"      :policy
    "session"     :session
    :other))

(defn- failure-taxonomy
  [act-errors workflow-errors]
  (let [by-type (merge-with + (or act-errors {}) (or workflow-errors {}))
        by-domain (reduce-kv (fn [acc failure-type count']
                               (update acc
                                       (failure-domain failure-type)
                                       (fnil + 0)
                                       (counter-value count')))
                             {}
                             by-type)]
    {:by-type by-type
     :by-domain by-domain}))

(defn- telemetry-kpi
  [state]
  (let [act                 (if (map? (:act state)) (:act state) {})
        workflow            (if (map? (:workflow state)) (:workflow state) {})
        cache               (normalize-counter-map (:cache act))
        act-errors          (normalize-counter-map (:error-types act))
        wf-errors           (normalize-counter-map (:calls/failure-types workflow))
        act-requests        (counter-value (:requests act))
        cache-lookups       (counter-value (:lookups cache))
        cache-hits          (counter-value (:hits cache))
        cache-misses        (counter-value (:misses cache))
        cache-stores        (counter-value (:stores cache))
        cache-evictions     (counter-value (:evictions cache))
        cache-invalidations (counter-value (:invalidations cache))
        wf-calls-total      (counter-value (:calls/total workflow))
        retries             (counter-value (:calls/retries workflow))
        fallback-hops       (counter-value (:calls/fallback-hops workflow))
        must-failed         (counter-value (:quality/must-failed workflow))
        judge-used          (counter-value (:quality/judge-used workflow))
        judge-pass          (counter-value (:quality/judge-pass workflow))
        judge-fail          (counter-value (:quality/judge-fail workflow))
        parse-source        (if (pos? wf-calls-total) :workflow :act)
        parse-total         (if (pos? wf-calls-total) wf-calls-total act-requests)
        parse-failures      (if (pos? wf-calls-total)
                              (sum-counter wf-errors parse-failure-types)
                              (sum-counter act-errors parse-failure-types))
        parse-ok            (max 0 (- parse-total parse-failures))]
    {:parse-rate       {:value    (safe-rate parse-ok parse-total)
                        :ok       parse-ok
                        :failures parse-failures
                        :total    parse-total
                        :source   parse-source}
     :retry-rate       {:value   (safe-rate retries wf-calls-total)
                        :retries retries
                        :total   wf-calls-total}
     :fallback-rate    {:value         (safe-rate fallback-hops wf-calls-total)
                        :fallback-hops fallback-hops
                        :total         wf-calls-total}
     :must-failed-rate {:value       (safe-rate must-failed wf-calls-total)
                        :must-failed must-failed
                        :total       wf-calls-total}
     :judge-pass-rate  {:value (safe-rate judge-pass judge-used)
                        :pass  judge-pass
                        :fail  judge-fail
                        :used  judge-used}
     :cache-hit-rate   {:value         (safe-rate cache-hits cache-lookups)
                        :hits          cache-hits
                        :misses        cache-misses
                        :lookups       cache-lookups
                        :stores        cache-stores
                        :evictions     cache-evictions
                        :invalidations cache-invalidations}
     :failure-taxonomy (failure-taxonomy act-errors wf-errors)}))

(defn- telemetry-orchestration
  [state]
  (let [orchestration (if (map? (:orchestration state)) (:orchestration state) {})
        routing (if (map? (get-in state [:act :routing]))
                  (get-in state [:act :routing])
                  {})
        participants-requests (counter-value (:participants/requests orchestration))
        participants-total (counter-value (:participants/total orchestration))
        participants-max (counter-value (:participants/max orchestration))
        context-lookups (counter-value (:context/default-lookups orchestration))
        context-hits (counter-value (:context/default-hits orchestration))
        context-misses (counter-value (:context/default-misses orchestration))
        context-principal-blocked (counter-value (:context/principal-isolation-blocked orchestration))
        route-total (counter-value (:route/decide-hit routing))
        continue' (counter-value (:route/decide-continue routing))
        final' (counter-value (:route/decide-final routing))
        fail-open' (counter-value (:route/fail-open routing))
        fail-closed' (counter-value (:route/fail-closed routing))
        strict' (counter-value (:route/strict routing))
        shadow-enabled' (counter-value (:route/shadow-enabled routing))
        shadow-attempt' (counter-value (:route/shadow-attempt routing))
        shadow-match' (counter-value (:route/shadow-match routing))
        shadow-mismatch' (counter-value (:route/shadow-mismatch routing))
        shadow-error' (counter-value (:route/shadow-error routing))]
    {:participants/diversity {:value (safe-rate participants-total participants-requests)
                              :participants/total participants-total
                              :participants/requests participants-requests
                              :participants/max participants-max}
     :route/decision-quality-trend
     {:value (safe-rate (+ continue' final') route-total)
      :route/decide-hit route-total
      :route/decide-continue continue'
      :route/decide-final final'
      :route/fail-open fail-open'
      :route/fail-closed fail-closed'
      :route/strict strict'
      :route/shadow-enabled shadow-enabled'
      :route/shadow-attempt shadow-attempt'
      :route/shadow-match shadow-match'
      :route/shadow-mismatch shadow-mismatch'
      :route/shadow-error shadow-error'
      :continue-rate (safe-rate continue' route-total)
      :final-rate (safe-rate final' route-total)
      :fail-open-rate (safe-rate fail-open' route-total)
      :fail-closed-rate (safe-rate fail-closed' route-total)
      :strict-rate (safe-rate strict' route-total)
      :shadow-match-rate (safe-rate shadow-match' shadow-attempt')}
     :context/hit-utility {:value (safe-rate context-hits context-lookups)
                           :lookups context-lookups
                           :hits context-hits
                           :misses context-misses}
     :context/principal-isolation
     {:blocked context-principal-blocked}}))

(defn- record-act-telemetry!
  ([telemetry response latency-ms]
   (record-act-telemetry! telemetry response latency-ms nil nil))
  ([telemetry response latency-ms routing-telemetry]
   (record-act-telemetry! telemetry response latency-ms routing-telemetry nil))
  ([telemetry response latency-ms routing-telemetry cache-telemetry]
   (when (instance? clojure.lang.IAtom telemetry)
     (let [status (or (:status response) 500)
           body   (:body response)
           ok?    (< (int status) 400)
           err-k  (telemetry-error-type body)
           wf-telemetry (get-in body [:result :plan/run :telemetry])
           wf-error-telemetry (workflow-telemetry-from-error body)
           orchestration-telemetry (orchestration-telemetry-from-response body)]
       (swap! telemetry
              (fn [state]
                (-> (telemetry/merge-counters (default-telemetry) state)
                    (update-in [:act :requests] (fnil inc 0))
                    (update-in [:act :status status] (fnil inc 0))
                    (update-in [:act :latency-ms :count] (fnil inc 0))
                    (update-in [:act :latency-ms :sum] (fnil + 0.0) latency-ms)
                    (update-in [:act :latency-ms :max] (fnil max 0.0) latency-ms)
                    (update-in [:act (if ok? :ok :errors)] (fnil inc 0))
                    (cond-> (keyword? err-k)
                      (update-in [:act :error-types err-k] (fnil inc 0)))
                    (cond-> (map? wf-telemetry)
                      (update :workflow telemetry/merge-counters wf-telemetry))
                    (cond-> (map? wf-error-telemetry)
                      (update :workflow telemetry/merge-counters wf-error-telemetry))
                    (cond-> (map? routing-telemetry)
                      (update-in [:act :routing] telemetry/merge-counters routing-telemetry))
                    (cond-> (map? cache-telemetry)
                      (update-in [:act :cache] telemetry/merge-counters cache-telemetry))
                    (cond-> (map? orchestration-telemetry)
                      (update :orchestration telemetry/merge-counters orchestration-telemetry)))))))))

(defn- record-cache-telemetry!
  [telemetry cache-telemetry]
  (when (and (instance? clojure.lang.IAtom telemetry)
             (map? cache-telemetry)
             (seq cache-telemetry))
    (swap! telemetry
           (fn [state]
             (-> (telemetry/merge-counters (default-telemetry) state)
                 (update-in [:act :cache] telemetry/merge-counters cache-telemetry)))))
  nil)

(defn- telemetry-snapshot
  ([telemetry]
   (telemetry-snapshot nil telemetry nil))
  ([runtime telemetry filters]
   (let [state0 (if (instance? clojure.lang.IAtom telemetry)
                  @telemetry
                  (default-telemetry))
         state  (telemetry/merge-counters (default-telemetry) state0)
         count  (counter-value (get-in state [:act :latency-ms :count]))
         sum    (double (or (get-in state [:act :latency-ms :sum]) 0.0))
         avg    (if (pos? count) (/ sum count) 0.0)
         tenancy-state (when (map? runtime) (:tenancy/state runtime))
         tenant-filter (some-> filters :tenant keywordish)
         principal-filter (some-> filters :principal trim-s)
         tenancy-view (tenancy/snapshot tenancy-state tenant-filter principal-filter)
         totals-tenant (if (map? (:by-tenant tenancy-view)) (:by-tenant tenancy-view) {})
         totals-principal (if (map? (:by-principal tenancy-view)) (:by-principal tenancy-view) {})
         requests-total (reduce + 0 (map #(counter-value (:requests %)) (vals totals-tenant)))
         errors-total (reduce + 0 (map #(counter-value (:errors %)) (vals totals-tenant)))
         rejected-total (reduce + 0
                                (map (fn [v]
                                       (let [rej (if (map? (:rejected v)) (:rejected v) {})]
                                         (reduce + 0 (map counter-value (vals rej)))))
                                     (vals totals-tenant)))
         billed-total (reduce + 0 (map #(counter-value (:billed-tokens %)) (vals totals-tenant)))
         state' (assoc-in state [:act :latency-ms :avg] avg)]
     (assoc state'
            :kpi (telemetry-kpi state')
            :orchestration (telemetry-orchestration state')
            :lifecycle (telemetry/lifecycle-snapshot)
            :queue (telemetry/queue-snapshot)
            :tenancy {:requests requests-total
                      :errors errors-total
                      :rejected rejected-total
                      :billed-tokens billed-total
                      :filters (cond-> {}
                                 (keyword? tenant-filter) (assoc :tenant tenant-filter)
                                 (string? principal-filter) (assoc :principal principal-filter))
                      :by-tenant totals-tenant
                      :by-principal totals-principal}))))

(defn- request-explicit-cap-id
  [request]
  (or (keywordish (:cap/id request))
      (keywordish (get-in request [:task :cap/id]))))

(defn- resolve-capability-decision
  [resolver request]
  (let [intent       (some-> (get-in request [:task :intent]) keywordish)
        explicit-cap (request-explicit-cap-id request)
        routed-cap   (some-> resolver :routing :intent->cap (get intent) keywordish)
        node         (cond-> {:intent intent
                              :requires (get-in request [:task :requires])
                              :effects (:effects request)}
                       (or (keyword? explicit-cap) (keyword? routed-cap))
                       (assoc :dispatch {:candidates (cond-> []
                                                       (keyword? explicit-cap) (conj explicit-cap)
                                                       (keyword? routed-cap) (conj routed-cap))}))
        decision0    (workflow/resolve-capability-decision resolver node)]
    (cond-> decision0
      (keyword? explicit-cap) (assoc :requested-cap/id explicit-cap)
      (keyword? routed-cap) (assoc :routed-cap/id routed-cap))))

(defn- runtime-protocol-config
  [runtime]
  (runtime-svc/artifact-config runtime :protocol))

(defn- runtime-router-config
  [runtime]
  (runtime-svc/artifact-config runtime :router))

(defn- effective-resolver
  [runtime]
  (let [resolver   (or (:resolver runtime) {})
        runtime'   (if (map? runtime)
                     (assoc runtime :router (runtime-router-config runtime))
                     runtime)
        routing    (router/resolver-routing runtime' resolver)
        router-cfg (if (map? (:router runtime')) (:router runtime') {})]
    (cond-> resolver
      (map? routing)                   (assoc :routing routing)
      (contains? router-cfg :profiles) (assoc :profiles (:profiles router-cfg))
      (contains? router-cfg :policy)   (assoc :policy (:policy router-cfg))
      (contains? router-cfg :policy-profiles) (assoc :policy/profiles (:policy-profiles router-cfg))
      (contains? router-cfg :intent->policy-profile) (assoc :intent->policy-profile (:intent->policy-profile router-cfg))
      (contains? router-cfg :policy/profile) (assoc :policy/profile (:policy/profile router-cfg)))))

(defn- positive-int
  [v]
  (when (and (integer? v) (pos? v))
    (int v)))

(def ^:private default-act-cache-ttl-ms
  120000)

(def ^:private default-act-cache-max-size
  256)

(defn- default-act-cache-state
  []
  {:entries {}
   :order []})

(def ^:private default-replay-ttl-ms
  (* 24 60 60 1000))

(def ^:private default-replay-max-size
  512)

(def ^:private default-replay-redact-keys
  #{:password
    :secret
    :token
    :authorization
    :api-key
    :api/key})

(def ^:private replay-redacted-placeholder
  "[REDACTED]")

(defn- default-replay-state
  []
  {:entries {}
   :order []})

(def ^:private default-training-transcript-intents
  #{:text/respond
    :code/patch})

(def ^:private default-training-collector-config
  {:enabled? false
   :store/type :fs-jsonl
   :store/path "target/training/collector"
   :flush-policy :per-event
   :max-file-size-bytes (* 8 1024 1024)})

(def ^:private default-training-judge-config
  {:mode :disabled
   :constitution/ref nil
   :rules [:no-internal-markers
           :non-empty-output-text
           :accepted-consistent]})

(def ^:private default-training-redaction-config
  {:enabled? true
   :placeholder "[REDACTED]"
   :deny/keys [:password :secret :token :authorization :api-key :api/key :cookie :set-cookie :email :phone :phone-number]
   :deny/paths []
   :deny/patterns ["(?i)bearer\\s+[a-z0-9._\\-]+"
                   "(?i)api[_-]?key\\s*[:=]\\s*[^\\s,;]+"
                   "(?i)[a-z0-9._%+\\-]+@[a-z0-9.\\-]+\\.[a-z]{2,}"
                   "\\+?[0-9][0-9\\-\\s]{7,}[0-9]"]})

(def ^:private default-training-dataset-config
  {:split {:ratios {:train 0.8
                    :valid 0.1
                    :test 0.1}
           :seed 1337}
   :include-failed? false
   :idempotency {:enabled? true
                 :state-file ".dataset-state.json"
                 :source-checksum? true
                 :fail-on-config-change? false}})

(def ^:private default-training-export-config
  {:target-format :sft-prompt-completion
   :out-dir "target/training/dataset"
   :out-events "target/training/events-v1.jsonl"
   :out-train "target/training/train.jsonl"
   :sanity-check {:enabled? true
                  :row/fn nil}})

(def ^:private default-training-eval-config
  {:enabled? true
   :suites [:protocol-conformance
            :constitution-compliance
            :regression]
   :report {:include-cases? true
            :failed-only? false}
   :thresholds {:overall/pass-rate-min 0.85
                :suite-pass-rate-min {:protocol-conformance 0.90
                                      :constitution-compliance 0.90
                                      :regression 0.90}}})

(def ^:private default-training-promotion-config
  {:enabled? true
   :blocking? true
   :required-suites [:protocol-conformance
                     :constitution-compliance
                     :regression]
   :thresholds {:overall/pass-rate-min 0.85
                :suite-pass-rate-min {:protocol-conformance 0.90
                                      :constitution-compliance 0.90
                                      :regression 0.90}
                :max-failed-cases nil
                :max-failed-by-suite {}}})

(defn- normalize-training-dataset-config
  [v]
  (let [src (if (map? v) v {})
        split-src (if (map? (:split src))
                    (:split src)
                    {})
        idempotency-src (if (map? (:idempotency src))
                          (:idempotency src)
                          {})
        ratios-src (if (map? (:ratios split-src))
                     (:ratios split-src)
                     {})
        cfg (-> default-training-dataset-config
                (merge src)
                (assoc :split (merge (:split default-training-dataset-config)
                                     split-src))
                (assoc :idempotency
                       (merge (:idempotency default-training-dataset-config)
                              idempotency-src))
                (assoc-in [:split :ratios]
                          (merge (get-in default-training-dataset-config [:split :ratios])
                                 ratios-src)))]
    (cond-> cfg
      (contains? src :include-failed?)
      (assoc :include-failed? (true? (:include-failed? src)))

      (contains? idempotency-src :enabled?)
      (assoc-in [:idempotency :enabled?]
                (true? (:enabled? idempotency-src)))

      (contains? idempotency-src :source-checksum?)
      (assoc-in [:idempotency :source-checksum?]
                (true? (:source-checksum? idempotency-src)))

      (contains? idempotency-src :fail-on-config-change?)
      (assoc-in [:idempotency :fail-on-config-change?]
                (true? (:fail-on-config-change? idempotency-src))))))

(defn- normalize-training-export-config
  [v]
  (let [src (if (map? v) v {})
        sanity-src (if (map? (:sanity-check src))
                     (:sanity-check src)
                     {})
        cfg (-> default-training-export-config
                (merge src)
                (assoc :sanity-check
                       (merge (:sanity-check default-training-export-config)
                              sanity-src)))]
    (if (contains? sanity-src :enabled?)
      (assoc-in cfg [:sanity-check :enabled?]
                (true? (:enabled? sanity-src)))
      cfg)))

(defn- normalize-training-eval-config
  [v]
  (let [src (if (map? v) v {})
        report-src (if (map? (:report src))
                     (:report src)
                     {})
        thresholds-src (if (map? (:thresholds src))
                         (:thresholds src)
                         {})]
    (-> default-training-eval-config
        (merge src)
        (assoc :report (merge (:report default-training-eval-config)
                              report-src))
        (assoc :thresholds (merge (:thresholds default-training-eval-config)
                                  thresholds-src)))))

(defn- normalize-training-promotion-config
  [v]
  (let [src (if (map? v) v {})
        thresholds-src (if (map? (:thresholds src))
                         (:thresholds src)
                         {})]
    (-> default-training-promotion-config
        (merge src)
        (assoc :thresholds (merge (:thresholds default-training-promotion-config)
                                  thresholds-src)))))

(defn- normalize-act-response-cache
  [cfg]
  (let [src (if (map? (:response-cache cfg))
              (:response-cache cfg)
              {})
        enabled? (true? (:enabled? src))
        ttl-ms  (or (parse-non-negative-long (:ttl-ms src))
                    default-act-cache-ttl-ms)
        max-size (parse-positive-int (:max-size src) default-act-cache-max-size)
        state' (or (when (instance? clojure.lang.IAtom (:state src))
                     (:state src))
                   (atom (default-act-cache-state)))]
    {:enabled? enabled?
     :ttl-ms ttl-ms
     :max-size max-size
     :state state'}))

(defn- normalize-replay-redact-keys
  [v]
  (let [xs (cond
             (set? v) v
             (sequential? v) v
             (some? v) [v]
             :else [])]
    (->> xs
         (keep keywordish)
         set
         not-empty
         (or default-replay-redact-keys))))

(defn- normalize-replay-config
  [cfg]
  (let [src (if (map? (:replay cfg))
              (:replay cfg)
              {})
        enabled? (if (contains? src :enabled?)
                   (boolean (:enabled? src))
                   false)
        ttl-ms  (or (parse-non-negative-long (:ttl-ms src))
                    default-replay-ttl-ms)
        max-size (parse-positive-int (:max-size src) default-replay-max-size)
        state' (or (when (instance? clojure.lang.IAtom (:state src))
                     (:state src))
                   (atom (default-replay-state)))]
    {:enabled? enabled?
     :ttl-ms ttl-ms
     :max-size max-size
     :redact-keys (normalize-replay-redact-keys (:redact-keys src))
     :state state'}))

(defn- normalize-training-config
  [cfg]
  (let [cfg' (if (map? cfg) cfg {})
        src (if (map? (:training cfg'))
              (:training cfg')
              {})
        collector-raw (if (map? (:collector src))
                        (:collector src)
                        {})
        judge-raw (if (map? (:judge src))
                    (:judge src)
                    {})
        redaction-raw (if (map? (:redaction src))
                        (:redaction src)
                        {})
        dataset-raw (if (map? (:dataset src))
                      (:dataset src)
                      {})
        export-raw (if (map? (:export src))
                     (:export src)
                     {})
        eval-raw (if (map? (:eval src))
                   (:eval src)
                   {})
        promotion-raw (if (map? (:promotion src))
                        (:promotion src)
                      {})
        transcript-intents
        (if (contains? src :transcript/intents)
          (keyword-set (:transcript/intents src))
          default-training-transcript-intents)
        enabled? (or (map-bool-option src training-enabled-nested-keys)
                     (map-bool-option cfg' training-enabled-top-keys))]
    {:enabled? (true? enabled?)
     :transcript/intents transcript-intents
     :collector (training-collector/normalize-config
                 (merge default-training-collector-config collector-raw))
     :judge (merge default-training-judge-config judge-raw)
     :redaction (merge default-training-redaction-config redaction-raw)
     :dataset (normalize-training-dataset-config dataset-raw)
     :export (normalize-training-export-config export-raw)
     :eval (normalize-training-eval-config eval-raw)
     :promotion (normalize-training-promotion-config promotion-raw)}))

(defn- act-cache-runtime
  [runtime]
  (let [cache (when (map? runtime) (:response-cache runtime))]
    (when (map? cache) cache)))

(defn- replay-runtime
  [runtime]
  (let [replay (when (map? runtime) (:replay runtime))]
    (when (map? replay) replay)))

(defn- runtime-training-enabled?
  [runtime]
  (true? (get-in runtime [:training :enabled?])))

(defn- runtime-training-transcript-intents
  [runtime]
  (let [intents (get-in runtime [:training :transcript/intents])]
    (if (set? intents)
      intents
      default-training-transcript-intents)))

(defn- runtime-training-collector
  [runtime]
  (let [collector (get-in runtime [:training :collector])]
    (when (map? collector)
      collector)))

(defn- runtime-training-collector-instance
  [runtime]
  (let [instance (:instance (runtime-training-collector runtime))]
    (when (satisfies? training-collector/TrainingCollector instance)
      instance)))

(defn- request-training-override
  [request]
  (when (contains-path? request [:training :enabled?])
    (true? (get-in request [:training :enabled?]))))

(defn- training-enabled?
  ([runtime]
   (runtime-training-enabled? runtime))
  ([runtime request]
   (if (contains-path? request [:training :enabled?])
     (request-training-override request)
     (runtime-training-enabled? runtime))))

(defn- runtime-replay-enabled?
  [runtime]
  (true? (get-in runtime [:replay :enabled?])))

(defn- replay-enabled?
  ([runtime]
   (or (runtime-replay-enabled? runtime)
       (training-enabled? runtime)))
  ([runtime request]
   (if (contains-path? request [:replay :enabled?])
     (true? (get-in request [:replay :enabled?]))
     (or (training-enabled? runtime request)
         (runtime-replay-enabled? runtime)))))

(defn- training-transcript-enabled-for-request?
  [runtime request]
  (let [intent (some-> request :task :intent keywordish)
        intents (runtime-training-transcript-intents runtime)]
    (and (keyword? intent)
         (or (contains? intents intent)
             (contains? intents :all)
             (contains? intents :*)))))

(defn- apply-request-training-defaults
  [runtime request]
  (if-not (map? request)
    request
    (let [training? (training-enabled? runtime request)
          transcript-enabled? (and training?
                                   (training-transcript-enabled-for-request?
                                    runtime
                                    request))
          replay-explicit? (contains-path? request [:replay :enabled?])
          transcript-explicit? (contains-path? request [:routing :debug/transcript?])]
      (cond-> request
        (and training? (not replay-explicit?))
        (assoc-in [:replay :enabled?] true)

        (and transcript-enabled? (not transcript-explicit?))
        (assoc-in [:routing :debug/transcript?] true)))))

(defn- act-cache-enabled?
  [runtime]
  (true? (get-in runtime [:response-cache :enabled?])))

(defn- order-without
  [order key']
  (->> (or order [])
       (remove #(= % key'))
       vec))

(defn- prune-expired-cache-state
  [state now-ms]
  (let [entries (or (:entries state) {})
        order   (or (:order state) [])
        [entries' expired]
        (reduce-kv (fn [[acc removed] k entry]
                     (let [expires-at (or (parse-non-negative-long (:expires-at entry)) 0)]
                       (if (and (pos? expires-at) (>= now-ms expires-at))
                         [acc (inc removed)]
                         [(assoc acc k entry) removed])))
                   [{} 0]
                   entries)]
    {:state (assoc state
                   :entries entries'
                   :order (vec (filter #(contains? entries' %) order)))
     :evicted expired}))

(defn- prune-cache-size-state
  [state max-size]
  (let [max-size' (max 1 (int (or max-size default-act-cache-max-size)))
        entries (or (:entries state) {})
        order   (vec (or (:order state) []))
        overflow (max 0 (- (count order) max-size'))
        evict-keys (if (pos? overflow) (subvec order 0 overflow) [])
        entries' (if (seq evict-keys)
                   (apply dissoc entries evict-keys)
                   entries)
        order' (if (pos? overflow)
                 (subvec order overflow)
                 order)]
    {:state (assoc state :entries entries' :order (vec order'))
     :evicted (count evict-keys)}))

(defn- replay-prune-expired-state
  [state now-ms]
  (let [entries (or (:entries state) {})
        order   (or (:order state) [])
        [entries' expired]
        (reduce-kv (fn [[acc removed] k entry]
                     (let [expires-at (or (parse-non-negative-long (:expires-at entry)) 0)]
                       (if (and (pos? expires-at) (>= now-ms expires-at))
                         [acc (inc removed)]
                         [(assoc acc k entry) removed])))
                   [{} 0]
                   entries)]
    {:state (assoc state
                   :entries entries'
                   :order (vec (filter #(contains? entries' %) order)))
     :evicted expired}))

(defn- replay-prune-size-state
  [state max-size]
  (let [max-size' (max 1 (int (or max-size default-replay-max-size)))
        entries   (or (:entries state) {})
        order     (vec (or (:order state) []))
        overflow  (max 0 (- (count order) max-size'))
        evict-keys (if (pos? overflow) (subvec order 0 overflow) [])
        entries'  (if (seq evict-keys)
                    (apply dissoc entries evict-keys)
                    entries)
        order'    (if (pos? overflow)
                    (subvec order overflow)
                    order)]
    {:state (assoc state :entries entries' :order (vec order'))
     :evicted (count evict-keys)}))

(defn- replay-redact
  [v redact-keys]
  (cond
    (map? v)
    (reduce-kv (fn [acc k value]
                 (let [k' (keywordish k)]
                   (assoc acc k
                          (if (and (keyword? k')
                                   (contains? redact-keys k'))
                            replay-redacted-placeholder
                            (replay-redact value redact-keys)))))
               {}
               v)

    (vector? v)
    (mapv #(replay-redact % redact-keys) v)

    (sequential? v)
    (into [] (map #(replay-redact % redact-keys)) v)

    :else v))

(defn- act-cache-key
  [request cap-id]
  (when (and (map? request) (keyword? cap-id))
    (let [auth-user (auth-user-public (:auth/user request))
          auth-key (when (map? auth-user)
                     (cond-> {}
                       (some? (:user/id auth-user)) (assoc :user/id (:user/id auth-user))
                       (some? (:user/account-type auth-user)) (assoc :user/account-type (:user/account-type auth-user))
                       (seq (:user/roles auth-user)) (assoc :user/roles (vec (sort (:user/roles auth-user))))))]
      {:intent (get-in request [:task :intent])
       :cap/id cap-id
       :role (:role request)
       :requires (get-in request [:task :requires])
       :model (or (:model request) (get-in request [:task :model]))
       :session/id (or (some-> (:session/id request) trim-s)
                       (some-> (:session-id request) trim-s))
       :auth auth-key
       :input (:input request)
       :context (:context request)
       :constraints (:constraints request)
       :done (:done request)
       :effects (:effects request)
       :response/type (:response/type request)})))

(defn- cacheable-act-request?
  [request]
  (and (map? request)
       (not= :accepted (keywordish (:response/type request)))
       (not (true? (:stream? request)))
       (not= :stream (keywordish (:response/type request)))))

(defn- cacheable-act-response?
  [response]
  (and (map? response)
       (= 200 (int (or (:status response) 500)))
       (not (some? (get-in response [:body :error])))
       (= :value (contracts/result-type-of (:body response)))))

(defn- act-cache-get!
  [runtime cache-key]
  (if-not (and (act-cache-enabled? runtime)
               cache-key)
    {:hit? false
     :response nil
     :telemetry {}}
    (let [cache (act-cache-runtime runtime)
          now-ms (System/currentTimeMillis)
          state-atom (:state cache)
          expired (atom 0)]
      (swap! state-atom
             (fn [state]
               (let [{state' :state evicted :evicted}
                     (prune-expired-cache-state (if (map? state) state (default-act-cache-state))
                                               now-ms)]
                 (reset! expired evicted)
                 state')))
      (let [entry (get-in @state-atom [:entries cache-key])]
        (if (map? entry)
          (do
           (swap! state-atom
                   (fn [state]
                     (-> state
                         (assoc-in [:entries cache-key :last-access-at] now-ms)
                         (update :order (fn [order]
                                          (conj (order-without order cache-key) cache-key)))))
                   )
            {:hit? true
             :response (:response entry)
             :telemetry (cond-> {:lookups 1
                                 :hits 1}
                          (pos? @expired) (assoc :evictions @expired))})
          {:hit? false
           :response nil
           :telemetry (cond-> {:lookups 1
                               :misses 1}
                        (pos? @expired) (assoc :evictions @expired))})))))

(defn- act-cache-put!
  [runtime cache-key response]
  (if-not (and (act-cache-enabled? runtime)
               cache-key
               (cacheable-act-response? response))
    {}
    (let [cache (act-cache-runtime runtime)
          ttl-ms (or (parse-non-negative-long (:ttl-ms cache)) default-act-cache-ttl-ms)
          max-size (or (positive-int (:max-size cache)) default-act-cache-max-size)
          now-ms (System/currentTimeMillis)
          expires-at (+ now-ms ttl-ms)
          state-atom (:state cache)
          evictions (atom 0)]
      (swap! state-atom
             (fn [state]
               (let [base (if (map? state) state (default-act-cache-state))
                     {:keys [state evicted]} (prune-expired-cache-state base now-ms)
                     evicted-expired evicted
                     entry {:response response
                            :created-at now-ms
                            :last-access-at now-ms
                            :expires-at expires-at}
                     state' (-> state
                                (assoc-in [:entries cache-key] entry)
                                (update :order (fn [order]
                                                 (conj (order-without order cache-key) cache-key))))
                     {:keys [state evicted]} (prune-cache-size-state state' max-size)
                     evicted-size evicted]
                 (reset! evictions (+ evicted-expired evicted-size))
                 state)))
      (cond-> {:stores 1}
        (pos? @evictions) (assoc :evictions @evictions)))))

(defn- act-cache-invalidate-session!
  [runtime session-id]
  (if-not (and (act-cache-enabled? runtime)
               (some? (trim-s session-id)))
    0
    (let [cache (act-cache-runtime runtime)
          sid (trim-s session-id)
          state-atom (:state cache)
          removed (atom 0)]
      (swap! state-atom
             (fn [state]
               (let [entries (or (:entries state) {})
                     keys-to-drop (->> entries
                                       (keep (fn [[k _]]
                                               (when (= sid (:session/id k)) k)))
                                       vec)]
                 (reset! removed (count keys-to-drop))
                 (if (seq keys-to-drop)
                   (-> state
                       (update :entries #(apply dissoc (or % {}) keys-to-drop))
                       (update :order (fn [order]
                                        (vec (remove (set keys-to-drop) (or order []))))))
                   state))))
      @removed)))

(defn- request-debug-plan?
  [request]
  (true? (get-in request [:routing :debug/plan?])))

(defn- request-debug-transcript?
  [request]
  (true? (get-in request [:routing :debug/transcript?])))

(declare request-routing-config)
(declare effective-routing-config)
(declare routing-profiles)

(defn- resolve-routing-policy-profile
  [runtime resolver request intent]
  (let [router-cfg (if (map? (:router runtime)) (:router runtime) {})
        routing-cfg (effective-routing-config runtime request)
        request-cfg (request-routing-config request)
        request-policy-explicit? (or (contains? request-cfg :policy/profile)
                                     (contains? request-cfg :policy-profile)
                                     (contains? request-cfg :profile))
        strict-policy-profile (when (and (true? (:strict? request-cfg))
                                         (not request-policy-explicit?))
                                (let [profiles (routing-profiles runtime)
                                      strict-cfg (or (when (map? profiles)
                                                       (get profiles :strict-meta))
                                                     (when (map? profiles)
                                                       (get profiles :strict)))]
                                  (some-> strict-cfg :policy/profile keywordish)))
        intent->profile (if (map? (:intent->policy-profile router-cfg))
                          (:intent->policy-profile router-cfg)
                          {})
        routed-profile (when (keyword? intent)
                         (some-> (get intent->profile intent) keywordish))]
    (or (some-> request-cfg :policy/profile keywordish)
        (some-> request-cfg :policy-profile keywordish)
        strict-policy-profile
        (some-> routing-cfg :policy/profile keywordish)
        (some-> routing-cfg :policy-profile keywordish)
        routed-profile
        (some-> resolver :policy/profile keywordish)
        :balanced)))

(defn- resolve-routing-policy-profiles
  [runtime resolver]
  (let [router-cfg (if (map? (:router runtime)) (:router runtime) {})
        profiles (or (when (map? (:policy-profiles router-cfg))
                       (:policy-profiles router-cfg))
                     (when (map? (:policy/profiles resolver))
                       (:policy/profiles resolver))
                     (when (map? (:policy-profiles resolver))
                       (:policy-profiles resolver)))]
    (if (map? profiles) profiles {})))

(defn- resolve-workflow-limit
  [policy-cfg k]
  (let [raw (or (get-in policy-cfg [:limits k])
                (get policy-cfg k))
        n (parse-positive-int raw -1)]
    (when (pos-int? n)
      n)))

(def ^:private context-summary-system-max-chars
  600)

(def ^:private context-summary-system-prefix
  "Context summary from previous turns. Reuse only if relevant and do not contradict current user prompt:\n")

(defn- context-summary->system
  [summary]
  (let [s (some-> summary trim-s)]
    (when (some? s)
      (str context-summary-system-prefix
           (if (> (count s) context-summary-system-max-chars)
             (subs s 0 context-summary-system-max-chars)
             s)))))

(defn- enrich-input-with-context-summary
  [input context]
  (let [input' (if (map? input) input {})
        summary (when (map? context) (:summary context))
        system-fragment (context-summary->system summary)]
    (if (or (nil? system-fragment)
            (some? (trim-s (:system input'))))
      input'
      (assoc input' :system system-fragment))))

(defn- act-request->invoke-opts
  [runtime resolver request cap-id]
  (let [intent        (get-in request [:task :intent])
        budget        (:budget request)
        auth-user     (auth-user-public (:auth/user request))
        auth-source-k (or (some-> (:auth/source request) keywordish)
                          (some-> (get-in request [:auth :source]) keywordish)
                          :http/basic)
        base-context  (if (map? (:context request)) (:context request) {})
        context'      (cond-> base-context
                        (map? auth-user) (assoc :auth/user auth-user))
        policy-profile (resolve-routing-policy-profile runtime resolver request intent)
        policy-profiles (resolve-routing-policy-profiles runtime resolver)
        runtime-protocol (runtime-protocol-config runtime)
        protocol-selection (protocol/select-protocol-artifact
                            runtime-protocol
                            {:trace-id (get-in request [:trace :id])
                             :requested-version (some-> request :routing :artifact/version)})
        selected-protocol (:protocol protocol-selection)
        selected-protocol-version (:artifact/version protocol-selection)
        selected-protocol-source (:artifact/source protocol-selection)
        selected-router-version (some-> (:router/artifact-version runtime) keywordish)
        selected-router-source (some-> (:router/artifact-source runtime) keywordish)
        policy-cfg   (if (and (keyword? policy-profile)
                              (map? policy-profiles))
                       (get policy-profiles policy-profile)
                       nil)
        max-call-attempts (when (map? policy-cfg)
                            (resolve-workflow-limit policy-cfg :call-tree/max-calls))
        max-fallback-hops (when (map? policy-cfg)
                            (resolve-workflow-limit policy-cfg :call-tree/max-fallback-hops))
        session-meta  (when (map? auth-user)
                        (cond-> {:auth/source auth-source-k}
                          (some? (:user/id auth-user))           (assoc :user/id (:user/id auth-user))
                          (some? (:user/email auth-user))        (assoc :user/email (:user/email auth-user))
                          (some? (:user/account-type auth-user)) (assoc :user/account-type (:user/account-type auth-user))
                          (seq   (:user/roles auth-user))        (assoc :user/roles (:user/roles auth-user))))]
    (cond-> {:role            (or (:role request)
                                  (cap-id->role runtime resolver cap-id intent))
             :intent          intent
             :cap-id          cap-id
             :input           (enrich-input-with-context-summary
                               (:input request)
                               context')
             :context         context'
             :constraints     (:constraints request)
             :done            (:done request)
             :budget          budget
             :effects         (:effects request)
             :requires        (get-in request [:task :requires])
             :request-id      (:request/id request)
             :trace           (:trace request)
             :proto           (:proto request)
             :session-id      (:session/id request)
             :session-version (:session/version request)
             :policy/profile  policy-profile}
      (some? (:model request))                        (assoc :model (:model request))
      (some? (get-in request [:task :model]))         (assoc :model (get-in request [:task :model]))
      (keyword? (:response/type request))             (assoc :response/type (:response/type request))
      (contains? request :stream?)                    (assoc :stream? (boolean (:stream? request)))
      (some? (positive-int (:max-roundtrips budget))) (assoc :max-attempts (positive-int (:max-roundtrips budget)))
      (some? (positive-int (:max-tokens budget)))     (assoc :max-tokens (positive-int (:max-tokens budget)))
      (some? (positive-int (:timeout-ms request)))    (assoc :timeout-ms (positive-int (:timeout-ms request)))
      (number? (:top-p budget))                       (assoc :top-p (double (:top-p budget)))
      (some? (:temperature budget))                   (assoc :temperature (:temperature budget))
      (map? auth-user)                                (assoc :auth/user auth-user)
      (map? (:roles runtime))                         (assoc :roles (:roles runtime))
      (map? selected-protocol)                        (assoc :protocol selected-protocol)
      (keyword? selected-protocol-version)            (assoc :protocol/artifact-version selected-protocol-version)
      (keyword? selected-protocol-source)             (assoc :protocol/artifact-source selected-protocol-source)
      (keyword? selected-router-version)              (assoc :router/artifact-version selected-router-version)
      (keyword? selected-router-source)               (assoc :router/artifact-source selected-router-source)
      (map? policy-profiles)                          (assoc :policy/profiles policy-profiles)
      (pos-int? max-call-attempts)                    (assoc :workflow/max-calls max-call-attempts)
      (pos-int? max-fallback-hops)                    (assoc :workflow/max-fallback-hops max-fallback-hops)
      (map? session-meta)                             (assoc :session/meta session-meta)
      (request-debug-plan? request)                   (assoc :debug/plan? true)
      (request-debug-transcript? request)             (assoc :debug/transcript? true))))

(def ^:private fallback-request-default-bindings
  {:session/language
   {:target [:constraints :language]
    :coerce :keyword-or-string}
   :session/style
   {:target [:constraints :style]
    :coerce :keyword-or-string}
   :session/system-prompt
   {:target [:input :system]
    :coerce :trimmed-string}
   :session/context-summary
   {:target [:context :summary]
    :coerce :trimmed-string}})

(defn- session-var-value
  [vars k]
  (or (get vars k)
      (get vars (name k))
      (get vars (str (namespace k) "/" (name k)))))

(defn- session-request-default-bindings
  [runtime]
  (let [service (when (map? runtime) (:session runtime))
        store (when (map? service) (:store service))
        bindings (when (map? store)
                   (memory/request-default-bindings store))]
    (if (seq bindings)
      bindings
      fallback-request-default-bindings)))

(defn- session-memory-policy
  [runtime]
  (let [service (when (map? runtime) (:session runtime))
        store (when (map? service) (:store service))
        policy (when (map? store)
                 (memory/session-memory-policy store))]
    (if (map? policy) policy {})))

(defn- coerce-session-default-value
  [coerce raw]
  (case (keywordish coerce)
    :identity raw
    :keyword (keywordish raw)
    :trimmed-string (trim-s raw)
    :string (some-> raw str)
    :keyword-or-string (or (keywordish raw) (trim-s raw))
    (or (keywordish raw) (trim-s raw))))

(defn- apply-session-var-defaults
  [request vars bindings]
  (reduce-kv (fn [req k binding]
               (let [target (when (map? binding) (:target binding))
                     target' (when (or (vector? target)
                                       (sequential? target))
                               (vec target))
                     raw (session-var-value vars k)
                     value (coerce-session-default-value
                            (when (map? binding) (:coerce binding))
                            raw)]
                 (if (and (seq target')
                          (nil? (get-in req target'))
                          (some? value))
                   (assoc-in req target' value)
                   req)))
             request
             (if (map? bindings) bindings {})))

(def ^:private session-get-vars-invokers-max-size
  256)

(def ^:private session-get-vars-invokers
  (atom {:entries {}
         :order []}))

(defn- prune-session-get-vars-invokers
  [cache]
  (let [entries (if (map? (:entries cache)) (:entries cache) {})
        order0  (if (vector? (:order cache))
                  (:order cache)
                  (vec (keys entries)))
        order1  (->> order0
                     (filter #(contains? entries %))
                     vec)
        overflow (max 0 (- (count order1) session-get-vars-invokers-max-size))
        evict-keys (if (pos? overflow) (subvec order1 0 overflow) [])
        entries' (if (seq evict-keys)
                   (apply dissoc entries evict-keys)
                   entries)
        order' (if (pos? overflow)
                 (subvec order1 overflow)
                 order1)]
    {:entries entries'
     :order (vec order')}))

(defn- make-session-get-vars-invoker
  [get-vars-fn]
  (let [mode (volatile! :auto)]
    (fn [sid ks opts]
      (case @mode
        :with-opts (get-vars-fn sid ks opts)
        :plain (get-vars-fn sid ks)
        (try
          (let [result (get-vars-fn sid ks opts)]
            (vreset! mode :with-opts)
            result)
          (catch clojure.lang.ArityException _
            (vreset! mode :plain)
            (get-vars-fn sid ks)))))))

(defn- session-get-vars-invoker
  [service]
  (let [get-vars-fn (when (map? service) (:get-vars! service))]
    (when (fn? get-vars-fn)
      (or (get-in @session-get-vars-invokers [:entries get-vars-fn])
          (let [invoker (make-session-get-vars-invoker get-vars-fn)
                cache' (swap! session-get-vars-invokers
                              (fn [cache]
                                (let [entries (if (map? (:entries cache))
                                                (:entries cache)
                                                {})]
                                  (if (contains? entries get-vars-fn)
                                    (prune-session-get-vars-invokers cache)
                                    (let [order0 (if (vector? (:order cache))
                                                   (:order cache)
                                                   (vec (keys entries)))
                                          order1 (conj (->> order0
                                                            (remove #(identical? % get-vars-fn))
                                                            vec)
                                                       get-vars-fn)]
                                      (prune-session-get-vars-invokers
                                       {:entries (assoc entries get-vars-fn invoker)
                                        :order order1}))))))
                cached (get-in cache' [:entries get-vars-fn])]
            (or cached invoker))))))

(defn- record-context-defaults-telemetry!
  [runtime lookups hits misses]
  (let [telemetry (when (map? runtime) (:telemetry runtime))
        lookups' (counter-value lookups)
        hits' (counter-value hits)
        misses' (counter-value misses)]
    (when (instance? clojure.lang.IAtom telemetry)
      (swap! telemetry
             (fn [state]
               (-> (telemetry/merge-counters (default-telemetry) state)
                   (update-in [:orchestration :context/default-lookups] (fnil + 0) lookups')
                   (update-in [:orchestration :context/default-hits] (fnil + 0) hits')
                   (update-in [:orchestration :context/default-misses] (fnil + 0) misses')))))))

(defn- record-context-principal-isolation-telemetry!
  [runtime blocked]
  (let [telemetry (when (map? runtime) (:telemetry runtime))
        blocked' (counter-value blocked)]
    (when (and (instance? clojure.lang.IAtom telemetry)
               (pos? blocked'))
      (swap! telemetry
             (fn [state]
               (-> (telemetry/merge-counters (default-telemetry) state)
                   (update-in [:orchestration :context/principal-isolation-blocked] (fnil + 0) blocked')))))))

(defn- memory-read-enabled?
  [policy intent]
  (let [by-intent (if (map? (:read/by-intent policy))
                    (:read/by-intent policy)
                    {})
        default? (if (contains? policy :read/default?)
                   (true? (:read/default? policy))
                   true)]
    (if (contains? by-intent intent)
      (true? (get by-intent intent))
      default?)))

(defn- binding-target
  [binding]
  (let [target (when (map? binding) (:target binding))]
    (when (or (vector? target)
              (sequential? target))
      (vec target))))

(defn- context-binding-key?
  [bindings k]
  (let [target (binding-target (get bindings k))]
    (= :context (first target))))

(defn- context-binding-keys
  [bindings]
  (->> (keys (if (map? bindings) bindings {}))
       (filter #(context-binding-key? bindings %))
       vec))

(defn- principal-token-from-request
  [request]
  (let [user (when (map? request)
               (:auth/user request))]
    (or (some-> (:user/id user) str trim-s)
        (some-> (:user/email user) str trim-s str/lower-case))))

(defn- principal-token-from-vars
  [vars principal-key]
  (when (keyword? principal-key)
    (some-> (session-var-value vars principal-key) str trim-s)))

(defn- request-with-session-defaults
  [runtime request]
  (let [service (when (map? runtime) (:session runtime))
        get-vars-invoker (session-get-vars-invoker service)
        bindings (session-request-default-bindings runtime)
        memory-policy (session-memory-policy runtime)
        intent  (some-> request :task :intent keywordish)
        memory-read? (memory-read-enabled? memory-policy intent)
        context-keys (context-binding-keys bindings)
        bindings' (if memory-read?
                    bindings
                    (apply dissoc bindings context-keys))
        binding-keys (->> (keys bindings')
                          (keep keywordish)
                          vec)
        principal-isolation? (and memory-read?
                                  (true? (:principal/isolation? memory-policy)))
        principal-key (or (some-> (:principal/key memory-policy) keywordish)
                          :context/principal-id)
        lookup-keys (cond-> binding-keys
                      (and principal-isolation?
                           (keyword? principal-key))
                      (conj principal-key))
        sid     (or (some-> request :session/id trim-s)
                    (some-> request :session-id trim-s))
        opts    (cond-> {:operation :act/defaults}
                  (keyword? intent) (assoc :intent intent))]
    (if (and (map? request)
             (map? service)
             (fn? get-vars-invoker)
             sid
             (seq lookup-keys))
      (let [vars (try
                   (get-vars-invoker sid lookup-keys opts)
                   (catch Throwable _
                     nil))]
        (if (map? vars)
          (let [request-principal (principal-token-from-request request)
                stored-principal (principal-token-from-vars vars principal-key)
                principal-mismatch? (and principal-isolation?
                                         (some? stored-principal)
                                         (not= stored-principal request-principal))
                blocked-count (if principal-mismatch?
                                (count context-keys)
                                0)
                bindings'' (if principal-mismatch?
                             (apply dissoc bindings' context-keys)
                             bindings')
                binding-keys' (->> (keys bindings'')
                                   (keep keywordish)
                                   vec)
                hits (count (filter #(contains? vars %) binding-keys'))
                lookups (count binding-keys')
                misses (max 0 (- lookups hits))]
            (record-context-defaults-telemetry! runtime lookups hits misses)
            (record-context-principal-isolation-telemetry! runtime blocked-count)
            (apply-session-var-defaults request vars bindings''))
          (do
            (record-context-defaults-telemetry! runtime (count binding-keys) 0 (count binding-keys))
            request)))
      request)))

(defn- runtime-tenancy-config
  [runtime]
  (if (map? runtime)
    (tenancy/normalize-config (:tenancy runtime))
    (tenancy/normalize-config nil)))

(defn- runtime-tenancy-state
  [runtime]
  (when (map? runtime)
    (:tenancy/state runtime)))

(defn- request-with-tenancy-defaults
  [runtime request]
  (let [cfg (runtime-tenancy-config runtime)
        ctx (tenancy/resolve-context cfg request)]
    (tenancy/apply-request-defaults request ctx)))

(def ^:private queue-job-accepted-public-keys
  [:job/id :job/status :submitted-at :updated-at :deadline-at :queue/class :attempt])

(def ^:private queue-job-status-public-keys
  [:job/id :job/status :submitted-at :updated-at :started-at :completed-at
   :deadline-at :queue/class :attempt :result :error :cancel/reason])

(def ^:private queue-job-cancel-public-keys
  [:job/id :job/status :cancel/accepted? :updated-at])

(defn- value-envelope
  [request out]
  {:proto (request-proto request)
   :trace (request-trace request)
   :result {:type :value
            :out out}})

(defn- accepted-envelope
  [request out]
  (assoc (value-envelope request out) :response/type :accepted))

(defn- response-type-accepted?
  [request]
  (= :accepted (keywordish (:response/type request))))

(defn- runtime-queue-service
  [runtime]
  (when (map? runtime)
    (:queue/service runtime)))

(defn- runtime-execution-graph-service
  [runtime]
  (when (map? runtime)
    (:execution-graph/service runtime)))

(defn- append-execution-graph-event!
  [runtime event]
  (let [service (runtime-execution-graph-service runtime)]
    (when (execution-graph/service? service)
      (execution-graph/append-event! service event))))

(defn- queue-submit-options
  [request]
  (let [queue-map (if (map? (:queue request)) (:queue request) {})
        task-map (if (map? (:task request)) (:task request) {})
        class-k (or (keywordish (:queue/class request))
                    (keywordish (:queue/class queue-map))
                    (keywordish (:class queue-map))
                    (keywordish (:queue/class task-map)))
        deadline-ms (or (parse-non-negative-long (:deadline-ms request))
                        (parse-non-negative-long (:deadline-ms queue-map))
                        (parse-non-negative-long (:job/deadline-ms queue-map))
                        (parse-non-negative-long (:deadline-ms task-map)))]
    (cond-> {}
      (keyword? class-k) (assoc :queue/class class-k)
      (some? deadline-ms) (assoc :deadline-ms deadline-ms))))

(defn- queue-job->accepted-payload
  [job]
  (select-keys job queue-job-accepted-public-keys))

(defn- queue-job->status-payload
  [job]
  (select-keys job queue-job-status-public-keys))

(defn- queue-job->cancel-payload
  [job accepted?]
  (-> {:job/id (:job/id job)
       :job/status (:job/status job)
       :cancel/accepted? (boolean accepted?)}
      (cond-> (contains? job :updated-at)
        (assoc :updated-at (:updated-at job)))
      (select-keys queue-job-cancel-public-keys)))

(defn- queue-schema-response
  [request schema-k payload status accepted?]
  (let [check (contracts/validate-schema schema-k payload)]
    (if (:ok? check)
      {:status status
       :body   (if accepted?
                 (accepted-envelope request payload)
                 (value-envelope request payload))}
      {:status 500
       :body   (error-envelope request
                               :runtime/internal
                               "Queue response payload failed schema validation."
                               {:schema schema-k
                                :reason (:reason check)})})))

(defn- async-submit-response
  [runtime request]
  (let [service (runtime-queue-service runtime)]
    (if-not (queue/service? service)
      {:status 503
       :body   (error-envelope request
                               :queue/unavailable
                               "Asynchronous queue is unavailable."
                               {:error :queue/not-initialized}
                               true)}
      (let [submit (queue/submit! service request (queue-submit-options request))]
        (cond
          (:ok? submit)
          (let [job (:job submit)
                _   (append-execution-graph-event!
                     runtime
                     {:event/type :job/submitted
                      :job/id (:job/id job)
                      :run/id (:job/id job)
                      :trace/id (some-> request :trace :id trim-s)
                      :session/id (or (some-> request :session/id trim-s)
                                      (some-> request :session-id trim-s))
                      :request request
                      :queue/class (:queue/class job)
                      :deadline-at (:deadline-at job)
                      :attempt (:attempt job)})
                ]
            (queue-schema-response request
                                   :res/job-accepted
                                   (queue-job->accepted-payload job)
                                   202
                                   true))

          (= :queue/full (:error submit))
          {:status 503
           :body   (error-envelope request
                                   :runtime/overloaded
                                   "Queue is full."
                                   {:error :queue/full
                                    :max-size (:max-size submit)}
                                   true)}

          (= :queue/disabled (:error submit))
          {:status 503
           :body   (error-envelope request
                                   :queue/unavailable
                                   "Asynchronous queue is disabled."
                                   {:error :queue/disabled}
                                   true)}

          :else
          {:status 500
           :body   (error-envelope request
                                   :runtime/internal
                                   "Failed to submit async job."
                                   {:error (:error submit)})})))))

(defn- queue-job-status-response
  [runtime request job-id]
  (let [service (runtime-queue-service runtime)]
    (if-not (queue/service? service)
      {:status 503
       :body   (error-envelope request
                               :queue/unavailable
                               "Asynchronous queue is unavailable."
                               {:error :queue/not-initialized}
                               true)}
      (let [poll (queue/poll! service job-id)]
        (cond
          (:ok? poll)
          (queue-schema-response request
                                 :res/job-status
                                 (queue-job->status-payload (:job poll))
                                 200
                                 false)

          (= :queue/job-not-found (:error poll))
          {:status 404
           :body   (error-envelope request
                                   :queue/job-not-found
                                   "Queue job not found."
                                   {:job/id (:job/id poll)})}

          :else
          {:status 500
           :body   (error-envelope request
                                   :runtime/internal
                                   "Failed to poll queue job."
                                   {:error (:error poll)
                                    :job/id job-id})})))))

(defn- queue-job-cancel-response
  [runtime request job-id reason]
  (let [service (runtime-queue-service runtime)]
    (if-not (queue/service? service)
      {:status 503
       :body   (error-envelope request
                               :queue/unavailable
                               "Asynchronous queue is unavailable."
                               {:error :queue/not-initialized}
                               true)}
      (let [cancel (queue/cancel! service job-id reason)]
        (cond
          (:ok? cancel)
          (let [job (:job cancel)
                _ (append-execution-graph-event!
                   runtime
                   {:event/type :job/canceled
                    :job/id (:job/id job)
                    :run/id (:job/id job)
                    :trace/id (some-> request :trace :id trim-s)
                    :session/id (or (some-> request :session/id trim-s)
                                    (some-> request :session-id trim-s))
                    :attempt (:attempt job)
                    :details {:reason reason}})]
            (queue-schema-response request
                                   :res/job-cancel
                                   (queue-job->cancel-payload job true)
                                   200
                                   false))

          (= :queue/job-not-found (:error cancel))
          {:status 404
           :body   (error-envelope request
                                   :queue/job-not-found
                                   "Queue job not found."
                                   {:job/id (:job/id cancel)})}

          (= :queue/invalid-transition (:error cancel))
          (let [current (queue/get-job service job-id)]
            (if (:ok? current)
              (queue-schema-response request
                                     :res/job-cancel
                                     (queue-job->cancel-payload (:job current) false)
                                     200
                                     false)
              {:status 404
               :body   (error-envelope request
                                       :queue/job-not-found
                                       "Queue job not found."
                                       {:job/id job-id})}))

          :else
          {:status 500
           :body   (error-envelope request
                                   :runtime/internal
                                   "Failed to cancel queue job."
                                   {:error (:error cancel)
                                    :job/id job-id})})))))

(defn- response-error-type
  [response]
  (let [body (if (map? response) (:body response) nil)]
    (or (some-> body :error :type keywordish)
        (some-> body :result :error :type keywordish))))

(defn- response-outcome
  [response]
  (if (< (int (or (:status response) 500)) 400)
    :ok
    :error))

(defn- audit-principal
  [auth request]
  (or (auth-user-public (some-> auth :user))
      (auth-user-public (:auth/user request))))

(defn- report-act!
  [runtime request response auth elapsed-ms]
  (let [logger (oplog/logger :act runtime)]
    (when (fn? logger)
      (let [principal (audit-principal auth request)
            trace-id (some-> request :trace :id trim-s)
            request-id (some-> request :request/id trim-s)
            session-id (or (some-> request :session/id trim-s)
                           (some-> request :session-id trim-s))
            intent (some-> request :task :intent keywordish)
            capability (or (some-> request :task :cap/id keywordish)
                           (some-> request :cap/id keywordish))
            tenant-id (some-> request :tenant/id keywordish)
            principal-ref (some-> request :principal/ref trim-s)
            status (int (or (:status response) 500))
            outcome (response-outcome response)
            error-type (response-error-type response)
            message (or (some-> response :body :error :message trim-s)
                        (when (= :ok outcome) "Request processed.")
                        "Request failed.")]
        (apply logger
               (mapcat identity
                       (cond-> {:trace-id trace-id
                                :request-id request-id
                                :session-id session-id
                                :intent intent
                                :capability capability
                                :outcome outcome
                                :status status
                                :error-type error-type
                                :latency-ms elapsed-ms
                                :message message}
                         (keyword? tenant-id)
                         (assoc :tenant-id tenant-id)
                         (string? principal-ref)
                         (assoc :principal-ref principal-ref)
                         (some? (:user/id principal))
                         (assoc :principal-id (:user/id principal))
                         (some? (:user/email principal))
                         (assoc :principal-email (:user/email principal))
                         (some? (:user/account-type principal))
                         (assoc :principal-account-type (:user/account-type principal))
                         (seq (:user/roles principal))
                         (assoc :principal-roles (vec (:user/roles principal))))))))))

(defn- replay-trace-id
  [phase response]
  (or (some-> phase :request* :trace :id trim-s)
      (some-> phase :request :trace :id trim-s)
      (some-> phase :payload :trace :id trim-s)
      (some-> response :body :trace :id trim-s)))

(defn- replay-policy-snapshot
  [phase]
  (let [intent   (some-> phase :request* :task :intent keywordish)
        resolver (if (map? (:resolver phase)) (:resolver phase) {})
        protocol (if (map? (:protocol phase)) (:protocol phase) {})
        routing  (if (map? (:routing resolver)) (:routing resolver) {})
        defaults (if (map? (:defaults routing)) (:defaults routing) {})
        intent->cap (if (map? (:intent->cap routing)) (:intent->cap routing) {})
        fallback (if (sequential? (:fallback routing))
                   (vec (keep keywordish (:fallback routing)))
                   [])
        snapshot
        (cond-> {:intent intent
                 :routing {:defaults defaults
                           :intent/cap (when (keyword? intent)
                                         (get intent->cap intent))
                           :fallback fallback
                           :policy/profile (or (some-> phase :request* :routing :profile keywordish)
                                               (some-> phase :request* :policy/profile keywordish)
                                               (some-> resolver :policy/profile keywordish))}
                 :protocol {:result/types (if (sequential? (:result/types protocol))
                                            (vec (keep keywordish (:result/types protocol)))
                                            [])
                            :policy/default (if (map? (:policy/default protocol))
                                              (:policy/default protocol)
                                              {})}}
          (keyword? intent)
          (assoc-in [:protocol :intent] (if (map? (get-in protocol [:intents intent]))
                                          (get-in protocol [:intents intent])
                                          {}))
          (keyword? intent)
          (assoc-in [:protocol :policy/intent] (if (map? (get-in protocol [:policy/intents intent]))
                                                 (get-in protocol [:policy/intents intent])
                                                 {})))
        snapshot-id (format "%08x" (bit-and 0xffffffff (long (hash snapshot))))]
    {:snapshot-id snapshot-id
     :snapshot snapshot}))

(defn- numeric-map-delta
  [before after]
  (letfn [(delta [a b]
            (cond
              (and (map? a) (map? b))
              (let [ks (into #{} (concat (keys a) (keys b)))
                    out (reduce (fn [acc k]
                                  (let [d (delta (get a k) (get b k))]
                                    (if (nil? d)
                                      acc
                                      (assoc acc k d))))
                                {}
                                ks)]
                (when (seq out) out))

              (and (number? a) (number? b))
              (let [d (- (double b) (double a))]
                (when (not (zero? d))
                  d))

              (and (nil? a) (number? b))
              (let [d (double b)]
                (when (not (zero? d))
                  d))

              :else nil))]
    (or (delta before after) {})))

(defn- structured-diff
  [left right]
  (letfn [(diff* [a b]
            (cond
              (= a b)
              nil

              (and (map? a) (map? b))
              (let [ks (into #{} (concat (keys a) (keys b)))
                    out (reduce (fn [acc k]
                                  (let [d (diff* (get a k) (get b k))]
                                    (if (nil? d)
                                      acc
                                      (assoc acc k d))))
                                {}
                                ks)]
                (when (seq out) out))

              (and (sequential? a) (sequential? b))
              (let [a' (vec a)
                    b' (vec b)]
                (when (not= a' b')
                  {:from a' :to b'}))

              :else
              {:from a :to b}))]
    (or (diff* left right) {})))

(defn- replay-telemetry-view
  [snapshot]
  (when (map? snapshot)
    (select-keys snapshot [:act :workflow :orchestration :kpi])))

(defn- replay-execution-path
  [phase response]
  (let [request* (if (map? (:request* phase)) (:request* phase) {})
        cap-decision (if (map? (:cap/decision phase)) (:cap/decision phase) {})
        selected-cap (some-> cap-decision :cap/id keywordish)
        candidates (->> (:candidates cap-decision)
                        (keep keywordish)
                        vec)
        rejected (->> (:rejected-candidates cap-decision)
                      (keep (fn [entry]
                              (when (map? entry)
                                (cond-> {}
                                  (keyword? (keywordish (:cap/id entry)))
                                  (assoc :cap/id (keywordish (:cap/id entry)))
                                  (keyword? (keywordish (:reason entry)))
                                  (assoc :reason (keywordish (:reason entry)))
                                  (keyword? (keywordish (:intent entry)))
                                  (assoc :intent (keywordish (:intent entry)))))))
                      vec)]
    (cond-> {:intent (some-> request* :task :intent keywordish)
             :requested-cap/id (or (some-> request* :task :cap/id keywordish)
                                   (some-> request* :cap/id keywordish))
             :selected-cap/id selected-cap
             :route/mode (keywordish (:route-mode phase))
             :route/routed? (boolean (:routed? phase))
             :response/outcome (response-outcome response)
             :response/error-type (response-error-type response)}
      (seq candidates) (assoc :candidates candidates)
      (seq rejected) (assoc :rejected-candidates rejected))))

(defn- replay-diagnostics
  [phase response telemetry-before telemetry-after]
  (let [telemetry-before' (replay-telemetry-view telemetry-before)
        telemetry-after'  (replay-telemetry-view telemetry-after)]
    {:execution-path (replay-execution-path phase response)
     :telemetry {:before telemetry-before'
                 :after telemetry-after'
                 :delta (numeric-map-delta telemetry-before' telemetry-after')}}))

(defn- replay-entry
  [phase response auth elapsed-ms replay-cfg telemetry-before telemetry-after]
  (let [request0 (:request phase)
        request* (:request* phase)
        payload  (:payload phase)
        policy   (replay-policy-snapshot phase)
        cap-decision (if (map? (:cap/decision phase)) (:cap/decision phase) {})
        redact-keys (if (set? (:redact-keys replay-cfg))
                      (:redact-keys replay-cfg)
                      default-replay-redact-keys)
        meta-step  (if (map? (:meta-step phase))
                     (select-keys (:meta-step phase)
                                  [:mode :enabled? :strict? :attempted? :reason
                                   :latency-ms :decider-latency-ms])
                     {})]
    {:recorded-at (str (java.time.Instant/now))
     :trace/id (replay-trace-id phase response)
     :request {:payload (replay-redact payload redact-keys)
               :prepared (replay-redact request0 redact-keys)
               :resolved (replay-redact request* redact-keys)}
     :routing {:mode (:route-mode phase)
               :routed? (boolean (:routed? phase))
               :meta-step meta-step
               :cap/decision (replay-redact cap-decision redact-keys)
               :route-telemetry (if (map? (:route-telemetry phase))
                                  (:route-telemetry phase)
                                  {})
               :route-decide-latency-ms (:route-decide-latency-ms phase)
               :route-phase-latency-ms (:route-phase-latency-ms phase)
               :route-decider-latency-ms (:route-decider-latency-ms phase)}
     :policy policy
     :response {:status (int (or (:status response) 500))
                :outcome (response-outcome response)
                :error/type (response-error-type response)
                :body (replay-redact (if (map? (:body response))
                                       (:body response)
                                       {})
                                     redact-keys)}
     :auth {:source (or (some-> auth :source keywordish)
                        (some-> request* :auth/source keywordish))
            :principal (replay-redact (audit-principal auth request*)
                                      redact-keys)}
     :diagnostics (replay-diagnostics phase response telemetry-before telemetry-after)
     :timing {:elapsed-ms (double (or elapsed-ms 0.0))}}))

(defn- append-training-events!
  [runtime request* replay-entry]
  (let [collector (runtime-training-collector-instance runtime)
        training-cfg (if (map? (:training runtime))
                       (:training runtime)
                       {})
        event-opts {:judge (:judge training-cfg)
                    :redaction (:redaction training-cfg)}]
    (when (and (some? collector)
               (training-enabled? runtime request*)
               (map? replay-entry))
      (doseq [event (training-events/replay-entry->events replay-entry event-opts)]
        (let [append-result (training-collector/append! collector event)]
          (when-not (:ok? append-result)
            (telemetry/record-lifecycle!
             :training/collector
             :error
             {:error :training/collector-append-failed
              :training.event/id (:training.event/id event)
              :trace/id (or (:trace/id replay-entry)
                            (get-in replay-entry [:request :resolved :trace :id]))
              :details append-result})))))))

(defn- record-act-replay!
  ([runtime phase response auth elapsed-ms]
   (record-act-replay! runtime phase response auth elapsed-ms nil nil))
  ([runtime phase response auth elapsed-ms telemetry-before telemetry-after]
   (let [request* (when (map? phase)
                    (or (:request* phase)
                        (:request phase)))]
     (when (and (replay-enabled? runtime request*)
                (map? phase)
                (map? response))
       (let [replay-cfg (replay-runtime runtime)
             state-atom (:state replay-cfg)
             trace-id   (replay-trace-id phase response)]
         (when (and (instance? clojure.lang.IAtom state-atom)
                    (some? trace-id))
           (let [now-ms    (System/currentTimeMillis)
                 ttl-ms    (or (parse-non-negative-long (:ttl-ms replay-cfg))
                               default-replay-ttl-ms)
                 max-size  (or (positive-int (:max-size replay-cfg))
                               default-replay-max-size)
                 entry0    (replay-entry phase response auth elapsed-ms replay-cfg telemetry-before telemetry-after)
                 entry     (assoc entry0 :expires-at (+ now-ms ttl-ms))]
             (swap! state-atom
                    (fn [state]
                      (let [base (if (map? state) state (default-replay-state))
                            {:keys [state]} (replay-prune-expired-state base now-ms)
                            state' (-> state
                                       (assoc-in [:entries trace-id] entry)
                                       (update :order (fn [order]
                                                        (conj (order-without order trace-id)
                                                              trace-id))))
                            {:keys [state]} (replay-prune-size-state state' max-size)]
                        state)))
             (append-training-events! runtime request* entry)))))
     nil)))

(defn- replay-get
  [runtime trace-id]
  (let [trace-id' (trim-s trace-id)]
    (cond
      (not (replay-enabled? runtime))
      {:ok? false :error :replay/disabled}

      (nil? trace-id')
      {:ok? false :error :replay/invalid-trace-id}

      :else
      (let [replay-cfg (replay-runtime runtime)
            state-atom (:state replay-cfg)
            now-ms (System/currentTimeMillis)]
        (if-not (instance? clojure.lang.IAtom state-atom)
          {:ok? false :error :replay/unavailable}
          (do
            (swap! state-atom
                   (fn [state]
                     (let [{:keys [state]} (replay-prune-expired-state
                                            (if (map? state) state (default-replay-state))
                                            now-ms)]
                       state)))
            (let [entry (get-in @state-atom [:entries trace-id'])]
              (if (map? entry)
                {:ok? true
                 :trace/id trace-id'
                 :replay entry}
                {:ok? false
                 :error :replay/not-found
                 :trace/id trace-id'}))))))))

(defn- invocation->participant
  [invocation]
  (when (map? invocation)
    (let [role      (keywordish (:role invocation))
          intent    (keywordish (:intent invocation))
          cap-id    (keywordish (:cap/id invocation))
          model-key (keywordish (:model-key invocation))
          model-id  (trim-s (:model invocation))]
      (cond-> {}
        (keyword? role) (assoc :role role)
        (keyword? intent) (assoc :intent intent)
        (keyword? cap-id) (assoc :cap/id cap-id)
        (keyword? model-key) (assoc :model-key model-key)
        (some? model-id) (assoc :model model-id)))))

(defn- slot-invocation
  [slot]
  (or (when (map? slot) (:invoke/meta slot))
      (when (map? slot) (get-in slot [:result :invoke/meta]))))

(defn- collect-response-participants
  [body]
  (let [top-invocation (or (:invoke/meta body)
                           (get-in body [:result :invoke/meta]))
        run-participants (or (get-in body [:result :plan/run :participants]) [])
        run-env        (or (get-in body [:result :plan/run :env]) {})
        run-invocations (if (map? run-env)
                          (keep slot-invocation (vals run-env))
                          [])
        participants   (->> (concat [top-invocation] run-participants run-invocations)
                            (keep invocation->participant)
                            distinct
                            vec)]
    participants))

(defn- attach-response-participants
  [response]
  (let [body (if (map? response) (:body response) nil)
        participants (if (map? body)
                       (collect-response-participants body)
                       [])]
    (if (seq participants)
      (assoc-in response [:body :models/used] participants)
      response)))

(defn- keyword-vec
  [v]
  (->> (cond
         (set? v) v
         (sequential? v) v
         (some? v) [v]
         :else [])
       (keep keywordish)
       vec))

(defn- nonneg-int
  [v]
  (when (and (integer? v) (<= 0 v))
    v))

(defn- normalize-retry-policy
  [retry-map]
  (let [same-cap-max (nonneg-int (:same-cap-max retry-map))
        fallback-max (nonneg-int (:fallback-max retry-map))]
    (cond-> {}
      (some? same-cap-max) (assoc :same-cap-max same-cap-max)
      (some? fallback-max) (assoc :fallback-max fallback-max))))

(def ^:private tool-call-tag-pattern
  #"(?is)<tool_call\b[^>]*>\s*(\{.*?\})\s*</tool_call>")

(def ^:private think-tag-pattern
  #"(?is)<think\b[^>]*>.*?</think>")

(def ^:private tool-call-block-pattern
  #"(?is)<tool_call\b[^>]*>.*?</tool_call>")

(def ^:private pseudo-tool-tag-pattern
  #"(?is)</?\s*(?:tool_calls?|function_call|analysis|observation)\b[^>]*>")

(def ^:private route-tool-call-names
  #{"solve_question" "ask_solver" "route_to_solver"})

(defn- parse-structured-text
  [s]
  (when (string? s)
    (let [s' (trim-s s)]
      (when s'
        (or (try
              (json/parse-string s' true)
              (catch Throwable _ nil))
            (try
              (edn/read-string s')
              (catch Throwable _ nil)))))))

(defn- parse-tool-call-arguments
  [v]
  (cond
    (map? v) v
    (string? v) (or (parse-structured-text v) {})
    :else {}))

(defn- extract-tool-call
  [text]
  (when (string? text)
    (when-some [[_ payload] (re-find tool-call-tag-pattern text)]
      (let [parsed (parse-structured-text payload)]
        (when (map? parsed) parsed)))))

(defn- map-tool-call
  [parsed]
  (when (map? parsed)
    (cond
      (map? (:tool_call parsed)) (:tool_call parsed)
      (map? (get parsed "tool_call")) (get parsed "tool_call")
      (and (sequential? (:tool_calls parsed))
           (map? (first (:tool_calls parsed))))
      (first (:tool_calls parsed))
      (and (sequential? (get parsed "tool_calls"))
           (map? (first (get parsed "tool_calls"))))
      (first (get parsed "tool_calls"))
      :else nil)))

(defn- strip-internal-markers
  [text]
  (-> (or text "")
      (str/replace think-tag-pattern "")
      (str/replace tool-call-block-pattern "")
      (str/replace pseudo-tool-tag-pattern "")
      str/trim))

(defn- sanitize-public-out
  [out]
  (cond
    (map? out)
    (cond-> out
      (string? (:text out))
      (assoc :text (strip-internal-markers (:text out)))

      (string? (:content out))
      (assoc :content (strip-internal-markers (:content out))))

    (string? out)
    (strip-internal-markers out)

    :else
    out))

(defn- sanitize-final-response-body
  [body]
  (let [result-type (contracts/result-type-of body)]
    (cond
      (and (map? body) (= :value result-type))
      (update-in body [:result :out] sanitize-public-out)

      (and (map? body) (= :stream result-type))
      (update-in body [:result :stream]
                 (fn [events]
                   (if (sequential? events)
                     (mapv (fn [event]
                             (if (and (map? event) (string? (:text event)))
                               (assoc event :text (strip-internal-markers (:text event)))
                               event))
                           events)
                     events)))

      :else
      body)))

(defn- route-tool-call-question
  [tool-call]
  (when (map? tool-call)
    (let [name' (or (trim-s (:name tool-call))
                    (trim-s (get tool-call "name")))
          args  (parse-tool-call-arguments
                 (or (:arguments tool-call)
                     (get tool-call "arguments")))
          q     (or (trim-s (:question args))
                    (trim-s (get args "question"))
                    (trim-s (:prompt args))
                    (trim-s (get args "prompt"))
                    (trim-s (:query args))
                    (trim-s (get args "query")))]
      (when (and (some? name')
                 (contains? route-tool-call-names name')
                 (some? q))
        q))))

(defn- route-request-prompt
  [request]
  (or (trim-s (get-in request [:input :request :input :prompt]))
      (trim-s (get-in request [:input :request :input :text]))
      (trim-s (get-in request [:input :request :input :content]))
      (trim-s (get-in request [:input :request :prompt]))
      (trim-s (get-in request [:input :request :text]))
      (trim-s (get-in request [:input :request :content]))
      (trim-s (get-in request [:input :prompt]))
      (trim-s (get-in request [:input :text]))
      (trim-s (get-in request [:input :content]))
      (trim-s (:prompt request))))

(def ^:private route-voice-preserve-system
  "Role: VOICE. Rewrite for tone/style only. Preserve all factual claims, technical details, constraints, and examples from input handoff. Do not summarize away meaning. Keep output compact. Keep the same language as input handoff/text and do not translate it. Correct spelling, grammar, punctuation, and obvious wording defects in the final text, while preserving facts and uncertainty level exactly. Never introduce new names, entities, attributions, numbers, or sources. If the input states uncertainty/unknown, keep that uncertainty explicit in final wording. If source text appears truncated, complete it naturally in at most two sentences, without adding new facts.")

(def ^:private route-voice-primary-system
  "Role: VOICE. Always return a JSON object with keys: text and answer/status. Allowed statuses: ok, unknown, needs-solver. If uncertain or missing reliable facts use needs-solver (or unknown). For confident answers use ok. Do not use markdown wrappers.")

(def ^:private route-solver-factual-system
  "Role: SOLVER. Always return a JSON object with keys: text and answer/status. Allowed statuses: ok, unknown, needs-solver. For fact questions (who/author/identity), if you cannot provide reliable evidence in text, set answer/status to unknown. When answer/status is ok, include a short source cue in text (for example: source/according to/URL). Do not fabricate facts.")

(defn- route-solver->voice-plan
  [user-prompt]
  {:nodes [{:op :call
            :intent :text/respond
            :cap/id :llm/voice
            :system route-voice-primary-system
           :constraints {:max-chars 420}
            :budget {:max-tokens 220}
            :done {:score-min 0.85}
            :input {:prompt user-prompt}
            :as :voice-primary
             :dispatch {:allow-failure? true
                       :checks/hard [:schema-valid :no-truncated-ending :sufficient-detail :answer-status-present :answer-known]
                       :checks/soft [:no-hallucinated-apis :no-list-expansion]
                       :switch-on #{:schema/invalid :format/drift :eval/low-score :eval/must-failed}
                       :retry {:same-cap-max 0
                               :fallback-max 0}}}
           {:op :call
           :intent :problem/solve
            :cap/id :llm/solver
            :system route-solver-factual-system
            :constraints {:max-chars 700}
            :budget {:max-tokens 360}
            :done {:score-min 0.5}
            :input {:prompt user-prompt}
            :dispatch {:checks/hard [:schema-valid :answer-status-present :fact-question-grounded]
                       :checks/soft [:no-hallucinated-apis :no-truncated-ending]
                       :retry {:same-cap-max 2
                               :fallback-max 0}}
            :as :solver
            :when {:failed? :voice-primary}}
           {:op :call
           :intent :text/respond
           :cap/id :llm/voice
           :system route-voice-preserve-system
            :input/schema :req/handoff
            :requires {:in-schema :req/handoff}
            :input {:handoff/text {:slot/id [:solver :out :text]}}
            :constraints {:max-chars 700}
            :budget {:max-tokens 240}
            :done {:score-min 0.2}
            :dispatch {:checks/hard [:schema-valid :no-truncated-ending]
                       :checks/soft [:no-hallucinated-apis :no-list-expansion]
                       :switch-on #{:schema/invalid :format/drift :eval/low-score :eval/must-failed}
                       :retry {:same-cap-max 2
                               :fallback-max 0}}
            :as :voice-final
            :when {:failed? :voice-primary}}
           {:op :emit
            :input {:slot/id [:voice-primary :out]}}
           {:op :emit
            :when {:failed? :voice-primary}
            :input {:slot/id [:voice-final :out]}}]})

(defn- route-solver->voice-plan?
  [plan]
  (let [nodes (when (map? plan) (:nodes plan))
        [node0 node1 node2 node3 node4] (if (vector? nodes) nodes [])]
    (and (= 5 (count nodes))
         (= :call (:op node0))
         (= :text/respond (:intent node0))
         (= :llm/voice (:cap/id node0))
         (= :voice-primary (:as node0))
         (true? (get-in node0 [:dispatch :allow-failure?]))
         (= :call (:op node1))
         (= :problem/solve (:intent node1))
         (= :llm/solver (:cap/id node1))
         (= :solver (:as node1))
         (= :voice-primary (get-in node1 [:when :failed?]))
         (= :call (:op node2))
         (= :text/respond (:intent node2))
         (= :llm/voice (:cap/id node2))
         (= :voice-final (:as node2))
         (= :voice-primary (get-in node2 [:when :failed?]))
         (= :req/handoff (keywordish (:input/schema node2)))
         (= [:solver :out :text] (get-in node2 [:input :handoff/text :slot/id]))
         (= :emit (:op node3))
         (= [:voice-primary :out] (get-in node3 [:input :slot/id]))
         (= :emit (:op node4))
         (= :voice-primary (get-in node4 [:when :failed?]))
         (= [:voice-final :out] (get-in node4 [:input :slot/id])))))

(defn- invalid-route-decide-result
  [request]
  {:proto  (request-proto request)
   :trace  (request-trace request)
   :result {:type :value}})

(defn- route-decide-result-parser
  [text {:keys [request mode]}]
  (let [text'       (trim-s text)
        parsed      (parse-structured-text text')
        map-tool*    (map-tool-call parsed)
        route-plan* (cond
                      (map? (:plan parsed))
                      (:plan parsed)

                      (map? (get-in parsed [:result :plan]))
                      (get-in parsed [:result :plan])

                      :else nil)
        route-plan (when (route-solver->voice-plan? route-plan*)
                     route-plan*)
        tool-call  (or map-tool*
                       (extract-tool-call text'))
        prompt'    (if (map? tool-call)
                     (route-tool-call-question tool-call)
                     (route-request-prompt request))]
    (cond
      (map? route-plan)
      {:proto  (request-proto request)
       :trace  (request-trace request)
       :result {:type  :plan
                :plan  route-plan
                :usage {:mode mode}}}

      (and (map? tool-call) (nil? prompt'))
      (invalid-route-decide-result request)

      (some? prompt')
      {:proto  (request-proto request)
       :trace  (request-trace request)
       :result {:type  :plan
                :plan  (route-solver->voice-plan prompt')
                :usage {:mode mode}}}

      :else
      (invalid-route-decide-result request))))

(defn- request-routing-config
  [request]
  (if (map? (:routing request))
    (:routing request)
    {}))

(defn- routing-profiles
  [runtime]
  (let [profiles (get-in runtime [:router :profiles])]
    (if (map? profiles) profiles {})))

(defn- routing-profile-config
  [runtime request]
  (let [profile-k (some-> (request-routing-config request) :profile keywordish)
        profile-cfg (when (keyword? profile-k)
                      (get (routing-profiles runtime) profile-k))]
    (if (map? profile-cfg) profile-cfg {})))

(defn- effective-routing-config
  [runtime request]
  (merge (router/routing-defaults runtime)
         (routing-profile-config runtime request)
         (request-routing-config request)))

(defn- meta-routing-target-intent
  [request]
  (or (some-> request :task :intent keywordish)
      :text/respond))

(defn- meta-routing-supported-intent?
  [request]
  (= :text/respond (meta-routing-target-intent request)))

(defn- meta-routing-enabled?
  [runtime request]
  (let [request-cfg (request-routing-config request)
        cfg (effective-routing-config runtime request)
        strict-request? (or (= :fail-closed (keywordish (:on-error request-cfg)))
                            (and (contains? request-cfg :strict?)
                                 (true? (:strict? request-cfg))))
        force-request? (and (contains? request-cfg :force?)
                            (true? (:force? request-cfg)))
        strict-default? (and (not strict-request?)
                             (not (contains? request-cfg :on-error))
                             (not (contains? request-cfg :strict?))
                             (or (= :fail-closed (keywordish (:on-error cfg)))
                                 (true? (:strict? cfg))))
        force-default? (and (not force-request?)
                            (not (contains? request-cfg :force?))
                            (true? (:force? cfg)))]
    (cond
      (or strict-request? force-request? strict-default? force-default?)
      true

      (contains? cfg :meta?)
      (boolean (:meta? cfg))

      :else
      (= :meta-decider (get-in runtime [:router :policy])))))

(defn- meta-routing-fail-mode
  [runtime request]
  (let [request-cfg          (request-routing-config request)
        request-on-error     (keywordish (:on-error request-cfg))
        request-strict?      (boolean (:strict? request-cfg))
        request-strict-set?  (contains? request-cfg :strict?)
        cfg                  (effective-routing-config runtime request)
        on-error             (keywordish (:on-error cfg))
        strict?              (boolean (:strict? cfg))]
    (cond
      ;; Explicit request on-error mode has top priority.
      (= :fail-closed request-on-error) :fail-closed
      (= :fail-open request-on-error) :fail-open

      ;; Request strict flag should override router default fail-open.
      (and request-strict-set? request-strict?) :fail-closed
      (and request-strict-set? (not request-strict?)) :fail-open

      (= :fail-closed on-error) :fail-closed
      (= :fail-open on-error) :fail-open
      strict? :fail-closed
      :else :fail-open)))

(defn- meta-routing-strict?
  [runtime request]
  (= :fail-closed (meta-routing-fail-mode runtime request)))

(defn- meta-routing-force?
  [runtime request]
  (boolean (:force? (effective-routing-config runtime request))))

(defn- meta-routing-intent
  [request]
  (or (some-> (request-routing-config request) :intent keywordish)
      :route/decide))

(defn- meta-routing-cap-id
  [runtime resolver request route-intent]
  (or (some-> (request-routing-config request) :cap/id keywordish)
      (some-> resolver :routing :intent->cap (get route-intent))
      (some-> (router/resolver-routing runtime resolver) :intent->cap (get route-intent))
      (when (= :route/decide route-intent) :llm/meta)))

(defn- routing-decision?
  [out]
  (and (map? out)
       (keyword? (:cap/id out))))

(defn- routing-decision-candidates
  [decision]
  (let [primary (keywordish (:cap/id decision))
        route-cands (keyword-vec (get-in decision [:dispatch :candidates]))]
    (->> (concat (when (keyword? primary) [primary]) route-cands)
         distinct
         vec)))

(defn- merge-routing-decision
  [runtime request decision]
  (let [force?       (meta-routing-force? runtime request)
        explicit-cap (keyword? (get-in request [:task :cap/id]))
        primary-cap  (keywordish (:cap/id decision))
        candidates   (routing-decision-candidates decision)
        chosen-cap   (or primary-cap
                         (first candidates))
        dispatch-in  (if (map? (:dispatch decision)) (:dispatch decision) {})
        switch-on    (keyword-vec (:switch-on dispatch-in))
        checks       (keyword-vec (:checks dispatch-in))
        checks-hard  (keyword-vec (:checks/hard dispatch-in))
        checks-soft  (keyword-vec (:checks/soft dispatch-in))
        retry        (normalize-retry-policy
                      (if (map? (:retry dispatch-in)) (:retry dispatch-in) {}))
        route-done   (if (map? (:done decision)) (:done decision) nil)
        route-constraints (if (map? (:constraints decision)) (:constraints decision) nil)
        route-budget (if (map? (:budget decision)) (:budget decision) nil)
        route-effects (if (map? (:effects decision)) (:effects decision) nil)
        dispatch     (cond-> {}
                       (seq candidates) (assoc :candidates candidates)
                       (seq switch-on) (assoc :switch-on (set switch-on))
                       (seq checks) (assoc :checks checks)
                       (seq checks-hard) (assoc :checks/hard checks-hard)
                       (seq checks-soft) (assoc :checks/soft checks-soft)
                       (seq retry) (assoc :retry retry))]
    (cond-> request
      (and (keyword? chosen-cap)
           (or force? (not explicit-cap)))
      (assoc-in [:task :cap/id] chosen-cap)

      (map? route-done)
      (update :done #(merge (if (map? %) % {}) route-done))

      (map? route-constraints)
      (update :constraints #(merge (if (map? %) % {}) route-constraints))

      (map? route-budget)
      (update :budget #(merge (if (map? %) % {}) route-budget))

      (map? route-effects)
      (update :effects #(merge (if (map? %) % {}) route-effects))

      (seq dispatch)
      (assoc :dispatch dispatch)

      true
      (assoc-in [:routing :decision] (cond-> {}
                                       (keyword? chosen-cap) (assoc :cap/id chosen-cap)
                                       (seq dispatch) (assoc :dispatch dispatch))))))

(defn- route-decider-opts
  [runtime resolver request cap-id intent]
  (let [route-role (router/resolve-role runtime resolver cap-id intent)
        strict-routing? (meta-routing-strict? runtime request)
        policy-profile (resolve-routing-policy-profile runtime resolver request intent)
        policy-profiles (resolve-routing-policy-profiles runtime resolver)
        policy-cfg (if (and (keyword? policy-profile)
                            (map? policy-profiles))
                     (get policy-profiles policy-profile)
                     nil)
        route-decider-max-chars (if strict-routing? 320 420)
        route-decider-max-tokens (if strict-routing? 64 96)
        route-decider-max-attempts (if strict-routing? 1 2)
        route-decider-timeout-ms (if strict-routing? 6000 12000)
        request-constraints (if (map? (:constraints request)) (:constraints request) {})
        request-budget (if (map? (:budget request)) (:budget request) {})
        bounded-max-chars (let [v (parse-positive-int (:max-chars request-constraints) nil)]
                            (if (integer? v)
                              (min route-decider-max-chars v)
                              route-decider-max-chars))
        bounded-max-tokens (let [v (parse-positive-int (:max-tokens request-budget) nil)]
                             (if (integer? v)
                               (min route-decider-max-tokens v)
                               route-decider-max-tokens))
        bounded-max-roundtrips (let [v (parse-positive-int (:max-roundtrips request-budget) nil)]
                                 (if (integer? v)
                                   (min route-decider-max-attempts v)
                                   route-decider-max-attempts))
        bounded-timeout-ms (let [v (parse-positive-int (:timeout-ms request) nil)]
                             (if (integer? v)
                               (min route-decider-timeout-ms v)
                               route-decider-timeout-ms))
        route-constraints (assoc request-constraints
                                 :max-chars bounded-max-chars)
        route-budget (assoc request-budget
                            :max-tokens bounded-max-tokens
                            :max-roundtrips bounded-max-roundtrips
                            :temperature 0.0)
        max-call-attempts (when (map? policy-cfg)
                            (resolve-workflow-limit policy-cfg :call-tree/max-calls))
        max-fallback-hops (when (map? policy-cfg)
                            (resolve-workflow-limit policy-cfg :call-tree/max-fallback-hops))]
    {:role route-role
     :intent intent
     :cap-id cap-id
     :input {:request request
             :resolver {:routing (:routing resolver)}}
     :context (merge {:route/for-intent (get-in request [:task :intent])}
                     (if (map? (:context request)) (:context request) {}))
     :constraints route-constraints
     :budget route-budget
     :max-attempts route-decider-max-attempts
     :timeout-ms bounded-timeout-ms
     :request-id (:request/id request)
     :trace (:trace request)
     :proto (:proto request)
     :session-id (:session/id request)
     :session-version (:session/version request)
     :auth/user (:auth/user request)
     :roles (:roles runtime)
     :debug/plan? (request-debug-plan? request)
     :debug/transcript? (request-debug-transcript? request)
     :result-parser route-decide-result-parser
     :policy/profile policy-profile
     :policy/profiles policy-profiles
     :workflow/max-calls max-call-attempts
     :workflow/max-fallback-hops max-fallback-hops
     :resolver resolver}))

(defn- compact-last-check
  [last-check]
  (when (map? last-check)
    (let [details (when (map? (:details last-check))
                    (:details last-check))
          nested-details (when (map? details)
                           (let [inner (when (map? (:details details))
                                         (:details details))]
                             (cond-> {}
                               (keyword? (:reason details)) (assoc :reason (:reason details))
                               (keyword? (:schema details)) (assoc :schema (:schema details))
                               (and (map? inner) (keyword? (:reason inner)))
                               (assoc :details {:reason (:reason inner)
                                                :schema (:schema inner)}))))]
      (cond-> {}
        (contains? last-check :ok?) (assoc :ok? (boolean (:ok? last-check)))
        (keyword? (:error last-check)) (assoc :error (:error last-check))
        (keyword? (:reason last-check)) (assoc :reason (:reason last-check))
        (keyword? (:intent last-check)) (assoc :intent (:intent last-check))
        (keyword? (:result/type last-check)) (assoc :result/type (:result/type last-check))
        (map? nested-details) (assoc :details nested-details)))))

(defn- compact-done-eval
  [done-eval]
  (when (map? done-eval)
    (let [checks (when (sequential? (:checks done-eval))
                   (->> (:checks done-eval)
                        (keep (fn [c]
                                (when (map? c)
                                  (cond-> {}
                                    (contains? c :check) (assoc :check (:check c))
                                    (contains? c :ok?) (assoc :ok? (boolean (:ok? c)))
                                    (keyword? (:error c)) (assoc :error (:error c))
                                    (keyword? (:reason c)) (assoc :reason (:reason c))
                                    (number? (:score c)) (assoc :score (double (:score c)))))))
                        vec))
          must-failed (when (sequential? (:must-failed done-eval))
                        (->> (:must-failed done-eval)
                             (keep keywordish)
                             vec))
          should-failed (when (sequential? (:should-failed done-eval))
                          (->> (:should-failed done-eval)
                               (keep keywordish)
                               vec))]
      (cond-> {}
        (contains? done-eval :ok?) (assoc :ok? (boolean (:ok? done-eval)))
        (number? (:score done-eval)) (assoc :score (double (:score done-eval)))
        (number? (:score-min done-eval)) (assoc :score-min (double (:score-min done-eval)))
        (number? (:judge-score done-eval)) (assoc :judge-score (double (:judge-score done-eval)))
        (contains? done-eval :judge/pass?) (assoc :judge/pass? (boolean (:judge/pass? done-eval)))
        (set? (:violations done-eval)) (assoc :violations (->> (:violations done-eval) sort vec))
        (some? must-failed) (assoc :must-failed must-failed)
        (some? should-failed) (assoc :should-failed should-failed)
        (seq checks) (assoc :checks checks)))))

(defn- compact-outcome
  [outcome]
  (when (map? outcome)
    (let [done-eval (compact-done-eval (:done/eval outcome))]
      (cond-> {}
        (contains? outcome :ok?) (assoc :ok? (boolean (:ok? outcome)))
        (keyword? (:failure/type outcome)) (assoc :failure/type (:failure/type outcome))
        (contains? outcome :failure/recover?) (assoc :failure/recover? (boolean (:failure/recover? outcome)))
        (keyword? (:cap/id outcome)) (assoc :cap/id (:cap/id outcome))
        (number? (:attempt outcome)) (assoc :attempt (long (:attempt outcome)))
        (map? done-eval) (assoc :done/eval done-eval)))))

(defn- compact-route-node
  [node]
  (when (map? node)
    (cond-> {}
      (keyword? (:op node)) (assoc :op (:op node))
      (keyword? (:intent node)) (assoc :intent (:intent node))
      (keyword? (:cap/id node)) (assoc :cap/id (:cap/id node))
      (keyword? (:tool/id node)) (assoc :tool/id (:tool/id node))
      (keyword? (:as node)) (assoc :as (:as node)))))

(defn- compact-rejected-candidate
  [entry]
  (when (map? entry)
    (cond-> {}
      (keyword? (:cap/id entry)) (assoc :cap/id (:cap/id entry))
      (keyword? (:reason entry)) (assoc :reason (:reason entry))
      (keyword? (:intent entry)) (assoc :intent (:intent entry))
      (keyword? (:result/type entry)) (assoc :result/type (:result/type entry))
      (keyword? (:required-kind entry)) (assoc :required-kind (:required-kind entry))
      (keyword? (:cap-kind entry)) (assoc :cap-kind (:cap-kind entry)))))

(defn- route-decider-error-details
  [data route-intent cap-id]
  (let [data' (if (map? data) data {})
        outcome (compact-outcome (:outcome data'))
        node (compact-route-node (:node data'))
        retry-policy (when (map? (:retry-policy data'))
                       (let [p (:retry-policy data')]
                         (cond-> {}
                           (integer? (:same-cap-max p)) (assoc :same-cap-max (:same-cap-max p))
                           (integer? (:fallback-max p)) (assoc :fallback-max (:fallback-max p)))))
        last-check (compact-last-check (:last-check data'))]
    (cond-> {:route/intent route-intent
             :route/cap-id cap-id}
      (keyword? (:error data')) (assoc :error (:error data'))
      (keyword? (:reason data')) (assoc :reason (:reason data'))
      (keyword? (:failure/type data')) (assoc :failure/type (:failure/type data'))
      (number? (:attempts data')) (assoc :attempts (long (:attempts data')))
      (map? last-check) (assoc :last-check last-check)
      (map? outcome) (assoc :outcome outcome)
      (map? node) (assoc :node node)
      (set? (:switch-on data')) (assoc :switch-on (->> (:switch-on data') sort vec))
      (sequential? (:candidates data')) (assoc :candidates (vec (filter keyword? (:candidates data'))))
      (map? retry-policy) (assoc :retry-policy retry-policy)
      (sequential? (:rejected-candidates data'))
      (assoc :rejected-candidates
             (->> (:rejected-candidates data')
                  (keep compact-rejected-candidate)
                  vec)))))

(defn- maybe-apply-meta-routing
  [runtime resolver request]
  (let [route-intent (meta-routing-intent request)
        strict?      (meta-routing-strict? runtime request)
        enabled?     (meta-routing-enabled? runtime request)]
    (cond
      (not enabled?)
      {:request request
       :mode :none
       :enabled? false
       :strict? strict?
       :attempted? false
       :reason :meta-disabled}

      (not (meta-routing-supported-intent? request))
      {:request request
       :mode :none
       :enabled? true
       :strict? strict?
       :attempted? false
       :reason :meta-intent-unsupported}

      (= route-intent (get-in request [:task :intent]))
      {:request request
       :mode :none
       :enabled? true
       :strict? strict?
       :attempted? false
       :reason :same-intent}

      :else
      (let [cap-id (meta-routing-cap-id runtime resolver request route-intent)]
        (if-not (keyword? cap-id)
          (if strict?
            {:mode :error
             :status 502
             :request request
             :enabled? true
             :strict? true
             :attempted? false
             :reason :fail-closed
             :body (error-envelope request
                                   :route/decide-failed
                                   "Meta routing is enabled, but route capability could not be resolved."
                                   {:route/intent route-intent}
                                   true)}
            {:request request
             :mode :none
             :enabled? true
             :strict? false
             :attempted? false
             :reason :fail-open})
          (let [started-at (now-nanos)]
            (try
              (let [route-response (core/call-capability
                                    runtime
                                    resolver
                                    (route-decider-opts runtime resolver request cap-id route-intent))
                    route-out (contracts/result-out-of route-response)
                    decider-latency-ms (nanos->millis started-at)]
                (if (routing-decision? route-out)
                  {:request (merge-routing-decision runtime request route-out)
                   :mode :continue
                   :enabled? true
                   :strict? strict?
                   :attempted? true
                   :reason :continue
                   :route-response route-response
                   :decider-latency-ms decider-latency-ms}
                  {:request request
                   :mode :final
                   :enabled? true
                   :strict? strict?
                   :attempted? true
                   :reason :final
                   :response route-response
                   :decider-latency-ms decider-latency-ms}))
              (catch clojure.lang.ExceptionInfo e
                (if strict?
                  {:mode :error
                   :status 502
                   :request request
                   :enabled? true
                   :strict? true
                   :attempted? true
                   :reason :fail-closed
                   :decider-latency-ms (nanos->millis started-at)
                   :body (error-envelope request
                                         :route/decide-failed
                                         (.getMessage e)
                                         (route-decider-error-details (ex-data e) route-intent cap-id)
                                         true)}
                  {:request request
                   :mode :none
                   :enabled? true
                   :strict? false
                   :attempted? true
                   :reason :fail-open
                   :decider-latency-ms (nanos->millis started-at)}))
              (catch Throwable t
                (if strict?
                  {:mode :error
                   :status 502
                   :request request
                   :enabled? true
                   :strict? true
                   :attempted? true
                   :reason :fail-closed
                   :decider-latency-ms (nanos->millis started-at)
                   :body (error-envelope request
                                         :route/decide-failed
                                         (.getMessage t)
                                         {:route/intent route-intent
                                          :route/cap-id cap-id
                                          :class (str (class t))}
                                         true)}
                  {:request request
                   :mode :none
                   :enabled? true
                   :strict? false
                   :attempted? true
                   :reason :fail-open
                   :decider-latency-ms (nanos->millis started-at)})))))))))

(defn- routing-telemetry-counters
  [meta-step]
  (let [step (if (map? meta-step) meta-step {})
        mode (:mode step)
        reason (:reason step)
        attempted? (true? (:attempted? step))
        strict? (true? (:strict? step))]
    (cond-> {}
      attempted? (update :route/decide-hit (fnil inc 0))
      (= :continue mode) (update :route/decide-continue (fnil inc 0))
      (= :final mode) (update :route/decide-final (fnil inc 0))
      (= :fail-open reason) (update :route/fail-open (fnil inc 0))
      (= :fail-closed reason) (update :route/fail-closed (fnil inc 0))
      strict? (update :route/strict (fnil inc 0)))))

(defn- cap-resolution-telemetry-counters
  [decision]
  (let [decision' (if (map? decision) decision {})
        resolved? (keyword? (:cap/id decision'))
        rejected  (if (sequential? (:rejected-candidates decision'))
                    (:rejected-candidates decision')
                    [])]
    (reduce (fn [acc entry]
              (let [reason (some-> entry :reason keywordish)]
                (if (keyword? reason)
                  (update-in acc [:cap/reject-reasons reason] (fnil inc 0))
                  acc)))
            (cond-> {:cap/resolve-attempt 1}
              resolved? (assoc :cap/resolve-hit 1)
              (not resolved?) (assoc :cap/resolve-miss 1))
            rejected)))

(defn- cap-resolution-error-details
  [request decision]
  (let [decision'   (if (map? decision) decision {})
        intent      (some-> request :task :intent keywordish)
        requested   (some-> decision' :requested-cap/id keywordish)
        routed-cap  (some-> decision' :routed-cap/id keywordish)
        candidates  (->> (:candidates decision')
                         (keep keywordish)
                         vec)
        rejected    (->> (:rejected-candidates decision')
                         (keep compact-rejected-candidate)
                         vec)]
    (cond-> {}
      (keyword? intent) (assoc :intent intent)
      (keyword? requested) (assoc :requested-cap/id requested)
      (keyword? routed-cap) (assoc :routed-cap/id routed-cap)
      (seq candidates) (assoc :candidates candidates)
      (seq rejected) (assoc :rejected-candidates rejected))))

(defn- invoke-act-prepared-request
  [runtime payload auth]
  (let [auth-user      (some-> auth :user auth-user-public)
        auth-source-k  (some-> auth :source keywordish)
        auth-session   (when (map? (:session auth))
                         (:session auth))
        auth-session-id (or (some-> auth-session :session/id trim-s)
                            (some-> auth-session :id trim-s))
        request0       (coerce-act-request payload)
        request1       (cond-> request0
                         (map? auth-user) (assoc :auth/user auth-user)
                         (keyword? auth-source-k) (assoc :auth/source auth-source-k)
                         (and (map? request0)
                              auth-session-id
                              (nil? (:session/id request0)))
                         (assoc :session/id auth-session-id))
        request2       (apply-request-training-defaults runtime request1)
        request3       (request-with-session-defaults runtime request2)]
    (request-with-tenancy-defaults runtime request3)))

(defn- invoke-act-select-runtime
  [runtime request]
  (if-not (map? runtime)
    runtime
    (let [trace-id (get-in request [:trace :id])
          requested-router-version (some-> request :routing :router/artifact-version)
          requested-router-shadow-version (some-> request :routing :router/shadow-artifact-version)
          requested-protocol-version (some-> request :routing :artifact/version)
          requested-protocol-shadow-version (some-> request :routing :shadow/artifact-version)
          router-cfg (runtime-router-config runtime)
          protocol-cfg (runtime-protocol-config runtime)
          router-selection (if (map? router-cfg)
                             (router/select-router-artifact
                              router-cfg
                              {:trace-id trace-id
                               :requested-version requested-router-version})
                             nil)
          protocol-selection (if (map? protocol-cfg)
                               (protocol/select-protocol-artifact
                                protocol-cfg
                                {:trace-id trace-id
                                 :requested-version requested-protocol-version})
                               nil)
          router-shadow-selection (if (map? router-cfg)
                                    (router/select-router-shadow-artifact
                                     router-cfg
                                     {:trace-id trace-id
                                      :requested-version requested-router-shadow-version})
                                    nil)
          protocol-shadow-selection (if (map? protocol-cfg)
                                      (protocol/select-protocol-shadow-artifact
                                       protocol-cfg
                                       {:trace-id trace-id
                                        :requested-version requested-protocol-shadow-version})
                                      nil)
          selected-router (:router router-selection)
          selected-router-version (some-> router-selection :artifact/version keywordish)
          selected-router-source (some-> router-selection :artifact/source keywordish)
          selected-protocol (:protocol protocol-selection)
          selected-protocol-version (some-> protocol-selection :artifact/version keywordish)
          selected-protocol-source (some-> protocol-selection :artifact/source keywordish)
          shadow-router (:router router-shadow-selection)
          shadow-router-version (some-> router-shadow-selection :artifact/version keywordish)
          shadow-router-source (some-> router-shadow-selection :artifact/source keywordish)
          shadow-router-enabled? (true? (:shadow/enabled? router-shadow-selection))
          shadow-router-applied? (and (true? (:shadow/applied? router-shadow-selection))
                                      (map? shadow-router))
          shadow-protocol-version (some-> protocol-shadow-selection :artifact/version keywordish)
          shadow-protocol-source (some-> protocol-shadow-selection :artifact/source keywordish)
          shadow-protocol-enabled? (true? (:shadow/enabled? protocol-shadow-selection))
          shadow-protocol-applied? (true? (:shadow/applied? protocol-shadow-selection))]
      (cond-> runtime
        (map? selected-router) (assoc :router selected-router)
        (map? selected-protocol) (assoc :protocol selected-protocol)
        (keyword? selected-router-version) (assoc :router/artifact-version selected-router-version)
        (keyword? selected-router-source) (assoc :router/artifact-source selected-router-source)
        (keyword? selected-protocol-version) (assoc :protocol/artifact-version selected-protocol-version)
        (keyword? selected-protocol-source) (assoc :protocol/artifact-source selected-protocol-source)
        shadow-router-enabled? (assoc :router/shadow-enabled? true)
        shadow-router-applied? (assoc :router/shadow shadow-router
                                      :router/shadow-applied? true)
        (keyword? shadow-router-version) (assoc :router/shadow-artifact-version shadow-router-version)
        (keyword? shadow-router-source) (assoc :router/shadow-artifact-source shadow-router-source)
        shadow-protocol-enabled? (assoc :protocol/shadow-enabled? true)
        shadow-protocol-applied? (assoc :protocol/shadow-applied? true)
        (keyword? shadow-protocol-version) (assoc :protocol/shadow-artifact-version shadow-protocol-version)
        (keyword? shadow-protocol-source) (assoc :protocol/shadow-artifact-source shadow-protocol-source)))))

(defn- routing-signature
  [mode request]
  (let [request' (if (map? request) request {})
        intent (some-> request' :task :intent keywordish)
        cap-id (or (some-> request' :task :cap/id keywordish)
                   (some-> request' :routing :decision :cap/id keywordish)
                   (some-> request' :routing :decision :selected keywordish))
        candidate0 (some-> request'
                           :routing
                           :decision
                           :candidates
                           first
                           keywordish)]
    (cond-> {:mode (or (keywordish mode) :none)}
      (keyword? intent) (assoc :intent intent)
      (keyword? cap-id) (assoc :cap/id cap-id)
      (keyword? candidate0) (assoc :candidate candidate0))))

(defn- shadow-routing-telemetry-counters
  [shadow-enabled? shadow-attempted? shadow-match? shadow-mode]
  (cond-> {}
    shadow-enabled? (update :route/shadow-enabled (fnil inc 0))
    shadow-attempted? (update :route/shadow-attempt (fnil inc 0))
    (and shadow-attempted? (true? shadow-match?)) (update :route/shadow-match (fnil inc 0))
    (and shadow-attempted? (false? shadow-match?)) (update :route/shadow-mismatch (fnil inc 0))
    (and shadow-attempted? (= :error shadow-mode)) (update :route/shadow-error (fnil inc 0))))

(defn- invoke-act-route-phase
  [runtime request accepted-mode? protocol resolver]
  (let [req-check        (contracts/validate-request protocol request)
        [meta-step0 route-phase-latency-ms]
        (if (and (:ok? req-check)
                 (not accepted-mode?))
          (let [started-at (now-nanos)
                step       (maybe-apply-meta-routing runtime resolver request)]
            [step (nanos->millis started-at)])
          [nil nil])
        route-decider-latency-ms (when (map? meta-step0)
                                   (let [latency (:decider-latency-ms meta-step0)]
                                     (when (number? latency)
                                       latency)))
        meta-step        (cond-> meta-step0
                           (and (map? meta-step0)
                                (number? route-phase-latency-ms))
                           (assoc :latency-ms route-phase-latency-ms))
        route-mode       (or (:mode meta-step) :none)
        request*         (or (:request meta-step) request)
        routed?          (not= request request*)
        shadow-enabled?  (true? (:router/shadow-enabled? runtime))
        shadow-applied?  (and shadow-enabled?
                              (true? (:router/shadow-applied? runtime))
                              (map? (:router/shadow runtime)))
        [shadow-meta-step shadow-route-phase-latency-ms]
        (if (and shadow-applied?
                 (:ok? req-check)
                 (not accepted-mode?))
          (let [runtime-shadow (cond-> (assoc runtime :router (:router/shadow runtime))
                                 (keyword? (:router/shadow-artifact-version runtime))
                                 (assoc :router/artifact-version (:router/shadow-artifact-version runtime))
                                 (keyword? (:router/shadow-artifact-source runtime))
                                 (assoc :router/artifact-source (:router/shadow-artifact-source runtime)))
                resolver-shadow (effective-resolver runtime-shadow)
                started-at (now-nanos)
                step (maybe-apply-meta-routing runtime-shadow resolver-shadow request)]
            [step (nanos->millis started-at)])
          [nil nil])
        shadow-route-mode (or (:mode shadow-meta-step) :none)
        shadow-request* (or (:request shadow-meta-step) request)
        primary-signature (routing-signature route-mode request*)
        shadow-signature (when (map? shadow-meta-step)
                           (routing-signature shadow-route-mode shadow-request*))
        shadow-match? (when (map? shadow-signature)
                        (= primary-signature shadow-signature))
        post-route-check (cond
                           (not (:ok? req-check)) req-check
                           (#{:error :final} route-mode) {:ok? true}
                           routed? (contracts/validate-request protocol request*)
                           :else req-check)
        route-telemetry (telemetry/merge-counters
                         (routing-telemetry-counters meta-step)
                         (shadow-routing-telemetry-counters shadow-enabled?
                                                            (map? shadow-meta-step)
                                                            shadow-match?
                                                            shadow-route-mode))]
    {:req-check req-check
     :meta-step meta-step
     :route-mode route-mode
     :request* request*
     :routed? routed?
     :routing/shadow-enabled? shadow-enabled?
     :routing/shadow-attempted? (map? shadow-meta-step)
     :routing/shadow-match? shadow-match?
     :routing/shadow-route-mode shadow-route-mode
     :routing/shadow-primary-signature primary-signature
     :routing/shadow-candidate-signature shadow-signature
     :routing/shadow-route-phase-latency-ms shadow-route-phase-latency-ms
     :route-decide-latency-ms route-decider-latency-ms
     :route-phase-latency-ms route-phase-latency-ms
     :route-decider-latency-ms route-decider-latency-ms
     :post-route-check post-route-check
     :route-telemetry route-telemetry}))

(defn- invoke-act-capability-error-response
  [request* ^clojure.lang.ExceptionInfo e]
  (let [data   (or (ex-data e) {})
        reason (or (:error data)
                   (:failure/type data))]
    (case reason
      :invalid-request
      {:status 400
       :body   (error-envelope request*
                               :input/invalid
                               (.getMessage e)
                               (select-keys data [:reason :intent]))}

      :invalid-result-after-retries
      {:status 502
       :body   (error-envelope request*
                               :schema/invalid
                               (.getMessage e)
                               (select-keys data [:attempts :last-check])
                               true)}

      :auth/forbidden-effect
      {:status 403
       :body   (error-envelope request*
                               :auth/forbidden-effect
                               (.getMessage e)
                               (select-keys data [:requested-effects
                                                  :denied-effects
                                                  :failure/type]))}

      :effects/scope-denied
      {:status 403
       :body   (error-envelope request*
                               :effects/scope-denied
                               (.getMessage e)
                               (select-keys data [:effect
                                                  :reason
                                                  :path
                                                  :cwd
                                                  :url
                                                  :allow
                                                  :allow-cwd
                                                  :allow-hosts
                                                  :allow-ports
                                                  :allow-schemes]))}

      :effects/invalid-input
      {:status 400
       :body   (error-envelope request*
                               :effects/invalid-input
                               (.getMessage e)
                               (select-keys data [:reason
                                                  :tool/id
                                                  :required-effect
                                                  :requested-effects]))}

      :effects/not-declared
      {:status 400
       :body   (error-envelope request*
                               :effects/invalid-input
                               (.getMessage e)
                               (merge {:reason :effects/not-declared}
                                      (select-keys data [:tool/id
                                                         :requested-effects])))}

      :effects/unsupported-tool
      {:status 422
       :body   (error-envelope request*
                               :effects/unsupported-tool
                               (.getMessage e)
                               (select-keys data [:tool/id :known-tools]))}

      (let [invoke-response (:invoke-response data)
            details' (merge (select-keys data [:error :reason :cap-id :intent :model-key :session/id])
                            (when (map? invoke-response)
                              (select-keys invoke-response [:error :message :details])))]
        {:status 502
         :body   (error-envelope request*
                                 :runtime/invoke-failed
                                 (.getMessage e)
                                 (when (seq details')
                                   details'))}))))

(defn- invoke-act-runtime-response
  [runtime resolver request* cap-id]
  (try
    {:status 200
     :body   (core/call-capability
              runtime
              resolver
              (act-request->invoke-opts runtime resolver request* cap-id))}
    (catch clojure.lang.ExceptionInfo e
      (invoke-act-capability-error-response request* e))
    (catch Throwable t
      {:status 500
       :body   (error-envelope request*
                               :runtime/internal
                               (.getMessage t))})))

(defn- invoke-act-cache-phase
  [runtime request* accepted-mode? cap-id]
  (let [cache-key     (when (and (cacheable-act-request? request*)
                                 (not accepted-mode?)
                                 (keyword? cap-id))
                        (act-cache-key request* cap-id))
        cache-lookup  (act-cache-get! runtime cache-key)
        cache-hit?    (true? (:hit? cache-lookup))
        cache-telemetry0 (if (map? (:telemetry cache-lookup))
                           (:telemetry cache-lookup)
                           {})]
    {:cache/key cache-key
     :cache/lookup cache-lookup
     :cache/hit? cache-hit?
     :cache/telemetry0 cache-telemetry0}))

(defn- invoke-act-response
  [runtime phase]
  (let [{:keys [request
                request*
                accepted-mode?
                req-check
                post-route-check
                route-mode
                meta-step
                routed?
                resolver
                cap-id]} phase
        cache-lookup (:cache/lookup phase)
        cache-hit?   (:cache/hit? phase)]
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

    accepted-mode?
    (async-submit-response runtime request*)

    (= :error route-mode)
    {:status (or (:status meta-step) 502)
     :body   (or (:body meta-step)
                 (error-envelope request
                                 :route/decide-failed
                                 "Meta routing failed."
                                 nil
                                 true))}

    (= :final route-mode)
    {:status 200
     :body   (or (:response meta-step)
                 (error-envelope request
                                 :route/decide-failed
                                 "Meta routing returned invalid response."
                                 nil
                                 true))}

    (not (:ok? post-route-check))
    (if routed?
      {:status 502
       :body   (error-envelope request*
                               :route/decide-failed
                               "Meta routing returned invalid request mutations."
                               (select-keys post-route-check [:reason :intent])
                               true)}
      {:status 400
       :body   (error-envelope request*
                               :input/invalid
                               "Request does not satisfy protocol contract."
                               (select-keys post-route-check [:reason :intent]))})

    :else
    (if-not (keyword? cap-id)
      {:status 422
       :body   (error-envelope request*
                               :unsupported/intent
                               "No capability can handle the requested intent."
                               (cap-resolution-error-details request*
                                                             (:cap/decision phase)))}
      (if (and cache-hit? (map? (:response cache-lookup)))
        (:response cache-lookup)
        (invoke-act-runtime-response runtime resolver request* cap-id))))))

(defn- invoke-act-execute-phase
  [runtime phase]
  (let [{:keys [request*
                accepted-mode?
                req-check
                post-route-check
                route-mode
                resolver]} phase
        tenancy-cfg (runtime-tenancy-config runtime)
        tenancy-ctx (when (map? request*)
                      (tenancy/resolve-context tenancy-cfg request*))
        tenancy-result (when (and (map? request*)
                                  (not accepted-mode?)
                                  (:ok? req-check)
                                  (:ok? post-route-check)
                                  (not= :error route-mode)
                                  (not= :final route-mode))
                         (tenancy/reserve! (runtime-tenancy-state runtime)
                                           tenancy-ctx
                                           (System/currentTimeMillis)))
        tenancy-failure? (and (map? tenancy-result)
                              (false? (:ok? tenancy-result)))
        tenancy-error-response (when tenancy-failure?
                                 {:status 429
                                  :body (error-envelope request*
                                                        (or (:error tenancy-result)
                                                            :tenant/limit-exceeded)
                                                        "Tenant policy rejected the request."
                                                        (:details tenancy-result)
                                                        true)})
        cap-decision (when (and (not tenancy-failure?)
                                (map? request*)
                                (:ok? req-check)
                                (:ok? post-route-check)
                                (not accepted-mode?)
                                (not= :error route-mode)
                                (not= :final route-mode))
                       (resolve-capability-decision resolver request*))
        cap-id       (some-> cap-decision :cap/id keywordish)
        cache-phase (invoke-act-cache-phase runtime request* accepted-mode? cap-id)
        routing-telemetry (telemetry/merge-counters
                           (:route-telemetry phase)
                           (cap-resolution-telemetry-counters cap-decision))
        phase' (merge phase
                      cache-phase
                      {:cap-id cap-id
                       :cap/decision cap-decision
                       :tenancy/context tenancy-ctx
                       :tenancy/reservation (:reservation tenancy-result)
                       :route-telemetry routing-telemetry})
        response (if (map? tenancy-error-response)
                   tenancy-error-response
                   (invoke-act-response runtime phase'))
        cache-store-telemetry (if (or (:cache/hit? phase')
                                     (not (cacheable-act-request? request*)))
                                {}
                                (act-cache-put! runtime (:cache/key phase') response))
        cache-telemetry (ferment.telemetry/merge-counters
                         (:cache/telemetry0 phase')
                         cache-store-telemetry)]
    (assoc phase'
           :response response
           :cache/telemetry cache-telemetry)))

(defn- memory-write-enabled?
  [policy intent]
  (let [by-intent (if (map? (:write/by-intent policy))
                    (:write/by-intent policy)
                    {})
        default? (true? (:write/default? policy))]
    (if (contains? by-intent intent)
      (true? (get by-intent intent))
      default?)))

(defn- compact-memory-text
  [policy text]
  (let [max-chars (or (positive-int (:write/max-chars policy)) 1200)
        trigger (or (positive-int (:compaction/trigger-chars policy)) max-chars)
        target (or (positive-int (:compaction/target-chars policy)) (min trigger max-chars))
        target' (min target max-chars)
        mode (keywordish (:compaction/mode policy))
        text' (or (some-> text str str/trim not-empty) "")]
    (cond
      (str/blank? text') nil
      (and (= :truncate mode)
           (> (count text') trigger))
      (subs text' 0 target')
      (> (count text') max-chars) (subs text' 0 max-chars)
      :else text')))

(defn- write-act-memory-summary!
  [runtime request body]
  (let [service (when (map? runtime) (:session runtime))
        sid (or (some-> (:session/id request) trim-s)
                (some-> (:session-id request) trim-s))
        intent (some-> request :task :intent keywordish)
        policy (session-memory-policy runtime)
        enabled? (and (map? policy)
                      (true? (:enabled? policy))
                      (keyword? intent)
                      (memory-write-enabled? policy intent)
                      sid)
        text (or (some-> body :result :out :text)
                 (some-> body :result :out :content))
        compacted (when enabled?
                    (compact-memory-text policy text))
        write-key (or (some-> (:write/key policy) keywordish)
                      :context/summary)
        principal-isolation? (true? (:principal/isolation? policy))
        principal-key (or (some-> (:principal/key policy) keywordish)
                          :context/principal-id)
        principal-token (principal-token-from-request request)
        history-enabled? (true? (:history/enabled? policy))
        history-key (or (some-> (:history/key policy) keywordish)
                        :context/history)
        history-max-items (or (positive-int (:history/max-items policy)) 8)]
    (when (and enabled?
               (keyword? write-key)
               (some? compacted)
               (fn? (:put-vars! service)))
      (try
        (let [history-current (when (and history-enabled?
                                         (keyword? history-key)
                                         (fn? (:get-var! service)))
                                (memory/get-var! service
                                                 sid
                                                 history-key
                                                 {:operation :act/memory-auto-write
                                                  :intent intent}))
              history-items0 (if (sequential? history-current)
                               (->> history-current
                                    (keep #(some-> % str trim-s))
                                    vec)
                               [])
              history-items1 (if (= compacted (last history-items0))
                               history-items0
                               (conj history-items0 compacted))
              history-items' (if (pos? history-max-items)
                               (->> history-items1
                                    (take-last history-max-items)
                                    vec)
                               [])
              vars (cond-> {write-key compacted
                            :context/last-intent intent
                            :context/last-updated-at (str (java.time.Instant/now))}
                     (and principal-isolation?
                          (keyword? principal-key)
                          (some? principal-token))
                     (assoc principal-key principal-token)
                     (and history-enabled?
                          (keyword? history-key)
                          (seq history-items'))
                     (assoc history-key history-items'))]
          (memory/put-vars! service
                            sid
                            vars
                            {:operation :act/memory-auto-write
                             :intent intent}))
        (catch Throwable _
          nil)))))

(defn- invoke-act-session-view
  [runtime request*]
  (let [sid (or (some-> (:session/id request*) trim-s)
                (some-> (:session-id request*) trim-s))
        session-state (when sid
                        (let [service (when (map? runtime) (:session runtime))]
                          (when (map? service)
                            (try
                              (memory/get! service sid)
                              (catch Throwable _ nil)))))]
    (when (map? session-state)
      (select-keys session-state
                   [:session/id
                    :session/version
                    :session/state
                    :session/frozen?
                    :session/updated-at
                    :session/last-access-at
                    :session/frozen-at
                    :session/thawed-at]))))

(defn- invoke-act-finalize-phase
  [runtime {:keys [response request*] :as phase}]
  (let [route-decide-latency-ms (:route-decide-latency-ms phase)
        route-phase-latency-ms (:route-phase-latency-ms phase)
        route-decider-latency-ms (:route-decider-latency-ms phase)
        routing-shadow-enabled? (true? (:routing/shadow-enabled? phase))
        routing-shadow-attempted? (true? (:routing/shadow-attempted? phase))
        routing-shadow-match? (:routing/shadow-match? phase)
        routing-shadow-route-mode (some-> (:routing/shadow-route-mode phase) keywordish)
        routing-shadow-primary-signature (if (map? (:routing/shadow-primary-signature phase))
                                           (:routing/shadow-primary-signature phase)
                                           nil)
        routing-shadow-candidate-signature (if (map? (:routing/shadow-candidate-signature phase))
                                             (:routing/shadow-candidate-signature phase)
                                             nil)
        routing-shadow-route-phase-latency-ms (:routing/shadow-route-phase-latency-ms phase)
        protocol-selection (protocol/select-protocol-artifact
                            (runtime-protocol-config runtime)
                            {:trace-id (get-in request* [:trace :id])
                             :requested-version (some-> request* :routing :artifact/version)})
        protocol-artifact-version (some-> (:artifact/version protocol-selection) keywordish)
        protocol-artifact-source (some-> (:artifact/source protocol-selection) keywordish)
        protocol-shadow-artifact-version (some-> runtime :protocol/shadow-artifact-version keywordish)
        protocol-shadow-artifact-source (some-> runtime :protocol/shadow-artifact-source keywordish)
        protocol-shadow-match? (when (and (keyword? protocol-shadow-artifact-version)
                                          (keyword? protocol-artifact-version))
                                 (= protocol-shadow-artifact-version protocol-artifact-version))
        router-artifact-version (some-> (:router/artifact-version runtime) keywordish)
        router-artifact-source (some-> (:router/artifact-source runtime) keywordish)
        router-shadow-artifact-version (some-> runtime :router/shadow-artifact-version keywordish)
        router-shadow-artifact-source (some-> runtime :router/shadow-artifact-source keywordish)
        response0   (if (and (map? response)
                             (map? (:body response)))
                      (cond-> response
                        (number? route-decide-latency-ms)
                        (assoc-in [:body :routing/route-decide-latency-ms]
                                  (double route-decide-latency-ms))
                        (number? route-phase-latency-ms)
                        (assoc-in [:body :routing/route-phase-latency-ms]
                                  (double route-phase-latency-ms))
                        (number? route-decider-latency-ms)
                        (assoc-in [:body :routing/route-decider-latency-ms]
                                  (double route-decider-latency-ms))
                        (keyword? protocol-artifact-version)
                        (assoc-in [:body :protocol/artifact-version]
                                  protocol-artifact-version)
                        (keyword? protocol-artifact-source)
                        (assoc-in [:body :protocol/artifact-source]
                                  protocol-artifact-source)
                        (keyword? router-artifact-version)
                        (assoc-in [:body :routing/router-artifact-version]
                                  router-artifact-version)
                        (keyword? router-artifact-source)
                        (assoc-in [:body :routing/router-artifact-source]
                                  router-artifact-source)
                        routing-shadow-enabled?
                        (assoc-in [:body :routing/router-shadow-enabled?] true)
                        routing-shadow-attempted?
                        (assoc-in [:body :routing/router-shadow-attempted?] true)
                        (boolean? routing-shadow-match?)
                        (assoc-in [:body :routing/router-shadow-match?] routing-shadow-match?)
                        (keyword? routing-shadow-route-mode)
                        (assoc-in [:body :routing/router-shadow-route-mode] routing-shadow-route-mode)
                        (map? routing-shadow-primary-signature)
                        (assoc-in [:body :routing/router-shadow-primary-signature] routing-shadow-primary-signature)
                        (map? routing-shadow-candidate-signature)
                        (assoc-in [:body :routing/router-shadow-candidate-signature] routing-shadow-candidate-signature)
                        (number? routing-shadow-route-phase-latency-ms)
                        (assoc-in [:body :routing/router-shadow-route-phase-latency-ms]
                                  (double routing-shadow-route-phase-latency-ms))
                        (keyword? router-shadow-artifact-version)
                        (assoc-in [:body :routing/router-shadow-artifact-version]
                                  router-shadow-artifact-version)
                        (keyword? router-shadow-artifact-source)
                        (assoc-in [:body :routing/router-shadow-artifact-source]
                                  router-shadow-artifact-source)
                        (keyword? protocol-shadow-artifact-version)
                        (assoc-in [:body :protocol/shadow-artifact-version]
                                  protocol-shadow-artifact-version)
                        (keyword? protocol-shadow-artifact-source)
                        (assoc-in [:body :protocol/shadow-artifact-source]
                                  protocol-shadow-artifact-source)
                        (boolean? protocol-shadow-match?)
                        (assoc-in [:body :protocol/shadow-match?]
                                  protocol-shadow-match?))
                      response)
        response'   (attach-response-participants response0)
        response''  (if (map? (:body response'))
                      (update response' :body sanitize-final-response-body)
                      response')
        _           (when (and (map? (:body response''))
                               (map? request*))
                      (write-act-memory-summary! runtime request* (:body response'')))
        session-view (invoke-act-session-view runtime request*)
        response''' (if (and (map? (:body response''))
                             (map? session-view))
                      (update response'' :body merge session-view)
                      response'')]
    (assoc phase :response/final response''')))

(defn- default-act-middleware-modules
  []
  [(act-middleware-prepare/middleware)
   (act-middleware-route/middleware)
   (act-middleware-execute/middleware)
   (act-middleware-finalize/middleware)])

(defn- act-middleware-module?
  [v]
  (and (map? v)
       (keyword? (:name v))
       (fn? (:compile v))))

(defn- compile-act-middleware-stage
  [runtime idx module]
  (when-not (act-middleware-module? module)
    (throw (ex-info "Act middleware module has invalid shape."
                    {:error :act/middleware-invalid
                     :index idx
                     :module module})))
  (let [compiled ((:compile module) runtime {:index idx
                                             :module/name (:name module)})]
    (when-not (fn? compiled)
      (throw (ex-info "Act middleware module compile did not return a handler wrapper."
                      {:error :act/middleware-compile-invalid
                       :index idx
                       :module/name (:name module)
                       :compiled compiled})))
    compiled))

(defn- compile-act-middleware-chain
  [runtime modules]
  (let [modules'  (->> modules (remove nil?) vec)
        modules'' (if (seq modules')
                    modules'
                    (default-act-middleware-modules))
        wrappers  (mapv (fn [idx module]
                          (compile-act-middleware-stage runtime idx module))
                        (range)
                        modules'')]
    (reduce (fn [handler wrapper]
              (wrapper handler))
            (fn [ctx] ctx)
            (reverse wrappers))))

(def ^:private default-act-pipeline
  (delay (compile-act-middleware-chain nil nil)))

(defn- runtime-act-pipeline
  [runtime]
  (let [pipeline (when (map? runtime) (:act/pipeline runtime))]
    (if (fn? pipeline)
      pipeline
      (let [modules (when (map? runtime) (:act/middleware runtime))]
        (if (sequential? modules)
          (compile-act-middleware-chain runtime modules)
          @default-act-pipeline)))))

(defn invoke-act
  "Runs canonical `/v1/act` request through contract validation and core capability flow.

  Returns:
  - `{:status <http-status> :body <canonical-response-envelope>}`"
  ([runtime payload]
   (invoke-act runtime payload nil nil))
  ([runtime payload telemetry]
   (invoke-act runtime payload telemetry nil))
  ([runtime payload telemetry auth]
   (let [runtime*        (cond-> runtime
                           (and (map? runtime)
                                (instance? clojure.lang.IAtom telemetry))
                           (assoc :telemetry telemetry))
         telemetry-before (when (instance? clojure.lang.IAtom telemetry)
                            (telemetry-snapshot telemetry))
         started-at      (now-nanos)
         phase3          ((runtime-act-pipeline runtime*)
                          {:runtime runtime*
                           :payload payload
                           :act/fns {:prepare-request invoke-act-prepared-request
                                     :select-runtime invoke-act-select-runtime
                                     :accepted-mode? response-type-accepted?
                                     :effective-resolver effective-resolver
                                     :route-phase invoke-act-route-phase
                                     :execute-phase invoke-act-execute-phase
                                     :finalize-phase invoke-act-finalize-phase}
                           :telemetry telemetry
                           :auth auth})
         response''      (or (:response phase3)
                             (:response/final phase3)
                             {:status 500
                              :body   (error-envelope nil
                                                      :runtime/internal
                                                      "Act middleware pipeline produced invalid response envelope.")})
         elapsed-ms      (nanos->millis started-at)
         route-telemetry (:route-telemetry phase3)
         cache-telemetry (:cache/telemetry phase3)
         request*        (:request* phase3)
         replay-enabled?* (replay-enabled? runtime* request*)]
     (when (map? (:tenancy/reservation phase3))
       (tenancy/finalize! (runtime-tenancy-state runtime*)
                          (:tenancy/reservation phase3)
                          response''
                          elapsed-ms))
     (record-act-telemetry! telemetry response'' elapsed-ms route-telemetry cache-telemetry)
     (let [telemetry-after (when (and replay-enabled?*
                                      (instance? clojure.lang.IAtom telemetry))
                             (telemetry-snapshot telemetry))]
       (report-act! runtime* request* response'' auth elapsed-ms)
       (when replay-enabled?*
         (record-act-replay! runtime* phase3 response'' auth elapsed-ms telemetry-before telemetry-after)))
     response'')))

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

(defn- session-var-key
  [payload]
  (or (some-> payload :key keywordish)
      (some-> payload :var/key keywordish)
      (some-> payload :k keywordish)))

(defn- session-var-keys
  [payload]
  (let [ks (or (:keys payload)
               (:var/keys payload))]
    (when (or (set? ks) (sequential? ks))
      (->> ks
           (keep keywordish)
           distinct
           vec))))

(defn- session-var-pairs
  [payload]
  (let [pairs (or (:vars payload)
                  (:kvs payload))]
    (when (map? pairs)
      (->> pairs
           (reduce-kv (fn [m k v]
                        (if-some [k' (keywordish k)]
                          (assoc m k' v)
                          m))
                      {})
           not-empty))))

(defn- session-var-value-present?
  [payload]
  (or (contains? payload :value)
      (contains? payload :var/value)))

(defn- session-var-value-from-payload
  [payload]
  (if (contains? payload :value)
    (:value payload)
    (:var/value payload)))

(defn- session-action
  [payload]
  (some-> payload :action keywordish))

(defn- session-var-op-opts
  [payload action opts]
  (let [opts'  (if (map? opts) opts {})
        intent (or (some-> opts' :intent keywordish)
                   (some-> payload :intent keywordish))
        op     (or (some-> opts' :operation keywordish)
                   action)]
    (cond-> opts'
      (keyword? intent) (assoc :intent intent)
      (keyword? op) (assoc :operation op))))

(defn- session-op-var-keys
  [action payload]
  (let [keys* (case action
                (:session/get-var :session/put-var :session/del-var)
                (some-> (session-var-key payload) vector)

                (:session/get-vars :session/del-vars)
                (session-var-keys payload)

                :session/put-vars
                (some-> (session-var-pairs payload) keys vec)

                nil)]
    (when (seq keys*)
      (->> keys* (keep keywordish) distinct sort vec))))

(defn- report-session!
  [runtime payload response auth elapsed-ms]
  (let [logger (oplog/logger :act runtime)]
    (when (fn? logger)
      (let [principal  (audit-principal auth payload)
            action     (session-action payload)
            status     (int (or (:status response) 500))
            outcome    (response-outcome response)
            error-type (response-error-type response)
            message    (or (some-> response :body :message trim-s)
                           (some-> response :body :error :message trim-s)
                           (when (= :ok outcome) "Session request processed.")
                           "Session request failed.")
            sid        (session-id-from-payload payload)
            tenant-id  (or (some-> payload :tenant/id keywordish)
                           (some-> principal :user/tenant-id keywordish))
            trace-id   (some-> payload :trace :id trim-s)
            request-id (some-> payload :request/id trim-s)
            var-keys   (session-op-var-keys action payload)]
        (apply logger
               (mapcat identity
                       (cond-> {:endpoint "/v1/session"
                                :operation action
                                :trace-id trace-id
                                :request-id request-id
                                :session-id sid
                                :outcome outcome
                                :status status
                                :error-type error-type
                                :latency-ms elapsed-ms
                                :message message}
                         (keyword? tenant-id)
                         (assoc :tenant-id tenant-id)
                         (seq var-keys) (assoc :session/var-keys var-keys)
                         (some? (:user/id principal))
                         (assoc :principal-id (:user/id principal))
                         (some? (:user/email principal))
                         (assoc :principal-email (:user/email principal))
                         (some? (:user/account-type principal))
                         (assoc :principal-account-type (:user/account-type principal))
                         (seq (:user/roles principal))
                         (assoc :principal-roles (vec (:user/roles principal))))))))))

(defn- session-op-error-status
  [error-k]
  (case error-k
    :session.vars/policy-read-forbidden 403
    :session.vars/policy-write-forbidden 403
    :session.vars/policy-delete-forbidden 403
    :session.vars/session-frozen 409
    :session.vars/limit-exceeded 409
    :session.vars/key-too-long 422
    :session.vars/value-too-large 422
    :session.vars/key-namespace-forbidden 422
    400))

(defn- session-op-error-response
  [^Throwable e]
  (let [data (or (ex-data e) {})
        err-k (or (some-> (:error data) keywordish)
                  :runtime/internal)
        details (when (map? data)
                  (dissoc data :error))]
    {:status (session-op-error-status err-k)
     :body (cond-> {:ok? false
                    :error err-k
                    :message (.getMessage e)}
             (seq details) (assoc :details details))}))

(defn- with-session-op
  [f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (session-op-error-response e))
    (catch Throwable t
      {:status 500
       :body {:ok? false
              :error :runtime/internal
              :message (.getMessage t)}})))

(defn- session-action-response
  [runtime payload telemetry]
  (let [action      (keywordish (:action payload))
        session-id  (session-id-from-payload payload)
        model-id    (or (:model payload)
                        (:model-id payload)
                        (:model/id payload))
        opts        (if (map? (:opts payload)) (:opts payload) {})
        var-opts    (session-var-op-opts payload action opts)
        service     (runtime-session-service runtime)
        response
        (case action
      :state
      {:status 200
       :body {:ok? true
              :workers (model-adapter/session-workers-state runtime)}}

      :expire
      (do
        (model-adapter/expire-session-workers! runtime)
        {:status 200
         :body {:ok? true
                :workers (model-adapter/session-workers-state runtime)}})

      :worker/thaw
      (if (and session-id model-id)
        {:status 200
         :body (model-adapter/thaw-session-worker! runtime model-id session-id)}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing required keys: :session/id and :model."}})

      :worker/freeze
      (if (and session-id model-id)
        {:status 200
         :body (model-adapter/freeze-session-worker! runtime model-id session-id)}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing required keys: :session/id and :model."}})

      :session/open
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (memory/open! service session-id opts))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/get
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (memory/get! service session-id))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/thaw
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (memory/thaw! service session-id opts))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/freeze
      (if (and service session-id)
        {:status 200
         :body {:ok? true
                :session (session-public (memory/freeze! service session-id opts))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service or :session/id."}})

      :session/list
      (if service
        {:status 200
         :body {:ok? true
                :sessions (mapv session-public (memory/list! service))}}
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing session service in runtime."}})

      :session/get-var
      (let [k (session-var-key payload)]
        (if (and service session-id (keyword? k))
          (with-session-op
            (fn []
              {:status 200
               :body {:ok? true
                      :session/id session-id
                      :key k
                      :value (memory/get-var! service session-id k var-opts)}}))
          {:status 400
           :body {:ok? false
                  :error :input/invalid
                  :message "Missing required keys: :session/id and :key (keyword-like)."}}))

      :session/get-vars
      (let [ks (session-var-keys payload)]
        (if (and service session-id (seq ks))
          (with-session-op
            (fn []
              {:status 200
               :body {:ok? true
                      :session/id session-id
                      :keys ks
                      :vars (memory/get-vars! service session-id ks var-opts)}}))
          {:status 400
           :body {:ok? false
                  :error :input/invalid
                  :message "Missing required keys: :session/id and :keys (sequential keyword-like values)."}}))

      :session/put-var
      (let [k (session-var-key payload)
            value-present? (session-var-value-present? payload)
            value' (session-var-value-from-payload payload)]
        (if (and service session-id (keyword? k) value-present?)
          (with-session-op
            (fn []
              {:status 200
               :body {:ok? true
                      :session/id session-id
                      :key k
                      :written? (memory/put-var! service session-id k value' var-opts)}}))
          {:status 400
           :body {:ok? false
                  :error :input/invalid
                  :message "Missing required keys: :session/id, :key and :value."}}))

      :session/put-vars
      (let [vars' (session-var-pairs payload)]
        (if (and service session-id (map? vars') (seq vars'))
          (with-session-op
            (fn []
              {:status 200
               :body {:ok? true
                      :session/id session-id
                      :written? (memory/put-vars! service session-id vars' var-opts)}}))
          {:status 400
           :body {:ok? false
                  :error :input/invalid
                  :message "Missing required keys: :session/id and :vars (map with keyword-like keys)."}}))

      :session/del-var
      (let [k (session-var-key payload)]
        (if (and service session-id (keyword? k))
          (with-session-op
            (fn []
              {:status 200
               :body {:ok? true
                      :session/id session-id
                      :key k
                      :deleted? (memory/del-var! service session-id k var-opts)}}))
          {:status 400
           :body {:ok? false
                  :error :input/invalid
                  :message "Missing required keys: :session/id and :key (keyword-like)."}}))

      :session/del-vars
      (let [ks (session-var-keys payload)]
        (if (and service session-id (seq ks))
          (with-session-op
            (fn []
              {:status 200
               :body {:ok? true
                      :session/id session-id
                      :keys ks
                      :deleted? (memory/del-vars! service session-id ks var-opts)}}))
          {:status 400
           :body {:ok? false
                  :error :input/invalid
                  :message "Missing required keys: :session/id and :keys (sequential keyword-like values)."}}))

      :session/del-all-vars
      (if (and service session-id)
        (with-session-op
          (fn []
            {:status 200
             :body {:ok? true
                    :session/id session-id
                    :deleted? (memory/del-all-vars! service session-id var-opts)}}))
        {:status 400
         :body {:ok? false
                :error :input/invalid
                :message "Missing required key: :session/id."}})

      {:status 400
       :body {:ok? false
              :error :input/invalid
              :message "Unsupported session action."
              :details {:action action
                        :supported #{:state :expire
                                     :worker/thaw :worker/freeze
                                     :session/open :session/get
                                     :session/thaw :session/freeze
                                     :session/list
                                     :session/get-var :session/get-vars
                                     :session/put-var :session/put-vars
                                     :session/del-var :session/del-vars
                                     :session/del-all-vars}}}})]
    (let [mutating-action? (contains? #{:session/open
                                        :session/thaw
                                        :session/freeze
                                        :session/put-var
                                        :session/put-vars
                                        :session/del-var
                                        :session/del-vars
                                        :session/del-all-vars}
                                      action)]
      (when (and mutating-action?
                 (< (int (or (:status response) 500)) 400)
                 session-id)
        (let [invalidated (act-cache-invalidate-session! runtime session-id)]
          (when (pos? invalidated)
            (record-cache-telemetry! telemetry {:invalidations invalidated}))))
      response)))

(def ^:private admin-supported-actions
  #{:admin/create-user
    :admin/create-role
    :admin/delete-user
    :admin/delete-role
    :admin/set-password
    :admin/lock-user
    :admin/unlock-user
    :admin/grant-role
    :admin/revoke-role
    :admin/list-roles
    :admin/list-known-roles
    :admin/migrate-db
    :admin/rollback-db
    :admin/reset-login-attempts
    :admin/get-artifact-rollout
    :admin/set-artifact-rollout})

(defn- normalize-admin-action
  [v]
  (when-some [action (keywordish v)]
    (when (contains? admin-supported-actions action)
      action)))

(defn- payload-params
  [payload]
  (let [payload' (if (map? payload) payload {})
        params   (if (map? (:params payload')) (:params payload') {})]
    (merge payload' params)))

(defn- selector-from-payload
  [payload]
  (cond
    (contains? payload :selector) (:selector payload)
    (contains? payload :id)       (:id payload)
    (contains? payload :user/id)  (:user/id payload)
    (contains? payload :email)    (:email payload)
    (contains? payload :user/email) (:user/email payload)
    :else nil))

(defn- admin-result-status
  [result]
  (if (and (map? result) (:ok? result))
    200
    (case (:error result)
      :input/invalid          400
      :user/invalid-lock-kind 400
      :user/not-found         404
      :user/already-exists    409
      :user/invalid-role      400
      :user/unknown-role      422
      :role/invalid-role      400
      :role/not-found         404
      :role/in-use            409
      :auth/not-configured    500
      :db/not-configured      500
      :runtime/not-configured 500
      400)))

(defn- invoke-admin-action
  [runtime action payload]
  (let [params     (payload-params payload)
        selector   (selector-from-payload params)
        email      (or (trim-s (:email params))
                       (trim-s (:user/email params)))
        password   (or (trim-s (:password params))
                       (trim-s (:new-password params)))
        role       (or (some-> (:role params) keywordish)
                       (some-> (:user/role params) keywordish))
        account-type (or (some-> (:account-type params) keywordish)
                         (some-> (:user/account-type params) keywordish))
        artifact   (or (some-> (:artifact params) keywordish)
                       (some-> (:target params) keywordish))
        active     (or (:active params)
                       (:artifact/version params))
        canary     (if (map? (:canary params)) (:canary params) nil)
        shadow     (if (map? (:shadow params)) (:shadow params) nil)
        clear?     (coerce-bool (:clear? params))]
    (case action
      :admin/create-user
      (if (and email password)
        (if account-type
          (admin/create-user! email password account-type)
          (admin/create-user! email password))
        {:ok? false
         :error :input/invalid
         :message "Missing required keys: :email and :password."})

      :admin/create-role
      (if (keyword? role)
        (if-some [description (trim-s (:description params))]
          (admin/create-role! role description)
          (admin/create-role! role))
        {:ok? false
         :error :input/invalid
         :message "Missing required key: :role."})

      :admin/delete-user
      (if (some? selector)
        (admin/delete-user! selector)
        {:ok? false
         :error :input/invalid
         :message "Missing required selector: :selector or :id or :email."})

      :admin/delete-role
      (if (keyword? role)
        (admin/delete-role! role)
        {:ok? false
         :error :input/invalid
         :message "Missing required key: :role."})

      :admin/set-password
      (if (and (some? selector) password)
        (if account-type
          (admin/set-password! selector password account-type)
          (admin/set-password! selector password))
        {:ok? false
         :error :input/invalid
         :message "Missing required keys: selector and :password (or :new-password)."})

      :admin/lock-user
      (if (some? selector)
        (if (contains? params :lock-kind)
          (admin/lock-user! selector (:lock-kind params))
          (admin/lock-user! selector))
        {:ok? false
         :error :input/invalid
         :message "Missing required selector: :selector or :id or :email."})

      :admin/unlock-user
      (if (some? selector)
        (admin/unlock-user! selector)
        {:ok? false
         :error :input/invalid
         :message "Missing required selector: :selector or :id or :email."})

      :admin/grant-role
      (if (and (some? selector) (keyword? role))
        (admin/grant-role! selector role)
        {:ok? false
         :error :input/invalid
         :message "Missing required keys: selector and :role."})

      :admin/revoke-role
      (if (and (some? selector) (keyword? role))
        (admin/revoke-role! selector role)
        {:ok? false
         :error :input/invalid
         :message "Missing required keys: selector and :role."})

      :admin/list-roles
      (if (some? selector)
        (admin/list-roles! selector)
        {:ok? false
         :error :input/invalid
         :message "Missing required selector: :selector or :id or :email."})

      :admin/list-known-roles
      (admin/list-known-roles!)

      :admin/reset-login-attempts
      (if (some? selector)
        (admin/reset-login-attempts! selector)
        {:ok? false
         :error :input/invalid
         :message "Missing required selector: :selector or :id or :email."})

      :admin/get-artifact-rollout
      (runtime-svc/get-artifact-rollout runtime artifact)

      :admin/set-artifact-rollout
      (runtime-svc/set-artifact-rollout!
       runtime
       {:artifact artifact
        :active active
        :canary canary
        :shadow shadow
        :clear? clear?})

      :admin/migrate-db
      (let [opts (if (map? (:opts params)) (:opts params) nil)]
        (if opts
          (admin/migrate! opts)
          (admin/migrate!)))

      :admin/rollback-db
      (let [opts (if (map? (:opts params)) (:opts params) nil)
            amount-or-id (or (:amount-or-id params)
                             (:amount params)
                             (:migration-id params))]
        (cond
          (and opts (some? amount-or-id))
          (admin/rollback! opts amount-or-id)

          (some? amount-or-id)
          (admin/rollback! nil amount-or-id)

          opts
          (admin/rollback! opts)

          :else
          (admin/rollback!)))

      {:ok? false
       :error :input/invalid
       :message "Unsupported admin action."
       :details {:action action
                 :supported admin-supported-actions}})))

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

(def ^:private default-session-principal-operations
  #{:http.v1/act})

(defn- auth-session-principal-config
  [runtime]
  (let [cfg (if (map? (get-in runtime [:auth :session-principal]))
              (get-in runtime [:auth :session-principal])
              {})
        operations (keyword-set (:operations cfg))]
    {:enabled?   (true? (:enabled? cfg))
     :operations (if (seq operations)
                   operations
                   default-session-principal-operations)
     :ttl-ms     (or (parse-non-negative-long (:ttl-ms cfg)) 1800000)
     :refresh-ms (or (parse-non-negative-long (:refresh-ms cfg)) 300000)}))

(defn- auth-session-principal-enabled?
  [runtime operation]
  (let [{:keys [enabled? operations]} (auth-session-principal-config runtime)
        op (keywordish operation)]
    (and enabled?
         (keyword? op)
         (contains? operations op))))

(defn- auth-session-service
  [runtime]
  (let [svc (when (map? runtime) (:session runtime))]
    (when (map? svc)
      svc)))

(defn- session-id-from-header
  [^HttpExchange exchange]
  (let [headers (some-> exchange (.getRequestHeaders))]
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
  (when-some [header (some-> exchange
                             (.getRequestHeaders)
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

(defn- forbidden-response
  [message]
  {:status 403
   :body {:ok? false
          :error :auth/forbidden
          :message (or (trim-s message)
                       "Access forbidden.")}})

(defn- auth-config-error-response
  []
  {:status 500
   :body {:ok? false
          :error :auth/not-configured
          :message "HTTP authentication enabled, but auth source is missing."}})

(defn- auth-session-config-error-response
  []
  {:status 500
   :body {:ok? false
          :error :auth/not-configured
          :message "Session principal auth enabled, but session service is missing."}})

(defn- session-principal-meta
  [session-state]
  (let [meta (when (map? session-state)
               (:session/meta session-state))]
    (if (map? meta) meta {})))

(defn- session-principal-user
  [session-state]
  (let [meta      (session-principal-meta session-state)
        principal (if (map? (:auth/principal meta))
                    (:auth/principal meta)
                    meta)
        user      (auth-user-public principal)]
    (when (and (map? user)
               (or (some? (:user/id user))
                   (some? (:user/email user))))
      user)))

(defn- session-principal-refreshed-at-ms
  [session-state]
  (let [meta (session-principal-meta session-state)]
    (or (parse-non-negative-long (:auth/principal-refreshed-at meta))
        (parse-non-negative-long (:auth/principal-at meta)))))

(defn- session-principal-fresh?
  [ttl-ms refreshed-at-ms now-ms]
  (or (<= (long (or ttl-ms 0)) 0)
      (and (some? refreshed-at-ms)
           (<= (- now-ms refreshed-at-ms) ttl-ms))))

(defn- session-principal-meta-update
  [user now-ms]
  (let [user' (auth-user-public user)
        ts    (long now-ms)]
    (cond-> {:auth/principal user'
             :auth/principal-at ts
             :auth/principal-refreshed-at ts}
      (some? (:user/id user')) (assoc :user/id (:user/id user'))
      (some? (:user/email user')) (assoc :user/email (:user/email user'))
      (some? (:user/account-type user')) (assoc :user/account-type (:user/account-type user'))
      (seq (:user/roles user')) (assoc :user/roles (:user/roles user')))))

(defn- refresh-session-principal
  [runtime sid session-state user]
  (let [cfg         (auth-session-principal-config runtime)
        refresh-ms  (long (or (:refresh-ms cfg) 0))
        refreshed-at (session-principal-refreshed-at-ms session-state)
        now         (System/currentTimeMillis)
        due?        (and (pos? refresh-ms)
                         (or (nil? refreshed-at)
                             (>= (- now refreshed-at) refresh-ms)))
        service     (auth-session-service runtime)
        existing-meta (session-principal-meta session-state)]
    (if (and due?
             sid
             (map? service)
             (fn? (:open! service)))
      (or (try
            (memory/open! service sid {:session/meta (merge existing-meta
                                                             (session-principal-meta-update user now))})
            (catch Throwable _ nil))
          session-state)
      session-state)))

(defn- authorize-authenticated-principal
  [runtime exchange operation {:keys [auth-op
                                      source
                                      user
                                      session
                                      user-id
                                      success-message
                                      denied-message]}]
  (let [allowed? (roles/allowed? (:roles runtime) operation user)]
    (if allowed?
      (do
        (report-auth! runtime exchange
                      {:operation auth-op
                       :success true
                       :user-id user-id
                       :message success-message})
        {:ok? true
         :auth (cond-> {:source source}
                 (map? user) (assoc :user user)
                 (map? session) (assoc :session session))})
      (do
        (report-auth! runtime exchange
                      {:operation auth-op
                       :success false
                       :level :warning
                       :user-id user-id
                       :message denied-message})
        {:ok? false
         :response (forbidden-response "Missing required role for this operation.")}))))

(defn- auth-request-strategy
  [runtime exchange operation]
  (let [basic-credentials (parse-basic-credentials exchange)
        session-principal? (auth-session-principal-enabled? runtime operation)]
    (cond
      (map? basic-credentials)
      {:strategy :basic
       :basic-credentials basic-credentials}

      session-principal?
      {:strategy :session-principal}

      :else
      {:strategy :missing-credentials})))

(defn- authenticate-request-via-basic
  [runtime exchange payload operation basic-credentials]
  (if-not (some? (auth-source runtime))
    (do
      (report-auth! runtime exchange
                    {:operation :auth/http-basic
                     :success false
                     :level :error
                     :message "HTTP auth enabled, but auth source is missing."})
      {:ok? false
       :response (auth-config-error-response)})
    (let [{:keys [login password]} basic-credentials
          result (auth-user/authenticate-password
                  (auth-source runtime)
                  login
                  password
                  (auth-account-type runtime)
                  (auth-options runtime exchange payload))]
      (if (:ok? result)
        (let [user      (when (map? result) (:user result))
              user'     (auth-user-public user)]
          (authorize-authenticated-principal
           runtime
           exchange
           operation
           {:auth-op :auth/http-basic
            :source :http/basic
            :user user'
            :session (when (map? (:session result))
                       (:session result))
            :user-id (get-in result [:user :user/id])
            :success-message "HTTP basic auth accepted."
            :denied-message (str "HTTP auth forbidden for operation "
                                 (or operation :unknown) ".")}))
        (do
          (report-auth! runtime exchange
                        {:operation :auth/http-basic
                         :success false
                         :level :warning
                         :message (str "HTTP basic auth rejected: " (or (:error result) :unknown))})
          {:ok? false
           :response (unauthorized-response runtime "Invalid credentials.")})))))

(defn- authenticate-request-via-session-principal
  [runtime exchange payload operation]
  (let [cfg      (auth-session-principal-config runtime)
        sid      (auth-session-id exchange payload)
        service  (auth-session-service runtime)]
    (cond
      (nil? sid)
      (do
        (report-auth! runtime exchange
                      {:operation :auth/http-session
                       :success false
                       :level :notice
                       :message "Missing session id for session principal auth."})
        {:ok? false
         :response (unauthorized-response runtime "Missing session id.")})

      (not (and (map? service) (fn? (:get! service))))
      (do
        (report-auth! runtime exchange
                      {:operation :auth/http-session
                       :success false
                       :level :error
                       :message "Session principal auth enabled, but session service is missing."})
        {:ok? false
         :response (auth-session-config-error-response)})

      :else
      (if-some [session-state (try
                                (memory/get! service sid)
                                (catch Throwable _ nil))]
        (let [user            (session-principal-user session-state)
              now-ms          (System/currentTimeMillis)
              refreshed-at-ms (session-principal-refreshed-at-ms session-state)
              fresh?          (session-principal-fresh? (:ttl-ms cfg)
                                                       refreshed-at-ms
                                                       now-ms)]
          (cond
            (nil? user)
            (do
              (report-auth! runtime exchange
                            {:operation :auth/http-session
                             :success false
                             :level :warning
                             :message "Session principal is missing."})
              {:ok? false
               :response (unauthorized-response runtime "Session has no principal.")})

            (not fresh?)
            (do
              (report-auth! runtime exchange
                            {:operation :auth/http-session
                             :success false
                             :level :notice
                             :user-id (:user/id user)
                             :message "Session principal expired."})
              {:ok? false
               :response (unauthorized-response runtime "Session principal expired. Re-authenticate with Basic Auth.")})

            :else
            (let [session-state' (refresh-session-principal runtime sid session-state user)]
              (authorize-authenticated-principal
               runtime
               exchange
               operation
               {:auth-op :auth/http-session
                :source :http/session-principal
                :user user
                :session (cond-> {:session/id sid}
                           (map? session-state')
                           (merge (select-keys session-state'
                                               [:session/version
                                                :session/state
                                                :session/frozen?
                                                :session/updated-at
                                                :session/last-access-at
                                                :session/frozen-at
                                                :session/thawed-at])))
                :user-id (:user/id user)
                :success-message "Session principal auth accepted."
                :denied-message (str "Session auth forbidden for operation "
                                     (or operation :unknown) ".")}))))
        (do
          (report-auth! runtime exchange
                        {:operation :auth/http-session
                         :success false
                         :level :notice
                         :message "Session not found for session principal auth."})
          {:ok? false
           :response (unauthorized-response runtime "Session not found.")})))))

(defn- authenticate-request
  ([runtime exchange]
   (authenticate-request runtime exchange nil nil))
  ([runtime exchange payload]
   (authenticate-request runtime exchange payload nil))
  ([runtime exchange payload operation]
   (if-not (auth-enabled? runtime)
     {:ok? true}
     (let [{:keys [strategy basic-credentials]}
           (auth-request-strategy runtime exchange operation)]
       (case strategy
         :basic
         (authenticate-request-via-basic runtime exchange payload operation basic-credentials)

         :session-principal
         (authenticate-request-via-session-principal runtime exchange payload operation)

         (do
           (report-auth! runtime exchange
                         {:operation :auth/http-basic
                          :success false
                          :level :notice
                          :message "Missing or invalid Authorization header."})
           {:ok? false
            :response (unauthorized-response runtime "Missing or invalid Authorization header.")}))))))

(defn- authorize-request
  ([runtime exchange]
   (authorize-request runtime exchange nil nil))
  ([runtime exchange payload]
   (authorize-request runtime exchange payload nil))
  ([runtime exchange payload operation]
   (let [authn (authenticate-request runtime exchange payload operation)]
     (when-not (:ok? authn)
       (:response authn)))))

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
        (or (model-adapter/invoke-worker! worker payload)
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
                auth-payload  (safe-decode-request-body body-str ctype)
                authn         (authenticate-request runtime exchange auth-payload :http.v1/act)]
            (if-not (:ok? authn)
              (let [{:keys [status body headers]} (:response authn)]
                (write-response! exchange status (encode-response body) headers))
              (try
                (let [payload (decode-request-body body-str ctype)
                      {:keys [status body]} (invoke-act runtime payload telemetry (:auth authn))]
                  (write-response! exchange status (encode-response body)))
                (catch Throwable t
                  (write-response! exchange
                                   400
                                   (encode-response
                                    (error-envelope nil
                                                    :input/invalid
                                                    (.getMessage t)))))))))))))

(defn- decode-uri-part
  [v]
  (try
    (some-> v str (URLDecoder/decode StandardCharsets/UTF_8) trim-s)
    (catch Throwable _
      nil)))

(defn- parse-query-params
  [^HttpExchange exchange]
  (let [raw (some-> exchange .getRequestURI .getRawQuery trim-s)]
    (if (str/blank? raw)
      {}
      (reduce (fn [acc part]
                (let [[k v] (str/split (str part) #"=" 2)
                      k' (decode-uri-part k)
                      v' (decode-uri-part v)]
                  (if (some? k')
                    (assoc acc k' v')
                    acc)))
              {}
              (str/split raw #"&")))))

(defn- parse-job-route
  [^HttpExchange exchange]
  (let [path (some-> exchange .getRequestURI .getPath trim-s)
        prefix "/v1/act/jobs/"]
    (when (and (string? path)
               (str/starts-with? path prefix))
      (let [tail0 (subs path (count prefix))
            tail  (if (str/ends-with? tail0 "/")
                    (subs tail0 0 (dec (count tail0)))
                    tail0)]
        (cond
          (str/blank? tail)
          nil

          (str/ends-with? tail "/cancel")
          (let [raw-id (subs tail 0 (- (count tail) (count "/cancel")))]
            (when-some [job-id (decode-uri-part raw-id)]
              {:action :cancel
               :job/id job-id}))

          :else
          (when-some [job-id (decode-uri-part tail)]
            {:action :status
             :job/id job-id}))))))

(defn- parse-replay-route
  [^HttpExchange exchange]
  (let [path (some-> exchange .getRequestURI .getPath trim-s)
        query-params (parse-query-params exchange)
        against-trace-id (or (get query-params "against")
                             (get query-params "compare"))
        prefix "/v1/act/replay/"]
    (when (and (string? path)
               (str/starts-with? path prefix))
      (let [tail0 (subs path (count prefix))
            tail  (if (str/ends-with? tail0 "/")
                    (subs tail0 0 (dec (count tail0)))
                    tail0)]
        (cond
          (str/blank? tail)
          nil

          (str/ends-with? tail "/rerun")
          (let [raw-id (subs tail 0 (- (count tail) (count "/rerun")))]
            (when-some [trace-id (decode-uri-part raw-id)]
              (cond-> {:action :rerun
                       :trace/id trace-id}
                (some? against-trace-id)
                (assoc :against/trace-id against-trace-id))))

          :else
          (when-some [trace-id (decode-uri-part tail)]
            (cond-> {:action :get
                     :trace/id trace-id}
              (some? against-trace-id)
              (assoc :against/trace-id against-trace-id))))))))

(defn- replay-error-response
  [trace-id error]
  (case error
    :replay/disabled
    {:status 404
     :body (error-envelope nil
                           :replay/disabled
                           "Replay storage is disabled for this environment.")}

    :replay/invalid-trace-id
    {:status 400
     :body (error-envelope nil
                           :input/invalid
                           "Replay trace id is missing or invalid.")}

    :replay/not-found
    {:status 404
     :body (error-envelope nil
                           :replay/not-found
                           "Replay package not found for provided trace id."
                           {:trace/id trace-id})}

    :replay/unavailable
    {:status 503
     :body (error-envelope nil
                           :runtime/unavailable
                           "Replay storage is unavailable.")}

    {:status 500
     :body (error-envelope nil
                           :runtime/internal
                           "Failed to read replay package.")}))

(defn- replay-entry-summary
  [trace-id entry]
  (let [policy-id (some-> entry :policy :snapshot-id trim-s)
        status (some-> entry :response :status)
        outcome (some-> entry :response :outcome keywordish)
        error-type (some-> entry :response :error/type keywordish)]
    (cond-> {:trace/id trace-id
             :recorded-at (:recorded-at entry)
             :execution-path (or (get-in entry [:diagnostics :execution-path]) {})
             :timing (or (:timing entry) {})}
      (some? status) (assoc :response/status status)
      (keyword? outcome) (assoc :response/outcome outcome)
      (keyword? error-type) (assoc :response/error-type error-type)
      (some? policy-id) (assoc :policy/snapshot-id policy-id))))

(defn- replay-comparison
  [left-trace-id left-entry right-trace-id right-entry]
  (let [left-policy-id (some-> left-entry :policy :snapshot-id trim-s)
        right-policy-id (some-> right-entry :policy :snapshot-id trim-s)
        left-policy-snapshot (if (map? (get-in left-entry [:policy :snapshot]))
                               (get-in left-entry [:policy :snapshot])
                               {})
        right-policy-snapshot (if (map? (get-in right-entry [:policy :snapshot]))
                                (get-in right-entry [:policy :snapshot])
                                {})
        policy-diff (structured-diff left-policy-snapshot right-policy-snapshot)
        left-path (or (get-in left-entry [:diagnostics :execution-path]) {})
        right-path (or (get-in right-entry [:diagnostics :execution-path]) {})
        left-outcome (some-> left-entry :response :outcome keywordish)
        right-outcome (some-> right-entry :response :outcome keywordish)
        left-error (some-> left-entry :response :error/type keywordish)
        right-error (some-> right-entry :response :error/type keywordish)
        left-elapsed (double (or (get-in left-entry [:timing :elapsed-ms]) 0.0))
        right-elapsed (double (or (get-in right-entry [:timing :elapsed-ms]) 0.0))
        left-telemetry-delta (if (map? (get-in left-entry [:diagnostics :telemetry :delta]))
                               (get-in left-entry [:diagnostics :telemetry :delta])
                               {})
        right-telemetry-delta (if (map? (get-in right-entry [:diagnostics :telemetry :delta]))
                                (get-in right-entry [:diagnostics :telemetry :delta])
                                {})
        telemetry-diff (numeric-map-delta left-telemetry-delta right-telemetry-delta)]
    (cond-> {:left (replay-entry-summary left-trace-id left-entry)
             :right (replay-entry-summary right-trace-id right-entry)
             :same-execution-path? (= left-path right-path)
             :policy/config {:same? (and (= left-policy-id right-policy-id)
                                         (= left-policy-snapshot right-policy-snapshot))}}
      (not= left-policy-id right-policy-id)
      (assoc :policy/snapshot-id {:from left-policy-id
                                  :to right-policy-id})
      (seq policy-diff)
      (assoc-in [:policy/config :diff] policy-diff)
      (not= left-path right-path)
      (assoc :execution-path {:from left-path
                              :to right-path})
      (or (not= left-outcome right-outcome)
          (not= left-error right-error))
      (assoc :response {:outcome {:from left-outcome
                                  :to right-outcome}
                        :error-type {:from left-error
                                     :to right-error}})
      (not= left-elapsed right-elapsed)
      (assoc :timing/elapsed-ms {:from left-elapsed
                                 :to right-elapsed
                                 :delta (- right-elapsed left-elapsed)})
      (seq telemetry-diff)
      (assoc :telemetry/delta {:from left-telemetry-delta
                               :to right-telemetry-delta
                               :diff telemetry-diff}))))

(defn- replay-rerun-trace-id
  [source-trace-id rerun-options]
  (or (some-> rerun-options :trace/id trim-s)
      (format "%s::rerun::%s"
              source-trace-id
              (subs (str (java.util.UUID/randomUUID)) 0 8))))

(defn- replay-rerun-response
  [runtime telemetry source-trace-id rerun-options auth]
  (let [source-id (trim-s source-trace-id)
        source-result (replay-get runtime source-id)]
    (if-not (:ok? source-result)
      (replay-error-response source-id (:error source-result))
      (let [source-entry (:replay source-result)
            payload0 (get-in source-entry [:request :payload])]
        (if-not (map? payload0)
          {:status 422
           :body (error-envelope nil
                                 :replay/invalid-payload
                                 "Replay package does not contain valid request payload."
                                 {:trace/id source-id})}
          (let [rerun-id (replay-rerun-trace-id source-id rerun-options)
                payload  (assoc payload0 :trace {:id rerun-id})
                invoke-response (invoke-act runtime payload telemetry auth)
                rerun-result (replay-get runtime rerun-id)]
            (if-not (:ok? rerun-result)
              {:status 502
               :body (error-envelope nil
                                     :replay/rerun-not-recorded
                                     "Replay rerun finished, but replay package was not recorded."
                                     {:trace/id source-id
                                      :rerun/trace-id rerun-id})}
              {:status 200
               :body {:ok? true
                      :source/trace-id source-id
                      :rerun/trace-id rerun-id
                      :rerun/response {:status (:status invoke-response)
                                       :body (:body invoke-response)}
                      :comparison (replay-comparison source-id
                                                    source-entry
                                                    rerun-id
                                                    (:replay rerun-result))}})))))))

(defn- act-replay-response
  ([runtime trace-id]
   (act-replay-response runtime trace-id nil))
  ([runtime trace-id against-trace-id]
   (let [trace-id' (trim-s trace-id)
         against'  (trim-s against-trace-id)
         result    (replay-get runtime trace-id')]
     (if-not (:ok? result)
       (replay-error-response trace-id' (:error result))
       (if-not against'
         {:status 200
          :body result}
         (let [against-result (replay-get runtime against')]
           (if-not (:ok? against-result)
             (let [{:keys [status body]} (replay-error-response against' (:error against-result))]
               {:status status
                :body (update body :error merge {:details {:against/trace-id against'}})})
             {:status 200
              :body {:ok? true
                     :trace/id trace-id'
                     :replay (:replay result)
                     :against/trace-id against'
                     :comparison (replay-comparison trace-id'
                                                   (:replay result)
                                                   against'
                                                   (:replay against-result))}})))))))

(defn- act-replay-handler
  [runtime telemetry]
  (reify HttpHandler
    (handle [_ exchange]
      (let [route (parse-replay-route exchange)
            method (some-> (.getRequestMethod exchange) str/upper-case)]
        (cond
          (not (map? route))
          (write-response! exchange
                           404
                           (encode-response
                            (error-envelope nil
                                            :not-found
                                            "Replay endpoint not found.")))

          (= :get (:action route))
          (if (not= "GET" method)
            (write-response! exchange
                             405
                             (encode-response
                              (error-envelope nil
                                              :method-not-allowed
                                              "Only GET is supported for replay endpoint."
                                              {:allowed ["GET"]})))
            (let [authn (authenticate-request runtime exchange nil :http.v1/act)]
              (if-not (:ok? authn)
                (let [{:keys [status body headers]} (:response authn)]
                  (write-response! exchange status (encode-response body) headers))
                (let [{:keys [status body]} (act-replay-response runtime
                                                                 (:trace/id route)
                                                                 (:against/trace-id route))]
                  (write-response! exchange status (encode-response body))))))

          (= :rerun (:action route))
          (if (not= "POST" method)
            (write-response! exchange
                             405
                             (encode-response
                              (error-envelope nil
                                              :method-not-allowed
                                              "Only POST is supported for replay rerun endpoint."
                                              {:allowed ["POST"]})))
            (let [ctype        (content-type exchange)
                  body-str     (read-body exchange)
                  auth-payload (safe-decode-request-body body-str ctype)
                  authn        (authenticate-request runtime exchange auth-payload :http.v1/act)]
              (if-not (:ok? authn)
                (let [{:keys [status body headers]} (:response authn)]
                  (write-response! exchange status (encode-response body) headers))
                (try
                  (let [rerun-options (decode-request-body body-str ctype)
                        rerun-options' (if (map? rerun-options) rerun-options {})
                        {:keys [status body]} (replay-rerun-response runtime
                                                                     telemetry
                                                                     (:trace/id route)
                                                                     rerun-options'
                                                                     (:auth authn))]
                    (write-response! exchange status (encode-response body)))
                  (catch Throwable t
                    (write-response! exchange
                                     400
                                     (encode-response
                                      (error-envelope nil
                                                      :input/invalid
                                                      (.getMessage t)))))))))

          :else
          (write-response! exchange
                           404
                           (encode-response
                            (error-envelope nil
                                            :not-found
                                            "Replay endpoint not found."))))))))

(defn- act-jobs-handler
  [runtime]
  (reify HttpHandler
    (handle [_ exchange]
      (let [route (parse-job-route exchange)
            method (some-> (.getRequestMethod exchange) str/upper-case)]
        (cond
          (not (map? route))
          (write-response! exchange
                           404
                           (encode-response
                            (error-envelope nil
                                            :not-found
                                            "Queue job endpoint not found.")))

          (= :status (:action route))
          (if (not= "GET" method)
            (write-response! exchange
                             405
                             (encode-response
                              (error-envelope nil
                                              :method-not-allowed
                                              "Only GET is supported for job status endpoint."
                                              {:allowed ["GET"]})))
            (let [authn (authenticate-request runtime exchange nil :http.v1/act)]
              (if-not (:ok? authn)
                (let [{:keys [status body headers]} (:response authn)]
                  (write-response! exchange status (encode-response body) headers))
                (let [{:keys [status body]} (queue-job-status-response runtime {} (:job/id route))]
                  (write-response! exchange status (encode-response body))))))

          (= :cancel (:action route))
          (if (not= "POST" method)
            (write-response! exchange
                             405
                             (encode-response
                              (error-envelope nil
                                              :method-not-allowed
                                              "Only POST is supported for job cancel endpoint."
                                              {:allowed ["POST"]})))
            (let [ctype        (content-type exchange)
                  body-str     (read-body exchange)
                  auth-payload (safe-decode-request-body body-str ctype)
                  authn        (authenticate-request runtime exchange auth-payload :http.v1/act)]
              (if-not (:ok? authn)
                (let [{:keys [status body headers]} (:response authn)]
                  (write-response! exchange status (encode-response body) headers))
                (try
                  (let [payload (decode-request-body body-str ctype)
                        reason  (or (some-> payload :cancel/reason keywordish)
                                    (some-> payload :reason keywordish))
                        {:keys [status body]} (queue-job-cancel-response runtime payload (:job/id route) reason)]
                    (write-response! exchange status (encode-response body)))
                  (catch Throwable t
                    (write-response! exchange
                                     400
                                     (encode-response
                                      (error-envelope nil
                                                      :input/invalid
                                                      (.getMessage t)))))))))

          :else
          (write-response! exchange
                           404
                           (encode-response
                            (error-envelope nil
                                            :not-found
                                            "Queue job endpoint not found."))))))))

(defn- telemetry-handler
  [runtime telemetry]
  (reify HttpHandler
    (handle [_ exchange]
      (let [method (some-> (.getRequestMethod exchange) str/upper-case)]
        (if (contains? #{"GET" "POST"} method)
          (let [query-params (parse-query-params exchange)
                tenant-filter (or (get query-params "tenant")
                                  (get query-params "tenant/id"))
                principal-filter (or (get query-params "principal")
                                     (get query-params "principal-ref"))]
            (write-response! exchange 200 (encode-response {:ok? true
                                                            :service :ferment.http
                                                            :telemetry (telemetry-snapshot runtime
                                                                                           telemetry
                                                                                           {:tenant tenant-filter
                                                                                            :principal principal-filter})})))
          (write-response! exchange
                           405
                           (encode-response {:ok? false
                                             :error :method-not-allowed
                                             :allowed ["GET" "POST"]})))))))

(defn- session-handler
  [runtime telemetry]
  (reify HttpHandler
    (handle [_ exchange]
      (let [method (some-> (.getRequestMethod exchange) str/upper-case)]
        (if (not= "POST" method)
          (write-response! exchange
                           405
                           (encode-response {:ok? false
                                             :error :method-not-allowed
                                             :allowed ["POST"]}))
          (let [started-at   (System/nanoTime)
                ctype        (content-type exchange)
                body-str     (read-body exchange)
                auth-payload (safe-decode-request-body body-str ctype)
                authn        (authenticate-request runtime exchange auth-payload :http.v1/session)]
            (if-not (:ok? authn)
              (let [{:keys [status body headers]} (:response authn)
                    response {:status status :body body}
                    elapsed-ms (nanos->millis started-at)]
                (report-session! runtime auth-payload response (:auth authn) elapsed-ms)
                (write-response! exchange status (encode-response body) headers))
              (try
                (let [payload (decode-request-body body-str ctype)
                      response (session-action-response runtime payload telemetry)
                      elapsed-ms (nanos->millis started-at)]
                  (report-session! runtime payload response (:auth authn) elapsed-ms)
                  (write-response! exchange
                                   (:status response)
                                   (encode-response (:body response))))
                (catch Throwable t
                  (let [response {:status 500
                                  :body {:ok? false
                                         :error :runtime/internal
                                         :message (.getMessage t)}}
                        elapsed-ms (nanos->millis started-at)]
                    (report-session! runtime auth-payload response (:auth authn) elapsed-ms)
                    (write-response! exchange
                                     500
                                     (encode-response (:body response)))))))))))))

(defn- admin-handler
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
          (let [ctype        (content-type exchange)
                body-str     (read-body exchange)
                auth-payload (safe-decode-request-body body-str ctype)
                action       (normalize-admin-action (:action auth-payload))]
            (if-not action
              (write-response! exchange
                               400
                               (encode-response {:ok? false
                                                 :error :input/invalid
                                                 :message "Unsupported or missing admin action."
                                                 :details {:action (:action auth-payload)
                                                           :supported admin-supported-actions}}))
              (if-some [{:keys [status body headers]}
                        (authorize-request runtime exchange auth-payload action)]
                (do
                  (when (= 403 status)
                    (report-auth! runtime exchange
                                  {:operation action
                                   :success false
                                   :level :warning
                                   :message "Admin operation rejected by role policy."}))
                  (write-response! exchange status (encode-response body) headers))
                (try
                  (let [result (invoke-admin-action runtime action auth-payload)
                        status (admin-result-status result)
                        body   (assoc (if (map? result)
                                        result
                                        {:ok? false
                                         :error :runtime/invalid-result})
                                      :action action)]
                    (write-response! exchange status (encode-response body)))
                  (catch Throwable t
                    (write-response! exchange
                                     500
                                     (encode-response {:ok? false
                                                       :error :runtime/internal
                                                       :message (.getMessage t)}))))))))))))

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

(defn- init-training-runtime
  [training]
  (let [training' (if (map? training) training {})
        collector-cfg (if (map? (:collector training'))
                        (:collector training')
                        (training-collector/normalize-config nil))]
    (if-not (:enabled? collector-cfg)
      (assoc training' :collector (assoc collector-cfg :instance nil))
      (try
        (let [collector-instance (training-collector/init-collector collector-cfg)
              collector-stats (when (some? collector-instance)
                                (training-collector/stats collector-instance))]
          (telemetry/record-lifecycle!
           :training/collector
           :start
           {:enabled? true
            :store/type (:store/type collector-cfg)
            :store/path (:store/path collector-cfg)
            :flush-policy (:flush-policy collector-cfg)
            :max-file-size-bytes (:max-file-size-bytes collector-cfg)
            :stats collector-stats})
          (assoc training' :collector (assoc collector-cfg :instance collector-instance)))
        (catch Throwable t
          (telemetry/record-lifecycle!
           :training/collector
           :error
           {:error :training/collector-init-failed
            :message (.getMessage t)
            :store/type (:store/type collector-cfg)
            :store/path (:store/path collector-cfg)})
          (assoc training'
                 :collector
                 (assoc collector-cfg
                        :enabled? false
                        :instance nil
                        :init/error (.getMessage t))))))))

(defn- stop-training-runtime!
  [training]
  (let [collector (get-in training [:collector :instance])]
    (when (satisfies? training-collector/TrainingCollector collector)
      (try
        (let [collector-stats (training-collector/stats collector)]
          (training-collector/close! collector)
          (telemetry/record-lifecycle!
           :training/collector
           :stop
           {:stats collector-stats
            :store/type (get-in training [:collector :store/type])
            :store/path (get-in training [:collector :store/path])}))
        (catch Throwable t
          (telemetry/record-lifecycle!
           :training/collector
           :error
           {:error :training/collector-stop-failed
            :message (.getMessage t)
            :store/type (get-in training [:collector :store/type])
            :store/path (get-in training [:collector :store/path])}))))))

(defn preconfigure-http
  "Pre-configuration hook for HTTP bridge."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (cond-> cfg
      (not (contains? cfg :models))
      (assoc :models (system/ref :ferment/models))

      (not (contains? cfg :runtime))
      (assoc :runtime (system/ref :ferment.runtime/default)))))

(defn init-http
  "Initializes HTTP bridge for model runtime workers."
  [_k config]
  (try
    (let [cfg      (preconfigure-http _k config)
          host     (or (trim-s (:host cfg)) "127.0.0.1")
          port     (parse-port (:port cfg))
          response-cache (normalize-act-response-cache cfg)
          replay (normalize-replay-config cfg)
          training (normalize-training-config cfg)
          training-runtime (init-training-runtime training)
          act-middleware (when (sequential? (:act/middleware cfg))
                           (vec (:act/middleware cfg)))
          tenancy-cfg (let [runtime-cfg (if (map? (:runtime cfg)) (:runtime cfg) {})]
                        (tenancy/normalize-config (or (:tenancy runtime-cfg)
                                                      (:tenancy cfg))))
          tenancy-state (atom (tenancy/default-state))
          runtime0 (let [r (if (map? (:runtime cfg)) (:runtime cfg) {})]
                     (cond-> (if (contains? r :models)
                               r
                               (assoc r :models (:models cfg)))
                       (contains? cfg :auth)
                       (assoc :auth (:auth cfg))
                       (map? response-cache)
                       (assoc :response-cache response-cache)
                       (map? replay)
                       (assoc :replay replay)
                       (map? training-runtime)
                       (assoc :training training-runtime)
                       true
                       (assoc :tenancy tenancy-cfg
                              :tenancy/state tenancy-state)
                       (seq act-middleware)
                       (assoc :act/middleware act-middleware)))
          runtime  (assoc runtime0
                          :act/pipeline (compile-act-middleware-chain runtime0 act-middleware))
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
                 "/v1/act/jobs/{job-id}" {:type :protocol-act-job-status}
                 "/v1/act/jobs/{job-id}/cancel" {:type :protocol-act-job-cancel}
                 "/v1/act/replay/{trace-id}" {:type :protocol-act-replay}
                 "/v1/act/replay/{trace-id}/rerun" {:type :protocol-act-replay-rerun}
                 "/v1/session" {:type :session-bridge}
                 "/v1/admin" {:type :admin}
                 "/health" {:type :health}
                 "/routes" {:type :routes}
                 "/diag/telemetry" {:type :diag-telemetry})
          telemetry-state (atom (default-telemetry))
          executor (create-http-executor cfg)
          server   (HttpServer/create (InetSocketAddress. ^String host (int port)) 0)]
      (doseq [[endpoint route] routes]
        (.createContext server endpoint (invoke-handler route)))
      (.createContext server "/v1/act" (act-handler runtime telemetry-state))
      (.createContext server "/v1/act/jobs" (act-jobs-handler runtime))
      (.createContext server "/v1/act/replay" (act-replay-handler runtime telemetry-state))
      (.createContext server "/v1/session" (session-handler runtime telemetry-state))
      (.createContext server "/v1/admin" (admin-handler runtime))
      (.createContext server "/health" (health-handler (count public-routes)))
      (.createContext server "/routes" (routes-handler public-routes))
      (.createContext server "/diag/telemetry" (telemetry-handler runtime telemetry-state))
      (.setExecutor server executor)
      (.start server)
      (telemetry/record-lifecycle! :http :start {:key _k
                                                 :host host
                                                 :port port
                                                 :routes (count public-routes)
                                                 :act/middleware (count (if (seq act-middleware)
                                                                          act-middleware
                                                 (default-act-middleware-modules)))
                                                 :cache/enabled? (true? (:enabled? response-cache))
                                                 :replay/enabled? (true? (:enabled? replay))
                                                 :training/enabled? (true? (:enabled? training-runtime))
                                                 :training/collector-enabled? (true? (get-in training-runtime [:collector :enabled?]))})
      {:host host
       :port port
       :server server
       :executor executor
       :telemetry telemetry-state
       :response-cache response-cache
       :replay replay
       :training training-runtime
       :routes public-routes})
    (catch Throwable t
      (telemetry/record-lifecycle! :http :error {:key _k
                                                 :error (.getMessage t)})
      (throw t))))

(defn stop-http
  "Stops HTTP bridge."
  [_k state]
  (stop-training-runtime! (:training state))
  (when-some [^HttpServer server (:server state)]
    (.stop server 0))
  (when-some [^ExecutorService executor (:executor state)]
    (.shutdown executor)
    (try
      (when-not (.awaitTermination executor 2000 TimeUnit/MILLISECONDS)
        (.shutdownNow executor))
      (catch InterruptedException _
        (.shutdownNow executor)
        (.interrupt (Thread/currentThread)))))
  (telemetry/record-lifecycle! :http :stop {:key _k
                                            :host (:host state)
                                            :port (:port state)})
  nil)

(derive ::service :ferment.system/value)
(derive :ferment.http/default ::service)

(system/add-expand ::service [k config] {k (preconfigure-http k config)})
(system/add-init   ::service [k config]    (init-http k config))
(system/add-halt!  ::service [k state]     (stop-http k state))
