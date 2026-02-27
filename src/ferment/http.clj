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
            [ferment.memory :as memory]
            [ferment.middleware.remote-ip :as remote-ip]
            [ferment.oplog :as oplog]
            [ferment.roles :as roles]
            [ferment.router :as router]
            [ferment.system :as system]
            [ferment.telemetry :as telemetry]
            [ferment.workflow :as workflow]
            [io.randomseed.utils.ip :as ip])

  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
    (java.io OutputStream)
    (java.net InetSocketAddress)
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

(defn- coerce-act-request
  [payload]
  (letfn [(->keyword-coll
            [v]
            (cond
              (nil? v)        nil
              (set? v)        (into #{} (keep keywordish) v)
              (sequential? v) (into [] (keep keywordish) v)
              :else           v))
          (->bool
            [v]
            (cond
              (boolean? v) v
              (number? v)  (not (zero? (long v)))
              (string? v)  (contains? #{"1" "true" "yes" "on"}
                                      (-> v str/trim str/lower-case))
              (nil? v)     nil
              :else        (boolean v)))
          (->int
            [v default]
            (cond
              (integer? v) (int v)
              (number? v)  (int (Math/round (double v)))
              (string? v)  (try
                             (Integer/parseInt (str/trim v))
                             (catch Throwable _ default))
              :else        default))
          (normalize-routing
            [routing]
            (let [routing'    (if (map? routing) routing {})
                  intent      (keywordish (:intent routing'))
                  cap-id      (keywordish (:cap/id routing'))
                  profile     (keywordish (:profile routing'))
                  meta?       (->bool (:meta? routing'))
                  strict?     (->bool (:strict? routing'))
                  force?      (->bool (:force? routing'))
                  on-error    (keywordish (:on-error routing'))
                  debug-plan? (->bool (or (:debug/plan? routing')
                                          (:debug-plan? routing')
                                          (get-in routing' [:debug :plan?])))
                  debug-transcript? (->bool (or (:debug/transcript? routing')
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
                (some? debug-plan?)                             (assoc :debug/plan? debug-plan?)
                (some? debug-transcript?)                       (assoc :debug/transcript? debug-transcript?))))
          (normalize-top
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
                  stream?       (or (->bool (:stream? request))
                                    (->bool (get-in request [:response :stream?])))]
              (cond-> request
                (keyword? intent)          (assoc-in [:task :intent] intent)
                (keyword? cap-id)          (assoc-in [:task :cap/id] cap-id)
                (map? requires)            (assoc-in [:task :requires] requires)
                (keyword? role)            (assoc :role role)
                (keyword? response-type)   (assoc :response/type response-type)
                (some? stream?)            (assoc :stream? stream?)
                (contains? request :proto) (update :proto ->int 1)
                (contains? request :done)
                (-> (update-in [:done :must] ->keyword-coll)
                    (update-in [:done :should] ->keyword-coll))
                (contains? request :effects)
                (update-in [:effects :allowed] ->keyword-coll)
                (contains? request :budget)
                (update-in [:budget :max-roundtrips] ->int nil)
                (contains? request :constraints)
                (update-in [:constraints :language] #(or (keywordish %) %))
                (contains? request :routing)
                (update :routing normalize-routing))))]
    (cond
      (and (map? payload) (map? (:task payload)))
      (normalize-top payload)

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
        (normalize-top req))

      :else payload)))

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
                            :route/strict          0}
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
              :quality/must-failed 0}})

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
           wf-error-telemetry (workflow-telemetry-from-error body)]
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
                      (update-in [:act :cache] telemetry/merge-counters cache-telemetry)))))))))

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
  [telemetry]
  (let [state0 (if (instance? clojure.lang.IAtom telemetry)
                 @telemetry
                 (default-telemetry))
        state  (telemetry/merge-counters (default-telemetry) state0)
        count  (counter-value (get-in state [:act :latency-ms :count]))
        sum    (double (or (get-in state [:act :latency-ms :sum]) 0.0))
        avg    (if (pos? count) (/ sum count) 0.0)
        state' (assoc-in state [:act :latency-ms :avg] avg)]
    (assoc state'
           :kpi (telemetry-kpi state')
           :lifecycle (telemetry/lifecycle-snapshot))))

(defn- resolve-cap-id
  [resolver request]
  (or (:cap/id request)
      (get-in request [:task :cap/id])
      (workflow/resolve-capability
       resolver
       {:intent  (get-in request [:task :intent])
        :requires (get-in request [:task :requires])
        :effects (:effects request)})))

(defn- effective-resolver
  [runtime]
  (let [resolver   (or (:resolver runtime) {})
        routing    (router/resolver-routing runtime resolver)
        router-cfg (if (map? (:router runtime)) (:router runtime) {})]
    (cond-> resolver
      (map? routing)                   (assoc :routing routing)
      (contains? router-cfg :profiles) (assoc :profiles (:profiles router-cfg))
      (contains? router-cfg :policy)   (assoc :policy (:policy router-cfg)))))

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

(defn- act-cache-runtime
  [runtime]
  (let [cache (when (map? runtime) (:response-cache runtime))]
    (when (map? cache) cache)))

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
             :input           (:input request)
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
             :session-version (:session/version request)}
      (some? (:model request))                        (assoc :model (:model request))
      (some? (get-in request [:task :model]))         (assoc :model (get-in request [:task :model]))
      (keyword? (:response/type request))             (assoc :response/type (:response/type request))
      (contains? request :stream?)                    (assoc :stream? (boolean (:stream? request)))
      (some? (positive-int (:max-roundtrips budget))) (assoc :max-attempts (positive-int (:max-roundtrips budget)))
      (some? (positive-int (:max-tokens budget)))     (assoc :max-tokens (positive-int (:max-tokens budget)))
      (number? (:top-p budget))                       (assoc :top-p (double (:top-p budget)))
      (some? (:temperature budget))                   (assoc :temperature (:temperature budget))
      (map? auth-user)                                (assoc :auth/user auth-user)
      (map? (:roles runtime))                         (assoc :roles (:roles runtime))
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

(defn- request-with-session-defaults
  [runtime request]
  (let [service (when (map? runtime) (:session runtime))
        bindings (session-request-default-bindings runtime)
        binding-keys (->> (keys bindings)
                          (keep keywordish)
                          vec)
        sid     (or (some-> request :session/id trim-s)
                    (some-> request :session-id trim-s))
        intent  (some-> request :task :intent keywordish)
        opts    (cond-> {:operation :act/defaults}
                  (keyword? intent) (assoc :intent intent))]
    (if (and (map? request)
             (map? service)
             (fn? (:get-vars! service))
             sid
             (seq binding-keys))
      (let [vars (try
                   (memory/get-vars! service sid binding-keys opts)
                   (catch clojure.lang.ArityException _
                     (memory/get-vars! service sid binding-keys))
                   (catch Throwable _
                     nil))]
        (if (map? vars)
          (apply-session-var-defaults request vars bindings)
          request))
      request)))

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
                         (some? (:user/id principal))
                         (assoc :principal-id (:user/id principal))
                         (some? (:user/email principal))
                         (assoc :principal-email (:user/email principal))
                         (some? (:user/account-type principal))
                         (assoc :principal-account-type (:user/account-type principal))
                         (seq (:user/roles principal))
                         (assoc :principal-roles (vec (:user/roles principal))))))))))

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
  "Role: VOICE. Rewrite for tone/style only. Preserve all factual claims, technical details, constraints, and examples from input. Do not summarize away meaning. Keep output compact. If source text appears truncated, complete it naturally in at most two sentences, without adding new facts.")

(defn- route-solver->voice-plan
  [user-prompt]
  {:nodes [{:op :call
            :intent :text/respond
            :cap/id :llm/voice
            :constraints {:max-chars 420}
            :budget {:max-tokens 220}
            :input {:prompt user-prompt}
            :as :voice-primary
            :dispatch {:allow-failure? true
                       :checks/hard [:schema-valid :no-truncated-ending]
                       :checks/soft [:no-hallucinated-apis :sufficient-detail :no-list-expansion]
                       :switch-on #{:schema/invalid :format/drift :eval/low-score :eval/must-failed}
                       :retry {:same-cap-max 0
                               :fallback-max 0}}}
           {:op :call
            :intent :problem/solve
            :cap/id :llm/solver
            :constraints {:max-chars 700}
            :budget {:max-tokens 240}
            :input {:prompt user-prompt}
            :dispatch {:checks/hard [:schema-valid]
                       :checks/soft [:no-hallucinated-apis :no-truncated-ending]}
            :as :solver
            :when {:failed? :voice-primary}}
           {:op :call
            :intent :text/respond
            :cap/id :llm/voice
            :system route-voice-preserve-system
            :input {:prompt {:slot/id [:solver :out :text]}}
            :constraints {:max-chars 420}
            :budget {:max-tokens 160}
            :done {:score-min 0.0}
            :dispatch {:checks/hard [:schema-valid :no-truncated-ending]
                       :checks/soft [:no-hallucinated-apis :no-list-expansion]
                       :switch-on #{:schema/invalid :format/drift :eval/low-score :eval/must-failed}
                       :retry {:same-cap-max 1
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
         (= [:solver :out :text] (get-in node2 [:input :prompt :slot/id]))
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
        route-plan* (cond
                      (map? (:plan parsed))
                      (:plan parsed)

                      (map? (get-in parsed [:result :plan]))
                      (get-in parsed [:result :plan])

                      :else nil)
        route-plan (when (route-solver->voice-plan? route-plan*)
                     route-plan*)
        tool-call  (extract-tool-call text')
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
  (let [cfg (effective-routing-config runtime request)]
    (if (contains? cfg :meta?)
      (boolean (:meta? cfg))
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
  (let [route-role (router/resolve-role runtime resolver cap-id intent)]
    {:role route-role
     :intent intent
     :cap-id cap-id
     :input {:request request
             :resolver {:routing (:routing resolver)}}
     :context (merge {:route/for-intent (get-in request [:task :intent])}
                     (if (map? (:context request)) (:context request) {}))
     :constraints (:constraints request)
     :budget (:budget request)
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
          (try
            (let [route-response (core/call-capability
                                  runtime
                                  resolver
                                  (route-decider-opts runtime resolver request cap-id route-intent))
                  route-out (contracts/result-out-of route-response)]
              (if (routing-decision? route-out)
                {:request (merge-routing-decision runtime request route-out)
                 :mode :continue
                 :enabled? true
                 :strict? strict?
                 :attempted? true
                 :reason :continue
                 :route-response route-response}
                {:request request
                 :mode :final
                 :enabled? true
                 :strict? strict?
                 :attempted? true
                 :reason :final
                 :response route-response}))
            (catch clojure.lang.ExceptionInfo e
              (if strict?
                {:mode :error
                 :status 502
                 :request request
                 :enabled? true
                 :strict? true
                 :attempted? true
                 :reason :fail-closed
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
                 :reason :fail-open}))
            (catch Throwable t
              (if strict?
                {:mode :error
                 :status 502
                 :request request
                 :enabled? true
                 :strict? true
                 :attempted? true
                 :reason :fail-closed
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
                 :reason :fail-open}))))))))

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

(defn invoke-act
  "Runs canonical `/v1/act` request through contract validation and core capability flow.

  Returns:
  - `{:status <http-status> :body <canonical-response-envelope>}`"
  ([runtime payload]
   (invoke-act runtime payload nil nil))
  ([runtime payload telemetry]
   (invoke-act runtime payload telemetry nil))
  ([runtime payload telemetry auth]
   (let [started-at (now-nanos)
         auth-user (some-> auth :user auth-user-public)
         auth-source-k (some-> auth :source keywordish)
         auth-session (when (map? (:session auth))
                        (:session auth))
         auth-session-id (or (some-> auth-session :session/id trim-s)
                             (some-> auth-session :id trim-s))
         request0  (coerce-act-request payload)
         request1  (cond-> request0
                     (map? auth-user) (assoc :auth/user auth-user)
                     (keyword? auth-source-k) (assoc :auth/source auth-source-k)
                     (and (map? request0)
                          auth-session-id
                          (nil? (:session/id request0)))
                     (assoc :session/id auth-session-id))
         request   (request-with-session-defaults runtime request1)
         protocol (or (:protocol runtime) {})
         resolver (effective-resolver runtime)
         req-check (contracts/validate-request protocol request)
         meta-step (when (:ok? req-check)
                     (maybe-apply-meta-routing runtime resolver request))
         route-mode (or (:mode meta-step) :none)
         request* (or (:request meta-step) request)
         routed? (not= request request*)
         post-route-check (cond
                            (not (:ok? req-check)) req-check
                            (#{:error :final} route-mode) {:ok? true}
                            routed? (contracts/validate-request protocol request*)
                            :else req-check)
         cap-id* (when (and (map? request*)
                            (:ok? req-check)
                            (:ok? post-route-check)
                            (not= :error route-mode)
                            (not= :final route-mode))
                   (resolve-cap-id resolver request*))
         cache-key (when (and (cacheable-act-request? request*)
                              (keyword? cap-id*))
                     (act-cache-key request* cap-id*))
         cache-lookup (act-cache-get! runtime cache-key)
         cache-hit? (true? (:hit? cache-lookup))
         cache-telemetry0 (if (map? (:telemetry cache-lookup))
                            (:telemetry cache-lookup)
                            {})
         route-telemetry (routing-telemetry-counters meta-step)
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
           (if-not (keyword? cap-id*)
               {:status 422
                :body   (error-envelope request*
                                        :unsupported/intent
                                        "No capability can handle the requested intent."
                                        {:intent (get-in request* [:task :intent])})}
             (if (and cache-hit? (map? (:response cache-lookup)))
               (:response cache-lookup)
               (try
                 {:status 200
                  :body   (core/call-capability
                           runtime
                           resolver
                           (act-request->invoke-opts runtime resolver request* cap-id*))}
                 (catch clojure.lang.ExceptionInfo e
                   (let [data (or (ex-data e) {})
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
                 (catch Throwable t
                   {:status 500
                    :body   (error-envelope request*
                                            :runtime/internal
                                            (.getMessage t))})))))
         cache-store-telemetry (if (or cache-hit?
                                      (not (cacheable-act-request? request*)))
                                {}
                                (act-cache-put! runtime cache-key response))
         cache-telemetry (ferment.telemetry/merge-counters cache-telemetry0 cache-store-telemetry)
         elapsed-ms (nanos->millis started-at)
         sid        (or (some-> (:session/id request*) trim-s)
                        (some-> (:session-id request*) trim-s))
         session-state
         (when sid
           (let [service (when (map? runtime) (:session runtime))]
             (when (map? service)
               (try
                 (memory/get! service sid)
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
                       response)
         response''  (attach-response-participants response')]
     (record-act-telemetry! telemetry response'' elapsed-ms route-telemetry cache-telemetry)
     (report-act! runtime request* response'' auth elapsed-ms)
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
    :admin/reset-login-attempts})

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
                         (some-> (:user/account-type params) keywordish))]
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
              user'     (auth-user-public user)
              allowed?  (roles/allowed? (:roles runtime) operation user')]
          (if allowed?
            (do
              (report-auth! runtime exchange
                            {:operation :auth/http-basic
                             :success true
                             :user-id (get-in result [:user :user/id])
                             :message "HTTP basic auth accepted."})
              {:ok? true
               :auth (cond-> {:source :http/basic}
                       (map? user') (assoc :user user')
                       (map? (:session result)) (assoc :session (:session result)))})
            (do
              (report-auth! runtime exchange
                            {:operation :auth/http-basic
                             :success false
                             :level :warning
                             :user-id (get-in result [:user :user/id])
                             :message (str "HTTP auth forbidden for operation "
                                           (or operation :unknown) ".")})
              {:ok? false
               :response (forbidden-response "Missing required role for this operation.")})))
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
            (let [allowed?       (roles/allowed? (:roles runtime) operation user)
                  session-state' (refresh-session-principal runtime sid session-state user)]
              (if allowed?
                (do
                  (report-auth! runtime exchange
                                {:operation :auth/http-session
                                 :success true
                                 :user-id (:user/id user)
                                 :message "Session principal auth accepted."})
                  {:ok? true
                   :auth {:source :http/session-principal
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
                                                          :session/thawed-at])))}})
                (do
                  (report-auth! runtime exchange
                                {:operation :auth/http-session
                                 :success false
                                 :level :warning
                                 :user-id (:user/id user)
                                 :message (str "Session auth forbidden for operation "
                                               (or operation :unknown) ".")})
                  {:ok? false
                   :response (forbidden-response "Missing required role for this operation.")})))))
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
     (let [basic-credentials (parse-basic-credentials exchange)
           session-principal? (auth-session-principal-enabled? runtime operation)]
       (cond
         (map? basic-credentials)
         (authenticate-request-via-basic runtime exchange payload operation basic-credentials)

         session-principal?
         (authenticate-request-via-session-principal runtime exchange payload operation)

         :else
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
          runtime  (let [r (if (map? (:runtime cfg)) (:runtime cfg) {})]
                     (cond-> (if (contains? r :models)
                               r
                               (assoc r :models (:models cfg)))
                       (contains? cfg :auth)
                       (assoc :auth (:auth cfg))
                       (map? response-cache)
                       (assoc :response-cache response-cache)))
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
      (.createContext server "/v1/session" (session-handler runtime telemetry-state))
      (.createContext server "/v1/admin" (admin-handler runtime))
      (.createContext server "/health" (health-handler (count public-routes)))
      (.createContext server "/routes" (routes-handler public-routes))
      (.createContext server "/diag/telemetry" (telemetry-handler telemetry-state))
      (.setExecutor server executor)
      (.start server)
      (telemetry/record-lifecycle! :http :start {:key _k
                                                 :host host
                                                 :port port
                                                 :routes (count public-routes)
                                                 :cache/enabled? (true? (:enabled? response-cache))})
      {:host host
       :port port
       :server server
       :executor executor
       :telemetry telemetry-state
       :response-cache response-cache
       :routes public-routes})
    (catch Throwable t
      (telemetry/record-lifecycle! :http :error {:key _k
                                                 :error (.getMessage t)})
      (throw t))))

(defn stop-http
  "Stops HTTP bridge."
  [_k state]
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
