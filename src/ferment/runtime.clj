(ns

    ^{:doc    "Runtime configuration branch for Ferment core orchestration."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.runtime

  (:require [clojure.string :as str]
            [ferment.execution-graph :as execution-graph]
            [ferment.queue :as queue]
            [ferment.contracts :as contracts]
            [ferment.core :as core]
            [ferment.router :as router]
            [ferment.system :as system]
            [ferment.telemetry :as telemetry]
            [ferment.workflow :as workflow])

  (:import (java.time Instant)))

(defn- resolver-cap-ids
  [resolver]
  (let [by-id (if (map? (:caps/by-id resolver))
                (keys (:caps/by-id resolver))
                nil)
        listed (if (sequential? (:caps resolver))
                 (keep :cap/id (:caps resolver))
                 nil)]
    (into #{} (concat by-id listed))))

(defn- fail-runtime-router!
  [path cap-id known-caps]
  (throw (ex-info "Router references unknown capability id."
                  {:error :router/unknown-capability
                   :path path
                   :cap/id cap-id
                   :known-cap-ids (vec (sort-by str known-caps))})))

(defn- validate-router-capability-ref!
  [known-caps path cap-id]
  (when-not (contains? known-caps cap-id)
    (fail-runtime-router! path cap-id known-caps)))

(defn- validate-router-capabilities!
  [cfg]
  (let [router-cfg (if (map? (:router cfg)) (:router cfg) {})
        routing    (if (map? (:routing router-cfg)) (:routing router-cfg) {})
        resolver   (if (map? (:resolver cfg)) (:resolver cfg) {})
        known-caps (resolver-cap-ids resolver)]
    (when (seq known-caps)
      (doseq [[intent cap-id] (or (:intent->cap routing) {})]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :intent->cap intent]
                                         cap-id))
      (doseq [[cap-id _] (or (:cap->model-key routing) {})]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :cap->model-key cap-id]
                                         cap-id))
      (doseq [[cap-id _] (or (:cap->role routing) {})]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :cap->role cap-id]
                                         cap-id))
      (doseq [[idx cap-id] (map-indexed vector (or (:fallback routing) []))]
        (validate-router-capability-ref! known-caps
                                         [:router :routing :fallback idx]
                                         cap-id))))
  cfg)

(defn- attach-router-to-resolver
  [cfg]
  (if-not (map? cfg)
    {}
    (let [router   (if (map? (:router cfg)) (:router cfg) {})
          resolver (if (map? (:resolver cfg)) (:resolver cfg) {})
          resolver' (cond-> (dissoc resolver :routing :profiles :policy)
                      (map? (:routing router))
                      (assoc :routing (:routing router))
                      (contains? router :profiles)
                      (assoc :profiles (:profiles router))
                      (contains? router :policy)
                      (assoc :policy (:policy router)))]
      (if (map? resolver')
        (assoc cfg :resolver resolver')
        cfg))))

(defn- fallback-queue-config
  []
  ;; Keep preconfigure defaults derived from queue source-of-truth.
  ;; Omit normalized runtime helpers rebuilt by queue/init-service.
  (-> (queue/default-config)
      (dissoc :clock :classes/set)))

(defn- fallback-execution-graph-config
  []
  (execution-graph/normalize-config nil))

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- pos-int
  [v]
  (when (and (integer? v) (pos? v))
    (int v)))

(defn- nonneg-long
  [v]
  (let [n (cond
            (integer? v) v
            (number? v) (long (Math/floor (double v)))
            (string? v) (try
                          (Long/parseLong (clojure.string/trim v))
                          (catch Throwable _ nil))
            :else nil)]
    (when (and (integer? n) (<= 0 n))
      (long n))))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v trim-s keyword)
    :else nil))

(defn- boolish
  [v]
  (cond
    (true? v) true
    (false? v) false
    (string? v) (let [s (str/lower-case (or (trim-s v) ""))]
                  (cond
                    (#{"true" "1" "yes" "y" "on"} s) true
                    (#{"false" "0" "no" "n" "off"} s) false
                    :else nil))
    :else nil))

(defn- instant->ms
  [v]
  (try
    (.toEpochMilli (Instant/parse (str v)))
    (catch Throwable _ nil)))

(declare artifact-config)

(defn- runtime-protocol
  [runtime]
  (artifact-config runtime :protocol))

(defn- runtime-resolver
  [runtime]
  (if (map? (:resolver runtime))
    (:resolver runtime)
    {}))

(def ^:private supported-artifact-keys
  #{:protocol :router})

(defn artifact-overrides-state
  "Returns runtime artifact overrides atom, if available."
  [runtime]
  (let [state (when (map? runtime) (:artifact/overrides runtime))]
    (when (instance? clojure.lang.IAtom state)
      state)))

(defn artifact-overrides
  "Returns current artifact rollout overrides map from runtime."
  [runtime]
  (if-some [state (artifact-overrides-state runtime)]
    (let [v @state]
      (if (map? v) v {}))
    {}))

(defn artifact-config
  "Returns runtime artifact config (`:protocol` or `:router`) with applied rollout overrides."
  [runtime artifact]
  (let [artifact' (keywordish artifact)
        cfg (if (and (map? runtime)
                     (keyword? artifact')
                     (map? (get runtime artifact')))
              (get runtime artifact')
              {})
        override (when (keyword? artifact')
                   (get (artifact-overrides runtime) artifact'))
        active' (some-> override :active keywordish)
        canary' (if (map? (:canary override))
                  (:canary override)
                  nil)]
    (cond-> cfg
      (keyword? active') (assoc-in [:rollout :active] active')
      (map? canary') (assoc-in [:rollout :canary] canary'))))

(defn get-artifact-rollout
  "Returns current rollout override state for artifact(s).

  Input:
  - `artifact`: optional `:protocol` or `:router`.

  Output:
  - `{:ok? true :overrides <map>}` for all artifacts,
  - `{:ok? true :artifact <kw> :override <map|nil>}` for single artifact."
  ([runtime]
   (get-artifact-rollout runtime nil))
  ([runtime artifact]
   (let [artifact' (some-> artifact keywordish)]
     (cond
       (and (some? artifact')
            (not (contains? supported-artifact-keys artifact')))
       {:ok? false
        :error :input/invalid
        :message "Unsupported artifact key. Expected :protocol or :router."
        :details {:artifact artifact'}}

       (and (some? artifact')
            (contains? supported-artifact-keys artifact'))
       {:ok? true
        :artifact artifact'
        :override (get (artifact-overrides runtime) artifact')}

       :else
       {:ok? true
        :overrides (artifact-overrides runtime)}))))

(defn set-artifact-rollout!
  "Sets or clears runtime rollout override for a selected artifact.

  Params map keys:
  - `:artifact` (required): `:protocol` or `:router`,
  - `:active` (optional): active version keyword/string,
  - `:canary` (optional): `{ :enabled? bool, :version kw, :percent int }`,
  - `:clear?` (optional): when true, removes override for artifact.

  Returns operation result map (`:ok?` + details)."
  [runtime {:keys [artifact active canary clear?]}]
  (let [artifact' (some-> artifact keywordish)
        state (artifact-overrides-state runtime)
        clear?' (true? (boolish clear?))]
    (cond
      (not (contains? supported-artifact-keys artifact'))
      {:ok? false
       :error :input/invalid
       :message "Unsupported artifact key. Expected :protocol or :router."
       :details {:artifact artifact}}

      (nil? state)
      {:ok? false
       :error :runtime/not-configured
       :message "Runtime artifact override state is not available."}

      :else
      (let [base-cfg (artifact-config runtime artifact')
            known-versions (if (map? (:versions base-cfg))
                             (set (keys (:versions base-cfg)))
                             #{})
            active' (some-> active keywordish)
            canary-map (if (map? canary) canary {})
            canary-enabled? (boolish (:enabled? canary-map))
            canary-version (some-> (:version canary-map) keywordish)
            canary-percent (:percent canary-map)
            canary' (cond-> {}
                      (some? canary-enabled?) (assoc :enabled? canary-enabled?)
                      (keyword? canary-version) (assoc :version canary-version)
                      (integer? canary-percent) (assoc :percent canary-percent))
            invalid-active? (and (keyword? active')
                                 (seq known-versions)
                                 (not (contains? known-versions active')))
            invalid-canary-version? (and (keyword? canary-version)
                                         (seq known-versions)
                                         (not (contains? known-versions canary-version)))
            invalid-canary-percent? (and (contains? canary-map :percent)
                                         (not (and (integer? canary-percent)
                                                   (<= 0 canary-percent 100))))]
        (cond
          invalid-active?
          {:ok? false
           :error :input/invalid
           :message "Active artifact version is not defined in config versions."
           :details {:artifact artifact'
                     :active active'
                     :known-versions (vec (sort known-versions))}}

          invalid-canary-version?
          {:ok? false
           :error :input/invalid
           :message "Canary artifact version is not defined in config versions."
           :details {:artifact artifact'
                     :canary/version canary-version
                     :known-versions (vec (sort known-versions))}}

          invalid-canary-percent?
          {:ok? false
           :error :input/invalid
           :message "Canary percent must be integer in [0,100]."
           :details {:artifact artifact'
                     :canary/percent canary-percent}}

          :else
          (let [next-override (cond-> {}
                                (keyword? active') (assoc :active active')
                                (seq canary') (assoc :canary canary'))
                overrides (swap! state
                                 (fn [current]
                                   (let [current' (if (map? current) current {})]
                                     (if clear?'
                                       (dissoc current' artifact')
                                       (assoc current' artifact' next-override)))))
                effective (get overrides artifact')]
            {:ok? true
             :artifact artifact'
             :override effective
             :overrides overrides
             :cleared? (if clear?' true false)}))))))

(defn- request-explicit-cap-id
  [request]
  (or (some-> (:cap/id request) keywordish)
      (some-> (get-in request [:task :cap/id]) keywordish)))

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
                                                       (keyword? routed-cap) (conj routed-cap))}))]
    (workflow/resolve-capability-decision resolver node)))

(defn- request->invoke-opts
  [runtime resolver request cap-id]
  (let [intent  (get-in request [:task :intent])
        budget  (if (map? (:budget request)) (:budget request) {})
        auth-user (when (map? (:auth/user request))
                    (:auth/user request))]
    (cond-> {:role            (or (:role request)
                                  (router/resolve-role runtime resolver cap-id intent))
             :intent          intent
             :cap-id          cap-id
             :input           (:input request)
             :context         (:context request)
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
      (some? (:model request))                (assoc :model (:model request))
      (some? (get-in request [:task :model])) (assoc :model (get-in request [:task :model]))
      (keyword? (:response/type request))     (assoc :response/type (:response/type request))
      (contains? request :stream?)            (assoc :stream? (boolean (:stream? request)))
      (some? (pos-int (:max-roundtrips budget))) (assoc :max-attempts (pos-int (:max-roundtrips budget)))
      (some? (pos-int (:max-tokens budget)))  (assoc :max-tokens (pos-int (:max-tokens budget)))
      (number? (:top-p budget))               (assoc :top-p (double (:top-p budget)))
      (some? (:temperature budget))           (assoc :temperature (:temperature budget))
      (map? auth-user)                        (assoc :auth/user auth-user)
      (map? (:roles runtime))                 (assoc :roles (:roles runtime)))))

(defn- runtime-execution-graph
  [runtime]
  (when (map? runtime)
    (:execution-graph/service runtime)))

(defn- append-execution-event!
  [runtime event]
  (let [service (runtime-execution-graph runtime)]
    (when (execution-graph/service? service)
      (execution-graph/append-event! service event))))

(defn- node-callback-context
  [request]
  {:job/id (trim-s (:queue/job-id request))
   :run/id (or (trim-s (:queue/job-id request))
               (some-> request :trace :id trim-s))
   :trace/id (some-> request :trace :id trim-s)
   :session/id (trim-s (:session/id request))})

(defn- workflow-node-state-callback
  [runtime request]
  (let [ctx (node-callback-context request)
        job-id (:job/id ctx)
        run-id (:run/id ctx)
        trace-id (:trace/id ctx)
        session-id (:session/id ctx)]
    (when (or run-id job-id)
      (fn [event]
        (when (map? event)
          (append-execution-event!
           runtime
           (cond-> {:event/type (or (keywordish (:event/type event))
                                    :node/running)
                    :details (select-keys event [:node/index
                                                 :node/op
                                                 :node/as
                                                 :node/intent
                                                 :error
                                                 :failure/type])}
             (string? job-id) (assoc :job/id job-id)
             (string? run-id) (assoc :run/id run-id)
             (string? trace-id) (assoc :trace/id trace-id)
             (string? session-id) (assoc :session/id session-id)
             (map? (:checkpoint event)) (assoc :checkpoint (:checkpoint event)))))))))

(defn- non-retryable-failure?
  [error-type]
  (contains? #{:invalid-request
               :input/invalid
               :unsupported/intent
               :auth/forbidden-effect
               :effects/invalid-input
               :effects/not-declared
               :effects/unsupported-tool
               :effects/scope-denied
               :schema/invalid
               :eval/must-failed}
             error-type))

(defn- invoke-queued-request
  [runtime request]
  (let [protocol (runtime-protocol runtime)
        resolver (runtime-resolver runtime)
        req-check (contracts/validate-request protocol request)]
    (cond
      (not (map? request))
      {:ok? false
       :retryable? false
       :error {:type :input/invalid
               :message "Queued request payload must be a map."}}

      (not (:ok? req-check))
      {:ok? false
       :retryable? false
       :error {:type :invalid-request
               :message "Queued request does not satisfy protocol contract."
               :details (select-keys req-check [:reason :intent])}}

      :else
      (let [cap-decision (resolve-capability-decision resolver request)
            cap-id       (some-> cap-decision :cap/id keywordish)
            rejected-candidates (if (sequential? (:rejected-candidates cap-decision))
                                  (->> (:rejected-candidates cap-decision)
                                       (keep (fn [entry]
                                               (when (map? entry)
                                                 (cond-> {}
                                                   (keyword? (:cap/id entry)) (assoc :cap/id (:cap/id entry))
                                                   (keyword? (:reason entry)) (assoc :reason (:reason entry))
                                                   (keyword? (:intent entry)) (assoc :intent (:intent entry))
                                                   (keyword? (:result/type entry)) (assoc :result/type (:result/type entry))))))
                                       vec)
                                  [])
            unsupported-details (cond-> {:intent (some-> request :task :intent keywordish)}
                                  (keyword? (:requested-cap/id cap-decision))
                                  (assoc :requested-cap/id (:requested-cap/id cap-decision))
                                  (keyword? (:routed-cap/id cap-decision))
                                  (assoc :routed-cap/id (:routed-cap/id cap-decision))
                                  (sequential? (:candidates cap-decision))
                                  (assoc :candidates (vec (keep keywordish (:candidates cap-decision))))
                                  (seq rejected-candidates)
                                  (assoc :rejected-candidates rejected-candidates))]
        (if-not (keyword? cap-id)
          {:ok? false
           :retryable? false
           :error {:type :unsupported/intent
                   :message "No capability can handle the queued intent."
                   :details unsupported-details}}
          (try
            (let [node-state-cb (workflow-node-state-callback runtime request)
                  invoke-opts (cond-> (request->invoke-opts runtime resolver request cap-id)
                                (map? (:workflow/resume-checkpoint request))
                                (assoc :workflow/resume-checkpoint (:workflow/resume-checkpoint request))
                                (fn? node-state-cb)
                                (assoc :workflow/on-node-state node-state-cb))]
             {:ok? true
             :result (core/call-capability runtime resolver invoke-opts)})
            (catch clojure.lang.ExceptionInfo e
              (let [data (or (ex-data e) {})
                    error-type (or (:error data) (:failure/type data) :runtime/invoke-failed)]
                {:ok? false
                 :retryable? (not (non-retryable-failure? error-type))
                 :error {:type error-type
                         :message (.getMessage e)
                         :details (dissoc data :stacktrace)}}))
            (catch Throwable t
              {:ok? false
               :retryable? true
               :error {:type :runtime/internal
                       :message (.getMessage t)
                       :details {:class (str (class t))}}})))))))

(defn- queue-invoke-fn
  [runtime]
  (let [f (:queue/invoke-fn runtime)]
    (if (ifn? f)
      f
      (fn [request]
        (invoke-queued-request runtime request)))))

(defn- queue-request-map
  [job]
  (if (map? (:request job))
    (:request job)
    {}))

(defn- queue-request-timeout-ms
  [queue-cfg request]
  (let [queue-opts (if (map? (:queue request)) (:queue request) {})
        from-request (or (nonneg-long (:timeout-ms request))
                         (nonneg-long (:timeout-ms queue-opts))
                         (nonneg-long (:job/timeout-ms queue-opts)))]
    (or from-request
        (nonneg-long (:default-timeout-ms queue-cfg))
        15000)))

(defn- queue-retry-policy
  [queue-cfg request]
  (let [queue-opts (if (map? (:queue request)) (:queue request) {})
        retry-opts (if (map? (:retry queue-opts)) (:retry queue-opts) {})
        cfg-retry  (if (map? (:retry queue-cfg)) (:retry queue-cfg) {})]
    {:max-attempts (or (pos-int (:max-attempts retry-opts))
                       (pos-int (:max-attempts cfg-retry))
                       1)
     :base-backoff-ms (or (pos-int (:base-backoff-ms retry-opts))
                          (pos-int (:base-backoff-ms cfg-retry))
                          100)
     :jitter-ms (or (nonneg-long (:jitter-ms retry-opts))
                    (nonneg-long (:jitter-ms cfg-retry))
                    0)}))

(defn- queue-job-deadline-ms
  [job]
  (some-> job :deadline-at instant->ms))

(defn- queue-timeout-ms-for-attempt
  [queue-cfg job attempt-start-ms]
  (let [request (queue-request-map job)
        timeout-ms (queue-request-timeout-ms queue-cfg request)
        deadline-ms (queue-job-deadline-ms job)]
    (cond
      (nil? deadline-ms) timeout-ms
      (>= attempt-start-ms deadline-ms) 0
      :else (long (min timeout-ms (- deadline-ms attempt-start-ms))))))

(defn- invoke-with-timeout
  [invoke-fn request timeout-ms]
  (let [f (future
            (try
              (let [out (invoke-fn request)]
                (if (map? out)
                  out
                  {:ok? false
                   :retryable? false
                   :error {:type :runtime/invalid-result
                           :message "Queue invoke function returned non-map result."}}))
              (catch Throwable t
                {:ok? false
                 :retryable? true
                 :error {:type :runtime/internal
                         :message (.getMessage t)
                         :details {:class (str (class t))}}})))
        out (deref f timeout-ms ::timeout)]
    (if (identical? ::timeout out)
      (do
        (future-cancel f)
        {:ok? false
         :retryable? true
         :error {:type :queue/timeout
                 :message "Queued job timed out while running."
                 :details {:timeout-ms timeout-ms}}})
      out)))

(defn- retry-sleep-ms
  [base-backoff-ms jitter-ms attempt-no]
  (let [base (* (long base-backoff-ms)
                (long (Math/pow 2.0 (double (max 0 (dec attempt-no))))))
        jitter (if (pos? (long jitter-ms))
                 (rand-int (inc (int jitter-ms)))
                 0)]
    (+ base jitter)))

(defn- sleep-ms!
  [ms]
  (when (and (number? ms) (pos? (long ms)))
    (Thread/sleep (long ms))))

(defn- queue-job-status
  [service job-id]
  (let [res (queue/get-job service job-id)]
    (when (:ok? res)
      (get-in res [:job :job/status]))))

(defn- terminal-transition-ok?
  [res]
  (or (:ok? res)
      (= :queue/invalid-transition (:error res))))

(defn- execute-queued-job!
  [invoke-fn queue-cfg service job]
  (let [job-id (:job/id job)
        request (assoc (queue-request-map job)
                       :queue/job-id job-id)
        {:keys [max-attempts base-backoff-ms jitter-ms]} (queue-retry-policy queue-cfg request)]
    (loop [attempt 1]
      (let [status (queue-job-status service job-id)]
        (if (= :canceled status)
          {:ok? false
           :terminal? true
           :error {:type :queue/canceled
                   :message "Queued job was canceled during execution."}}
          (let [started-at (System/currentTimeMillis)
                timeout-ms (queue-timeout-ms-for-attempt queue-cfg job started-at)]
            (if (<= timeout-ms 0)
              {:ok? false
               :terminal? true
               :error {:type :queue/deadline-exceeded
                       :message "Queued job exceeded deadline before execution."}}
              (let [outcome (invoke-with-timeout invoke-fn request timeout-ms)]
                (if (:ok? outcome)
                  outcome
                  (if (and (:retryable? outcome)
                           (< attempt max-attempts))
                    (do
                      (sleep-ms! (retry-sleep-ms base-backoff-ms jitter-ms attempt))
                      (recur (inc attempt)))
                    outcome))))))))))

(defn- handle-queued-job!
  [runtime invoke-fn queue-cfg service job]
  (let [job-id (:job/id job)
        _ (append-execution-event! runtime
                                   {:event/type :job/running
                                    :job/id job-id
                                    :run/id job-id
                                    :trace/id (some-> job :request :trace :id trim-s)
                                    :session/id (some-> job :request :session/id trim-s)
                                    :attempt (:attempt job)
                                    :queue/class (:queue/class job)
                                    :deadline-at (:deadline-at job)})
        outcome (execute-queued-job! invoke-fn queue-cfg service job)]
    (if (:ok? outcome)
      (let [ok? (terminal-transition-ok? (queue/complete! service job-id (:result outcome)))]
        (append-execution-event! runtime
                                 {:event/type :job/completed
                                  :job/id job-id
                                  :run/id job-id
                                  :trace/id (some-> job :request :trace :id trim-s)
                                  :session/id (some-> job :request :session/id trim-s)
                                  :attempt (:attempt job)})
        ok?)
      (let [ok? (terminal-transition-ok? (queue/fail! service job-id (:error outcome)))]
        (append-execution-event! runtime
                                 {:event/type :job/failed
                                  :job/id job-id
                                  :run/id job-id
                                  :trace/id (some-> job :request :trace :id trim-s)
                                  :session/id (some-> job :request :session/id trim-s)
                                  :attempt (:attempt job)
                                  :details {:error (:error outcome)}})
        ok?))))

(defn- queue-worker-loop!
  [runtime queue-cfg service running? invoke-fn]
  (let [poll-interval-ms (or (pos-int (:poll-interval-ms queue-cfg)) 25)]
    (while @running?
      (let [started (queue/start-next! service)]
        (if (:ok? started)
          (try
            (handle-queued-job! runtime invoke-fn queue-cfg service (:job started))
            (catch Throwable t
              (let [job-id (get-in started [:job :job/id])]
                (when (string? job-id)
                  (queue/fail! service job-id {:type :runtime/internal
                                               :message (.getMessage t)
                                               :details {:class (str (class t))}})
                  (append-execution-event! runtime
                                           {:event/type :job/failed
                                            :job/id job-id
                                            :run/id job-id
                                            :trace/id (some-> started :job :request :trace :id trim-s)
                                            :session/id (some-> started :job :request :session/id trim-s)
                                            :details {:error {:type :runtime/internal
                                                              :message (.getMessage t)
                                                              :class (str (class t))}}})))))
          (sleep-ms! poll-interval-ms))))))

(defn- start-queue-workers!
  [runtime]
  (let [queue-cfg (if (map? (:queue runtime)) (:queue runtime) {})
        queue-service (:queue/service runtime)
        enabled? (true? (:enabled? queue-cfg))
        workers-n (or (pos-int (:workers queue-cfg)) 0)]
    (if (or (not enabled?)
            (not (queue/service? queue-service))
            (<= workers-n 0))
      nil
      (let [running? (atom true)
            invoke-fn (queue-invoke-fn runtime)
            threads  (vec
                      (for [idx (range workers-n)]
                        (doto
                          (Thread.
                           (reify Runnable
                             (run [_]
                               (queue-worker-loop! runtime queue-cfg queue-service running? invoke-fn)))
                           (str "ferment-queue-worker-" idx))
                          (.setDaemon true)
                          (.start))))]
        {:running? running?
         :threads threads}))))

(defn- stop-queue-workers!
  [runtime]
  (let [running? (:queue/workers-running? runtime)
        threads  (if (vector? (:queue/workers runtime))
                   (:queue/workers runtime)
                   [])]
    (when (instance? clojure.lang.IAtom running?)
      (reset! running? false))
    (doseq [^Thread t threads]
      (when (instance? Thread t)
        (try
          (.join t 2000)
          (catch Throwable _ nil))))))

(defn- resume-queued-jobs!
  [runtime]
  (let [service (:queue/service runtime)
        graph   (:execution-graph/service runtime)
        queue-cfg (if (map? (:queue runtime)) (:queue runtime) {})
        enabled? (true? (:enabled? queue-cfg))]
    (when (and enabled?
               (queue/service? service)
               (execution-graph/service? graph))
      (let [jobs (execution-graph/inflight-jobs graph)]
        (doseq [job jobs]
          (let [request0 (if (map? (:request job)) (:request job) {})
                request' (cond-> request0
                           (map? (:checkpoint job))
                           (assoc :workflow/resume-checkpoint (:checkpoint job)))
                restore-job (-> job
                                (assoc :request request')
                                (assoc :job/status :queued))
                restored (queue/restore! service restore-job)]
            (when (:ok? restored)
              (append-execution-event! runtime
                                       {:event/type :job/queued
                                        :job/id (:job/id restore-job)
                                        :run/id (or (:run/id restore-job)
                                                    (:job/id restore-job))
                                        :trace/id (:trace/id restore-job)
                                        :session/id (:session/id restore-job)
                                        :queue/class (:queue/class restore-job)
                                        :deadline-at (:deadline-at restore-job)
                                        :attempt (:attempt restore-job)}))))
        (when (seq jobs)
          (telemetry/record-lifecycle! :runtime
                                       :resume
                                       {:queue/jobs-restored (count jobs)}))))))

(defn preconfigure-runtime
  "Pre-configuration hook for runtime config branch."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (-> cfg
        (cond-> (not (contains? cfg :router))
          (assoc :router (system/ref :ferment.router/default)))
        (cond-> (not (contains? cfg :roles))
          (assoc :roles (system/ref :ferment.roles/default)))
        (cond-> (not (contains? cfg :oplog))
          (assoc :oplog (system/ref :ferment.logging/oplog)))
        (cond-> (not (contains? cfg :queue))
          (assoc :queue (fallback-queue-config)))
        (cond-> (not (contains? cfg :execution-graph))
          (assoc :execution-graph (fallback-execution-graph-config)))
        (update :ferment.model.session/enabled? #(if (nil? %) true (boolean %)))
        (update :ferment.model.session/idle-ttl-ms #(or % 900000))
        (update :ferment.model.session/max-per-model #(or % 4)))))

(defn init-runtime
  "Initialization hook for runtime config branch.

  Runtime branch is configuration-oriented and passed through unchanged."
  [_k config]
  (try
    (let [cfg (-> (preconfigure-runtime _k config)
                  validate-router-capabilities!
                  attach-router-to-resolver)
          gateway-health (atom {})
          resolver' (if (map? (:resolver cfg))
                      (assoc (:resolver cfg) :gateway/model-health gateway-health)
                      (:resolver cfg))
          queue-service (queue/init-service (:queue cfg))
          execution-graph-service (execution-graph/init-service (:execution-graph cfg))
          runtime0 (assoc cfg
                          :resolver resolver'
                          :queue (queue/config queue-service)
                          :queue/service queue-service
                          :execution-graph (execution-graph/config execution-graph-service)
                          :execution-graph/service execution-graph-service
                          :artifact/overrides (atom {})
                          :gateway/model-health gateway-health
                          :ferment.model.session/workers (atom {})
                          :ferment.model.session/last-id-by-model (atom {})
                          :ferment.model.session/lock (Object.))
          _ (resume-queued-jobs! runtime0)
          workers  (start-queue-workers! runtime0)
          runtime' (cond-> runtime0
                     (map? workers)
                     (assoc :queue/workers-running? (:running? workers)
                            :queue/workers (:threads workers)))]
      (telemetry/record-lifecycle! :runtime :start {:key _k})
      runtime')
    (catch Throwable t
      (telemetry/record-lifecycle! :runtime :error {:key _k
                                                    :error (.getMessage t)})
      (throw t))))

(defn stop-runtime
  "Stop hook for runtime config branch."
  [_k state]
  (stop-queue-workers! state)
  (execution-graph/stop-service (:execution-graph/service state))
  (telemetry/record-lifecycle! :runtime :stop {:key _k})
  nil)

(derive ::default :ferment.system/value)

(system/add-expand ::default [k config] {k (preconfigure-runtime k config)})
(system/add-init   ::default [k config]    (init-runtime k config))
(system/add-halt!  ::default [k state]     (stop-runtime k state))
