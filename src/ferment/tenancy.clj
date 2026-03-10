(ns

    ^{:doc    "Multi-tenant governance helpers: limits, policy overrides, and accounting."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.tenancy

  (:require [clojure.string :as str])

  (:import (java.time Instant ZoneOffset LocalDate)))

(def ^:private default-tenant-id
  :tenant/default)

(def ^:private minute-ms
  60000)

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v trim-s keyword)
    :else nil))

(defn- nonneg-long
  [v]
  (cond
    (nil? v) nil
    (integer? v) (when (<= 0 (long v)) (long v))
    (number? v) (let [n (long (Math/floor (double v)))]
                  (when (<= 0 n) n))
    (string? v) (try
                  (let [n (Long/parseLong (trim-s v))]
                    (when (<= 0 n) n))
                  (catch Throwable _ nil))
    :else nil))

(defn- positive-long
  [v]
  (let [n (nonneg-long v)]
    (when (and (some? n) (pos? n))
      n)))

(defn- normalize-limits
  [src]
  (let [m (if (map? src) src {})]
    {:requests-per-minute (positive-long (:requests-per-minute m))
     :max-concurrent-requests (positive-long (:max-concurrent-requests m))
     :max-tokens-per-request (positive-long (:max-tokens-per-request m))
     :max-timeout-ms (positive-long (:max-timeout-ms m))
     :daily-max-billed-tokens (positive-long (:daily-max-billed-tokens m))}))

(defn- normalize-routing-defaults
  [src]
  (let [m (if (map? src) src {})]
    (cond-> {}
      (keyword? (keywordish (:profile m))) (assoc :profile (keywordish (:profile m)))
      (keyword? (keywordish (:policy/profile m))) (assoc :policy/profile (keywordish (:policy/profile m)))
      (keyword? (keywordish (:policy-profile m))) (assoc :policy/profile (keywordish (:policy-profile m)))
      (contains? m :meta?) (assoc :meta? (boolean (:meta? m)))
      (contains? m :strict?) (assoc :strict? (boolean (:strict? m)))
      (contains? m :force?) (assoc :force? (boolean (:force? m)))
      (keyword? (keywordish (:on-error m))) (assoc :on-error (keywordish (:on-error m))))))

(defn- normalize-principal-ref
  [v]
  (cond
    (nil? v) nil
    (integer? v) (str "id:" v)
    (string? v) (let [s (trim-s v)
                      s-l (some-> s str/lower-case)]
                  (cond
                    (nil? s) nil
                    (str/starts-with? s-l "id:") s-l
                    (str/starts-with? s-l "email:") s-l
                    (str/includes? s-l "@") (str "email:" s-l)
                    :else s-l))
    (keyword? v) (normalize-principal-ref (name v))
    :else nil))

(defn- normalize-principal-overrides
  [src]
  (if (map? src)
    (reduce-kv (fn [acc k v]
                 (if-some [k' (normalize-principal-ref k)]
                   (assoc acc k' {:limits (normalize-limits (if (map? v) (:limits v) nil))})
                   acc))
               {}
               src)
    {}))

(defn- normalize-tenant-policy
  [src]
  (let [m (if (map? src) src {})]
    {:limits (normalize-limits (:limits m))
     :routing/defaults (normalize-routing-defaults (:routing/defaults m))
     :principal/limits (normalize-limits (:principal/limits m))
     :principal/overrides (normalize-principal-overrides (:principal/overrides m))}))

(defn- merge-limits
  [base override]
  (merge (if (map? base) base {})
         (if (map? override) override {})))

(defn- merge-tenant-policy
  [base override]
  {:limits (merge-limits (:limits base) (:limits override))
   :routing/defaults (merge (if (map? (:routing/defaults base)) (:routing/defaults base) {})
                            (if (map? (:routing/defaults override)) (:routing/defaults override) {}))
   :principal/limits (merge-limits (:principal/limits base) (:principal/limits override))
   :principal/overrides (merge (if (map? (:principal/overrides base)) (:principal/overrides base) {})
                               (if (map? (:principal/overrides override)) (:principal/overrides override) {}))})

(defn default-state
  []
  {:minute/by-tenant {}
   :minute/by-principal {}
   :inflight/by-tenant {}
   :inflight/by-principal {}
   :daily/by-tenant {}
   :daily/by-principal {}
   :totals/by-tenant {}
   :totals/by-principal {}})

(defn normalize-config
  [cfg]
  (let [src (if (map? cfg) cfg {})
        default-policy (normalize-tenant-policy (:default src))
        tenants (if (map? (:tenants src))
                  (reduce-kv (fn [acc k v]
                               (if-some [tenant-id (keywordish k)]
                                 (assoc acc tenant-id (merge-tenant-policy default-policy
                                                                           (normalize-tenant-policy v)))
                                 acc))
                             {}
                             (:tenants src))
                  {})
        principal->tenant (if (map? (:principal->tenant src))
                            (reduce-kv (fn [acc k v]
                                         (if-some [principal-ref (normalize-principal-ref k)]
                                           (if-some [tenant-id (keywordish v)]
                                             (assoc acc principal-ref tenant-id)
                                             acc)
                                           acc))
                                       {}
                                       (:principal->tenant src))
                            {})]
    {:enabled? (boolean (:enabled? src))
     :default-tenant (or (keywordish (:default-tenant src))
                         default-tenant-id)
     :default (merge-tenant-policy default-policy {})
     :tenants tenants
     :principal->tenant principal->tenant}))

(defn- request-principal-ref
  [request]
  (let [auth-user (when (map? request) (:auth/user request))
        id-ref (normalize-principal-ref (:user/id auth-user))
        email-ref (normalize-principal-ref (:user/email auth-user))]
    (or id-ref email-ref)))

(defn- request-tenant-id
  [cfg request]
  (let [auth-user (when (map? request) (:auth/user request))
        principal-ref (request-principal-ref request)]
    (or (keywordish (:tenant/id request))
        (keywordish (:tenant-id request))
        (keywordish (:user/tenant-id auth-user))
        (when (string? principal-ref)
          (get (:principal->tenant cfg) principal-ref))
        (:default-tenant cfg)
        default-tenant-id)))

(defn- tenant-policy
  [cfg tenant-id]
  (let [base (:default cfg)
        override (get (:tenants cfg) tenant-id)]
    (merge-tenant-policy base override)))

(defn- request-max-tokens
  [request]
  (let [budget (if (map? (:budget request)) (:budget request) {})
        n (or (positive-long (:max-tokens budget))
              (positive-long (:max_tokens budget)))]
    (or n 0)))

(defn- clamp-positive
  [value limit]
  (let [v (positive-long value)
        l (positive-long limit)]
    (cond
      (and (some? v) (some? l)) (min v l)
      (some? v) v
      (some? l) l
      :else nil)))

(defn resolve-context
  [cfg request]
  (let [cfg' (normalize-config cfg)
        tenant-id (request-tenant-id cfg' request)
        principal-ref (request-principal-ref request)
        tenant-policy' (tenant-policy cfg' tenant-id)
        principal-override (when (string? principal-ref)
                             (get-in tenant-policy' [:principal/overrides principal-ref]))
        principal-limits (merge-limits (:principal/limits tenant-policy')
                                       (:limits principal-override))
        request-max-tokens' (request-max-tokens request)]
    {:enabled? (true? (:enabled? cfg'))
     :tenant/id tenant-id
     :principal/ref principal-ref
     :routing/defaults (if (map? (:routing/defaults tenant-policy'))
                         (:routing/defaults tenant-policy')
                         {})
     :tenant/limits (if (map? (:limits tenant-policy'))
                      (:limits tenant-policy')
                      {})
     :principal/limits (if (map? principal-limits)
                         principal-limits
                         {})
     :request/max-tokens request-max-tokens'}))

(defn apply-request-defaults
  [request tenancy-ctx]
  (let [ctx (if (map? tenancy-ctx) tenancy-ctx {})
        request0 (if (map? request) request {})
        enabled? (true? (:enabled? ctx))
        routing-defaults (if (map? (:routing/defaults ctx))
                           (:routing/defaults ctx)
                           {})
        routing' (merge routing-defaults
                        (if (map? (:routing request0)) (:routing request0) {}))
        tenant-limit (get-in ctx [:tenant/limits :max-tokens-per-request])
        principal-limit (get-in ctx [:principal/limits :max-tokens-per-request])
        max-token-limit (clamp-positive tenant-limit principal-limit)
        timeout-limit (clamp-positive (get-in ctx [:tenant/limits :max-timeout-ms])
                                      (get-in ctx [:principal/limits :max-timeout-ms]))
        req-budget (if (map? (:budget request0)) (:budget request0) {})
        requested-max (clamp-positive (or (:max-tokens req-budget)
                                          (:max_tokens req-budget))
                                      nil)
        bounded-max (clamp-positive requested-max max-token-limit)]
    (if-not enabled?
      request0
      (cond-> (assoc request0 :routing routing')
        (keyword? (:tenant/id ctx))
        (assoc :tenant/id (:tenant/id ctx))

        (string? (:principal/ref ctx))
        (assoc :principal/ref (:principal/ref ctx))

        (some? bounded-max)
        (assoc :budget (assoc req-budget :max-tokens bounded-max))

        (some? timeout-limit)
        (assoc :timeout-ms (long timeout-limit))))))

(defn- minute-slot
  [now-ms]
  (long (quot (long now-ms) minute-ms)))

(defn- day-slot
  [now-ms]
  (str (LocalDate/ofInstant (Instant/ofEpochMilli (long now-ms))
                            ZoneOffset/UTC)))

(defn- window-count
  [entry slot]
  (if (= slot (:slot entry))
    (long (or (:count entry) 0))
    0))

(defn- daily-used
  [entry day]
  (if (= day (:day entry))
    (long (or (:billed-tokens entry) 0))
    0))

(defn- bump-total
  [totals key reason]
  (let [base (if (map? totals) totals {})]
    (if (= key :rejected)
      (cond-> (update base :rejected/total (fnil inc 0))
        (keyword? reason) (update-in [:rejected reason] (fnil inc 0)))
      (update base key (fnil inc 0)))))

(defn- reserve-window!
  [state key-path slot]
  (let [entry (get-in state key-path)
        count' (inc (window-count entry slot))]
    (assoc-in state key-path {:slot slot :count count'})))

(defn- inc-inflight!
  [state key-path]
  (update-in state key-path (fnil inc 0)))

(defn- dec-inflight!
  [state key-path]
  (update-in state key-path (fn [n]
                              (max 0 (dec (long (or n 0)))))))

(defn- reservation-key-paths
  [tenant-id principal-ref]
  {:tenant-minute [:minute/by-tenant tenant-id]
   :principal-minute (when (string? principal-ref)
                       [:minute/by-principal principal-ref])
   :tenant-inflight [:inflight/by-tenant tenant-id]
   :principal-inflight (when (string? principal-ref)
                         [:inflight/by-principal principal-ref])
   :tenant-daily [:daily/by-tenant tenant-id]
   :principal-daily (when (string? principal-ref)
                      [:daily/by-principal principal-ref])
   :tenant-total [:totals/by-tenant tenant-id]
   :principal-total (when (string? principal-ref)
                      [:totals/by-principal principal-ref])})

(defn reserve!
  [state-atom tenancy-ctx now-ms]
  (let [ctx (if (map? tenancy-ctx) tenancy-ctx {})
        enabled? (true? (:enabled? ctx))]
    (if (or (not enabled?) (not (instance? clojure.lang.IAtom state-atom)))
      {:ok? true
       :reservation nil}
      (let [tenant-id (or (keywordish (:tenant/id ctx)) default-tenant-id)
            principal-ref (when (string? (:principal/ref ctx))
                            (:principal/ref ctx))
            limits-tenant (if (map? (:tenant/limits ctx)) (:tenant/limits ctx) {})
            limits-principal (if (map? (:principal/limits ctx)) (:principal/limits ctx) {})
            est-billed (long (or (positive-long (:request/max-tokens ctx)) 0))
            slot (minute-slot now-ms)
            day (day-slot now-ms)
            paths (reservation-key-paths tenant-id principal-ref)
            result* (volatile! nil)]
        (swap! state-atom
               (fn [state0]
                 (let [state (if (map? state0) state0 (default-state))
                       tenant-minute-count (window-count (get-in state (:tenant-minute paths)) slot)
                       principal-minute-count (if (:principal-minute paths)
                                                (window-count (get-in state (:principal-minute paths)) slot)
                                                0)
                       tenant-inflight (long (or (get-in state (:tenant-inflight paths)) 0))
                       principal-inflight (if (:principal-inflight paths)
                                            (long (or (get-in state (:principal-inflight paths)) 0))
                                            0)
                       tenant-daily-used (daily-used (get-in state (:tenant-daily paths)) day)
                       principal-daily-used (if (:principal-daily paths)
                                              (daily-used (get-in state (:principal-daily paths)) day)
                                              0)
                       tenant-rpm-limit (positive-long (:requests-per-minute limits-tenant))
                       principal-rpm-limit (positive-long (:requests-per-minute limits-principal))
                       tenant-inflight-limit (positive-long (:max-concurrent-requests limits-tenant))
                       principal-inflight-limit (positive-long (:max-concurrent-requests limits-principal))
                       tenant-daily-limit (positive-long (:daily-max-billed-tokens limits-tenant))
                       principal-daily-limit (positive-long (:daily-max-billed-tokens limits-principal))
                       blocked
                       (cond
                         (and (some? tenant-rpm-limit)
                              (>= tenant-minute-count tenant-rpm-limit))
                         {:reason :tenant/rate-limit-exceeded
                          :scope :tenant
                          :limit tenant-rpm-limit}

                         (and (some? principal-rpm-limit)
                              (>= principal-minute-count principal-rpm-limit))
                         {:reason :principal/rate-limit-exceeded
                          :scope :principal
                          :limit principal-rpm-limit}

                         (and (some? tenant-inflight-limit)
                              (>= tenant-inflight tenant-inflight-limit))
                         {:reason :tenant/concurrency-limit-exceeded
                          :scope :tenant
                          :limit tenant-inflight-limit}

                         (and (some? principal-inflight-limit)
                              (>= principal-inflight principal-inflight-limit))
                         {:reason :principal/concurrency-limit-exceeded
                          :scope :principal
                          :limit principal-inflight-limit}

                         (and (some? tenant-daily-limit)
                              (> (+ tenant-daily-used est-billed) tenant-daily-limit))
                         {:reason :tenant/budget-exceeded
                          :scope :tenant
                          :limit tenant-daily-limit}

                         (and (some? principal-daily-limit)
                              (> (+ principal-daily-used est-billed) principal-daily-limit))
                         {:reason :principal/budget-exceeded
                          :scope :principal
                          :limit principal-daily-limit}

                         :else nil)]
                   (if (map? blocked)
                     (do
                       (vreset! result* {:ok? false
                                         :error (:reason blocked)
                                         :details (cond-> {:tenant/id tenant-id
                                                           :principal/ref principal-ref
                                                           :limit (:limit blocked)
                                                           :estimated-billed-tokens est-billed}
                                                    (keyword? (:scope blocked))
                                                    (assoc :scope (:scope blocked)))})
                       (cond-> state
                         (:tenant-total paths)
                         (update-in (:tenant-total paths) bump-total :rejected (:reason blocked))
                         (:principal-total paths)
                         (update-in (:principal-total paths) bump-total :rejected (:reason blocked))))
                     (let [state1 (reserve-window! state (:tenant-minute paths) slot)
                           state2 (if (:principal-minute paths)
                                    (reserve-window! state1 (:principal-minute paths) slot)
                                    state1)
                           state3 (inc-inflight! state2 (:tenant-inflight paths))
                           state4 (if (:principal-inflight paths)
                                    (inc-inflight! state3 (:principal-inflight paths))
                                    state3)
                           reservation {:tenant/id tenant-id
                                        :principal/ref principal-ref
                                        :estimated-billed-tokens est-billed
                                        :slot slot
                                        :day day}]
                       (vreset! result* {:ok? true
                                         :reservation reservation})
                       state4)))))
        @result*))))

(defn- response-billed-tokens
  [response reservation]
  (let [body (if (map? response) (:body response) nil)
        usage (if (map? body) (get-in body [:result :usage]) nil)
        from-usage (or (positive-long (:total-tokens usage))
                       (positive-long (:tokens/total usage))
                       (positive-long (:completion-tokens usage))
                       (positive-long (:tokens/completion usage)))]
    (long (or from-usage
              (positive-long (:estimated-billed-tokens reservation))
              0))))

(defn finalize!
  [state-atom reservation response latency-ms]
  (when (and (instance? clojure.lang.IAtom state-atom)
             (map? reservation))
    (let [tenant-id (or (keywordish (:tenant/id reservation)) default-tenant-id)
          principal-ref (when (string? (:principal/ref reservation))
                          (:principal/ref reservation))
          day (or (trim-s (:day reservation))
                  (day-slot (System/currentTimeMillis)))
          billed (response-billed-tokens response reservation)
          status (int (or (some-> response :status) 500))
          error? (>= status 400)
          paths (reservation-key-paths tenant-id principal-ref)]
      (swap! state-atom
             (fn [state0]
               (let [state (if (map? state0) state0 (default-state))
                     tenant-day-entry (get-in state (:tenant-daily paths))
                     principal-day-entry (when (:principal-daily paths)
                                           (get-in state (:principal-daily paths)))
                     tenant-used (daily-used tenant-day-entry day)
                     principal-used (if principal-day-entry
                                      (daily-used principal-day-entry day)
                                      0)
                     state1 (dec-inflight! state (:tenant-inflight paths))
                     state2 (if (:principal-inflight paths)
                              (dec-inflight! state1 (:principal-inflight paths))
                              state1)
                     state3 (assoc-in state2 (:tenant-daily paths)
                                      {:day day
                                       :billed-tokens (+ tenant-used billed)})
                     state4 (if (:principal-daily paths)
                              (assoc-in state3 (:principal-daily paths)
                                        {:day day
                                         :billed-tokens (+ principal-used billed)})
                              state3)
                     total-update (fn [m]
                                    (-> (or m {})
                                        (update :requests (fnil inc 0))
                                        (update :latency-ms/sum (fnil + 0.0) (double (or latency-ms 0.0)))
                                        (update :latency-ms/max (fnil max 0.0) (double (or latency-ms 0.0)))
                                        (update :billed-tokens (fnil + 0) billed)
                                        (cond-> error?
                                          (update :errors (fnil inc 0)))))]
                 (cond-> state4
                   (:tenant-total paths)
                   (update-in (:tenant-total paths) total-update)
                   (:principal-total paths)
                   (update-in (:principal-total paths) total-update)))))))
  nil)

(defn snapshot
  ([state-atom]
   (snapshot state-atom nil nil))
  ([state-atom tenant-id principal-ref]
   (let [state (if (instance? clojure.lang.IAtom state-atom)
                 @state-atom
                 (default-state))
         totals-tenant (if (map? (:totals/by-tenant state))
                         (:totals/by-tenant state)
                         {})
         totals-principal (if (map? (:totals/by-principal state))
                            (:totals/by-principal state)
                            {})
         tenant-k (keywordish tenant-id)
         principal-k (normalize-principal-ref principal-ref)
         selected-tenant (if (keyword? tenant-k)
                           (select-keys totals-tenant [tenant-k])
                           totals-tenant)
         selected-principal (if (string? principal-k)
                              (select-keys totals-principal [principal-k])
                              totals-principal)]
     {:by-tenant selected-tenant
      :by-principal selected-principal})))
