(ns

    ^{:doc    "Model adapters facade for runtime worker operations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.adapters.model

  (:require [clojure.string :as str]
            [ferment.model :as model]
            [ferment.router :as router]))

(def ^:private known-transport-types
  #{:local-runtime :remote-http :peer})

(def ^:private known-transport-auth-modes
  #{:none :bearer :basic :api-key :mtls :session :custom})

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- nonneg-int-or-nil
  [v]
  (let [n (cond
            (int? v) v
            (integer? v) (int v)
            :else nil)]
    (when (and (int? n) (<= 0 n))
      n)))

(defn- normalize-retry
  [v]
  (let [m (if (map? v) v {})
        max' (nonneg-int-or-nil (:max m))
        backoff-ms' (nonneg-int-or-nil (:backoff-ms m))]
    (cond-> {}
      (int? max') (assoc :max max')
      (int? backoff-ms') (assoc :backoff-ms backoff-ms'))))

(defn- cap-transport-raw
  [cap]
  (let [transport (if (map? (:transport cap)) (:transport cap) {})]
    {:type (or (keywordish (:transport/type cap))
               (keywordish (:type transport)))
     :auth (or (keywordish (:transport/auth cap))
               (keywordish (:auth transport)))
     :timeout-ms (or (nonneg-int-or-nil (:transport/timeout-ms cap))
                     (nonneg-int-or-nil (:timeout-ms transport)))
     :retry (let [retry' (or (:transport/retry cap)
                             (:retry transport))]
              (when (map? retry')
                (normalize-retry retry')))}))

(defn- runtime-transport-defaults
  [runtime model-k]
  (let [runtime-cfg (some-> (model/model-entry runtime model-k)
                            :runtime)
        invoke-http? (map? (:invoke/http runtime-cfg))
        timeout-ms (or (some-> runtime-cfg :invoke/http :timeout-ms nonneg-int-or-nil)
                       (some-> runtime-cfg :invoke/timeout-ms nonneg-int-or-nil))
        retry-max (or (some-> runtime-cfg :invoke/http :retries nonneg-int-or-nil)
                      (some-> runtime-cfg :invoke/http :retry-max nonneg-int-or-nil)
                      (some-> runtime-cfg :invoke/http :retry/max nonneg-int-or-nil))
        retry-backoff-ms (or (some-> runtime-cfg :invoke/http :retry-ms nonneg-int-or-nil)
                             (some-> runtime-cfg :invoke/http :retry/backoff-ms nonneg-int-or-nil))]
    (cond-> {:transport/type (if invoke-http? :remote-http :local-runtime)
             :transport/auth :none}
      (int? timeout-ms) (assoc :transport/timeout-ms timeout-ms)
      (or (int? retry-max) (int? retry-backoff-ms))
      (assoc :transport/retry
             (cond-> {}
               (int? retry-max) (assoc :max retry-max)
               (int? retry-backoff-ms) (assoc :backoff-ms retry-backoff-ms))))))

(defn transport-descriptor
  "Returns canonical transport descriptor for selected capability.

  Descriptor keys:
  - `:transport/type`      keyword (`:local-runtime`, `:remote-http`, `:peer`, ...),
  - `:transport/auth`      keyword auth mode,
  - `:transport/timeout-ms` optional non-negative int,
  - `:transport/retry`      optional map (`:max`, `:backoff-ms`),
  - `:transport/model-key`  selected model key."
  [runtime resolver cap-id intent]
  (let [cap       (router/resolver-capability runtime resolver cap-id)
        model-k   (router/resolve-model-key runtime resolver cap-id intent)
        defaults  (runtime-transport-defaults runtime model-k)
        cap-raw   (if (map? cap) (cap-transport-raw cap) {})
        type'     (or (when (contains? known-transport-types (:type cap-raw))
                        (:type cap-raw))
                      (:transport/type defaults)
                      :local-runtime)
        auth'     (or (when (contains? known-transport-auth-modes (:auth cap-raw))
                        (:auth cap-raw))
                      (:transport/auth defaults)
                      :none)
        timeout-ms' (or (:timeout-ms cap-raw)
                        (:transport/timeout-ms defaults))
        retry'    (let [from-cap (:retry cap-raw)
                        from-defaults (:transport/retry defaults)]
                    (cond
                      (and (map? from-cap) (seq from-cap)) from-cap
                      (and (map? from-defaults) (seq from-defaults)) from-defaults
                      :else nil))]
    (cond-> {:transport/type type'
             :transport/auth auth'
             :transport/model-key model-k}
      (int? timeout-ms') (assoc :transport/timeout-ms timeout-ms')
      (and (map? retry') (seq retry')) (assoc :transport/retry retry'))))

(defn- invoke-response-error-key
  [invoke-response]
  (or (some-> invoke-response :details :error keywordish)
      (some-> invoke-response :error keywordish)))

(defn- invoke-response-transport-type
  [invoke-response]
  (or (some-> invoke-response :result :transport keywordish)
      (some-> invoke-response :details :transport keywordish)
      (some-> invoke-response :details :transport/type keywordish)))

(defn- transport-failure-class
  [error-k]
  (case error-k
    (:invoke-timeout :timeout) :timeout
    :invoke-http-status :remote-status
    (:invoke-http-failed
     :invoke-http-url-missing
     :runtime-port-in-use) :connectivity
    (:runtime-process-missing
     :runtime-process-dead
     :runtime-process-io-unavailable
     :runtime-stdin-write-failed
     :runtime-not-ready) :unavailable
    (:runtime-session-reset-failed
     :runtime-command-write-failed
     :quit-not-configured
     :reset-not-configured) :runtime-control
    nil))

(defn invoke-failure-info
  "Extracts deterministic transport failure info from model invoke response."
  [runtime resolver cap-id intent invoke-response]
  (let [descriptor      (transport-descriptor runtime resolver cap-id intent)
        error-k         (invoke-response-error-key invoke-response)
        response-type   (invoke-response-transport-type invoke-response)
        descriptor-type (keywordish (:transport/type descriptor))
        transport-class (or response-type descriptor-type :local-runtime)
        failure-class   (transport-failure-class error-k)]
    (cond-> {:transport/class transport-class}
      (keyword? error-k) (assoc :transport/error error-k)
      (keyword? failure-class) (assoc :transport/failure-class failure-class))))

(defn invoke-worker!
  "Invokes worker with payload over command channel."
  [worker payload]
  (model/command-bot-worker! worker :invoke payload))

(defn session-workers-state
  [runtime]
  (model/session-workers-state runtime))

(defn expire-session-workers!
  [runtime]
  (model/expire-session-workers! runtime))

(defn freeze-session-worker!
  [runtime model-id sid]
  (model/freeze-session-worker! runtime model-id sid))

(defn thaw-session-worker!
  [runtime model-id sid]
  (model/thaw-session-worker! runtime model-id sid))
