(ns

    ^{:doc    "Canonical training-event constructors from replay/runtime artifacts."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.events

  (:require [clojure.string :as str]
            [ferment.training.judge :as training-judge]
            [ferment.training.redaction :as training-redaction]))

(def ^:private replay-redacted-placeholder
  "[REDACTED]")

(def ^:private internal-marker-pattern
  #"<(?:think|tool_call)>")

(def ^:private default-opts
  {:train-task :meta-protocol
   :judge {:mode :disabled}
   :redaction {:enabled? false}})

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

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

(defn- replay-entry?
  [m]
  (and (map? m)
       (map? (:request m))
       (map? (:response m))))

(defn- normalize-replay-entry
  [record]
  (cond
    (replay-entry? record)
    record

    (and (map? record)
         (map? (:replay record)))
    (let [entry (:replay record)
          trace-id (or (trim-s (:trace/id entry))
                       (trim-s (:trace/id record)))]
      (cond-> entry
        (and (some? trace-id) (nil? (:trace/id entry)))
        (assoc :trace/id trace-id)))

    (and (map? record)
         (map? (:body record))
         (map? (get-in record [:body :replay])))
    (normalize-replay-entry (get-in record [:body :replay]))

    :else nil))

(defn- record-trace-id
  [replay]
  (or (trim-s (:trace/id replay))
      (trim-s (get-in replay [:request :resolved :trace :id]))
      (trim-s (get-in replay [:request :prepared :trace :id]))
      (trim-s (get-in replay [:request :payload :trace :id]))))

(defn- contains-marker?
  [v marker]
  (cond
    (string? v) (str/includes? v marker)
    (map? v) (boolean (some #(contains-marker? % marker) (vals v)))
    (sequential? v) (boolean (some #(contains-marker? % marker) v))
    :else false))

(defn- contains-internal-marker?
  [v]
  (cond
    (string? v) (boolean (re-find internal-marker-pattern v))
    (map? v) (boolean (some contains-internal-marker? (vals v)))
    (sequential? v) (boolean (some contains-internal-marker? v))
    :else false))

(defn- transcript-calls
  [replay]
  (let [transcript (or (get-in replay [:response :body :result :plan/run :transcript])
                       (get-in replay [:response :body :result :transcript]))]
    (if (sequential? transcript)
      (->> transcript
           (map-indexed vector)
           (filter (fn [[_ entry]]
                     (= :call (keywordish (:op entry)))))
           vec)
      [])))

(defn- event-id
  [trace-id idx attempt]
  (let [trace-id' (or trace-id "unknown-trace")
        attempt'  (or attempt 1)]
    (format "%s#call-%04d#a%s" trace-id' idx attempt')))

(defn- transcript-entry->event
  [replay idx call opts]
  (let [trace-id    (record-trace-id replay)
        request     (if (map? (:request replay)) (:request replay) {})
        routing     (if (map? (:routing replay)) (:routing replay) {})
        policy      (if (map? (:policy replay)) (:policy replay) {})
        response    (if (map? (:response replay)) (:response replay) {})
        diagnostics (if (map? (:diagnostics replay)) (:diagnostics replay) {})
        timing      (if (map? (:timing replay)) (:timing replay) {})
        failure-k   (keywordish (:failure/type call))
        attempt-no  (if (int? (:attempt call)) (:attempt call) 1)
        request-resolved (if (map? (:resolved request))
                           (:resolved request)
                           {})
        out         (:out call)]
    {:training.event/version 1
     :training.event/id      (event-id trace-id idx attempt-no)
     :training.event/type    :call-attempt
     :recorded-at            (:recorded-at replay)

     :source {:trace/id trace-id
              :request/id (trim-s (:request/id request-resolved))
              :session/id (trim-s (:session/id request-resolved))
              :replay/snapshot-id (trim-s (:snapshot-id policy))
              :transcript/index idx}

     :request {:payload  (if (map? (:payload request)) (:payload request) {})
               :prepared (if (map? (:prepared request)) (:prepared request) {})
               :resolved request-resolved}

     :routing {:mode            (keywordish (:mode routing))
               :routed?         (boolean (:routed? routing))
               :meta-step       (if (map? (:meta-step routing)) (:meta-step routing) {})
               :cap/decision    (if (map? (:cap/decision routing)) (:cap/decision routing) {})
               :execution-path  (if (map? (:execution-path diagnostics))
                                  (:execution-path diagnostics)
                                  {})}

     :policy {:snapshot-id (:snapshot-id policy)
              :snapshot    (if (map? (:snapshot policy))
                             (:snapshot policy)
                             {})}

     :call {:op              (keywordish (:op call))
            :intent          (keywordish (:intent call))
            :cap/id          (keywordish (:cap/id call))
            :as              (keywordish (:as call))
            :attempt         attempt-no
            :candidate-index (if (int? (:candidate-index call))
                               (:candidate-index call)
                               0)
            :input           (if (map? (:input call)) (:input call) {})
            :result/type     (keywordish (:result/type call))
            :out             out
            :error           (:error call)
            :failure/type    failure-k
            :invoke/meta     (if (map? (:invoke/meta call)) (:invoke/meta call) {})
            :plan/run        (if (map? (:plan/run call)) (:plan/run call) {})}

     :response {:status     (or (:status response) 500)
                :outcome    (keywordish (:outcome response))
                :error/type (keywordish (:error/type response))}

     :timing {:request/elapsed-ms (or (:elapsed-ms timing) 0.0)
              :call/latency-ms    (or (:latency-ms call) 0.0)}

     :labels {:accepted? (nil? failure-k)
              :train/task (or (keywordish (:train-task opts))
                              :meta-protocol)}

     :redaction {:replay/redacted? (contains-marker? request replay-redacted-placeholder)
                 :internal-markers/present? (contains-internal-marker? out)}}))

(defn- apply-event-policies
  [event opts]
  (let [redaction-cfg (:redaction opts)
        judge-cfg (:judge opts)
        {event1 :event
         redaction-audit :audit}
        (training-redaction/redact-event event redaction-cfg)
        judge-verdict (training-judge/evaluate! event1 judge-cfg)]
    (-> event1
        (update :redaction
                (fn [redaction]
                  (cond-> (if (map? redaction) redaction {})
                    (map? redaction-audit)
                    (assoc :audit redaction-audit)))
        )
        (update :labels
                (fn [labels]
                  (assoc (if (map? labels) labels {})
                         :judge judge-verdict))))))

(defn replay-entry->events
  "Converts one replay package map to canonical `training.event/v1` events.
  Returns empty vector when replay shape is invalid or transcript has no call entries."
  ([replay]
   (replay-entry->events replay default-opts))
  ([replay opts]
   (let [opts' (merge default-opts (if (map? opts) opts {}))
         entry (normalize-replay-entry replay)]
     (if-not (replay-entry? entry)
       []
       (->> (transcript-calls entry)
            (map (fn [[idx call]]
                   (-> (transcript-entry->event entry idx call opts')
                       (apply-event-policies opts'))))
            vec)))))

(defn replay-records->events
  "Converts replay records (endpoint responses or raw replay entries) to
  `training.event/v1` maps."
  ([records]
   (replay-records->events records default-opts))
  ([records opts]
   (let [opts' (merge default-opts (if (map? opts) opts {}))]
     (->> (or records [])
        (mapcat (fn [record]
                  (replay-entry->events record opts')))
        vec))))
