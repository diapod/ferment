(ns

    ^{:doc    "Exports replay packages to training events and JSONL datasets."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.export-events

  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private replay-redacted-placeholder
  "[REDACTED]")

(def ^:private internal-marker-pattern
  #"<(?:think|tool_call)>")

(def ^:private default-opts
  {:in nil
   :out-events "target/training/events-v1.jsonl"
   :out-train "target/training/train.jsonl"
   :train-task :meta-protocol
   :include-failed? false})

(defn- usage
  []
  (str/join
   \newline
   ["Usage: bin/export-training-events --in PATH [options]"
    ""
    "Required:"
    "  --in PATH                Input JSON/JSONL with replay records."
    ""
    "Options:"
    "  --out-events PATH        Output JSONL for training.event/v1 (default: target/training/events-v1.jsonl)"
    "  --out-train PATH         Output JSONL for LoRA SFT rows (default: target/training/train.jsonl)"
    "  --train-task KEYWORD     Label for training task (default: :meta-protocol)"
    "  --include-failed         Include failed call attempts in train JSONL"
    "  -h, --help               Show this help"]))

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

(defn- parse-json-safe
  [s]
  (try
    (json/parse-string s true)
    (catch Throwable t
      (throw (ex-info "Cannot parse JSON input."
                      {:error :training/export-invalid-json
                       :message (.getMessage t)} t)))))

(defn- read-input-records
  [path]
  (let [body  (slurp path)
        body' (trim-s body)]
    (cond
      (nil? body')
      []

      (or (str/starts-with? body' "{")
          (str/starts-with? body' "["))
      (let [parsed (parse-json-safe body')]
        (cond
          (vector? parsed) parsed
          (map? parsed)    [parsed]
          :else
          (throw (ex-info "JSON input must be object or array."
                          {:error :training/export-invalid-shape
                           :type (type parsed)}))))

      :else
      (->> (str/split-lines body)
           (map trim-s)
           (remove nil?)
           (mapv parse-json-safe)))))

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

(defn replay-records->events
  "Converts replay records (endpoint responses or raw replay entries) to
  `training.event/v1` maps."
  ([records]
   (replay-records->events records default-opts))
  ([records opts]
   (->> (or records [])
        (map normalize-replay-entry)
        (filter replay-entry?)
        (mapcat (fn [replay]
                  (->> (transcript-calls replay)
                       (map (fn [[idx call]]
                              (transcript-entry->event replay idx call opts))))))
         vec)))

(defn event->train-row
  "Converts one `training.event/v1` map to a LoRA-friendly JSONL row."
  [event]
  (let [request-resolved (if (map? (get-in event [:request :resolved]))
                           (get-in event [:request :resolved])
                           {})
        call            (if (map? (:call event)) (:call event) {})
        routing         (if (map? (:routing event)) (:routing event) {})
        source          (if (map? (:source event)) (:source event) {})
        task            (if (map? (get request-resolved :task))
                          (get request-resolved :task)
                          {})
        prompt-map {:task {:intent (keywordish (:intent task))
                           :requires (:requires task)}
                    :input (:input call)
                    :routing {:mode (keywordish (:mode routing))
                              :execution-path (:execution-path routing)
                              :policy/snapshot-id (get-in event [:policy :snapshot-id])}
                    :call {:intent (keywordish (:intent call))
                           :cap/id (keywordish (:cap/id call))
                           :attempt (:attempt call)
                           :candidate-index (:candidate-index call)}}
        completion-map {:result {:type (keywordish (:result/type call))
                                 :out  (:out call)}
                        :decision {:accepted? (true? (get-in event [:labels :accepted?]))
                                   :failure/type (keywordish (:failure/type call))}}]
    {:id (:training.event/id event)
     :prompt (pr-str prompt-map)
     :completion (pr-str completion-map)
     :meta {:trace_id (:trace/id source)
            :request_id (:request/id source)
            :intent (some-> call :intent keywordish name)
            :cap_id (some-> call :cap/id keywordish name)
            :status (get-in event [:response :status])
            :outcome (some-> event :response :outcome keywordish name)
            :call_latency_ms (get-in event [:timing :call/latency-ms])}}))

(defn events->train-rows
  "Converts training events to LoRA JSONL rows.
  By default includes only accepted events."
  ([events]
   (events->train-rows events default-opts))
  ([events opts]
   (let [include-failed? (true? (:include-failed? opts))]
     (->> (or events [])
          (filter (fn [event]
                    (or include-failed?
                        (true? (get-in event [:labels :accepted?])))))
          (map event->train-row)
          vec))))

(defn- write-jsonl!
  [path rows]
  (io/make-parents (io/file path))
  (with-open [w (io/writer path)]
    (doseq [row rows]
      (.write w (json/generate-string row))
      (.write w "\n")))
  path)

(defn export-training-events!
  "Reads replay records from `:in`, writes:
  - `:out-events` -> canonical `training.event/v1` JSONL
  - `:out-train`  -> LoRA SFT JSONL"
  [opts]
  (let [opts'   (merge default-opts (or opts {}))
        in-path (trim-s (:in opts'))]
    (when-not in-path
      (throw (ex-info "Missing required option --in PATH."
                      {:error :training/export-missing-input})))
    (let [records    (read-input-records in-path)
          events     (replay-records->events records opts')
          train-rows (events->train-rows events opts')
          events-out (write-jsonl! (:out-events opts') events)
          train-out  (write-jsonl! (:out-train opts') train-rows)]
      {:ok? true
       :input/count (count records)
       :events/count (count events)
       :train/count (count train-rows)
       :out/events events-out
       :out/train train-out})))

(defn- parse-args
  [argv]
  (loop [args argv
         opts default-opts]
    (if (empty? args)
      opts
      (let [[arg & more] args]
        (cond
          (or (= arg "-h") (= arg "--help"))
          (assoc opts :help? true)

          (= arg "--in")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :in v))
            {:error "Missing value for --in"})

          (= arg "--out-events")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :out-events v))
            {:error "Missing value for --out-events"})

          (= arg "--out-train")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :out-train v))
            {:error "Missing value for --out-train"})

          (= arg "--train-task")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :train-task (or (keywordish v) :meta-protocol)))
            {:error "Missing value for --train-task"})

          (= arg "--include-failed")
          (recur more (assoc opts :include-failed? true))

          :else
          {:error (str "Unknown argument: " arg)})))))

(defn -main
  [& argv]
  (let [opts (parse-args argv)]
    (cond
      (:help? opts)
      (do
        (println (usage))
        (System/exit 0))

      (:error opts)
      (do
        (binding [*out* *err*]
          (println (:error opts))
          (println)
          (println (usage)))
        (System/exit 2))

      :else
      (try
        (let [result (export-training-events! opts)]
          (println "Export completed.")
          (println (str "  input records: " (:input/count result)))
          (println (str "  events:        " (:events/count result)))
          (println (str "  train rows:    " (:train/count result)))
          (println (str "  events file:   " (:out/events result)))
          (println (str "  train file:    " (:out/train result)))
          (System/exit 0))
        (catch Throwable t
          (binding [*out* *err*]
            (println "Export failed.")
            (println (.getMessage t))
            (when-let [d (ex-data t)]
              (println (pr-str d))))
          (System/exit 1))))))
