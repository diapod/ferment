(ns

    ^{:doc    "Offline evaluation runner and promotion-gate CLI for student checkpoints."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.eval

  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ferment.training.judge :as training-judge]
            [ferment.training.promotion :as training-promotion]))

(def ^:private suite-aliases
  {:protocol :protocol-conformance
   :protocol-conformance :protocol-conformance
   :constitution :constitution-compliance
   :constitution-compliance :constitution-compliance
   :regression :regression})

(def ^:private default-config
  {:suites [:protocol-conformance
            :constitution-compliance
            :regression]
   :report {:include-cases? true
            :failed-only? false}})

(def ^:private default-cli-opts
  {:in nil
   :out-report "target/training/eval-report.json"
   :out-promotion "target/training/promotion-report.json"
   :promotion? true
   :fail-on-reject? false
   :eval-config-path nil
   :promotion-config-path nil
   :eval {}
   :promotion {}})

(def ^:private internal-marker-pattern
  #"<(?:think|tool_call)>")

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

(defn- parse-bool-safe
  [v default]
  (cond
    (boolean? v) v
    (nil? v) default
    (number? v) (not (zero? (long v)))
    (string? v) (let [s (-> v str/trim str/lower-case)]
                  (if (contains? #{"1" "true" "yes" "on"} s)
                    true
                    (if (contains? #{"0" "false" "no" "off"} s)
                      false
                      default)))
    :else default))

(defn- parse-double-safe
  [v]
  (cond
    (number? v) (double v)
    (string? v) (try
                  (Double/parseDouble (str/trim v))
                  (catch Throwable _ nil))
    :else nil))

(defn- parse-json-safe
  [s]
  (try
    (json/parse-string s true)
    (catch Throwable t
      (throw (ex-info "Cannot parse JSON input."
                      {:error :training/eval-invalid-json
                       :message (.getMessage t)} t)))))

(defn- read-input-records
  [path]
  (let [body  (slurp path)
        body' (trim-s body)
        lines (->> (str/split-lines (or body ""))
                   (map trim-s)
                   (remove nil?)
                   vec)]
    (cond
      (nil? body')
      []

      (str/starts-with? body' "[")
      (let [parsed (parse-json-safe body')]
        (if (vector? parsed)
          parsed
          (throw (ex-info "JSON array expected for evaluation input."
                          {:error :training/eval-invalid-shape
                           :type (type parsed)}))))

      (str/starts-with? body' "{")
      (if (and (> (count lines) 1)
               (every? #(str/starts-with? % "{") lines))
        (mapv parse-json-safe lines)
        (let [parsed (parse-json-safe body')]
          (cond
            (vector? parsed) parsed
            (map? parsed) [parsed]
            :else (throw (ex-info "JSON object/array expected for evaluation input."
                                  {:error :training/eval-invalid-shape
                                   :type (type parsed)})))))

      :else
      (mapv parse-json-safe lines))))

(defn- write-json!
  [path value]
  (io/make-parents (io/file path))
  (with-open [w (io/writer path)]
    (.write w (json/generate-string value {:pretty true})))
  path)

(defn- normalize-suite
  [suite]
  (let [k (keywordish suite)]
    (get suite-aliases k)))

(defn- normalize-config
  [cfg]
  (let [cfg' (merge default-config (if (map? cfg) cfg {}))
        suites (->> (or (:suites cfg') [])
                    (keep normalize-suite)
                    vec)
        report (if (map? (:report cfg'))
                 (:report cfg')
                 {})]
    {:suites (if (seq suites) suites (:suites default-config))
     :report {:include-cases? (parse-bool-safe (:include-cases? report) true)
              :failed-only? (parse-bool-safe (:failed-only? report) false)}}))

(defn- extract-output-text
  [m]
  (or (trim-s (:output/text m))
      (trim-s (get-in m [:output :text]))
      (trim-s (get-in m [:actual :text]))
      (trim-s (get-in m [:call :out :text]))
      (trim-s (get-in m [:result :out :text]))
      (trim-s (:text m))))

(defn- extract-result-type
  [m]
  (or (keywordish (:result/type m))
      (keywordish (get-in m [:result :type]))
      (keywordish (get-in m [:call :result/type]))))

(defn- extract-latency
  [m]
  (or (parse-double-safe (:latency-ms m))
      (parse-double-safe (get-in m [:timing :call/latency-ms]))
      0.0))

(defn- explicit-pass?
  [m]
  (cond
    (contains? m :pass?) (boolean (:pass? m))
    (contains? m :verdict/pass?) (boolean (:verdict/pass? m))
    (contains? m :judge/pass?) (boolean (:judge/pass? m))
    (contains? m :regression/pass?) (boolean (:regression/pass? m))
    :else nil))

(defn- protocol-verdict
  [m]
  (if (contains? m :pass?)
    {:pass? (boolean (:pass? m))
     :reason :explicit-pass}
    (let [text (extract-output-text m)
          result-type (extract-result-type m)
          expected-result-type (or (keywordish (:expected/result-type m))
                                   (keywordish (get-in m [:expected :result/type])))
          has-text? (some? text)
          has-markers? (boolean (and (string? text)
                                     (re-find internal-marker-pattern text)))
          result-type-ok? (or (nil? expected-result-type)
                              (= expected-result-type result-type))
          pass? (and has-text?
                     (not has-markers?)
                     result-type-ok?)]
      {:pass? pass?
       :reason (cond
                 (not has-text?) :protocol/empty-output
                 has-markers? :protocol/internal-markers
                 (not result-type-ok?) :protocol/result-type-mismatch
                 :else :protocol/pass)})))

(defn- constitution-verdict
  [m]
  (if-some [pass? (explicit-pass? m)]
    {:pass? pass?
     :reason :explicit-pass}
    (let [judge-pass (or (get m :judge/pass?)
                         (get-in m [:judge :pass?])
                         (get-in m [:labels :judge :judge/pass?]))]
      (if (some? judge-pass)
        {:pass? (boolean judge-pass)
         :reason :judge-label}
        (if (integer? (:training.event/version m))
          (let [verdict (training-judge/evaluate! m {:mode :rules-only})
                pass? (true? (:judge/pass? verdict))]
            {:pass? pass?
             :reason (if pass?
                       :judge-rules-pass
                       :judge-rules-failed)})
          {:pass? false
           :reason :constitution/no-verdict})))))

(defn- regression-verdict
  [m]
  (if-some [pass? (explicit-pass? m)]
    {:pass? pass?
     :reason :explicit-pass}
    (let [expected (or (trim-s (:expected/text m))
                       (trim-s (get-in m [:expected :text]))
                       (trim-s (:expected m)))
          actual (extract-output-text m)
          mode (or (keywordish (:match m))
                   (keywordish (:regression/match m))
                   :exact)
          pass? (case mode
                  :contains (and (some? expected)
                                 (some? actual)
                                 (str/includes? actual expected))
                  :exact (and (some? expected)
                              (some? actual)
                              (= expected actual))
                  false)]
      {:pass? pass?
       :reason (cond
                 (nil? expected) :regression/missing-expected
                 (nil? actual) :regression/missing-actual
                 pass? :regression/pass
                 :else :regression/mismatch)})))

(defn- infer-suite
  [m]
  (or (normalize-suite (:suite m))
      (normalize-suite (:case/suite m))
      (when (integer? (:training.event/version m))
        :protocol-conformance)))

(defn- evaluate-one
  [m idx]
  (let [suite (infer-suite m)
        case-id (or (trim-s (:case/id m))
                    (trim-s (:id m))
                    (trim-s (:training.event/id m))
                    (str "case-" idx))]
    (if-not (keyword? suite)
      {:case/id case-id
       :suite nil
       :pass? false
       :score 0.0
       :reason :eval/unknown-suite
       :skipped? true
       :latency-ms (extract-latency m)}
      (let [{:keys [pass? reason]} (case suite
                                     :protocol-conformance (protocol-verdict m)
                                     :constitution-compliance (constitution-verdict m)
                                     :regression (regression-verdict m)
                                     {:pass? false :reason :eval/unknown-suite})]
        {:case/id case-id
         :suite suite
         :pass? (boolean pass?)
         :score (if pass? 1.0 0.0)
         :reason reason
         :skipped? false
         :latency-ms (extract-latency m)}))))

(defn evaluate-cases
  "Evaluates cases and returns deterministic report map."
  ([cases]
   (evaluate-cases cases nil))
  ([cases cfg]
   (let [{:keys [suites report] :as cfg'} (normalize-config cfg)
         suite-set (set suites)
         raw-cases (->> (or cases [])
                        (mapcat (fn [entry]
                                  (if (and (map? entry)
                                           (sequential? (:cases entry)))
                                    (:cases entry)
                                    [entry])))
                        vec)
         evaluated (->> raw-cases
                        (map-indexed (fn [idx entry]
                                       (evaluate-one entry idx)))
                        (filter (fn [result]
                                  (and (not (:skipped? result))
                                       (contains? suite-set (:suite result)))))
                        (sort-by (juxt :suite :case/id))
                        vec)
         skipped (- (count raw-cases) (count evaluated))
         by-suite (reduce (fn [acc result]
                            (let [suite (:suite result)]
                              (update acc suite
                                      (fn [entry]
                                        (let [entry' (if (map? entry)
                                                       entry
                                                       {:total 0 :passed 0 :failed 0 :score/sum 0.0 :latency/sum 0.0})]
                                          (-> entry'
                                              (update :total inc)
                                              (update :passed (fn [n] (+ n (if (:pass? result) 1 0))))
                                              (update :failed (fn [n] (+ n (if (:pass? result) 0 1))))
                                              (update :score/sum + (:score result))
                                              (update :latency/sum + (:latency-ms result))))))))
                          {}
                          evaluated)
         by-suite (->> by-suite
                       (map (fn [[suite stats]]
                              [suite
                               {:total (:total stats)
                                :passed (:passed stats)
                                :failed (:failed stats)
                                :pass-rate (if (pos? (:total stats))
                                             (/ (double (:passed stats))
                                                (double (:total stats)))
                                             0.0)
                                :avg-score (if (pos? (:total stats))
                                             (/ (double (:score/sum stats))
                                                (double (:total stats)))
                                             0.0)
                                :avg-latency-ms (if (pos? (:total stats))
                                                  (/ (double (:latency/sum stats))
                                                     (double (:total stats)))
                                                  0.0)}]))
                       (into (sorted-map)))
         total (count evaluated)
         passed (count (filter :pass? evaluated))
         failed (- total passed)
         report-cases (cond
                        (not (true? (:include-cases? report))) []
                        (true? (:failed-only? report)) (vec (filter (comp false? :pass?) evaluated))
                        :else evaluated)]
     {:eval/version 1
      :config cfg'
      :input/count (count raw-cases)
      :cases/evaluated total
      :cases/skipped skipped
      :summary {:overall {:total total
                          :passed passed
                          :failed failed
                          :pass-rate (if (pos? total)
                                       (/ (double passed) (double total))
                                       0.0)}
                :by-suite by-suite}
      :failed/case-ids (->> evaluated
                            (filter (comp false? :pass?))
                            (mapv :case/id))
      :cases report-cases})))

(defn run-eval!
  "Runs offline evaluation from input artifact and writes report JSON."
  [opts]
  (let [opts' (merge default-cli-opts (if (map? opts) opts {}))
        in-path (trim-s (:in opts'))]
    (when-not in-path
      (throw (ex-info "Missing required option --in PATH."
                      {:error :training/eval-missing-input})))
    (let [cases (read-input-records in-path)
          report (evaluate-cases cases (:eval opts'))
          out-report (or (trim-s (:out-report opts'))
                         "target/training/eval-report.json")]
      (write-json! out-report report)
      {:ok? true
       :input/count (:input/count report)
       :cases/evaluated (:cases/evaluated report)
       :out/report out-report
       :report report})))

(defn- parse-config-file
  [path]
  (let [path' (trim-s path)]
    (if-not path'
      {}
      (let [body (slurp path')
            ext (-> path' str/lower-case)]
        (cond
          (str/ends-with? ext ".edn")
          (let [parsed (edn/read-string body)]
            (if (map? parsed) parsed {}))

          (or (str/ends-with? ext ".json")
              (str/ends-with? ext ".jsonl"))
          (let [parsed (parse-json-safe body)]
            (if (map? parsed) parsed {}))

          :else
          (throw (ex-info "Unsupported config file format (expected .edn or .json)."
                          {:error :training/eval-unsupported-config-format
                           :path path'})))))))

(defn- usage
  []
  (str/join
   \newline
   ["Usage: bin/eval-student --in PATH [options]"
    ""
    "Required:"
    "  --in PATH                  Input JSON/JSONL with eval cases."
    ""
    "Options:"
    "  --out-report PATH          Output eval report JSON (default: target/training/eval-report.json)"
    "  --out-promotion PATH       Output promotion report JSON (default: target/training/promotion-report.json)"
    "  --eval-config PATH         Extra eval config (.edn/.json)"
    "  --promotion-config PATH    Extra promotion config (.edn/.json)"
    "  --overall-min X            Override promotion threshold: overall pass-rate min"
    "  --protocol-min X           Override promotion threshold: protocol-conformance pass-rate min"
    "  --constitution-min X       Override promotion threshold: constitution-compliance pass-rate min"
    "  --regression-min X         Override promotion threshold: regression pass-rate min"
    "  --no-promotion             Skip promotion gate decision"
    "  --fail-on-reject           Exit with code 3 when promotion rejects"
    "  -h, --help                 Show this help"]))

(defn- parse-args
  [argv]
  (loop [args argv
         opts default-cli-opts]
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

          (= arg "--out-report")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :out-report v))
            {:error "Missing value for --out-report"})

          (= arg "--out-promotion")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :out-promotion v))
            {:error "Missing value for --out-promotion"})

          (= arg "--eval-config")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :eval-config-path v))
            {:error "Missing value for --eval-config"})

          (= arg "--promotion-config")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :promotion-config-path v))
            {:error "Missing value for --promotion-config"})

          (= arg "--overall-min")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc-in opts [:promotion :thresholds :overall/pass-rate-min] n))
              {:error "Invalid value for --overall-min"})
            {:error "Missing value for --overall-min"})

          (= arg "--protocol-min")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc-in opts [:promotion :thresholds :suite-pass-rate-min :protocol-conformance] n))
              {:error "Invalid value for --protocol-min"})
            {:error "Missing value for --protocol-min"})

          (= arg "--constitution-min")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc-in opts [:promotion :thresholds :suite-pass-rate-min :constitution-compliance] n))
              {:error "Invalid value for --constitution-min"})
            {:error "Missing value for --constitution-min"})

          (= arg "--regression-min")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc-in opts [:promotion :thresholds :suite-pass-rate-min :regression] n))
              {:error "Invalid value for --regression-min"})
            {:error "Missing value for --regression-min"})

          (= arg "--no-promotion")
          (recur more (assoc opts :promotion? false))

          (= arg "--fail-on-reject")
          (recur more (assoc opts :fail-on-reject? true))

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
        (let [eval-config-file (parse-config-file (:eval-config-path opts))
              promotion-config-file (parse-config-file (:promotion-config-path opts))
              eval-cfg (merge eval-config-file (:eval opts))
              eval-result (run-eval! (assoc opts :eval eval-cfg))
              promotion-cfg (merge promotion-config-file (:promotion opts))
              promotion? (true? (:promotion? opts))
              promotion-report (when promotion?
                                 (training-promotion/evaluate-report
                                  (:report eval-result)
                                  promotion-cfg))
              out-promotion (trim-s (:out-promotion opts))
              _ (when (and promotion? (some? out-promotion))
                  (write-json! out-promotion promotion-report))
              rejected? (and promotion?
                             (false? (:promotion/eligible? promotion-report)))]
          (println "Evaluation completed.")
          (println (str "  input cases:      " (:input/count eval-result)))
          (println (str "  evaluated cases:  " (:cases/evaluated eval-result)))
          (println (str "  eval report:      " (:out/report eval-result)))
          (when promotion?
            (println (str "  promotion status: " (:promotion/status promotion-report)))
            (println (str "  promotion out:    " out-promotion)))
          (if (and rejected? (true? (:fail-on-reject? opts)))
            (System/exit 3)
            (System/exit 0)))
        (catch Throwable t
          (binding [*out* *err*]
            (println "Evaluation failed.")
            (println (.getMessage t))
            (when-let [d (ex-data t)]
              (println (pr-str d))))
          (System/exit 1))))))
