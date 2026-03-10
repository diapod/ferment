(ns

    ^{:doc    "Release gate for live benchmark summaries (candidate + optional baseline diff)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.benchmark.gate

  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def ^:private default-opts
  {:preset :default
   :runs 1
   :run? true
   :candidate nil
   :baseline nil
   :out nil
   :max-interactive-regress-ms 1500.0
   :max-interactive-regress-ratio 0.20
   :max-must-failed-regress 0.05
   :max-truncated-increase 0
   :require-pass? true})

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

(defn- parse-long-safe
  [v]
  (cond
    (integer? v) (long v)
    (number? v) (long (Math/floor (double v)))
    (string? v) (try
                  (Long/parseLong (str/trim v))
                  (catch Throwable _ nil))
    :else nil))

(defn- parse-double-safe
  [v]
  (cond
    (number? v) (double v)
    (string? v) (try
                  (Double/parseDouble (str/trim v))
                  (catch Throwable _ nil))
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

(defn- usage
  []
  (str/join
   \newline
   ["Usage: bin/benchmark-gate [options]"
    ""
    "Modes:"
    "  default: run benchmark-live, then evaluate release gate"
    "  --no-run: evaluate existing candidate summary"
    ""
    "Options:"
    "  --preset NAME                  default|low-latency|sla (default: default)"
    "  --runs N                       benchmark-live runs (default: 1)"
    "  --case-dir PATH                optional benchmark case directory"
    "  --baseline PATH                optional baseline summary.json for regression diff"
    "  --candidate PATH               candidate summary.json (required with --no-run)"
    "  --out PATH                     output gate report JSON path"
    "  --no-run                       skip benchmark-live and use --candidate summary"
    "  --max-interactive-regress-ms N      default: 1500"
    "  --max-interactive-regress-ratio X   default: 0.20"
    "  --max-must-failed-regress X         default: 0.05"
    "  --max-truncated-increase N          default: 0"
    "  --require-pass true|false           default: true"
    "  -h, --help                     Show this help"
    ""
    "Exit codes:"
    "  0  gate passed"
    "  2  invalid arguments"
    "  3  gate failed"
    "  4  benchmark-live run failed"]))

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

          (= arg "--preset")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :preset (or (keywordish v) :default)))
            {:error "Missing value for --preset"})

          (= arg "--runs")
          (if-let [v (first more)]
            (if-let [n (parse-long-safe v)]
              (if (pos? n)
                (recur (rest more) (assoc opts :runs n))
                {:error "Invalid value for --runs (must be >= 1)"})
              {:error "Invalid value for --runs"})
            {:error "Missing value for --runs"})

          (= arg "--case-dir")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :case-dir (trim-s v)))
            {:error "Missing value for --case-dir"})

          (= arg "--baseline")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :baseline (trim-s v)))
            {:error "Missing value for --baseline"})

          (= arg "--candidate")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :candidate (trim-s v)))
            {:error "Missing value for --candidate"})

          (= arg "--out")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :out (trim-s v)))
            {:error "Missing value for --out"})

          (= arg "--no-run")
          (recur more (assoc opts :run? false))

          (= arg "--max-interactive-regress-ms")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc opts :max-interactive-regress-ms n))
              {:error "Invalid value for --max-interactive-regress-ms"})
            {:error "Missing value for --max-interactive-regress-ms"})

          (= arg "--max-interactive-regress-ratio")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc opts :max-interactive-regress-ratio n))
              {:error "Invalid value for --max-interactive-regress-ratio"})
            {:error "Missing value for --max-interactive-regress-ratio"})

          (= arg "--max-must-failed-regress")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc opts :max-must-failed-regress n))
              {:error "Invalid value for --max-must-failed-regress"})
            {:error "Missing value for --max-must-failed-regress"})

          (= arg "--max-truncated-increase")
          (if-let [v (first more)]
            (if-let [n (parse-long-safe v)]
              (if (>= n 0)
                (recur (rest more) (assoc opts :max-truncated-increase n))
                {:error "Invalid value for --max-truncated-increase (must be >= 0)"})
              {:error "Invalid value for --max-truncated-increase"})
            {:error "Missing value for --max-truncated-increase"})

          (= arg "--require-pass")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :require-pass? (parse-bool-safe v true)))
            {:error "Missing value for --require-pass"})

          :else
          {:error (str "Unknown argument: " arg)})))))

(defn- now-iso
  []
  (str (java.time.Instant/now)))

(defn- timestamp
  []
  (.format (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss'Z'")
           (java.time.ZonedDateTime/now java.time.ZoneOffset/UTC)))

(defn- read-json-file
  [path]
  (let [path' (trim-s path)]
    (when-not path'
      (throw (ex-info "Missing JSON file path."
                      {:error :benchmark-gate/missing-path})))
    (when-not (.exists (io/file path'))
      (throw (ex-info "JSON file does not exist."
                      {:error :benchmark-gate/file-not-found
                       :path path'})))
    (let [parsed (json/parse-string (slurp path') true)]
      (if (map? parsed)
        parsed
        (throw (ex-info "Expected JSON object in summary file."
                        {:error :benchmark-gate/invalid-summary-shape
                         :path path'
                         :type (type parsed)}))))))

(defn- first-number
  [xs]
  (first (filter number? xs)))

(defn extract-summary-metrics
  "Extracts normalized benchmark metrics from benchmark-live summary JSON map."
  [summary]
  (let [metrics (if (map? (:metrics summary)) (:metrics summary) {})]
    {:pass? (true? (:pass summary))
     :preset (keywordish (:preset summary))
     :interactive-p95-ms
     (first-number [(:text_respond_interactive_p95_ms metrics)
                    (:text_respond_p95_ms metrics)])
     :must-failed-rate-sla
     (first-number [(:must_failed_rate_sla metrics)
                    (:must_failed_rate_sla_p95 metrics)])
     :truncated-total
     (long (or (parse-long-safe (:text_truncated_total metrics))
               (parse-long-safe (:text_truncated_case_count metrics))
               0))
     :raw summary}))

(defn- safe-ratio
  [candidate baseline]
  (if (and (number? baseline) (pos? baseline))
    (/ (- candidate baseline) baseline)
    (if (and (number? candidate) (number? baseline) (<= candidate baseline))
      0.0
      Double/POSITIVE_INFINITY)))

(defn evaluate-gate
  "Evaluates candidate summary and optional baseline summary against release-gate rules.

  Returns deterministic report map."
  [candidate-summary baseline-summary cfg]
  (let [cfg' (merge default-opts (if (map? cfg) cfg {}))
        candidate (extract-summary-metrics candidate-summary)
        baseline (when (map? baseline-summary)
                   (extract-summary-metrics baseline-summary))
        require-pass? (true? (:require-pass? cfg'))
        checks (transient [])]
    (when require-pass?
      (conj! checks {:check :candidate/pass
                     :required true
                     :pass (:pass? candidate)
                     :actual (:pass? candidate)
                     :expected true}))
    (when (and baseline
               (number? (:interactive-p95-ms candidate))
               (number? (:interactive-p95-ms baseline)))
      (let [delta (- (:interactive-p95-ms candidate)
                     (:interactive-p95-ms baseline))
            ratio (safe-ratio (:interactive-p95-ms candidate)
                              (:interactive-p95-ms baseline))
            pass? (and (<= delta (:max-interactive-regress-ms cfg'))
                       (<= ratio (:max-interactive-regress-ratio cfg')))]
        (conj! checks {:check :regression/interactive-p95
                       :required true
                       :pass pass?
                       :candidate (:interactive-p95-ms candidate)
                       :baseline (:interactive-p95-ms baseline)
                       :delta delta
                       :delta-ratio ratio
                       :threshold-ms (:max-interactive-regress-ms cfg')
                       :threshold-ratio (:max-interactive-regress-ratio cfg')})))
    (when (and baseline
               (number? (:must-failed-rate-sla candidate))
               (number? (:must-failed-rate-sla baseline)))
      (let [delta (- (:must-failed-rate-sla candidate)
                     (:must-failed-rate-sla baseline))
            pass? (<= delta (:max-must-failed-regress cfg'))]
        (conj! checks {:check :regression/must-failed-rate-sla
                       :required true
                       :pass pass?
                       :candidate (:must-failed-rate-sla candidate)
                       :baseline (:must-failed-rate-sla baseline)
                       :delta delta
                       :threshold (:max-must-failed-regress cfg')})))
    (when baseline
      (let [delta (- (:truncated-total candidate)
                     (:truncated-total baseline))
            pass? (<= delta (:max-truncated-increase cfg'))]
        (conj! checks {:check :regression/truncated-total
                       :required true
                       :pass pass?
                       :candidate (:truncated-total candidate)
                       :baseline (:truncated-total baseline)
                       :delta delta
                       :threshold (:max-truncated-increase cfg')})))
    (let [checks' (persistent! checks)
          pass? (every? true? (map :pass checks'))]
      {:benchmark-gate/version 1
       :generated-at (now-iso)
       :pass? pass?
       :candidate {:pass? (:pass? candidate)
                   :preset (:preset candidate)
                   :interactive-p95-ms (:interactive-p95-ms candidate)
                   :must-failed-rate-sla (:must-failed-rate-sla candidate)
                   :truncated-total (:truncated-total candidate)}
       :baseline (when baseline
                   {:pass? (:pass? baseline)
                    :preset (:preset baseline)
                    :interactive-p95-ms (:interactive-p95-ms baseline)
                    :must-failed-rate-sla (:must-failed-rate-sla baseline)
                    :truncated-total (:truncated-total baseline)})
       :config {:require-pass? require-pass?
                :max-interactive-regress-ms (:max-interactive-regress-ms cfg')
                :max-interactive-regress-ratio (:max-interactive-regress-ratio cfg')
                :max-must-failed-regress (:max-must-failed-regress cfg')
                :max-truncated-increase (:max-truncated-increase cfg')}
       :checks checks'})))

(defn- default-report-path
  [candidate-summary-path]
  (let [f (io/file candidate-summary-path)
        parent (.getParentFile f)
        dir (if (some? parent) parent (io/file "."))]
    (str (io/file dir "gate-report.json"))))

(defn- md-path
  [json-path]
  (str/replace json-path #"\.json$" ".md"))

(defn- write-json!
  [path value]
  (io/make-parents (io/file path))
  (with-open [w (io/writer path)]
    (.write w (json/generate-string value {:pretty true})))
  path)

(defn- write-md!
  [path report]
  (let [checks (or (:checks report) [])
        lines (concat
               ["# Benchmark Gate Report"
                ""
                (str "- Generated at: " (:generated-at report))
                (str "- Overall pass: " (if (:pass? report) "YES" "NO"))
                ""]
               ["## Candidate"
                ""
                (str "- pass: " (pr-str (get-in report [:candidate :pass?])))
                (str "- interactive p95 [ms]: " (pr-str (get-in report [:candidate :interactive-p95-ms])))
                (str "- must-failed SLA: " (pr-str (get-in report [:candidate :must-failed-rate-sla])))
                (str "- truncated total: " (pr-str (get-in report [:candidate :truncated-total])))
                ""]
               (if (map? (:baseline report))
                 ["## Baseline"
                  ""
                  (str "- pass: " (pr-str (get-in report [:baseline :pass?])))
                  (str "- interactive p95 [ms]: " (pr-str (get-in report [:baseline :interactive-p95-ms])))
                  (str "- must-failed SLA: " (pr-str (get-in report [:baseline :must-failed-rate-sla])))
                  (str "- truncated total: " (pr-str (get-in report [:baseline :truncated-total])))
                  ""]
                 [])
               ["## Checks"
                ""]
               (if (seq checks)
                 (map (fn [c]
                        (str "- " (name (:check c)) ": "
                             (if (:pass c) "PASS" "FAIL")))
                      checks)
                 ["- no checks"]))]
    (io/make-parents (io/file path))
    (spit path (str (str/join "\n" lines) "\n"))
    path))

(defn- run-benchmark!
  [opts]
  (let [run-root (str "target/benchmarks-gate/" (timestamp) "/candidate")
        args (cond-> ["bin/benchmark-live" "--preset" (name (:preset opts)) "--runs" (str (:runs opts))]
               (some? (trim-s (:case-dir opts)))
               (into ["--case-dir" (trim-s (:case-dir opts))]))
        env (cond-> {"FERMENT_BENCH_OUT_DIR" run-root}
              (some? (trim-s (System/getenv "FERMENT_BENCH_URL")))
              (assoc "FERMENT_BENCH_URL" (System/getenv "FERMENT_BENCH_URL")))
        result (apply sh/sh (concat args [:env env]))
        exit (:exit result)]
    (when-not (zero? exit)
      (throw (ex-info "benchmark-live failed"
                      {:error :benchmark-gate/benchmark-run-failed
                       :exit exit
                       :out (:out result)
                       :err (:err result)
                       :run-root run-root})))
    (let [summary-path (str (io/file run-root "summary.json"))]
      (when-not (.exists (io/file summary-path))
        (throw (ex-info "benchmark-live summary.json missing after run."
                        {:error :benchmark-gate/missing-candidate-summary
                         :run-root run-root
                         :summary-path summary-path})))
      {:run-root run-root
       :summary-path summary-path
       :stdout (:out result)
       :stderr (:err result)})))

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
        (let [run-result (when (:run? opts)
                           (run-benchmark! opts))
              candidate-path (or (trim-s (:candidate opts))
                                 (:summary-path run-result))]
          (when-not candidate-path
            (throw (ex-info "Missing candidate summary path. Use --candidate or run without --no-run."
                            {:error :benchmark-gate/missing-candidate})))
          (let [candidate-summary (read-json-file candidate-path)
                baseline-path (trim-s (:baseline opts))
                baseline-summary (when baseline-path
                                   (read-json-file baseline-path))
                report (evaluate-gate candidate-summary baseline-summary opts)
                out-path (or (trim-s (:out opts))
                             (default-report-path candidate-path))
                out-md (md-path out-path)]
            (write-json! out-path report)
            (write-md! out-md report)
            (println "Benchmark gate report generated.")
            (println (str "  candidate: " candidate-path))
            (when baseline-path
              (println (str "  baseline:  " baseline-path)))
            (println (str "  report:    " out-path))
            (println (str "  report md: " out-md))
            (println (str "  pass:      " (:pass? report)))
            (if (:pass? report)
              (System/exit 0)
              (System/exit 3))))
        (catch clojure.lang.ExceptionInfo e
          (let [d (ex-data e)]
            (binding [*out* *err*]
              (println "Benchmark gate failed.")
              (println (.getMessage e))
              (when (map? d)
                (println (pr-str d))))
            (if (= :benchmark-gate/benchmark-run-failed (:error d))
              (System/exit 4)
              (System/exit 1))))
        (catch Throwable t
          (binding [*out* *err*]
            (println "Benchmark gate failed.")
            (println (.getMessage t)))
          (System/exit 1))))))
