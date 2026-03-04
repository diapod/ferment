(ns

    ^{:doc    "Deterministic dataset builder for training events (train/valid/test + manifest)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.dataset

  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ferment.training.events :as training-events]
            [ferment.training.export-events :as training-export])
  (:import (java.io File)
           (java.math BigInteger)
           (java.security MessageDigest)))

(def ^:private default-config
  {:in nil
   :out-dir "target/training/dataset"
   :train-task :meta-protocol
   :include-failed? false
   :target-format :sft-prompt-completion
   :split {:ratios {:train 0.8
                    :valid 0.1
                    :test 0.1}
           :seed 1337}
   :judge {:mode :rules-only}
   :redaction {:enabled? true}
   :idempotency {:enabled? true
                 :state-file ".dataset-state.json"
                 :source-checksum? true
                 :fail-on-config-change? false}})

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

(defn- parse-double-safe
  [v]
  (cond
    (number? v) (double v)
    (string? v) (try
                  (Double/parseDouble (str/trim v))
                  (catch Throwable _ nil))
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

(defn- normalize-ratios
  [m]
  (let [src (if (map? m) m {})
        train (or (parse-double-safe (:train src)) 0.8)
        valid (or (parse-double-safe (:valid src)) 0.1)
        test  (or (parse-double-safe (:test src)) 0.1)
        total (+ train valid test)]
    (if (or (<= train 0.0) (<= valid 0.0) (<= test 0.0) (<= total 0.0))
      {:train 0.8 :valid 0.1 :test 0.1}
      {:train (/ train total)
       :valid (/ valid total)
       :test (/ test total)})))

(defn normalize-config
  [cfg]
  (let [cfg' (merge default-config (if (map? cfg) cfg {}))
        split-src (if (map? (:split cfg'))
                    (:split cfg')
                    {})
        idempotency-src (if (map? (:idempotency cfg'))
                          (:idempotency cfg')
                          {})
        target-format (or (keywordish (:target-format cfg'))
                          :sft-prompt-completion)]
    (-> cfg'
        (assoc :target-format (if (contains? #{:sft-prompt-completion :messages :chatml}
                                             target-format)
                                target-format
                                :sft-prompt-completion))
        (assoc :split {:ratios (normalize-ratios (:ratios split-src))
                       :seed (or (parse-long-safe (:seed split-src)) 1337)})
        (assoc :idempotency
               {:enabled? (parse-bool-safe (:enabled? idempotency-src) true)
                :state-file (or (trim-s (:state-file idempotency-src))
                                ".dataset-state.json")
                :source-checksum? (parse-bool-safe (:source-checksum? idempotency-src) true)
                :fail-on-config-change? (parse-bool-safe (:fail-on-config-change? idempotency-src) false)}))))

(defn- parse-json-safe
  [s]
  (try
    (json/parse-string s true)
    (catch Throwable t
      (throw (ex-info "Cannot parse JSON input."
                      {:error :training/dataset-invalid-json
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
        (cond
          (vector? parsed) parsed
          (map? parsed) [parsed]
          :else (throw (ex-info "JSON input must be object or array."
                                {:error :training/dataset-invalid-shape
                                 :type (type parsed)}))))

      (str/starts-with? body' "{")
      (if (and (> (count lines) 1)
               (every? #(str/starts-with? % "{") lines))
        (mapv parse-json-safe lines)
        (let [parsed (parse-json-safe body')]
          (cond
            (vector? parsed) parsed
            (map? parsed) [parsed]
            :else (throw (ex-info "JSON input must be object or array."
                                  {:error :training/dataset-invalid-shape
                                   :type (type parsed)})))))

      :else
      (mapv parse-json-safe lines))))

(defn- split-input-spec
  [in-spec]
  (->> (str/split (or (trim-s in-spec) "") #",")
       (map trim-s)
       (remove nil?)
       vec))

(defn- json-input-file?
  [^File f]
  (let [name' (-> (.getName f) str/lower-case)]
    (or (str/ends-with? name' ".json")
        (str/ends-with? name' ".jsonl"))))

(defn- expand-input-item
  [item]
  (let [f (io/file item)]
    (cond
      (not (.exists f))
      (throw (ex-info "Input path does not exist."
                      {:error :training/dataset-missing-input-path
                       :path item}))

      (.isDirectory f)
      (let [children (or (.listFiles f) (make-array File 0))
            files (->> children
                       (filter #(.isFile ^File %))
                       (filter json-input-file?)
                       (sort-by #(.getName ^File %))
                       (map #(.getCanonicalPath ^File %))
                       vec)]
        (when (empty? files)
          (throw (ex-info "Input directory does not contain JSON/JSONL files."
                          {:error :training/dataset-empty-input-directory
                           :path item})))
        files)

      (.isFile f)
      [(.getCanonicalPath f)]

      :else
      (throw (ex-info "Unsupported input path kind."
                      {:error :training/dataset-invalid-input-path
                       :path item})))))

(defn- resolve-input-paths
  [in-spec]
  (let [items (split-input-spec in-spec)]
    (when (empty? items)
      (throw (ex-info "Missing required option --in PATH."
                      {:error :training/dataset-missing-input})))
    (->> items
         (mapcat expand-input-item)
         distinct
         sort
         vec)))

(defn- training-event?
  [m]
  (and (map? m)
       (integer? (:training.event/version m))
       (some? (trim-s (:training.event/id m)))))

(defn- canonicalize
  [v]
  (cond
    (map? v) (into (sorted-map)
                   (map (fn [[k value]]
                          [k (canonicalize value)]))
                   v)
    (set? v) (->> v
                  (map canonicalize)
                  (sort-by pr-str)
                  vec)
    (vector? v) (mapv canonicalize v)
    (sequential? v) (mapv canonicalize v)
    :else v))

(defn- sha256-hex
  [s]
  (let [^MessageDigest digest (MessageDigest/getInstance "SHA-256")
        _ (.update digest (.getBytes (str s) "UTF-8"))
        bytes (.digest digest)]
    (format "%064x" (BigInteger. 1 bytes))))

(defn- stable-hash
  [v]
  (sha256-hex (pr-str (canonicalize v))))

(defn- dataset-config-hash
  [cfg]
  (stable-hash {:split (:split cfg)
                :include-failed? (:include-failed? cfg)
                :target-format (:target-format cfg)
                :train-task (:train-task cfg)}))

(defn- row-hash
  [rows]
  (stable-hash rows))

(defn- event-recorded-at
  [event]
  (trim-s (:recorded-at event)))

(defn- time-window
  [events]
  (let [times (->> events
                   (keep event-recorded-at)
                   sort
                   vec)]
    (when (seq times)
      {:from (first times)
       :to (peek times)})))

(defn- dedup-events
  [events]
  (reduce (fn [acc event]
            (let [id (trim-s (:training.event/id event))]
              (if (and id (contains? acc id))
                acc
                (assoc acc id event))))
          (sorted-map)
          (sort-by :training.event/id events)))

(defn- bucket-01
  [seed key]
  (let [hex (sha256-hex (str seed "|" key))
        upper (subs hex 0 15)
        n (BigInteger. upper 16)
        max-n (BigInteger. "FFFFFFFFFFFFFFF" 16)]
    (/ (.doubleValue n) (.doubleValue max-n))))

(defn- choose-split
  [ratios bucket]
  (let [train-th (:train ratios)
        valid-th (+ train-th (:valid ratios))]
    (cond
      (< bucket train-th) :train
      (< bucket valid-th) :valid
      :else :test)))

(defn split-events
  "Deterministically assigns events to train/valid/test by stable event id and seed."
  [events split-cfg]
  (let [ratios (normalize-ratios (:ratios split-cfg))
        seed (or (parse-long-safe (:seed split-cfg)) 1337)]
    (reduce (fn [acc event]
              (let [event-id (trim-s (:training.event/id event))
                    bucket (bucket-01 seed (or event-id "missing-id"))
                    split (choose-split ratios bucket)]
                (update acc split conj event)))
            {:train [] :valid [] :test []}
            (sort-by :training.event/id events))))

(defn- append-jsonl!
  [path rows]
  (when (seq rows)
    (io/make-parents (io/file path))
    (with-open [w (io/writer path :append true)]
      (doseq [row rows]
        (.write w (json/generate-string row))
        (.write w "\n"))))
  path)

(defn- read-jsonl-file
  [path]
  (if-not (.exists (io/file path))
    []
    (->> (str/split-lines (slurp path))
         (map trim-s)
         (remove nil?)
         (mapv parse-json-safe))))

(defn- read-json-file
  [path]
  (if-not (.exists (io/file path))
    nil
    (parse-json-safe (slurp path))))

(defn- read-existing-events
  [events-path]
  (->> (read-jsonl-file events-path)
       (filter training-event?)
       vec))

(defn- default-state
  []
  {:dataset/state-version 1
   :dataset/config-hash nil
   :dataset/snapshot-id nil
   :sources {}})

(defn- read-state
  [path]
  (if-not (.exists (io/file path))
    (default-state)
    (try
      (let [parsed (json/parse-string (slurp path) true)]
        (if (map? parsed)
          parsed
          (default-state)))
      (catch Throwable _
        (default-state)))))

(declare write-json!)

(defn- write-state!
  [path state]
  (write-json! path state))

(defn- file-checksum
  [path]
  (sha256-hex (slurp path)))

(defn- source-fingerprint
  [path source-checksum?]
  (let [f (io/file path)]
    {:path (.getCanonicalPath f)
     :name (.getName f)
     :size-bytes (.length f)
     :mtime-ms (.lastModified f)
     :checksum (when source-checksum?
                 (file-checksum path))}))

(defn- source-unchanged?
  [state source]
  (let [old (get-in state [:sources (:path source)])
        old-fp (select-keys (if (map? old) old {})
                            [:size-bytes :mtime-ms :checksum])
        new-fp (select-keys source [:size-bytes :mtime-ms :checksum])]
    (= old-fp new-fp)))

(defn- output-paths
  [out-dir]
  {:events (str (io/file out-dir "events-v1.jsonl"))
   :train (str (io/file out-dir "train.jsonl"))
   :valid (str (io/file out-dir "valid.jsonl"))
   :test (str (io/file out-dir "test.jsonl"))
   :manifest (str (io/file out-dir "manifest.json"))})

(defn- outputs-ready?
  [paths]
  (every? #(-> % io/file .exists) (vals paths)))

(defn- state-entry-for-source
  [source]
  (assoc source :processed-at (str (java.time.Instant/now))))

(defn- update-state
  [state sources cfg manifest]
  (let [source-map (into {}
                         (map (fn [source]
                                [(:path source)
                                 (state-entry-for-source source)]))
                         sources)]
    (-> state
        (assoc :dataset/state-version 1)
        (assoc :dataset/config-hash (dataset-config-hash cfg))
        (assoc :dataset/snapshot-id (:snapshot/id manifest))
        (assoc :dataset/updated-at (str (java.time.Instant/now)))
        (update :sources #(merge (if (map? %) % {}) source-map)))))

(defn- write-jsonl!
  [path rows]
  (io/make-parents (io/file path))
  (with-open [w (io/writer path)]
    (doseq [row rows]
      (.write w (json/generate-string row))
      (.write w "\n")))
  path)

(defn- write-json!
  [path value]
  (io/make-parents (io/file path))
  (with-open [w (io/writer path)]
    (.write w (json/generate-string value {:pretty true})))
  path)

(defn build-dataset
  "Builds deterministic dataset from canonical events.

  Returns map with split rows and manifest."
  [events cfg]
  (let [cfg' (normalize-config cfg)
        include-failed? (true? (:include-failed? cfg'))
        target-format (:target-format cfg')
        dedup-map (dedup-events events)
        deduped-events (vec (vals dedup-map))
        splits (split-events deduped-events (:split cfg'))
        split-rows (into {}
                         (map (fn [[split xs]]
                                [split (training-export/events->train-rows xs
                                                                           {:include-failed? include-failed?
                                                                            :target-format target-format})]))
                         splits)
        manifest-base {:dataset/version 1
                       :config {:split (:split cfg')
                                :include-failed? include-failed?
                                :target-format target-format
                                :train-task (:train-task cfg')}
                       :input {:events/raw (count events)
                               :events/deduped (count deduped-events)
                               :events/dropped-duplicate (- (count events) (count deduped-events))}
                       :counts {:train (count (:train split-rows))
                                :valid (count (:valid split-rows))
                                :test (count (:test split-rows))}
                       :hashes {:events (row-hash deduped-events)
                                :train (row-hash (:train split-rows))
                                :valid (row-hash (:valid split-rows))
                                :test (row-hash (:test split-rows))}
                       :time/window (or (time-window deduped-events)
                                        {:from nil :to nil})
                       :filters {:include-failed? include-failed?}}
        manifest {:dataset/version 1
                  :snapshot/id (stable-hash manifest-base)
                  :config (:config manifest-base)
                  :input (:input manifest-base)
                  :counts (:counts manifest-base)
                  :hashes (:hashes manifest-base)
                  :time/window (:time/window manifest-base)
                  :filters (:filters manifest-base)}]
    {:events deduped-events
     :splits split-rows
     :manifest manifest
     :config cfg'}))

(defn- records->events
  [records cfg]
  (if (every? training-event? records)
    records
    (training-events/replay-records->events
     records
     {:train-task (:train-task cfg)
      :judge (:judge cfg)
      :redaction (:redaction cfg)})))

(defn build-dataset-from-input!
  "Reads input records (events or replay), builds deterministic dataset and writes artifacts.

  Idempotent mode (enabled by default):
  - skips unchanged source files by fingerprint,
  - avoids re-exporting already known training.event ids,
  - appends only new rows when possible."
  [cfg]
  (let [cfg' (normalize-config cfg)
        in-spec (trim-s (:in cfg'))
        out-dir (:out-dir cfg')
        paths (output-paths out-dir)
        idempotency (:idempotency cfg')
        idempotent? (true? (:enabled? idempotency))
        state-path (str (io/file out-dir (:state-file idempotency)))
        state (if idempotent?
                (read-state state-path)
                (default-state))
        config-hash (dataset-config-hash cfg')
        fail-on-config-change? (true? (:fail-on-config-change? idempotency))
        config-changed? (and idempotent?
                             (some? (:dataset/config-hash state))
                             (not= (:dataset/config-hash state) config-hash))
        input-paths (resolve-input-paths in-spec)
        source-fingerprints (mapv #(source-fingerprint % (true? (:source-checksum? idempotency)))
                                  input-paths)
        changed-sources (if idempotent?
                          (->> source-fingerprints
                               (remove #(source-unchanged? state %))
                               vec)
                          source-fingerprints)
        outputs-ready? (outputs-ready? paths)
        unchanged-sources? (and idempotent?
                                outputs-ready?
                                (not config-changed?)
                                (seq source-fingerprints)
                                (empty? changed-sources))]
    (when (and fail-on-config-change? config-changed?)
      (throw (ex-info "Dataset config changed and fail-on-config-change is enabled."
                      {:error :training/dataset-config-changed
                       :mode/reason :config-changed
                       :dataset/config-hash/previous (:dataset/config-hash state)
                       :dataset/config-hash/current config-hash
                       :state-file state-path})))
    (if unchanged-sources?
      (let [manifest (or (read-json-file (:manifest paths)) {})]
        {:ok? true
         :skipped? true
         :skip/reason :idempotency/sources-unchanged
         :input/files (count input-paths)
         :input/count 0
         :events/new-count 0
         :events/count (or (get-in manifest [:input :events/deduped]) 0)
         :snapshot/id (:snapshot/id manifest)
         :out/events (:events paths)
         :out/train (:train paths)
         :out/valid (:valid paths)
         :out/test (:test paths)
         :out/manifest (:manifest paths)
         :manifest manifest})
      (let [read-paths (cond
                         (not idempotent?) input-paths
                         (not outputs-ready?) input-paths
                         config-changed? (mapv :path changed-sources)
                         :else (mapv :path changed-sources))
            records (->> read-paths
                         (mapcat read-input-records)
                         vec)
            input-events (if (seq records)
                           (records->events records cfg')
                           [])
            existing-events (if (and idempotent? outputs-ready?)
                              (read-existing-events (:events paths))
                              [])
            existing-map (dedup-events existing-events)
            existing-events' (vec (vals existing-map))
            existing-ids (set (keys existing-map))
            input-map (dedup-events input-events)
            input-events' (vec (vals input-map))
            new-events (->> input-events'
                            (remove #(contains? existing-ids (trim-s (:training.event/id %))))
                            vec)
            combined-events (vec (vals (dedup-events (concat existing-events' new-events))))
            rebuild-full? (or (not idempotent?)
                              (not outputs-ready?)
                              config-changed?)
            mode-reason (cond
                          (not idempotent?) :idempotency/disabled
                          (not outputs-ready?) :outputs/missing
                          config-changed? :config-changed
                          :else :new-events)
            no-new-events? (and idempotent?
                                outputs-ready?
                                (not config-changed?)
                                (empty? new-events))
            {:keys [splits manifest]} (build-dataset combined-events cfg')
            state' (if idempotent?
                     (update-state state source-fingerprints cfg' manifest)
                     state)]
        (if no-new-events?
          (do
            (when idempotent?
              (write-state! state-path state'))
            {:ok? true
             :skipped? true
             :skip/reason :idempotency/no-new-events
             :input/files (count input-paths)
             :input/count (count records)
             :events/new-count 0
             :events/count (count combined-events)
             :snapshot/id (:snapshot/id manifest)
             :out/events (:events paths)
             :out/train (:train paths)
             :out/valid (:valid paths)
             :out/test (:test paths)
             :out/manifest (:manifest paths)
             :manifest manifest})
          (do
            (if rebuild-full?
              (do
                (write-jsonl! (:events paths) combined-events)
                (write-jsonl! (:train paths) (:train splits))
                (write-jsonl! (:valid paths) (:valid splits))
                (write-jsonl! (:test paths) (:test splits)))
              (let [new-splits (split-events new-events (:split cfg'))
                    include-failed? (true? (:include-failed? cfg'))
                    target-format (:target-format cfg')
                    new-rows (into {}
                                   (map (fn [[split events]]
                                          [split (training-export/events->train-rows
                                                  events
                                                  {:include-failed? include-failed?
                                                   :target-format target-format})]))
                                   new-splits)]
                (append-jsonl! (:events paths) new-events)
                (append-jsonl! (:train paths) (:train new-rows))
                (append-jsonl! (:valid paths) (:valid new-rows))
                (append-jsonl! (:test paths) (:test new-rows))))
            (write-json! (:manifest paths) manifest)
            (when idempotent?
              (write-state! state-path state'))
            {:ok? true
             :skipped? false
             :mode (if rebuild-full? :full-rebuild :incremental-append)
             :mode/reason mode-reason
             :input/files (count input-paths)
             :input/count (count records)
             :events/new-count (count new-events)
             :events/count (count combined-events)
             :snapshot/id (:snapshot/id manifest)
             :out/events (:events paths)
             :out/train (:train paths)
             :out/valid (:valid paths)
             :out/test (:test paths)
             :out/manifest (:manifest paths)
             :manifest manifest}))))))

(defn- usage
  []
  (str/join
   \newline
   ["Usage: bin/build-training-dataset --in PATH [options]"
    ""
    "Required:"
    "  --in PATH                Input file/dir/list (JSON/JSONL events or replay records)."
    ""
   "Options:"
    "  --out-dir PATH           Output directory (default: target/training/dataset)"
    "  --target-format FORMAT   :sft-prompt-completion (default), :messages, :chatml"
    "  --split-seed N           Deterministic split seed (default: 1337)"
    "  --train-ratio X          Train ratio (default: 0.8)"
    "  --valid-ratio X          Valid ratio (default: 0.1)"
    "  --test-ratio X           Test ratio (default: 0.1)"
    "  --include-failed         Include failed attempts in trainer rows"
    "  --train-task KEYWORD     Label for training task (default: :meta-protocol)"
    "  --state-file PATH        Idempotency state file under --out-dir (default: .dataset-state.json)"
    "  --no-source-checksum     Fingerprint source files without content checksum"
    "  --fail-on-config-change  Stop with error when dataset config hash changed"
    "  --no-idempotency         Disable idempotent mode (always rebuild from input)"
    "  -h, --help               Show this help"]))

(defn- parse-args
  [argv]
  (loop [args argv
         opts default-config]
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

          (= arg "--out-dir")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :out-dir v))
            {:error "Missing value for --out-dir"})

          (= arg "--target-format")
          (if-let [v (first more)]
            (let [fmt (or (keywordish v) :sft-prompt-completion)]
              (if (contains? #{:sft-prompt-completion :messages :chatml} fmt)
                (recur (rest more) (assoc opts :target-format fmt))
                {:error (str "Unsupported --target-format: " v)}))
            {:error "Missing value for --target-format"})

          (= arg "--split-seed")
          (if-let [v (first more)]
            (if-let [n (parse-long-safe v)]
              (recur (rest more) (assoc-in opts [:split :seed] n))
              {:error "Invalid value for --split-seed"})
            {:error "Missing value for --split-seed"})

          (= arg "--train-ratio")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc-in opts [:split :ratios :train] n))
              {:error "Invalid value for --train-ratio"})
            {:error "Missing value for --train-ratio"})

          (= arg "--valid-ratio")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc-in opts [:split :ratios :valid] n))
              {:error "Invalid value for --valid-ratio"})
            {:error "Missing value for --valid-ratio"})

          (= arg "--test-ratio")
          (if-let [v (first more)]
            (if-let [n (parse-double-safe v)]
              (recur (rest more) (assoc-in opts [:split :ratios :test] n))
              {:error "Invalid value for --test-ratio"})
            {:error "Missing value for --test-ratio"})

          (= arg "--include-failed")
          (recur more (assoc opts :include-failed? true))

          (= arg "--train-task")
          (if-let [v (first more)]
            (recur (rest more) (assoc opts :train-task (or (keywordish v) :meta-protocol)))
            {:error "Missing value for --train-task"})

          (= arg "--state-file")
          (if-let [v (first more)]
            (recur (rest more) (assoc-in opts [:idempotency :state-file] v))
            {:error "Missing value for --state-file"})

          (= arg "--no-source-checksum")
          (recur more (assoc-in opts [:idempotency :source-checksum?] false))

          (= arg "--fail-on-config-change")
          (recur more (assoc-in opts [:idempotency :fail-on-config-change?] true))

          (= arg "--no-idempotency")
          (recur more (assoc-in opts [:idempotency :enabled?] false))

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
        (let [result (build-dataset-from-input! opts)]
          (if (:skipped? result)
            (println "Dataset build skipped (idempotent).")
            (println "Dataset build completed."))
          (println (str "  input records: " (:input/count result)))
          (println (str "  events:        " (:events/count result)))
          (println (str "  new events:    " (:events/new-count result)))
          (when (:skip/reason result)
            (println (str "  skip reason:   " (:skip/reason result))))
          (when (:mode result)
            (println (str "  write mode:    " (:mode result))))
          (when (:mode/reason result)
            (println (str "  mode reason:   " (:mode/reason result))))
          (println (str "  snapshot id:   " (:snapshot/id result)))
          (println (str "  train file:    " (:out/train result)))
          (println (str "  valid file:    " (:out/valid result)))
          (println (str "  test file:     " (:out/test result)))
          (println (str "  manifest:      " (:out/manifest result)))
          (System/exit 0))
        (catch Throwable t
          (binding [*out* *err*]
            (println "Dataset build failed.")
            (println (.getMessage t))
            (when-let [d (ex-data t)]
              (println (pr-str d))))
          (System/exit 1))))))
