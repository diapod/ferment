(ns

    ^{:doc    "Durable append-only collector for runtime training events."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.collector

  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])

  (:import (java.io BufferedWriter File)
           (java.nio.charset StandardCharsets)))

(defprotocol TrainingCollector
  (append! [collector event] "Appends one canonical `training.event/v1` map.")
  (flush! [collector] "Flushes pending buffered writes.")
  (close! [collector] "Closes collector resources.")
  (stats [collector] "Returns runtime collector stats map."))

(def ^:private default-store-type
  :fs-jsonl)

(def ^:private default-store-path
  "target/training/collector")

(def ^:private default-flush-policy
  :per-event)

(def ^:private default-max-file-size-bytes
  (* 8 1024 1024))

(def ^:private file-name-prefix
  "events-")

(def ^:private file-name-pattern
  #"^events-(\d{6})\.jsonl$")

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (some-> v trim-s keyword)
    :else nil))

(defn- parse-positive-long
  [v default]
  (let [n (cond
            (integer? v) (long v)
            (number? v) (long (Math/floor (double v)))
            (string? v) (try
                          (Long/parseLong (str/trim v))
                          (catch Throwable _ nil))
            :else nil)]
    (if (and (integer? n) (pos? n))
      n
      (long default))))

(defn- normalize-flush-policy
  [v]
  (let [policy (keywordish v)]
    (if (contains? #{:per-event :batch} policy)
      policy
      default-flush-policy)))

(defn normalize-config
  "Normalizes collector config branch.

  Output keys:
  - `:enabled?`
  - `:store/type`
  - `:store/path`
  - `:flush-policy`
  - `:max-file-size-bytes`"
  [cfg]
  (let [cfg' (if (map? cfg) cfg {})
        store-path (or (trim-s (:store/path cfg'))
                       default-store-path)
        store-type (or (keywordish (:store/type cfg'))
                       default-store-type)]
    {:enabled? (true? (:enabled? cfg'))
     :store/type store-type
     :store/path store-path
     :flush-policy (normalize-flush-policy (:flush-policy cfg'))
     :max-file-size-bytes (parse-positive-long
                           (:max-file-size-bytes cfg')
                           default-max-file-size-bytes)}))

(defn- parse-file-index
  [^File f]
  (when (instance? File f)
    (let [name (.getName f)]
      (when-let [[_ digits] (re-matches file-name-pattern name)]
        (try
          (Long/parseLong digits)
          (catch Throwable _ nil))))))

(defn- event-file-name
  [idx]
  (format "%s%06d.jsonl" file-name-prefix (long idx)))

(defn- collect-event-files
  [^File dir]
  (->> (or (.listFiles dir) (into-array File []))
       (keep (fn [^File f]
               (when (and (.isFile f)
                          (parse-file-index f))
                 {:index (parse-file-index f)
                  :file f
                  :size (long (.length f))})))
       (sort-by :index)
       vec))

(defn- parse-event-id-line
  [line]
  (when-let [s (trim-s line)]
    (try
      (let [m (json/parse-string s true)
            id (trim-s (:training.event/id m))]
        id)
      (catch Throwable _
        nil))))

(defn- load-seen-ids
  [files]
  (reduce (fn [acc {:keys [file]}]
            (try
              (with-open [r (io/reader ^File file)]
                (reduce (fn [acc' line]
                          (if-let [id (parse-event-id-line line)]
                            (conj acc' id)
                            acc'))
                        acc
                        (line-seq r)))
              (catch Throwable _
                acc)))
          #{}
          files))

(defn- ensure-dir!
  [path]
  (let [dir (io/file path)]
    (io/make-parents (io/file dir ".keep"))
    (.mkdirs dir)
    dir))

(defn- open-writer!
  [^File file]
  (io/writer file :append true))

(defn- next-writer-state
  [state next-index]
  (let [^File dir (:dir state)
        next-file (io/file dir (event-file-name next-index))]
    {:writer (open-writer! next-file)
     :current/index next-index
     :current/path (.getAbsolutePath next-file)
     :current/size-bytes (long (.length next-file))}))

(defn- init-state
  [cfg]
  (let [dir (ensure-dir! (:store/path cfg))
        files (collect-event-files dir)
        seen-ids (load-seen-ids files)
        last-file (last files)
        max-size (long (:max-file-size-bytes cfg))
        can-append? (and (map? last-file)
                         (< (long (:size last-file)) max-size))
        start-index (if can-append?
                      (long (:index last-file))
                      (inc (long (or (:index last-file) 0))))
        start-state {:cfg cfg
                     :dir dir
                     :seen-ids seen-ids
                     :appended 0
                     :duplicates 0
                     :rotations 0
                     :errors 0}]
    (if can-append?
      (merge start-state
             {:writer (open-writer! (:file last-file))
              :current/index (long (:index last-file))
              :current/path (.getAbsolutePath ^File (:file last-file))
              :current/size-bytes (long (:size last-file))})
      (merge start-state
             (next-writer-state start-state start-index)))))

(defn- close-writer!
  [writer]
  (when (instance? BufferedWriter writer)
    (.flush ^BufferedWriter writer)
    (.close ^BufferedWriter writer)))

(defn- maybe-rotate!
  [state event-bytes]
  (let [max-size (long (get-in state [:cfg :max-file-size-bytes]))
        current-size (long (or (:current/size-bytes state) 0))
        overflow? (and (pos? current-size)
                       (> (+ current-size (long event-bytes)) max-size))]
    (if-not overflow?
      state
      (do
        (close-writer! (:writer state))
        (let [next-index (inc (long (:current/index state)))
              next-state (next-writer-state state next-index)]
          (-> state
              (merge next-state)
              (update :rotations (fnil inc 0))))))))

(defrecord FsJsonlCollector [state]
  TrainingCollector
  (append! [_ event]
    (let [event-id (trim-s (:training.event/id event))]
      (cond
        (not (map? event))
        {:ok? false
         :error :training/collector-invalid-event}

        (nil? event-id)
        {:ok? false
         :error :training/collector-missing-event-id}

        :else
        (locking state
          (let [snapshot @state]
            (if (contains? (:seen-ids snapshot) event-id)
              (do
                (swap! state update :duplicates (fnil inc 0))
                {:ok? true
                 :duplicate? true
                 :training.event/id event-id})
              (try
                (let [line (str (json/generate-string event) "\n")
                      event-bytes (alength (.getBytes line StandardCharsets/UTF_8))
                      state1 (maybe-rotate! snapshot event-bytes)
                      writer (:writer state1)
                      flush-policy (get-in state1 [:cfg :flush-policy])]
                  (.write ^BufferedWriter writer line)
                  (when (= :per-event flush-policy)
                    (.flush ^BufferedWriter writer))
                  (let [state2 (-> state1
                                   (update :appended (fnil inc 0))
                                   (update :seen-ids conj event-id)
                                   (update :current/size-bytes
                                           (fnil #(+ (long %) (long event-bytes)) 0)))]
                    (reset! state state2)
                    {:ok? true
                     :training.event/id event-id
                     :duplicate? false
                     :path (:current/path state2)}))
                (catch Throwable t
                  (swap! state update :errors (fnil inc 0))
                  {:ok? false
                   :error :training/collector-append-failed
                   :training.event/id event-id
                   :message (.getMessage t)}))))))))
  (flush! [_]
    (locking state
      (when-some [writer (:writer @state)]
        (.flush ^BufferedWriter writer))
      nil))
  (close! [_]
    (locking state
      (close-writer! (:writer @state))
      (swap! state assoc :writer nil)
      nil))
  (stats [_]
    (let [snapshot @state]
      {:enabled? true
       :store/type (get-in snapshot [:cfg :store/type])
       :store/path (get-in snapshot [:cfg :store/path])
       :flush-policy (get-in snapshot [:cfg :flush-policy])
       :max-file-size-bytes (get-in snapshot [:cfg :max-file-size-bytes])
       :current/index (:current/index snapshot)
       :current/path (:current/path snapshot)
       :current/size-bytes (:current/size-bytes snapshot)
       :appended (:appended snapshot)
       :duplicates (:duplicates snapshot)
       :rotations (:rotations snapshot)
       :errors (:errors snapshot)
       :seen/count (count (:seen-ids snapshot))})))

(defn init-collector
  "Builds collector from normalized or raw config.
  Returns nil when collector is disabled."
  [cfg]
  (let [cfg' (normalize-config cfg)]
    (when (:enabled? cfg')
      (case (:store/type cfg')
        :fs-jsonl (->FsJsonlCollector (atom (init-state cfg')))
        nil))))
