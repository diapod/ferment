(ns ferment.training.collector-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [ferment.training.collector :as collector]))

(defn- temp-dir!
  []
  (-> (java.nio.file.Files/createTempDirectory
       "ferment-training-collector-test-"
       (make-array java.nio.file.attribute.FileAttribute 0))
      (.toFile)))

(defn- list-event-files
  [dir]
  (->> (or (.listFiles (io/file dir)) (into-array java.io.File []))
       (filter #(.isFile ^java.io.File %))
       (filter #(re-matches #"events-\d{6}\.jsonl" (.getName ^java.io.File %)))
       (sort-by #(.getName ^java.io.File %))
       vec))

(defn- read-events
  [dir]
  (->> (list-event-files dir)
       (mapcat (fn [^java.io.File f]
                 (with-open [r (io/reader f)]
                   (doall
                    (for [line (line-seq r)
                          :let [s (some-> line str clojure.string/trim)]
                          :when (seq s)]
                      (json/parse-string s true))))))
       vec))

(defn- training-event
  [id text]
  {:training.event/version 1
   :training.event/id id
   :training.event/type :call-attempt
   :recorded-at "2026-03-04T00:00:00Z"
   :source {:trace/id "trace-1"
            :request/id "req-1"
            :transcript/index 0}
   :call {:op :call
          :intent :text/respond
          :cap/id :llm/voice
          :attempt 1
          :candidate-index 0
          :result/type :value
          :out {:text text}}
   :labels {:accepted? true
            :train/task :meta-protocol}})

(deftest collector-appends-across-restart
  (testing "fs-jsonl collector keeps append-only contract across restart."
    (let [dir (.getAbsolutePath (temp-dir!))
          cfg {:enabled? true
               :store/type :fs-jsonl
               :store/path dir
               :flush-policy :per-event
               :max-file-size-bytes 8192}
          c1 (collector/init-collector cfg)]
      (is (= true (:ok? (collector/append! c1 (training-event "e-1" "one")))))
      (is (= true (:ok? (collector/append! c1 (training-event "e-2" "two")))))
      (collector/close! c1)
      (let [c2 (collector/init-collector cfg)]
        (is (= true (:ok? (collector/append! c2 (training-event "e-3" "three")))))
        (collector/close! c2))
      (let [events (read-events dir)]
        (is (= 3 (count events)))
        (is (= #{"e-1" "e-2" "e-3"}
               (set (map :training.event/id events))))))))

(deftest collector-rotates-files-by-size
  (testing "collector rotates deterministic events-XXXXXX.jsonl files when max-size is exceeded."
    (let [dir (.getAbsolutePath (temp-dir!))
          cfg {:enabled? true
               :store/type :fs-jsonl
               :store/path dir
               :flush-policy :per-event
               :max-file-size-bytes 220}
          c (collector/init-collector cfg)]
      (doseq [idx (range 1 7)]
        (let [event-id (str "rot-" idx)
              text (str "payload-" idx "-" (apply str (repeat 70 "x")))]
          (is (= true (:ok? (collector/append! c (training-event event-id text)))))))
      (let [stats (collector/stats c)]
        (collector/close! c)
        (is (>= (count (list-event-files dir)) 2))
        (is (pos? (long (:rotations stats))))))))

(deftest collector-deduplicates-by-training-event-id
  (testing "collector skips duplicate training.event/id and keeps one persisted row."
    (let [dir (.getAbsolutePath (temp-dir!))
          cfg {:enabled? true
               :store/type :fs-jsonl
               :store/path dir
               :flush-policy :per-event
               :max-file-size-bytes 4096}
          c (collector/init-collector cfg)
          event (training-event "dup-1" "same")]
      (is (= {:ok? true :duplicate? false}
             (select-keys (collector/append! c event) [:ok? :duplicate?])))
      (is (= {:ok? true :duplicate? true}
             (select-keys (collector/append! c event) [:ok? :duplicate?])))
      (collector/close! c)
      (let [events (read-events dir)]
        (is (= 1 (count events)))
        (is (= "dup-1" (:training.event/id (first events))))))))
