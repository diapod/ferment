(ns ferment.training.dataset-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ferment.training.dataset :as dataset])
  (:import (java.math BigInteger)
           (java.nio.file Files)
           (java.security MessageDigest)))

(defn- sample-event
  [n]
  {:training.event/version 1
   :training.event/id (format "trace-%02d#call-0000#a1" n)
   :recorded-at (format "2026-03-%02dT10:00:00Z" (inc n))
   :source {:trace/id (format "trace-%02d" n)
            :request/id (format "req-%02d" n)}
   :request {:resolved {:task {:intent :text/respond
                               :requires {:out-schema :res/text}}}}
   :routing {:mode :meta-decider}
   :policy {:snapshot-id "policy-1"}
   :call {:intent :text/respond
          :cap/id :llm/voice
          :attempt 1
          :candidate-index 0
          :input {:prompt (str "Pytanie " n)}
          :result/type :value
          :out {:text (str "Odpowiedz " n)}}
   :response {:status 200
              :outcome :ok}
   :timing {:call/latency-ms (+ 10.0 n)}
   :labels {:accepted? true}})

(defn- sample-replay
  [n]
  (let [trace-id (format "replay-%02d" n)
        req-id (format "replay-req-%02d" n)
        prompt (str "Wyjasnij ACID " n)]
    {:trace/id trace-id
     :recorded-at (format "2026-03-%02dT10:00:00Z" (inc n))
     :request {:payload {:trace {:id trace-id}
                         :task {:intent :text/respond}
                         :input {:prompt prompt
                                 :email "john@example.com"
                                 :token "abc-secret"}}
               :prepared {:trace {:id trace-id}
                          :request/id req-id
                          :task {:intent :text/respond
                                 :requires {:out-schema :res/text}}
                          :input {:prompt prompt
                                  :email "john@example.com"
                                  :token "abc-secret"}}
               :resolved {:trace {:id trace-id}
                          :request/id req-id
                          :task {:intent :text/respond
                                 :requires {:out-schema :res/text}}
                          :input {:prompt prompt
                                  :email "john@example.com"
                                  :token "abc-secret"}}}
     :routing {:mode :meta-decider
               :routed? true
               :cap/decision {:cap/id :llm/voice}}
     :policy {:snapshot-id (str "policy-" n)}
     :response {:status 200
                :outcome :ok
                :body {:result {:type :value
                                :plan/run {:transcript [{:op :call
                                                         :intent :text/respond
                                                         :cap/id :llm/voice
                                                         :as :voice-primary
                                                         :attempt 1
                                                         :candidate-index 0
                                                         :input {:prompt prompt
                                                                 :authorization "Bearer top.secret"}
                                                         :result/type :value
                                                         :out {:text (str "ACID " n " kontakt: john@example.com")}
                                                         :latency-ms (+ 30.0 n)}]}}}}
     :diagnostics {:execution-path {:intent :text/respond
                                    :selected-cap/id :llm/voice}}
     :timing {:elapsed-ms (+ 100.0 n)}}))

(defn- sha256-hex
  [s]
  (let [^MessageDigest digest (MessageDigest/getInstance "SHA-256")
        _ (.update digest (.getBytes (str s) "UTF-8"))
        bytes (.digest digest)]
    (format "%064x" (BigInteger. 1 bytes))))

(defn- file-hash
  [path]
  (sha256-hex (slurp path)))

(defn- create-temp-dir
  [prefix]
  (-> (Files/createTempDirectory prefix (make-array java.nio.file.attribute.FileAttribute 0))
      str))

(defn- parse-jsonl
  [path]
  (->> (str/split-lines (slurp path))
       (remove str/blank?)
       (mapv #(json/parse-string % true))))

(deftest build-dataset-is-deterministic-for-identical-input
  (testing "Repeated build over identical events yields identical split rows and manifest snapshot."
    (let [events (mapv sample-event (range 1 13))
          cfg {:split {:seed 20260301
                       :ratios {:train 0.7 :valid 0.2 :test 0.1}}
               :target-format :sft-prompt-completion}
          build-a (dataset/build-dataset events cfg)
          build-b (dataset/build-dataset events cfg)]
      (is (= (:manifest build-a) (:manifest build-b)))
      (is (= (:splits build-a) (:splits build-b)))
      (is (= (get-in build-a [:manifest :snapshot/id])
             (get-in build-b [:manifest :snapshot/id])))
      (is (= {:from "2026-03-02T10:00:00Z"
              :to "2026-03-13T10:00:00Z"}
             (get-in build-a [:manifest :time/window]))))))

(deftest split-events-are-disjoint-and-cover-all-event-ids
  (testing "Stable split assignment keeps each event id in exactly one split."
    (let [events (mapv sample-event (range 1 40))
          splits (dataset/split-events events {:seed 42
                                               :ratios {:train 0.8 :valid 0.1 :test 0.1}})
          ids-all (set (map :training.event/id events))
          ids-train (set (map :training.event/id (:train splits)))
          ids-valid (set (map :training.event/id (:valid splits)))
          ids-test (set (map :training.event/id (:test splits)))]
      (is (empty? (set/intersection ids-train ids-valid)))
      (is (empty? (set/intersection ids-train ids-test)))
      (is (empty? (set/intersection ids-valid ids-test)))
      (is (= ids-all (set/union ids-train ids-valid ids-test))))))

(deftest build-dataset-from-input-produces-stable-manifest-and-file-hashes
  (testing "Two builds from the same input file produce identical manifest and artifact hashes."
    (let [events (mapv sample-event (range 1 16))
          input-dir (create-temp-dir "ferment-dataset-in-")
          out-a (create-temp-dir "ferment-dataset-out-a-")
          out-b (create-temp-dir "ferment-dataset-out-b-")
          in-path (str (io/file input-dir "events.jsonl"))
          _ (spit in-path
                  (str/join
                   "\n"
                   (map #(json/generate-string %)
                        events)))
          res-a (dataset/build-dataset-from-input! {:in in-path
                                                    :out-dir out-a
                                                    :target-format :messages
                                                    :split {:seed 20260301
                                                            :ratios {:train 0.75 :valid 0.15 :test 0.10}}})
          res-b (dataset/build-dataset-from-input! {:in in-path
                                                    :out-dir out-b
                                                    :target-format :messages
                                                    :split {:seed 20260301
                                                            :ratios {:train 0.75 :valid 0.15 :test 0.10}}})
          manifest-a (json/parse-string (slurp (:out/manifest res-a)) true)
          manifest-b (json/parse-string (slurp (:out/manifest res-b)) true)]
      (is (= manifest-a manifest-b))
      (is (= (file-hash (:out/events res-a)) (file-hash (:out/events res-b))))
      (is (= (file-hash (:out/train res-a)) (file-hash (:out/train res-b))))
      (is (= (file-hash (:out/valid res-a)) (file-hash (:out/valid res-b))))
      (is (= (file-hash (:out/test res-a)) (file-hash (:out/test res-b))))
      (is (every? #(contains? % :messages)
                  (parse-jsonl (:out/train res-a)))))))

(deftest build-dataset-from-input-is-idempotent-for-unchanged-source
  (testing "Second run on unchanged source and same out-dir is skipped and does not rewrite artifacts."
    (let [events (mapv sample-event (range 1 8))
          input-dir (create-temp-dir "ferment-dataset-idem-in-")
          out-dir (create-temp-dir "ferment-dataset-idem-out-")
          in-path (str (io/file input-dir "events.jsonl"))
          _ (spit in-path
                  (str/join
                   "\n"
                   (map #(json/generate-string %)
                        events)))
          res-a (dataset/build-dataset-from-input! {:in in-path
                                                    :out-dir out-dir
                                                    :target-format :sft-prompt-completion})
          events-hash-a (file-hash (:out/events res-a))
          train-hash-a (file-hash (:out/train res-a))
          res-b (dataset/build-dataset-from-input! {:in in-path
                                                    :out-dir out-dir
                                                    :target-format :sft-prompt-completion})]
      (is (= false (:skipped? res-a)))
      (is (= true (:skipped? res-b)))
      (is (contains? #{:idempotency/sources-unchanged
                       :idempotency/no-new-events}
                     (:skip/reason res-b)))
      (is (= events-hash-a (file-hash (:out/events res-b))))
      (is (= train-hash-a (file-hash (:out/train res-b)))))))

(deftest build-dataset-from-input-does-not-duplicate-existing-events
  (testing "Incremental import appends only unseen training.event ids."
    (let [events-a (mapv sample-event (range 1 6))
          events-b (mapv sample-event (range 4 9))
          input-dir (create-temp-dir "ferment-dataset-overlap-in-")
          out-dir (create-temp-dir "ferment-dataset-overlap-out-")
          in-a (str (io/file input-dir "events-a.jsonl"))
          in-b (str (io/file input-dir "events-b.jsonl"))
          _ (spit in-a (str/join "\n" (map #(json/generate-string %) events-a)))
          _ (spit in-b (str/join "\n" (map #(json/generate-string %) events-b)))
          res-a (dataset/build-dataset-from-input! {:in in-a
                                                    :out-dir out-dir})
          res-b (dataset/build-dataset-from-input! {:in in-b
                                                    :out-dir out-dir})
          events-jsonl (parse-jsonl (:out/events res-b))
          all-ids (map :training.event/id events-jsonl)]
      (is (= false (:skipped? res-a)))
      (is (= false (:skipped? res-b)))
      (is (= :incremental-append (:mode res-b)))
      (is (= 3 (:events/new-count res-b)))
      (is (= 8 (count events-jsonl)))
      (is (= 8 (count (set all-ids)))))))

(deftest build-dataset-from-input-rebuilds-when-config-changes
  (testing "Config change forces full rebuild even when source files are unchanged."
    (let [events (mapv sample-event (range 1 7))
          input-dir (create-temp-dir "ferment-dataset-config-in-")
          out-dir (create-temp-dir "ferment-dataset-config-out-")
          in-path (str (io/file input-dir "events.jsonl"))
          _ (spit in-path
                  (str/join
                   "\n"
                   (map #(json/generate-string %)
                        events)))
          res-a (dataset/build-dataset-from-input! {:in in-path
                                                    :out-dir out-dir
                                                    :split {:seed 11
                                                            :ratios {:train 0.8 :valid 0.1 :test 0.1}}})
          res-b (dataset/build-dataset-from-input! {:in in-path
                                                    :out-dir out-dir
                                                    :split {:seed 12
                                                            :ratios {:train 0.8 :valid 0.1 :test 0.1}}})]
      (is (= false (:skipped? res-a)))
      (is (= false (:skipped? res-b)))
      (is (= :full-rebuild (:mode res-b)))
      (is (= :config-changed (:mode/reason res-b)))
      (is (= 0 (:events/new-count res-b)))
      (is (= (count events) (:events/count res-b))))))

(deftest build-dataset-from-input-fails-on-config-change-when-guard-enabled
  (testing "Optional guard aborts build when config hash changes."
    (let [events (mapv sample-event (range 1 7))
          input-dir (create-temp-dir "ferment-dataset-guard-in-")
          out-dir (create-temp-dir "ferment-dataset-guard-out-")
          in-path (str (io/file input-dir "events.jsonl"))
          _ (spit in-path
                  (str/join
                   "\n"
                   (map #(json/generate-string %)
                        events)))
          _ (dataset/build-dataset-from-input! {:in in-path
                                                :out-dir out-dir
                                                :split {:seed 11
                                                        :ratios {:train 0.8 :valid 0.1 :test 0.1}}})
          ex (try
               (dataset/build-dataset-from-input! {:in in-path
                                                   :out-dir out-dir
                                                   :split {:seed 12
                                                           :ratios {:train 0.8 :valid 0.1 :test 0.1}}
                                                   :idempotency {:fail-on-config-change? true}})
               nil
               (catch clojure.lang.ExceptionInfo e
                 e))]
      (is (some? ex))
      (is (= :training/dataset-config-changed
             (:error (ex-data ex))))
      (is (= :config-changed
             (:mode/reason (ex-data ex)))))))

(deftest build-dataset-from-replay-input-is-json-serializable
  (testing "Replay input with default redaction produces JSON-serializable dataset artifacts."
    (let [records (mapv sample-replay (range 1 4))
          input-dir (create-temp-dir "ferment-dataset-replay-in-")
          out-dir (create-temp-dir "ferment-dataset-replay-out-")
          in-path (str (io/file input-dir "replay.jsonl"))
          _ (spit in-path
                  (str/join
                   "\n"
                   (map #(json/generate-string %)
                        records)))
          res (dataset/build-dataset-from-input! {:in in-path
                                                  :out-dir out-dir
                                                  :target-format :messages})
          events-jsonl (parse-jsonl (:out/events res))]
      (is (= false (:skipped? res)))
      (is (= 3 (:events/count res)))
      (is (= 3 (count events-jsonl)))
      (is (every? string?
                  (mapcat #(get-in % [:redaction :audit :config :deny/patterns])
                          events-jsonl)))
      (is (not (str/includes? (slurp (:out/events res)) "john@example.com")))
      (is (not (str/includes? (slurp (:out/events res)) "abc-secret")))
      (is (not (str/includes? (slurp (:out/events res)) "top.secret")))
      (is (str/includes? (slurp (:out/events res)) "[REDACTED]")))))
