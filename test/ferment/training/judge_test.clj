(ns ferment.training.judge-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.training.judge :as judge]))

(deftest evaluate-disabled-mode-returns-pass
  (testing "Judge disabled mode is deterministic pass with no failed rules."
    (let [event {:training.event/id "j-1"
                 :labels {:accepted? true}
                 :call {:out {:text "ok"}}}
          verdict (judge/evaluate! event {:mode :disabled})]
      (is (= false (:judge/enabled? verdict)))
      (is (= :disabled (:judge/mode verdict)))
      (is (= true (:judge/pass? verdict)))
      (is (= [] (:judge/failed-rules verdict)))
      (is (= 1.0 (:judge/score verdict))))))

(deftest evaluate-rules-only-fails-on-internal-markers
  (testing "Judge returns stable failed rule taxonomy when event contains internal markers."
    (let [event {:training.event/id "j-2"
                 :labels {:accepted? true}
                 :redaction {:internal-markers/present? true}
                 :call {:out {:text "<think>secret</think>"}
                        :failure/type nil}}
          verdict (judge/evaluate! event {:mode :rules-only
                                          :constitution/ref "constitution/v1"
                                          :rules [:no-internal-markers
                                                  :non-empty-output-text
                                                  :accepted-consistent]})]
      (is (= true (:judge/enabled? verdict)))
      (is (= :rules-only (:judge/mode verdict)))
      (is (= false (:judge/pass? verdict)))
      (is (= [:no-internal-markers] (:judge/failed-rules verdict)))
      (is (= :judge/rules-failed (:judge/reject-reason verdict)))
      (is (= "constitution/v1" (:judge/constitution-ref verdict)))
      (is (< (Math/abs (- (double 2/3) (:judge/score verdict))) 1.0e-9)))))

(deftest evaluate-rules-only-fails-on-accepted-inconsistency
  (testing "Accepted event with failure/type is marked inconsistent."
    (let [event {:training.event/id "j-3"
                 :labels {:accepted? true}
                 :call {:out {:text "ok"}
                        :failure/type :schema/invalid}}
          verdict (judge/evaluate! event {:mode :rules-only
                                          :rules [:accepted-consistent]})]
      (is (= false (:judge/pass? verdict)))
      (is (= [:accepted-consistent] (:judge/failed-rules verdict)))
      (is (= 0.0 (:judge/score verdict))))))
