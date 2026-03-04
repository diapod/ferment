(ns ferment.training.eval-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.training.eval :as eval]))

(deftest evaluate-cases-computes-summary-by-suite
  (testing "Eval runner aggregates pass/fail metrics across protocol/constitution/regression suites."
    (let [cases [{:case/id "p-1"
                  :suite :protocol-conformance
                  :actual {:text "Output OK"}}
                 {:case/id "p-2"
                  :suite :protocol-conformance
                  :actual {:text "<think>internal</think>"}}
                 {:case/id "c-1"
                  :suite :constitution-compliance
                  :judge/pass? true}
                 {:case/id "r-1"
                  :suite :regression
                  :expected {:text "ACID"}
                  :actual {:text "ACID"}}]
          report (eval/evaluate-cases cases nil)]
      (is (= 4 (:cases/evaluated report)))
      (is (= 0 (:cases/skipped report)))
      (is (= 0.75 (get-in report [:summary :overall :pass-rate])))
      (is (= 2 (get-in report [:summary :by-suite :protocol-conformance :total])))
      (is (= 1 (get-in report [:summary :by-suite :protocol-conformance :failed])))
      (is (= ["p-2"] (:failed/case-ids report))))))

(deftest evaluate-cases-supports-regression-contains-mode
  (testing "Regression suite supports :contains match mode."
    (let [cases [{:case/id "r-contains"
                  :suite :regression
                  :expected {:text "ACID"}
                  :actual {:text "To jest opis ACID."}
                  :match :contains}]
          report (eval/evaluate-cases cases nil)]
      (is (= 1 (:cases/evaluated report)))
      (is (= 1.0 (get-in report [:summary :overall :pass-rate]))))))

(deftest evaluate-cases-honors-report-failed-only
  (testing "Report may include only failed cases when configured."
    (let [cases [{:case/id "ok-1"
                  :suite :protocol-conformance
                  :actual {:text "ok"}}
                 {:case/id "bad-1"
                  :suite :protocol-conformance
                  :actual {:text ""}}]
          report (eval/evaluate-cases cases {:report {:include-cases? true
                                                      :failed-only? true}})]
      (is (= 1 (count (:cases report))))
      (is (= "bad-1" (:case/id (first (:cases report))))))))
