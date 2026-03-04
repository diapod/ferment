(ns ferment.training.promotion-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.training.promotion :as promotion]))

(def ^:private good-eval-report
  {:eval/version 1
   :cases/evaluated 10
   :summary {:overall {:total 10
                       :passed 9
                       :failed 1
                       :pass-rate 0.9}
             :by-suite {:protocol-conformance {:total 4 :passed 4 :failed 0 :pass-rate 1.0}
                        :constitution-compliance {:total 3 :passed 3 :failed 0 :pass-rate 1.0}
                        :regression {:total 3 :passed 2 :failed 1 :pass-rate 0.6666667}}}})

(deftest evaluate-report-rejects-missing-required-suite
  (testing "Promotion gate rejects report when required suite is missing."
    (let [report (assoc-in good-eval-report
                           [:summary :by-suite]
                           {:protocol-conformance {:total 4 :passed 4 :failed 0 :pass-rate 1.0}})
          decision (promotion/evaluate-report report nil)]
      (is (= :rejected (:promotion/status decision)))
      (is (= false (:promotion/eligible? decision)))
      (is (= :promotion/missing-required-suites
             (get-in decision [:promotion/reasons 0 :reason]))))))

(deftest evaluate-report-accepts-when-thresholds-are-met
  (testing "Promotion gate accepts when all thresholds and required suites pass."
    (let [cfg {:thresholds {:overall/pass-rate-min 0.8
                            :suite-pass-rate-min {:protocol-conformance 0.95
                                                  :constitution-compliance 0.95
                                                  :regression 0.60}}}
          decision (promotion/evaluate-report good-eval-report cfg)]
      (is (= :accepted (:promotion/status decision)))
      (is (= true (:promotion/eligible? decision)))
      (is (empty? (:promotion/reasons decision))))))

(deftest evaluate-report-rejects-below-overall-threshold
  (testing "Promotion gate rejects when overall pass rate is below threshold."
    (let [cfg {:thresholds {:overall/pass-rate-min 0.95}}
          decision (promotion/evaluate-report good-eval-report cfg)]
      (is (= :rejected (:promotion/status decision)))
      (is (= false (:promotion/eligible? decision)))
      (is (= :promotion/overall-pass-rate-below-min
             (get-in decision [:promotion/reasons 0 :reason]))))))
