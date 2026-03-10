(ns ferment.benchmark.gate-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.benchmark.gate :as gate]))

(defn- summary
  [{:keys [pass interactive must truncated preset]}]
  {:pass (boolean pass)
   :preset (or preset :default)
   :metrics {:text_respond_interactive_p95_ms interactive
             :must_failed_rate_sla must
             :text_truncated_total truncated}})

(deftest evaluate-gate-passes-with-candidate-only
  (testing "Without baseline, gate relies on candidate pass flag."
    (let [candidate (summary {:pass true
                              :interactive 9000.0
                              :must 0.10
                              :truncated 0})
          report (gate/evaluate-gate candidate nil {:require-pass? true})]
      (is (= true (:pass? report)))
      (is (= true (get-in report [:candidate :pass?])))
      (is (= 1 (count (:checks report))))
      (is (= :candidate/pass (get-in report [:checks 0 :check]))))))

(deftest evaluate-gate-fails-on-candidate-pass-false
  (testing "Candidate with pass=false fails hard candidate/pass check."
    (let [candidate (summary {:pass false
                              :interactive 8000.0
                              :must 0.05
                              :truncated 0})
          report (gate/evaluate-gate candidate nil {:require-pass? true})]
      (is (= false (:pass? report)))
      (is (= false (get-in report [:checks 0 :pass]))))))

(deftest evaluate-gate-detects-regression-vs-baseline
  (testing "Interactive latency regression above threshold fails report."
    (let [baseline (summary {:pass true
                             :interactive 5000.0
                             :must 0.05
                             :truncated 0})
          candidate (summary {:pass true
                              :interactive 7001.0
                              :must 0.05
                              :truncated 0})
          report (gate/evaluate-gate candidate baseline {:require-pass? true
                                                         :max-interactive-regress-ms 1500.0
                                                         :max-interactive-regress-ratio 0.20})]
      (is (= false (:pass? report)))
      (is (= true (some #(= :regression/interactive-p95 (:check %)) (:checks report))))
      (is (= false
             (:pass
              (first
               (filter #(= :regression/interactive-p95 (:check %))
                       (:checks report)))))))))

(deftest evaluate-gate-allows-small-regression-within-thresholds
  (testing "Small candidate drift within configured thresholds passes."
    (let [baseline (summary {:pass true
                             :interactive 5000.0
                             :must 0.10
                             :truncated 0})
          candidate (summary {:pass true
                              :interactive 5600.0
                              :must 0.12
                              :truncated 0})
          report (gate/evaluate-gate candidate baseline {:require-pass? true
                                                         :max-interactive-regress-ms 700.0
                                                         :max-interactive-regress-ratio 0.20
                                                         :max-must-failed-regress 0.05
                                                         :max-truncated-increase 0})]
      (is (= true (:pass? report)))
      (is (= true (every? :pass (:checks report)))))))

(deftest evaluate-gate-fails-on-truncated-growth
  (testing "Growth in truncated output count beyond threshold fails report."
    (let [baseline (summary {:pass true
                             :interactive 4000.0
                             :must 0.05
                             :truncated 0})
          candidate (summary {:pass true
                              :interactive 4100.0
                              :must 0.05
                              :truncated 2})
          report (gate/evaluate-gate candidate baseline {:require-pass? true
                                                         :max-interactive-regress-ms 500.0
                                                         :max-interactive-regress-ratio 0.20
                                                         :max-must-failed-regress 0.05
                                                         :max-truncated-increase 0})]
      (is (= false (:pass? report)))
      (is (= false
             (:pass
              (first
               (filter #(= :regression/truncated-total (:check %))
                       (:checks report)))))))))
