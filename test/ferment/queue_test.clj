(ns

    ^{:doc    "Tests for in-memory queue service."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.queue-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.queue :as queue]))

(defn- test-clock
  [t]
  (fn []
    (let [ms @t]
      (swap! t inc)
      ms)))

(deftest queue-submit-and-poll-flow
  (testing "submit! enqueues job and poll!/get-job returns canonical queued record."
    (let [time* (atom 1700000000000)
          svc (queue/init-service {:enabled? true
                                   :clock (test-clock time*)})
          submit (queue/submit! svc {:task {:intent :text/respond}
                                     :input {:prompt "hej"}}
                                {:queue/class :interactive
                                 :deadline-ms 1000})
          job-id (get-in submit [:job :job/id])
          poll (queue/poll! svc job-id)]
      (is (:ok? submit))
      (is (= :queued (get-in submit [:job :job/status])))
      (is (= :interactive (get-in submit [:job :queue/class])))
      (is (string? (get-in submit [:job :submitted-at])))
      (is (string? (get-in submit [:job :deadline-at])))
      (is (:ok? poll))
      (is (= :queued (get-in poll [:job :job/status]))))))

(deftest queue-priority-prefers-interactive-over-batch
  (testing "start-next! uses configured class priority order."
    (let [time* (atom 1700000000100)
          svc (queue/init-service {:enabled? true
                                   :classes [:interactive :batch]
                                   :priority-order [:interactive :batch]
                                   :clock (test-clock time*)})
          _ (queue/submit! svc {:id :batch} {:queue/class :batch})
          _ (queue/submit! svc {:id :interactive} {:queue/class :interactive})
          started (queue/start-next! svc)]
      (is (:ok? started))
      (is (= :interactive (get-in started [:job :queue/class])))
      (is (= :running (get-in started [:job :job/status]))))))

(deftest queue-has-deterministic-full-error
  (testing "submit! returns deterministic queue/full error when max-size is reached."
    (let [time* (atom 1700000000200)
          svc (queue/init-service {:enabled? true
                                   :max-size 1
                                   :clock (test-clock time*)})
          first-submit (queue/submit! svc {:n 1})
          second-submit (queue/submit! svc {:n 2})]
      (is (:ok? first-submit))
      (is (= {:ok? false
              :error :queue/full
              :max-size 1}
             second-submit)))))

(deftest queue-frees-capacity-after-cancel
  (testing "cancel on active job decrements active counter and allows another submit."
    (let [time* (atom 1700000000250)
          svc (queue/init-service {:enabled? true
                                   :max-size 1
                                   :clock (test-clock time*)})
          first-submit (queue/submit! svc {:n 1})
          job-id (get-in first-submit [:job :job/id])
          full-submit (queue/submit! svc {:n 2})
          cancel-res (queue/cancel! svc job-id :user-request)
          third-submit (queue/submit! svc {:n 3})
          snap (queue/snapshot svc)]
      (is (:ok? first-submit))
      (is (= :queue/full (:error full-submit)))
      (is (:ok? cancel-res))
      (is (= :canceled (get-in cancel-res [:job :job/status])))
      (is (:ok? third-submit))
      (is (= 1 (:jobs/active snap))))))

(deftest queue-frees-capacity-after-complete
  (testing "running -> completed transition decrements active counter and allows another submit."
    (let [time* (atom 1700000000275)
          svc (queue/init-service {:enabled? true
                                   :max-size 1
                                   :clock (test-clock time*)})
          first-submit (queue/submit! svc {:n 1})
          job-id (get-in first-submit [:job :job/id])
          start-res (queue/start-next! svc)
          full-submit (queue/submit! svc {:n 2})
          complete-res (queue/complete! svc job-id {:text "done"})
          third-submit (queue/submit! svc {:n 3})
          snap (queue/snapshot svc)]
      (is (:ok? start-res))
      (is (= :running (get-in start-res [:job :job/status])))
      (is (= :queue/full (:error full-submit)))
      (is (:ok? complete-res))
      (is (= :completed (get-in complete-res [:job :job/status])))
      (is (:ok? third-submit))
      (is (= 1 (:jobs/active snap))))))

(deftest queue-rejects-invalid-transition
  (testing "complete! cannot be called for queued job without running transition."
    (let [time* (atom 1700000000300)
          svc (queue/init-service {:enabled? true
                                   :clock (test-clock time*)})
          submit (queue/submit! svc {:n 1})
          job-id (get-in submit [:job :job/id])
          complete-res (queue/complete! svc job-id {:text "done"})]
      (is (:ok? submit))
      (is (= :queue/invalid-transition (:error complete-res)))
      (is (= :queued (:from complete-res)))
      (is (= :completed (:to complete-res))))))

(deftest queue-cancel-prevents-later-start
  (testing "canceled queued job is skipped by start-next!."
    (let [time* (atom 1700000000400)
          svc (queue/init-service {:enabled? true
                                   :clock (test-clock time*)})
          submit (queue/submit! svc {:n 1})
          job-id (get-in submit [:job :job/id])
          cancel-res (queue/cancel! svc job-id :user-request)
          start-res (queue/start-next! svc)]
      (is (:ok? cancel-res))
      (is (= :canceled (get-in cancel-res [:job :job/status])))
      (is (= :queue/empty (:error start-res))))))
