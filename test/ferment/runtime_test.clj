(ns

    ^{:doc    "Runtime branch tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.runtime-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.queue :as queue]
            [ferment.runtime :as runtime]
            [ferment.telemetry :as telemetry]))

(defn- await-job-status
  [service job-id statuses timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))
        statuses-set (set statuses)]
    (loop []
      (let [res (queue/get-job service job-id)
            status (get-in res [:job :job/status])]
        (if (and (:ok? res)
                 (contains? statuses-set status))
          (:job res)
          (if (< (System/currentTimeMillis) deadline)
            (do
              (Thread/sleep 10)
              (recur))
            (when (:ok? res)
              (:job res))))))))

(deftest preconfigure-runtime-adds-queue-defaults
  (testing "preconfigure-runtime provides queue branch defaults when missing."
    (let [cfg (runtime/preconfigure-runtime :ferment.runtime/default {})]
      (is (map? (:queue cfg)))
      (is (= false (get-in cfg [:queue :enabled?])))
      (is (= 256 (get-in cfg [:queue :max-size])))
      (is (= [:interactive :batch] (get-in cfg [:queue :classes]))))))

(deftest init-runtime-wires-queue-service
  (testing "init-runtime attaches queue service under stable :queue/service key."
    (let [state (runtime/init-runtime
                 :ferment.runtime/default
                 {:queue {:enabled? true
                          :workers 0
                          :max-size 2}})
          queue-service (:queue/service state)
          submit (queue/submit! queue-service {:task {:intent :text/respond}
                                               :input {:prompt "hej"}})]
      (is (queue/service? queue-service))
      (is (map? (:queue state)))
      (is (= true (get-in state [:queue :enabled?])))
      (is (:ok? submit))
      (is (= :queued (get-in submit [:job :job/status])))
      (runtime/stop-runtime :ferment.runtime/default state))))

(deftest runtime-queue-worker-completes-jobs
  (testing "Queue worker consumes queued jobs and writes completed result."
    (telemetry/clear-queue!)
    (let [state (runtime/init-runtime
                 :ferment.runtime/default
                 {:protocol {}
                  :router {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :queue/invoke-fn (fn [_request]
                                     {:ok? true
                                      :result {:result {:type :value
                                                        :out {:text "ok"}}}})
                  :queue {:enabled? true
                          :workers 1
                          :poll-interval-ms 5
                          :default-timeout-ms 1000
                          :retry {:max-attempts 1
                                  :base-backoff-ms 1
                                  :jitter-ms 0}}})
          queue-service (:queue/service state)]
      (try
        (let [submit (queue/submit! queue-service {:proto 1
                                                   :trace {:id "runtime-queue-complete-1"}
                                                   :task {:intent :text/respond}
                                                   :input {:prompt "hej"}})
              job-id (get-in submit [:job :job/id])
              job    (await-job-status queue-service job-id #{:completed} 3000)]
          (is (:ok? submit))
          (is (string? job-id))
          (is (= :completed (:job/status job)))
          (is (= "ok" (get-in job [:result :result :out :text]))))
        (finally
          (runtime/stop-runtime :ferment.runtime/default state))))
    (let [snapshot (telemetry/queue-snapshot)]
      (is (= 1 (get-in snapshot [:counters :jobs/submitted])))
      (is (= 1 (get-in snapshot [:counters :jobs/started])))
      (is (= 1 (get-in snapshot [:counters :jobs/completed]))))))

(deftest runtime-queue-worker-retries-transient-failures
  (testing "Queue worker retries runtime/invoke-failed once and completes when second attempt succeeds."
    (let [attempts (atom 0)
          state (runtime/init-runtime
                 :ferment.runtime/default
                 {:protocol {}
                  :router {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :queue/invoke-fn (fn [_request]
                                     (if (= 1 (swap! attempts inc))
                                       {:ok? false
                                        :retryable? true
                                        :error {:type :runtime/invoke-failed
                                                :message "temporary"}}
                                       {:ok? true
                                        :result {:result {:type :value
                                                          :out {:text "ok-after-retry"}}}}))
                  :queue {:enabled? true
                          :workers 1
                          :poll-interval-ms 5
                          :default-timeout-ms 1000
                          :retry {:max-attempts 2
                                  :base-backoff-ms 1
                                  :jitter-ms 0}}})
          queue-service (:queue/service state)]
      (try
        (let [submit (queue/submit! queue-service {:proto 1
                                                   :trace {:id "runtime-queue-retry-1"}
                                                   :task {:intent :text/respond}
                                                   :input {:prompt "hej"}})
              job-id (get-in submit [:job :job/id])
              job    (await-job-status queue-service job-id #{:completed} 3000)]
          (is (:ok? submit))
          (is (= :completed (:job/status job)))
          (is (= 2 @attempts))
          (is (= "ok-after-retry" (get-in job [:result :result :out :text]))))
        (finally
          (runtime/stop-runtime :ferment.runtime/default state))))))

(deftest runtime-queue-worker-fails-job-on-timeout
  (testing "Queue worker marks job as failed with queue/timeout when execution exceeds timeout."
    (let [state (runtime/init-runtime
                 :ferment.runtime/default
                 {:protocol {}
                  :router {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :queue/invoke-fn (fn [_request]
                                     (Thread/sleep 150)
                                     {:ok? true
                                      :result {:result {:type :value
                                                        :out {:text "late"}}}})
                  :queue {:enabled? true
                          :workers 1
                          :poll-interval-ms 5
                          :default-timeout-ms 15
                          :retry {:max-attempts 1
                                  :base-backoff-ms 1
                                  :jitter-ms 0}}})
          queue-service (:queue/service state)]
      (try
        (let [submit (queue/submit! queue-service {:proto 1
                                                   :trace {:id "runtime-queue-timeout-1"}
                                                   :task {:intent :text/respond}
                                                   :input {:prompt "hej"}})
              job-id (get-in submit [:job :job/id])
              job    (await-job-status queue-service job-id #{:failed} 4000)]
          (is (:ok? submit))
          (is (= :failed (:job/status job)))
          (is (= :queue/timeout (get-in job [:error :type]))))
        (finally
          (runtime/stop-runtime :ferment.runtime/default state))))))
