(ns

    ^{:doc    "Runtime branch tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.runtime-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.execution-graph :as execution-graph]
            [ferment.queue :as queue]
            [ferment.runtime :as runtime]
            [ferment.telemetry :as telemetry]))

(defn- tmp-events-path
  [suffix]
  (str (System/getProperty "java.io.tmpdir")
       "/ferment-runtime-test-"
       suffix
       "-"
       (System/currentTimeMillis)
       ".ednl"))

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

(deftest invoke-queued-request-emits-near-miss-diagnostics
  (testing "Queued invoke path returns deterministic rejected-candidate reason for capability near-miss."
    (let [runtime-state {:protocol {:policy/default {:fallback []}}
                         :resolver {:caps/by-id {:llm/voice {:cap/id :llm/voice
                                                             :cap/intents #{:text/respond}
                                                             :cap/can-produce #{:value}
                                                             :cap/effects-allowed #{:none}
                                                             :io/in-schema :req/text
                                                             :io/out-schema :res/text}}}
                         :router {:routing {:intent->cap {:text/respond :llm/voice}}}}
          request {:proto 1
                   :trace {:id "runtime-near-miss-1"}
                   :task {:intent :text/respond
                          :cap/id :llm/voice
                          :requires {:result/type :plan}}
                   :input {:prompt "hej"}}
          result (#'ferment.runtime/invoke-queued-request runtime-state request)]
      (is (false? (:ok? result)))
      (is (= false (:retryable? result)))
      (is (= :unsupported/intent (get-in result [:error :type])))
      (is (= :result-type/not-supported
             (get-in result [:error :details :rejected-candidates 0 :reason]))))))

(deftest init-runtime-restores-inflight-jobs-from-execution-graph
  (testing "Runtime restores queued/running jobs from durable execution graph on startup."
    (let [path (tmp-events-path "resume")
          graph-cfg {:enabled? true
                     :store/type :fs-ednl
                     :store/path path}
          graph (execution-graph/init-service graph-cfg)
          _ (execution-graph/append-event! graph
                                           {:event/type :job/submitted
                                            :job/id "job/42"
                                            :run/id "job/42"
                                            :trace/id "resume-42"
                                            :request {:proto 1
                                                      :trace {:id "resume-42"}
                                                      :task {:intent :text/respond}
                                                      :input {:prompt "hej"}}})
          _ (execution-graph/append-event! graph
                                           {:event/type :job/running
                                            :job/id "job/42"})
          _ (execution-graph/append-event! graph
                                           {:event/type :node/succeeded
                                            :job/id "job/42"
                                            :checkpoint {:next-index 1
                                                         :env {:seed "x"}
                                                         :emitted nil}})
          state (runtime/init-runtime
                 :ferment.runtime/default
                 {:protocol {}
                  :router {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :resolver {:routing {:intent->cap {:text/respond :llm/voice}}}
                  :execution-graph graph-cfg
                  :queue {:enabled? true
                          :workers 0
                          :max-size 8}})
          queue-service (:queue/service state)
          restored (queue/get-job queue-service "job/42")]
      (try
        (is (:ok? restored))
        (is (contains? #{:queued :running}
                       (get-in restored [:job :job/status])))
        (is (map? (get-in restored [:job :request :workflow/resume-checkpoint])))
        (is (= 1 (get-in restored [:job :request :workflow/resume-checkpoint :next-index])))
        (finally
          (runtime/stop-runtime :ferment.runtime/default state))))))

(deftest artifact-rollout-overrides-are-mutable-and-validated
  (testing "Runtime artifact rollout helpers set/get/clear overrides with version validation."
    (let [runtime-state {:artifact/overrides (atom {})
                         :protocol {:versions {:baseline-v1 {}
                                               :canary-v1 {}}
                                    :rollout {:active :baseline-v1}}
                         :router {:versions {:baseline-v1 {}
                                             :canary-v1 {}}
                                  :rollout {:active :baseline-v1}}}
          set-ok (runtime/set-artifact-rollout!
                  runtime-state
                  {:artifact :protocol
                   :active :canary-v1
                   :canary {:enabled? true
                            :version :baseline-v1
                            :percent 20}})
          get-one (runtime/get-artifact-rollout runtime-state :protocol)
          get-all (runtime/get-artifact-rollout runtime-state)
          effective (runtime/artifact-config runtime-state :protocol)
          cleared (runtime/set-artifact-rollout!
                   runtime-state
                   {:artifact :protocol
                    :clear? true})
          invalid-version (runtime/set-artifact-rollout!
                           runtime-state
                           {:artifact :router
                            :active :unknown-v9})]
      (is (= true (:ok? set-ok)))
      (is (= :canary-v1 (get-in set-ok [:override :active])))
      (is (= true (get-in set-ok [:override :canary :enabled?])))
      (is (= :protocol (:artifact get-one)))
      (is (= :canary-v1 (get-in get-one [:override :active])))
      (is (= :canary-v1 (get-in get-all [:overrides :protocol :active])))
      (is (= :canary-v1 (get-in effective [:rollout :active])))
      (is (= 20 (get-in effective [:rollout :canary :percent])))
      (is (= true (:ok? cleared)))
      (is (= true (:cleared? cleared)))
      (is (nil? (get-in (runtime/get-artifact-rollout runtime-state :protocol)
                        [:override :active])))
      (is (= false (:ok? invalid-version)))
      (is (= :input/invalid (:error invalid-version))))))
