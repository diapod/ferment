(ns

    ^{:doc    "Execution graph persistence and recovery tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.execution-graph-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.execution-graph :as execution-graph]))

(defn- tmp-events-path
  [suffix]
  (str (System/getProperty "java.io.tmpdir")
       "/ferment-execution-graph-test-"
       suffix
       "-"
       (System/currentTimeMillis)
       ".ednl"))

(deftest execution-graph-rebuilds-inflight-jobs-from-durable-log
  (testing "Execution graph keeps durable events and recovers inflight jobs with checkpoints."
    (let [path (tmp-events-path "inflight")
          cfg {:enabled? true
               :store/type :fs-ednl
               :store/path path}
          svc (execution-graph/init-service cfg)]
      (is (:ok? (execution-graph/append-event! svc
                                               {:event/type :job/submitted
                                                :job/id "job/17"
                                                :run/id "job/17"
                                                :trace/id "trace-17"
                                                :request {:proto 1
                                                          :trace {:id "trace-17"}
                                                          :task {:intent :text/respond}
                                                          :input {:prompt "hej"}}
                                                :queue/class :interactive})))
      (is (:ok? (execution-graph/append-event! svc
                                               {:event/type :job/running
                                                :job/id "job/17"})))
      (is (:ok? (execution-graph/append-event! svc
                                               {:event/type :node/succeeded
                                                :job/id "job/17"
                                                :checkpoint {:next-index 3
                                                             :env {:answer {:text "ok"}}
                                                             :emitted {:text "ok"}}})))
      (is (:ok? (execution-graph/append-event! svc
                                               {:event/type :job/submitted
                                                :job/id "job/18"
                                                :request {:proto 1
                                                          :task {:intent :text/respond}
                                                          :input {:prompt "bye"}}})))
      (is (:ok? (execution-graph/append-event! svc
                                               {:event/type :job/completed
                                                :job/id "job/18"})))
      (let [svc2 (execution-graph/init-service cfg)
            inflight (execution-graph/inflight-jobs svc2)]
        (is (= 1 (count inflight)))
        (is (= "job/17" (:job/id (first inflight))))
        (is (= :queued (:job/status (first inflight))))
        (is (= {:next-index 3
                :env {:answer {:text "ok"}}
                :emitted {:text "ok"}}
               (:checkpoint (first inflight))))))))
