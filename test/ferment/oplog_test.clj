(ns ferment.oplog-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.oplog :as oplog]))

(deftest reads-oplog-config-from-new-and-legacy-branches
  (testing "Auth and subsystem config are resolved from both new and legacy keys."
    (let [new-cfg    {:ferment.logging/oplog
                      {:auth {:fn/reporter :auth-r}
                       :act  {:fn/reporter :act-r}
                       :ops  {:fn/reporter :ops-r}}}
          runtime-cfg {:oplog
                       {:auth {:fn/reporter :runtime-auth-r}
                        :act  {:fn/reporter :runtime-act-r}
                        :ops  {:fn/reporter :runtime-ops-r}}}
          legacy-cfg {:ferment.oplog.auth/log {:fn/reporter :legacy-auth-r}
                      :ferment.oplog.act/log  {:fn/reporter :legacy-act-r}
                      :ferment.oplog.ops/log  {:fn/reporter :legacy-ops-r}}]
      (is (= {:fn/reporter :auth-r}
             (oplog/auth-config new-cfg)))
      (is (= {:fn/reporter :act-r}
             (oplog/act-config new-cfg)))
      (is (= {:fn/reporter :runtime-auth-r}
             (oplog/auth-config runtime-cfg)))
      (is (= {:fn/reporter :runtime-act-r}
             (oplog/act-config runtime-cfg)))
      (is (= {:fn/reporter :legacy-auth-r}
             (oplog/auth-config legacy-cfg)))
      (is (= {:fn/reporter :legacy-act-r}
             (oplog/act-config legacy-cfg)))
      (is (= {:fn/reporter :ops-r}
             (oplog/config :ops new-cfg)))
      (is (= {:fn/reporter :runtime-ops-r}
             (oplog/config :ops runtime-cfg)))
      (is (= {:fn/reporter :legacy-ops-r}
             (oplog/config :ops legacy-cfg))))))

(deftest logger-wrappers-use-reporter-functions
  (testing "logger/auth-logger/get-logger forward keyword arguments to reporter."
    (let [seen-auth (atom nil)
          seen-act  (atom nil)
          seen-ops  (atom nil)
          seen-exp  (atom nil)
          cfg {:ferment.logging/oplog
               {:auth {:fn/reporter (fn [m] (reset! seen-auth m))}
                :act  {:fn/reporter (fn [m] (reset! seen-act m))}
                :ops  {:fn/reporter (fn [m] (reset! seen-ops m))}}
               :ferment.oplog.explicit/log
               {:fn/reporter (fn [m] (reset! seen-exp m))}}
          auth-log (oplog/auth-logger cfg)
          act-log  (oplog/act-logger cfg)
          ops-log  (oplog/logger :ops cfg)
          exp-log  (oplog/get-logger :ferment.oplog.explicit/log cfg)
          missing  (oplog/logger :missing cfg)]
      (auth-log :operation :login :success true)
      (act-log :trace-id "t-1" :outcome :ok)
      (ops-log :operation :other :success false)
      (exp-log :operation :explicit :success true)
      (is (= {:operation :login :success true} @seen-auth))
      (is (= {:trace-id "t-1" :outcome :ok} @seen-act))
      (is (= {:operation :other :success false} @seen-ops))
      (is (= {:operation :explicit :success true} @seen-exp))
      (is (nil? (missing :operation :none :success true))))))
