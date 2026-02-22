(ns

    ^{:doc    "Model runtime startup and invoke behavior tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.model-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.model :as model])
  (:import (java.net ServerSocket)))

(deftest start-command-process-fails-fast-when-http-port-is-busy
  (testing "Runtime startup fails fast with readable :runtime-port-in-use details."
    (with-open [^ServerSocket occupied (ServerSocket. 0)]
      (let [port (.getLocalPort occupied)
            worker-config {:command ["mlx_lm.server" "--model" "mock/model"]
                           :invoke/http {:base-url (str "http://127.0.0.1:" port)
                                         :endpoint "/v1/chat/completions"}}]
        (try
          (model/start-command-process! worker-config nil)
          (is false "Expected runtime start to fail when HTTP port is already busy.")
          (catch clojure.lang.ExceptionInfo ex
            (let [data (ex-data ex)]
              (is (= :runtime-port-in-use (:error data)))
              (is (= :runtime/port-in-use (:type data)))
              (is (= "127.0.0.1" (:host data)))
              (is (= port (:port data)))
              (is (vector? (:listener/pids data))))))))))

(deftest runtime-request-handler-returns-runtime-not-ready-before-invoke
  (testing "Invoke returns a readable startup error when runtime failed before serving requests."
    (let [called? (atom false)
          session {:runtime/error {:error :runtime-port-in-use
                                   :message "port busy"}}
          req {:body :invoke
               :args [{:prompt "diagnostic"}]}
          worker-config {:id :ferment.model.runtime/meta
                         :name "meta model runtime"
                         :invoke-fn (fn [_ _ _]
                                      (reset! called? true)
                                      {:text "should-not-run"})}
          response (model/runtime-request-handler session nil req worker-config)]
      (is (= false (:ok? response)))
      (is (= :runtime-not-ready (:error response)))
      (is (= :runtime-port-in-use (get-in response [:details :error])))
      (is (false? @called?)))))
