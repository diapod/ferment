(ns

    ^{:doc    "Model runtime startup and invoke behavior tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.model-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.model :as model]
            [ferment.providers.http :as providers.http])
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

(deftest runtime-request-handler-preserves-http-status-diagnostics
  (testing "Invoke preserves upstream HTTP status/body details from ExceptionInfo ex-data."
    (let [req {:body :invoke
               :args [{:prompt "diagnostic"}]}
          worker-config {:id :ferment.model.runtime/voice-api
                         :name "voice remote api runtime"
                         :invoke-fn (fn [_ _ _]
                                      (throw (ex-info
                                              "Failed to invoke runtime model over HTTP."
                                              {:error :invoke-http-status
                                               :status 401
                                               :url "https://api.anthropic.com/v1/messages"
                                               :body "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\"}}"})))}
          response (model/runtime-request-handler {} nil req worker-config)]
      (is (= false (:ok? response)))
      (is (= :invoke-failed (:error response)))
      (is (= :invoke-http-status (get-in response [:details :error])))
      (is (= 401 (get-in response [:details :status])))
      (is (= "https://api.anthropic.com/v1/messages"
             (get-in response [:details :url])))
      (is (string? (get-in response [:details :body]))))))

(deftest runtime-request-handler-preserves-cause-class-for-generic-throwables
  (testing "Invoke preserves throwable class/message even when failure is not ExceptionInfo."
    (let [req {:body :invoke
               :args [{:prompt "diagnostic"}]}
          worker-config {:id :ferment.model.runtime/voice-api
                         :name "voice remote api runtime"
                         :invoke-fn (fn [_ _ _]
                                      (throw (java.net.ConnectException. "connection refused")))}
          response (model/runtime-request-handler {} nil req worker-config)]
      (is (= false (:ok? response)))
      (is (= :invoke-failed (:error response)))
      (is (= "java.net.ConnectException" (get-in response [:details :cause/class])))
      (is (= "connection refused" (get-in response [:details :cause/message]))))))

(deftest invoke-runtime-http-preserves-non-success-status-details
  (testing "HTTP invoke wraps upstream non-success status without dropping status/body/response metadata."
    (let [worker-config {:invoke/http {:provider/id :anthropic-messages
                                       :base-url "https://api.anthropic.com"
                                       :endpoint "/v1/messages"
                                       :request-params {:model "claude-opus-4-6"
                                                        :max_tokens 16}}}]
      (with-redefs [model/send-http-json!
                    (fn [_url _body-map _worker-config _payload]
                      (throw (ex-info
                              "Model HTTP invoke failed with non-success status."
                              {:error :invoke-http-status
                               :status 401
                               :url "https://api.anthropic.com/v1/messages"
                               :body "{\"type\":\"error\"}"
                               :response {:type "error"}})))
                    model/pick-response-text
                    (fn [_response-map _worker-config] nil)
                    providers.http/invoke-http-body
                    (fn [_opts] {:messages [{:role "user" :content "hej"}]})]
        (try
          (#'ferment.model/invoke-runtime-http!
           {:prompt "hej"}
           nil
           worker-config)
          (is false "Expected invoke-runtime-http! to throw on non-success status.")
          (catch clojure.lang.ExceptionInfo ex
            (let [data (ex-data ex)]
              (is (= :invoke-http-status (:error data)))
              (is (= 401 (:status data)))
              (is (= "https://api.anthropic.com/v1/messages" (:url data)))
              (is (= "{\"type\":\"error\"}" (:body data)))
              (is (= {:type "error"} (:response data))))))))))
