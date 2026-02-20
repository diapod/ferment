(ns

    ^{:doc    "Runtime effects scope enforcement tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.effects-test

  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ferment.effects :as effects]))

(defn- temp-dir
  []
  (-> (java.nio.file.Files/createTempDirectory
       "ferment-effects-test"
       (make-array java.nio.file.attribute.FileAttribute 0))
      str))

(deftest invoke-tool-fs-write-respects-runtime-scope
  (testing "File write succeeds inside configured scope and fails outside whitelist."
    (let [root (temp-dir)
          cfg  {:fs/write {:enabled? true
                           :root root
                           :allow ["sandbox/"]}}
          ok-node {:tool/id :fs/write-file
                   :effects {:allowed #{:fs/write}}
                   :input {:path "sandbox/out.txt"
                           :content "abc"
                           :mkdirs? true}}
          ok-res  (effects/invoke-tool! cfg ok-node)
          bad-node {:tool/id :fs/write-file
                    :effects {:allowed #{:fs/write}}
                    :input {:path "outside/out.txt"
                            :content "xyz"
                            :mkdirs? true}}
          bad-ex (try
                   (effects/invoke-tool! cfg bad-node)
                   nil
                   (catch clojure.lang.ExceptionInfo e
                     (ex-data e)))]
      (is (= true (get-in ok-res [:result :out :wrote?])))
      (is (= "abc"
             (slurp (io/file root "sandbox/out.txt"))))
      (is (= :effects/scope-denied (:failure/type bad-ex)))
      (is (= :path-not-allowed (:reason bad-ex))))))

(deftest invoke-tool-fs-write-cannot-enable-disabled-global-scope
  (testing "Local tool scope cannot override globally disabled effect."
    (let [root (temp-dir)
          cfg  {:fs/write {:enabled? false
                           :root root
                           :allow ["sandbox/"]}}
          node {:tool/id :fs/write-file
                :effects {:allowed #{:fs/write}
                          :scope {:fs/write {:enabled? true}}}
                :input {:path "sandbox/out.txt"
                        :content "abc"
                        :mkdirs? true}}
          ex-data (try
                    (effects/invoke-tool! cfg node)
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (ex-data e)))]
      (is (= :effects/scope-denied (:failure/type ex-data)))
      (is (= :disabled (:reason ex-data))))))

(deftest invoke-tool-process-run-respects-command-and-cwd-scope
  (testing "Process run executes only allowed commands in allowed cwd."
    (let [root (temp-dir)
          cfg  {:process/run {:enabled? true
                              :root root
                              :allow-cwd ["."]
                              :allow-commands ["clojure"]}}
          ok-node {:tool/id :process/run
                   :effects {:allowed #{:process/run}}
                   :input {:command ["clojure" "-e" "(println \"ok\")"]
                           :cwd "."}}
          ok-res  (effects/invoke-tool! cfg ok-node)
          bad-node {:tool/id :process/run
                    :effects {:allowed #{:process/run}}
                    :input {:command ["git" "--version"]
                            :cwd "."}}
          bad-ex (try
                   (effects/invoke-tool! cfg bad-node)
                   nil
                   (catch clojure.lang.ExceptionInfo e
                     (ex-data e)))]
      (is (= 0 (get-in ok-res [:result :out :exit])))
      (is (str/includes? (or (get-in ok-res [:result :out :stdout]) "")
                         "ok"))
      (is (= :effects/scope-denied (:failure/type bad-ex)))
      (is (= :command-not-allowed (:reason bad-ex))))))

(deftest invoke-tool-net-http-denies-host-outside-scope
  (testing "HTTP effect rejects request when host is outside allowed list."
    (let [cfg {:net/http {:enabled? true
                          :allow-schemes #{:https}
                          :allow-hosts ["example.org"]}}
          node {:tool/id :net/http-request
                :effects {:allowed #{:net/http}}
                :input {:url "https://example.com/api"}}]
      (let [ex-data (try
                      (effects/invoke-tool! cfg node)
                      nil
                      (catch clojure.lang.ExceptionInfo e
                        (ex-data e)))]
        (is (= :effects/scope-denied (:failure/type ex-data)))
        (is (= :host-not-allowed (:reason ex-data)))))))
