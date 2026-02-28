(ns

    ^{:doc    "Remote IP middleware tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.remote-ip-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.middleware.remote-ip :as remote-ip]))

(deftest remote-ip-handler-detects-proxy-request
  (testing "When proxy header is present and trusted, middleware marks request as proxied."
    (let [req {:headers {"x-forwarded-for" "203.0.113.10"}
               :remote-addr "10.0.0.1"}
          out (remote-ip/handler req "x-forwarded-for" nil)]
      (is (= "203.0.113.10" @(get out :remote-ip/str)))
      (is (true? @(get out :remote-ip/proxy?))))))

(deftest remote-ip-handler-detects-direct-request
  (testing "When proxy header is missing, middleware marks request as non-proxied."
    (let [req {:headers {}
               :remote-addr "10.0.0.1"}
          out (remote-ip/handler req "x-forwarded-for" nil)]
      (is (= "10.0.0.1" @(get out :remote-ip/str)))
      (is (false? @(get out :remote-ip/proxy?))))))
