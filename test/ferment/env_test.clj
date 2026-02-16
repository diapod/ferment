(ns

    ^{:doc    "Environment preconfiguration tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.env-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.env :as env]))

(deftest hf-paths-are-expanded-when-relative
  (testing "HF_HOME/HF_HUB_CACHE loaded from env are made absolute against project root."
    (with-redefs [env/project-root (constantly "/tmp/ferment-root")]
      (is (= "/tmp/ferment-root/dev/resources/ferment/models/AI-CACHE"
             (env/preconfigure-env-value
              :ferment.env/hf.home
              "dev/resources/ferment/models/AI-CACHE")))
      (is (= "/tmp/ferment-root/dev/resources/ferment/models/AI-CACHE/hub"
             (env/preconfigure-env-value
              :ferment.env/hf.hub.cache
              "dev/resources/ferment/models/AI-CACHE/hub"))))))

(deftest hf-paths-remain-unchanged-when-absolute
  (testing "Absolute HF paths are not modified."
    (with-redefs [env/project-root (constantly "/tmp/ferment-root")]
      (is (= "/Volumes/DEV/AI-CACHE"
             (env/preconfigure-env-value
              :ferment.env/hf.home
              "/Volumes/DEV/AI-CACHE")))
      (is (= "/Volumes/DEV/AI-CACHE/hub"
             (env/preconfigure-env-value
              :ferment.env/hf.hub.cache
              "/Volumes/DEV/AI-CACHE/hub"))))))

(deftest non-hf-keys-are-not-modified
  (testing "Only selected HF keys are normalized."
    (with-redefs [env/project-root (constantly "/tmp/ferment-root")]
      (is (= "dev/resources/ferment/models/AI-CACHE"
             (env/preconfigure-env-value
              :ferment.env/other.path
              "dev/resources/ferment/models/AI-CACHE")))
      (is (= "mini"
             (env/preconfigure-env-value
              :ferment.env/ferment.model.profile
              "mini"))))))
