(ns

    ^{:doc    "Protocol normalization tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.protocol-test

  (:require [clojure.test :refer [deftest is testing]]
            [ferment.protocol :as protocol]
            [io.randomseed.utils :as utils]))

(deftest normalize-protocol-normalizes-prompt-text-values
  (testing "Prompt text values accept vectors/lists and are normalized via prompt-text rules."
    (let [cfg {:prompts {:default ["You" "are" "default"]
                         :roles {:voice '("Role:" "VOICE")
                                 :solver :solver/prompt}}
               :intents {:text/respond {:system ["Use" "Polish"]}
                         :problem/solve {:system '("Solve" "briefly")}
                         :code/patch {:system :coder/prompt}
                         :context/summarize {:constraints {:max-chars 10}}}}
          normalized (protocol/normalize-protocol cfg)]
      (is (= "You are default" (get-in normalized [:prompts :default])))
      (is (= "Role: VOICE" (get-in normalized [:prompts :roles :voice])))
      (is (= (utils/some-str :solver/prompt)
             (get-in normalized [:prompts :roles :solver])))
      (is (= "Use Polish" (get-in normalized [:intents :text/respond :system])))
      (is (= "Solve briefly" (get-in normalized [:intents :problem/solve :system])))
      (is (= (utils/some-str :coder/prompt)
             (get-in normalized [:intents :code/patch :system])))
      (is (= {:constraints {:max-chars 10}}
             (get-in normalized [:intents :context/summarize])))
      (is (= 1 (:proto/version normalized)))
      (is (= :application/edn (:transport/content-type normalized)))
      (is (= 3 (:retry/max-attempts normalized))))))

(deftest normalize-protocol-normalizes-version-catalog
  (testing "Version catalog prompts/intents are normalized like top-level protocol."
    (let [cfg {:prompts {:default ["Base" "prompt"]}
               :versions {:v1 {:prompts {:default ["V1" "prompt"]}
                               :intents {:text/respond {:system ["Use" "PL"]}}}
                          :v2 {:prompts {:default '("V2" "prompt")}}}
               :rollout {:active "v1"
                         :canary {:enabled? true
                                  :version ":v2"
                                  :percent 15}
                         :shadow {:enabled? true
                                  :version "v2"
                                  :percent 10}}}
          normalized (protocol/normalize-protocol cfg)]
      (is (= "Base prompt" (get-in normalized [:prompts :default])))
      (is (= "V1 prompt" (get-in normalized [:versions :v1 :prompts :default])))
      (is (= "Use PL" (get-in normalized [:versions :v1 :intents :text/respond :system])))
      (is (= "V2 prompt" (get-in normalized [:versions :v2 :prompts :default])))
      (is (= :v1 (get-in normalized [:rollout :active])))
      (is (= :v2 (get-in normalized [:rollout :canary :version])))
      (is (= 15 (get-in normalized [:rollout :canary :percent])))
      (is (= :v2 (get-in normalized [:rollout :shadow :version])))
      (is (= 10 (get-in normalized [:rollout :shadow :percent]))))))

(deftest select-protocol-artifact-picks-request-active-or-canary-version
  (testing "Explicit request version wins when present."
    (let [cfg (protocol/normalize-protocol
               {:prompts {:default "Base"}
                :versions {:v1 {:prompts {:default "V1"}}
                           :v2 {:prompts {:default "V2"}}}
                :rollout {:active :v1
                          :canary {:enabled? true :version :v2 :percent 0}}})
          selected (protocol/select-protocol-artifact cfg {:trace-id "t-1"
                                                           :requested-version :v2})]
      (is (= :v2 (:artifact/version selected)))
      (is (= :request (:artifact/source selected)))
      (is (= "V2" (get-in selected [:protocol :prompts :default])))))

  (testing "Canary version is selected deterministically when enabled and bucket is in range."
    (let [cfg (protocol/normalize-protocol
               {:prompts {:default "Base"}
                :versions {:v1 {:prompts {:default "V1"}}
                           :v2 {:prompts {:default "V2"}}}
                :rollout {:active :v1
                          :canary {:enabled? true :version :v2 :percent 100}}})
          selected (protocol/select-protocol-artifact cfg {:trace-id "t-canary"})]
      (is (= :v2 (:artifact/version selected)))
      (is (= :canary (:artifact/source selected)))
      (is (= "V2" (get-in selected [:protocol :prompts :default])))))

  (testing "Active version is selected when request/canary do not apply."
    (let [cfg (protocol/normalize-protocol
               {:prompts {:default "Base"}
                :versions {:v1 {:prompts {:default "V1"}}
                           :v2 {:prompts {:default "V2"}}}
                :rollout {:active :v1
                          :canary {:enabled? true :version :v2 :percent 0}}})
          selected (protocol/select-protocol-artifact cfg {:trace-id "t-2"
                                                           :requested-version :v9})]
      (is (= :v1 (:artifact/version selected)))
      (is (= :active (:artifact/source selected)))
      (is (= "V1" (get-in selected [:protocol :prompts :default]))))))

(deftest select-protocol-shadow-artifact-selects-shadow-variant
  (testing "Shadow protocol is selected when enabled and bucket matches."
    (let [cfg (protocol/normalize-protocol
               {:prompts {:default "Base"}
                :versions {:v1 {:prompts {:default "V1"}}
                           :v2 {:prompts {:default "V2"}}}
                :rollout {:active :v1
                          :shadow {:enabled? true :version :v2 :percent 100}}})
          selected (protocol/select-protocol-shadow-artifact cfg {:trace-id "proto-shadow"})]
      (is (= true (:shadow/enabled? selected)))
      (is (= true (:shadow/applied? selected)))
      (is (= :v2 (:artifact/version selected)))
      (is (= :shadow (:artifact/source selected)))
      (is (= "V2" (get-in selected [:protocol :prompts :default])))))

  (testing "Shadow request override has priority when present."
    (let [cfg (protocol/normalize-protocol
               {:prompts {:default "Base"}
                :versions {:v1 {:prompts {:default "V1"}}
                           :v2 {:prompts {:default "V2"}}}
                :rollout {:active :v1
                          :shadow {:enabled? false :version :v2 :percent 0}}})
          selected (protocol/select-protocol-shadow-artifact
                    cfg
                    {:trace-id "proto-shadow-req"
                     :requested-version :v2})]
      (is (= true (:shadow/enabled? selected)))
      (is (= true (:shadow/applied? selected)))
      (is (= :v2 (:artifact/version selected)))
      (is (= :request (:artifact/source selected))))))
