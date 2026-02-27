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
