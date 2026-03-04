(ns ferment.training.redaction-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.training.redaction :as redaction]))

(deftest redact-event-redacts-keys-paths-and-patterns-deterministically
  (testing "Redaction pipeline scrubs secrets by key, explicit path and regex pattern with stable counters."
    (let [event {:training.event/id "e-1"
                 :request {:payload {:input {:token "secret-token"
                                             :email "john@example.com"
                                             :meta {:raw "Bearer abc.def.ghi"}}}}
                 :call {:out {:text "Kontakt: +48 600 700 800"}
                        :input {:authorization "Bearer qwe.123"}}}
          cfg {:enabled? true
               :placeholder "[MASKED]"
               :deny/keys [:token :authorization]
               :deny/paths [[:request :payload :input :email]]
               :deny/patterns ["(?i)bearer\\s+[a-z0-9._\\-]+"
                               "(?i)[a-z0-9._%+\\-]+@[a-z0-9.\\-]+\\.[a-z]{2,}"
                               "\\+?[0-9][0-9\\-\\s]{7,}[0-9]"]}
          {:keys [event audit]} (redaction/redact-event event cfg)]
      (is (= "[MASKED]" (get-in event [:request :payload :input :token])))
      (is (= "[MASKED]" (get-in event [:request :payload :input :email])))
      (is (= "[MASKED]" (get-in event [:call :input :authorization])))
      (is (= "[MASKED]" (get-in event [:request :payload :input :meta :raw])))
      (is (= "Kontakt: [MASKED]" (get-in event [:call :out :text])))
      (is (= 1 (:redacted/paths audit)))
      (is (= 2 (:redacted/keys audit)))
      (is (= 2 (:redacted/patterns audit))))))

(deftest redact-event-disabled-is-noop
  (testing "Disabled redaction preserves event payload and reports disabled audit."
    (let [event {:training.event/id "e-2"
                 :call {:out {:text "ok"}}}
          {:keys [event audit]} (redaction/redact-event event {:enabled? false})]
      (is (= "ok" (get-in event [:call :out :text])))
      (is (= false (:enabled? audit)))
      (is (= 0 (:redacted/paths audit)))
      (is (= 0 (:redacted/keys audit)))
      (is (= 0 (:redacted/patterns audit))))))
