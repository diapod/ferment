(ns ferment.training.export-events-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ferment.training.export-events :as export]))

(def ^:private replay-record
  {:trace/id "trace-1"
   :recorded-at "2026-03-04T10:20:30.123Z"
   :request {:payload {:trace {:id "trace-1"}
                       :task {:intent :text/respond}
                       :input {:prompt "Wyjasnij ACID."}}
             :prepared {:trace {:id "trace-1"}
                        :request/id "req-1"
                        :task {:intent :text/respond
                               :requires {:out-schema :res/text}}
                        :input {:prompt "Wyjasnij ACID."}}
             :resolved {:trace {:id "trace-1"}
                        :request/id "req-1"
                        :task {:intent :text/respond
                               :requires {:out-schema :res/text}}
                        :input {:prompt "Wyjasnij ACID."}}}
   :routing {:mode :meta-decider
             :routed? true
             :cap/decision {:cap/id :llm/voice}}
   :policy {:snapshot-id "8f3a12cd"
            :snapshot {:intent :text/respond}}
   :response {:status 200
              :outcome :ok
              :error/type nil
              :body {:result {:type :value
                              :out {:text "ACID to atomowosc, spojnosc, izolacja i trwalosc."}
                              :plan/run {:transcript [{:op :call
                                                       :intent :text/respond
                                                       :cap/id :llm/voice
                                                       :as :voice
                                                       :attempt 1
                                                       :candidate-index 0
                                                       :input {:prompt "Wyjasnij ACID."}
                                                       :result/type :value
                                                       :out {:text "ACID to atomowosc, spojnosc, izolacja i trwalosc."}
                                                       :latency-ms 18.2}]}}}}
   :diagnostics {:execution-path {:intent :text/respond
                                  :selected-cap/id :llm/voice}}
   :timing {:elapsed-ms 52.1}})

(deftest replay-records->events-maps-transcript-1-to-1
  (testing "One replay transcript call produces one training event with aligned fields."
    (let [events (export/replay-records->events [{:ok? true
                                                  :trace/id "trace-1"
                                                  :replay replay-record}])
          event (first events)]
      (is (= 1 (count events)))
      (is (= 1 (:training.event/version event)))
      (is (= :call-attempt (:training.event/type event)))
      (is (= "trace-1" (get-in event [:source :trace/id])))
      (is (= "req-1" (get-in event [:source :request/id])))
      (is (= "8f3a12cd" (get-in event [:source :replay/snapshot-id])))
      (is (= :text/respond (get-in event [:call :intent])))
      (is (= :llm/voice (get-in event [:call :cap/id])))
      (is (= :value (get-in event [:call :result/type])))
      (is (= true (get-in event [:labels :accepted?])))
      (is (= 18.2 (get-in event [:timing :call/latency-ms]))))))

(deftest events->train-rows-defaults-to-accepted-only
  (testing "Failed attempts are excluded from SFT output unless explicitly requested."
    (let [ok-event (first (export/replay-records->events [replay-record]))
          fail-event (assoc-in ok-event [:call :failure/type] :schema/invalid)
          fail-event (assoc-in fail-event [:labels :accepted?] false)
          default-rows (export/events->train-rows [ok-event fail-event] {})
          all-rows (export/events->train-rows [ok-event fail-event] {:include-failed? true})]
      (is (= 1 (count default-rows)))
      (is (= 2 (count all-rows)))
      (is (= (:training.event/id ok-event)
             (:id (first default-rows)))))))

(deftest event->train-row-emits-edn-prompt-and-completion
  (testing "Train row uses EDN prompt/completion strings and carries trace metadata."
    (let [event (first (export/replay-records->events [replay-record]))
          row   (export/event->train-row event)]
      (is (string? (:prompt row)))
      (is (string? (:completion row)))
      (is (= "trace-1" (get-in row [:meta :trace_id])))
      (is (= "req-1" (get-in row [:meta :request_id]))))))

(deftest events->train-rows-support-messages-and-chatml-target-format
  (testing "messages/chatml target formats emit role-based rows ready for chat trainers."
    (let [event (first (export/replay-records->events [replay-record]))
          messages-rows (export/events->train-rows [event] {:target-format :messages})
          chatml-rows (export/events->train-rows [event] {:target-format :chatml})
          msg-row (first messages-rows)
          chat-row (first chatml-rows)]
      (is (= 1 (count messages-rows)))
      (is (= 1 (count chatml-rows)))
      (is (vector? (:messages msg-row)))
      (is (vector? (:messages chat-row)))
      (is (= ["system" "user" "assistant"]
             (mapv :role (:messages msg-row))))
      (is (= ["system" "user" "assistant"]
             (mapv :role (:messages chat-row))))
      (is (every? string? (map :content (:messages msg-row))))
      (is (every? string? (map :content (:messages chat-row))))
      (is (nil? (:prompt msg-row)))
      (is (nil? (:completion msg-row))))))

(deftest events->train-rows-applies-custom-sanity-hook
  (testing "Custom sanity hook can accept or reject exported rows deterministically."
    (let [event (first (export/replay-records->events [replay-record]))
          accepted (export/events->train-rows
                    [event]
                    {:target-format :messages
                     :sanity-check {:enabled? true
                                    :row/fn (fn [row]
                                              (and (vector? (:messages row))
                                                   (= "assistant"
                                                      (get-in row [:messages 2 :role]))))}})]
      (is (= 1 (count accepted)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"custom sanity hook"
           (export/events->train-rows
            [event]
            {:target-format :messages
             :sanity-check {:enabled? true
                            :row/fn (constantly false)}}))))))

(deftest replay-records->events-apply-redaction-before-export
  (testing "Default export options apply strict redaction in request/call/output branches."
    (let [record (assoc-in replay-record
                           [:request :payload :input]
                           {:prompt "Wyjasnij ACID."
                            :token "abc-secret"
                            :email "john@example.com"})
          record (assoc-in record
                           [:response :body :result :plan/run :transcript 0 :out :text]
                           "Contact me at john@example.com, token: Bearer abc.def")
          event (first (export/replay-records->events [record]))
          row (first (export/events->train-rows [event] {:include-failed? true}))]
      (is (= "[REDACTED]" (get-in event [:request :payload :input :token])))
      (is (= "[REDACTED]" (get-in event [:request :payload :input :email])))
      (is (= "Contact me at [REDACTED], token: [REDACTED]"
             (get-in event [:call :out :text])))
      (is (not (str/includes? (:prompt row) "john@example.com")))
      (is (not (str/includes? (:completion row) "Bearer abc.def"))))))

(deftest replay-records->events-attach-stable-judge-labels
  (testing "Judge labels are deterministic for fixed replay snapshot and include reject taxonomy."
    (let [record (assoc-in replay-record
                           [:response :body :result :plan/run :transcript 0 :out :text]
                           "<think>internal</think>")
          event-a (first (export/replay-records->events [record]))
          event-b (first (export/replay-records->events [record]))
          judge-a (get-in event-a [:labels :judge])
          judge-b (get-in event-b [:labels :judge])]
      (is (= judge-a judge-b))
      (is (= true (:judge/enabled? judge-a)))
      (is (= :rules-only (:judge/mode judge-a)))
      (is (= false (:judge/pass? judge-a)))
      (is (= [:no-internal-markers] (:judge/failed-rules judge-a)))
      (is (= :judge/rules-failed (:judge/reject-reason judge-a))))))
