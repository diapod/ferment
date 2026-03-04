(ns

    ^{:doc    "Rule-based judge labels for training events."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.judge

  (:require [clojure.string :as str]))

(def ^:private default-mode
  :disabled)

(def ^:private default-rules
  [:no-internal-markers
   :non-empty-output-text
   :accepted-consistent])

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (trim-s v)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- normalize-rules
  [v]
  (let [rules (->> (cond
                     (sequential? v) v
                     (some? v) [v]
                     :else [])
                   (keep keywordish)
                   vec)]
    (if (seq rules)
      rules
      default-rules)))

(defn normalize-config
  "Normalizes judge config.

  Input keys:
  - `:mode` (`:disabled`/`:rules-only`/`:teacher`)
  - `:constitution/ref`
  - `:rules`."
  [cfg]
  (let [cfg' (if (map? cfg) cfg {})
        mode (or (keywordish (:mode cfg')) default-mode)
        mode' (if (contains? #{:disabled :rules-only :teacher} mode)
                mode
                default-mode)]
    {:mode mode'
     :constitution/ref (trim-s (:constitution/ref cfg'))
     :rules (normalize-rules (:rules cfg'))}))

(defn- rule-no-internal-markers
  [event]
  (let [marker? (true? (get-in event [:redaction :internal-markers/present?]))
        text (or (get-in event [:call :out :text])
                 (get-in event [:response :body :result :out :text]))]
    (if (and (string? text)
             (re-find #"<(?:think|tool_call)>" text))
      false
      (not marker?))))

(defn- rule-non-empty-output-text
  [event]
  (some? (trim-s (or (get-in event [:call :out :text])
                     (get-in event [:response :body :result :out :text])))))

(defn- rule-accepted-consistent
  [event]
  (let [accepted? (true? (get-in event [:labels :accepted?]))
        failure-type (keywordish (get-in event [:call :failure/type]))]
    (if accepted?
      (nil? failure-type)
      true)))

(defn- rule-fn
  [rule]
  (case rule
    :no-internal-markers rule-no-internal-markers
    :non-empty-output-text rule-non-empty-output-text
    :accepted-consistent rule-accepted-consistent
    nil))

(defn evaluate!
  "Evaluates canonical training event and returns normalized verdict map."
  ([event]
   (evaluate! event nil))
  ([event cfg]
   (let [{:keys [mode rules] :as cfg'} (normalize-config cfg)
         constitution-ref (:constitution/ref cfg')]
     (if (= :disabled mode)
       {:judge/enabled? false
        :judge/mode :disabled
        :judge/pass? true
        :judge/score 1.0
        :judge/rules []
        :judge/failed-rules []
        :judge/reject-reason nil
        :judge/constitution-ref constitution-ref}
       (let [results (->> rules
                          (mapv (fn [rule]
                                  (let [rule-fn' (rule-fn rule)
                                        pass? (if (fn? rule-fn')
                                                (true? (rule-fn' event))
                                                false)]
                                    {:rule rule
                                     :pass? pass?
                                     :reason (when-not pass?
                                               (or (when (nil? rule-fn') :rule/unknown)
                                                   :rule/failed))}))))
             failed (->> results
                         (filterv (comp false? :pass?))
                         (mapv :rule))
             total (count results)
             passed (- total (count failed))
             score (if (pos? total)
                     (/ (double passed) (double total))
                     1.0)
             pass? (empty? failed)]
         {:judge/enabled? true
          :judge/mode mode
          :judge/pass? pass?
          :judge/score score
          :judge/rules results
          :judge/failed-rules failed
          :judge/reject-reason (when-not pass? :judge/rules-failed)
          :judge/constitution-ref constitution-ref
          :judge/config cfg'})))))
