(ns

    ^{:doc    "Promotion gate policy for student evaluation reports."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.promotion

  (:require [clojure.string :as str]))

(def ^:private default-required-suites
  [:protocol-conformance
   :constitution-compliance
   :regression])

(def ^:private default-thresholds
  {:overall/pass-rate-min 0.85
   :suite-pass-rate-min {:protocol-conformance 0.90
                         :constitution-compliance 0.90
                         :regression 0.90}
   :max-failed-cases nil
   :max-failed-by-suite {}})

(def ^:private default-config
  {:enabled? true
   :blocking? true
   :required-suites default-required-suites
   :thresholds default-thresholds})

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

(defn- parse-bool
  [v default]
  (cond
    (boolean? v) v
    (nil? v) default
    (number? v) (not (zero? (long v)))
    (string? v) (let [s (-> v str/trim str/lower-case)]
                  (if (contains? #{"1" "true" "yes" "on"} s)
                    true
                    (if (contains? #{"0" "false" "no" "off"} s)
                      false
                      default)))
    :else default))

(defn- parse-double-safe
  [v]
  (cond
    (number? v) (double v)
    (string? v) (try
                  (Double/parseDouble (str/trim v))
                  (catch Throwable _ nil))
    :else nil))

(defn- parse-long-safe
  [v]
  (cond
    (integer? v) (long v)
    (number? v) (long (Math/floor (double v)))
    (string? v) (try
                  (Long/parseLong (str/trim v))
                  (catch Throwable _ nil))
    :else nil))

(defn- normalize-suite-coll
  [v]
  (let [items (->> (cond
                     (sequential? v) v
                     (some? v) [v]
                     :else [])
                   (keep keywordish)
                   vec)]
    (if (seq items)
      items
      default-required-suites)))

(defn- normalize-thresholds
  [thresholds]
  (let [src (if (map? thresholds) thresholds {})
        suite-min-src (if (map? (:suite-pass-rate-min src))
                        (:suite-pass-rate-min src)
                        {})
        max-failed-by-suite-src (if (map? (:max-failed-by-suite src))
                                  (:max-failed-by-suite src)
                                  {})
        suite-min (->> suite-min-src
                       (map (fn [[suite threshold]]
                              [(keywordish suite)
                               (parse-double-safe threshold)]))
                       (filter (fn [[suite threshold]]
                                 (and (keyword? suite)
                                      (number? threshold))))
                       (into {}))
        max-failed-by-suite (->> max-failed-by-suite-src
                                 (map (fn [[suite limit]]
                                        [(keywordish suite)
                                         (parse-long-safe limit)]))
                                 (filter (fn [[suite limit]]
                                           (and (keyword? suite)
                                                (some? limit)
                                                (not (neg? limit)))))
                                 (into {}))]
    {:overall/pass-rate-min (or (parse-double-safe (:overall/pass-rate-min src))
                                (:overall/pass-rate-min default-thresholds))
     :suite-pass-rate-min (merge (:suite-pass-rate-min default-thresholds)
                                 suite-min)
     :max-failed-cases (let [n (parse-long-safe (:max-failed-cases src))]
                         (when (and (some? n) (not (neg? n)))
                           n))
     :max-failed-by-suite max-failed-by-suite}))

(defn normalize-config
  "Normalizes promotion gate config."
  [cfg]
  (let [cfg' (if (map? cfg) cfg {})]
    {:enabled? (parse-bool (:enabled? cfg') true)
     :blocking? (parse-bool (:blocking? cfg') true)
     :required-suites (normalize-suite-coll (:required-suites cfg'))
     :thresholds (normalize-thresholds (:thresholds cfg'))}))

(defn- suite-keyword
  [suite]
  (or (keywordish suite)
      :unknown))

(defn- report-overall
  [eval-report]
  (let [overall (if (map? (get-in eval-report [:summary :overall]))
                  (get-in eval-report [:summary :overall])
                  {})]
    {:total (or (parse-long-safe (:total overall)) 0)
     :passed (or (parse-long-safe (:passed overall)) 0)
     :failed (or (parse-long-safe (:failed overall)) 0)
     :pass-rate (or (parse-double-safe (:pass-rate overall)) 0.0)}))

(defn- report-by-suite
  [eval-report]
  (let [suite-map (if (map? (get-in eval-report [:summary :by-suite]))
                    (get-in eval-report [:summary :by-suite])
                    {})]
    (->> suite-map
         (map (fn [[suite v]]
                [(suite-keyword suite)
                 {:total (or (parse-long-safe (:total v)) 0)
                  :passed (or (parse-long-safe (:passed v)) 0)
                  :failed (or (parse-long-safe (:failed v)) 0)
                  :pass-rate (or (parse-double-safe (:pass-rate v)) 0.0)}]))
         (into {}))))

(defn evaluate-report
  "Evaluates an eval report against promotion policy.

  Returns deterministic promotion decision map."
  ([eval-report]
   (evaluate-report eval-report nil))
  ([eval-report cfg]
   (let [{:keys [enabled? blocking? required-suites thresholds] :as cfg'}
         (normalize-config cfg)
         overall (report-overall eval-report)
         by-suite (report-by-suite eval-report)
         present-suites (set (keys by-suite))]
     (if-not enabled?
       {:promotion/version 1
        :promotion/status :disabled
        :promotion/eligible? true
        :promotion/blocking? blocking?
        :promotion/reasons []
        :promotion/config cfg'
        :promotion/metrics {:overall overall
                            :by-suite by-suite}}
       (let [required-set (set required-suites)
             missing-suites (->> required-set
                                 (remove present-suites)
                                 sort
                                 vec)
             overall-min (:overall/pass-rate-min thresholds)
             max-failed-cases (:max-failed-cases thresholds)
             suite-min-map (if (map? (:suite-pass-rate-min thresholds))
                             (:suite-pass-rate-min thresholds)
                             {})
             max-failed-by-suite (if (map? (:max-failed-by-suite thresholds))
                                   (:max-failed-by-suite thresholds)
                                   {})
             reasons (cond-> []
                       (seq missing-suites)
                       (conj {:reason :promotion/missing-required-suites
                              :required required-suites
                              :present (->> present-suites sort vec)
                              :missing missing-suites})

                       (and (number? overall-min)
                            (< (:pass-rate overall) overall-min))
                       (conj {:reason :promotion/overall-pass-rate-below-min
                              :threshold overall-min
                              :actual (:pass-rate overall)})

                       (and (some? max-failed-cases)
                            (> (:failed overall) max-failed-cases))
                       (conj {:reason :promotion/failed-cases-above-max
                              :threshold max-failed-cases
                              :actual (:failed overall)}))
             reasons (reduce (fn [acc [suite threshold]]
                               (let [suite' (suite-keyword suite)
                                     pass-rate (get-in by-suite [suite' :pass-rate] 0.0)]
                                 (if (and (number? threshold)
                                          (< pass-rate threshold))
                                   (conj acc {:reason :promotion/suite-pass-rate-below-min
                                              :suite suite'
                                              :threshold threshold
                                              :actual pass-rate})
                                   acc)))
                             reasons
                             suite-min-map)
             reasons (reduce (fn [acc [suite max-failed]]
                               (let [suite' (suite-keyword suite)
                                     failed (get-in by-suite [suite' :failed] 0)]
                                 (if (and (some? max-failed)
                                          (> failed max-failed))
                                   (conj acc {:reason :promotion/suite-failed-cases-above-max
                                              :suite suite'
                                              :threshold max-failed
                                              :actual failed})
                                   acc)))
                             reasons
                             max-failed-by-suite)
             eligible? (empty? reasons)]
         {:promotion/version 1
          :promotion/status (if eligible? :accepted :rejected)
          :promotion/eligible? eligible?
          :promotion/blocking? blocking?
          :promotion/reasons reasons
          :promotion/config cfg'
          :promotion/metrics {:overall overall
                              :by-suite by-suite}
          :promotion/report-ref {:eval/version (:eval/version eval-report)
                                 :cases/evaluated (:cases/evaluated eval-report)}})))))
