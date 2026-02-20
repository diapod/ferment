(ns

    ^{:doc    "ferment service, /v1/act operation logger."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.oplog.act

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.core.async      :as      async]
            [clojure.string          :as        str]
            [tick.core               :as          t]
            [ferment.db              :as         db]
            [ferment.oplog           :as      oplog]
            [io.randomseed.utils     :refer    :all]))

(def ^:const actlog-fields
  [:trace-id
   :request-id
   :session-id
   :principal-id
   :principal-email
   :principal-account-type
   :principal-roles
   :intent
   :capability
   :outcome
   :status
   :error-type
   :latency-ms
   :executed
   :message])

(defn- nonblank
  [v]
  (some-> v some-str str/trim not-empty))

(defn- kw-text
  [v]
  (some-> (or (some-keyword v)
              (when (string? v)
                (some-keyword v)))
          some-str))

(defn- parse-double-safe
  [v]
  (cond
    (number? v) (double v)
    (string? v) (try
                  (Double/parseDouble (str/trim v))
                  (catch Throwable _ nil))
    :else nil))

(defn- outcome-text
  [v]
  (let [k (some-keyword-simple v)]
    (if (contains? #{:ok :error} k)
      (name k)
      "error")))

(defn- prep-oplog
  [_db [{:keys [trace-id request-id session-id principal-id principal-email
                principal-account-type principal-roles
                intent capability outcome status error-type latency-ms
                message executed time]
         :as   m}
        ts]]
  (when (map? m)
    (let [trace-id      (nonblank trace-id)
          intent        (kw-text intent)
          capability    (kw-text capability)
          outcome       (outcome-text outcome)
          status        (safe-parse-long status 500)
          principal-id  (safe-parse-long principal-id)
          account-type  (kw-text principal-account-type)
          roles         (when (seq principal-roles)
                          (pr-str (vec principal-roles)))
          request-id    (nonblank request-id)
          session-id    (nonblank session-id)
          principal-email (nonblank principal-email)
          error-type    (kw-text error-type)
          latency-ms    (parse-double-safe latency-ms)
          executed      (or executed time ts)
          message       (nonblank message)]
      (when (and trace-id intent)
        [trace-id request-id session-id principal-id principal-email account-type roles
         intent capability outcome status error-type latency-ms executed message]))))

(defn log-writer
  "Writes buffered `/v1/act` audit events to DB table."
  [id db table messages]
  (if-some [mseq (seq messages)]
    (do
      (if-some [msgs (->> mseq (map #(prep-oplog db %)) (filter identity) seq)]
        (db/insert-or-ignore-multi! db table actlog-fields msgs db/opts-simple-vec))
      (empty messages))
    messages))

(defn log-reporter
  "Reports one `/v1/act` audit event to logger channel."
  [id channel message]
  (when (and (map? message) channel)
    (async/>!! channel [message (t/now)])))

(derive ::log ::oplog/log)
