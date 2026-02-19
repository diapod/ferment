(ns

    ^{:doc    "ferment service, auth operations logger."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.oplog.auth

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.core.async       :as      async]
            [tick.core                :as          t]
            [ferment.logging          :as        log]
            [ferment.system           :as     system]
            [ferment.db               :as         db]
            [ferment.oplog            :as      oplog]
            [io.randomseed.utils.ip   :as         ip]
            [io.randomseed.utils.var  :as        var]
            [io.randomseed.utils.map  :as        map]
            [io.randomseed.utils      :refer    :all]))

(defonce ^:redef log nil)

(def ^:const authlog-levels
  {:dbg           "debug"
   :debug         "debug"
   :msg           "info"
   :log           "info"
   :inf           "info"
   :nfo           "info"
   :info          "info"
   :information   "info"
   :informational "info"
   :message       "info"
   :not           "notice"
   :ntc           "notice"
   :notic         "notice"
   :notice        "notice"
   :wrn           "warning"
   :warn          "warning"
   :warning       "warning"
   :err           "error"
   :error         "error"
   :crit          "critical"
   :critical      "critical"
   :alrt          "alert"
   :alert         "alert"
   :mrg           "emergency"
   :emerg         "emergency"
   :emergency     "emergency"})

(def ^:const authlog-default-level
  "info")

(def ^:const authlog-fields
  [:user-id :client-ip :operation :success :level :executed :message])

(defn- normalize-client-ip
  [v]
  (some-> (or (some-str v) v)
          ip/to-address
          ip/to-v6
          ip/to-str-v6))

(defn- prep-oplog
  [db [{:keys [user-id client-ip client-id op operation success ok ok? message msg level executed time]
        :or   {success :missing, ok :missing, ok? :missing}
        :as   m}
       ts]]
  (if-some [operation (or (and operation (some-str operation))
                          (and op (some-str op))
                          (if-not (map? m) (some-str m)))]
    (let [user-id   (safe-parse-long user-id)
          client-ip (normalize-client-ip (or client-ip client-id))
          executed  (or executed time ts)
          success   (boolean (first (remove #{:missing} [success ok ok? true])))
          level     (get authlog-levels (some-keyword level) authlog-default-level)
          message   (or (some-str message) (some-str msg))]
      [user-id client-ip operation success level executed message])))

(defn log-writer
  "Authentication log buffered writer. For the given `id` (configuration key, ignored),
  database connection (`db`), database table (`table`) and a queue of
  messages (`messages`) performs multiple insert operation to a database and flushes
  the queue. Returns empty queue which should be reused by the writing thread."
  [id db table messages]
  (if-some [mseq (seq messages)]
    (do (if-some [msgs (->> mseq (map #(prep-oplog db %)) (filter identity) seq)]
          (db/insert-or-ignore-multi! db table authlog-fields msgs db/opts-simple-vec))
        (empty messages))
    messages))

(defn log-reporter
  "Report an event to a database using `channel`."
  [id channel
   {:keys [user-id client-ip client-id op operation msg message success ok ok? level executed time]
    :as   m}]
  (if (and m channel)
    (async/>!! channel [m (t/now)])))

(derive ::log ::oplog/log)
