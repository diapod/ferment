(ns

    ^{:doc    "ferment service, operation logger."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.oplog

  (:refer-clojure :exclude [parse-long uuid random-uuid run!])

  (:require [clojure.core.async              :as      async]
            [ferment.logging                 :as        log]
            [ferment.system                  :as     system]
            [ferment.db                      :as         db]
            [ferment.app                     :as        app]
            [io.randomseed.utils.time        :as       time]
            [io.randomseed.utils.var         :as        var]
            [io.randomseed.utils.map         :as        map]
            [io.randomseed.utils             :refer    :all]))

(defonce ^:redef log nil)

;; Queue and channel helpers

(defn queue
  "Creates new FIFO queue. If `coll` is given, new queue will be initialized with
  elements from it."
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn drain!
  "Drains the channel by getting all elements from it until nothing is left to get."
  [chan]
  (when chan (take-while some? (repeatedly #(async/poll! chan)))))

;; Runner

(defn run!
  "Starts logger thread which waits for new data to be put on queue whenever there are
  any on communication channel (`:channel/messages` key of the `config`). If the
  timeout is exceeded (`:timeout` key) or if the queue holds more messages than a
  number under `:buffered-max` configuration key, will run the function passed under
  the `:writer` key of the configuration map to actually insert all of the
  accumulated messages into a database. The thread will finish its work (after
  draining the channel and flushing queue) when there is any value received on its
  kill channel (`:channel/kill`)."
  [{:keys [id db table timeout
           fn/writer-buffered fn/writer
           channel/kill channel/messages]
    :as   config}]
  (if (and (ident? id) (keyword? table) (db/data-source? db)
           (fn? writer) (fn? writer-buffered) kill messages)
    (let [id             (some-keyword id)
          table          (some-keyword-simple table)
          timeout        (time/milliseconds timeout 5000)
          chans-to-watch [messages kill]
          new-timeout    #(async/timeout timeout)]
      (log/msg "Starting operations logger:" id)
      (assoc config :channel/runner
             (async/thread
               (log/dbg "Buffered OP Logger" id "is started")
               (loop [tout      (new-timeout)
                      msg-queue (queue)]
                 (let [[msg chan] (async/alts!! (conj chans-to-watch tout))]
                   (condp = chan

                     ;; new message
                     messages (recur (new-timeout) (writer-buffered (conj msg-queue msg)))

                     ;; timeout
                     tout (recur (new-timeout) (writer msg-queue))

                     ;; kill command received
                     kill (do (log/dbg "Buffered OP Logger" id "is shutting down")
                              (async/close! kill) (async/close! messages)
                              (->> messages drain! (into msg-queue) writer))))))))
    config))

;; Logging

;; Operations logging

(defn auth-config
  "Returns default auth operations logger configuration obtained from a system config."
  ([]
   (auth-config app/state))
  ([config]
   (or (some-> config (get :oplog) (get :auth))
       (some-> config (get :ferment.logging/oplog) (get :auth))
       (get config :ferment.oplog.auth/log))))

(defn act-config
  "Returns default `/v1/act` operations logger configuration obtained from a system config."
  ([]
   (act-config app/state))
  ([config]
   (or (some-> config (get :oplog) (get :act))
       (some-> config (get :ferment.logging/oplog) (get :act))
       (get config :ferment.oplog.act/log))))

(defn config
  "Returns default operations logger configuration obtained from a system config. Takes
  a subsystem name."
  ([sub-name]
   (config sub-name app/state))
  ([sub-name cfg]
   (let [sub-k (some-keyword-simple sub-name)]
     (or (some-> cfg (get :oplog) (get sub-k))
         (some-> cfg (get :ferment.logging/oplog) (get sub-k))
         (when sub-k
           (get cfg (keyword (str "ferment.oplog." (name sub-k)) "log")))))))

(defn get-config
  "Returns default operations logger configuration obtained from a system config. Takes
  a key under which logger configuration should reside."
  ([k]
   (get-config k app/state))
  ([k cfg]
   (get cfg (keyword k))))

(defn reporter
  "Returns a reporter function of a logger."
  ([sub-name]
   (some-> (config sub-name) (get :fn/reporter)))
  ([sub-name cfg]
   (some-> (config sub-name cfg) (get :fn/reporter))))

(defn get-reporter
  "Returns a reporter function of a logger."
  ([k]
   (some-> (get-config k) (get :fn/reporter)))
  ([k cfg]
   (some-> (get-config k cfg) (get :fn/reporter))))

(defn auth-reporter
  "Returns a reporter function of an auth logger."
  ([]
   (some-> (auth-config) (get :fn/reporter)))
  ([cfg]
   (some-> (auth-config cfg) (get :fn/reporter))))

(defn act-reporter
  "Returns a reporter function of `/v1/act` logger."
  ([]
   (some-> (act-config) (get :fn/reporter)))
  ([cfg]
   (some-> (act-config cfg) (get :fn/reporter))))

(defn logger
  "Retrieves operations logger function from a given `cfg` (via the given sub-name
  translated to a key using `config` function and then the `:fn/reporter` key) and
  creates a wrapper for handling keyword arguments."
  ([sub-name]
   (if-some [rep (reporter sub-name)]
     (fn [& {:as message}] (rep message))
     (constantly nil)))
  ([sub-name cfg]
   (if-some [rep (reporter sub-name cfg)]
     (fn [& {:as message}] (rep message))
     (constantly nil))))

(defn get-logger
  "Retrieves operations logger function from a given `cfg` (via the given key and then
  the `:fn/reporter` key of a system configuration) and creates a wrapper for
  handling keyword arguments."
  ([k]
   (if-some [rep (get-reporter k)]
     (fn [& {:as message}] (rep message))
     (constantly nil)))
  ([k cfg]
   (if-some [rep (get-reporter k cfg)]
     (fn [& {:as message}] (rep message))
     (constantly nil))))

(defn auth-logger
  "Retrieves operations auth logger function from the given `cfg` (using
  `oplog-auth-config` and then the `:fn/reporter` key of a system configuration) and
  creates a wrapper for handling keyword arguments."
  ([]
   (if-some [rep (auth-reporter)]
     (fn [& {:as message}] (rep message))
     (constantly nil)))
  ([cfg]
   (if-some [rep (auth-reporter cfg)]
     (fn [& {:as message}] (rep message))
     (constantly nil))))

(defn act-logger
  "Retrieves operations `/v1/act` logger function from the given `cfg` and
  creates a wrapper for handling keyword arguments."
  ([]
   (if-some [rep (act-reporter)]
     (fn [& {:as message}] (rep message))
     (constantly nil)))
  ([cfg]
   (if-some [rep (act-reporter cfg)]
     (fn [& {:as message}] (rep message))
     (constantly nil))))

;; Initialization

(defn init!
  "Initializes operations logging by starting a separate thread listening for log
  messages on a channel which is shared with a generated reporting function using
  lexical closure.

  The writer (`:writer`) should be a function taking the following arguments:
  `id` (will be used to pass a configuration key `k`), `db` (will be used to pass a
  database connection (a data source), if set under `:db` configuration key),
  `table` (will be set to a table name, if set under the `:table` configuration key),
  `messages` (will be set to a queue of messages to be written). If there is no
  `:writer` option given in the configuration map, the initializer will try to find
  one by using `k` with a string `-writer` attached (and the converted to a
  symbol).

  The reporter (`:reporter`) should be a function taking the following arguments:
  `id` (will be used to pass a configuration key `k`), `channel` (will be used to
  pass a channel for sending messages to the listening thread), `message` (will be
  used to pass a message. The responsibility for handling the message is delegated to
  the writer function, with the exception that a message cannot be nil (such messages
  should be ignored).

  A call of the reporter function is wrapped into anonymous function which closes
  over initialized `k` and `channel` values, providing the call with two first
  arguments. This new function is exposed under `:fn/reporter` configuration key
  associated by the initializer within a returned map.

  So, if no `:reporter` nor `:writer` is set in configuration, and the configuration
  key is `:ferment.oplog.auth/log` then the defaults would be:
  `ferment.oplog.auth/log-reporter` (reporting) and
  `ferment.oplog.auth/log-writer` (writing). Both must be defined."
  [k {:keys [db table writer reporter buffered-max timeout] :as config}]
  (if (and db (ident? k) (valuable? table))
    (let [db          (db/ds db)
          writer-id   (some-symbol writer)
          writer-fn   (var/deref writer-id)
          writer-id   (if writer-fn writer-id (some-symbol (str (some-str k) "-writer")))
          writer-fn   (or writer-fn (var/deref writer-id))
          reporter-id (some-symbol reporter)
          reporter-fn (var/deref reporter-id)
          reporter-id (if reporter-fn reporter-id (some-symbol (str (some-str k) "-reporter")))
          reporter-fn (or reporter-fn (var/deref reporter-id))]
      (if (and (fn? writer-fn) (fn? reporter-fn) (db/data-source? db))
        (let [id            (keyword k)
              table         (some-keyword-simple table)
              buffered-max  (safe-parse-long buffered-max 108)
              timeout       (time/parse-duration (or timeout 5) :seconds)
              buffer-size   (+ 2 (* 3 buffered-max))
              mesg-buff     (async/buffer buffer-size)
              kill-chan     (async/chan)
              mesg-chan     (async/chan mesg-buff)
              reporter-fn   #(reporter-fn id mesg-chan %)
              writer-nbf-fn #(writer-fn id db table %)
              writer-buf-fn #(if (> buffered-max (count %)) % (writer-fn id db table %))]
          (-> config
              (dissoc :writer :reporter)
              (assoc :id                   k
                     :db                   db
                     :table                table
                     :messages-buffer-size buffer-size
                     :fn/writer            writer-nbf-fn
                     :fn/writer-buffered   writer-buf-fn
                     :fn/reporter          reporter-fn
                     :fn/reporter-name     reporter-id
                     :channel/messages     mesg-chan
                     :channel/kill         kill-chan)
              run!))))))

(defn stop!
  "Stops the operations logger by sending a message via the given kill
  channel (`:channel/kill` configuration key) and waiting for the listening thread to
  finish its work."
  [k {:keys [fn/reporter-name channel/kill channel/runner]}]
  (when kill
    (log/msg "Stopping operations logger:" k)
    (async/put! kill true)
    (when runner (async/<!! runner))
    (log/msg "Operations logger" k "stopped"))
  nil)

(system/add-expand ::log [k config] {k (map/assoc-missing config :table (some-keyword-simple k))})
(system/add-init   ::log [k config] (let [c (init! k config)] (var/make k (:fn/reporter c)) c))
(system/add-halt!  ::log [k config] (stop! k config) (var/make k nil))
