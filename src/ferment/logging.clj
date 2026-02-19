(ns

    ^{:doc    "Logging support for ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.logging

  (:require [potemkin                        :as                 p]
            [ferment.system                  :as            system]
            [buddy.core.hash                 :as              hash]
            [buddy.core.codecs               :as            codecs]
            [io.randomseed.utils.ip          :as                ip]
            [io.randomseed.utils.map         :as               map]
            [io.randomseed.utils.var         :as               var]
            [io.randomseed.utils.log         :as               log]
            [io.randomseed.utils             :refer [some-str-spc
                                                     some-str]])

  (:import  (logback_bundle.json                 FlatJsonLayout
                                                 ValueDecoder)
            (ch.qos.logback.contrib.jackson      JacksonJsonFormatter)
            (ch.qos.logback.core.encoder         LayoutWrappingEncoder)
            (ch.qos.logback.contrib.json.classic JsonLayout)
            (ch.qos.logback.classic.filter       ThresholdFilter)
            (ch.qos.logback.classic.encoder      PatternLayoutEncoder)
            (ch.qos.logback.core                 ConsoleAppender)
            (java.nio.charset                    Charset)))

(def ^:dynamic *pseudo-salt* "98jj348jvj28ncJIJ21398")

(def ^:dynamic *already-logged* false)

;;
;; Logging wrappers
;;

(p/import-vars [io.randomseed.utils.log
                default-config log-context with-ctx
                log trace debug info warn warning error fatal
                msg-with-val err-with-val msg err wrn dbg
                log-exceptions])

(defmacro web-msg
  [req msg & more]
  (if more
    (#'log/info &form &env (list* #'some-str-spc
                                  (list #'clojure.core/str
                                        "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                        (list #'some-str msg))
                                  more))
    (#'log/info &form &env (list  #'clojure.core/str
                                  "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                  (list #'some-str msg)))))

(defmacro web-wrn
  [req msg & more]
  (if more
    (#'log/warn &form &env (list* #'some-str-spc
                                  (list #'clojure.core/str
                                        "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                        (list #'some-str msg))
                                  more))
    (#'log/warn &form &env (list  #'clojure.core/str
                                  "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                  (list #'some-str msg)))))

(defmacro web-err
  [req msg & more]
  (if more
    (#'log/error &form &env (list* #'some-str-spc
                                   (list #'clojure.core/str
                                         "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                         (list #'some-str msg))
                                   more))
    (#'log/error &form &env (list  #'clojure.core/str
                                   "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                   (list #'some-str msg)))))

(defmacro web-dbg
  [req msg & more]
  (if more
    (#'log/debug &form &env (list* #'some-str-spc
                                   (list #'clojure.core/str
                                         "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                         (list #'some-str msg))
                                   more))
    (#'log/debug &form &env (list  #'clojure.core/str
                                   "[" (list #'clojure.core/or (list :uri req) "-") "]: "
                                   (list #'some-str msg)))))

;;
;; Context processing
;;

(defn mask         [_] "************")
(defn pseudonymize [v] (-> (hash/md5 (str v *pseudo-salt*)) (codecs/bytes->hex)))
(defn pr-session   [v] (if-not (map? v) v (assoc v :data "--------------" :api  "--------------")))

(def ctx-transformer
  {mask         [:user/password :user/repeated-password :repeated-password :password :pwd :private-key :private :secret :signature :request-id :anti-phisihng-code]
   pseudonymize [:user/login :login :user/email :email :user :username :nick :nickname]
   str          [:currency]
   pr-session   [:session]})

;;
;; Logging helpers
;;

(defn- ip->str
  [ip]
  (cond
    (ip/is-ip? ip) (ip/plain-ip-str ip)
    (string? ip)   ip
    (nil? ip)      nil
    :else          (some-str ip)))

(defn id-email
  ([user-id user-email]
   (some-str-spc user-email
                 (when user-id (str "(" user-id ")"))))
  ([user-id user-email ip-addr]
   (some-str-spc user-email
                 (when user-id (str "(" user-id ")"))
                 (when ip-addr (str "[" (ip->str ip-addr) "]")))))

(defn for-user
  ([user-id user-email]
   (when (or user-id user-email)
     (str "for " (id-email user-id user-email))))
  ([user-id user-email ip-addr]
   (when (or user-id user-email ip-addr)
     (if (or user-id user-email)
       (str "for " (id-email user-id user-email ip-addr))
       (str "for [" (ip->str ip-addr) "]")))))

;;
;; System handlers
;;

;; Oplog aggregate is a pure config/value branch.
;; Concrete logger workers (e.g. :ferment.oplog.auth/log) are initialized in
;; `ferment.oplog` and `ferment.oplog.auth`.
(derive :ferment.logging/oplog :ferment.system/value)

(defn prep-context-transformer
  [m]
  (when m (map/map-keys var/deref-symbol m)))

(system/add-expand
 ::unilog [k config]
 {k (log/preprocess-config
     (map/update-existing config :context-transformer prep-context-transformer))})

(system/add-init
 ::unilog
 [_ config]
 (log/init! (-> config
                (map/update-existing :context-transformer prep-context-transformer)
                (map/assoc-missing   :context-transformer ctx-transformer)))
 (msg-with-val
  "Configuration profile:" (or (some-str (:profile (:system config))) "unknown")
  (str "[" (or (some-str (:node (:system config))) "unknown") " node, log level: " (some-str (:level config)) "]")
  config))

(system/add-halt!
 ::unilog
 [_ config]
 (log/stop! config))
