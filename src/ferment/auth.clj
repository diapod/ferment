(ns

    ^{:doc    "ferment service, authentication."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [ferment.db             :as        db]
            [ferment.db.sql         :as       sql]
            [ferment.logging        :as       log]
            [ferment.system         :as    system]
            [ferment.proto.auth     :as         p]
            [ferment.types.auth     :refer   :all]
            [ferment                :refer   :all]
            [io.randomseed.utils.auth :as  auth-utils]
            [io.randomseed.utils.auth.types :as auth-types]
            [io.randomseed.utils      :refer   :all]
            [io.randomseed.utils.time :as      time]
            [io.randomseed.utils.var  :as       var]
            [io.randomseed.utils.map  :as       map]
            [tick.core                :as         t])

  (:import (ferment                 AccountTypes
                                    AuthConfirmation
                                    AuthConfig
                                    AuthSettings)
           (clojure.lang           Keyword
                                   Associative
                                   IPersistentMap)
           (io.randomseed.utils.auth.types AuthLocking
                                           AuthPasswords
                                           Suites
                                           SuitesJSON)
           (io.randomseed.lazy_map LazyMap)
           (javax.sql              DataSource)
           (java.time              Duration)))

(defonce ^:redef setup nil)

(def confirmation-expires-default (t/new-duration 10 :minutes))

;; Access to settings and configuration

(defn settings
  "Returns authentication settings for the given authentication settings source
  `src`."
  ^AuthSettings [src] (p/settings src))

(defn settings?
  "Returns `true` if the given object is an instance of `AuthSettings`."
  [v]
  (instance? AuthSettings v))

(defn config
  "Returns an authentication configuration for the given account type `account-type`
  using authentication settings source `src`. If the second argument is not given it
  will use a default account type."
  (^AuthConfig [src] (p/config src))
  (^AuthConfig [src ^Keyword account-type] (p/config src account-type)))

(defn db
  "Returns an authentication database connection object using the given authentication
  settings source `src` and optional account type `account-type`."
  (^DataSource [src] (p/db src))
  (^DataSource [src account-type] (p/db src account-type)))

(defn config-by-type
  "Returns authentication configuration for the given account type using an
  authentication configuration map."
  [settings-src account-type]
  (config settings-src account-type))

(defn config-by-type-with-var
  "Returns authentication configuration for the given `account-type` using an
  authentication settings map stored in a Var of the given (fully-qualified) name
  `var-name`."
  [var-name account-type]
  (config-by-type (var/deref var-name) account-type))

;; Password authentication

(defn- ->auth-passwords-record
  [passwords]
  (when passwords
    (if (instance? AuthPasswords passwords)
      passwords
      (auth-types/->AuthPasswords (or (:id passwords) (:handler-id passwords))
                                  (:suite passwords)
                                  (or (:check passwords) (:check-fn passwords))
                                  (or (:check-json passwords) (:check-json-fn passwords))
                                  (or (:encrypt passwords) (:encrypt-fn passwords))
                                  (or (:encrypt-json passwords) (:encrypt-json-fn passwords))
                                  (or (:wait passwords) (:wait-fn passwords))))))

(defn- ->auth-locking-record
  [locking]
  (when locking
    (if (instance? AuthLocking locking)
      locking
      (auth-types/->AuthLocking (:max-attempts locking)
                                (:lock-wait locking)
                                (:fail-expires locking)))))

(defn- ->suites-record
  [suites]
  (if (and (map? suites)
           (contains? suites :shared)
           (contains? suites :intrinsic))
    (if (instance? Suites suites)
      suites
      (auth-types/->Suites (:shared suites) (:intrinsic suites)))
    suites))

(defn- ->suites-json-record
  [suites]
  (if (and (map? suites)
           (contains? suites :shared)
           (contains? suites :intrinsic))
    (if (instance? SuitesJSON suites)
      suites
      (auth-types/->SuitesJSON (:shared suites) (:intrinsic suites)))
    suites))

(defn check-password
  "Checks password for a user against an encrypted password given in password
  suites. Specific authentication configuration map must be given."
  ([password pwd-suites auth-config]
   (auth-utils/check-password password
                              (->suites-record pwd-suites)
                              auth-config))
  ([password pwd-shared-suite pwd-user-suite auth-config]
   (auth-utils/check-password password
                              pwd-shared-suite
                              pwd-user-suite
                              auth-config)))

(defn check-password-json
  "Checks password for a user against a JSON-encoded password suites. Specific
  authentication configuration map must be given."
  ([password json-pwd-suites auth-config]
   (auth-utils/check-password-json password
                                   (->suites-json-record json-pwd-suites)
                                   auth-config))
  ([password json-pwd-shared-suite json-pwd-user-suite auth-config]
   (auth-utils/check-password-json password
                                   json-pwd-shared-suite
                                   json-pwd-user-suite
                                   auth-config)))

(defn make-password
  "Creates new password for a user. Specific authentication configuration map must be
  given."
  [password auth-config]
  (some-> (auth-utils/make-password password auth-config)
          ->suites-record))

(defn make-password-json
  "Creates new password for a user in JSON format. Specific authentication
  configuration map must be given."
  [password auth-config]
  (some-> (auth-utils/make-password-json password auth-config)
          ->suites-json-record))

;; Authenticable implementation

(extend-protocol p/Authenticable

  AuthSettings

  (settings
    ^AuthSettings [settings-src]
    settings-src)

  (config
    (^AuthConfig [settings-src]
     (.default ^AuthSettings settings-src))
    (^AuthConfig [settings-src account-type]
     (if account-type
       (get (.types ^AuthSettings settings-src)
            (if (keyword? account-type) account-type (keyword account-type))))))

  (db
    (^DataSource [settings-src]
     (.db ^AuthSettings settings-src))
    (^DataSource [settings-src account-type]
     (if account-type
       (let [at (if (keyword? account-type) account-type (keyword account-type))]
         (if-some [^AuthConfig ac (get (.types ^AuthSettings settings-src) at)]
           (.db ^AuthConfig ac))))))

  AuthConfig

  (config
    (^AuthConfig [config-source] config-source)
    (^AuthConfig [config-source account-type]
     (if (or (not account-type)
             (contains?
              (.ids ^AccountTypes (.account-types ^AuthConfig config-source))
              (if (keyword? account-type) account-type (keyword account-type))))
       config-source)))

  (db
    (^DataSource [settings-src]
     (.db ^AuthConfig settings-src))
    (^DataSource [settings-src account-type]
     (if (or (not account-type)
             (contains?
              (.ids ^AccountTypes (.account-types ^AuthConfig settings-src))
              (if (keyword? account-type) account-type (keyword account-type))))
       (.db ^AuthConfig settings-src))))

  DataSource

  (db
    (^DataSource [settings-src]
     settings-src))

  ;; Match

  ;; (settings
  ;;   ^AuthSettings [m]
  ;;   (get (.data ^Match m) :auth/setup))

  ;; (config
  ;;   (^AuthConfig [m]
  ;;    (if-some [^AuthSettings as (get (.data ^Match m) :auth/setup)]
  ;;      (.default ^AuthSettings as)))
  ;;   (^AuthConfig [m account-type]
  ;;    (if account-type
  ;;      (if-some [^AuthSettings as (get (.data ^Match m) :auth/setup)]
  ;;        (get (.types ^AuthSettings as)
  ;;             (if (keyword? account-type) account-type (keyword account-type)))))))

  ;; (db
  ;;   (^DataSource [m]
  ;;    (if-some [as (get (.data ^Match m) :auth/setup)]
  ;;      (.db ^AuthSettings as)))
  ;;   (^DataSource [m account-type]
  ;;    (if account-type
  ;;      (if-some [^AuthSettings as (get (.data ^Match m) :auth/setup)]
  ;;        (let [at (if (keyword? account-type) account-type (keyword account-type))]
  ;;          (if-some [^AuthConfig ac (get (.types ^AuthSettings as) at)]
  ;;            (.db ^AuthConfig ac)))))))

  ;; Associative

  ;; (settings
  ;;   ^AuthSettings [req]
  ;;   (http/get-route-data req :auth/setup))

  ;;(config
  ;;(^AuthConfig [req]
  ;;   (if-some [^AuthSettings as (http/get-route-data req :auth/setup)]
  ;;    (.default ^AuthSettings as)))
  ;; (^AuthConfig [req account-type]
  ;;  (if account-type
  ;;    (if-some [^AuthSettings as (http/get-route-data req :auth/setup)]
  ;;      (get (.types ^AuthSettings as)
  ;;           (if (keyword? account-type) account-type (keyword account-type)))))))

  ;; (db
  ;;   (^DataSource [req]
  ;;    (if-some [^AuthSettings as (http/get-route-data req :auth/setup)]
  ;;      (.db ^AuthSettings as)))
  ;;   (^DataSource [req account-type]
  ;;    (if account-type
  ;;      (if-some [^AuthSettings as (http/get-route-data req :auth/setup)]
  ;;        (let [at (if (keyword? account-type) account-type (keyword account-type))]
  ;;          (if-some [^AuthConfig ac (get (.types ^AuthSettings as) at)]
  ;;            (.db ^AuthConfig ac)))))))

  nil

  (settings [settings-src] nil)

  (config
    ([settings-src] nil)
    ([settings-src account-type] nil))

  (db
    ([settings-src] nil)
    ([settings-src account-type] nil)))

;; Settings initialization

(defn make-passwords
  [m]
  (if (instance? AuthPasswords m)
    m
    (some-> (auth-utils/make-passwords m)
            ->auth-passwords-record)))

(defn parse-account-ids
  ([v]
   (parse-account-ids some-keyword-simple v))
  ([f v]
   (if v
     (some->> (if (coll? v) (if (map? v) (keys v) v) (cons v nil))
              seq (filter valuable?) (map f) (filter keyword?) seq))))

(defn new-account-types
  ([ids]
   (new-account-types ids nil))
  ([ids default-id]
   (let [ids (some->> ids parse-account-ids (filter identity) distinct seq)
         dfl (or (some-keyword-simple default-id) (first ids))
         dfn (if dfl (name dfl))
         ids (if dfl (conj ids dfl))
         ids (if ids (set ids))
         nms (if ids (mapv name ids))
         sql (if ids (if (= 1 (count nms)) " = ?" (str " IN " (sql/braced-join-? nms))))]
     (->AccountTypes sql ids nms dfl dfn))))

(defn make-account-types
  [m]
  (if (instance? AccountTypes m) m
      (let [act (:account-types m)
            act (if (instance? AccountTypes act) (:ids act) act)
            act (if act (parse-account-ids act))
            ids (some->> [:account-types/ids :account-types/names]
                         (map (partial get m))
                         (apply concat act))]
        (new-account-types ids (or (:account-types/default m)
                                   (:account-types/default-name m))))))

(defn make-confirmation
  [m]
  (if (instance? AuthConfirmation m) m
      (->AuthConfirmation
       (safe-parse-long (:confirmation/max-attempts m) 3)
       ((fnil time/parse-duration [1 :minutes]) (:confirmation/expires m)))))

(defn make-locking
  [m]
  (if (instance? AuthLocking m)
    m
    (some-> (auth-utils/make-locking m)
            ->auth-locking-record)))

(defn make-auth
  ([m]
   (make-auth nil m))
  ([k m]
   (if (instance? AuthConfig m)
     m
     (let [m         (or m {})
           base      (or (auth-utils/make-auth k m) {})
           id        (keyword (or (:id base) (:id m) k))
           db-src    (db/ds (or (:db m) (:db base)))
           passwords (or (some-> (:passwords base) ->auth-passwords-record)
                         (make-passwords m))
           locking   (or (some-> (:locking base) ->auth-locking-record)
                         (make-locking m))]
       (map->AuthConfig {:id            id
                         :db            db-src
                         :passwords     passwords
                         :account-types (make-account-types m)
                         :locking       locking
                         :confirmation  (make-confirmation m)})))))

(defn init-auth
  "Authentication configurator."
  [k config]
  (log/msg "Configuring auth engine" k
           (str "(attempts: "  (:locking/max-attempts config)
                ", lock wait: "    (time/seconds  (:locking/lock-wait    config)) " s"
                ", lock expires: " (time/seconds  (:locking/fail-expires config)) " s)"))
  (make-auth k config))

(defn index-by-type
  "Prepares static authentication preference map by mapping a copy of each
  authentication configuration (of type `AuthConfig`) to any account type identifier
  found within it. So, `[{:account-types {:ids [:a :b]}}]` becomes:
  `{:a {:account-types {:ids [:a :b]}}, :b {:account-types {:ids [:a :b]}}`.

  Additionally, it sets `:db` from global settings and updates `:account-types` field
  to have current account type set as its default (including SQL query). Original
  account types data is preserved under `:parent-account-types`. Each authentication
  configuration will be initialized if it isn't already."
  [coll db]
  (->> coll
       (filter map?)
       (map #(assoc % :account-types (make-account-types %) :db (db/ds db)))
       (mapcat #(map list (map keyword (:ids (:account-types %))) (repeat %)))
       (filter #(and (coll? %) (keyword? (first %)) (map? (second %))))
       (map (fn [[id auth-config]]
              (if-some [id (some-keyword-simple id)]
                (vector
                 id
                 (make-auth (or (:id auth-config) id)
                            (assoc auth-config
                                   :parent-account-types (:account-types auth-config)
                                   :account-types (new-account-types id)))))))
       (filter identity)
       (into {})))

(defn init-config
  "Prepares authentication settings."
  [config]
  (let [config (or config {})
        config (auth-utils/init-config config)
        config (map/update-existing config :db db/ds)
        config (update config :types index-by-type (:db config))]
    (-> config
        (assoc :default (get (:types config) (:default-type config)))
        map->AuthSettings)))

(system/add-init  ::auth [k config] (init-auth k config))
(system/add-halt! ::auth [_ config] nil)

(system/add-init  ::setup [k config] (var/make k (init-config config)))
(system/add-halt! ::setup [k config] (var/make k nil))

(derive ::strong ::auth)
(derive ::simple ::auth)
(derive ::setup  ::system/var-make)
