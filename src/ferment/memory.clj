(ns

    ^{:doc    "Working-memory facade over session service and session store."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.memory

  (:require [ferment.session :as session]
            [ferment.session.store :as store]))

(defn service?
  [svc]
  (and (map? svc)
       (fn? (:open! svc))))

(defn open!
  ([session-service sid]
   (session/open! session-service sid))
  ([session-service sid opts]
   (session/open! session-service sid opts)))

(defn get!
  [session-service sid]
  (session/get! session-service sid))

(defn list!
  [session-service]
  (session/list! session-service))

(defn thaw!
  ([session-service sid]
   (session/thaw! session-service sid))
  ([session-service sid opts]
   (session/thaw! session-service sid opts)))

(defn freeze!
  ([session-service sid]
   (session/freeze! session-service sid))
  ([session-service sid opts]
   (session/freeze! session-service sid opts)))

(defn append-turn!
  [session-service sid turn]
  (session/append-turn! session-service sid turn))

(defn get-var!
  ([session-service sid k]
   (session/get-var! session-service sid k))
  ([session-service sid k opts]
   (session/get-var! session-service sid k opts)))

(defn get-vars!
  ([session-service sid ks]
   (session/get-vars! session-service sid ks))
  ([session-service sid ks opts]
   (session/get-vars! session-service sid ks opts)))

(defn put-var!
  ([session-service sid k v]
   (session/put-var! session-service sid k v))
  ([session-service sid k v opts]
   (session/put-var! session-service sid k v opts)))

(defn put-vars!
  ([session-service sid kvs]
   (session/put-vars! session-service sid kvs))
  ([session-service sid kvs opts]
   (session/put-vars! session-service sid kvs opts)))

(defn del-var!
  ([session-service sid k]
   (session/del-var! session-service sid k))
  ([session-service sid k opts]
   (session/del-var! session-service sid k opts)))

(defn del-vars!
  ([session-service sid ks]
   (session/del-vars! session-service sid ks))
  ([session-service sid ks opts]
   (session/del-vars! session-service sid ks opts)))

(defn del-all-vars!
  ([session-service sid]
   (session/del-all-vars! session-service sid))
  ([session-service sid opts]
   (session/del-all-vars! session-service sid opts)))

(defn request-default-bindings
  "Returns declarative session var -> request path bindings."
  [session-store]
  (store/request-default-bindings session-store))

