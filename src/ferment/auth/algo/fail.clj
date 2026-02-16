(ns

    ^{:doc    "ferment service, appender which always fails."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.algo.fail

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [ferment.auth.pwd      :as         pwd]
            [io.randomseed.utils.map :as         map]
            [io.randomseed.utils.map :refer [qassoc]]
            [io.randomseed.utils     :refer     :all]))

(defn encrypt
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (if (map? options)
     (qassoc options :password nil)
     {:password nil})))

(def check (partial pwd/standard-check (constantly nil)))

(def handler
  {:encrypt-fn encrypt
   :check-fn   check})
