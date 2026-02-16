(ns

    ^{:doc    "ferment service, plain-text appender."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.algo.append

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string          :as         str]
            [ferment.auth.pwd      :as         pwd]
            [io.randomseed.utils.map :as         map]
            [io.randomseed.utils.map :refer [qassoc]]
            [io.randomseed.utils     :refer     :all]))

(def ^:const default-options       {})
(def ^:const required-keys         [:prefix :suffix])
(def ^:const default-random-length 8)
(def ^:const default-charset       (vec "abcdefghijklmnopqrstuvwzyxABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
(def ^:const re-rnd                #"\{\{RND(?:\s*)(\d+)?\}\}")

(defn parse-random
  [v charset]
  (if (string? v)
    (str/replace
     v re-rnd
     (fn [m] (pwd/salt-string (to-long (last m) default-random-length) charset)))
    v))

(defn encrypt
  "Append the given prefix and/or suffix to a password."
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (let [options  (if (or (nil? options) (map? options)) options {})
         no-check (not (get options :checking false))
         salt-set (get options :salt-charset default-charset)
         options  (cond-> options
                    true     (select-keys required-keys)
                    no-check (map/update-existing :prefix parse-random salt-set)
                    no-check (map/update-existing :suffix parse-random salt-set)
                    true     (map/update-to-bytes :prefix :suffix)
                    true     map/remove-empty-values)
         options  (conj default-options options)
         result   (bytes-concat (get options :prefix bzero)
                                (text-to-bytes plain)
                                (get options :suffix bzero))]
     (qassoc options :password result))))

(def check (partial pwd/standard-check encrypt))

(def handler
  {:encrypt-fn encrypt
   :check-fn   check})
