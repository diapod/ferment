(ns

    ^{:doc    "ferment service, scrypt algorithm."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.algo.scrypt

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import (com.lambdaworks.crypto SCrypt))

  (:require [ferment.auth.pwd       :as        pwd]
            [io.randomseed.utils.map :as         map]
            [io.randomseed.utils.map :refer [qassoc]]
            [io.randomseed.utils     :refer     :all]))

(def ^:const default-options
  {:cpu-cost 32768
   :mem-cost     8
   :parallel     1})

(def ^:const required-keys
  [:salt :cpu-cost :mem-cost :parallel])

(defn encrypt
  "Encrypt a password string using the scrypt algorithm."
  {:arglists '([plain]
               [plain options]
               [plain salt]
               [plain options settings]
               [plain salt settings])}
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (let [options    (if (or (nil? options) (map? options)) options {:salt options})
         options    (conj default-options
                          (map/remove-empty-values (select-keys settings required-keys))
                          (map/remove-empty-values (select-keys options  required-keys)))
         ^"[B" salt (to-bytes (or (get options :salt) (pwd/salt-bytes 16)))
         result     (SCrypt/scrypt
                     ^"[B" (text-to-bytes plain)
                     salt
                     (int (get options :cpu-cost))
                     (int (get options :mem-cost))
                     (int (get options :parallel))
                     (int 32))]
     (qassoc options :salt salt :password result))))

(def check (partial pwd/standard-check encrypt))

(def handler
  {:encrypt-fn encrypt
   :check-fn   check
   :defaults   default-options
   :shared     [:cpu-cost :mem-cost :parallel]})
