(ns

    ^{:doc    "ferment config readers."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.readers

  (:refer-clojure :exclude [ref])

  (:require [integrant.core             :as          ig]
            [maailma.core               :as        conf]
            [ferment                    :as   ferment]
            [tick.core                  :as           t]
            [clojure.java.io            :as          io]
            [clojure.string             :as         str]
            [io.randomseed.utils        :as       utils]
            [io.randomseed.utils.var    :as         var]
            [io.randomseed.utils.fs     :as          fs]))

(defn regex
  "Tagged literal reader function for regular expressions."
  [rgx]
  (re-pattern rgx))
