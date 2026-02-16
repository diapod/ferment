(ns

    ^{:doc    "Sample data generator interface for ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.generator

  (:require [ferment.core                :as  ferment]
            [clojure.test.check.rose-tree  :as  rose]
            [clojure.test.check.generators :as  gens])

  (:import (java.util Random)))

(def ferment
  (gens/no-shrink
   (clojure.test.check.generators/->Generator
    (fn [^Random rng _]
      (rose/make-rose
       (ferment/generate nil nil (constantly true) nil nil (.nextLong rng))
       [])))))
