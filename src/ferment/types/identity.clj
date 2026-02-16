(ns

    ^{:doc    "ferment service, identity record type."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.types.identity

  (:require [ferment]))

(in-ns 'ferment.identity)

(clojure.core/declare to-str*)

(in-ns 'ferment)

(defrecord Identity [^clojure.lang.Keyword id-type value]
  Object
  (toString ^String [^Identity i] (ferment.identity/to-str* ^Identity i)))
