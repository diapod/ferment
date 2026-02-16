(ns

    ^{:doc    "Protocol configuration branch for Ferment (meta-language and envelope)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.protocol

  (:require [ferment.system :as system]))

(defn normalize-protocol
  "Applies lightweight defaults to protocol config."
  [config]
  (-> config
      (update :proto/version #(or % 1))
      (update :transport/content-type #(or % :application/edn))
      (update :retry/max-attempts #(or % 3))))

(derive ::default :ferment.system/value)

(system/add-expand
 ::default
 [k config]
 {k (normalize-protocol config)})
