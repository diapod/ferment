(ns

    ^{:doc    "Public specs of ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.spec

  (:require [ferment                 :as             GW]
            [ferment.core            :as      ferment]
            [io.randomseed.utils       :as          utils]
            [ferment.locale          :as         locale]
            [clojure.spec.alpha        :as              s]
            [orchestra.spec.test       :as             st]
            [clojure.spec.gen.alpha    :as            gen]))

;;
;; Namespaces for easy use of keywords
;;

(alias 'arg   (create-ns 'ferment.arg))   ;; for internal arg specs
(alias 'args  (create-ns 'ferment.args))  ;; for internal args specs
(alias 'prop  (create-ns 'ferment.prop))  ;; for additional properties

(alias 'input (create-ns 'ferment.input)) ;; in for public input specs

