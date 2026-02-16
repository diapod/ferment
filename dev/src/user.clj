(ns user

  (:require
   [clojure.spec.alpha                          :as                s]
   [orchestra.spec.test                         :as               st]
   [clojure.spec.test.alpha                     :as              cst]
   [clojure.spec.gen.alpha                      :as              gen]
   [clojure.repl                                :refer          :all]
   [clojure.test                                :refer [run-tests
                                                        run-all-tests]]
   [clojure.tools.namespace.repl                :refer [refresh
                                                        refresh-all]]
   [expound.alpha                               :as          expound]

   [ferment                       :as          ferment]
   [ferment.app                   :as              app]
   [ferment.utils                 :as             util]
   [ferment.utils.fs              :as               fs]
   [ferment.utils.map             :as              map]
   [ferment.cloverage-workaround :as   cov-workaround]

   [puget.printer                               :refer      [cprint]]
   [kaocha.repl                                 :refer          :all]
   [infra]))

(set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

(alter-var-root
 #'s/*explain-out*
 (constantly
  (expound/custom-printer {:show-valid-values? false
                           :print-specs?        true
                           :theme    :figwheel-theme})))

(when (System/getProperty "nrepl.load")
  (require 'ferment.nrepl))

(st/instrument)

(defn cloverage-workaround!
  "Installs a safe cloverage wrapper for REPL use."
  []
  (cov-workaround/install!))

(cloverage-workaround!)

(defn test-all []
  (refresh)
  (cst/with-instrument-disabled
    (run-all-tests)))

(alter-var-root #'*warn-on-reflection*
                (constantly true)
                (when (thread-bound? #'*warn-on-reflection*)
                  (set! *warn-on-reflection* true)))

;; (alter-var-root #'*unchecked-math*
;;                (constantly :warn-on-boxed)
;;                (when (thread-bound? #'*unchecked-math*)
;;                  (set! *unchecked-math* :warn-on-boxed)))

(comment 
  (refresh-all)
  (cst/with-instrument-disabled (test-all))
  (cst/with-instrument-disabled (run-all))
  )
