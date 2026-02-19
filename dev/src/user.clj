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
   [ferment.model                 :as            model]
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

(defn model-invoke!
  "REPL helper for diagnostic model calls through runtime workers.

  Uses current `ferment.app/state` by default."
  ([model-id payload]
   (model/diagnostic-invoke! app/state model-id payload))
  ([system model-id payload]
   (model/diagnostic-invoke! system model-id payload)))

(def ^:private ns->profile
  {'user  :dev
   'dev   :dev
   'test  :test
   'test-live :test-live
   'prod  :prod
   'admin :admin})

(defn current-profile
  "Infers runtime profile from current namespace.

  Defaults to `:dev` for unknown namespaces."
  []
  (or (get ns->profile (ns-name *ns*)) :dev))

(defn- start-for-profile!
  [profile keys]
  (case profile
    :dev   (apply app/start-dev! keys)
    :test  (apply app/start-test! keys)
    :test-live (apply app/start-test-live! keys)
    :admin (apply app/start-admin! keys)
    :prod  (apply app/start! keys)
    (throw (ex-info "Unsupported runtime profile."
                    {:profile profile
                     :supported (set (vals ns->profile))}))))

(defn- stop-for-profile!
  [_profile keys]
  (apply app/stop! keys))

(defn- restart-for-profile!
  [profile keys]
  ;; Deterministic restart for selected profile:
  ;; stop selected keys and start the same key-set using profile-specific config dirs.
  (apply stop-for-profile! profile keys)
  (apply start-for-profile! profile keys))

(defn- status-for-profile
  [profile ns-sym]
  {:profile profile
   :namespace ns-sym
   :phase app/phase
   :configured? (app/configured?)
   :running? (app/running?)
   :stopped? (app/stopped?)
   :suspended? (app/suspended?)
   :failed? (app/failed?)
   :start-args app/start-args
   :state-keys (some-> app/state keys sort vec)})

(defn start!
  "Starts app for profile inferred from caller namespace."
  [& keys]
  (start-for-profile! (current-profile) keys))

(defn stop!
  "Stops app (or selected keys)."
  [& keys]
  (stop-for-profile! (current-profile) keys))

(defn restart!
  "Restarts app for profile inferred from caller namespace."
  [& keys]
  (restart-for-profile! (current-profile) keys))

(defn status!
  "Returns a concise runtime status map for profile inferred from caller namespace."
  []
  (status-for-profile (current-profile) (ns-name *ns*)))

(defn- install-profile-wrappers!
  []
  (doseq [[ns-sym profile] (dissoc ns->profile 'user)]
    (let [target-ns (or (find-ns ns-sym) (create-ns ns-sym))]
      (binding [*ns* target-ns]
        (clojure.core/refer 'clojure.core)
        (intern *ns* 'start! (fn [& keys]
                               (start-for-profile! profile keys)))
        (intern *ns* 'stop! (fn [& keys]
                              (stop-for-profile! profile keys)))
        (intern *ns* 'restart! (fn [& keys]
                                 (restart-for-profile! profile keys)))
        (intern *ns* 'status! (fn []
                                (status-for-profile profile ns-sym)))))))

(install-profile-wrappers!)
