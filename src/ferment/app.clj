(ns

    ^{:doc    "ferment service, application."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.app

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [puget.printer            :refer [cprint pprint]]
            [ns-tracker.core          :as         ns-tracker]
            [ferment.system           :as               system]
            [ferment.logging          :as                  log]
            [ferment.env              :as                  env]
            [ferment.router]
            [ferment.session]
            [tick.core                :as                  t]
            [io.randomseed.utils.map  :as                map]
            [io.randomseed.utils.var  :as                var]
            [io.randomseed.utils      :refer            :all]))

(set! *warn-on-reflection* true)

;;
;; defaults
;;

(def ^:dynamic *ns-reload-watch-dirs*             ["src" "test"])
(def ^:dynamic *local-config*                                nil)

(def ^:dynamic *resource-config-dirs* (system/profile-resource-dirs :prod))
(def ^:dynamic *resource-admin-dirs*  (system/profile-resource-dirs :admin))
(def ^:dynamic *resource-dev-dirs*    (system/profile-resource-dirs :dev))
(def ^:dynamic *resource-test-dirs*   (system/profile-resource-dirs :test))
(def ^:dynamic *resource-test-live-dirs* (system/profile-resource-dirs :test-live))

(defmacro with-config-dirs
  "Binds `ferment.app/*resource-config-dirs*` to `dirs` for the dynamic extent
  of `body`. Intended for ad-hoc overrides of the default resource config
  directories."
  [dirs & body]
  `(binding [*resource-config-dirs* ~dirs]
     ~@body))

(defmacro with-local-config
  "Binds `ferment.app/*local-config*` to `local-file` for the dynamic extent of `body`.
  Useful for temporarily overriding the local (filesystem) config source."
  [local-file & body]
  `(binding [*local-config* ~local-file]
     ~@body))

(defmacro with-configs
  "Binds all config-source dynamic vars for the dynamic extent of `body`:
  `ferment.app/*resource-config-dirs*`, `ferment.app/*resource-dev-dirs*`,
  `ferment.app/*resource-admin-dirs*`, `ferment.app/*resource-test-dirs*`, and
  `ferment.app/*local-config*`.

  Useful for instantiating `app.clj` with custom defaults and building wrappers
  around common management functions such as `ferment.app/start!`,
  `ferment.app/stop!`, `ferment.app/reload!`, etc."
  [local-file dirs dev-dirs admin-dirs test-dirs & body]
  `(binding [*resource-config-dirs* ~dirs
             *resource-dev-dirs*    ~dev-dirs
             *resource-admin-dirs*  ~admin-dirs
             *resource-test-dirs*   ~test-dirs
             *local-config*         ~local-file]
     ~@body))

(defmacro with-watch-dirs
  "Binds `ferment.app/*ns-reload-watch-dirs*` to `watch-dirs` for the dynamic extent
  of `body`. Useful when building a custom namespace tracker (see
  `ferment.app/*ns-tracker*`)."
  [watch-dirs & body]
  `(binding [*ns-reload-watch-dirs* ~watch-dirs]
     ~@body))

(defmacro with-ns-tracker
  "Binds `ferment.app/*ns-tracker*` to `tracker` for the dynamic extent of `body`.
  The tracker is typically created with `ferment.app/make-ns-tracker` and later
  used by `ferment.app/reload-namespaces` / `ferment.app/reload!`."
  [ns-tracker & body]
  `(binding [*ns-tracker* ~ns-tracker]
     ~@body))

;;
;; application init key (used to force deps, e.g. with logger)
;;

(derive ::init ::system/nil)

;;
;; property names
;;

(system/add-init
 ::properties
 [_ config]
 (let [config (-> config
                  (update :name        normalize-name "unnamed system")
                  (update :title       normalize-name "unnamed system")
                  (update :author      normalize-name "unknown author")
                  (update :profile     some-keyword)
                  (update :node        some-keyword)
                  (update :version     normalize-name "1.0.0")
                  (update :license     normalize-name "Copyright")
                  (update :description normalize-name ""))]
   config))

;;
;; time zone reference
;;

(derive ::timezone ::system/value)

;;
;; hot reloading
;;

(defn make-ns-tracker
  "Creates an `ns-tracker` instance for tracking code changes in directories configured
  by `ferment.app/*ns-reload-watch-dirs*`. Returns nil when no watch dirs are
  configured."
  []
  (if-some [wdirs *ns-reload-watch-dirs*]
    (if-some [wdirs (and (sequential? wdirs) (seq wdirs))]
      (ns-tracker/ns-tracker wdirs))))

(def ^:dynamic *ns-tracker*
  (make-ns-tracker))

(defn reload-namespaces
  "Reloads namespaces reported by `ferment.app/*ns-tracker*` using
  `clojure.core/require` with the `:reload` flag. Does nothing when no tracker is
  configured."
  []
  (if-some [nstracker *ns-tracker*]
    (doseq [ns-sym (nstracker)]
      (require ns-sym :reload))))

;;
;; direct references
;;

(derive ::db                ::system/var-make)
(derive ::auth-db           ::system/var-make)
(derive ::logger            ::system/var-make)
(derive ::http-server       ::system/var-make)
(derive ::http-handler      ::system/var-make)
(derive ::http-router       ::system/var-make)
(derive ::http-routes       ::system/var-make)
(derive ::http-middleware   ::system/var-make)

;;
;; url
;;

(system/add-init  ::url [k url] (var/make k (some-str url)))
(system/add-halt! ::url [k url] (var/make k nil))

;;
;; application state management
;;

(defonce ^:private ^:redef lock 'lock)

(defonce ^:redef config            nil)  ;; configuration which was read from files
(defonce ^:redef post-config       nil)  ;; configuration prepared by parser
(defonce ^:redef state             nil)  ;; current state of this application
(defonce ^:redef exception         nil)  ;; unhandled exception
(defonce ^:redef start-args         [])  ;; arguments used to start this application
(defonce ^:redef phase        :stopped)  ;; phase flag

(defn expanding?   [] (locking lock (identical? :expanding  phase)))
(defn expanded?    [] (locking lock (identical? :expanded   phase)))
(defn starting?    [] (locking lock (identical? :starting   phase)))
(defn failed?      [] (locking lock (identical? :failed     phase)))
(defn running?     [] (locking lock (identical? :running    phase)))
(defn stopping?    [] (locking lock (identical? :stopping   phase)))
(defn stopped?     [] (locking lock (identical? :stopped    phase)))
(defn suspended?   [] (locking lock (identical? :suspended  phase)))
(defn resuming?    [] (locking lock (identical? :resuming   phase)))
(defn suspending?  [] (locking lock (identical? :suspending phase)))
(defn configured?  [] (locking lock (some? post-config)))
(defn start-args?  [] (locking lock (not (empty? start-args))))

(defdoc! expanding?
  "Returns true when the application is currently in the `:expanding` phase.")

(defdoc! expanded?
  "Returns true when the last expand/prep operation finished and the application is in the
`:expanded` phase.")

(defdoc! starting?
  "Returns true when the application is currently in the `:starting` phase.")

(defdoc! failed?
  "Returns true when the application is in the `:failed` phase (i.e. the last lifecycle
operation ended with an exception stored in `ferment.app/exception`).")

(defdoc! running?
  "Returns true when the application is running (phase is `:running`).")

(defdoc! stopping?
  "Returns true when the application is currently stopping (phase is `:stopping`).")

(defdoc! stopped?
  "Returns true when the application is stopped (phase is `:stopped`).")

(defdoc! suspended?
  "Returns true when the application is suspended (phase is `:suspended`).")

(defdoc! resuming?
  "Returns true when the application is currently resuming (phase is `:resuming`).")

(defdoc! suspending?
  "Returns true when the application is currently suspending (phase is
  `:suspending`).")

(defdoc! configured?
  "Returns true when `ferment.app/post-config` is present (i.e. configuration has
  been read and expanded/prepared).")

(defdoc! start-args?
  "Returns true when the application has remembered startup arguments in
  `ferment.app/start-args`.")

;;
;; application management helpers
;;

(declare start-app)
(declare restart-app)
(declare resume-app)

(defn state-from-exception
  "Takes an exception object and updates the global application state to reflect
  failure.

  Sets:
  - `ferment.app/exception` to the exception,
  - `ferment.app/phase` to `:failed`,
  and logs the exception (error level).

  Returns `:failed`."
  [ex]
  (locking lock
    (var/reset exception ex)
    (var/reset state (:system (ex-data ex)))
    (log/err "Exception during " (normalize-name phase) ": " (ex-message ex) ": " (ex-cause ex))
    (var/reset phase :failed)))

(defn configure-app
  "Reads and expands configuration for the application.

  `local-config-file` is a filesystem config overlay (EDN or ENV). `rc-dirs` is a
  list (or single value) of classpath resource directories scanned for `.edn` and
  `.env` configuration sources.

  Uses `ferment.system/read-configs` to produce `ferment.app/config`, then uses
  `ferment.system/expand` to produce `ferment.app/post-config`.

  When `keys` are provided, `config` is only read if it is not already present, and
  only the specified keys are expanded.

  Returns `:configured`."
  ([]
   (configure-app nil nil))
  ([local-config-file rc-dirs & keys]
   (let [rc-dirs (if (coll? rc-dirs) rc-dirs (cons rc-dirs nil))]
     (locking lock
       (if-some [keys (seq keys)]
         (do (when-not config
               (var/reset config (apply system/read-configs local-config-file rc-dirs)))
             (var/reset post-config (system/expand config identity keys)))
         (do (if (and (nil? local-config-file) (nil? rc-dirs))
               (var/reset config (apply system/read-configs config))
               (var/reset config (apply system/read-configs local-config-file rc-dirs)))
             (var/reset post-config (system/expand config))))))
   :configured))

(defn start-app
  "Starts (initializes) the application Integrant system.

  If the app is suspended, delegates to `ferment.app/resume-app`. If the app is not
  configured, calls `ferment.app/configure-app` with the given config sources.
  Then initializes the Integrant system via `ferment.system/init`, optionally
  restricted to `keys`.

  Remembers startup arguments in `ferment.app/start-args` and updates lifecycle
  `phase`.  Returns the current `phase` keyword."
  ([]
   (start-app nil nil))
  ([local-config-file rc-dirs & keys]
   (locking lock
     (if (suspended?)
       (apply resume-app keys)
       (try
         (when-not (configured?) (apply configure-app local-config-file rc-dirs keys))
         (var/reset start-args [local-config-file rc-dirs keys])
         (if-some [keys (seq keys)]
           (do
             (var/reset phase    :starting)
             ;;(apply configure-app local-config-file rc-dirs keys)
             (var/reset state    (system/init post-config keys))
             (var/reset phase    :running)
             (var/reset exception nil))
           (when (stopped?)
             (var/reset phase     :starting)
             (var/reset state     (system/init post-config))
             (var/reset phase     :running)
             (var/reset exception nil)))
         (catch Throwable e (state-from-exception e))))
     phase)))

(defn stop-app
  "Stops (halts) the application Integrant system.

  With no `keys`, halts the whole system, clears `ferment.app/state`,
  `ferment.app/config`, and `ferment.app/post-config`, and sets phase to
  `:stopped`.

  With `keys`, halts only the selected components and nils them out in
  `ferment.app/state`.

  Returns the current `phase` keyword."
  [& keys]
  (locking lock
    (when-not (stopped?)
      (try
        (var/reset phase :stopping)
        (if-some [keys (seq keys)]
          (do (when-some [s state] (system/halt! s keys))
              (var/reset state         (map/nil-existing-keys state keys))
              (var/reset exception     nil))
          (do (when-some [s state] (system/halt! s))
              (var/reset state         nil)
              (var/reset post-config   nil)
              (var/reset config        nil)
              (var/reset phase    :stopped)
              (var/reset exception     nil)))
        (catch Throwable e (state-from-exception e))))
    phase))

(defn restart-app
  "Restarts the application.

  Stops selected `keys` (or the last remembered key set from
  `ferment.app/start-args`) and then starts again using the last remembered startup
  arguments when available; otherwise uses the current dynamic
  defaults (`ferment.app/*local-config*` and
  `ferment.app/*resource-config-dirs*`)."
  [& keys]
  (locking lock
    (let [new-keys (or (seq keys) (seq (peek start-args)))]
      (apply stop-app new-keys)
      (if-some [new-args (when (seq start-args) (pop start-args))]
        (apply start-app (concat new-args new-keys))
        (apply start-app *local-config* *resource-config-dirs* new-keys)))))

(defn suspend-app
  "Suspends the running application via `ferment.system/suspend!`.

  Optionally restricts suspension to selected `keys`.

  Updates lifecycle `phase` to `:suspended` on success and stores exceptions via
  `state-from-exception`.  Returns the current `phase` keyword."
  [& keys]
  (locking lock
    (if (running?)
      (try
        (var/reset phase :suspending)
        (if (seq keys) (system/suspend! state keys) (system/suspend! state))
        (var/reset phase :suspended)
        (var/reset exception nil)
        (catch Throwable e (state-from-exception e))))
    phase))

(defn resume-app
  "Resumes a suspended application via `ferment.system/resume`.

  Optionally restricts resume to selected `keys`.

  If the app is stopped, falls back to `ferment.app/restart-app`.
  Returns the current `phase` keyword."
  [& keys]
  (locking lock
    (if (suspended?)
      (try
        (var/reset phase :resuming)
        (if (seq keys) (system/resume post-config state keys) (system/resume post-config state))
        (var/reset phase :running)
        (catch Throwable e (state-from-exception e)))
      (when (stopped?)
        (apply restart-app keys)))
    phase))

(defn expand-app
  "Expands the current runtime state with `ferment.system/expand`.

  With no args, expands using the default expansion strategy.

  With args, supports either:
  - an explicit expansion function as the first arg (any `ifn?` that is not a keyword),
    followed by keys, or
  - a list of keys (expanded with `identity`).

  Updates lifecycle `phase` to `:expanded` on success and returns the current `phase`
  keyword."
  [& ks]
  (locking lock
    (var/reset phase :expanding)
    (if (seq ks)
      (let [fkey (some-> ks first)]
        (if (or (fn? fkey) (and (not (keyword? fkey)) (ifn? fkey)))
          (system/expand state fkey (next ks))
          (system/expand state identity ks)))
      (system/expand state))
    (var/reset phase :expanded)
    phase))

(defn prep-app
  "Alias for `ferment.app/expand-app` (historically used as “prep”)."
  [& keys]
  (apply expand-app keys))

;;
;; application control
;;

(defn configure!         [   ] (configure-app *local-config* *resource-config-dirs*))
(defn configure-dev!     [   ] (configure-app *local-config* *resource-dev-dirs*))
(defn configure-admin!   [   ] (configure-app *local-config* *resource-admin-dirs*))
(defn configure-test!    [   ] (configure-app *local-config* *resource-test-dirs*))
(defn configure-test-live! [ ] (configure-app *local-config* *resource-test-live-dirs*))

(defn start!             [& k] (apply start-app *local-config* *resource-config-dirs* k))
(defn start-dev!         [& k] (apply start-app *local-config* *resource-dev-dirs*    k))
(defn start-admin!       [& k] (apply start-app *local-config* *resource-admin-dirs*  k))
(defn start-test!        [& k] (apply start-app *local-config* *resource-test-dirs*   k))
(defn start-test-live!   [& k] (apply start-app *local-config* *resource-test-live-dirs* k))

(defn stop!              [& k] (apply stop-app    k))
(defn restart!           [& k] (apply restart-app k))
(defn suspend!           [& k] (apply suspend-app k))
(defn resume!            [& k] (apply resume-app  k))

(defdoc! configure!
  "Configures the application using the current defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-config-dirs*`.

Delegates to `ferment.app/configure-app`.")

(defdoc! configure-dev!
  "Configures the application using the current dev defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-dev-dirs*`.

Delegates to `ferment.app/configure-app`.")

(defdoc! configure-admin!
  "Configures the application using the current admin defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-admin-dirs*`.

Delegates to `ferment.app/configure-app`.")

(defdoc! configure-test!
  "Configures the application using the current test defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-test-dirs*`.

Delegates to `ferment.app/configure-app`.")

(defdoc! configure-test-live!
  "Configures the application using the current test-live defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-test-live-dirs*`.

Delegates to `ferment.app/configure-app`.")

(defdoc! start!
  "Starts the application using the current defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-config-dirs*`.

Delegates to `ferment.app/start-app`.")

(defdoc! stop!
  "Stops the application (optionally only selected keys).

Delegates to `ferment.app/stop-app`.")

(defdoc! restart!
  "Restarts the application (optionally only selected keys).

Delegates to `ferment.app/restart-app`.")

(defdoc! suspend!
  "Suspends the running application (optionally only selected keys).

Delegates to `ferment.app/suspend-app`.")

(defdoc! resume!
  "Resumes a suspended application (optionally only selected keys).

Delegates to `ferment.app/resume-app`.")

(defdoc! start-dev!
  "Starts the application using dev config defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-dev-dirs*`.

Delegates to `ferment.app/start-app`.")

(defdoc! start-admin!
  "Starts the application using admin config defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-admin-dirs*`.

Delegates to `ferment.app/start-app`.")

(defdoc! start-test!
  "Starts the application using test config defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-test-dirs*`.

Delegates to `ferment.app/start-app`.")

(defdoc! start-test-live!
  "Starts the application using test-live config defaults:
`ferment.app/*local-config*` and `ferment.app/*resource-test-live-dirs*`.

Delegates to `ferment.app/start-app`.")

(defn reload!
  "If the application is stopped, reloads code via `ferment.app/reload-namespaces`.
  Otherwise, stops the application, reloads namespaces, and starts it again."
  [& k]
  (if (stopped?)
    (reload-namespaces)
    (do (apply stop-app k)
        (reload-namespaces)
        (apply start-app *local-config* *resource-config-dirs* k))))

(defn print-state        [ ] (pprint state))
(defn print-config       [ ] (pprint config))
(defn print-post-config  [ ] (pprint post-config))
(defn print-start-args   [ ] (pprint start-args))

(defn cprint-state       [ ] (cprint state))
(defn cprint-config      [ ] (cprint config))
(defn cprint-post-config [ ] (cprint post-config))
(defn cprint-start-args  [ ] (cprint start-args))

(defdoc! print-state
  "Pretty-prints the current application runtime `ferment.app/state` using
`puget.printer/pprint`.")

(defdoc! print-config
  "Pretty-prints the last loaded raw configuration (`ferment.app/config`) using
`puget.printer/pprint`.")

(defdoc! print-post-config
  "Pretty-prints the expanded/prepared configuration (`ferment.app/post-config`)
using `puget.printer/pprint`.")

(defdoc! print-start-args
  "Pretty-prints the remembered startup arguments (`ferment.app/start-args`) using
`puget.printer/pprint`.")

(defdoc! cprint-state
  "Color-prints the current application runtime `ferment.app/state` using
`puget.printer/cprint`.")

(defdoc! cprint-config
  "Color-prints the last loaded raw configuration (`ferment.app/config`) using
`puget.printer/cprint`.")

(defdoc! cprint-post-config
  "Color-prints the expanded/prepared configuration (`ferment.app/post-config`)
using `puget.printer/cprint`.")

(defdoc! cprint-start-args
  "Color-prints the remembered startup arguments (`ferment.app/start-args`) using
`puget.printer/cprint`.")

;;
;; main function
;;

(defn -main
  "Program entry point. Starts the application using `ferment.app/start!`."
  []
  (start!))

;; documentation

(defdoc! config      "A nested map containing application configuration which was read from files.")
(defdoc! post-config "A nested map containing application configuration which was pre-parsed.")
(defdoc! state       "A nested map containing current state of application when it is running.")
(defdoc! exception   "Unhandled exception object thrown during starting, stopping or suspending.")

(defdoc! phase
  "A keyword describing current phase (`:stopping`, `:stoppped`, `:expanding`,
`:expanded`, `:starting`, `:running`, `:suspended`, `:suspending`, `:resuming`,
`:failed`).")

(defdoc! *ns-reload-watch-dirs*
  "A sequence of directories to be watched when reloading code. Used by
  `ferment.app/reload-namespaces` and (indirectly) by `ferment.app/reload!` and
  `ferment.app/make-ns-tracker`. Can also be set using
  `ferment.app/with-watch-dirs`.")

(defdoc! *local-config*
  "A local configuration file in EDN or ENV format which will be loaded after all other
  configuration files so its entries will replace any existing entries during
  merge. Be aware that `meta-merge` is used in the process so values of nested maps
  are replaced not the whole branches. Used when `ferment.app/configure!` is
  called. Please be aware that using this setting to override settings in certain
  environments may be considered less elastic than creating a separate, local folder
  and putting local configuration files there.")

(defdoc! *resource-config-dirs*
  "A sequence of paths (relative to the `resources` directory) to be scanned for EDN
  configuration files in regular mode of operation. Loaded in the same order as they
  appear and used by `ferment.app/configure!`.

  Please note that when building your own instance of application you still may refer
  to the original ferment configs since resource directories are shared across
  loaded libraries. Therefore, it is possible to load original files (for instance
  some basic translations) and override some of them in your i18n configuration. See
  also `ferment.app/start!` and `ferment.app/configure!`.")

(defdoc! *resource-admin-dirs*
  "The same as `ferment.app/*resource-config-dirs*` but loaded when application is
  run in administrative mode (and configured with `ferment.app/configure-admin!`).

   See also `ferment.app/start-admin!` and `ferment.app/configure-admin!`.")

(defdoc! *resource-dev-dirs*
  "The same as `ferment.app/*resource-config-dirs*` but loaded when application is
  run in development mode (and configured with `ferment.app/configure-dev!`).

  See also `ferment.app/start-dev!` and `ferment.app/configure-dev!`.")

(defdoc! *resource-test-dirs*
  "The same as `ferment.app/*resource-config-dirs*` but loaded when application is
  run in test mode (and configured with `ferment.app/configure-test!`).

  See also `ferment.app/start-test!` and `ferment.app/configure-test!`.")

(defdoc! *ns-tracker*
  "Instance of `ns-tracker` used to track directories for code changes. By default it
  is initialized by calling `ns-tracker.core/ns-tracker` (from
  `ferment.app/make-ns-tracker`) with a sequence of directories from
  `ferment.app/*ns-reload-watch-dirs*`.")
