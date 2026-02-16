(ns

    ^{:doc    "Model selection and model runtime configuration branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.model

  (:require [clojure.string :as str]
            [ferment.system :as system]
            [io.randomseed.utils.bot :as bot]
            [io.randomseed.utils.bus :as bus])

  (:import (java.io File)
           (java.lang ProcessBuilder ProcessBuilder$Redirect)
           (java.time Instant)
           (java.util.concurrent TimeUnit)))

(defn runtime-config
  "Extracts runtime config map from supported runtime shapes."
  [runtime]
  (cond
    (nil? runtime) nil
    (and (map? runtime) (map? (:runtime runtime))) (runtime-config (:runtime runtime))
    (and (map? runtime) (map? (:config runtime))) (:config runtime)
    (map? runtime) runtime
    :else nil))

(defn runtime-value
  "Reads raw value from normalized runtime config."
  [runtime k]
  (get (runtime-config runtime) k))

(defn runtimev
  "Reads scalar value from runtime config and normalizes it to non-blank string."
  [runtime k]
  (let [v (runtime-value runtime k)]
    (cond
      (nil? v) nil
      (keyword? v) (some-> v name str/trim not-empty)
      :else (some-> v str str/trim not-empty))))

(defn preconfigure-model
  "Pre-configuration hook for model selector keys."
  [_k config]
  config)

(defn- normalize-profile
  [v]
  (some-> v str str/trim not-empty str/lower-case))

(defn init-model-key
  "Initializes a model selector value.

  Accepts either a plain string (already selected model id) or a map with keys:
  - :profile (string/keyword),
  - :id/default (string),
  - :id/mini (string, optional),
  - :id/fallback (string, optional)."
  [_k config]
  (if-not (map? config)
    config
    (let [profile  (or (normalize-profile (:profile config)) "default")
          default' (some-> (or (:id/default config) (:default config)) str str/trim not-empty)
          mini'    (some-> (or (:id/mini config) (:mini config)) str str/trim not-empty)
          fallback (some-> (or (:id/fallback config) (:fallback config)) str str/trim not-empty)]
      (or (case profile
            "mini" mini'
            nil)
          default'
          fallback))))

(defn stop-model
  "Stop hook for model selector keys."
  [_k _state]
  nil)

(defn preconfigure-bot-session
  "Pre-configuration hook for bot session key."
  [_k config]
  (cond
    (map? config) config
    (some? config) {:sid (str config)}
    :else {:sid "ferment-model-runtime"}))

(defn init-bot-session
  "Initializes bot session map used by model runtime workers."
  [_k config]
  (let [cfg (preconfigure-bot-session _k config)
        sid (or (some-> (:sid cfg) str str/trim not-empty)
                "ferment-model-runtime")]
    (assoc cfg :sid sid)))

(defn stop-bot-session
  "Stop hook for bot session key."
  [_k _state]
  nil)

(defn start-bot-worker!
  "Starts a bot worker."
  [session cfg]
  (bot/run! session cfg))

(defn stop-bot-worker!
  "Stops a bot worker."
  [worker]
  (when worker
    (bot/stop! worker)))

(defn command-bot-worker!
  "Sends a command to a bot worker."
  [worker command & args]
  (when worker
    (apply bot/command worker command args)))

(defn- merge-runtime-defaults
  [cfg]
  (let [defaults      (if (map? (:defaults cfg)) (:defaults cfg) {})
        cfg'          (dissoc cfg :defaults)
        merged        (merge defaults cfg')
        merged-env    (when (or (map? (:env defaults))
                                (map? (:env cfg')))
                        (merge (or (:env defaults) {})
                               (or (:env cfg') {})))
        merged-session (if (and (map? (:session defaults))
                                (map? (:session cfg')))
                         (merge (:session defaults) (:session cfg'))
                         (:session merged))]
    (cond-> (assoc merged :session merged-session)
      (some? merged-env) (assoc :env merged-env))))

(defn- normalize-runtime-worker-config
  [k config]
  (let [cfg      (-> (if (map? config) config {})
                     merge-runtime-defaults)
        wid      (or (:id cfg) k)
        name'    (or (:name cfg) (str (name wid) " runtime"))
        session' (or (:session cfg) {:sid "ferment-model-runtime"})]
    (-> cfg
        (assoc :id wid
               :bot (or (:bot cfg) wid)
               :name (str name')
               :session session'
               :enabled? (not= false (:enabled? cfg)))
        (update :ns #(or % 'ferment.model)))))

(defn preconfigure-runtime-worker
  "Pre-configuration hook for model runtime worker keys."
  [k config]
  (let [cfg (if (map? config) config {})]
    (normalize-runtime-worker-config
     k
     (if (contains? cfg :defaults)
       cfg
       (assoc cfg :defaults (system/ref :ferment.model.defaults/runtime))))))

(defn start-command-process!
  "Starts a local process for worker runtime when `:command` is present in config.

  Expected config shape:
  - :command (vector of executable and args)
  - :workdir (optional path)
  - :env (optional map of env overrides)
  - :inherit-io? (optional, default false)"
  [worker-config _session]
  (when-some [cmd (seq (map str (:command worker-config)))]
    (let [^java.util.List command (mapv str cmd)
          pb (ProcessBuilder. ^java.util.List command)
          _  (if (:inherit-io? worker-config)
               (.inheritIO pb)
               (doto pb
                 (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                 (.redirectError  ProcessBuilder$Redirect/DISCARD)))
          _  (when-some [workdir (:workdir worker-config)]
               (.directory pb (File. (str workdir))))
          _  (when-some [env-map (:env worker-config)]
               (let [^java.util.Map env (.environment pb)]
                 (doseq [[ek ev] env-map]
                   (.put env (str ek) (str ev)))))
          process (.start pb)]
      {:type    :process
       :pid     (.pid process)
       :command (vec cmd)
       :process process})))

(defn stop-command-process!
  "Stops process runtime started by `start-command-process!`."
  [runtime-state _worker-config _session]
  (when-some [^Process process (:process runtime-state)]
    (.destroy process)
    (when-not (.waitFor process 2000 TimeUnit/MILLISECONDS)
      (.destroyForcibly process)))
  nil)

(defn runtime-request-handler
  "Default request handler for model runtime workers.

  Supports:
  - :status
  - :runtime
  - :invoke (if `:invoke-fn` is configured)"
  [session wrk req worker-config]
  (let [command   (:body req)
        req-args  (:args req)
        invoke-fn (:invoke-fn worker-config)]
    (case command
      :status {:worker/id      (or (:id worker-config) (bus/worker-id wrk))
               :worker/name    (:name worker-config)
               :worker/running (some? wrk)
               :stage          (:stage session)
               :started-at     (:started-at session)
               :runtime/error  (:runtime/error session)}
      :runtime (:runtime/state session)
      :invoke (if (fn? invoke-fn)
                (let [payload (first req-args)]
                  (try
                    {:ok? true
                     :result (invoke-fn payload session worker-config)}
                    (catch Throwable t
                      {:ok? false
                       :error :invoke-failed
                       :message (.getMessage t)})))
                {:ok? false
                 :error :invoke-not-configured})
      {:ok? false
       :error :unsupported-command
       :command command})))

(defn runtime-worker-run!
  "Default worker runner used by model runtime keys."
  [wrk bot-session worker-config]
  (let [start-fn   (or (:start-fn worker-config)
                       (when (seq (:command worker-config))
                         start-command-process!))
        stop-fn    (or (:stop-fn worker-config)
                       (when (seq (:command worker-config))
                         stop-command-process!))
        handler-fn (or (:on-request worker-config)
                       runtime-request-handler)
        base-state (volatile! nil)]
    (try
      (let [runtime-state (when (fn? start-fn)
                            (try
                              (start-fn worker-config bot-session)
                              (catch Throwable t
                                {:error :runtime-start-failed
                                 :class (str (class t))
                                 :message (.getMessage t)})))
            initial-state (cond-> (merge bot-session
                                         {:stage :RUNNING
                                          :started-at (str (Instant/now))})
                            (contains? runtime-state :error)
                            (assoc :runtime/error runtime-state)
                            (and (some? runtime-state)
                                 (not (contains? runtime-state :error)))
                            (assoc :runtime/state runtime-state))]
        (vreset! base-state initial-state)
        (loop [session initial-state]
          (vreset! base-state session)
          (let [request (bus/wait-for-request wrk)
                outcome (bot/handle-request
                         session
                         handler-fn
                         bot/generic-data-handler
                         wrk
                         request
                         worker-config)
                data (:data outcome)]
            (cond
              (= :QUIT data) nil
              (map? data)    (recur data)
              :else          (recur session)))))
      (finally
        (when (fn? stop-fn)
          (try
            (stop-fn (:runtime/state @base-state) worker-config @base-state)
            (catch Throwable _ nil)))))))

(defn init-runtime-worker
  "Initializes a model runtime worker.

  When `:enabled?` is false returns a disabled state without starting a thread."
  [k config]
  (let [cfg (normalize-runtime-worker-config k config)]
    (if-not (:enabled? cfg)
      {:id (:id cfg)
       :name (:name cfg)
       :enabled? false
       :worker nil
       :session (init-bot-session ::bot-session (:session cfg))
       :config cfg}
      (let [session (init-bot-session ::bot-session (:session cfg))
            runner  (or (:fn cfg)
                        (fn [wrk bot-session]
                          (runtime-worker-run! wrk bot-session cfg)))
            bot-cfg (assoc cfg :fn runner)
            worker  (start-bot-worker! session bot-cfg)]
        {:id (:id cfg)
         :name (:name cfg)
         :enabled? true
         :worker worker
         :session session
         :config cfg}))))

(defn stop-runtime-worker
  "Stops a model runtime worker."
  [_k state]
  (stop-bot-worker! (:worker state))
  nil)

(defn preconfigure-model-runtime
  "Pre-configuration hook for model runtime aggregate key."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (if (contains? cfg :models)
      cfg
      (assoc cfg :models (system/ref :ferment/models)))))

(defn init-model-runtime
  "Initializes model runtime aggregate state."
  [_k config]
  (let [models (or (:models config) {})
        workers-from-models
        (into {}
              (keep (fn [[model-k model-config]]
                      (let [runtime' (when (map? model-config) (:runtime model-config))]
                        (when runtime'
                          [(keyword (name model-k)) runtime']))))
              models)
        workers (or (:workers config) workers-from-models)]
    (assoc config :models models :workers workers)))

(defn stop-model-runtime
  "Stop hook for model runtime aggregate key."
  [_k _state]
  nil)

(defn runtime-worker-state
  "Returns runtime worker state by id from model runtime aggregate."
  [runtime worker-id]
  (let [workers (:workers runtime)
        wid    (if (keyword? worker-id) worker-id (keyword (str worker-id)))
        alias  (keyword (name wid))]
    (or (get workers worker-id)
        (get workers wid)
        (get workers alias))))

(defn runtime-worker
  "Returns worker handle by id from model runtime aggregate."
  [runtime worker-id]
  (some-> (runtime-worker-state runtime worker-id) :worker))

(defn runtime-command!
  "Sends command to selected runtime worker."
  [runtime worker-id command & args]
  (when-some [wrk (runtime-worker runtime worker-id)]
    (apply command-bot-worker! wrk command args)))

(defn runtime-ping!
  "Pings selected runtime worker."
  [runtime worker-id]
  (runtime-command! runtime worker-id :ping))

(defn runtime-pause!
  "Pauses selected runtime worker."
  [runtime worker-id]
  (runtime-command! runtime worker-id :pause))

(defn runtime-run!
  "Resumes selected runtime worker."
  [runtime worker-id]
  (runtime-command! runtime worker-id :run))

(defn preconfigure-model-entry
  "Pre-configuration hook for grouped model entries."
  [_k config]
  (if (map? config) config {:id config}))

(defn init-model-entry
  "Initializes grouped model entry (`:id` + optional `:runtime`)."
  [_k config]
  (let [cfg (if (map? config) config {:id config})
        id' (some-> (:id cfg) str str/trim not-empty)]
    (cond-> cfg
      id' (assoc :id id'))))

(defn stop-model-entry
  "Stop hook for grouped model entry."
  [_k _state]
  nil)

(defn preconfigure-models
  "Pre-configuration hook for aggregate `:ferment/models` map."
  [_k config]
  (or config {}))

(defn init-models
  "Initializes aggregate `:ferment/models` map."
  [_k config]
  config)

(defn stop-models
  "Stop hook for aggregate `:ferment/models` map."
  [_k _state]
  nil)

(defn model-entry
  "Returns grouped model entry from runtime `:models` map."
  [runtime model-k]
  (let [cfg    (runtime-config runtime)
        models (:models cfg)
        shortk (keyword (name model-k))]
    (or (when (map? models)
          (or (get models model-k)
              (get models shortk)))
        (let [legacy (get cfg model-k)]
          (when (map? legacy) legacy)))))

(defn model-id
  "Returns model identifier for grouped model key."
  [runtime model-k default-id]
  (or (some-> (model-entry runtime model-k) :id str str/trim not-empty)
      (runtimev runtime model-k)
      default-id))

(defn profile
  "Returns effective model profile (`default`, `mini`, ...)."
  [runtime]
  (or (normalize-profile (runtime-value runtime :ferment.model.defaults/profile))
      "default"))

(defn solver-id
  "Resolves solver model identifier from runtime config."
  [runtime]
  (or (model-id runtime :ferment.model/solver nil)
      (model-id runtime :ferment.model/coding nil)
      "qwen2.5-coder:7b"))

(defn voice-id
  "Resolves voice model identifier from runtime config."
  [runtime]
  (or (model-id runtime :ferment.model/voice nil)
      "SpeakLeash/bielik-1.5b-instruct"))

(defn meta-id
  "Resolves meta/router model identifier from runtime config."
  [runtime]
  (or (model-id runtime :ferment.model/meta nil)
      "mlx-community/SmolLM3-3B-8bit"))

(derive ::defaults-profile :ferment.system/value)
(derive ::runtime-defaults :ferment.system/value)
(derive ::bot-session :ferment.system/value)
(derive :ferment.model.defaults/profile ::defaults-profile)
(derive :ferment.model.defaults/runtime ::runtime-defaults)
(derive :ferment.model.defaults/bot-session ::bot-session)

(derive :ferment.model.id/solver ::selector)
(derive :ferment.model.id/voice  ::selector)
(derive :ferment.model.id/coding ::selector)
(derive :ferment.model.id/meta   ::selector)

(derive ::model-entry :ferment.system/value)
(derive :ferment.model/solver ::model-entry)
(derive :ferment.model/voice  ::model-entry)
(derive :ferment.model/coding ::model-entry)
(derive :ferment.model/meta   ::model-entry)

(derive :ferment.model.runtime/solver ::runtime-worker)
(derive :ferment.model.runtime/voice  ::runtime-worker)
(derive :ferment.model.runtime/coding ::runtime-worker)
(derive :ferment.model.runtime/meta   ::runtime-worker)

(derive ::models-aggregate :ferment.system/value)
(derive :ferment/models ::models-aggregate)

(derive ::runtime ::runtime-service)

(system/add-expand ::selector [k config] {k (preconfigure-model k config)})
(system/add-init   ::selector [k config]    (init-model-key k config))
(system/add-halt!  ::selector [k state]     (stop-model k state))

(system/add-expand ::model-entry [k config] {k (preconfigure-model-entry k config)})
(system/add-init   ::model-entry [k config]    (init-model-entry k config))
(system/add-halt!  ::model-entry [k state]     (stop-model-entry k state))

(system/add-expand ::bot-session [k config] {k (preconfigure-bot-session k config)})
(system/add-init   ::bot-session [k config]    (init-bot-session k config))
(system/add-halt!  ::bot-session [k state]     (stop-bot-session k state))

(system/add-expand ::runtime-worker [k config] {k (preconfigure-runtime-worker k config)})
(system/add-init   ::runtime-worker [k config]    (init-runtime-worker k config))
(system/add-halt!  ::runtime-worker [k state]     (stop-runtime-worker k state))

(system/add-expand ::models-aggregate [k config] {k (preconfigure-models k config)})
(system/add-init   ::models-aggregate [k config]    (init-models k config))
(system/add-halt!  ::models-aggregate [k state]     (stop-models k state))

(system/add-expand ::runtime-service [k config] {k (preconfigure-model-runtime k config)})
(system/add-init   ::runtime-service [k config]    (init-model-runtime k config))
(system/add-halt!  ::runtime-service [k state]     (stop-model-runtime k state))
