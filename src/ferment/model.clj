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

(defn runtimev
  "Reads keyword value from runtime config and normalizes blank strings to nil."
  [runtime k]
  (some-> (get (runtime-config runtime) k)
          str
          str/trim
          not-empty))

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
  - :default (string),
  - :mini (string, optional),
  - :fallback (string, optional)."
  [_k config]
  (if-not (map? config)
    config
    (let [profile  (or (normalize-profile (:profile config)) "default")
          default' (some-> (:default config) str str/trim not-empty)
          mini'    (some-> (:mini config) str str/trim not-empty)
          fallback (some-> (:fallback config) str str/trim not-empty)]
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

(defn- normalize-runtime-worker-config
  [k config]
  (let [cfg      (if (map? config) config {})
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
  (normalize-runtime-worker-config k config))

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
          pb (ProcessBuilder. command)
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
  config)

(defn init-model-runtime
  "Initializes model runtime aggregate state."
  [_k config]
  (let [workers (or (:workers config)
                    (select-keys config [:solver :voice :meta]))]
    (assoc config :workers workers)))

(defn stop-model-runtime
  "Stop hook for model runtime aggregate key."
  [_k _state]
  nil)

(defn runtime-worker-state
  "Returns runtime worker state by id from model runtime aggregate."
  [runtime worker-id]
  (some-> runtime :workers (get worker-id)))

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

(defn profile
  "Returns effective model profile (`default`, `mini`, ...)."
  [runtime]
  (-> (or (runtimev runtime :ferment.model/profile)
          "default")
      str/lower-case))

(defn solver-id
  "Resolves solver model identifier from runtime config."
  [runtime]
  (or (runtimev runtime :ferment.model/solver)
      (runtimev runtime :ferment.model/coding)
      "qwen2.5-coder:7b"))

(defn voice-id
  "Resolves voice model identifier from runtime config."
  [runtime]
  (or (runtimev runtime :ferment.model/voice)
      "SpeakLeash/bielik-1.5b-instruct"))

(derive ::profile :ferment.system/value)
(derive ::solver  ::selector)
(derive ::voice   ::selector)
(derive :ferment.model/coding ::selector)
(derive :ferment.model/meta ::selector)
(derive ::runtime-env :ferment.system/value)
(derive ::bot-session :ferment.system/value)
(derive :ferment.model.solver/runtime ::runtime-worker)
(derive :ferment.model.voice/runtime  ::runtime-worker)
(derive :ferment.model.solver/env ::runtime-env)
(derive :ferment.model.voice/env  ::runtime-env)
(derive :ferment.model.meta/env   ::runtime-env)
(derive ::runtime        ::runtime-service)

(system/add-expand ::selector [k config] {k (preconfigure-model k config)})
(system/add-init   ::selector [k config]    (init-model-key k config))
(system/add-halt!  ::selector [k state]     (stop-model k state))

(system/add-expand ::bot-session [k config] {k (preconfigure-bot-session k config)})
(system/add-init   ::bot-session [k config]    (init-bot-session k config))
(system/add-halt!  ::bot-session [k state]     (stop-bot-session k state))

(system/add-expand ::runtime-worker [k config] {k (preconfigure-runtime-worker k config)})
(system/add-init   ::runtime-worker [k config]    (init-runtime-worker k config))
(system/add-halt!  ::runtime-worker [k state]     (stop-runtime-worker k state))

(system/add-expand ::runtime-service [k config] {k (preconfigure-model-runtime k config)})
(system/add-init   ::runtime-service [k config]    (init-model-runtime k config))
(system/add-halt!  ::runtime-service [k state]     (stop-model-runtime k state))
