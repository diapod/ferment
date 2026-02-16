(ns

    ^{:doc    "Model selection and model runtime configuration branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.model

  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [ferment.system :as system]
            [io.randomseed.utils.bot :as bot]
            [io.randomseed.utils.bus :as bus])

  (:import (java.io File)
           (java.lang ProcessBuilder ProcessBuilder$Redirect)
           (java.nio.charset StandardCharsets)
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
  (let [defaults-v     (:defaults cfg)
        unresolved-ref? (system/ref? defaults-v)
        defaults       (if (and (map? defaults-v)
                                (not unresolved-ref?))
                         defaults-v
                         {})
        cfg'           (if unresolved-ref?
                         cfg
                         (dissoc cfg :defaults))
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

(def ^:private env-var-pattern
  #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}|\$([A-Za-z_][A-Za-z0-9_]*)")

(defn- expand-env-template
  [^String s ^java.util.Map base-env]
  (str/replace
   (or s "")
   env-var-pattern
   (fn [[_ braced plain]]
     (let [k (or braced plain)]
       (or (some-> (.get base-env k) str)
           (some-> (System/getenv k) str)
           (when (= "HOME" k) (System/getProperty "user.home"))
           "")))))

(defn- apply-process-env!
  [^ProcessBuilder pb env-map]
  (when (map? env-map)
    (let [^java.util.Map base-env (.environment pb)]
      (doseq [[ek ev] env-map]
        (let [k (str ek)
              v (expand-env-template (str (or ev "")) base-env)]
          (.put base-env k v))))))

(defn- effective-env-map
  [env-map]
  (let [^java.util.Map effective (java.util.HashMap. ^java.util.Map (System/getenv))]
    (when (map? env-map)
      (doseq [[ek ev] env-map]
        (let [k (str ek)
              v (expand-env-template (str (or ev "")) effective)]
          (.put effective k v))))
    effective))

(defn- resolve-executable-on-path
  [exe path-str]
  (let [sep-rx (re-pattern
                (java.util.regex.Pattern/quote
                 (or (System/getProperty "path.separator") ":")))]
    (some (fn [dir]
            (let [d (some-> dir str str/trim not-empty)
                  f (when d (io/file d exe))]
              (when (and f (.isFile f) (.canExecute f))
                (.getPath f))))
          (str/split (or path-str "") sep-rx))))

(defn- resolve-command-executable
  [command env-map]
  (let [cmd (vec (map str command))
        exe (first cmd)]
    (if (or (str/blank? exe)
            (str/includes? exe "/"))
      cmd
      (let [^java.util.Map env* (effective-env-map env-map)
            path-str (some-> (.get env* "PATH") str)
            resolved (resolve-executable-on-path exe path-str)]
        (if resolved
          (assoc cmd 0 resolved)
          cmd)))))

(defn- invoke-prompt
  [payload]
  (let [prompt (cond
                 (string? payload) payload
                 (string? (:prompt payload)) (:prompt payload)
                 (string? (get-in payload [:input :prompt])) (get-in payload [:input :prompt])
                 (map? payload) (pr-str payload)
                 (nil? payload) ""
                 :else (str payload))
        system (some-> (cond
                         (string? (:system payload)) (:system payload)
                         (string? (get-in payload [:input :system])) (get-in payload [:input :system])
                         :else nil)
                       str/trim
                       not-empty)]
    (if system
      (str "SYSTEM:\n" system "\n\nUSER:\n" prompt "\n\nASSISTANT:\n")
      prompt)))

(defn invoke-command!
  "Invokes model command in one-shot mode for bot `:invoke` request."
  [worker-config payload]
  (let [prompt      (invoke-prompt payload)
        command0    (vec (map str (:command worker-config)))
        prompt-via  (or (:prompt-via worker-config) :arg)
        prompt-arg  (or (:prompt-arg worker-config) "--prompt")
        command     (if (= :arg prompt-via)
                      (into command0 [prompt-arg prompt])
                      command0)
        command'    (resolve-command-executable command (:env worker-config))
        ^java.util.List cmd-list (mapv str command')
        pb          (ProcessBuilder. ^java.util.List cmd-list)
        _           (when-some [workdir (:workdir worker-config)]
                      (.directory pb (File. (str workdir))))
        _           (apply-process-env! pb (:env worker-config))
        process     (.start pb)
        _           (when (= :stdin prompt-via)
                      (let [^String in-str (str prompt)]
                        (with-open [^java.io.Writer w (io/writer (.getOutputStream process))]
                          (.write w in-str))))
        out         (slurp (.getInputStream process))
        err         (slurp (.getErrorStream process))
        exit        (.waitFor process)
        out'        (some-> out str str/trim not-empty)
        err'        (some-> err str str/trim not-empty)
        text        (or out' err' "")]
    (when-not (zero? exit)
      (throw (ex-info "Model invoke command failed."
                      {:exit exit
                       :command command'
                       :stdout out
                       :stderr err})))
    {:text text
     :command command'
     :exit exit}))

(defn- normalize-runtime-worker-config
  [k config]
  (let [cfg      (-> (if (map? config) config {})
                     merge-runtime-defaults)
        wid      (or (:id cfg) k)
        name'    (or (:name cfg) (str (name wid) " runtime"))
        session' (or (:session cfg) {:sid "ferment-model-runtime"})
        invoke-fn' (or (:invoke-fn cfg)
                       (when (seq (:command cfg))
                         (fn [payload _session worker-config]
                           (invoke-command! worker-config payload))))]
    (-> cfg
        (assoc :id wid
               :bot (or (:bot cfg) wid)
               :name (str name')
               :session session'
               :enabled? (not= false (:enabled? cfg))
               :invoke-fn invoke-fn')
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

(def ^:private io-streams
  #{:in :out :err})

(defn- normalize-io-selection
  [inherit-io]
  (cond
    (true? inherit-io) io-streams
    (false? inherit-io) #{}
    (keyword? inherit-io) (if (contains? io-streams inherit-io) #{inherit-io} #{})
    (set? inherit-io) (into #{} (filter io-streams) inherit-io)
    (sequential? inherit-io) (into #{} (filter io-streams) inherit-io)
    :else #{}))

(defn- configure-process-io!
  [^ProcessBuilder pb inherit-io]
  (let [selection (normalize-io-selection inherit-io)]
    (if (= selection io-streams)
      (.inheritIO pb)
      (doto pb
        (.redirectInput  (if (contains? selection :in)
                           ProcessBuilder$Redirect/INHERIT
                           ProcessBuilder$Redirect/PIPE))
        (.redirectOutput (if (contains? selection :out)
                           ProcessBuilder$Redirect/INHERIT
                           ProcessBuilder$Redirect/DISCARD))
        (.redirectError  (if (contains? selection :err)
                           ProcessBuilder$Redirect/INHERIT
                           ProcessBuilder$Redirect/DISCARD))))))

(defn start-command-process!
  "Starts a local process for worker runtime when `:command` is present in config.

  Expected config shape:
  - :command (vector of executable and args)
  - :workdir (optional path)
  - :env (optional map of env overrides)
  - :inherit-io? (optional; `true` or one of `:in`, `:out`, `:err`,
    or collection/set of these keywords)"
  [worker-config _session]
  (when-some [cmd0 (seq (map str (:command worker-config)))]
    (let [cmd     (resolve-command-executable (vec cmd0) (:env worker-config))
          ^java.util.List command (mapv str cmd)
          pb (ProcessBuilder. ^java.util.List command)
          _  (configure-process-io! pb (:inherit-io? worker-config))
          _  (when-some [workdir (:workdir worker-config)]
               (.directory pb (File. (str workdir))))
          _  (apply-process-env! pb (:env worker-config))
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

(defn- quit-command
  [worker-config]
  (when-some [cmd (some-> (:cmd/quit worker-config) str str/trim not-empty)]
    (if (str/ends-with? cmd "\n")
      cmd
      (str cmd "\n"))))

(defn- send-quit-command!
  [session worker-config]
  (if-some [cmd (quit-command worker-config)]
    (if-some [^Process process (get-in session [:runtime/state :process])]
      (try
        (let [^java.io.OutputStream out (.getOutputStream process)
              bytes (.getBytes cmd StandardCharsets/UTF_8)]
          (.write out bytes)
          (.flush out)
          {:ok? true
           :sent cmd})
        (catch Throwable t
          {:ok? false
           :error :quit-write-failed
           :message (.getMessage t)}))
      {:ok? false
       :error :runtime-process-missing})
    {:ok? false
     :error :quit-not-configured}))

(defn- warn-quit-stop!
  [message details]
  (binding [*out* *err*]
    (println (str "WARN ferment.model - "
                  message
                  " "
                  (pr-str details)))))

(def ^:private runtime-state-public-keys
  #{:type
    :kind
    :pid
    :command
    :host
    :port
    :url
    :transport
    :model
    :profile
    :state
    :ready?
    :started-at
    :stopped-at
    :uptime-ms
    :health})

(defn- public-runtime-error
  [runtime-error]
  (cond
    (map? runtime-error)
    (let [err (select-keys runtime-error [:error :type :class :message :details])]
      (when (seq err) err))

    (some? runtime-error) {:message (str runtime-error)}
    :else nil))

(defn- process-snapshot
  [^Process process]
  (let [alive? (.isAlive process)
        exit-code (when-not alive?
                    (try
                      (.exitValue process)
                      (catch IllegalThreadStateException _ nil)))]
    (cond-> {:process/alive? alive?}
      (some? exit-code) (assoc :process/exit exit-code))))

(defn- runtime-state-snapshot
  [runtime-state]
  (let [state        (if (map? runtime-state) runtime-state {})
        process      (:process state)
        public-state (select-keys state runtime-state-public-keys)]
    (if (instance? Process process)
      (merge public-state (process-snapshot ^Process process))
      public-state)))

(defn- runtime-snapshot
  [session wrk worker-config]
  {:worker/id      (or (:id worker-config) (bus/worker-id wrk))
   :worker/name    (:name worker-config)
   :worker/running (some? wrk)
   :stage          (:stage session)
   :started-at     (:started-at session)
   :runtime/error  (public-runtime-error (:runtime/error session))
   :runtime/state  (runtime-state-snapshot (:runtime/state session))})

(defn runtime-request-handler
  "Default request handler for model runtime workers.

  Supports:
  - :status
  - :runtime (safe runtime snapshot for operators)
  - :quit (writes configured `:cmd/quit` + newline to runtime process stdin)
  - :invoke (if `:invoke-fn` is configured)"
  [session wrk req worker-config]
  (let [worker-config (if (and (sequential? worker-config)
                               (not (map? worker-config)))
                        (first worker-config)
                        worker-config)
        command   (:body req)
        req-args  (:args req)
        invoke-fn (:invoke-fn worker-config)]
    (case command
      :status {:worker/id      (or (:id worker-config) (bus/worker-id wrk))
               :worker/name    (:name worker-config)
               :worker/running (some? wrk)
               :stage          (:stage session)
               :started-at     (:started-at session)
               :runtime/error  (public-runtime-error (:runtime/error session))}
      :runtime (runtime-snapshot session wrk worker-config)
      :quit (send-quit-command! session worker-config)
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
                _ (when-some [response (:response outcome)]
                    (bus/send-response wrk response))
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
  (let [worker (:worker state)
        cfg    (:config state)]
    (when (and worker (some? (:cmd/quit cfg)))
      (try
        (let [quit-response (command-bot-worker! worker :quit)]
          (when (and (map? quit-response)
                     (= false (:ok? quit-response)))
            (warn-quit-stop! "Failed to send model quit command before stop."
                             {:worker (:id state)
                              :cmd/quit (:cmd/quit cfg)
                              :quit-response quit-response})))
        (catch Throwable t
          (warn-quit-stop! "Exception while sending model quit command before stop."
                           {:worker (:id state)
                            :cmd/quit (:cmd/quit cfg)
                            :error (.getMessage t)}))))
    (stop-bot-worker! worker))
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
      "mlx-community/Qwen2.5-7B-Instruct-4bit"))

(defn coding-id
  "Resolves coding model identifier from runtime config."
  [runtime]
  (or (model-id runtime :ferment.model/coding nil)
      "mlx-community/Qwen2.5-Coder-14B-Instruct-4bit"))

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
