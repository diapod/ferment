(ns

    ^{:doc    "Model selection and model runtime configuration branch for Ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.model

  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [ferment.session :as fsession]
            [ferment.system :as system]
            [io.randomseed.utils.bot :as bot]
            [io.randomseed.utils.bus :as bus])

  (:import (java.io File)
           (java.lang ProcessBuilder ProcessBuilder$Redirect)
           (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util.concurrent TimeUnit)))

(def ^:private default-session-idle-ttl-ms 900000)
(def ^:private default-session-max-per-model 4)

(defn- now-ms
  []
  (System/currentTimeMillis))

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
        prompt-via  (or (:invoke/prompt-via worker-config)
                        (:prompt-via worker-config)
                        :stdin)
        prompt-arg  (or (:invoke/prompt-arg worker-config)
                        (:prompt-arg worker-config)
                        "--prompt")
        command     (if (and (= :arg prompt-via)
                             (string? prompt-arg)
                             (not (str/blank? prompt-arg)))
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
                      (let [^String in-str (str prompt "\n")]
                        (with-open [^java.io.Writer w (io/writer (.getOutputStream process))]
                          (.write w in-str))))
        out         (slurp (.getInputStream process))
        err         (slurp (.getErrorStream process))
        exit        (.waitFor process)
        out'        (some-> out str str/trim not-empty)
        err'        (some-> err str str/trim not-empty)
        eof-exit?   (and (not (zero? exit))
                         (= :stdin prompt-via)
                         (or (some-> err' (str/includes? "EOFError: EOF when reading a line"))
                             (some-> err' (str/includes? "EOF when reading a line")))
                         (some? out'))
        text        (or out' err' "")]
    (when (and (not (zero? exit))
               (not eof-exit?))
      (throw (ex-info "Model invoke command failed."
                      {:exit exit
                       :command command'
                       :stdout out
                       :stderr err})))
    {:text text
     :command command'
     :exit exit
     :ok-nonzero? eof-exit?}))

(declare invoke-runtime-process!)

(defn- normalize-runtime-worker-config
  [k config]
  (let [cfg      (-> (if (map? config) config {})
                     merge-runtime-defaults)
        wid      (or (:id cfg) k)
        name'    (or (:name cfg) (str (name wid) " runtime"))
        session' (or (:session cfg) {:sid "ferment-model-runtime"})
        invoke-mode (or (:invoke/mode cfg) :runtime-process)
        invoke-fn' (or (:invoke-fn cfg)
                       (when (seq (:command cfg))
                         (if (= :oneshot invoke-mode)
                           (fn [payload _session worker-config]
                             (invoke-command! worker-config payload))
                           (fn [payload session worker-config]
                             (invoke-runtime-process! payload session worker-config)))))]
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
    ;; Keep stdout/stderr as pipes so :invoke can reuse an already-running process.
    ;; Optional forwarding to parent stdio is handled by stream pump threads.
    (doto pb
      (.redirectInput  (if (contains? selection :in)
                         ProcessBuilder$Redirect/INHERIT
                         ProcessBuilder$Redirect/PIPE))
      (.redirectOutput ProcessBuilder$Redirect/PIPE)
      (.redirectError  ProcessBuilder$Redirect/PIPE))))

(def ^:private default-io-buffer-max-chars 300000)
(def ^:private default-invoke-timeout-ms 120000)
(def ^:private default-invoke-poll-ms 40)
(def ^:private default-invoke-settle-ms 350)
(def ^:private default-invoke-ready-pattern #">>\s*$")

(defn- io-buffer-state
  []
  {:total 0
   :text  ""})

(defn- append-buffer-text
  [state chunk max-chars]
  (let [chunk' (str (or chunk ""))
        total' (+ (long (or (:total state) 0))
                  (count chunk'))
        text0  (str (or (:text state) "") chunk')
        max'   (long (max 1024 (or max-chars default-io-buffer-max-chars)))
        text'  (if (> (count text0) max')
                 (subs text0 (- (count text0) (int max')))
                 text0)]
    {:total total'
     :text text'}))

(defn- buffer-view-since
  [buffer-atom since-total]
  (let [{:keys [total text]} (or @buffer-atom (io-buffer-state))
        t      (or text "")
        total' (long (or total 0))
        since' (long (or since-total 0))
        tail-start (- total' (count t))
        truncated? (< since' tail-start)
        local-start (max 0 (- since' tail-start))]
    {:total total'
     :text (subs t (int local-start))
     :truncated? truncated?}))

(defn- pump-stream!
  [^java.io.InputStream in stream-k buffer-atom selection max-chars]
  (let [forward? (contains? selection stream-k)
        ^java.io.PrintStream target (if (= :err stream-k) System/err System/out)
        max' (long (max 1024 (or max-chars default-io-buffer-max-chars)))]
    (future
      (try
        (with-open [^java.io.Reader rdr (io/reader in :encoding "UTF-8")]
          (let [cbuf (char-array 2048)]
            (loop []
              (let [n (.read rdr cbuf 0 (alength cbuf))]
                (when (pos? n)
                  (let [chunk (String. cbuf 0 n)]
                    (swap! buffer-atom append-buffer-text chunk max')
                    (when forward?
                      (.print target chunk)
                      (.flush target)))
                  (recur))))))
        (catch Throwable _ nil)))))

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
          selection (normalize-io-selection (:inherit-io? worker-config))
          max-chars (or (:io/max-buffer-chars worker-config)
                        default-io-buffer-max-chars)
          ^java.util.List command (mapv str cmd)
          pb (ProcessBuilder. ^java.util.List command)
          _  (configure-process-io! pb (:inherit-io? worker-config))
          _  (when-some [workdir (:workdir worker-config)]
               (.directory pb (File. (str workdir))))
          _  (apply-process-env! pb (:env worker-config))
          process (.start pb)
          out-buf (atom (io-buffer-state))
          err-buf (atom (io-buffer-state))
          out-pump (pump-stream! (.getInputStream process) :out out-buf selection max-chars)
          err-pump (pump-stream! (.getErrorStream process) :err err-buf selection max-chars)]
      {:type    :process
       :pid     (.pid process)
       :command (vec cmd)
       :process process
       :io     {:selection selection
                :lock (Object.)
                :max-buffer-chars max-chars
                :stdout out-buf
                :stderr err-buf
                :pumps [out-pump err-pump]}})))

(defn stop-command-process!
  "Stops process runtime started by `start-command-process!`."
  [runtime-state _worker-config _session]
  (when-some [^Process process (:process runtime-state)]
    (.destroy process)
    (when-not (.waitFor process 2000 TimeUnit/MILLISECONDS)
      (.destroyForcibly process)))
  (doseq [p (get-in runtime-state [:io :pumps])]
    (when (future? p)
      (future-cancel p)))
  nil)

(defn- quit-command
  [worker-config]
  (when-some [^String cmd (some-> (:cmd/quit worker-config) str str/trim not-empty)]
    (if (str/ends-with? cmd "\n")
      cmd
      (str cmd "\n"))))

(defn- send-quit-command!
  [session worker-config]
  (if-some [cmd (quit-command worker-config)]
    (if-some [^Process process (get-in session [:runtime/state :process])]
      (try
        (let [lock (or (get-in session [:runtime/state :io :lock]) (Object.))]
          (locking lock
            (let [^java.io.OutputStream out (.getOutputStream process)
                  ^bytes bytes (.getBytes ^String cmd StandardCharsets/UTF_8)]
              (.write out ^bytes bytes)
              (.flush out)
              {:ok? true
               :sent cmd})))
        (catch Throwable t
          {:ok? false
           :error :quit-write-failed
           :message (.getMessage t)}))
      {:ok? false
       :error :runtime-process-missing})
    {:ok? false
     :error :quit-not-configured}))

(defn- tail-text
  [s n]
  (let [s' (str (or s ""))
        n' (max 1 (int (or n 1)))
        len (count s')]
    (if (<= len n')
      s'
      (subs s' (- len n')))))

(defn- ready-pattern
  [worker-config]
  (let [rp-key? (contains? worker-config :invoke/ready-pattern)
        rp (:invoke/ready-pattern worker-config)]
    (cond
      (and rp-key? (or (nil? rp) (false? rp))) nil
      (instance? java.util.regex.Pattern rp) rp
      (string? rp) (try
                     (re-pattern rp)
                     (catch Throwable _ default-invoke-ready-pattern))
      :else default-invoke-ready-pattern)))

(defn- wait-for-runtime-response!
  [stdout-buf stderr-buf out-total0 err-total0 worker-config]
  (let [timeout-ms (long (max 100 (or (:invoke/timeout-ms worker-config)
                                      default-invoke-timeout-ms)))
        poll-ms    (long (max 10 (or (:invoke/poll-ms worker-config)
                                     default-invoke-poll-ms)))
        settle-ms  (long (max 20 (or (:invoke/settle-ms worker-config)
                                     default-invoke-settle-ms)))
        ready-re   (ready-pattern worker-config)
        started-at (System/currentTimeMillis)
        initial-total (+ out-total0 err-total0)]
    (loop [last-total initial-total
           last-change started-at]
      (Thread/sleep poll-ms)
      (let [out-view (buffer-view-since stdout-buf out-total0)
            err-view (buffer-view-since stderr-buf err-total0)
            stdout' (:text out-view)
            stderr' (:text err-view)
            total-now (+ (:total out-view) (:total err-view))
            changed? (not= total-now last-total)
            now-ms (System/currentTimeMillis)
            last-change' (if changed? now-ms last-change)
            produced? (> total-now initial-total)
            ready? (when ready-re
                     (boolean (re-find ready-re (tail-text stdout' 512))))
            settled? (and (nil? ready-re)
                          produced?
                          (>= (- now-ms last-change') settle-ms))
            timeout? (>= (- now-ms started-at) timeout-ms)]
        (cond
          ready? {:stdout stdout'
                  :stderr stderr'
                  :stdout/truncated? (:truncated? out-view)
                  :stderr/truncated? (:truncated? err-view)}
          settled? {:stdout stdout'
                    :stderr stderr'
                    :stdout/truncated? (:truncated? out-view)
                    :stderr/truncated? (:truncated? err-view)}
          timeout? (throw (ex-info "Timed out waiting for runtime model response."
                                   {:error :invoke-timeout
                                    :timeout-ms timeout-ms
                                    :stdout stdout'
                                    :stderr stderr'
                                    :stdout/truncated? (:truncated? out-view)
                                    :stderr/truncated? (:truncated? err-view)}))
          :else (recur total-now last-change'))))))

(defn invoke-runtime-process!
  "Invokes a running runtime process by writing prompt to stdin and reading
  response from runtime stdout/stderr buffers."
  [payload session worker-config]
  (let [runtime-state (:runtime/state session)
        ^Process process (:process runtime-state)
        stdout-buf (get-in runtime-state [:io :stdout])
        stderr-buf (get-in runtime-state [:io :stderr])
        lock (or (get-in runtime-state [:io :lock]) (Object.))
        prompt (str (invoke-prompt payload) "\n")]
    (when-not (instance? Process process)
      (throw (ex-info "Runtime process is not available for invoke."
                      {:error :runtime-process-missing})))
    (when-not (.isAlive process)
      (throw (ex-info "Runtime process is not alive."
                      {:error :runtime-process-dead
                       :exit (try
                               (.exitValue process)
                               (catch Throwable _ nil))})))
    (when-not (and (instance? clojure.lang.IDeref stdout-buf)
                   (instance? clojure.lang.IDeref stderr-buf))
      (throw (ex-info "Runtime process buffers are not available for invoke."
                      {:error :runtime-process-io-unavailable})))
    (locking lock
      (let [out-total0 (:total @stdout-buf)
            err-total0 (:total @stderr-buf)]
        (try
          (let [^java.io.OutputStream out (.getOutputStream process)
                ^bytes bytes (.getBytes ^String prompt StandardCharsets/UTF_8)]
            (.write out ^bytes bytes)
            (.flush out))
          (catch Throwable t
            (throw (ex-info "Failed to write invoke payload to runtime process."
                            {:error :runtime-stdin-write-failed
                             :message (.getMessage t)}
                            t))))
        (let [{:keys [stdout stderr]
               :as capture} (wait-for-runtime-response!
                             stdout-buf stderr-buf out-total0 err-total0 worker-config)
              stdout' (some-> stdout str str/trim not-empty)
              stderr' (some-> stderr str str/trim not-empty)
              text (or stdout' stderr' "")]
          (assoc capture :text text))))))

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
                       :message (.getMessage t)
                       :details (when (instance? clojure.lang.ExceptionInfo t)
                                  (select-keys (ex-data t)
                                               [:error
                                                :exit
                                                :command
                                                :stderr
                                                :stdout
                                                :timeout-ms
                                                :stdout/truncated?
                                                :stderr/truncated?]))})))
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
  [_k state]
  (when-some [registry (and (map? state)
                            (instance? clojure.lang.IDeref
                                       (:ferment.model.session/workers state))
                            (:ferment.model.session/workers state))]
    (doseq [[_model sid->entry] @registry
            [_sid {:keys [worker-state]}] sid->entry]
      (when (map? worker-state)
        (try
          (stop-runtime-worker (:id worker-state) worker-state)
          (catch Throwable _ nil)))))
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

(defn- unwrap-system
  [system]
  (cond
    (map? system) system
    (instance? clojure.lang.Var system) (var-get ^clojure.lang.Var system)
    (instance? clojure.lang.IDeref system) @system
    :else nil))

(defn- normalize-model-key
  [model-id]
  (let [k (cond
            (keyword? model-id) model-id
            (string? model-id) (some-> model-id str/trim not-empty keyword)
            :else nil)]
    (when k
      (case (namespace k)
        "ferment.model" k
        "ferment.model.runtime" (keyword "ferment.model" (name k))
        (keyword "ferment.model" (name k))))))

(defn- runtime-state-from-system
  [system model-k]
  (let [runtime-k (keyword "ferment.model.runtime" (name model-k))
        short-k   (keyword (name model-k))
        candidates [(get system runtime-k)
                    (get-in system [model-k :runtime])
                    (get-in system [:ferment/models model-k :runtime])
                    (get-in system [:ferment/models short-k :runtime])
                    (get-in system [:ferment.runtime/default :models model-k :runtime])
                    (get-in system [:ferment.runtime/default :models short-k :runtime])
                    (get-in system [:ferment.model/runtime :workers short-k])
                    (get-in system [:ferment.runtime/default :workers short-k])]]
    (some #(when (and (map? %) (contains? % :worker)) %) candidates)))

(defn diagnostic-invoke!
  "Sends diagnostic payload to a running model runtime worker using `:invoke`.

  Arguments:
  - `system`   running system map (e.g. `ferment.app/state`) or derefable/Var with it
  - `model-id` model identifier (`:meta`, `:solver`, `:voice`, `:coding` or qualified key)
  - `payload`  data sent to worker as `:invoke` argument"
  [system model-id payload]
  (let [system' (unwrap-system system)
        model-k (normalize-model-key model-id)]
    (when-not (map? system')
      (throw (ex-info "Diagnostic invoke requires running system map."
                      {:error :system/not-available
                       :model-id model-id
                       :system-type (some-> system class str)})))
    (when-not (keyword? model-k)
      (throw (ex-info "Diagnostic invoke requires model identifier."
                      {:error :model/id-invalid
                       :model-id model-id})))
    (let [runtime-state (runtime-state-from-system system' model-k)
          worker (:worker runtime-state)]
      (when-not worker
        (throw (ex-info "Model runtime worker is not running or unavailable in system."
                        {:error :model/worker-not-found
                         :model model-k
                         :runtime-key (keyword "ferment.model.runtime" (name model-k))})))
      (or (command-bot-worker! worker :invoke payload)
          {:ok? false
           :error :invoke/empty-response
           :model model-k}))))

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

(defn- runtime-session-registry
  [runtime]
  (when (map? runtime)
    (let [v (:ferment.model.session/workers runtime)]
      (when (instance? clojure.lang.IDeref v)
        v))))

(defn- runtime-session-lock
  [runtime]
  (if (map? runtime)
    (or (:ferment.model.session/lock runtime) (Object.))
    (Object.)))

(defn- session-mode-enabled?
  [runtime]
  (and (map? runtime)
       (not= false (:ferment.model.session/enabled? runtime))))

(defn- session-idle-ttl-ms
  [runtime]
  (let [v (when (map? runtime) (:ferment.model.session/idle-ttl-ms runtime))]
    (long (max 0 (or (when (number? v) v)
                     default-session-idle-ttl-ms)))))

(defn- session-max-per-model
  [runtime]
  (let [v (when (map? runtime) (:ferment.model.session/max-per-model runtime))]
    (long (max 1 (or (when (number? v) v)
                     default-session-max-per-model)))))

(defn- normalize-runtime-session-id
  [sid]
  (cond
    (string? sid)  (some-> sid str/trim not-empty)
    (keyword? sid) (some-> sid name str/trim not-empty)
    (uuid? sid)    (str sid)
    (nil? sid)     nil
    :else          (some-> sid str str/trim not-empty)))

(defn- model-runtime-template
  [runtime model-k]
  (let [runtime' (some-> (model-entry runtime model-k) :runtime)]
    (cond
      (and (map? runtime') (map? (:config runtime'))) (:config runtime')
      (map? runtime') runtime'
      :else nil)))

(defn- sanitize-session-token
  [sid]
  (-> (or sid "")
      str
      str/lower-case
      (str/replace #"[^a-z0-9._-]+" "-")
      (str/replace #"^-+|-+$" "")
      not-empty
      (or "anon")))

(defn- session-worker-key
  [model-k sid]
  (keyword "ferment.model.runtime.session"
           (str (name model-k) "--" (sanitize-session-token sid))))

(defn- session-worker-config
  [runtime model-k sid]
  (when-some [template (model-runtime-template runtime model-k)]
    (let [sid' (normalize-runtime-session-id sid)
          sid* (or sid' "session/default")
          sid-token (sanitize-session-token sid*)
          worker-id (session-worker-key model-k sid*)
          worker-name (str (or (:name template) (name model-k) " model runtime")
                           " [session:" sid-token "]")]
      (-> template
          (dissoc :http)
          (assoc :id worker-id
                 :name worker-name
                 :session {:sid (str "ferment-model-runtime/"
                                     (name model-k)
                                     "/"
                                     sid-token)})))))

(defn- runtime-session-service
  [runtime]
  (let [svc (when (map? runtime) (:session runtime))]
    (when (map? svc) svc)))

(defn- touch-runtime-session!
  [runtime sid model-k]
  (when-some [service (runtime-session-service runtime)]
    (try
      (fsession/open! service sid {:session/meta {:source :model/invoke
                                                  :model model-k}})
      (catch Throwable _ nil))))

(defn- freeze-runtime-session!
  [runtime sid model-k reason]
  (when-some [service (runtime-session-service runtime)]
    (try
      (fsession/freeze! service sid {:session/meta {:source :model/runtime
                                                    :reason reason
                                                    :model model-k}})
      (catch Throwable _ nil))))

(defn- dissoc-session-entry
  [registry model-k sid]
  (let [registry' (update registry model-k #(dissoc (or % {}) sid))]
    (if (seq (get registry' model-k))
      registry'
      (dissoc registry' model-k))))

(defn- stop-session-entry!
  [runtime model-k sid {:keys [worker-state]} reason]
  (when (map? worker-state)
    (try
      (stop-runtime-worker (:id worker-state) worker-state)
      (catch Throwable _ nil)))
  (freeze-runtime-session! runtime sid model-k reason)
  nil)

(defn- touch-session-entry!
  [registry model-k sid]
  (swap! registry assoc-in [model-k sid :last-used-ms] (now-ms))
  (get-in @registry [model-k sid]))

(defn- drop-session-worker!
  [runtime model-k sid reason]
  (when-some [registry (runtime-session-registry runtime)]
    (when-some [entry (get-in @registry [model-k sid])]
      (swap! registry dissoc-session-entry model-k sid)
      (stop-session-entry! runtime model-k sid entry reason))
    nil))

(defn expire-session-workers!
  "Stops and removes idle session workers according to runtime TTL policy."
  [runtime]
  (when (session-mode-enabled? runtime)
    (when-some [registry (runtime-session-registry runtime)]
      (let [ttl-ms (session-idle-ttl-ms runtime)]
        (when (pos? ttl-ms)
          (let [now (now-ms)
                victims (for [[model-k sid->entry] @registry
                              [sid entry] sid->entry
                              :let [last-used-ms (long (or (:last-used-ms entry) 0))]
                              :when (> (- now last-used-ms) ttl-ms)]
                          [model-k sid entry])]
            (doseq [[model-k sid entry] victims]
              (swap! registry dissoc-session-entry model-k sid)
              (stop-session-entry! runtime model-k sid entry :session/ttl-expired)))))))
  runtime)

(defn- enforce-session-max-per-model!
  [runtime model-k keep-sid]
  (when-some [registry (runtime-session-registry runtime)]
    (let [max-per-model (session-max-per-model runtime)
          sid->entry (get @registry model-k)
          overflow (- (count sid->entry) max-per-model)]
      (when (pos? overflow)
        (let [victims (->> sid->entry
                           (remove (fn [[sid _]] (= sid keep-sid)))
                           (sort-by (fn [[_ entry]] (long (or (:last-used-ms entry) 0))))
                           (take overflow))]
          (doseq [[sid entry] victims]
            (swap! registry dissoc-session-entry model-k sid)
            (stop-session-entry! runtime model-k sid entry :session/max-per-model)))))))

(defn- ensure-session-worker!
  [runtime model-k sid]
  (when (and (session-mode-enabled? runtime)
             (keyword? model-k))
    (when-some [sid' (normalize-runtime-session-id sid)]
      (when-some [registry (runtime-session-registry runtime)]
        (expire-session-workers! runtime)
        (let [lock (runtime-session-lock runtime)]
          (locking lock
            (if-let [entry (get-in @registry [model-k sid'])]
              (touch-session-entry! registry model-k sid')
              (when-some [cfg (session-worker-config runtime model-k sid')]
                (touch-runtime-session! runtime sid' model-k)
                (let [worker-state (init-runtime-worker (:id cfg) cfg)
                      entry {:model-key model-k
                             :session/id sid'
                             :worker-state worker-state
                             :last-used-ms (now-ms)}]
                  (swap! registry assoc-in [model-k sid'] entry)
                  (enforce-session-max-per-model! runtime model-k sid')
                  entry)))))))))

(defn session-workers-state
  "Returns operator-friendly map of runtime session workers."
  [runtime]
  (if-some [registry (runtime-session-registry runtime)]
    (into {}
          (for [[model-k sid->entry] @registry]
            [model-k
             (into {}
                   (for [[sid {:keys [worker-state last-used-ms]}] sid->entry]
                     [sid {:last-used-ms last-used-ms
                           :worker-id (or (:id worker-state)
                                          (some-> worker-state :config :id))
                           :running? (some? (:worker worker-state))}]))]))
    {}))

(defn freeze-session-worker!
  "Stops session worker for model/session pair and marks runtime session as frozen."
  [runtime model-id session-id]
  (let [model-k (normalize-model-key model-id)
        sid     (normalize-runtime-session-id session-id)]
    (when (and (keyword? model-k) sid)
      (drop-session-worker! runtime model-k sid :session/manual-freeze))
    {:ok? true
     :model model-k
     :session/id sid}))

(defn thaw-session-worker!
  "Ensures session worker for model/session pair exists and is ready."
  [runtime model-id session-id]
  (let [model-k (normalize-model-key model-id)
        sid     (normalize-runtime-session-id session-id)
        entry   (when (and (keyword? model-k) sid)
                  (ensure-session-worker! runtime model-k sid))]
    {:ok? (some? entry)
     :model model-k
     :session/id sid
     :worker-id (some-> entry :worker-state :id)}))

(defn- restartable-invoke-error?
  [response]
  (let [details (when (map? response) (:details response))]
    (or (nil? response)
        (and (map? response)
             (= false (:ok? response))
             (= :invoke-failed (:error response))
             (contains? #{:runtime-process-missing
                          :runtime-process-dead
                          :runtime-process-io-unavailable}
                        (:error details))))))

(defn invoke-model!
  "Invokes model runtime worker with optional session-aware worker lifecycle.

  Returns worker response map (the same shape as `:invoke` command response)
  or nil when runtime worker is unavailable."
  ([runtime model-id payload]
   (invoke-model! runtime model-id payload nil))
  ([runtime model-id payload opts]
   (let [opts'    (if (map? opts) opts {})
         model-k  (normalize-model-key model-id)
         sid      (normalize-runtime-session-id (or (:session/id opts')
                                                   (:session-id opts')))
         _        (expire-session-workers! runtime)
         session-entry (when sid (ensure-session-worker! runtime model-k sid))
         worker-state  (or (:worker-state session-entry)
                           (some-> (model-entry runtime model-k) :runtime))
         worker        (some-> worker-state :worker)
         invoke!       (fn [wrk]
                         (when wrk
                           (command-bot-worker! wrk :invoke payload)))
         response0     (invoke! worker)
         response      (if (and sid (restartable-invoke-error? response0))
                         (do
                           (drop-session-worker! runtime model-k sid :session/restart)
                           (when-some [retry-entry (ensure-session-worker! runtime model-k sid)]
                             (invoke! (some-> retry-entry :worker-state :worker))))
                         response0)]
     (when (and sid (some? response))
       (touch-runtime-session! runtime sid model-k)
       (when-some [registry (runtime-session-registry runtime)]
         (touch-session-entry! registry model-k sid)))
     response)))

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
      "speakleash/Bielik-1.5B-v3.0-Instruct-MLX-8bit"))

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
