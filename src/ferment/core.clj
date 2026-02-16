(ns ferment.core

  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ferment.contracts :as contracts]
            [ferment.model :as model]
            [ferment.workflow :as workflow]
            [ferment.system :as system])

  (:import (java.io File)
           (java.lang ProcessBuilder)))

(defn- getenv
  ([k] (System/getenv k))
  ([k default]
   (or (some-> (System/getenv k) str/trim not-empty)
       default)))

(defn- env-name->config-key
  [env-name]
  (keyword "ferment.env"
           (-> (str env-name)
               str/lower-case
               (str/replace "_" "."))))

(defn- runtime-config
  [runtime]
  (cond
    (nil? runtime) nil
    (and (map? runtime) (map? (:runtime runtime))) (runtime-config (:runtime runtime))
    (and (map? runtime) (map? (:config runtime))) (:config runtime)
    (map? runtime) runtime
    :else nil))

(defn- runtime-protocol
  [runtime]
  (some-> (runtime-config runtime) :protocol))

(defn- protocol-default
  [protocol k fallback]
  (let [v (get protocol k ::missing)]
    (if (identical? ::missing v) fallback v)))

(declare input->prompt)

(defn- configv
  ([runtime env-name]
   (configv runtime env-name nil))
  ([runtime env-name default]
   (or (getenv env-name)
       (some-> (get (runtime-config runtime) (env-name->config-key env-name))
               str
               str/trim
               not-empty)
       default)))

(defn- llm-profile [runtime]
  (-> (or (configv runtime "FERMENT_PROFILE")
          "dev")
      str/lower-case))

(defn- llm-mode [runtime]
  (keyword (-> (configv runtime "FERMENT_LLM_MODE"
                        (if (= "test" (llm-profile runtime)) "mock" "live"))
               str/lower-case)))

(defn- mock-llm-response
  [{:keys [prompt]}]
  (if (and (string? prompt) (str/includes? prompt "Schema:"))
    "{\"intent\":\"mock\",\"summary\":\"mock response\",\"steps\":[],\"patch\":\"\",\"tests\":[],\"open_questions\":[]}"
    (str "MOCK_RESPONSE: "
         (subs (str/replace (or prompt "") #"\s+" " ")
               0 (min 120 (count (or prompt "")))))))

(def ^:private env-var-pattern
  #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}|\$([A-Za-z_][A-Za-z0-9_]*)")

(defn- expand-env-template
  [^String s ^java.util.Map pb-env]
  (str/replace
   (or s "")
   env-var-pattern
   (fn [[_ braced plain]]
     (let [k (or braced plain)]
       (or (some-> (.get pb-env k) str)
           (some-> (System/getenv k) str)
           (when (= "HOME" k) (System/getProperty "user.home"))
           "")))))

(defn sh!
  "Run a process, return {:exit int :out string :err string}."
  [command & {:keys [workdir env stdin]
              :or {env {}}}]
  (let [^java.util.List command (mapv str command)
        pb (doto (ProcessBuilder. ^java.util.List command)
             (.redirectErrorStream false))
        _  (when workdir
             (.directory pb (File. (str workdir))))
        _  (when (seq env)
             (let [^java.util.Map pb-env (.environment pb)]
               (doseq [[k v] env]
                 (.put pb-env
                       (str k)
                       (expand-env-template (str (or v "")) pb-env)))))
        p  (.start pb)
        _  (when (string? stdin)
             (let [^String in-str stdin]
               (with-open [^java.io.Writer w (io/writer (.getOutputStream p))]
                 (.write w in-str))))
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))
        ec  (.waitFor p)]
    {:exit ec :out out :err err}))

(defn- cap-id->model-key
  [cap-id intent]
  (or (case cap-id
        :llm/code :ferment.model/coding
        :llm/solver :ferment.model/solver
        :llm/meta :ferment.model/meta
        :llm/voice :ferment.model/voice
        nil)
      (if (= :text/respond intent)
        :ferment.model/voice
        (if (#{:code/generate :code/patch :code/explain :code/review} intent)
          :ferment.model/coding
          :ferment.model/solver))))

(defn- model-runtime-cfg
  [runtime cap-id intent]
  (let [model-k (cap-id->model-key cap-id intent)]
    (some-> (model/model-entry runtime model-k)
            :runtime)))

(defn- compose-prompt
  [system prompt]
  (if (and (string? system) (not (str/blank? system)))
    (str "SYSTEM:\n" system "\n\nUSER:\n" (or prompt "") "\n\nASSISTANT:\n")
    (or prompt "")))

(defn- extract-generated-text
  [{:keys [out err]}]
  (let [out' (some-> out str str/trim not-empty)
        err' (some-> err str str/trim not-empty)]
    (or out' err' "")))

(defn- run-model-command!
  [{:keys [command prompt env workdir prompt-via prompt-arg]}]
  (let [cmd0        (vec (map str command))
        prompt-via' (or prompt-via :arg)
        prompt-arg' (or prompt-arg "--prompt")
        cmd         (if (= :arg prompt-via')
                      (into cmd0 [prompt-arg' prompt])
                      cmd0)
        res         (sh! cmd
                         :workdir workdir
                         :env env
                         :stdin (when (= :stdin prompt-via') prompt))
        text        (extract-generated-text res)]
    (when-not (zero? (:exit res))
      (throw (ex-info "Model command failed"
                      {:exit (:exit res)
                       :command cmd
                       :stderr (:err res)
                       :stdout (:out res)})))
    {:response text
     :raw (assoc res :command cmd)}))

(defn ollama-generate!
  "Model generator (HF/MLX command backend, mock-aware).

  Name kept for backward compatibility in tests."
  [{:keys [runtime cap-id intent model prompt system mode]}]
  (if (= :mock mode)
    {:response (mock-llm-response {:prompt (compose-prompt system prompt)})
     :raw {:mode :mock}}
    (let [runtime-cfg (model-runtime-cfg runtime cap-id intent)
          command     (or (seq (:command runtime-cfg))
                          ["mlx_lm.chat" "--model" (str model)])
          env         (or (:env runtime-cfg) {})
          workdir     (:workdir runtime-cfg)
          prompt-via  (or (:prompt-via runtime-cfg) :arg)
          prompt-arg  (or (:prompt-arg runtime-cfg) "--prompt")]
      (run-model-command! {:command command
                           :prompt (compose-prompt system prompt)
                           :env env
                           :workdir workdir
                           :prompt-via prompt-via
                           :prompt-arg prompt-arg}))))

(defn looks-like-coding? [s]
  (boolean (re-find #"(stacktrace|Exception|NullPointer|compile|deps\.edn|defn|ns\s|\bClojure\b|patch|diff|regex|SQL|HTTP|API)" s)))

(defn- build-request
  [runtime {:keys [role intent cap-id input context constraints done budget effects protocol]}]
  (let [request-id (str (java.util.UUID/randomUUID))
        protocol'  (or protocol (runtime-protocol runtime) {})
        context'   (merge {:profile (keyword (llm-profile runtime))
                           :mode    (llm-mode runtime)}
                          (if (map? context) context {}))
        constraints' (or constraints (protocol-default protocol' :constraints/default nil))
        done'        (or done (protocol-default protocol' :done/default nil))
        budget'      (or budget (protocol-default protocol' :budget/default nil))
        effects'     (or effects (protocol-default protocol' :effects/default nil))
        input'       (if (map? input) input {:prompt (input->prompt input)})]
    (cond-> {:proto      1
             :trace      {:id request-id}
             :role       role
             :request/id request-id
             :session/id (str "session/" (llm-profile runtime))
             :cap/id     cap-id
             :context    context'
             :task       {:intent intent}
             :input      input'}
      (map? constraints') (assoc :constraints constraints')
      (map? done')        (assoc :done done')
      (map? budget')      (assoc :budget budget')
      (map? effects')     (assoc :effects effects'))))

(defn- default-result-parser
  [text {:keys [request mode]}]
  {:proto  1
   :trace  (:trace request)
   :result {:type  :value
            :out   {:text text}
            :usage {:mode mode}}})

(defn- intent->role
  [intent]
  (case intent
    :problem/solve :solver
    (:code/generate :code/patch :code/explain :code/review) :coder
    (:route/decide :context/summarize :eval/grade) :router
    :voice))

(defn- cap-id->role
  [cap-id intent]
  (or (case cap-id
        :llm/code  :coder
        :llm/solver :solver
        :llm/meta  :router
        :llm/voice :voice
        :llm/mock  :router
        nil)
      (intent->role intent)))

(defn- capability-model-id
  [runtime cap-id intent]
  (or (case cap-id
        :llm/code  (model/coding-id runtime)
        :llm/solver (model/solver-id runtime)
        :llm/meta  (model/meta-id runtime)
        :llm/voice (model/voice-id runtime)
        :llm/mock  "mock/model"
        nil)
      (if (= :text/respond intent)
        (model/voice-id runtime)
        (if (#{:code/generate :code/patch :code/explain :code/review} intent)
          (model/coding-id runtime)
          (model/solver-id runtime)))))

(defn- input->prompt
  [input]
  (cond
    (string? input) input
    (string? (:prompt input)) (:prompt input)
    (map? input) (pr-str input)
    (nil? input) ""
    :else (str input)))

(defn invoke-capability!
  "Invokes a model capability under protocol contract.

  Returns validated result envelope (`:result` or `:error`), which may represent
  `:value`, `:plan` or `:stream` depending on the configured parser."
  [runtime {:keys [role intent cap-id model system prompt input temperature
                   max-attempts result-parser context constraints done budget effects]}]
  (let [mode (llm-mode runtime)
        protocol (runtime-protocol runtime)
        input' (if (map? input)
                 input
                 {:prompt (or prompt "")})
        prompt' (input->prompt input')
        system' (or system
                    (when (map? input')
                      (:system input')))
        request (build-request runtime {:role role
                                        :intent intent
                                        :cap-id cap-id
                                        :input input'
                                        :context context
                                        :constraints constraints
                                        :done done
                                        :budget budget
                                        :effects effects
                                        :protocol protocol})
        invoke-fn (fn [_request _attempt]
                    (let [result (ollama-generate! {:model model
                                                    :runtime runtime
                                                    :cap-id cap-id
                                                    :intent intent
                                                    :system system'
                                                    :prompt prompt'
                                                    :temperature temperature
                                                    :mode mode})
                          text (:response result)]
                      (if (and (string? text) (not (str/blank? text)))
                        ((or result-parser default-result-parser)
                         text
                         {:request request
                          :mode mode
                          :attempt _attempt
                          :cap/id cap-id
                          :raw result})
                        ;; Missing :out makes the result invalid and triggers retry.
                        {:trace  (:trace request)
                         :result {:type :value}})))
        run (contracts/invoke-with-contract invoke-fn request
                                            {:max-attempts (or max-attempts
                                                               (protocol-default protocol :retry/max-attempts 3)
                                                               3)
                                             :protocol protocol})]
    (if (:ok? run)
      (:result run)
      (throw (ex-info "LLM invocation failed after retries" run)))))

(defn- invoke-plan-call!
  [runtime resolver]
  (fn [call-node _env]
    (let [intent  (:intent call-node)
          cap-id  (or (:cap/id call-node)
                      (workflow/resolve-capability-id resolver call-node))
          role    (or (:role call-node) (cap-id->role cap-id intent))
          prompt  (input->prompt (:input call-node))
          model   (or (:model call-node)
                      (capability-model-id runtime cap-id intent))
          system  (:system call-node)
          temp    (if (contains? call-node :temperature)
                    (:temperature call-node)
                    (if (= :voice role) 0.4 0.0))]
      (when-not cap-id
        (throw (ex-info "Cannot execute call node without capability id"
                        {:node call-node
                         :resolver resolver})))
      (invoke-capability! runtime
                          {:role role
                           :intent intent
                           :cap-id cap-id
                           :model model
                           :system system
                           :input (:input call-node)
                           :prompt prompt
                           :context (:context call-node)
                           :constraints (:constraints call-node)
                           :done (:done call-node)
                           :budget (:budget call-node)
                           :effects (:effects call-node)
                           :temperature temp
                           :max-attempts (:max-attempts call-node)
                           :result-parser (:result-parser call-node)}))))

(defn- emitted->out
  [emitted]
  (cond
    (and (map? emitted) (map? (:out emitted))) (:out emitted)
    (map? emitted) emitted
    (string? emitted) {:text emitted}
    (nil? emitted) {}
    :else {:value emitted}))

(defn execute-capability!
  "Invokes capability and executes returned plan (if any) in runtime evaluator."
  ([runtime opts]
   (execute-capability! runtime nil opts))
  ([runtime resolver opts]
   (let [result (invoke-capability! runtime opts)
         rtype  (contracts/result-type-of result)]
     (if (= :plan rtype)
       (let [plan (or (contracts/materialize-plan-result result)
                      (contracts/result-plan-of result))
             run  (workflow/execute-plan
                   {:plan plan
                    :resolver resolver
                    :invoke-call (invoke-plan-call! runtime resolver)})
             out  (emitted->out (:emitted run))]
         (assoc result
                :result {:type :value
                         :out out
                         :plan/run run}))
       result))))

(defn- find-text
  [v]
  (cond
    (string? v) v
    (and (map? v) (string? (:text v))) (:text v)
    (map? v) (some find-text (vals v))
    (sequential? v) (some find-text v)
    :else nil))

(defn- invoke-llm!
  "Value-only wrapper for simple text capabilities."
  [runtime opts]
  (let [resolver (some-> runtime :resolver)
        result (execute-capability! runtime resolver opts)
        rtype  (contracts/result-type-of result)]
    (if (= :value rtype)
      (let [text (find-text (contracts/result-out-of result))]
        (if (and (string? text) (not (str/blank? text)))
          text
          (throw (ex-info "Capability returned :value without :out/:text"
                          {:result result}))))
      (throw (ex-info "Capability returned non-value result; use invoke-capability! for stratified plan/stream flow."
                      {:result/type rtype
                       :result result
                       :plan/materialized (contracts/materialize-plan-result result)})))))

(defn coder!
  "Returns a strict JSON object as string for coding tasks."
  ([user-text]
   (coder! user-text nil))
  ([user-text runtime]
    (invoke-llm!
    runtime
    {:role :coder
     :intent :code/patch
     :cap-id :llm/code
     :model (model/coding-id runtime)
     :system (str
              "You are CODER. Do NOT write prose. Return ONLY valid JSON.\n"
              "Schema:\n"
              "{intent, summary, steps[], patch, tests[], open_questions[]}\n"
              "If no patch, use empty string. No markdown.")
     :prompt user-text
     :temperature 0})))

(defn solver!
  "Solves general (non-coding) problems and returns structured JSON string."
  ([user-text]
   (solver! user-text nil))
  ([user-text runtime]
   (invoke-llm!
    runtime
    {:role :solver
     :intent :problem/solve
     :cap-id :llm/solver
     :model (model/solver-id runtime)
     :system (str
              "You are SOLVER. Solve general problems precisely and briefly.\n"
              "Return ONLY valid JSON (no prose, no markdown).\n"
              "Schema:\n"
              "{intent, summary, answer, steps[], open_questions[]}")
     :prompt user-text
     :temperature 0})))

(defn voice!
  "Takes solver-json and produces Polish natural language. No new technical facts."
  ([solver-json user-text]
   (voice! solver-json user-text nil))
  ([solver-json user-text runtime]
    (invoke-llm!
    runtime
    {:role :voice
     :intent :text/respond
     :cap-id :llm/voice
     :model (model/voice-id runtime)
     :system (str
              "Jesteś VOICE. Twoim zadaniem jest sformułować wypowiedź po polsku.\n"
              "NIE wymyślaj faktów technicznych. Opieraj się WYŁĄCZNIE na JSON od SOLVER.\n"
              "Jeśli brakuje danych, zadaj max 2 pytania doprecyzowujące.\n"
              "Mów jasno i zwięźle, jak rzemieślnik.")
     :prompt (str "USER:\n" user-text "\n\nSOLVER_JSON:\n" solver-json)
     :temperature 0.4})))

(defn respond!
  ([user-text]
   (respond! user-text nil))
  ([user-text runtime]
   (if (looks-like-coding? user-text)
     (let [sj (coder! user-text runtime)]
       (voice! sj user-text runtime))
     (let [sj (solver! user-text runtime)]
       (voice! sj user-text runtime)))))

(defn preconfigure-service
  "Pre-configuration hook for core service branch."
  [_k config]
  config)

(defn init-service
  "Initializes core service map.

  Service is configuration-driven and keeps no global mutable state."
  [_k {:keys [runtime resolver protocol] :as config}]
  {:config   config
   :runtime  runtime
   :resolver resolver
   :protocol protocol
   :coder!   (fn [user-text] (coder! user-text runtime))
   :solver!  (fn [user-text] (solver! user-text runtime))
   :voice!   (fn [solver-json user-text] (voice! solver-json user-text runtime))
   :respond! (fn [user-text] (respond! user-text runtime))})

(defn stop-service
  "Stops core service branch (no-op for config-oriented service)."
  [_k _state]
  nil)

(derive ::default ::service)

(system/add-expand ::service [k config] {k (preconfigure-service k config)})
(system/add-init   ::service [k config]    (init-service k config))
(system/add-halt!  ::service [k state]     (stop-service k state))

(defn generate
  [& more]
  nil)

(defn -main [& _]
  (println "Agent ready. Type and press Enter. Ctrl-D to exit.")
  (with-open [r (io/reader System/in)]
    (doseq [line (line-seq r)]
      (println "\n---")
      (println (respond! line))
      (println "---\n"))))
