(ns ferment.core

  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [ferment.contracts :as contracts]
            [ferment.model :as model]
            [ferment.workflow :as workflow]
            [ferment.system :as system])

  (:import (java.lang ProcessBuilder)))

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

(defn sh!
  "Run a process, return {:exit int :out string :err string}."
  [& args]
  (let [^java.util.List command (mapv str args)
        pb (doto (ProcessBuilder. command)
             (.redirectErrorStream false))
        p  (.start pb)
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))
        ec  (.waitFor p)]
    {:exit ec :out out :err err}))

(defn ollama-chat!
  "Calls Ollama CLI. Expects model pulled already.
   Returns assistant text (best-effort)."
  [{:keys [model system user temperature]}]
  ;; Ollama CLI supports: `ollama run MODEL` with prompt via stdin.
  ;; We pack system+user into one prompt (quick & dirty).
  (let [prompt (str (when system (str "SYSTEM:\n" system "\n\n"))
                    "USER:\n" user "\n\nASSISTANT:\n")
        res (-> (apply sh! (concat ["ollama" "run" model]
                                   (when temperature ["--temperature" (str temperature)])))
                (update :out str))
        ;; If CLI doesn't read stdin in your setup, swap to HTTP API (below).
        ]
    ;; NOTE: Ollama CLI reads from stdin only in interactive mode; for reliability,
    ;; prefer the HTTP API. Keeping this minimal here.
    (:out res)))

;; --- reliable: Ollama HTTP API via curl (still minimal) ----------------------

(defn ollama-generate!
  "Reliable non-interactive call using Ollama HTTP API (localhost:11434).
   Returns {:response \"...\" :raw ...}."
  [{:keys [model prompt system temperature mode]}]
  (if (= :mock mode)
    {:response (mock-llm-response {:prompt (if system (str "SYSTEM:\n" system "\n\n" prompt) prompt)})
     :raw {:mode :mock}}
    (let [payload (cond-> {:model model
                         :prompt (if system (str "SYSTEM:\n" system "\n\n" prompt) prompt)
                         :stream false}
                  temperature (assoc :temperature temperature))
          tmp (java.io.File/createTempFile "ollama" ".json")]
      (spit tmp (json/write-str payload))
      (let [{:keys [exit out err]} (sh! "curl" "-s" "http://localhost:11434/api/generate"
                                        "-H" "Content-Type: application/json"
                                        "--data-binary" (str "@" (.getAbsolutePath tmp)))]
        (when-not (zero? exit)
          (throw (ex-info "Ollama call failed" {:exit exit :err err})))
        (let [m (json/read-str out :key-fn keyword)]
          {:response (:response m)
           :raw m})))))

(defn looks-like-coding? [s]
  (boolean (re-find #"(stacktrace|Exception|NullPointer|compile|deps\.edn|defn|ns\s|\bClojure\b|patch|diff|regex|SQL|HTTP|API)" s)))

(defn- build-request
  [runtime {:keys [role intent cap-id input]}]
  (let [request-id (str (java.util.UUID/randomUUID))]
    {:proto      1
     :trace      {:id request-id}
     :role       role
     :request/id request-id
     :session/id (str "session/" (llm-profile runtime))
     :cap/id     cap-id
     :context    {:profile (keyword (llm-profile runtime))
                  :mode    (llm-mode runtime)}
     :task       {:intent intent}
     :input      input}))

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
    (:code/generate :code/patch :code/explain :code/review) :coder
    (:route/decide :context/summarize :eval/grade) :router
    :voice))

(defn- cap-id->role
  [cap-id intent]
  (or (case cap-id
        :llm/code  :coder
        :llm/meta  :router
        :llm/voice :voice
        :llm/mock  :router
        nil)
      (intent->role intent)))

(defn- capability-model-id
  [runtime cap-id intent]
  (or (case cap-id
        :llm/code  (model/solver-id runtime)
        :llm/meta  (model/runtimev runtime :ferment.model/meta)
        :llm/voice (model/voice-id runtime)
        :llm/mock  "mock/model"
        nil)
      (if (= :text/respond intent)
        (model/voice-id runtime)
        (model/solver-id runtime))))

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
  [runtime {:keys [role intent cap-id model system prompt temperature max-attempts result-parser]}]
  (let [mode (llm-mode runtime)
        request (build-request runtime {:role role
                                        :intent intent
                                        :cap-id cap-id
                                        :input {:prompt prompt}})
        invoke-fn (fn [_request _attempt]
                    (let [result (ollama-generate! {:model model
                                                    :system system
                                                    :prompt prompt
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
                                            {:max-attempts (or max-attempts 3)})]
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
                           :prompt prompt
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

(defn solver!
  "Returns a strict JSON object as string."
  ([user-text]
   (solver! user-text nil))
  ([user-text runtime]
    (invoke-llm!
    runtime
    {:role :coder
     :intent :code/patch
     :cap-id :llm/code
     :model (model/solver-id runtime)
     :system (str
              "You are SOLVER. Do NOT write prose. Return ONLY valid JSON.\n"
              "Schema:\n"
              "{intent, summary, steps[], patch, tests[], open_questions[]}\n"
              "If no patch, use empty string. No markdown.")
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
     (let [sj (solver! user-text runtime)]
       (voice! sj user-text runtime))
     ;; non-coding: od razu voice
     (invoke-llm!
     runtime
     {:role :voice
       :intent :text/respond
       :cap-id :llm/voice
       :model (model/voice-id runtime)
       :system "Jesteś VOICE. Odpowiadasz po polsku, zwięźle i rzeczowo."
       :prompt user-text
       :temperature 0.6}))))

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
