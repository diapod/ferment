(ns ferment.core

  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ferment.contracts :as contracts]
            [ferment.effects :as effects]
            [ferment.model :as model]
            [ferment.router :as router]
            [ferment.session :as session]
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

(defn- runtime-effects
  [runtime]
  (or (some-> (runtime-config runtime) :effects)
      {}))

(declare input->prompt find-text llm-profile judge-score-from-output)

(defn- protocol-default
  [protocol k fallback]
  (let [v (get protocol k ::missing)]
    (if (identical? ::missing v) fallback v)))

(defn- intent-config
  [protocol intent]
  (if (and (map? protocol) (keyword? intent))
    (let [cfg (get-in protocol [:intents intent])]
      (if (map? cfg) cfg {}))
    {}))

(defn- intent-quality-config
  [protocol intent]
  (let [cfg (some-> (intent-config protocol intent) :quality)]
    (if (map? cfg) cfg {})))

(defn- intent-default-done
  [protocol intent]
  (let [done-cfg (:done (intent-quality-config protocol intent))]
    (if (map? done-cfg) done-cfg nil)))

(defn- intent-judge-config
  [protocol intent]
  (let [global-cfg (if (map? (:quality/judge protocol))
                     (:quality/judge protocol)
                     {})
        intent-cfg (let [cfg (:judge (intent-quality-config protocol intent))]
                     (if (map? cfg) cfg {}))]
    (merge global-cfg intent-cfg)))

(defn- parse-double-safe
  [v]
  (cond
    (number? v) (double v)
    (string? v) (try
                  (Double/parseDouble (str/trim v))
                  (catch Throwable _ nil))
    :else nil))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- parse-structured-text
  [s]
  (when (string? s)
    (let [s' (some-> s str/trim not-empty)]
      (when s'
        (or (try
              (json/parse-string s' true)
              (catch Throwable _ nil))
            (try
              (edn/read-string s')
              (catch Throwable _ nil)))))))

(defn- score-at-path
  [m score-path]
  (let [path-k (vec (or score-path [:score]))
        path-s (mapv (fn [k]
                       (if (keyword? k) (name k) k))
                     path-k)]
    (or (parse-double-safe (get-in m path-k))
        (parse-double-safe (get-in m path-s)))))

(declare judge-score-from-output)

(defn- score-from-map
  [m score-path]
  (or (score-at-path m score-path)
      (parse-double-safe (:score m))
      (parse-double-safe (get m "score"))
      (parse-double-safe (:eval/score m))
      (parse-double-safe (get m "eval/score"))
      (some-> (:text m) parse-structured-text (judge-score-from-output score-path))
      (some-> (get m "text") parse-structured-text (judge-score-from-output score-path))))

(defn- result-error?
  [result]
  (or (contains? result :error)
      (= :error (contracts/result-type-of result))))

(defn- tests-pass?
  [_call-node _env result]
  (let [out (contracts/result-out-of result)]
    (cond
      (result-error? result) false
      (boolean? (get out :tests/pass?)) (get out :tests/pass?)
      (sequential? (get out :tests))
      (every? (fn [t]
                (or (true? t)
                    (and (map? t)
                         (contains? #{:pass "pass" true} (:status t)))))
              (get out :tests))
      :else true)))

(defn- no-hallucinated-apis?
  [_call-node _env result]
  (let [out (contracts/result-out-of result)
        violations (set (or (when (set? (:violations out)) (:violations out))
                            (when (sequential? (:violations out)) (:violations out))
                            []))]
    (cond
      (= :hallucinated/api (get-in result [:error :type])) false
      (seq (get out :hallucinated-apis)) false
      (contains? violations :hallucinated/api) false
      :else true)))

(defn- schema-valid?
  [_call-node _env result]
  (:ok? (contracts/validate-result result)))

(def ^:private builtin-check-fns
  {:schema-valid schema-valid?
   :tests-pass tests-pass?
   :no-hallucinated-apis no-hallucinated-apis?})

(def ^:private default-check-descriptors
  {:schema-valid :builtin/schema-valid
   :tests-pass :builtin/tests-pass
   :no-hallucinated-apis :builtin/no-hallucinated-apis})

(defn- resolve-check-descriptor
  [descriptor]
  (cond
    (= descriptor :builtin/schema-valid) schema-valid?
    (= descriptor :builtin/tests-pass) tests-pass?
    (= descriptor :builtin/no-hallucinated-apis) no-hallucinated-apis?
    (keyword? descriptor) (get builtin-check-fns descriptor)
    (ifn? descriptor) descriptor
    :else nil))

(defn- runtime-check-fns
  [runtime]
  (let [protocol    (or (runtime-protocol runtime) {})
        descriptors (merge default-check-descriptors
                           (or (:quality/checks protocol) {}))]
    (reduce-kv
     (fn [acc check-k descriptor]
       (if-some [f (resolve-check-descriptor descriptor)]
         (assoc acc check-k f)
         acc))
     {}
     descriptors)))

(defn- runtime-session
  [runtime]
  (some-> (runtime-config runtime) :session))

(defn- keyword-set
  [v]
  (cond
    (set? v) (into #{} (filter keyword?) v)
    (sequential? v) (into #{} (filter keyword?) v)
    (keyword? v) #{v}
    :else #{}))

(defn- resolve-session-id
  [runtime opts]
  (or (:session/id opts)
      (:session-id opts)
      (get-in opts [:context :session/id])
      (get-in opts [:input :session/id])
      (str "session/" (llm-profile runtime))))

(defn- auth-user-session-meta
  [auth-user]
  (let [roles' (->> (keyword-set (or (:user/roles auth-user)
                                     (:roles auth-user)))
                    sort
                    vec)]
    (cond-> {}
      (some? (:user/id auth-user)) (assoc :user/id (:user/id auth-user))
      (some? (:user/email auth-user)) (assoc :user/email (:user/email auth-user))
      (some? (:user/account-type auth-user)) (assoc :user/account-type (:user/account-type auth-user))
      (seq roles') (assoc :user/roles roles'))))

(defn- open-runtime-session!
  [runtime opts]
  (let [service (or (:session/service opts)
                    (runtime-session runtime))
        sid     (resolve-session-id runtime opts)
        auth-user (when (map? (:auth/user opts))
                    (:auth/user opts))
        meta-from-opts (if (map? (:session/meta opts))
                         (:session/meta opts)
                         {})
        meta'   (merge {:source :core/invoke-capability}
                       (if (map? auth-user)
                         (auth-user-session-meta auth-user)
                         {})
                       meta-from-opts)]
    (when (and (map? service) sid)
      {:service service
       :state   (session/open! service sid {:session/meta meta'})
       :id      sid})))

(defn- append-session-turn-safe!
  [session-service sid turn]
  (when (and (map? session-service)
             sid
             (map? turn))
    (try
      (session/append-turn! session-service sid turn)
      (catch Throwable _ nil))))

(def ^:private session-response-keys
  [:session/id
   :session/version
   :session/state
   :session/frozen?
   :session/updated-at
   :session/last-access-at
   :session/frozen-at
   :session/thawed-at])

(defn- session-state-safe
  [session-service sid]
  (when (and (map? session-service) sid)
    (try
      (session/get! session-service sid)
      (catch Throwable _ nil))))

(defn- attach-session-response
  [response sid session-state]
  (if-not (and (map? response) sid)
    response
    (let [state' (when (map? session-state)
                   (select-keys session-state session-response-keys))]
      (cond-> response
        sid (assoc :session/id sid)
        (map? state') (merge (dissoc state' :session/id))))))

(defn- session-turn-text
  [v]
  (or (find-text v)
      (when (string? v) v)
      (when (some? v) (pr-str v))
      ""))

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
             (let [in-str ^String stdin]
               (with-open [^java.io.Writer w (io/writer (.getOutputStream p))]
                 (.write w ^String in-str))))
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))
        ec  (.waitFor p)]
    {:exit ec :out out :err err}))

(defn- model-id-by-key
  [runtime model-k]
  (case model-k
    :ferment.model/coding (model/coding-id runtime)
    :ferment.model/solver (model/solver-id runtime)
    :ferment.model/meta   (model/meta-id runtime)
    :ferment.model/voice  (model/voice-id runtime)
    (or (when (keyword? model-k)
          (model/model-id runtime model-k nil))
        (model/solver-id runtime))))

(defn- model-runtime-cfg
  [runtime resolver cap-id intent]
  (let [model-k (router/resolve-model-key runtime resolver cap-id intent)]
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

(defn- runtime-invoke-text
  [response]
  (let [result (when (map? response) (:result response))]
    (or (when (string? result) result)
        (when (string? (:text result)) (:text result))
        (when (string? (:stdout result)) (:stdout result))
        (when (string? (:stderr result)) (:stderr result)))))

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
  [{:keys [runtime resolver cap-id intent model prompt system mode session-id]}]
  (if (= :mock mode)
    {:response (mock-llm-response {:prompt (compose-prompt system prompt)})
     :raw {:mode :mock}}
    (let [model-k (router/resolve-model-key runtime resolver cap-id intent)
          invoke-response (when model-k
                            (model/invoke-model!
                             runtime
                             model-k
                             {:prompt prompt
                              :system system}
                             {:session/id session-id}))
          invoke-text (runtime-invoke-text invoke-response)]
      (cond
        (and (map? invoke-response)
             (= false (:ok? invoke-response)))
        (throw (ex-info "Model runtime invoke failed."
                        {:error :runtime-invoke-failed
                         :cap-id cap-id
                         :intent intent
                         :model-key model-k
                         :session/id session-id
                         :invoke-response invoke-response}))

        (some? invoke-response)
        {:response (or invoke-text "")
         :raw {:mode :runtime/invoke
               :invoke-response invoke-response}}

        :else
        (let [runtime-cfg (model-runtime-cfg runtime resolver cap-id intent)
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
                               :prompt-arg prompt-arg}))))))

(defn looks-like-coding? [s]
  (boolean (re-find #"(stacktrace|Exception|NullPointer|compile|deps\.edn|defn|ns\s|\bClojure\b|patch|diff|regex|SQL|HTTP|API)" s)))

(defn- build-request
  [runtime {:keys [role intent cap-id input context constraints done budget effects protocol
                   auth-user
                   session-id session-version session-state request-id trace proto]}]
  (let [request-id (or (some-> request-id str str/trim not-empty)
                       (str (java.util.UUID/randomUUID)))
        protocol'  (or protocol (runtime-protocol runtime) {})
        trace-id   (or (some-> trace :id str str/trim not-empty)
                       request-id)
        trace-turn (when (and (map? trace)
                              (nat-int? (:turn trace)))
                     (:turn trace))
        trace'     (cond-> {:id trace-id}
                     (some? trace-turn) (assoc :turn trace-turn))
        proto'     (let [p (or proto
                               (protocol-default protocol' :proto/version 1)
                               1)]
                     (if (pos-int? p) p 1))
        session-fragment (when (map? session-state)
                           (cond-> {:session/id      (:session/id session-state)
                                    :session/version (:session/version session-state)
                                    :session/state   (:session/state session-state)
                                    :session/frozen? (:session/frozen? session-state)}
                             (contains? session-state :session/summary)
                             (assoc :session/summary (:session/summary session-state))))
        auth-fragment
        (when (map? auth-user)
          (let [roles' (->> (keyword-set (or (:user/roles auth-user)
                                             (:roles auth-user)))
                            sort
                            vec)]
            {:auth/user
             (cond-> {}
               (some? (:user/id auth-user)) (assoc :id (:user/id auth-user))
               (some? (:user/email auth-user)) (assoc :email (:user/email auth-user))
               (some? (:user/account-type auth-user)) (assoc :account-type (:user/account-type auth-user))
               (seq roles') (assoc :roles roles'))}))
        context'   (merge {:profile (keyword (llm-profile runtime))
                           :mode    (llm-mode runtime)}
                          (if (map? session-fragment) session-fragment {})
                          (if (map? auth-fragment) auth-fragment {})
                          (if (map? context) context {}))
        constraints' (or constraints (protocol-default protocol' :constraints/default nil))
        done'        (or done
                         (intent-default-done protocol' intent)
                         (protocol-default protocol' :done/default nil))
        budget'      (or budget (protocol-default protocol' :budget/default nil))
        effects'     (or effects (protocol-default protocol' :effects/default nil))
        input'       (if (map? input) input {:prompt (input->prompt input)})]
    (cond-> {:proto      proto'
             :trace      trace'
             :role       role
             :request/id request-id
             :session/id (or session-id (str "session/" (llm-profile runtime)))
             :cap/id     cap-id
             :context    context'
             :task       {:intent intent}
             :input      input'}
      (some? session-version) (assoc :session/version session-version)
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

(defn- default-stream-result-parser
  [text {:keys [request mode]}]
  {:proto 1
   :trace (:trace request)
   :result {:type :stream
            :stream [{:seq 0
                      :event :delta
                      :text text}
                     {:seq 1
                      :event :done}]
            :usage {:mode mode}}})

(defn- stream-response-mode?
  [opts]
  (let [response-type (or (keywordish (:response/type opts))
                          (keywordish (get-in opts [:response :type]))
                          (keywordish (:result/type opts))
                          (keywordish (:result-type opts)))]
    (or (= :stream response-type)
        (true? (:stream? opts))
        (true? (get-in opts [:response :stream?])))))

(defn- judge-result-parser
  [text {:keys [request mode]}]
  (let [parsed (parse-structured-text text)
        score  (judge-score-from-output (or parsed text) [:score])
        out-map (cond
                  (map? parsed) parsed
                  :else {:text text})
        out-map (if (number? score)
                  (assoc out-map :score score)
                  out-map)]
    {:proto  1
     :trace  (:trace request)
     :result {:type  :value
              :out   out-map
              :usage {:mode mode}}}))

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
                   max-attempts result-parser context constraints done budget effects
                   request-id trace proto session-version resolver]
            :as opts}]
  (let [mode (llm-mode runtime)
        protocol (runtime-protocol runtime)
        parser (or result-parser
                   (when (stream-response-mode? opts)
                     default-stream-result-parser)
                   default-result-parser)
        resolver' (router/resolver-config runtime resolver)
        input' (if (map? input)
                 input
                 {:prompt (or prompt "")})
        prompt' (input->prompt input')
        system' (or system
                    (when (map? input')
                      (:system input')))
        session-runtime (open-runtime-session! runtime opts)
        session-service (:service session-runtime)
        session-id (:id session-runtime)
        session-state (:state session-runtime)
        request (build-request runtime {:role role
                                        :intent intent
                                        :cap-id cap-id
                                        :input input'
                                        :context context
                                        :constraints constraints
                                        :done done
                                        :budget budget
                                        :effects effects
                                        :protocol protocol
                                        :request-id request-id
                                        :trace trace
                                        :proto proto
                                        :auth-user (:auth/user opts)
                                        :session-id session-id
                                        :session-version (or session-version
                                                             (:session/version session-state))
                                        :session-state session-state})
        _ (append-session-turn-safe! session-service session-id
                                     {:turn/role :user
                                      :turn/text prompt'
                                      :turn/request-id (:request/id request)
                                      :turn/intent intent
                                      :turn/cap-id cap-id})
        invoke-fn (fn [request* attempt-no]
                    (let [result (ollama-generate! {:model model
                                                    :runtime runtime
                                                    :resolver resolver'
                                                    :cap-id cap-id
                                                    :intent intent
                                                    :system system'
                                                    :prompt prompt'
                                                    :session-id session-id
                                                    :temperature temperature
                                                    :mode mode})
                          text (:response result)]
                      (if (and (string? text) (not (str/blank? text)))
                        (parser
                         text
                         {:request request*
                          :mode mode
                          :attempt attempt-no
                          :cap/id cap-id
                          :raw result})
                        ;; Missing :out makes the result invalid and triggers retry.
                        {:trace  (:trace request*)
                         :result {:type :value}})))
        run (contracts/invoke-with-contract invoke-fn request
                                            {:max-attempts (or max-attempts
                                                               (protocol-default protocol :retry/max-attempts 3)
                                                               3)
                                             :protocol protocol})]
    (if (:ok? run)
      (let [result (:result run)]
        (append-session-turn-safe! session-service session-id
                                   {:turn/role :assistant
                                    :turn/text (session-turn-text
                                                (or (contracts/result-out-of result)
                                                    result))
                                    :turn/request-id (:request/id request)
                                    :turn/intent intent
                                    :turn/cap-id cap-id
                                    :turn/result-type (contracts/result-type-of result)})
        (attach-session-response
         result
         session-id
         (or (session-state-safe session-service session-id)
             session-state)))
      (do
        (append-session-turn-safe! session-service session-id
                                   {:turn/role :assistant
                                    :turn/text (session-turn-text
                                                (or (:last-result run)
                                                    (:last-check run)
                                                    run))
                                    :turn/request-id (:request/id request)
                                    :turn/intent intent
                                    :turn/cap-id cap-id
                                    :turn/error (:error run)
                                    :turn/retries (:attempts run)})
        (throw (ex-info "LLM invocation failed after retries" run))))))

(defn- invoke-plan-call!
  [runtime resolver]
  (fn [call-node env]
    (let [intent  (:intent call-node)
          cap-id  (or (:cap/id call-node)
                      (workflow/resolve-capability-id resolver call-node))
          role    (or (:role call-node) (router/resolve-role runtime resolver cap-id intent))
          prompt  (input->prompt (:input call-node))
          model-k (router/resolve-model-key runtime resolver cap-id intent)
          model   (or (:model call-node)
                      (model-id-by-key runtime model-k))
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
                           :result-parser (:result-parser call-node)
                           :auth/user (:auth/user env)
                           :roles (:roles/config env)
                           :resolver resolver}))))

(defn- judge-score-from-output
  [out score-path]
  (or (parse-double-safe out)
      (when (map? out)
        (or (score-from-map out score-path)
            (parse-double-safe (:grade/score out))
            (parse-double-safe (get out "grade/score"))))
      (when (string? out)
        (some-> out
                parse-structured-text
                (judge-score-from-output score-path)))))

(defn- runtime-judge-fn
  [runtime resolver parent-opts]
  (let [protocol (or (runtime-protocol runtime) {})]
    (fn [call-node env result]
      (let [intent       (:intent call-node)
            judge-cfg    (intent-judge-config protocol intent)
            enabled?     (boolean (:enabled? judge-cfg))
            judge-intent (or (:intent judge-cfg) :eval/grade)
            judge-cap    (or (:cap/id judge-cfg)
                             (some-> (router/resolver-routing runtime resolver)
                                     :intent->cap
                                     (get judge-intent))
                             :llm/judge)
            judge-role   (or (:role judge-cfg) :router)
            score-path   (:score-path judge-cfg)
            max-attempts (or (:max-attempts judge-cfg) 1)]
        (when (and enabled?
                   (not (or (= judge-intent intent)
                            (= judge-cap (:cap/id call-node)))))
          (try
            (let [judge-result (invoke-capability!
                                runtime
                                {:role judge-role
                                 :intent judge-intent
                                 :cap-id judge-cap
                                 :input {:task/call call-node
                                         :task/result result
                                         :task/done (:done call-node)
                                         :task/env env}
                                 :context {:judge/for-intent intent
                                           :judge/for-cap (:cap/id call-node)}
                                 :max-attempts max-attempts
                                 :result-parser judge-result-parser
                                 :auth/user (:auth/user parent-opts)
                                 :roles (:roles parent-opts)
                                 :session/id (:session/id parent-opts)})
                  out (contracts/result-out-of judge-result)
                  score (judge-score-from-output out score-path)]
              (cond-> {}
                (number? score) (assoc :score score)))
            (catch Throwable _
              nil)))))))

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
   (let [effects-cfg (runtime-effects runtime)
         check-fns (runtime-check-fns runtime)
         judge-fn  (runtime-judge-fn runtime resolver opts)
         result (invoke-capability! runtime opts)
         rtype  (contracts/result-type-of result)
         workflow-env (cond-> {}
                        (map? (:workflow/env opts)) (merge (:workflow/env opts))
                        (map? (:auth/user opts)) (assoc :auth/user (:auth/user opts))
                        (map? (:roles opts)) (assoc :roles/config (:roles opts)))]
     (if (= :plan rtype)
       (let [plan (or (contracts/materialize-plan-result result)
                      (contracts/result-plan-of result))
             run  (workflow/execute-plan
                   {:plan plan
                    :resolver resolver
                    :invoke-call (invoke-plan-call! runtime resolver)
                    :invoke-tool (fn [tool-node env]
                                   (effects/invoke-tool! effects-cfg tool-node env))
                    :check-fns check-fns
                    :judge-fn judge-fn
                    :env workflow-env})
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

(defn- normalize-invoke-opts
  [opts]
  (if (map? opts) opts {}))

(defn coder!
  "Returns a strict JSON object as string for coding tasks."
  ([user-text]
   (coder! user-text nil nil))
  ([user-text runtime]
   (coder! user-text runtime nil))
  ([user-text runtime opts]
   (let [opts' (normalize-invoke-opts opts)]
    (invoke-llm!
    runtime
    (merge opts'
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
            :temperature 0})))))

(defn solver!
  "Solves general (non-coding) problems and returns structured JSON string."
  ([user-text]
   (solver! user-text nil nil))
  ([user-text runtime]
   (solver! user-text runtime nil))
  ([user-text runtime opts]
   (let [opts' (normalize-invoke-opts opts)]
   (invoke-llm!
    runtime
    (merge opts'
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
            :temperature 0})))))

(defn voice!
  "Takes solver-json and produces Polish natural language. No new technical facts."
  ([solver-json user-text]
   (voice! solver-json user-text nil nil))
  ([solver-json user-text runtime]
   (voice! solver-json user-text runtime nil))
  ([solver-json user-text runtime opts]
   (let [opts' (normalize-invoke-opts opts)]
    (invoke-llm!
    runtime
    (merge opts'
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
            :temperature 0.4})))))

(defn respond!
  ([user-text]
   (respond! user-text nil nil))
  ([user-text runtime]
   (respond! user-text runtime nil))
  ([user-text runtime opts]
   (let [opts' (normalize-invoke-opts opts)]
   (if (looks-like-coding? user-text)
     (let [sj (coder! user-text runtime opts')]
       (voice! sj user-text runtime opts'))
     (let [sj (solver! user-text runtime opts')]
       (voice! sj user-text runtime opts'))))))

(defn- with-session-service
  [session-service opts]
  (let [opts' (normalize-invoke-opts opts)]
    (if (and (map? session-service)
             (not (contains? opts' :session/service)))
      (assoc opts' :session/service session-service)
      opts')))

(defn preconfigure-service
  "Pre-configuration hook for core service branch."
  [_k config]
  config)

(defn init-service
  "Initializes core service map.

  Service is configuration-driven and keeps no global mutable state."
  [_k {:keys [runtime resolver protocol session] :as config}]
  {:config   config
   :runtime  runtime
   :resolver resolver
   :protocol protocol
   :session  session
   :session-open! (fn
                    ([sid] (when (map? session) (session/open! session sid)))
                    ([sid opts] (when (map? session) (session/open! session sid opts))))
   :session-get! (fn [sid]
                   (when (map? session) (session/get! session sid)))
   :session-freeze! (fn
                      ([sid] (when (map? session) (session/freeze! session sid)))
                      ([sid opts] (when (map? session) (session/freeze! session sid opts))))
   :session-thaw! (fn
                    ([sid] (when (map? session) (session/thaw! session sid)))
                    ([sid opts] (when (map? session) (session/thaw! session sid opts))))
   :session-workers (fn []
                      (model/session-workers-state runtime))
   :session-worker-freeze! (fn [model-id sid]
                             (model/freeze-session-worker! runtime model-id sid))
   :session-worker-thaw! (fn [model-id sid]
                           (model/thaw-session-worker! runtime model-id sid))
   :coder!   (fn
               ([user-text] (coder! user-text runtime (with-session-service session nil)))
               ([user-text opts] (coder! user-text runtime (with-session-service session opts))))
   :solver!  (fn
               ([user-text] (solver! user-text runtime (with-session-service session nil)))
               ([user-text opts] (solver! user-text runtime (with-session-service session opts))))
   :voice!   (fn
               ([solver-json user-text]
                (voice! solver-json user-text runtime (with-session-service session nil)))
               ([solver-json user-text opts]
                (voice! solver-json user-text runtime (with-session-service session opts))))
   :respond! (fn
               ([user-text] (respond! user-text runtime (with-session-service session nil)))
               ([user-text opts] (respond! user-text runtime (with-session-service session opts))))})

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
