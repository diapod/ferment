(ns

    ^{:doc    "ferment, var tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.var-test

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.edn                     :as             edn]
            [clojure.test                    :refer [deftest
                                                     is
                                                     testing]]
            [clojure.spec.alpha              :as               s]
            [clojure.spec.gen.alpha          :as             gen]
            [orchestra.spec.test             :as              st]
            [ferment.caps                    :as            caps]
            [ferment.core                    :as            core]
            [ferment.http                    :as           fhttp]
            [ferment.model                   :as           model]
            [ferment.session                 :as         session]
            [ferment.session.store           :as   session-store]
            [ferment.system                  :as          system]
            [ferment.resolver                :as        resolver]
            [io.randomseed.utils.bot         :as             bot]
            [io.randomseed.utils.bus         :as             bus]
            [ferment                       :refer         :all]
            [expound.alpha                   :as         expound]))

(s/check-asserts true)

(defn- read-edn-with-integrant-readers
  [path]
  (edn/read-string {:readers {'ref identity 'refset identity}}
                   (slurp path)))

(deftest capabilities-config-is-flattened
  (testing "Każde capability jest osobnym kluczem, ma metadata kontraktu i agregat nadal istnieje."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/capabilities.edn")
          cap-keys #{:ferment.caps.registry/llm-voice
                     :ferment.caps.registry/llm-code
                     :ferment.caps.registry/llm-solver
                     :ferment.caps.registry/llm-meta
                     :ferment.caps.registry/llm-mock}
          refs (get cfg :ferment.caps/registry)]
      (is (every? #(contains? cfg %) cap-keys))
      (is (every? (fn [k]
                    (let [cap (get cfg k)]
                      (and (contains? cap :cap/intents)
                           (contains? cap :cap/can-produce)
                           (contains? cap :cap/effects-allowed))))
                  cap-keys))
      (is (= 5 (count refs)))
      (is (= cap-keys (set refs))))))

(deftest resolver-config-references-flat-capabilities
  (testing "Resolver ma listę cap refs i wyprowadza indeks :caps/by-id."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/resolver.edn")
          default (get cfg :ferment.resolver/default)
          caps [{:cap/id :llm/voice :x 1}
                {:cap/id :llm/code :x 2}]
          initialized (resolver/init-resolver :ferment.resolver/default
                                              {:caps caps})]
      (is (map? default))
      (is (= #{:ferment.caps.registry/llm-voice
               :ferment.caps.registry/llm-code
               :ferment.caps.registry/llm-solver
               :ferment.caps.registry/llm-meta
               :ferment.caps.registry/llm-mock}
             (set (:caps default))))
      (is (= 2 (count (:caps/by-id initialized))))
      (is (= {:cap/id :llm/code :x 2}
             (get-in initialized [:caps/by-id :llm/code]))))))

(deftest caps-entry-hooks-normalize-capability-metadata
  (testing "Hooki entry normalizują metadata kontraktu capabilities."
    (let [entry {:cap/id :llm/meta
                 :dispatch/tag :meta
                 :cap/intents [:route/decide]
                 :cap/can-produce :plan}
          initialized (caps/init-capability-value
                       :ferment.caps.registry/llm-meta
                       entry)]
      (is (= entry
             (caps/preconfigure-capability-value
              :ferment.caps.registry/llm-meta
              entry)))
      (is (= #{:route/decide} (:cap/intents initialized)))
      (is (= #{:plan} (:cap/can-produce initialized)))
      (is (= #{:none} (:cap/effects-allowed initialized))))))

(deftest caps-entry-hooks-fail-fast-on-missing-required-metadata
  (testing "Init capability fail-fastuje, gdy brakuje wymaganych kluczy metadata."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Capability metadata is incomplete."
         (caps/init-capability-value
          :ferment.caps.registry/llm-meta
          {:cap/id :llm/meta
           :dispatch/tag :meta
           :cap/intents #{:route/decide}})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Capability metadata is incomplete."
         (caps/init-capability-value
          :ferment.caps.registry/llm-meta
          {:cap/id :llm/meta
           :dispatch/tag :meta
           :cap/can-produce #{:plan}})))))

(deftest runtime-config-contains-core-runtime-branch
  (testing "Runtime branch ma refs do resolvera/protokołu, sesji oraz agregat modeli."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/runtime.edn")
          runtime (get cfg :ferment.runtime/default)]
      (is (map? runtime))
      (is (= :ferment.resolver/default (:resolver runtime)))
      (is (= :ferment.protocol/default (:protocol runtime)))
      (is (= :ferment.session/default (:session runtime)))
      (is (= :ferment/models
             (:models runtime))))))

(deftest http-config-references-models-aggregate
  (testing "Gałąź HTTP ma klucz :ferment.http/default i referencję do :ferment/models."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/http.edn")
          http (get cfg :ferment.http/default)]
      (is (map? http))
      (is (= "127.0.0.1" (:host http)))
      (is (= 12002 (:port http)))
      (is (= :ferment/models (:models http))))))

(deftest http-route-builder-collects-enabled-runtime-endpoints
  (testing "Endpointy HTTP są budowane tylko dla runtime z :http {:enabled? true ...}."
    (let [worker-a {:worker-id :a}
          worker-b {:worker-id :b}
          routes (fhttp/model-http-routes
                  {:ferment.model/solver {:runtime {:id :ferment.model.runtime/solver
                                                    :worker worker-a
                                                    :config {:http {:enabled? true
                                                                    :endpoint "solver/responses"}}}}
                   :ferment.model/meta   {:runtime {:id :ferment.model.runtime/meta
                                                    :worker worker-b
                                                    :config {:http {:enabled? true
                                                                    :endpoint "/meta/responses"}}}}
                   :ferment.model/voice  {:runtime {:id :ferment.model.runtime/voice
                                                    :worker worker-b
                                                    :config {:http {:enabled? false
                                                                    :endpoint "voice/responses"}}}}})]
      (is (= #{"/solver/responses" "/meta/responses"} (set (keys routes))))
      (is (= :ferment.model/solver (get-in routes ["/solver/responses" :model])))
      (is (= :ferment.model.runtime/meta (get-in routes ["/meta/responses" :worker-id]))))))

(deftest models-config-defines-selector-values-in-edn
  (testing "Selektory modeli są utrzymywane w models.edn."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/models.edn")]
      (is (= "default" (get cfg :ferment.model.defaults/profile)))
      (is (= "mlx-community/Qwen2.5-7B-Instruct-4bit"
             (get-in cfg [:ferment.model.id/solver :id/default])))
      (is (= "mlx-community/SmolLM3-3B-8bit"
             (get-in cfg [:ferment.model.id/solver :id/mini])))
      (is (= "speakleash/Bielik-4.5B-v3.0-Instruct-MLX-8bit"
             (get-in cfg [:ferment.model.id/voice :id/default])))
      (is (= {"HF_HOME" :ferment.env/hf.home
              "HF_HUB_CACHE" :ferment.env/hf.hub.cache
              "PATH" "${HOME}/.pyenv/shims:${PATH}"}
             (get-in cfg [:ferment.model.defaults/runtime :env])))
      (is (= :ferment.model.defaults/runtime
             (get-in cfg [:ferment.model.runtime/solver :defaults]))))))

(deftest model-runtime-config-wires-session-and-workers
  (testing "models.edn zawiera runtime defaults, runtime gałęzie i agregat :ferment/models."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/models.edn")
          session (get cfg :ferment.model.defaults/bot-session)
          defaults (get cfg :ferment.model.defaults/runtime)
          solver-rt  (get cfg :ferment.model.runtime/solver)
          voice-rt   (get cfg :ferment.model.runtime/voice)
          models (get cfg :ferment/models)]
      (is (map? session))
      (is (= "ferment-model-runtime" (:sid session)))
      (is (= :ferment.model.defaults/bot-session (:session defaults)))
      (is (= :ferment.model.defaults/runtime (:defaults solver-rt)))
      (is (= :ferment.model.defaults/runtime (:defaults voice-rt)))
      (is (= :ferment.model/solver (get models :ferment.model/solver)))
      (is (= :ferment.model/voice  (get models :ferment.model/voice))))))

(deftest core-config-references-runtime-branches
  (testing "Core branch ma refs do runtime/resolver/protocol/session."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/core.edn")
          corecfg (get cfg :ferment.core/default)]
      (is (map? corecfg))
      (is (= :ferment.runtime/default (:runtime corecfg)))
      (is (= :ferment.resolver/default (:resolver corecfg)))
      (is (= :ferment.protocol/default (:protocol corecfg)))
      (is (= :ferment.session/default (:session corecfg))))))

(deftest session-config-defines-store-manager-and-service
  (testing "Session config definiuje gałęzie store/context/manager/service."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/session.edn")]
      (is (= :db
             (get-in cfg [:ferment.session.store/default :backend])))
      (is (= :ferment.db/main
             (get-in cfg [:ferment.session.store/default :db])))
      (is (= :sessions
             (get-in cfg [:ferment.session.store/default :sessions-table])))
      (is (= :session_vars
             (get-in cfg [:ferment.session.store/default :vars-table])))
      (is (= 1
             (get-in cfg [:ferment.session.context/default :context/version])))
      (is (= :ferment.session.store/default
             (get-in cfg [:ferment.session.manager/default :store])))
      (is (= :ferment.session.context/default
             (get-in cfg [:ferment.session.manager/default :context])))
      (is (= :ferment.session.manager/default
             (get-in cfg [:ferment.session/default :manager]))))))

(deftest session-manager-open-freeze-thaw-and-turns
  (testing "Session manager wspiera open/freeze/thaw i dopisywanie turnów."
    (let [store   (session-store/init-store :ferment.session.store/default {:backend :memory})
          context (session/init-context :ferment.session.context/default {:context/version 1})
          manager (session/init-manager :ferment.session.manager/default
                                        {:store store
                                         :context context
                                         :max-hot-sessions 2
                                         :idle-ttl-ms 1000000})]
      (is (= :hot (:session/state (session/open-session! manager "s1" nil))))
      (is (= 1 (count (:session/turns
                       (session/append-session-turn! manager "s1" {:turn/role :user
                                                                   :turn/text "hej"})))))
      (is (true? (:session/frozen? (session/freeze-session! manager "s1" {:session/summary "sum"}))))
      (is (= :hot (:session/state (session/thaw-session! manager "s1" nil))))
      (is (= "s1" (:session/id (session/get-session manager "s1"))))
      (is (= 1 (count (session/list-sessions manager)))))))

(deftest core-service-initializer-builds-callable-map
  (testing "Core service init zwraca funkcje operujące na runtime z configu."
    (let [called-with (atom nil)
          runtime {:models {:ferment.model/solver {:id "solver-mini"}}}]
      (with-redefs [core/ollama-generate!
                    (fn [m]
                      (reset! called-with m)
                      {:response "{\"intent\":\"ok\"}"})]
        (let [service (core/init-service :ferment.core/default
                                         {:runtime runtime
                                          :resolver {}
                                          :protocol {}})]
          (is (fn? (:respond! service)))
          (is (fn? (:solver! service)))
          (is (fn? (:voice! service)))
          (is (= "{\"intent\":\"ok\"}" ((:solver! service) "napisz patch")))
          (is (= "solver-mini" (:model @called-with))))))))

(deftest dev-config-overlays-prod-config
  (testing "Konfiguracja dev ładuje prod i nadpisuje profil."
    (let [cfg (system/read-configs nil
                                   "config/common/prod"
                                   "config/local/prod"
                                   "config/common/dev"
                                   "config/local/dev")]
      (is (contains? cfg :ferment.core/default))
      (is (contains? cfg :ferment.runtime/default))
      (is (contains? cfg :ferment.model/solver))
      (is (contains? cfg :ferment.model/voice))
      (is (contains? cfg :ferment.resolver/default))
      (is (contains? cfg :ferment.protocol/default))
      (is (contains? cfg :ferment.caps/registry))
      (is (contains? cfg :ferment.logging/unilog))
      (is (= :dev (get-in cfg [:ferment.app/properties :profile]))))))

(deftest test-config-overlays-prod-config
  (testing "Konfiguracja test ładuje prod i nadpisuje profil oraz nazwę bazy."
    (let [cfg (system/read-configs nil
                                   "config/common/prod"
                                   "config/local/prod"
                                   "config/common/test"
                                   "config/local/test")]
      (is (contains? cfg :ferment.core/default))
      (is (= :test (get-in cfg [:ferment.app/properties :profile])))
      (is (= "ferment_test" (:ferment.env/db.main.name cfg))))))

(deftest dev-overlay-order-beats-local-prod
  (testing "Kolejność overlay gwarantuje, że dev nadpisuje prod (także lokalny)."
    (let [prod-only (system/read-configs nil
                                         "config/test-merge/common/prod"
                                         "config/test-merge/local/prod")
          with-common-dev (system/read-configs nil
                                               "config/test-merge/common/prod"
                                               "config/test-merge/local/prod"
                                               "config/test-merge/common/dev")
          with-local-dev (system/read-configs nil
                                              "config/test-merge/common/prod"
                                              "config/test-merge/local/prod"
                                              "config/test-merge/common/dev"
                                              "config/test-merge/local/dev")]
      (is (= :local-prod (get prod-only :ferment.test/overlay)))
      (is (= :common-dev (get with-common-dev :ferment.test/overlay)))
      (is (= :local-dev (get with-local-dev :ferment.test/overlay))))))

(deftest profile-resource-dirs-include-profile-overlay-at-the-end
  (testing "Helper katalogów profilowych trzyma local/<profile> jako ostatnią warstwę."
    (is (= ["translations/ferment"
            "config/common"
            "config/common/env"
            "config/common/prod"
            "config/local"
            "config/local/env"
            "config/local/prod"]
           (system/profile-resource-dirs :prod)))
    (is (= "config/common/test"
           (nth (system/profile-resource-dirs :test) 7)))
    (is (= "config/local/test"
           (peek (system/profile-resource-dirs :test))))))

(deftest model-selector-initialization-picks-profile-specific-value
  (testing "model/init-model-key wybiera model zgodnie z profilem."
    (is (= "solver-mini"
           (model/init-model-key
            :ferment.model.id/solver
            {:profile "mini" :id/default "solver-default" :id/mini "solver-mini"})))
    (is (= "voice-default"
           (model/init-model-key
            :ferment.model.id/voice
            {:profile "default" :id/default "voice-default" :id/mini "voice-mini"})))))

(deftest model-selector-runtime-functions-prefer-model-branch
  (testing "solver-id/voice-id czytają wartości z gałęzi :ferment.model/*."
    (let [runtime {:models {:ferment.model/solver {:id "solver-from-model-branch"}
                            :ferment.model/voice  {:id "voice-from-model-branch"}
                            :ferment.model/coding {:id "coding-from-model-branch"}}}]
      (is (= "solver-from-model-branch" (model/solver-id runtime)))
      (is (= "voice-from-model-branch" (model/voice-id runtime)))
      (is (= "coding-from-model-branch"
             (model/coding-id {:models {:ferment.model/coding {:id "coding-from-model-branch"}}})))
      (is (= "mlx-community/Qwen2.5-7B-Instruct-4bit"
             (model/solver-id {:models {:ferment.model/coding {:id "coding-from-model-branch"}}}))))))

(deftest model-runtime-worker-lifecycle-uses-bot-start-stop
  (testing "Worker runtime używa bot/start i bot/stop przez wrappery model.clj."
    (let [started (atom nil)
          stopped (atom nil)
          session {:sid "test-session"}]
      (with-redefs [model/start-bot-worker!
                    (fn [s cfg]
                      (reset! started {:session s :cfg cfg})
                      {:worker-id :mock-worker})
                    model/stop-bot-worker!
                    (fn [worker]
                      (reset! stopped worker)
                      true)]
        (let [state (model/init-runtime-worker
                     :ferment.model.runtime/solver
                     {:session session
                      :enabled? true
                      :name "solver runtime test"})]
          (is (= session (:session @started)))
          (is (= :ferment.model.runtime/solver
                 (get-in @started [:cfg :id])))
          (is (= :mock-worker (:worker-id (:worker state))))
          (is (true? (:enabled? state)))
          (is (nil? (model/stop-runtime-worker :ferment.model.runtime/solver state)))
          (is (= {:worker-id :mock-worker} @stopped)))))))

(deftest model-runtime-worker-default-invoke-function-uses-runtime-process
  (testing "Worker z :command dostaje domyślny invoke-fn oparty o runtime process i odpowiada przez runtime-request-handler."
    (let [called (atom nil)]
      (with-redefs [model/invoke-runtime-process!
                    (fn [payload session worker-config]
                      (reset! called {:cfg worker-config
                                      :session session
                                      :payload payload})
                      {:text "meta-ok"})]
        (let [cfg (model/preconfigure-runtime-worker
                   :ferment.model.runtime/meta
                   {:command ["mlx_lm.chat" "--model" "meta-model"]})
              response (model/runtime-request-handler
                        {:stage :RUNNING}
                        {:id :ferment.model.runtime/meta}
                        {:body :invoke
                         :args [{:prompt "hej"}]}
                        [cfg])]
          (is (fn? (:invoke-fn cfg)))
          (is (= {:ok? true
                  :result {:text "meta-ok"}}
                 response))
          (is (= {:prompt "hej"} (:payload @called)))
          (is (= :RUNNING (get-in @called [:session :stage]))))))))

(deftest model-runtime-request-quit-writes-command-to-process-stdin
  (testing "Komenda :quit zapisuje :cmd/quit do stdin procesu z kończącym znakiem nowej linii."
    (let [sink (java.io.ByteArrayOutputStream.)
          process (proxy [Process] []
                    (getOutputStream [] sink)
                    (getInputStream [] (java.io.ByteArrayInputStream. (byte-array 0)))
                    (getErrorStream [] (java.io.ByteArrayInputStream. (byte-array 0)))
                    (waitFor [] 0)
                    (exitValue [] 0)
                    (destroy [] nil)
                    (destroyForcibly [] this)
                    (isAlive [] true))
          response (model/runtime-request-handler
                    {:runtime/state {:process process}}
                    {:id :ferment.model.runtime/meta}
                    {:body :quit}
                    {:id :ferment.model.runtime/meta
                     :name "meta model runtime"
                     :cmd/quit "q"})]
      (is (= true (:ok? response)))
      (is (= "q\n" (.toString sink "UTF-8"))))))

(deftest model-runtime-request-runtime-returns-safe-snapshot
  (testing "Komenda :runtime zwraca snapshot operatorski bez surowych/sekretnych pól."
    (let [response (model/runtime-request-handler
                    {:stage :RUNNING
                     :started-at "2026-02-16T15:00:00Z"
                     :runtime/error {:error :runtime-start-failed
                                     :class "java.io.IOException"
                                     :message "boom"
                                     :stacktrace "..."}
                     :runtime/state {:type :process
                                     :pid 1234
                                     :command ["mlx_lm.chat" "--model" "m"]
                                     :secret/token "hidden"}}
                    {:id :ferment.model.runtime/meta}
                    {:body :runtime}
                    {:id :ferment.model.runtime/meta
                     :name "meta model runtime"})]
      (is (= :ferment.model.runtime/meta (:worker/id response)))
      (is (= "meta model runtime" (:worker/name response)))
      (is (= :RUNNING (:stage response)))
      (is (= {:error :runtime-start-failed
              :class "java.io.IOException"
              :message "boom"}
             (:runtime/error response)))
      (is (= {:type :process
              :pid 1234
              :command ["mlx_lm.chat" "--model" "m"]}
             (:runtime/state response)))
      (is (not (contains? (:runtime/state response) :secret/token))))))

(deftest model-runtime-worker-run-sends-bot-responses
  (testing "runtime-worker-run! odsyła odpowiedź z Outcome na kanał bus."
    (let [sent (atom nil)]
      (with-redefs [model/start-command-process! (fn [_ _] nil)
                    bus/wait-for-request (fn [_] {:request :config})
                    bot/handle-request
                    (fn [_ _ _ wrk _ _]
                      {:response {:body {:ok true}
                                  :request {:id "req-1"}}
                       :data :QUIT
                       :wrk wrk})
                    bus/send-response
                    (fn [_wrk response]
                      (reset! sent response)
                      response)]
        (is (nil? (model/runtime-worker-run! {:id :worker}
                                             {:sid "sid"}
                                             {:name "test"})))
        (is (= {:body {:ok true}
                :request {:id "req-1"}}
               @sent))))))

(deftest stop-runtime-worker-sends-quit-before-stop
  (testing "stop-runtime-worker wysyła :quit przed zatrzymaniem workera, gdy :cmd/quit jest ustawione."
    (let [calls (atom [])]
      (with-redefs [model/command-bot-worker!
                    (fn [worker cmd & _]
                      (swap! calls conj [:command worker cmd])
                      :ok)
                    model/stop-bot-worker!
                    (fn [worker]
                      (swap! calls conj [:stop worker])
                      true)]
        (is (nil? (model/stop-runtime-worker
                   :ferment.model.runtime/meta
                   {:worker :mock-worker
                    :config {:cmd/quit "q"}})))
        (is (= [[:command :mock-worker :quit]
                [:stop :mock-worker]]
               @calls))))))

(deftest model-runtime-aggregate-builds-workers-map
  (testing "Agregat runtime zachowuje workers map i helpery odczytu działają."
    (let [runtime (model/init-model-runtime
                   :ferment.model/runtime
                   {:workers {:solver {:worker :solver-w}
                              :voice  {:worker :voice-w}}})]
      (is (= {:worker :solver-w}
             (model/runtime-worker-state runtime :solver)))
      (is (= :voice-w
             (model/runtime-worker runtime :voice))))))

(deftest model-diagnostic-invoke-uses-running-worker-from-system-map
  (testing "diagnostic-invoke! odnajduje worker po model-id i wysyła :invoke z payload."
    (let [called (atom nil)
          system {:ferment.model/meta {:runtime {:worker :meta-worker}}}]
      (with-redefs [model/command-bot-worker!
                    (fn [worker cmd payload]
                      (reset! called {:worker worker :cmd cmd :payload payload})
                      {:ok? true :result {:text "meta-ok"}})]
        (is (= {:ok? true :result {:text "meta-ok"}}
               (model/diagnostic-invoke! system :meta {:prompt "hej"})))
        (is (= {:worker :meta-worker
                :cmd :invoke
                :payload {:prompt "hej"}}
               @called))))))

(deftest model-diagnostic-invoke-fails-when-worker-not-running
  (testing "diagnostic-invoke! zwraca czytelny wyjątek, gdy worker nie jest dostępny."
    (let [ex (try
               (model/diagnostic-invoke! {:ferment.model/meta {:runtime {:enabled? true}}}
                                         :meta
                                         {:prompt "hej"})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo ex))
      (is (= :model/worker-not-found (:error (ex-data ex))))
      (is (= :ferment.model/meta (:model (ex-data ex)))))))

(deftest model-session-runtime-starts-on-demand-and-reuses-worker
  (testing "invoke-model! tworzy worker per (model, session), a kolejne wywołania reuse'ują proces."
    (let [starts (atom [])
          calls  (atom [])
          open-calls (atom [])
          runtime {:models {:ferment.model/meta
                            {:runtime {:config {:id :ferment.model.runtime/meta
                                                :name "meta runtime"
                                                :command ["mlx_lm.chat" "--model" "meta"]}}}}
                   :session {:open! (fn [sid opts]
                                      (swap! open-calls conj {:sid sid :opts opts})
                                      {:session/id sid})}
                   :ferment.model.session/workers (atom {})
                   :ferment.model.session/lock (Object.)
                   :ferment.model.session/enabled? true
                   :ferment.model.session/idle-ttl-ms 60000
                   :ferment.model.session/max-per-model 4}]
      (with-redefs [model/init-runtime-worker
                    (fn [_k cfg]
                      (let [worker (keyword (str "worker-" (inc (count @starts))))
                            state {:id (:id cfg)
                                   :worker worker
                                   :config cfg}]
                        (swap! starts conj state)
                        state))
                    model/command-bot-worker!
                    (fn [worker command payload]
                      (swap! calls conj {:worker worker :command command :payload payload})
                      {:ok? true
                       :result {:text (str "OK-" (name worker))}})]
        (is (= {:ok? true :result {:text "OK-worker-1"}}
               (model/invoke-model! runtime :meta {:prompt "hej"} {:session/id "s-1"})))
        (is (= {:ok? true :result {:text "OK-worker-1"}}
               (model/invoke-model! runtime :meta {:prompt "hej-2"} {:session/id "s-1"})))
        (is (= 1 (count @starts)))
        (is (= 2 (count @calls)))
        (is (= :worker-1 (:worker (first @calls))))
        (is (= :worker-1 (:worker (second @calls))))
        (is (<= 2 (count @open-calls)))
        (is (= "s-1" (:sid (first @open-calls))))
        (is (contains? (get (model/session-workers-state runtime) :ferment.model/meta) "s-1"))))))

(deftest model-session-runtime-expires-workers-by-ttl
  (testing "Worker sesyjny jest zatrzymywany po TTL i startowany ponownie przy kolejnym invoke."
    (let [starts (atom [])
          stops  (atom [])
          runtime {:models {:ferment.model/meta
                            {:runtime {:config {:id :ferment.model.runtime/meta
                                                :name "meta runtime"
                                                :command ["mlx_lm.chat" "--model" "meta"]}}}}
                   :session {:open! (fn [sid _opts] {:session/id sid})}
                   :ferment.model.session/workers (atom {})
                   :ferment.model.session/lock (Object.)
                   :ferment.model.session/enabled? true
                   :ferment.model.session/idle-ttl-ms 1
                   :ferment.model.session/max-per-model 4}]
      (with-redefs [model/init-runtime-worker
                    (fn [_k cfg]
                      (let [worker (keyword (str "worker-" (inc (count @starts))))
                            state {:id (:id cfg)
                                   :worker worker
                                   :config cfg}]
                        (swap! starts conj state)
                        state))
                    model/stop-runtime-worker
                    (fn [_k state]
                      (swap! stops conj (:worker state))
                      nil)
                    model/command-bot-worker!
                    (fn [_worker _command _payload]
                      {:ok? true :result {:text "OK"}})]
        (model/invoke-model! runtime :meta {:prompt "hej"} {:session/id "s-ttl"})
        (swap! (:ferment.model.session/workers runtime)
               assoc-in
               [:ferment.model/meta "s-ttl" :last-used-ms]
               0)
        (model/invoke-model! runtime :meta {:prompt "hej-2"} {:session/id "s-ttl"})
        (is (= 2 (count @starts)))
        (is (= 1 (count @stops)))
        (is (= :worker-1 (first @stops)))))))

(deftest model-session-runtime-freeze-and-thaw
  (testing "thaw-session-worker!/freeze-session-worker! zarządzają workerem i wołają session service."
    (let [starts (atom [])
          stops  (atom [])
          freezes (atom [])
          runtime {:models {:ferment.model/meta
                            {:runtime {:config {:id :ferment.model.runtime/meta
                                                :name "meta runtime"
                                                :command ["mlx_lm.chat" "--model" "meta"]}}}}
                   :session {:open! (fn [sid _opts] {:session/id sid})
                             :freeze! (fn [sid opts]
                                        (swap! freezes conj {:sid sid :opts opts})
                                        {:session/id sid :session/frozen? true})}
                   :ferment.model.session/workers (atom {})
                   :ferment.model.session/lock (Object.)
                   :ferment.model.session/enabled? true
                   :ferment.model.session/idle-ttl-ms 60000
                   :ferment.model.session/max-per-model 4}]
      (with-redefs [model/init-runtime-worker
                    (fn [_k cfg]
                      (let [worker (keyword (str "worker-" (inc (count @starts))))
                            state {:id (:id cfg)
                                   :worker worker
                                   :config cfg}]
                        (swap! starts conj state)
                        state))
                    model/stop-runtime-worker
                    (fn [_k state]
                      (swap! stops conj (:worker state))
                      nil)]
        (is (= {:ok? true
                :model :ferment.model/meta
                :session/id "s-freeze"
                :worker-id :ferment.model.runtime.session/meta--s-freeze}
               (model/thaw-session-worker! runtime :meta "s-freeze")))
        (is (= 1 (count @starts)))
        (is (= {:ok? true
                :model :ferment.model/meta
                :session/id "s-freeze"}
               (model/freeze-session-worker! runtime :meta "s-freeze")))
        (is (= 1 (count @stops)))
        (is (= 1 (count @freezes)))
        (is (= "s-freeze" (:sid (first @freezes))))
        (is (empty? (model/session-workers-state runtime)))))))
