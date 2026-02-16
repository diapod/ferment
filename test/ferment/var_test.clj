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
  (testing "Runtime branch ma refs do resolvera/protokołu oraz agregat modeli."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/runtime.edn")
          runtime (get cfg :ferment.runtime/default)]
      (is (map? runtime))
      (is (= :ferment.resolver/default (:resolver runtime)))
      (is (= :ferment.protocol/default (:protocol runtime)))
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
      (is (= "SpeakLeash/bielik-1.5b-instruct"
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
  (testing "Core branch ma refs do runtime/resolver/protocol."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/core.edn")
          corecfg (get cfg :ferment.core/default)]
      (is (map? corecfg))
      (is (= :ferment.runtime/default (:runtime corecfg)))
      (is (= :ferment.resolver/default (:resolver corecfg)))
      (is (= :ferment.protocol/default (:protocol corecfg))))))

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

(deftest model-runtime-worker-default-invoke-function-uses-command
  (testing "Worker z :command dostaje domyślny invoke-fn i odpowiada przez runtime-request-handler."
    (let [called (atom nil)]
      (with-redefs [model/invoke-command!
                    (fn [worker-config payload]
                      (reset! called {:cfg worker-config :payload payload})
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
          (is (= {:prompt "hej"} (:payload @called))))))))

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
