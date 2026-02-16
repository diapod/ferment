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
            [ferment.model                   :as           model]
            [ferment.system                  :as          system]
            [ferment.resolver                :as        resolver]
            [ferment                       :refer         :all]
            [expound.alpha                   :as         expound]))

(s/check-asserts true)

(defn- read-edn-with-integrant-readers
  [path]
  (edn/read-string {:readers {'ref identity 'refset identity}}
                   (slurp path)))

(deftest capabilities-config-is-flattened
  (testing "Każde capability jest osobnym kluczem i agregat nadal istnieje."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/capabilities.edn")
          cap-keys #{:ferment.caps.registry/llm-voice
                     :ferment.caps.registry/llm-code
                     :ferment.caps.registry/llm-meta
                     :ferment.caps.registry/llm-mock}
          refs (get cfg :ferment.caps/registry)]
      (is (every? #(contains? cfg %) cap-keys))
      (is (= 4 (count refs)))
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
               :ferment.caps.registry/llm-meta
               :ferment.caps.registry/llm-mock}
             (set (:caps default))))
      (is (= 2 (count (:caps/by-id initialized))))
      (is (= {:cap/id :llm/code :x 2}
             (get-in initialized [:caps/by-id :llm/code]))))))

(deftest caps-entry-hooks-are-pass-through-by-default
  (testing "Domyślne hooki caps entry nie modyfikują wartości."
    (let [entry {:cap/id :llm/meta :dispatch/tag :meta}]
      (is (= entry
             (caps/preconfigure-capability-value
              :ferment.caps.registry/llm-meta
              entry)))
      (is (= entry
             (caps/init-capability-value
              :ferment.caps.registry/llm-meta
              entry))))))

(deftest runtime-config-contains-core-runtime-branch
  (testing "Runtime branch ma refs do resolvera/protokołu i mapę core bez zależności od modelowych kluczy env."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/runtime.edn")
          runtime (get cfg :ferment.runtime/default)
          envcfg (:config runtime)]
      (is (map? runtime))
      (is (= :ferment.resolver/default (:resolver runtime)))
      (is (= :ferment.protocol/default (:protocol runtime)))
      (is (= :ferment.model/solver
             (get envcfg :ferment.model/solver)))
      (is (= :ferment.model/voice
             (get envcfg :ferment.model/voice)))
      (is (= :ferment.model/runtime
             (:models runtime)))
      (is (nil? (get envcfg :ferment.env/ferment.model.solver)))
      (is (nil? (get envcfg :ferment.env/ferment.model.voice))))))

(deftest models-config-defines-selector-values-in-edn
  (testing "Selektory modeli są utrzymywane w models.edn."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/models.edn")]
      (is (= "default" (get cfg :ferment.model/profile)))
      (is (= "qwen2.5-coder:7b"
             (get-in cfg [:ferment.model/solver :default])))
      (is (= "qwen2.5-coder:1.5b"
             (get-in cfg [:ferment.model/solver :mini])))
      (is (= "SpeakLeash/bielik-1.5b-instruct"
             (get-in cfg [:ferment.model/voice :default])))
      (is (= {"HF_HOME" :ferment.env/hf.home
              "HF_HUB_CACHE" :ferment.env/hf.hub.cache}
             (get cfg :ferment.model.solver/env)))
      (is (= {"HF_HOME" :ferment.env/hf.home
              "HF_HUB_CACHE" :ferment.env/hf.hub.cache}
             (get cfg :ferment.model.voice/env))))))

(deftest model-runtime-config-wires-session-and-workers
  (testing "Gałąź runtime modeli ma session key, worker keys i agregat workers."
    (let [cfg (read-edn-with-integrant-readers "resources/config/common/prod/model-runtime.edn")
          session (get cfg :ferment.model/bot-session)
          solver  (get cfg :ferment.model.solver/runtime)
          voice   (get cfg :ferment.model.voice/runtime)
          runtime (get cfg :ferment.model/runtime)]
      (is (map? session))
      (is (= "ferment-model-runtime" (:sid session)))
      (is (= :ferment.model/bot-session (:session solver)))
      (is (= :ferment.model/bot-session (:session voice)))
      (is (= :ferment.model.solver/env (:env solver)))
      (is (= :ferment.model.voice/env (:env voice)))
      (is (= :ferment.model.solver/runtime
             (get-in runtime [:workers :solver])))
      (is (= :ferment.model.voice/runtime
             (get-in runtime [:workers :voice]))))))

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
          runtime {:config {:ferment.model/solver "solver-mini"}}]
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
            :ferment.model/solver
            {:profile "mini" :default "solver-default" :mini "solver-mini"})))
    (is (= "voice-default"
           (model/init-model-key
            :ferment.model/voice
            {:profile "default" :default "voice-default" :mini "voice-mini"})))))

(deftest model-selector-runtime-functions-prefer-model-branch
  (testing "solver-id/voice-id czytają wartości z gałęzi :ferment.model/*."
    (let [runtime {:config {:ferment.model/solver "solver-from-model-branch"
                            :ferment.model/voice  "voice-from-model-branch"
                            :ferment.model/coding "coding-from-model-branch"}}]
      (is (= "solver-from-model-branch" (model/solver-id runtime)))
      (is (= "voice-from-model-branch" (model/voice-id runtime)))
      (is (= "coding-from-model-branch"
             (model/solver-id {:config {:ferment.model/coding "coding-from-model-branch"}}))))))

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
                     :ferment.model.solver/runtime
                     {:session session
                      :enabled? true
                      :name "solver runtime test"})]
          (is (= session (:session @started)))
          (is (= :ferment.model.solver/runtime
                 (get-in @started [:cfg :id])))
          (is (= :mock-worker (:worker-id (:worker state))))
          (is (true? (:enabled? state)))
          (is (nil? (model/stop-runtime-worker :ferment.model.solver/runtime state)))
          (is (= {:worker-id :mock-worker} @stopped)))))))

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
