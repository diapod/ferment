(ns

    ^{:doc    "Routing helper tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.router-test

  (:require [clojure.test :refer [deftest is testing]]
            [integrant.core :as ig]
            [ferment.router :as router]))

(deftest resolver-config-normalization
  (testing "resolver config is taken from explicit arg or runtime branch."
    (let [resolver {:routing {:intent->cap {:problem/solve :llm/solver}}}
          runtime  {:config {:resolver resolver
                             :router {:routing {:intent->cap {:problem/solve :llm/meta}}}}}]
      (is (= resolver (router/resolver-config runtime nil)))
      (is (= resolver (router/resolver-config runtime resolver)))
      (is (= resolver (router/resolver-config {:runtime runtime} nil)))
      (is (= {:routing {:intent->cap {:problem/solve :llm/meta}}}
             (router/router-config runtime)))
      (is (= {:intent->cap {:problem/solve :llm/meta}}
             (router/resolver-routing runtime nil))))))

(deftest resolver-capability-lookup
  (testing "capability metadata is resolved from index or fallback caps vector."
    (let [cap-a {:cap/id :llm/solver :dispatch/role :solver}
          cap-b {:cap/id :llm/voice :dispatch/role :voice}
          resolver-a {:caps/by-id {:llm/solver cap-a}}
          resolver-b {:caps [cap-a cap-b]}]
      (is (= cap-a (router/resolver-capability nil resolver-a :llm/solver)))
      (is (= cap-b (router/resolver-capability nil resolver-b :llm/voice)))
      (is (nil? (router/resolver-capability nil resolver-b :llm/meta))))))

(deftest router-config-validation-rejects-missing-required-routing
  (testing "Router branch fails fast when :routing is missing."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing required :routing"
         (router/init-router :ferment.router/default {})))))

(deftest router-config-validation-rejects-invalid-routing-shape
  (testing "Router branch fails fast on invalid routing mappings and unsupported keys."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"unsupported keys"
         (router/init-router
          :ferment.router/default
          {:routing {:intent->cap {:problem/solve :llm/solver}
                     :bad/key true}})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"mapping values must be keywords"
         (router/init-router
          :ferment.router/default
          {:routing {:intent->cap {:problem/solve "llm/solver"}}})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be a set or sequence of keywords"
         (router/init-router
          :ferment.router/default
          {:routing {:intent->cap {:problem/solve :llm/solver}
                     :switch-on :eval/low-score}})))))

(deftest router-config-validation-validates-defaults
  (testing "Router branch validates :defaults for meta/strict/force/on-error."
    (let [cfg (router/init-router
               :ferment.router/default
               {:routing {:intent->cap {:problem/solve :llm/solver}}
                :defaults {:meta? true
                           :strict? false
                           :force? false
                           :on-error :fail-open}})]
      (is (= {:meta? true
              :strict? false
              :force? false
              :on-error :fail-open}
             (:defaults cfg))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"flags must be booleans"
         (router/init-router
          :ferment.router/default
          {:routing {:intent->cap {:problem/solve :llm/solver}}
           :defaults {:strict? :yes}})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":on-error must be :fail-open or :fail-closed"
         (router/init-router
          :ferment.router/default
          {:routing {:intent->cap {:problem/solve :llm/solver}}
           :defaults {:on-error :panic}})))))

(deftest router-config-validation-allows-integrant-refs
  (testing "Router branch accepts unresolved Integrant refs for :routing and :profiles."
    (let [cfg (router/init-router
               :ferment.router/default
               {:routing  (ig/ref :ferment.caps/routing)
                :profiles (ig/ref :ferment.caps/profiles)
                :policy   :quality-aware})]
      (is (ig/ref? (:routing cfg)))
      (is (ig/ref? (:profiles cfg)))
      (is (= :quality-aware (:policy cfg))))))

(deftest routing-defaults-from-runtime
  (testing "Routing defaults are read from runtime router branch."
    (is (= {:meta? true
            :strict? true
            :force? false
            :on-error :fail-closed}
           (router/routing-defaults
            {:router {:defaults {:meta? true
                                 :strict? true
                                 :force? false
                                 :on-error :fail-closed}}})))
    (is (= {} (router/routing-defaults {:router {}})))
    (is (= {} (router/routing-defaults nil)))))

(deftest resolve-model-key-precedence
  (testing "model key resolution uses capability dispatch, then router branch routing, then defaults."
    (let [resolver {:caps/by-id {:llm/solver {:cap/id :llm/solver
                                              :dispatch/model-key :ferment.model/meta}}
                    :routing {:cap->model-key {:llm/voice :ferment.model/voice}
                              :intent->model-key {:problem/solve :ferment.model/coding}}}]
      (is (= :ferment.model/meta
             (router/resolve-model-key nil resolver :llm/solver :problem/solve)))
      (is (= :ferment.model/solver
             (router/resolve-model-key nil resolver :llm/voice :problem/solve)))
      (is (= :ferment.model/solver
             (router/resolve-model-key nil resolver :llm/unknown :problem/solve)))
      (is (= :ferment.model/solver
             (router/resolve-model-key nil {} :llm/unknown :problem/solve)))
      (is (= :ferment.model/voice
             (router/resolve-model-key nil {} :llm/unknown :text/respond)))
      (is (= :ferment.model/voice
             (router/resolve-model-key
              {:router {:routing {:cap->model-key {:llm/voice :ferment.model/voice}}}}
              resolver
              :llm/voice
              :problem/solve)))
      (is (= :ferment.model/coding
             (router/resolve-model-key
              {:router {:routing {:intent->model-key {:problem/solve :ferment.model/coding}}}}
              resolver
              :llm/unknown
              :problem/solve)))
      (is (= :ferment.model/meta
             (router/resolve-model-key
              {:router {:routing {:intent->model-key {:problem/solve :ferment.model/meta}}}}
              resolver
              :llm/unknown
              :problem/solve))))))

(deftest resolve-role-precedence
  (testing "role resolution uses capability dispatch, then router branch routing, then defaults."
    (let [resolver {:caps/by-id {:llm/solver {:cap/id :llm/solver
                                              :dispatch/role :router}}
                    :routing {:cap->role {:llm/voice :voice}
                              :intent->role {:problem/solve :coder}}}]
      (is (= :router
             (router/resolve-role nil resolver :llm/solver :problem/solve)))
      (is (= :solver
             (router/resolve-role nil resolver :llm/voice :problem/solve)))
      (is (= :solver
             (router/resolve-role nil resolver :llm/unknown :problem/solve)))
      (is (= :solver
             (router/resolve-role nil {} :llm/unknown :problem/solve)))
      (is (= :voice
             (router/resolve-role nil {} :llm/unknown :text/respond)))
      (is (= :voice
             (router/resolve-role
              {:router {:routing {:cap->role {:llm/voice :voice}}}}
              resolver
              :llm/voice
              :problem/solve)))
      (is (= :coder
             (router/resolve-role
              {:router {:routing {:intent->role {:problem/solve :coder}}}}
              resolver
              :llm/unknown
              :problem/solve)))
      (is (= :router
             (router/resolve-role
              {:router {:routing {:intent->role {:problem/solve :router}}}}
              resolver
              :llm/unknown
              :problem/solve))))))
