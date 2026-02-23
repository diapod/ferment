(ns

    ^{:doc    "Session store DB backend tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    ferment.session-store-test

  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [io.randomseed.utils.db :as udb]
            [ferment.session.store :as session-store]))

(defn- fake-db-execute-one!
  [rows* _connectable query & _opts]
  (let [sql (first query)]
    (cond
      (str/includes? sql "INSERT INTO")
      (let [[_ sid version state frozen created-at updated-at last-access-at
             frozen-at thawed-at summary snapshot meta turns facts] query]
        (swap! rows* assoc sid {:session_id sid
                                :version version
                                :state state
                                :frozen frozen
                                :created_at created-at
                                :updated_at updated-at
                                :last_access_at last-access-at
                                :frozen_at frozen-at
                                :thawed_at thawed-at
                                :summary summary
                                :snapshot snapshot
                                :meta meta
                                :turns turns
                                :facts facts})
        {:update-count 1})

      (str/includes? sql "WHERE `session_id` = ? LIMIT 1")
      (get @rows* (second query))

      (str/includes? sql "DELETE FROM")
      (do
        (swap! rows* dissoc (second query))
        {:update-count 1})

      :else
      (throw (ex-info "Unexpected SQL in fake execute-one!."
                      {:query query})))))

(defn- fake-db-execute!
  [rows* _connectable query & _opts]
  (let [sql (first query)]
    (if (str/includes? sql "ORDER BY `updated_at` DESC")
      (->> (vals @rows*)
           (sort-by :updated_at)
           reverse
           vec)
      (throw (ex-info "Unexpected SQL in fake execute!."
                      {:query query})))))

(defn- fake-setting-getter
  [vars*]
  (fn [_db sid & ks]
    (let [all (get @vars* sid {})]
      (cond
        (empty? ks) all
        (= 1 (count ks)) (get all (first ks))
        :else (select-keys all ks)))))

(defn- fake-setting-setter
  [vars*]
  (fn [_db sid & kvs]
    (when (seq kvs)
      (swap! vars* update sid (fnil merge {}) (apply hash-map kvs)))
    true))

(defn- fake-setting-deleter
  [vars*]
  (fn [_db sid & ks]
    (if (seq ks)
      (swap! vars* update sid
             (fn [m]
               (apply dissoc (or m {}) ks)))
      (swap! vars* dissoc sid))
    true))

(deftest init-store-db-requires-datasource
  (testing "DB backend fails fast without datasource."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires :db data source"
         (session-store/init-store
          :ferment.session.store/default
          {:backend :db
           :sessions-table :sessions
           :vars-table :session_vars})))))

(deftest db-store-session-crud-roundtrip
  (testing "DB backend supports session CRUD and transactional update/append/freeze/thaw."
    (let [rows* (atom {})
          vars* (atom {})]
      (with-redefs [jdbc/transact (fn [connectable f _opts]
                                    (f connectable))
                    jdbc/execute-one! (partial fake-db-execute-one! rows*)
                    jdbc/execute! (partial fake-db-execute! rows*)
                    udb/make-setting-getter (fn [_table _entity-col]
                                              (fake-setting-getter vars*))
                    udb/make-setting-setter (fn [_table _entity-col]
                                              (fake-setting-setter vars*))
                    udb/make-setting-deleter (fn [_table _entity-col]
                                               (fake-setting-deleter vars*))]
        (let [store (session-store/init-store
                     :ferment.session.store/default
                     {:backend :db
                      :db {:datasource ::fake-ds}
                      :sessions-table :sessions
                      :vars-table :session_vars})]
          (is (= :db (:backend store)))
          (is (= "sessions" (:sessions-table store)))
          (is (= "session_vars" (:vars-table store)))

          (is (= "db-s1" (:session/id (session-store/ensure-session! store "db-s1"))))
          (is (= "db-s1" (:session/id (session-store/get-session store "db-s1"))))

          (session-store/append-turn! store "db-s1" {:turn/role :user
                                                     :turn/text "hej"})
          (let [updated (session-store/get-session store "db-s1")]
            (is (= :hot (:session/state updated)))
            (is (false? (:session/frozen? updated)))
            (is (= 1 (count (:session/turns updated)))))

          (session-store/freeze-session! store "db-s1" {:session/summary {:pl "krótko"}})
          (is (true? (:session/frozen? (session-store/get-session store "db-s1"))))

          (session-store/thaw-session! store "db-s1" nil)
          (is (false? (:session/frozen? (session-store/get-session store "db-s1"))))

          (is (= 1 (count (session-store/list-sessions store))))
          (session-store/delete-session! store "db-s1")
          (is (nil? (session-store/get-session store "db-s1"))))))))

(deftest db-store-session-vars-roundtrip
  (testing "DB backend supports get/put/del for session_vars."
    (let [rows* (atom {})
          vars* (atom {})]
      (with-redefs [jdbc/transact (fn [connectable f _opts]
                                    (f connectable))
                    jdbc/execute-one! (partial fake-db-execute-one! rows*)
                    jdbc/execute! (partial fake-db-execute! rows*)
                    udb/make-setting-getter (fn [_table _entity-col]
                                              (fake-setting-getter vars*))
                    udb/make-setting-setter (fn [_table _entity-col]
                                              (fake-setting-setter vars*))
                    udb/make-setting-deleter (fn [_table _entity-col]
                                               (fake-setting-deleter vars*))]
        (let [store (session-store/init-store
                     :ferment.session.store/default
                     {:backend :db
                      :db {:datasource ::fake-ds}
                      :sessions-table :sessions
                      :vars-table :session_vars})]
          (is (true? (session-store/put-var! store "s-vars" :k1 "v1")))
          (is (true? (session-store/put-vars! store "s-vars" {:k2 2 :k3 3})))
          (is (= "v1" (session-store/get-var store "s-vars" :k1)))
          (is (= {:k1 "v1" :k2 2}
                 (session-store/get-vars store "s-vars" [:k1 :k2 :missing])))

          (is (true? (session-store/del-var! store "s-vars" :k2)))
          (is (= {:k1 "v1" :k3 3}
                 (session-store/get-vars store "s-vars" [:k1 :k2 :k3])))

          (is (true? (session-store/del-vars! store "s-vars" [:k3])))
          (is (= {:k1 "v1"}
                 (session-store/get-vars store "s-vars" [:k1 :k3])))

          (is (true? (session-store/del-all-vars! store "s-vars")))
          (is (= {} (session-store/get-vars store "s-vars" [:k1 :k2]))))))))

(deftest session-vars-contract-enforces-key-namespace
  (testing "Session vars reject keys outside configured namespace contract."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:keys/require-qualified? true
                   :keys/allowed-namespaces #{"session"}
                   :freeze/allow-write? true
                   :freeze/allow-delete? true}})]
      (session-store/ensure-session! store "s-ns")
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"namespace-qualified"
           (session-store/put-var! store "s-ns" :k1 "v1")))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"not allowed"
           (session-store/put-var! store "s-ns" :other/k2 "v2")))
      (is (true? (session-store/put-var! store "s-ns" :session/k3 "v3")))
      (is (= "v3" (session-store/get-var store "s-ns" :session/k3))))))

(deftest session-vars-contract-enforces-freeze-thaw-policy
  (testing "Session vars respect write/delete policy when session is frozen."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:keys/require-qualified? true
                   :keys/allowed-namespaces #{"session"}
                   :freeze/allow-write? false
                   :freeze/allow-delete? false}})]
      (session-store/ensure-session! store "s-freeze")
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"read-only while session is frozen"
           (session-store/put-var! store "s-freeze" :session/a 1)))

      (session-store/thaw-session! store "s-freeze" nil)
      (is (true? (session-store/put-var! store "s-freeze" :session/a 1)))
      (is (= 1 (session-store/get-var store "s-freeze" :session/a)))

      (session-store/freeze-session! store "s-freeze" nil)
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"read-only while session is frozen"
           (session-store/del-var! store "s-freeze" :session/a)))

      (session-store/thaw-session! store "s-freeze" nil)
      (is (true? (session-store/del-var! store "s-freeze" :session/a)))
      (is (nil? (session-store/get-var store "s-freeze" :session/a))))))

(deftest session-vars-contract-enforces-ttl
  (testing "Session vars expire by TTL and are evicted lazily on read."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:keys/require-qualified? true
                   :keys/allowed-namespaces #{"session"}
                   :ttl/default-ms 100
                   :ttl/max-ms 1000
                   :freeze/allow-write? true
                   :freeze/allow-delete? true}})]
      (session-store/ensure-session! store "s-ttl")
      (with-redefs [session-store/now-ms (constantly 1000)]
        (is (true? (session-store/put-var! store "s-ttl" :session/ttl "ok"))))
      (with-redefs [session-store/now-ms (constantly 1050)]
        (is (= "ok" (session-store/get-var store "s-ttl" :session/ttl))))
      (with-redefs [session-store/now-ms (constantly 1201)]
        (is (nil? (session-store/get-var store "s-ttl" :session/ttl))))
      (is (= {} (session-store/get-vars store "s-ttl" [:session/ttl]))))))

(deftest session-vars-contract-enforces-limit
  (testing "Session vars refuse writes above configured max-vars limit."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:keys/require-qualified? true
                   :keys/allowed-namespaces #{"session"}
                   :limits/max-vars 2
                   :freeze/allow-write? true
                   :freeze/allow-delete? true}})]
      (session-store/ensure-session! store "s-limit")
      (is (true? (session-store/put-var! store "s-limit" :session/a 1)))
      (is (true? (session-store/put-var! store "s-limit" :session/b 2)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"count exceeds"
           (session-store/put-var! store "s-limit" :session/c 3)))
      (is (true? (session-store/put-var! store "s-limit" :session/a 11)))
      (is (= {:session/a 11 :session/b 2}
             (session-store/get-vars store "s-limit" [:session/a :session/b]))))))

(deftest session-vars-contract-enforces-per-intent-policy
  (testing "Session vars policy can restrict read/write/delete namespaces per intent."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:keys/require-qualified? true
                   :keys/allowed-namespaces #{"session" "context"}
                   :freeze/allow-write? true
                   :freeze/allow-delete? true
                   :policy/default {:read-namespaces #{"session" "context"}
                                    :write-namespaces #{"session" "context"}
                                    :delete-namespaces #{"session" "context"}}
                   :policy/by-intent {:text/respond {:read-namespaces #{"session"}
                                                     :write-namespaces #{"session"}
                                                     :delete-namespaces #{"session"}}}}})]
      (session-store/ensure-session! store "s-policy")

      (is (true? (session-store/put-var! store
                                         "s-policy"
                                         :context/summary
                                         "ctx"
                                         {:intent :route/decide})))

      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"forbidden by policy"
           (session-store/put-var! store
                                   "s-policy"
                                   :context/summary
                                   "ctx2"
                                   {:intent :text/respond})))

      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"forbidden by policy"
           (session-store/get-var store
                                  "s-policy"
                                  :context/summary
                                  {:intent :text/respond})))

      (is (= "ctx"
             (session-store/get-var store
                                    "s-policy"
                                    :context/summary
                                    {:intent :route/decide})))

      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"forbidden by policy"
           (session-store/del-var! store
                                   "s-policy"
                                   :context/summary
                                   {:intent :text/respond})))

      (is (true? (session-store/del-var! store
                                         "s-policy"
                                         :context/summary
                                         {:intent :route/decide})))
      (is (nil? (session-store/get-var store
                                       "s-policy"
                                       :context/summary
                                       {:intent :route/decide}))))))

(deftest session-vars-contract-uses-class-based-ttl
  (testing "Session vars TTL can be selected by namespace class."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:keys/require-qualified? true
                   :keys/allowed-namespaces #{"request" "context"}
                   :ttl/default-ms 1000
                   :ttl/max-ms 5000
                   :freeze/allow-write? true
                   :freeze/allow-delete? true
                   :class/default :session.vars/default
                   :class/by-namespace {"request" :session.vars/request-data}
                   :class/policy {:session.vars/request-data {:ttl/default-ms 100
                                                              :ttl/max-ms 250}}}})]
      (session-store/ensure-session! store "s-class-ttl")
      (with-redefs [session-store/now-ms (constantly 1000)]
        (is (true? (session-store/put-var! store "s-class-ttl" :request/topic "acid")))
        (is (true? (session-store/put-var! store "s-class-ttl" :context/summary "ctx"))))
      (with-redefs [session-store/now-ms (constantly 1150)]
        (is (nil? (session-store/get-var store "s-class-ttl" :request/topic)))
        (is (= "ctx" (session-store/get-var store "s-class-ttl" :context/summary))))
      (with-redefs [session-store/now-ms (constantly 2101)]
        (is (nil? (session-store/get-var store "s-class-ttl" :context/summary)))))))

(deftest session-vars-contract-uses-class-based-freeze-policy
  (testing "Session vars freeze write/delete can be overridden per namespace class."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:keys/require-qualified? true
                   :keys/allowed-namespaces #{"runtime" "session"}
                   :freeze/allow-write? false
                   :freeze/allow-delete? false
                   :class/default :session.vars/default
                   :class/by-namespace {"runtime" :session.vars/runtime-data}
                   :class/policy {:session.vars/runtime-data {:freeze/allow-write? true
                                                              :freeze/allow-delete? true}}}})]
      (session-store/ensure-session! store "s-class-freeze")
      (is (true? (session-store/put-var! store "s-class-freeze" :runtime/diag "ok")))
      (is (= "ok" (session-store/get-var store "s-class-freeze" :runtime/diag)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"read-only while session is frozen"
           (session-store/put-var! store "s-class-freeze" :session/note "x")))
      (is (true? (session-store/del-var! store "s-class-freeze" :runtime/diag)))
      (is (nil? (session-store/get-var store "s-class-freeze" :runtime/diag))))))

(deftest session-vars-contract-exposes-request-default-bindings
  (testing "Session vars contract normalizes request default bindings."
    (let [store (session-store/init-store
                 :ferment.session.store/default
                 {:backend :memory
                  :session-vars/contract
                  {:request/default-bindings
                   {"session/language" {:target ["constraints" "language"]
                                        :coerce "keyword-or-string"}
                    :session/context-summary {:target [:context :summary]
                                              :coerce :trimmed-string}}}})
          bindings (session-store/request-default-bindings store)]
      (is (= {:target [:constraints :language]
              :coerce :keyword-or-string}
             (get bindings :session/language)))
      (is (= {:target [:context :summary]
              :coerce :trimmed-string}
             (get bindings :session/context-summary))))))
