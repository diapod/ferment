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
