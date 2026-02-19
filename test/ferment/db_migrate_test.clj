(ns ferment.db-migrate-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.app :as app]
            [ferment.db :as db]
            [ragtime.repl :as ragtime-repl]))

(defn- migrator-fn
  []
  {:dbkey :ferment.db/main.migrator
   :datastore {:datasource {}}})

(deftest migrate-uses-fresh-app-state-after-start-admin
  (testing "migrate! reads migrators from refreshed app/state when it had to start admin."
    (let [mig-key ::db/migrators
          started (atom [])
          migrated (atom [])]
      (with-redefs [app/state {}
                    app/post-config {}
                    app/start-admin! (fn [k]
                                       (swap! started conj k)
                                       (alter-var-root #'app/state
                                                       (constantly {mig-key [migrator-fn]}))
                                       :running)
                    app/stop! (fn [& _] :stopped)
                    db/try-initialize-db (fn [_] [{}])
                    db/db-name (fn [_] "ferment-test")
                    db/db-key-name (fn [_] :ferment.db/main.migrator)
                    ragtime-repl/migrate (fn [cfg] (swap! migrated conj cfg) nil)]
        (db/migrate! {:migrators-key mig-key})
        (is (= [mig-key] @started))
        (is (= 1 (count @migrated)))))))

(deftest rollback-uses-fresh-app-state-after-start-admin
  (testing "rollback! reads migrators from refreshed app/state when it had to start admin."
    (let [mig-key ::db/migrators
          started (atom [])
          rolled-back (atom [])]
      (with-redefs [app/state {}
                    app/post-config {}
                    app/start-admin! (fn [k]
                                       (swap! started conj k)
                                       (alter-var-root #'app/state
                                                       (constantly {mig-key [migrator-fn]}))
                                       :running)
                    app/stop! (fn [& _] :stopped)
                    ragtime-repl/rollback (fn
                                            ([cfg] (swap! rolled-back conj cfg) nil)
                                            ([cfg _amount-or-id] (swap! rolled-back conj cfg) nil))]
        (db/rollback! {:migrators-key mig-key})
        (is (= [mig-key] @started))
        (is (= 1 (count @rolled-back)))))))
