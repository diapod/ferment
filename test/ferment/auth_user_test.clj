(ns ferment.auth-user-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ferment.auth :as auth]
            [ferment.auth.user :as auth-user]
            [ferment.session :as session]
            [ferment.session.store :as session-store]
            [ferment.test-support.fake-auth-db :as fake-db]
            [ferment.user :as user]
            [next.jdbc :as jdbc]
            [tick.core :as t]))

(deftest get-login-data-builds-query
  (testing "get-login-data builds query with account-type filter and maps row"
    (let [seen (atom nil)]
      (with-redefs [auth/db (fn
                              ([_] :db)
                              ([_ _] :db))
                    jdbc/execute-one! (fn [db-src params _opts]
                                        (reset! seen {:db db-src :params params})
                                        {:id 7
                                         :email "user@example.com"
                                         :account_type "system"
                                         :login_attempts 2
                                         :roles "role/admin,role/reviewer"
                                         :shared "{\"x\":1}"
                                         :intrinsic "{\"y\":2}"})]
        (is (= {:user/id 7
                :user/email "user@example.com"
                :user/account-type :system
                :user/roles #{:role/admin :role/reviewer}
                :user/login-attempts 2
                :auth/locked-at nil
                :auth/soft-locked-at nil
                :auth/locked? false
                :auth/password-intrinsic "{\"y\":2}"
                :auth/password-shared "{\"x\":1}"}
               (auth-user/get-login-data :auth "user@example.com" :system)))
        (is (= :db (:db @seen)))
        (is (= "user@example.com" (second (:params @seen))))
        (is (= "system" (nth (:params @seen) 2)))
        (is (str/includes? (first (:params @seen))
                           "COALESCE((SELECT GROUP_CONCAT(user_roles.role"))
        (is (str/includes? (first (:params @seen))
                           "WHERE users.email = ? AND users.account_type = ?"))))))

(deftest authenticate-password-success
  (testing "authenticate-password returns user without password fields"
    (with-redefs [auth/config (fn
                                ([_] :cfg)
                                ([_ _] :cfg))
                  auth-user/get-login-data (fn [_ _ _]
                                             {:user/id 1
                                              :user/email "a@b.c"
                                              :user/account-type :system
                                              :auth/locked? false
                                              :auth/password-shared "{}"
                                              :auth/password-intrinsic "{}"})
                  auth/check-password-json (fn [_ _ _ _] true)]
      (is (= {:ok? true
              :user {:user/id 1
                     :user/email "a@b.c"
                     :user/account-type :system}}
             (auth-user/authenticate-password :auth "a@b.c" "secret" :system))))))

(deftest authenticate-password-opens-session-when-configured
  (testing "Successful auth may open session via :session/service and include public session fragment."
    (with-redefs [auth/config (fn
                                ([_] :cfg)
                                ([_ _] :cfg))
                  auth-user/get-login-data (fn [_ _ _]
                                             {:user/id 11
                                              :user/email "s@f.io"
                                              :user/account-type :operator
                                              :user/roles #{:role/operator :role/reviewer}
                                              :auth/locked? false
                                              :auth/password-shared "{}"
                                              :auth/password-intrinsic "{}"})
                  auth/check-password-json (fn [_ _ _ _] true)]
      (let [seen-open (atom nil)
            service {:open! (fn [sid opts]
                              (reset! seen-open {:sid sid :opts opts})
                              {:session/id sid
                               :session/version 7
                               :session/state :hot
                               :session/frozen? false
                               :session/updated-at "2026-02-19T00:00:00Z"
                               :session/turns []})}
            result (auth-user/authenticate-password
                    :auth "s@f.io" "secret" :operator
                    {:session/service service
                     :session/id "auth-s1"
                     :session/meta {:origin :test}})]
        (is (= true (:ok? result)))
        (is (= {:user/id 11
                :user/email "s@f.io"
                :user/account-type :operator
                :user/roles #{:role/operator :role/reviewer}}
               (:user result)))
        (is (= {:session/id "auth-s1"
                :session/version 7
                :session/state :hot
                :session/frozen? false
                :session/updated-at "2026-02-19T00:00:00Z"}
               (:session result)))
        (is (= "auth-s1" (:sid @seen-open)))
        (is (= :auth/authenticate-password
               (get-in @seen-open [:opts :session/meta :source])))
        (is (= 11
               (get-in @seen-open [:opts :session/meta :auth/principal :user/id])))
        (is (integer? (get-in @seen-open [:opts :session/meta :auth/principal-at])))
        (is (integer? (get-in @seen-open [:opts :session/meta :auth/principal-refreshed-at])))
        (is (= 11 (get-in @seen-open [:opts :session/meta :user/id])))
        (is (= [:role/operator :role/reviewer]
               (get-in @seen-open [:opts :session/meta :user/roles])))
        (is (= :test (get-in @seen-open [:opts :session/meta :origin])))))))

(deftest authenticate-password-updates-login-counters
  (testing "Successful auth resets counters, failed auth increments counters."
    (let [ok-calls   (atom [])
          fail-calls (atom [])]
      (with-redefs [auth/config (fn
                                  ([_] :cfg)
                                  ([_ _] :cfg))
                    auth/db (fn
                              ([_] :db)
                              ([_ _] :db))
                    auth-user/get-login-data (fn [_ _ _]
                                               {:user/id 9
                                                :user/email "a@b.c"
                                                :user/account-type :system
                                                :auth/locked? false
                                                :auth/password-shared "{}"
                                                :auth/password-intrinsic "{}"})
                    auth/check-password-json (fn [_ _ _ _] true)
                    user/update-login-ok! (fn [db-src user-id]
                                            (swap! ok-calls conj [db-src user-id])
                                            true)]
        (is (= {:ok? true
                :user {:user/id 9
                       :user/email "a@b.c"
                       :user/account-type :system}}
               (auth-user/authenticate-password :auth "a@b.c" "secret" :system)))
        (is (= [[:db 9]] @ok-calls)))
      (with-redefs [auth/config (fn
                                  ([_] :cfg)
                                  ([_ _] :cfg))
                    auth/db (fn
                              ([_] :db)
                              ([_ _] :db))
                    auth-user/get-login-data (fn [_ _ _]
                                               {:user/id 9
                                                :user/email "a@b.c"
                                                :user/account-type :system
                                                :auth/locked? false
                                                :auth/password-shared "{}"
                                                :auth/password-intrinsic "{}"})
                    auth/check-password-json (fn [_ _ _ _] false)
                    user/update-login-failed! (fn [db-src user-id max-attempts]
                                                (swap! fail-calls conj [db-src user-id max-attempts])
                                                true)]
        (is (= {:ok? false :error :auth/invalid-credentials}
               (auth-user/authenticate-password :auth "a@b.c" "bad" :system)))
        (is (= [[:db 9 3]] @fail-calls))))))

(deftest authenticate-password-failures
  (testing "returns error for invalid input data"
    (with-redefs [auth/config (fn
                                ([_] :cfg)
                                ([_ _] :cfg))]
      (is (= {:ok? false :error :input/invalid}
             (auth-user/authenticate-password :auth nil "secret")))))
  (testing "returns error when auth is not configured"
    (with-redefs [auth/config (fn
                                ([_] nil)
                                ([_ _] nil))]
      (is (= {:ok? false :error :auth/not-configured}
             (auth-user/authenticate-password :auth "a@b.c" "secret")))))
  (testing "returns error when account is locked"
    (with-redefs [auth/config (fn
                                ([_] :cfg)
                                ([_ _] :cfg))
                  auth-user/get-login-data (fn [_ _ _]
                                             {:user/id 1
                                              :user/email "a@b.c"
                                              :auth/locked? true
                                              :auth/password-shared "{}"
                                              :auth/password-intrinsic "{}"})]
      (is (= {:ok? false
              :error :auth/account-locked
              :auth/lock-kind :unknown}
             (auth-user/authenticate-password :auth "a@b.c" "secret")))))
  (testing "returns error when password does not match"
    (with-redefs [auth/config (fn
                                ([_] :cfg)
                                ([_ _] :cfg))
                  auth-user/get-login-data (fn [_ _ _]
                                             {:user/id 1
                                              :user/email "a@b.c"
                                              :auth/locked? false
                                              :auth/password-shared "{}"
                                              :auth/password-intrinsic "{}"})
                  auth/check-password-json (fn [_ _ _ _] false)]
      (is (= {:ok? false :error :auth/invalid-credentials}
             (auth-user/authenticate-password :auth "a@b.c" "secret"))))))

(deftest create-user-login-success-opens-session-e2e
  (testing "User can be created, authenticated, and attached to real session service."
    (let [fake-db (fake-db/make-db)
          store   (session-store/init-store :ferment.session.store/default {:backend :memory})
          context (session/init-context :ferment.session.context/default {:context/version 1})
          manager (session/init-manager :ferment.session.manager/default
                                        {:store store :context context})
          service (session/init-service :ferment.session/default
                                        {:store store :context context :manager manager})
          auth-cfg {:id :ferment.auth/simple
                    :account-types {:default-name "user"}
                    :locking {:max-attempts 3
                              :lock-wait (t/new-duration 10 :minutes)}}]
      (with-redefs [jdbc/transact (fn [connectable f _opts] (f connectable))
                    jdbc/execute! (partial fake-db/execute! fake-db)
                    jdbc/execute-one! (partial fake-db/execute-one! fake-db)
                    auth/config (fn
                                  ([_] auth-cfg)
                                  ([_ _] auth-cfg))
                    auth/db (fn
                              ([_] ::db)
                              ([_ _] ::db))
                    auth/make-password-json (fn [plain _]
                                              {:shared "{\"algo\":\"test\"}"
                                               :intrinsic plain})
                    auth/check-password-json (fn [plain _shared intrinsic _]
                                              (= plain intrinsic))]
        (let [created (user/create-user! ::auth "Tester@Example.com" "sekret" :operator)
              login   (auth-user/authenticate-password
                       ::auth
                       "tester@example.com"
                       "sekret"
                       :operator
                       {:session/service service
                        :session/id "auth-e2e-s1"})]
          (is (:ok? created))
          (is (:created? created))
          (is (= "tester@example.com" (get-in created [:user :user/email])))
          (is (= :operator (get-in created [:user :user/account-type])))
          (is (= true (:ok? login)))
          (is (= "auth-e2e-s1" (get-in login [:session :session/id])))
          (is (integer? (get-in login [:session :session/version])))
          (is (= :operator (get-in login [:user :user/account-type])))
          (is (= "auth-e2e-s1" (:session/id (session/get! service "auth-e2e-s1"))))
          (is (contains? #{:hot :warm} (:session/state (session/get! service "auth-e2e-s1")))))))))

(deftest repeated-login-failures-trigger-soft-lock
  (testing "Failed login increments attempts and then blocks next login with soft lock."
    (let [fake-db (fake-db/make-db)
          auth-cfg {:id :ferment.auth/simple
                    :account-types {:default-name "user"}
                    :locking {:max-attempts 1
                              :lock-wait (t/new-duration 10 :minutes)}}]
      (with-redefs [jdbc/transact (fn [connectable f _opts] (f connectable))
                    jdbc/execute! (partial fake-db/execute! fake-db)
                    jdbc/execute-one! (partial fake-db/execute-one! fake-db)
                    auth/config (fn
                                  ([_] auth-cfg)
                                  ([_ _] auth-cfg))
                    auth/db (fn
                              ([_] ::db)
                              ([_ _] ::db))
                    auth/make-password-json (fn [plain _]
                                              {:shared "{\"algo\":\"test\"}"
                                               :intrinsic plain})
                    auth/check-password-json (fn [plain _shared intrinsic _]
                                              (= plain intrinsic))]
        (is (:ok? (user/create-user! ::auth "lock@test.io" "good-pass" :user)))
        (let [fail-1 (auth-user/authenticate-password ::auth "lock@test.io" "bad-pass" :user)
              fail-2 (auth-user/authenticate-password ::auth "lock@test.io" "bad-pass" :user)]
          (is (= {:ok? false :error :auth/invalid-credentials} fail-1))
          (is (= false (:ok? fail-2)))
          (is (= :auth/account-locked (:error fail-2)))
          (is (= :soft (:auth/lock-kind fail-2)))
          (is (some? (:auth/lock-remaining fail-2))))))))
