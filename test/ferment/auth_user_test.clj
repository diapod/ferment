(ns ferment.auth-user-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.auth :as auth]
            [ferment.auth.user :as auth-user]
            [next.jdbc :as jdbc]))

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
                                         :shared "{\"x\":1}"
                                         :intrinsic "{\"y\":2}"})]
        (is (= {:user/id 7
                :user/email "user@example.com"
                :user/account-type :system
                :auth/locked? false
                :auth/password-intrinsic "{\"y\":2}"
                :auth/password-shared "{\"x\":1}"}
               (auth-user/get-login-data :auth "user@example.com" :system)))
        (is (= :db (:db @seen)))
        (is (= ["SELECT users.id AS id, users.email AS email, users.account_type AS account_type, users.locked AS locked, users.soft_locked AS soft_locked, users.password AS intrinsic, password_suites.suite AS shared FROM users JOIN password_suites ON password_suites.id = users.password_suite_id WHERE users.email = ? AND users.account_type = ?"
                "user@example.com"
                "system"]
               (:params @seen)))))))

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
      (is (= {:ok? false :error :auth/account-locked}
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
