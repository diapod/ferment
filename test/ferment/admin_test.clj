(ns ferment.admin-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.admin :as admin]
            [ferment.app :as app]
            [ferment.oplog :as oplog]
            [ferment.user :as user]))

(deftest create-user-delegates-auth-source-and-logs-success
  (testing "Admin create-user forwards auth source and writes success event to auth oplog."
    (let [called (atom nil)
          logs   (atom [])]
      (with-redefs [app/state {:ferment.auth/setup :auth-source}
                    oplog/auth-logger (fn [_cfg]
                                        (fn [& {:as message}]
                                          (swap! logs conj message)))
                    user/create-user! (fn [auth email password account-type]
                                        (reset! called [auth email password account-type])
                                        {:ok? true
                                         :user {:user/id 42
                                                :user/email email
                                                :user/account-type account-type}})]
        (let [result (admin/create-user! "user@example.com" "secret" :operator)
              log-msg (first @logs)]
          (is (= [:auth-source "user@example.com" "secret" :operator] @called))
          (is (= true (:ok? result)))
          (is (= "admin/create-user" (:operation log-msg)))
          (is (= true (:success log-msg)))
          (is (= :info (:level log-msg)))
          (is (= 42 (:user-id log-msg))))))))

(deftest delete-user-logs-failure
  (testing "Admin delete-user writes failed outcome to auth oplog."
    (let [logs (atom [])]
      (with-redefs [app/state {:ferment.auth/setup :auth-source}
                    oplog/auth-logger (fn [_cfg]
                                        (fn [& {:as message}]
                                          (swap! logs conj message)))
                    user/delete-user! (fn [_auth _selector]
                                        {:ok? false
                                         :error :user/not-found})]
        (let [result (admin/delete-user! "missing@example.com")
              log-msg (first @logs)]
          (is (= false (:ok? result)))
          (is (= :user/not-found (:error result)))
          (is (= "admin/delete-user" (:operation log-msg)))
          (is (= false (:success log-msg)))
          (is (= :warning (:level log-msg)))
          (is (string? (:message log-msg))))))))

(deftest set-password-logs-exception-and-rethrows
  (testing "Admin set-password logs exception outcome and rethrows."
    (let [logs (atom [])]
      (with-redefs [app/state {:ferment.auth/setup :auth-source}
                    oplog/auth-logger (fn [_cfg]
                                        (fn [& {:as message}]
                                          (swap! logs conj message)))
                    user/change-password! (fn [_auth _selector _new-password]
                                            (throw (ex-info "Boom." {:kind :test/error})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Boom\."
                              (admin/set-password! "u@x.io" "new-secret")))
        (let [log-msg (first @logs)]
          (is (= "admin/set-password" (:operation log-msg)))
          (is (= false (:success log-msg)))
          (is (= :error (:level log-msg))))))))

(deftest admin-user-lifecycle-produces-oplog-sequence
  (testing "create-user, set-password, and delete-user produce ordered auth oplog events."
    (let [logs (atom [])
          users* (atom {"user@example.com" {:id 101
                                            :email "user@example.com"
                                            :account-type :operator}})]
      (with-redefs [app/state {:ferment.auth/setup :auth-source}
                    oplog/auth-logger (fn [_cfg]
                                        (fn [& {:as message}]
                                          (swap! logs conj message)))
                    user/create-user! (fn [_auth email _password account-type]
                                        (if (contains? @users* email)
                                          {:ok? false :error :user/already-exists}
                                          (let [id (inc (count @users*))]
                                            (swap! users* assoc email {:id id
                                                                       :email email
                                                                       :account-type account-type})
                                            {:ok? true
                                             :created? true
                                             :user {:user/id id
                                                    :user/email email
                                                    :user/account-type account-type}})))
                    user/change-password! (fn [_auth selector _new-password]
                                            (if-let [u (get @users* selector)]
                                              {:ok? true
                                               :updated? true
                                               :user {:user/id (:id u)
                                                      :user/email (:email u)
                                                      :user/account-type (:account-type u)}}
                                              {:ok? false :error :user/not-found}))
                    user/delete-user! (fn [_auth selector]
                                        (if-let [u (get @users* selector)]
                                          (do
                                            (swap! users* dissoc selector)
                                            {:ok? true
                                             :deleted? true
                                             :user {:user/id (:id u)
                                                    :user/email (:email u)
                                                    :user/account-type (:account-type u)}})
                                          {:ok? false :error :user/not-found}))]
        (is (:ok? (admin/create-user! "new@example.com" "sekret" :operator)))
        (is (:ok? (admin/set-password! "new@example.com" "sekret-2")))
        (is (:ok? (admin/delete-user! "new@example.com")))
        (is (= ["admin/create-user" "admin/set-password" "admin/delete-user"]
               (mapv :operation @logs)))
        (is (every? true? (map :success @logs)))
        (is (every? #(= :info (:level %)) @logs))))))

(deftest role-admin-helpers-delegate-and-log
  (testing "grant-role!, list-roles! and revoke-role! delegate to ferment.user and emit auth oplog."
    (let [calls (atom [])
          logs  (atom [])]
      (with-redefs [app/state {:ferment.auth/setup :auth-source}
                    oplog/auth-logger (fn [_cfg]
                                        (fn [& {:as message}]
                                          (swap! logs conj message)))
                    user/grant-role! (fn [_auth selector role]
                                       (swap! calls conj [:grant selector role])
                                       {:ok? true
                                        :granted? true
                                        :role role
                                        :user {:user/id 9}
                                        :roles #{role}})
                    user/list-roles! (fn [_auth selector]
                                       (swap! calls conj [:list selector])
                                       {:ok? true
                                        :user {:user/id 9}
                                        :roles #{:role/admin}})
                    user/revoke-role! (fn [_auth selector role]
                                        (swap! calls conj [:revoke selector role])
                                        {:ok? true
                                         :revoked? true
                                         :role role
                                         :user {:user/id 9}
                                         :roles #{}})]
        (is (:ok? (admin/grant-role! "user@example.com" :role/admin)))
        (is (:ok? (admin/list-roles! "user@example.com")))
        (is (:ok? (admin/revoke-role! "user@example.com" :role/admin)))
        (is (= [[:grant "user@example.com" :role/admin]
                [:list "user@example.com"]
                [:revoke "user@example.com" :role/admin]]
               @calls))
        (is (= ["admin/grant-role" "admin/list-roles" "admin/revoke-role"]
               (mapv :operation @logs)))))))

(deftest role-dictionary-admin-helpers-delegate-and-log
  (testing "create-role!, list-known-roles!, delete-role! delegate to ferment.user and emit auth oplog."
    (let [calls (atom [])
          logs  (atom [])]
      (with-redefs [app/state {:ferment.auth/setup :auth-source}
                    oplog/auth-logger (fn [_cfg]
                                        (fn [& {:as message}]
                                          (swap! logs conj message)))
                    user/create-role! (fn
                                        ([_auth role]
                                         (swap! calls conj [:create role nil])
                                         {:ok? true :created? true :role role})
                                        ([_auth role description]
                                         (swap! calls conj [:create role description])
                                         {:ok? true :created? true :role role :description description}))
                    user/list-known-roles! (fn [_auth]
                                             (swap! calls conj [:list-known])
                                             {:ok? true :roles [{:role :role/admin}]})
                    user/delete-role! (fn [_auth role]
                                        (swap! calls conj [:delete role])
                                        {:ok? true :deleted? true :role role})]
        (is (:ok? (admin/create-role! :role/researcher)))
        (is (:ok? (admin/create-role! :role/researcher "Role used in diagnostics.")))
        (is (:ok? (admin/list-known-roles!)))
        (is (:ok? (admin/delete-role! :role/researcher)))
        (is (= [[:create :role/researcher nil]
                [:create :role/researcher "Role used in diagnostics."]
                [:list-known]
                [:delete :role/researcher]]
               @calls))
        (is (= ["admin/create-role" "admin/create-role" "admin/list-known-roles" "admin/delete-role"]
               (mapv :operation @logs)))))))
