(ns ferment.roles-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.roles :as roles]))

(deftest roles-config-normalization
  (testing "Role config is normalized to keyword keys and sets."
    (let [cfg (roles/preconfigure-roles
               :ferment.roles/default
               {:enabled? "yes"
                :authorize-default? nil
                :account-type->roles {"system" [:role/admin]
                                      :user   :role/user}
                :operations {"http.v1/act" {:any [:role/admin]
                                            :forbidden [:role/banned]}}})]
      (is (true? (:enabled? cfg)))
      (is (true? (:authorize-default? cfg)))
      (is (= #{:role/admin}
             (get-in cfg [:account-type->roles :system])))
      (is (= #{:role/user}
             (get-in cfg [:account-type->roles :user])))
      (is (= #{:role/admin}
             (get-in cfg [:operations :http.v1/act :any])))
      (is (= #{:role/banned}
             (get-in cfg [:operations :http.v1/act :forbidden]))))))

(deftest roles-for-user-derives-from-account-type
  (testing "Effective roles are derived from account type and explicit user roles."
    (let [cfg {:account-type->roles {:manager #{:role/admin}
                                     :user    #{:role/user}}
               :logged-in-role :role/logged-in}
          roles' (roles/roles-for-user cfg {:user/id 10
                                            :user/account-type :manager
                                            :user/roles [:role/custom]})]
      (is (contains? roles' :role/admin))
      (is (contains? roles' :role/logged-in))
      (is (contains? roles' :role/custom)))))

(deftest allowed-evaluates-rules
  (testing "Authorization rules support :any, :all and :forbidden."
    (let [cfg {:enabled? true
               :authorize-default? false
               :account-type->roles {:manager #{:role/admin}
                                     :user #{:role/user}}
               :operations {:admin/create-user {:any #{:role/admin}}
                            :admin/audit {:all #{:role/admin :role/auditor}}
                            :admin/blocked {:forbidden #{:role/banned}}}}]
      (is (true? (roles/allowed? cfg :admin/create-user {:user/account-type :manager})))
      (is (false? (roles/allowed? cfg :admin/create-user {:user/account-type :user})))
      (is (true? (roles/allowed? cfg :admin/audit {:user/account-type :manager
                                                   :user/roles #{:role/auditor}})))
      (is (false? (roles/allowed? cfg :admin/audit {:user/account-type :manager})))
      (is (false? (roles/allowed? cfg :admin/blocked {:user/account-type :manager
                                                      :user/roles #{:role/banned}})))
      (is (false? (roles/allowed? cfg :admin/missing {:user/account-type :manager}))))))

(deftest effect-policy-evaluates-rules
  (testing "Effect authorization applies :any/:all/:forbidden rules to requested effects."
    (let [cfg {:enabled? true
               :authorize-default? false
               :account-type->roles {:manager #{:role/admin}
                                     :user #{:role/user}}
               :effects {:none {:any #{:role/user :role/admin}}
                         :fs/write {:any #{:role/admin}}
                         :fs/read {:any #{:role/user :role/admin}}
                         :db/write {:all #{:role/admin :role/db-writer}}
                         :process/run {:forbidden #{:role/suspended}}}}]
      (is (true? (roles/effect-allowed? cfg :none {:user/account-type :user})))
      (is (false? (roles/effect-allowed? cfg :fs/write {:user/account-type :user})))
      (is (true? (roles/effect-allowed? cfg :db/write {:user/account-type :manager
                                                       :user/roles #{:role/db-writer}})))
      (is (false? (roles/effect-allowed? cfg :process/run {:user/account-type :manager
                                                            :user/roles #{:role/suspended}})))
      (is (true? (roles/effects-allowed? cfg #{:none :fs/read} {:user/account-type :user})))
      (is (false? (roles/effects-allowed? cfg #{:none :fs/write} {:user/account-type :user})))
      (is (= #{:fs/write}
             (roles/denied-effects cfg #{:none :fs/write} {:user/account-type :user})))
      (is (= {:ok? false
              :error :auth/forbidden-effect
              :effects #{:none :fs/write}
              :denied #{:fs/write}
              :roles #{:role/user}}
             (roles/authorize-effects cfg #{:none :fs/write} {:user/account-type :user}))))))
