(ns ferment.tenancy-test
  (:require [clojure.test :refer [deftest is testing]]
            [ferment.tenancy :as tenancy]))

(deftest normalize-config-coerces-shape
  (testing "Tenancy config normalization coerces tenant/principal maps and numeric limits."
    (let [cfg (tenancy/normalize-config
               {:enabled? true
                :default-tenant "tenant/acme"
                :default {:limits {:requests-per-minute "30"}}
                :tenants {"tenant/acme" {:limits {:max-tokens-per-request 128}
                                         :routing/defaults {:profile "balanced"}
                                         :principal/overrides {"id:7" {:limits {:daily-max-billed-tokens 9000}}}}}
                :principal->tenant {"u@example.com" "tenant/acme"}})]
      (is (true? (:enabled? cfg)))
      (is (= :tenant/acme (:default-tenant cfg)))
      (is (= 30 (get-in cfg [:default :limits :requests-per-minute])))
      (is (= 128 (get-in cfg [:tenants :tenant/acme :limits :max-tokens-per-request])))
      (is (= :balanced (get-in cfg [:tenants :tenant/acme :routing/defaults :profile])))
      (is (= 9000 (get-in cfg [:tenants :tenant/acme :principal/overrides "id:7" :limits :daily-max-billed-tokens])))
      (is (= :tenant/acme (get-in cfg [:principal->tenant "email:u@example.com"])))))) ; normalized principal ref

(deftest apply-request-defaults-clamps-budget-and-timeout
  (testing "Request defaults clamp max-tokens and timeout to tenant limits."
    (let [ctx {:enabled? true
               :tenant/id :tenant/default
               :principal/ref "id:9"
               :routing/defaults {:profile :low-latency}
               :tenant/limits {:max-tokens-per-request 64
                               :max-timeout-ms 3000}
               :principal/limits {}
               :request/max-tokens 512}
          request {:routing {:strict? true}
                   :budget {:max-tokens 120}
                   :timeout-ms 9000}
          out (tenancy/apply-request-defaults request ctx)]
      (is (= :tenant/default (:tenant/id out)))
      (is (= "id:9" (:principal/ref out)))
      (is (= :low-latency (get-in out [:routing :profile])))
      (is (= true (get-in out [:routing :strict?])))
      (is (= 64 (get-in out [:budget :max-tokens])))
      (is (= 3000 (:timeout-ms out))))))
