(ns ferment.auth-locking-test
  (:require [clojure.test :refer [deftest is testing]]
            [tick.core :as t]
            [io.randomseed.utils.auth.locking :as locking]))

(deftest hard-lock-detection
  (testing "hard-locked? is true when hard lock timestamp exists."
    (is (true? (locking/hard-locked? {:locked (t/instant "2026-02-19T10:00:00Z")})))
    (is (false? (locking/hard-locked? {:locked nil})))
    (is (false? (locking/hard-locked? nil)))))

(deftest soft-lock-window
  (testing "soft-locked? uses lock wait window and soft-lock-remains returns positive duration."
    (let [auth-config {:locking {:lock-wait (t/new-duration 10 :minutes)}}
          now         (t/instant "2026-02-19T10:10:00Z")
          soft-5m     {:soft_locked (t/instant "2026-02-19T10:05:00Z")}
          soft-15m    {:soft_locked (t/instant "2026-02-19T09:55:00Z")}]
      (is (true? (locking/soft-locked? soft-5m auth-config now)))
      (is (false? (locking/soft-locked? soft-15m auth-config now)))
      (is (some? (locking/soft-lock-remains soft-5m auth-config now)))
      (is (nil? (locking/soft-lock-remains soft-15m auth-config now))))))
