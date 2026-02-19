(ns

    ^{:doc    "Authentication lock-state helpers (hard lock, soft lock, remaining wait)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.locking

  (:require [tick.core :as t]
            [io.randomseed.utils.time :as time]))

(def ^:const lock-wait-default
  (t/new-duration 10 :minutes))

(defn hard-lock-time
  "Gets hard-lock timestamp from user/auth map."
  [m]
  (when (map? m)
    (or (:auth/locked-at m)
        (:locked-at m)
        (:locked m))))

(defn soft-lock-time
  "Gets soft-lock timestamp from user/auth map."
  [m]
  (when (map? m)
    (or (:auth/soft-locked-at m)
        (:soft-locked-at m)
        (:soft_locked m)
        (:soft-locked m))))

(defn soft-lock-passed
  "Returns duration passed since soft lock until `now` (positive only)."
  [m now]
  (when-some [lock-time (soft-lock-time m)]
    (let [d (t/between lock-time now)]
      (when (time/pos-duration? d) d))))

(defn lock-wait
  "Returns lock wait duration from auth config map/record or explicit duration."
  [auth-config-or-lock-wait]
  (or (if (map? auth-config-or-lock-wait)
        (get-in auth-config-or-lock-wait [:locking :lock-wait])
        auth-config-or-lock-wait)
      lock-wait-default))

(defn hard-locked?
  "Returns true when hard lock timestamp is present."
  ^Boolean [m]
  (some? (hard-lock-time m)))

(defn soft-locked?
  "Returns true when current soft-lock window has not expired."
  (^Boolean [lock-passed auth-config-or-lock-wait]
   (if lock-passed
     (if-some [wait' (lock-wait auth-config-or-lock-wait)]
       (t/< lock-passed wait')
       false)
     false))
  (^Boolean [m auth-config-or-lock-wait now]
   (if auth-config-or-lock-wait
     (if-some [passed (soft-lock-passed m now)]
       (if-some [wait' (lock-wait auth-config-or-lock-wait)]
         (t/< passed wait')
         false)
       false)
     false)))

(defn soft-lock-remains
  "Returns remaining soft-lock duration (or nil when no active soft lock)."
  ([lock-passed auth-config-or-lock-wait]
   (when lock-passed
     (when-some [wait' (lock-wait auth-config-or-lock-wait)]
       (t/- wait' lock-passed))))
  ([m auth-config-or-lock-wait now]
   (when-some [passed (soft-lock-passed m now)]
     (when-some [wait' (lock-wait auth-config-or-lock-wait)]
       (let [d (t/- wait' passed)]
         (when (time/pos-duration? d) d))))))
