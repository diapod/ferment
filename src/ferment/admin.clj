(ns ferment.admin
  "Administrative wrappers over `ferment.user` functions using app auth setup."
  (:require [ferment.app :as app]
            [ferment.db :as db]
            [ferment.oplog :as oplog]
            [ferment.user :as user]))

(defn- auth-source
  []
  (:ferment.auth/setup app/state))

(defn- auth-logger
  []
  (oplog/auth-logger app/state))

(defn- log-admin-op!
  [operation result]
  (let [logger  (auth-logger)
        result' (if (map? result) result {})
        ok?     (true? (:ok? result'))
        level   (if ok? :info :warning)
        user-id (get-in result' [:user :user/id])
        message (if ok?
                  "Admin operation succeeded."
                  (str "Admin operation failed: " (or (:error result') :unknown)))
        event   (cond-> {:operation operation
                         :success ok?
                         :level level
                         :message message}
                  (some? user-id) (assoc :user-id user-id))]
    (when (fn? logger)
      (apply logger (mapcat identity event)))))

(defn- log-admin-exception!
  [operation ^Throwable t]
  (let [logger (auth-logger)
        event {:operation operation
               :success false
               :level :error
               :message (str "Admin operation crashed: " (.getMessage t))}]
    (when (fn? logger)
      (apply logger (mapcat identity event)))))

(defn- run-admin-op
  [operation f]
  (try
    (let [result (f)]
      (log-admin-op! operation result)
      result)
    (catch Throwable t
      (log-admin-exception! operation t)
      (throw t))))

(defn create-user!
  "Creates a user using auth source from current app state."
  ([params]
   (run-admin-op "admin/create-user"
                 #(user/create-user! (auth-source) params)))
  ([email password]
   (run-admin-op "admin/create-user"
                 #(user/create-user! (auth-source) email password)))
  ([email password account-type]
   (run-admin-op "admin/create-user"
                 #(user/create-user! (auth-source) email password account-type))))

(defn delete-user!
  "Deletes a user using auth source from current app state."
  [selector]
  (run-admin-op "admin/delete-user"
                #(user/delete-user! (auth-source) selector)))

(defn set-password!
  "Changes user password using auth source from current app state."
  ([selector new-password]
   (run-admin-op "admin/set-password"
                 #(user/change-password! (auth-source) selector new-password)))
  ([selector new-password account-type]
   (run-admin-op "admin/set-password"
                 #(user/change-password! (auth-source) selector new-password account-type))))

(defn lock-user!
  "Applies lock on user using auth source from current app state."
  ([selector]
   (run-admin-op "admin/lock-user"
                 #(user/lock-user! (auth-source) selector :hard)))
  ([selector lock-kind]
   (run-admin-op "admin/lock-user"
                 #(user/lock-user! (auth-source) selector lock-kind))))

(defn unlock-user!
  "Removes lock on user using auth source from current app state."
  [selector]
  (run-admin-op "admin/unlock-user"
                #(user/unlock-user! (auth-source) selector)))

(defn reset-login-attempts!
  "Resets login attempts for user using auth source from current app state."
  [selector]
  (run-admin-op "admin/reset-login-attempts"
                #(user/reset-login-attempts! (auth-source) selector)))

(defn migrate!
  "Runs DB migrations via `ferment.db/migrate!`."
  ([]
   (db/migrate!))
  ([opts]
   (db/migrate! opts)))

(defn rollback!
  "Rolls back DB migrations via `ferment.db/rollback!`."
  ([]
   (db/rollback!))
  ([opts]
   (db/rollback! opts))
  ([opts amount-or-id]
   (db/rollback! opts amount-or-id)))
