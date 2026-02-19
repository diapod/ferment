(ns ferment.admin
  "Administrative wrappers over `ferment.user` functions using app auth setup."
  (:require [ferment.app :as app]
            [ferment.db :as db]
            [ferment.user :as user]))

(defn- auth-source
  []
  (:ferment.auth/setup app/state))

(defn create-user!
  "Creates a user using auth source from current app state."
  ([params]
   (user/create-user! (auth-source) params))
  ([email password]
   (user/create-user! (auth-source) email password))
  ([email password account-type]
   (user/create-user! (auth-source) email password account-type)))

(defn delete-user!
  "Deletes a user using auth source from current app state."
  [selector]
  (user/delete-user! (auth-source) selector))

(defn set-password!
  "Changes user password using auth source from current app state."
  ([selector new-password]
   (user/change-password! (auth-source) selector new-password))
  ([selector new-password account-type]
   (user/change-password! (auth-source) selector new-password account-type)))

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
