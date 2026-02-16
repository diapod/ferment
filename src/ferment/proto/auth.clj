(ns

    ^{:doc    "ferment service, authentication protocol."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.proto.auth

  (:require [ferment] [ferment.types.auth])
  (:import  (ferment     AccountTypes
                         AuthLocking
                         AuthConfirmation
                         AuthPasswords
                         AuthConfig
                         AuthSettings)
            (javax.sql   DataSource)
            (java.time   Duration)))

(defprotocol Authenticable
  "This protocol is used to access authentication settings and configuration."

  (^{:tag AuthSettings}
   settings
   [settings-src]
   "Returns a global authentication settings of type `AuthSettings` on a basis of
  configuration source provided.")

  (^{:tag AuthConfig}
   config
   [settings-src] [settings-src account-type]
   "Returns an authentication configuration of type `AuthConfig` on a basis of
  configuration source `settings-src` and `account-type` provided.")

  (^{:tag DataSource}
   db
   [settings-src] [settings-src account-type]
   "Returns a database connection object on a basis of configuration source
  `settings-src` and optional `account-type` provided."))

(defprotocol Authorizable
  "Authorizable protocol is to support class-based, single method dispatch when dealing
  with authorization and authentication data. The `auth-source` can be a data source,
  an `AuthConfig` record, or a global `AuthSettings` record."

  (^{:tag clojure.lang.Associative}
   get-user-auth-data
   [auth-source email queries]
   [auth-source email account-type queries]))
