(ns

    ^{:doc    "ferment service, authorization record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.types.auth

  (:require [ferment]
            [io.randomseed.utils.auth.types]))

(in-ns 'ferment)

(import  [clojure.lang      Keyword PersistentVector IPersistentMap]
         [io.randomseed.utils.auth.types AuthLocking AuthPasswords]
         [javax.sql         DataSource]
         [java.time         Duration])

(defrecord AccountTypes     [^String           sql
                             ^PersistentVector ids
                             ^PersistentVector names
                             ^Keyword          default
                             ^String           default-name])

(defrecord AuthConfirmation [^Long             max-attempts
                             ^Duration         expires])

(defrecord AuthConfig       [^Keyword          id
                             ^DataSource       db
                             ^AccountTypes     account-types
                             ^AccountTypes     parent-account-types
                             ^AuthConfirmation confirmation
                             ^AuthLocking      locking
                             ^AuthPasswords    passwords])

(defrecord AuthSettings     [^DataSource       db
                             ^Keyword          default-type
                             ^AuthConfig       default
                             ^IPersistentMap   types])
