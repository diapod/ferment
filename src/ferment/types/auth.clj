(ns

    ^{:doc    "ferment service, authorization record types."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.types.auth

  (:require [ferment] [phone-number.core]))

(in-ns 'ferment)

(import  [clojure.lang      Keyword PersistentVector IPersistentMap ISeq Fn IFn]
         [phone_number.core Phoneable]
         [javax.sql         DataSource]
         [java.time         Duration])

(defrecord AccountTypes     [^String           sql
                             ^PersistentVector ids
                             ^PersistentVector names
                             ^Keyword          default
                             ^String           default-name])

(defrecord AuthLocking      [^Long             max-attempts
                             ^Duration         lock-wait
                             ^Duration         fail-expires])

(defrecord AuthConfirmation [^Long             max-attempts
                             ^Duration         expires])

(defrecord AuthPasswords    [^Keyword          id
                             ^ISeq             suite
                             ^Fn               check
                             ^Fn               check-json
                             ^Fn               encrypt
                             ^Fn               encrypt-json
                             ^Fn               wait])

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

;; DBPassword record is to pass intrinsic suite in JSON format and shared suite ID.
;; It's used to pass data around.

(defrecord DBPassword       [^Long             password-suite-id
                             ^String           password])

;; UserData record is to pass user data between models and controllers
;; in a bit faster way than with regular maps.

(defrecord UserData         [^String           email
                             ^Phoneable        phone
                             ^Keyword          account-type
                             ^AuthConfig       auth-config
                             ^DataSource       db
                             ^String           password
                             ^String           password-shared
                             ^Long             password-suite-id
                             ^String           first-name
                             ^String           middle-name
                             ^String           last-name
                             ^Duration         expires-in
                             ^Long             max-attempts])

(defrecord PasswordData     [^String           shared
                             ^String           intrinsic
                             ^Long             suite-id])

;; AuthQueries record is used to pass a set of SQL queries in some structured form.

(defrecord AuthQueries      [^String           generic
                             ^String           pre
                             ^String           post
                             ^String           single])

;; Password types

(defrecord Suites           [^IPersistentMap   shared
                             ^IPersistentMap   intrinsic])

(defrecord SuitesJSON       [^String           shared
                             ^String           intrinsic])
