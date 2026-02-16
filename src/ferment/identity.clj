(ns

    ^{:doc    "Basic identity management for ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.identity

  (:refer-clojure :exclude [parse-long random-uuid type])

  (:require  [clojure.string               :as                     str]
             [clj-uuid                     :as                    uuid]
             [ferment.proto.identity     :as                       p]
             [ferment.types.identity]
             [phone-number.core            :as                   phone]
             [phone-number.util            :as                  phutil]
             [io.randomseed.utils.db       :as                     rdb]
             [io.randomseed.utils.map      :as                     map]
             [io.randomseed.utils          :refer  [safe-parse-long
                                                    some-str
                                                    some-keyword
                                                    not-empty-string?]]
             [ferment])

  (:import  (java.util                    UUID)
            (clojure.lang                 Symbol
                                          Keyword
                                          Associative)
            (ferment                    Identity)
            (ferment.proto.identity     Identifiable)
            (com.google.i18n.phonenumbers Phonenumber$PhoneNumber)))

;; Standard identity types

(p/add-type! :email :phone :uid :id)

;; Standard identity string matchers

(declare type?)

(defn id-phone-string
  "Returns `true` if the given string looks like a phone number. If identity type or
  acceptable type `t` is given, then it must be a parent or be equal to `:phone`."
  (^Keyword [^String v]
   (when (and (= \+ (.charAt v 0))
              (> (.length v) 4))
     :phone))
  (^Keyword [^String v ^Keyword t]
   (when (type? :phone t)
     (id-phone-string v))))

(defn id-email-string
  "Returns `true` if the given string looks like an e-mail address. If identity type or
  acceptable type `t` is given, then it must be a parent or be equal to `:email`."
  (^Keyword [^String v]
   (when (and (> (.length v) 2)
              (pos-int? (str/index-of v \@ 1)))
     :email))
  (^Keyword [^String v ^Keyword t]
   (when (type? :email t)
     (id-email-string v))))

(defn id-uid-string
  "Returns `true` if the given string looks like a UID (UUID). If identity type or
  acceptable type `t` is given, then it must be a parent or be equal to `:uid`."
  (^Keyword [^String v]
   (when (uuid/uuid-string? v) :uid))
  (^Keyword [^String v ^Keyword t]
   (when (type? :uid t)
     (id-uid-string v))))

(defn id-id-string
  "Returns `true` if the given string looks like a user ID. If identity type or
  acceptable type `t` is given, then it must be a parent or be equal to `:id`."
  (^Keyword [^String v]
   (when (pos-int? (safe-parse-long v)) :id))
  (^Keyword [^String v ^Keyword t]
   (when (type? :id t)
     (id-id-string v))))

(p/add-type-string-matcher! id-phone-string
                            id-email-string
                            id-uid-string
                            id-id-string)

;; Standard identity parsers

(defn preparse-email
  "Parses e-mail by doing basic checks and transforming it to a string."
  ^String [v]
  (when-some [^String v (some-str v)]
    (let [l (unchecked-int (.length v))]
      (when (> l 2)
        (when-some [idx ^long (str/index-of v \@ 1)]
          (when (> (unchecked-dec-int l) idx 0)
            (str (subs v 0 idx) (str/lower-case (subs v idx l)))))))))

(defn of-email
  "Parses e-mail by doing basic checks and transforming it to an Identity object."
  ^Identity [v]
  (when-some [^String v (preparse-email v)]
    (Identity. :email v)))

(defn preparse-id
  "Parses user ID by doing basic checks and transforming it to a long number."
  ^Long [v]
  (when-let [v (safe-parse-long v)]
    (when (pos-int? v)
      v)))

(defn of-id
  "Parses user ID by doing basic checks and transforming it to an Identity record."
  ^Identity [v]
  (when-some [^Long v (preparse-id v)]
    (Identity. :id v)))

(defn preparse-phone
  "Tries to create an object representing phone number. Returns `nil` if the input
  cannot be parsed."
  ^Phonenumber$PhoneNumber [v]
  (phutil/try-parse
   (phone/number-optraw v)))

(defn of-phone
  "Tries to interpret `v` as a phone number and returns an Identity record."
  ^Identity [v]
  (when-some [v (preparse-phone v)]
    (Identity. :phone v)))

(defn preparse-uid
  "Tries to create UUID. Returns `nil` if the input cannot be converted to it."
  ^UUID [v]
  (if (uuid? v) v
      (when (and v (uuid/uuidable? v))
        (uuid/as-uuid v))))

(defn of-uid
  "Tries to interpret `v` as a UUID and returns an Identity record."
  ^Identity [v]
  (when-some [u (preparse-uid v)]
    (Identity. :uid u)))

;; Public functions

(defn type?
  "Returns `true` if the given identity type identifier `t` exists and is valid.

  If `acceptable-tag` is given this function will in addition check if the given tag
  is a parent of the given type identifier or is `:ferment.identity/any`. To add
  acceptable type(s) use `ferment.proto.identity/add-acceptable-type`."
  ([t]
   (isa? p/type-hierarchy (some-keyword t) ::valid))
  ([t ^Keyword acceptable-tag]
   (and acceptable-tag
        (if (identical? ::any acceptable-tag)
          (isa? p/type-hierarchy (some-keyword t) ::valid)
          (isa? p/type-hierarchy (some-keyword t) acceptable-tag)))))

(defn check-type
  "Returns a keyword for the given identity type `identity-type` if it is a valid
  identity. Otherwise it returns `nil`. To add acceptable type(s) use
  `ferment.proto.identity/add-acceptable-type`.

  If acceptable parent tag `acceptable-tag` is given, it must be a parent of the
  given identity type tag."
  (^Keyword [id-type]
   (when-some [t (some-keyword id-type)]
     (when (isa? p/type-hierarchy t ::valid) t)))
  (^Keyword [id-type acceptable-tag]
   (when-let [t (some-keyword id-type)]
     (when acceptable-tag
       (if (identical? ::any acceptable-tag)
         (when (isa? p/type-hierarchy t ::valid) t)
         (when (isa? p/type-hierarchy t acceptable-tag) t))))))

(defn type
  "Returns a keyword describing identity type detected by analyzing the given user
  identity `user-identity` and optional identity type `identity-type` given
  explicitly (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id`
  for numeric user ID, `:uid` for UUID). If the type is not given, analysis of the
  given identity will be performed.

  Value of the given identity type `identity-type` must match the identity type, and
  not be `nil` nor `false`. If any of these happens, `nil` will be returned."
  (^Keyword [user-identity]
   (when user-identity (p/type user-identity)))
  (^Keyword [^Keyword identity-type user-identity]
   (when user-identity
     (let [t (some-keyword identity-type)]
       (if (identical? ::any t)
         (p/type user-identity)
         (when t (p/type (p/make user-identity t))))))))

(defn type-opt
  "Returns a keyword describing identity type detected by analyzing the given user
  identity `user-identity` and optional identity type `identity-type` given
  explicitly (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id`
  for numeric user ID, `:uid` for UUID).

  If the type is not given or it is `nil`, analysis of the given identity will be
  performed.

  Value of the given identity type `identity-type` must match the identity type. If
  it does not, `nil` will be returned."
  (^Keyword [user-identity]
   (when user-identity (p/type user-identity)))
  (^Keyword [^Keyword identity-type user-identity]
   (when user-identity
     (let [t (some-keyword identity-type)]
       (if (and t (not (identical? ::any identity-type)))
         (p/type (p/make user-identity t))
         (p/type user-identity))))))

(defn acceptable-type
  "Returns a keyword describing identity type detected by analyzing the given user
  identity `user-identity` and optional identity type `identity-type` given
  explicitly (e.g. `:phone` for a phone number, `:email` for e-mail address, `:id`
  for numeric user ID, `:uid` for UUID).

  If the type is not given, or if it is `nil` or `false`, analysis of the given
  identity will be performed.

  The given identity type must always be valid (registered with
  `ferment.proto.identity/add-type!`) and the identity must not be `nil` nor
  `false`. If any of these happens, `nil` will be returned.

  The `acceptable-tag` should be a tag type from which the given type is derived
  within the hierarchy `ferment.proto.identity/type-hierarchy`.

  To add acceptable type(s) use `ferment.proto.identity/add-acceptable-type`."
  (^Keyword [user-identity ^Keyword acceptable-tag]
   (when (and user-identity acceptable-tag)
     (when-let [t (p/type user-identity)]
       (when (and acceptable-tag (isa? p/type-hierarchy t acceptable-tag)) t))))
  (^Keyword [user-identity identity-type ^Keyword acceptable-tag]
   (when (and user-identity acceptable-tag)
     (let [t (some-keyword identity-type)]
       (if (and t (not (identical? ::any t)))
         (when (and acceptable-tag (isa? p/type-hierarchy t acceptable-tag))
           (p/type (p/make user-identity t)))
         (acceptable-type user-identity acceptable-tag))))))

(defn of-known-type?
  "Returns `true` if the given value `v` is a user identity of the known type.

  If the `acceptable-tag` is present then it should be a tag type from which the
  given type is derived within hierarchy `ferment.proto.identity/type-hierarchy`.

  To add acceptable type(s) use `ferment.proto.identity/add-acceptable-type`."
  ([v]
   (if v
     (if-let [t (p/type v)]
       (isa? p/type-hierarchy t ::valid)
       false)
     false))
  ([v acceptable-tag]
   (if (and v acceptable-tag)
     (if-let [t (p/type v)]
       (isa? p/type-hierarchy t acceptable-tag)
       false)
     false)))

(defn value
  "Returns a value of the given identity type `user-identity` and optional identity
  type `identity-type`. If the type is not given, analysis of the given identity will
  be performed to establish the type first. If the type is given, it must be a valid
  type, otherwise `nil` will be returned."
  (^Keyword [user-identity]
   (when user-identity (p/value user-identity)))
  (^Keyword [^Keyword identity-type user-identity]
   (when user-identity
     (when-some [t (some-keyword identity-type)]
       (p/value user-identity t)))))

;; Parsing

(defmulti parser
  "Takes an identity type expressed as keyword and returns a parser suitable for that
  identity type. The parser function takes 1 argument and converts the given value to
  identity record (of type `ferment.Identity`).

  Caution: The identity type must be a keyword (it will not be coerced)."
  (fn ^Keyword [identity-type] identity-type)
  :hierarchy #'p/type-hierarchy)

(defn parse-group
  "Parses identity `identity-type` with `acceptable-type` tag which should be a parent
  tag grouping identity types (like `:ferment.identity/any` or another). Will use
  `ferment.proto.identity/type` passing a tag to it to limit the parsers tried and
  constrain the identity type to one that belongs to a parent tag in
  `ferment.proto.identity/type-hierarchy`. Hint: new parent tags can be registered
  with `ferment.proto.identity/add-acceptable-type!`.

  This is internal function. Use `of-type` instead, which will choose the appropriate
  parser via multimethod and cache the results."
  ^Identity [^Keyword acceptable-type ^Identifiable user-identity]
  (when-some [^Keyword t (p/type user-identity acceptable-type)]
    ((parser t) user-identity)))

(defn parse-single
  "Parses identity `user-identity` by choosing the parser with `parser` multimethod on
  a basis of the given `identity-type` or a detected identity type (by getting it
  with `ferment.proto.identity/type`). Returns user identity object or `nil`.

  This is internal function. Use `of` or `of-type` instead, which will cache the
  results."
  (^Identity [^Keyword identity-type ^Identifiable user-identity]
   (when user-identity ((parser identity-type) user-identity)))
  (^Identity [^Identifiable user-identity]
   (when user-identity ((parser (p/type user-identity)) user-identity))))

(def ^{:tag      Identity
       :arglists '(^Identity [^Identifiable user-identity]
                   ^Identity [^Keyword identity-type ^Identifiable user-identity])
       :see-also ["of" "of-type"]}
  parse
  "Parses the given identity `user-identity` with optional `identity-type`
  predefined. Returns an identity record of type `ferment.Identity`. Memoized proxy
  for parsing strings and other non-native data.

  Do not use it directly, use `of` or `of-type` instead.

  Caution: The identity type must be a keyword (it will not be coerced)."
  (rdb/memoize parse-single 4096))

;; (defn- parse-indexed
;;   "(Premature optimization is the root of all evil.)"
;;   [[^Keyword t indexed-ids]]
;;   (let [[indexes ids] (reduce
;;                        (fn [[indexes ids] [i v]] [(cons i indexes) (cons v ids)])
;;                        [() ()] indexed-ids)]
;;     (map vector indexes (map (parser t) ids))))

;; (defn- parse-multi
;;   "(Premature optimization is the root of all evil.)"
;;   ([^Keyword identity-type user-identities]
;;    (map (parser identity-type) user-identities))
;;   ([user-identities]
;;    (->> (map-indexed vector user-identities)
;;         (group-by #(p/type (nth % 1)))
;;         (mapcat parse-indexed)
;;         (sort-by #(nth % 0))
;;         (map #(nth % 1)))))

(defmethod parser :email   [_] of-email)
(defmethod parser :phone   [_] of-phone)
(defmethod parser :uid     [_] of-uid)
(defmethod parser :id      [_] of-id)
(defmethod parser ::any    [_] parse)
(defmethod parser :default [_] (constantly nil))

(def identity-map-keys
  "Commonly known map keys which may contain user identity."
  [[:identity       ::any]
   [:user/identity  ::any]
   [:id               :id]
   [:user-id          :id]
   [:phone         :phone]
   [:email         :email]
   [:uid             :uid]
   [:login          ::any]
   [:user/id          :id]
   [:user/phone    :phone]
   [:user/email    :email]
   [:user/uid        :uid]
   [:user/login     ::any]
   [:id             ::any]
   [:user-identity  ::any]
   [:user           ::any]])

(def identity-map-keys-by-type
  "Commonly known map keys which may contain user identity grouped by identity types."
  (map/map-vals-by-k
   #(->> identity-map-keys
         (filter (comp #{% ::any} second))
         (map first)
         (distinct)
         (vec))
   (dissoc (group-by second identity-map-keys) ::any)))

(defn- try-map-keys
  [m identity-type]
  (when-some [ks (get identity-map-keys-by-type identity-type)]
    (when (seq m)
      (some (fn [k]
              (when-let [i (get m k)]
                (if (and (map? i) (not (record? i))) nil (p/make i identity-type))))
            ks))))

(defn parse-map
  "Tries to parse identity `v` by analyzing it based on its type and/or its structure.

  In case of maps with `:user/id`, `:user/uid`, `:user/email`, `:user/phone` keys it
  will use corresponding values to parse it.

  In case of an `:identity/type` key it will use its value as the identity type tag
  and will use it to parse `:identity/value` or `:user/identity` key's value.

  In case of a `:user/identity` key and no `:identity/type` key it will try to parse
  the value as any of supported identities.

  In case of all other map values it will return `nil`."
  ([m]
   (when (seq m)
     (some (fn [[k t]]
             (when-let [i (get m k)]
               (if (and (map? i) (not (record? i))) nil (p/make i t))))
           identity-map-keys)))
  ([^Keyword identity-type m]
   (when identity-type
     (if (identical? ::any identity-type)
       (parse-map m)
       (or (try-map-keys m identity-type)
           (some #(try-map-keys m %) (descendants p/type-hierarchy identity-type)))))))

;; Creating identities

(defn of
  "For the given user identity `user-identity` tries to parse the identity and return
  an `ferment.Identity` record object containing a detected identity type and
  identity value in a form it expresses it best. If the identity type cannot be
  established and it was not given, `nil` is returned.

  Optional identity type `identity-type` (which must be a keyword) may be given as a
  first argument. It is used to pick the right parser (if parsing is needed) or to
  simply reject wrong identity type (if `ferment.Identity` record is given).

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  If there is a need to give list of keywords as user identities (including the
  first), and not to give any identity type, then first argument's value needs to be
  converted to a string, or `of-value` or `of-type` should be used as an alternative.

  If multiple identities are given it will return a sequence of these identities
  parsed with parsing functions chosen for detected identity types. If an identity
  type is cannot be detected and/or is not valid, `nil` value will be inserted into
  the corresponding location of generated output sequence."
  {:see-also ["of-value" "of-type" "of-seq"]
   :arglists '(^Identity [^Identifiable user-identity]
               ^Identity [identity-type ^Identifiable user-identity]
               ^Identity [identity-type ^Identifiable user-identity & user-identities]
               ^Identity [^Identifiable user-identity & user-identities])}
  (^Identity [^Identifiable user-identity]
   (p/make user-identity))
  (^Identity [identity-type ^Identifiable user-identity]
   (if (keyword? identity-type)
     (p/make user-identity identity-type)
     (map p/make (cons identity-type (cons user-identity nil)))))
  ([identity-type ^Identifiable user-identity & ids]
   (if (keyword? identity-type)
     (map #(p/make % identity-type) (cons user-identity ids))
     (map p/make (cons identity-type (cons user-identity ids))))))

(defn of-value
  "For the given user identity `user-identity` tries to parse its value and returns it.

  If `identity-type` is given and `user-identity` is a correct identity it will return
  the user identity if it is of the given type or its child.

  If `identity-type` is given and `user-identity` is not a correct identity it will
  try to parse it. If parsing is successful it will return it only if it is of the
  given type or its child."
  {:see-also ["of" "of-type" "of-seq"]}
  (^Identity [^Identifiable user-identity]
   (p/make user-identity))
  ([user-identity & ids]
   (map p/make (cons user-identity ids))))

(defn of-type
  "For the given user identity `user-identity` and identity type `identity-type` it
  tries to parse the identity and return an `ferment.Identity` record containing an
  identity type and identity value in a form it expresses it best. If the identity
  type is `nil` then `nil` is returned.

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  If multiple identities are given it will return a sequence of these identities
  parsed with parsing functions chosen for the given identity type. If the identity
  type is not valid or the parsing cannot be applied for an input value, `nil` will
  be inserted into corresponding location of the output sequence."
  {:see-also ["of" "of-value" "of-seq"]}
  (^Identity [identity-type ^Identifiable user-identity]
   (p/make user-identity (some-keyword identity-type)))
  ([identity-type ^Identifiable user-identity & ids]
   (let [identity-type (some-keyword identity-type)]
     (map #(p/make % identity-type) (cons user-identity ids)))))

(defn opt-type
  "For the given user identity `user-identity` and identity type `identity-type` it
  tries to parse it (if possible) and returns `ferment.Identity` object only if it
  is of the given type or its child.

  This function differs from `of-type` in that it will return `nil` if
  `identity-type` is nil or false."
  {:see-also ["of" "of-value" "of-seq"]}
  (^Identity [identity-type ^Identifiable user-identity]
   (p/make user-identity  (or (some-keyword identity-type) ::any)))
  ([identity-type ^Identifiable user-identity & ids]
   (let [identity-type (some-keyword identity-type)]
     (if identity-type
       (map #(p/make % identity-type) (cons user-identity ids))
       (map p/make (cons user-identity ids))))))

(defn of-seq
  "For the given user identities `user-identities` tries to parse each identity and
  return an `ferment.Identity` record containing a detected identity type and
  identity value in a form it expresses it best. If the given identity type was given
  as an optional `identity-type` argument, it will be assumed as the expected type
  for all input values.

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  If an identity type was not given but it cannot be detected and/or is not valid,
  `nil` value will be inserted into the corresponding location of generated output
  sequence. Same with the given identity type which is not applicable to a particular
  value or invalid."
  {:see-also ["of" "of-value" "of-type"]}
  ([user-identities]
   (map p/make user-identities))
  ([identity-type user-identities]
   (when-some [identity-type (some-keyword identity-type)]
     (map #(p/make % identity-type) user-identities))))

(defn some-seq
  "Tries to coerce identities to `ferment.Identity` objects and filters out those who
  are not of the acceptable type (given by `acceptable-tag`, can be `:ferment.identity/any`
  to accept all types).

  Then it tries to parse each of the identities given and returns a sequence of
  `ferment.Identity` objects. Returns `nil` if `acceptable-tag` is `nil` or if
  no identities are to be parsed or no identities have been parsed successfully."
  ([user-identities]
   (->> (of-seq user-identities) (filter identity) seq))
  ([^Keyword identity-type user-identities]
   (->> (of-seq identity-type user-identities) (filter identity) seq)))

;; Standard class-based matchers and getters

(extend-protocol p/Identifiable

  Identity

  (literal? ^Boolean [_] false)

  (type
    (^Keyword [v] (.id-type ^Identity v))
    (^Keyword [v ^Keyword t]
     (let [^Keyword id-type (.id-type ^Identity v)]
       (when (type? id-type t) id-type))))

  (value
    ([v] (.value ^Identity v))
    ([v ^Keyword identity-type]
     (when (type? (.id-type ^Identity v) identity-type)
       (.value ^Identity v))))

  (make
    (^Identity [v] v)
    (^Identity [v ^Keyword identity-type]
     (when (type? (.id-type ^Identity v) identity-type) v)))

  String

  (literal? ^Boolean [_] true)

  (type
    (^Keyword [v]
     (when (not-empty-string? v)
       (p/type-string-match v)))
    (^Keyword [v ^Keyword t]
     (when (not-empty-string? v)
       (p/type-string-match v t))))

  (value
    ([v] (p/value (p/make v)))
    ([v ^Keyword identity-type]
     (p/value (p/make v identity-type))))

  (make
    (^Identity [v]
     (when (not-empty-string? v) (parse v)))
    (^Identity [v ^Keyword identity-type]
     (when (not-empty-string? v) (parse identity-type v))))

  Associative

  (literal? ^Boolean [_] false)

  (type
    (^Keyword [v]
     (p/type (parse-map v)))
    (^Keyword [v ^Keyword t]
     (p/type (parse-map t v))))

  (value
    ([v] (p/value (parse-map v)))
    ([v ^Keyword identity-type]
     (p/value (parse-map identity-type v))))

  (make
    (^Identity [v]
     (parse-map v))
    (^Identity [v ^Keyword identity-type]
     (parse-map identity-type v)))

  Keyword

  (literal? ^Boolean [_] true)

  (type
    (^Keyword [v]
     (p/type (some-str v)))
    (^Keyword [v ^Keyword t]
     (p/type (some-str v) t)))

  (value
    ([v] (p/value (some-str v)))
    ([v ^Keyword identity-type]
     (p/value (some-str v) identity-type)))

  (make
    (^Identity [v]
     (p/make (some-str v)))
    (^Identity [v ^Keyword identity-type]
     (p/make (some-str v) identity-type)))

  Symbol

  (literal? ^Boolean [_] false)

  (type
    (^Keyword [v]
     (p/type (some-str v)))
    (^Keyword [v ^Keyword t]
     (p/type (some-str v) t)))

  (value
    ([v] (p/value (some-str v)))
    ([v ^Keyword identity-type]
     (p/value (some-str v) identity-type)))

  (make
    (^Identity [v]
     (p/make (some-str v)))
    (^Identity [v ^Keyword identity-type]
     (p/make (some-str v) identity-type)))

  Number

  (literal? ^Boolean [_] true)

  (type
    (^Keyword [v]
     (when (pos-int? v) :id))
    (^Keyword [v ^Keyword t]
     (when (and (pos-int? v) (type? :id t)) :id)))

  (value
    (^Long [v]
     (when (pos-int? v) (long v)))
    (^Long [v ^Keyword identity-type]
     (when (and (pos-int? v) (type? :id identity-type))
       (long v))))

  (make
    (^Identity [v]
     (when (pos-int? v)
       (Identity. :id (long v))))
    (^Identity [v ^Keyword identity-type]
     (when (and (pos-int? v) (type? :id identity-type))
       (Identity. :id (long v)))))

  Phonenumber$PhoneNumber

  (literal? ^Boolean [_] true)

  (type
    (^Keyword [_] :phone)
    (^Keyword [_ ^Keyword t] (when (type? :phone t) :phone)))

  (value
    (^Phonenumber$PhoneNumber [v] v)
    (^Phonenumber$PhoneNumber [v ^Keyword identity-type]
     (when (type? :phone identity-type) v)))

  (make
    (^Identity [v] (Identity. :phone v))
    (^Identity [v ^Keyword identity-type]
     (when (type? :phone identity-type)
       (Identity. :phone v))))

  UUID

  (literal? ^Boolean [_] true)

  (type
    (^Keyword [_] :uid)
    (^Keyword [_ ^Keyword t] (when (type? :uid t) :uid)))

  (value
    (^UUID [v] v)
    (^UUID [v ^Keyword identity-type]
     (when (type? :uid identity-type) v)))

  (make
    (^Identity [v] (Identity. :uid v))
    (^Identity [v ^Keyword identity-type]
     (when (type? :uid identity-type) (Identity. :uid v))))

  Character

  (literal? ^Boolean [_] true)

  (type
    ([_] nil)
    ([_ _] nil))

  (value
    ([_] nil)
    ([_ _] nil))

  (make
    ([_] nil)
    ([_ _] nil))

  Boolean

  (literal? ^Boolean [_] true)

  (type
    ([_] nil)
    ([_ _] nil))

  (value
    ([_] nil)
    ([_ _] nil))

  (make
    ([_] nil)
    ([_ _] nil))

  nil

  (literal? ^Boolean [_] true)

  (type
    ([_] nil)
    ([_ _] nil))

  (value
    ([_] nil)
    ([_ _] nil))

  (make
    ([_] nil)
    ([_ _] nil))

  Object

  (literal? ^Boolean [_] false)

  (type
    ([v] (throw (ex-info "Identifiable/type: unsupported value"
                         {:value v :class (class v)})))
    ([v t] (throw (ex-info "Identifiable/type: unsupported value"
                           {:value v :class (class v) :expected t}))))

  (value
    ([v] (throw (ex-info "Identifiable/value: unsupported value"
                         {:value v :class (class v)})))
    ([v t] (throw (ex-info "Identifiable/value: unsupported value"
                           {:value v :class (class v) :expected t}))))

  (make
    ([v] (throw (ex-info "Identifiable/make: unsupported value"
                         {:value v :class (class v)})))
    ([v t] (throw (ex-info "Identifiable/make: unsupported value"
                           {:value v :class (class v) :expected t})))))

;; DB conversions

(defmulti to-db*
  "For the given user identity `user-identity` which must be of type
  `ferment.Identity`, tries to express the identity in a database suitable format.

  If the given value cannot be used as valid identity, `nil` is returned.

  If the `identity-type` is given, it must be a keyword and it should be a valid
  identity type. It instructs the function to treat the given identity as of this
  type.

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  This is internal multimethod which does not perform conversions or checks. Use
  `to-db` or `->db` instead."
  {:arglists '([^Identifiable user-identity]
               [identity-type ^Identifiable user-identity])
   :see-also ["to-db" "->db"]}
  (fn
    (^Keyword [^Identity user-identity] (p/type user-identity))
    (^Keyword [^Keyword identity-type ^Identity _user-identity] identity-type))
  :hierarchy #'p/type-hierarchy)

(defn to-db
  "For the given user identity `user-identity` tries to express the identity in a
  database-ready form. It uses a `formatter` function from
  `ferment.proto.identity/type-formatters` or passes the value unchanged.

  Optional identity type `identity-type` is a type tag which can be used to check if
  the identity is of this type or its child.

  If formatter is not found, it will return `nil`.

  Built-ins:

  For identity type `:id` it will return a number.
  For identity type `:uid` it will return a UUID.
  For identity type `:email` it will return a lowercase string.
  For identity type `:phone` it will return a string."
  {:see-also ["->db" "to-db*"]}
  ([^Identifiable user-identity]
   (to-db* (p/make user-identity)))
  ([identity-type ^Identifiable user-identity]
   (to-db* (p/make user-identity (some-keyword identity-type)))))

(defmacro ->db
  "For the given user identity `user-identity` and optional identity type
  `identity-type` tries to express the given identity's value in a database suitable
  format.

  Uses `to-db*` multimethod to perform `user-identity` transformation on a basis of
  its identity type.

  If the given identity is not a kind of `ferment.Identity` record, it will be
  converted to it first. If the given value cannot be used as valid identity, `nil`
  is returned.

  If the `identity-type` is given, it should be a valid identity type. It instructs
  the function to treat the given identity as of this type during pre-conversion. If
  the identity is already an `ferment.Identity` record but its type is different,
  `nil` will be returned.

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  If the `identity-type` is given as literal keyword, string or `nil`, then a
  specific conversion function will be obtained at compile-time.

  If `identity-type` is an acceptable literal (or not given at all) and
  `user-identity` expression is a value for which the function
  `ferment.proto.identity/literal?` returns `true` then the conversion is done
  immediately, and its result replaces the macro call at compile-time.

  However, if the immediate conversion result is `nil`, or it is not a value for
  which the function `ferment.proto.identity/literal?` returns `true`, an
  expression with call to `to-db` or `to-db*` will be generated as fallback to
  perform the conversion at run-time."
  {:see-also ["->str" "of"]}
  ([user-identity]
   (if (nil? user-identity)
     nil
     (if-some [r (when (p/literal? user-identity)
                   (let [r (to-db `~user-identity)]
                     (when (p/literal? r) r)))]
       r
       `(to-db ~user-identity))))
  ([identity-type user-identity]
   (if (nil? user-identity)
     nil
     (let [identity-type (if (or (keyword? identity-type) (string? identity-type))
                           (some-keyword identity-type)
                           identity-type)]
       (if-some [r (when (and (or (nil? identity-type) (keyword? identity-type))
                              (p/literal? user-identity))
                     (let [r (to-db `~identity-type `~user-identity)]
                       (when (p/literal? r) r)))]
         r
         (or (if (nil? identity-type) (keyword? identity-type)
                 (if (identical? :ferment.identity/any identity-type)
                   (#'->db &form &env user-identity)
                   (when-some [f (get (methods to-db*) `~identity-type)]
                     `(~f (p/make ~user-identity ~identity-type)))))
             `(to-db ~identity-type ~user-identity)))))))

(defmethod to-db* :email
  (^String [^Identity user-identity]
   (p/value user-identity))
  (^String [^Keyword identity-type ^Identity user-identity]
   (p/value user-identity identity-type)))

(defmethod to-db* :phone
  (^Phonenumber$PhoneNumber [^Identity user-identity]
   (phone/format (p/value user-identity) nil
                 :phone-number.format/e164))
  (^Phonenumber$PhoneNumber [^Keyword identity-type ^Identity user-identity]
   (phone/format (p/value user-identity identity-type) nil
                 :phone-number.format/e164)))

(defmethod to-db* :uid
  (^UUID [^Identity user-identity]
   (p/value user-identity))
  (^UUID [^Keyword identity-type ^Identity user-identity]
   (p/value user-identity identity-type)))

(defmethod to-db* :id
  (^Long [^Identity user-identity]
   (p/value user-identity))
  (^Long [^Keyword identity-type ^Identity user-identity]
   (p/value user-identity identity-type)))

(defmethod to-db* :default
  (^Long [^Identity user-identity]
   (some-str (p/value user-identity)))
  (^Long [^Keyword identity-type ^Identity user-identity]
   (some-str (p/value user-identity identity-type))))

;; String conversions

(defmulti to-str*
  "Takes a user identity `user-identity` expressed as `Identity` record, and converts
  it to a string.

  If the `identity-type` is given, it should be a valid identity type expressed with
  a keyword. It instructs the function to treat the given identity as of this
  type. If the identity type is different than in the given object, `nil` is
  returned.

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  This is internal multimethod which does not perform conversions or checks. Use
  `to-str` function or `->str` macro instead."
  {:arglists '(^String [^Identity user-identity]
               ^String [^Keyword identity-type ^Identity user-identity])
   :see-also ["to-str" "->str"]}
  (fn
    (^Keyword [^Identity user-identity] (p/type user-identity))
    (^Keyword [^Keyword identity-type ^Identity _user-identity] identity-type))
  :hierarchy #'p/type-hierarchy)

(defn to-str
  "Takes a user identity `user-identity` and optional identity type `identity-type`,
  and converts it to a string.

  If the given identity is not a kind of `ferment.Identity` record it will be
  converted to it first. If the given value cannot be used as valid identity, `nil`
  is returned.

  If the `identity-type` is given, it should be a valid identity type. It instructs
  the function to treat the given identity as of this type.

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  If the identity is an `ferment.Identity` record but its type is different, `nil`
  will be returned."
  {:see-also ["->str" "->db" "of"]}
  (^String [^Identifiable user-identity]
   (to-str* (p/make user-identity)))
  (^String [identity-type ^Identifiable user-identity]
   (let [t (some-keyword identity-type)] (to-str* t (p/make user-identity t)))))

(defmacro ->str
  "For the given user identity `user-identity` and optional identity type
  `identity-type` tries to express the given identity's value as a string.

  Uses `to-str*` multimethod to perform `user-identity` transformation on a basis of
  its identity type.

  If the given identity is not a kind of `ferment.Identity` record, it will be
  converted to it first. If the given value cannot be used as valid identity, `nil`
  is returned.

  If the `identity-type` is given, it should be a valid identity type. It instructs
  the function to treat the given identity as of this type during pre-conversion. If
  the identity is already an `ferment.Identity` record but its type is different,
  `nil` will be returned.

  A value of `identity-type` may also be an acceptable type tag (registered with
  `add-acceptable-type!`) which groups identity types. In such case parsing and
  detection will be constrained to identity types being its descendants.

  If the `identity-type` is given as literal keyword, string or `nil`, then a
  specific conversion function will be obtained at compile-time.

  If `identity-type` is an acceptable literal (or not given at all) and
  `user-identity` expression is a value for which the function
  `ferment.proto.identity/literal?` returns `true` then the conversion is done
  immediately, and its result replaces the macro call at compile-time.

  However, if the immediate conversion result is `nil`, or it is not a value for
  which the function `ferment.proto.identity/literal?` returns `true`, an
  expression with call to `to-str` or `to-str*` will be generated as fallback to
  perform the conversion at run-time."
  {:see-also ["->str" "of"]}
  ([user-identity]
   (if (nil? user-identity)
     nil
     (if-some [r (when (p/literal? user-identity)
                   (let [r (to-str `~user-identity)]
                     (when (p/literal? r) r)))]
       r
       `(to-str ~user-identity))))
  ([identity-type user-identity]
   (if (nil? user-identity)
     nil
     (let [identity-type (if (or (keyword? identity-type) (string? identity-type))
                           (some-keyword identity-type)
                           identity-type)]
       (if-some [r (when (and (or (nil? identity-type) (keyword? identity-type))
                              (p/literal? user-identity))
                     (let [r (to-str `~identity-type `~user-identity)]
                       (when (p/literal? r) r)))]
         r
         (or (if (nil? identity-type) (keyword? identity-type)
                 (if (identical? :ferment.identity/any identity-type)
                   (#'->str &form &env user-identity)
                   (when-some [f (get (methods to-str*) `~identity-type)]
                     `(~f (p/make ~user-identity ~identity-type)))))
             `(to-str ~identity-type ~user-identity)))))))

(defmethod to-str* :email
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))

(defmethod to-str* :uid
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))

(defmethod to-str* :id
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))

(defmethod to-str* :phone
  (^String [^Identifiable user-identity]
   (phone/format (p/value user-identity) nil :phone-number.format/e164))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (phone/format (p/value user-identity identity-type) nil :phone-number.format/e164)))

(defmethod to-str* :default
  (^String [^Identifiable user-identity]
   (some-str (p/value user-identity)))
  (^String [^Keyword identity-type ^Identifiable user-identity]
   (some-str (p/value user-identity identity-type))))

;; Acceptable types / identity type groups support in detection and parsing

(defmethod parser ::p/group [^Keyword acceptable-tag]
  (fn ^Identity [^Identifiable user-identity]
    (parse-group acceptable-tag user-identity)))

(defn add-acceptable-type!
  "For the given parent tag `acceptable-tag` (which should be a qualified keyword) and
  an identity type `t`, creates a relation so that the identity type is a descendant
  of the given parent. It also ensures that the parent itself is a descendant of
  `:ferment.identity/valid` tag.

  Additionally, it makes an extra parent for the given acceptable tag in
  `ferment.proto.identity/type-hierarchy` which is
  `:ferment.proto.identity/group`. This special value is a valid dispatch for the
  `parser` multimethod which uses `parse-group` to handle identity parsing with
  acceptable tag given explicitly as its identity type specification. If you don't
  want that, use `ferment.proto.identity/add-acceptable-type!` directly.

  Useful when there is a need to accept a limited set of recognized identity
  types. Then the detection function can check whether an identity belongs to a
  parent.

  Makes changes in the global identity type hierarchy
  `ferment.identity.proto/type-hierarchy`."
  ([^Keyword acceptable-tag ^Keyword t]
   (p/add-subtype! ::p/group acceptable-tag)
   (p/add-acceptable-type! acceptable-tag t))
  ([^Keyword acceptable-tag ^Keyword t & more]
   (p/add-subtype! ::p/group acceptable-tag)
   (apply p/add-acceptable-type! acceptable-tag t more)))

(defn unaccept-type!
  "Removes identity type `t` from the given parent `acceptable-tag` and removes parent
  `:ferment.proto.identity/group` from the acceptable tag in the `ferment.proto.`
  hierarchy. Makes changes in the global identity type hierarchy
  `ferment.identity.proto/type-hierarchy`."
  ([^Keyword acceptable-tag ^Keyword t]
   (p/del-subtype! ::p/group acceptable-tag)
   (p/unaccept-type! acceptable-tag t))
  ([^Keyword acceptable-tag ^Keyword t & more]
   (p/del-subtype! ::p/group acceptable-tag)
   (apply p/unaccept-type! acceptable-tag t more)))
