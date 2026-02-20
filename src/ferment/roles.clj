(ns

    ^{:doc    "Role policy configuration and authorization helpers."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.roles

  (:require [clojure.set :as set]
            [clojure.string :as str]
            [ferment.system :as system]))

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v)
    (let [s (trim-s v)]
      (when s
        (if (str/starts-with? s ":")
          (keyword (subs s 1))
          (keyword s))))
    :else nil))

(defn- keyword-set
  [v]
  (cond
    (set? v) (into #{} (keep keywordish) v)
    (sequential? v) (into #{} (keep keywordish) v)
    (some? v) (if-some [k (keywordish v)] #{k} #{})
    :else #{}))

(defn- normalize-account-type-roles
  [v]
  (if (map? v)
    (reduce-kv
     (fn [m k roles]
       (if-some [k' (keywordish k)]
         (assoc m k' (keyword-set roles))
         m))
     {}
     v)
    {}))

(defn- normalize-operation-rules
  [v]
  (if (map? v)
    (reduce-kv
     (fn [m operation rule]
       (if-some [op' (keywordish operation)]
         (let [rule' (if (map? rule) rule {})
               any' (keyword-set (:any rule'))
               all' (keyword-set (:all rule'))
               forbidden' (keyword-set (:forbidden rule'))]
           (assoc m op'
                  {:any any'
                   :all all'
                   :forbidden forbidden'}))
         m))
     {}
     v)
    {}))

(defn- normalize-effect-rules
  [v]
  (normalize-operation-rules v))

(declare roles-for-user)

(defn- allowed-by-rule?
  [rule user-roles authorize-def?]
  (cond
    (not (map? rule)) authorize-def?
    (seq (set/intersection user-roles (:forbidden rule))) false

    (seq (:any rule))
    (boolean
     (or (seq (set/intersection user-roles (:any rule)))
         (and (seq (:all rule))
              (set/subset? (:all rule) user-roles))
         false))

    (seq (:all rule))
    (set/subset? (:all rule) user-roles)

    :else authorize-def?))

(defn- allowed-on-rules?
  [cfg rules operation user]
  (let [enabled?       (true? (:enabled? cfg))
        authorize-def? (boolean (:authorize-default? cfg))
        operation'     (keywordish operation)
        rule           (when operation'
                         (get rules operation'))
        user-roles     (roles-for-user cfg user)]
    (cond
      (not enabled?) true
      (nil? operation') authorize-def?
      :else (allowed-by-rule? rule user-roles authorize-def?))))

(defn preconfigure-roles
  "Normalizes roles configuration used by authorization checks."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (-> cfg
        (update :enabled? #(if (nil? %) true (boolean %)))
        (update :authorize-default? #(if (nil? %) true (boolean %)))
        (update :account-type->roles normalize-account-type-roles)
        (update :operations normalize-operation-rules)
        (update :effects normalize-effect-rules))))

(defn init-roles
  "Initialization hook for roles branch."
  [_k config]
  (preconfigure-roles _k config))

(defn stop-roles
  "Stop hook for roles branch."
  [_k _state]
  nil)

(defn roles-for-user
  "Returns effective roles for a user map according to roles configuration."
  [roles-config user]
  (let [cfg              (preconfigure-roles nil roles-config)
        user'            (if (map? user) user {})
        user-present?    (boolean (or (some? (:user/id user'))
                                      (some? (:user/email user'))))
        account-type     (or (keywordish (:user/account-type user'))
                             (keywordish (:account-type user')))
        mapped-roles     (if-some [roles (get-in cfg [:account-type->roles account-type])]
                           roles
                           #{})
        explicit-roles   (set/union (keyword-set (:user/roles user'))
                                    (keyword-set (:roles user')))
        logged-in-role   (if user-present?
                           (keyword-set (:logged-in-role cfg))
                           #{})
        anonymous-role   (if user-present?
                           #{}
                           (keyword-set (:anonymous-role cfg)))]
    (set/union mapped-roles explicit-roles logged-in-role anonymous-role)))

(defn allowed?
  "Returns true when `user` is authorized to perform `operation`."
  ([roles-config operation]
   (allowed? roles-config operation nil))
  ([roles-config operation user]
   (let [cfg (preconfigure-roles nil roles-config)]
     (allowed-on-rules? cfg (:operations cfg) operation user))))

(defn effect-allowed?
  "Returns true when `user` is authorized to request a specific effect keyword."
  ([roles-config effect]
   (effect-allowed? roles-config effect nil))
  ([roles-config effect user]
   (let [cfg (preconfigure-roles nil roles-config)]
     (allowed-on-rules? cfg (:effects cfg) effect user))))

(defn denied-effects
  "Returns set of effect keywords denied for `user` according to effect policy."
  [roles-config effects user]
  (let [effects' (keyword-set effects)]
    (if (empty? effects')
      #{}
      (let [cfg (preconfigure-roles nil roles-config)]
        (->> effects'
             (remove #(allowed-on-rules? cfg (:effects cfg) % user))
             set)))))

(defn effects-allowed?
  "Returns true when all requested effect keywords are authorized for `user`."
  ([roles-config effects]
   (effects-allowed? roles-config effects nil))
  ([roles-config effects user]
   (empty? (denied-effects roles-config effects user))))

(defn authorize-effects
  "Returns authorization result for requested effect keywords."
  [roles-config effects user]
  (let [cfg      (preconfigure-roles nil roles-config)
        roles    (roles-for-user cfg user)
        denied   (denied-effects cfg effects user)
        effects' (keyword-set effects)]
    (if (empty? denied)
      {:ok? true
       :effects effects'
       :roles roles}
      {:ok? false
       :error :auth/forbidden-effect
       :effects effects'
       :denied denied
       :roles roles})))

(defn authorize
  "Returns authorization result map for operation and user."
  [roles-config operation user]
  (let [ok?  (allowed? roles-config operation user)
        roles (roles-for-user roles-config user)]
    (if ok?
      {:ok? true
       :operation operation
       :roles roles}
      {:ok? false
       :error :auth/forbidden
       :operation operation
       :roles roles})))

(derive ::service :ferment.system/value)
(derive :ferment.roles/default ::service)

(system/add-expand ::service [k config] {k (preconfigure-roles k config)})
(system/add-init   ::service [k config]    (init-roles k config))
(system/add-halt!  ::service [k state]     (stop-roles k state))
