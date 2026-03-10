(ns

    ^{:doc    "Protocol configuration branch for Ferment (meta-language and envelope)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.protocol

  (:require [clojure.string :as str]
            [ferment.system :as system]
            [io.randomseed.utils :as utils]))

(declare normalize-protocol)

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (some-> v str str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- prompt-text
  [s]
  (cond
    (vector? s) (str/join " " s)
    (list? s) (str/join " " s)
    :else (utils/some-str s)))

(defn- normalize-prompts
  [prompts]
  (if-not (map? prompts)
    prompts
    (cond-> prompts
      (contains? prompts :default)
      (update :default prompt-text)

      (map? (:roles prompts))
      (update :roles (fn [roles]
                       (reduce-kv (fn [acc role prompt]
                                    (assoc acc role (prompt-text prompt)))
                                  {}
                                  roles))))))

(defn- normalize-intent-systems
  [intents]
  (if-not (map? intents)
    intents
               (reduce-kv (fn [acc intent cfg]
                 (if (and (map? cfg) (contains? cfg :system))
                   (assoc acc intent (update cfg :system prompt-text))
                   (assoc acc intent cfg)))
               {}
               intents)))

(defn- deep-merge
  [a b]
  (cond
    (and (map? a) (map? b)) (merge-with deep-merge a b)
    (nil? b) a
    :else b))

(defn- normalize-rollout
  [rollout]
  (if-not (map? rollout)
    rollout
    (let [canary (if (map? (:canary rollout))
                   (:canary rollout)
                   {})
          shadow (if (map? (:shadow rollout))
                   (:shadow rollout)
                   {})
          percent (cond
                    (int? (:percent canary)) (:percent canary)
                    (integer? (:percent canary)) (int (:percent canary))
                    :else nil)
          shadow-percent (cond
                           (int? (:percent shadow)) (:percent shadow)
                           (integer? (:percent shadow)) (int (:percent shadow))
                           :else nil)]
      (cond-> rollout
        (keyword? (keywordish (:active rollout)))
        (assoc :active (keywordish (:active rollout)))

        (map? (:canary rollout))
        (assoc :canary
               (cond-> canary
                 (contains? canary :enabled?) (update :enabled? boolean)
                 (keyword? (keywordish (:version canary)))
                 (assoc :version (keywordish (:version canary)))
                 (int? percent) (assoc :percent (max 0 (min 100 percent)))))

        (map? (:shadow rollout))
        (assoc :shadow
               (cond-> shadow
                 (contains? shadow :enabled?) (update :enabled? boolean)
                 (keyword? (keywordish (:version shadow)))
                 (assoc :version (keywordish (:version shadow)))
                 (int? shadow-percent) (assoc :percent (max 0 (min 100 shadow-percent)))))))))

(defn- normalize-version-catalog
  [versions]
  (if-not (map? versions)
    versions
    (reduce-kv (fn [acc k v]
                 (let [k' (keywordish k)
                       v' (if (map? v)
                            (normalize-protocol v)
                            v)]
                   (if (keyword? k')
                     (assoc acc k' v')
                     acc)))
               {}
               versions)))

(defn- trace-bucket
  [trace-id]
  (when-some [sid (some-> trace-id str not-empty)]
    (mod (bit-and (hash sid) 0x7fffffff) 100)))

(defn select-protocol-artifact
  "Selects protocol artifact/version for a request.

  Input:
  - `protocol`: normalized protocol map, optionally containing `:versions` and `:rollout`.
  - `opts`: `{:trace-id string? :requested-version keyword|string?}`.

  Output map:
  - `:protocol`: selected protocol map (base merged with selected version patch when catalog exists),
  - `:artifact/version`: selected version keyword,
  - `:artifact/source`: one of `:request`, `:canary`, `:active`, `:default`."
  [protocol {:keys [trace-id requested-version]}]
  (let [base-protocol (if (map? protocol) protocol {})
        versions (if (map? (:versions base-protocol))
                   (:versions base-protocol)
                   {})
        rollout (if (map? (:rollout base-protocol))
                  (:rollout base-protocol)
                  {})
        request-version (keywordish requested-version)
        active-version (or (keywordish (:active rollout))
                           (keywordish (:artifact/version base-protocol)))
        canary-cfg (if (map? (:canary rollout))
                     (:canary rollout)
                     {})
        canary-enabled? (true? (:enabled? canary-cfg))
        canary-version (keywordish (:version canary-cfg))
        canary-percent (let [n (:percent canary-cfg)]
                         (if (integer? n) (max 0 (min 100 (int n))) 0))
        bucket (trace-bucket trace-id)
        canary-hit? (and canary-enabled?
                         (keyword? canary-version)
                         (contains? versions canary-version)
                         (int? bucket)
                         (< bucket canary-percent))
        selected-version (cond
                           (and (keyword? request-version)
                                (contains? versions request-version))
                           request-version

                           canary-hit?
                           canary-version

                           (and (keyword? active-version)
                                (contains? versions active-version))
                           active-version

                           (seq versions)
                           (first (sort-by name (keys versions)))

                           :else
                           (or (keywordish (:artifact/version base-protocol))
                               :default))
        selected-source (cond
                          (and (keyword? request-version)
                               (= request-version selected-version))
                          :request

                          (and canary-hit?
                               (= canary-version selected-version))
                          :canary

                          (and (keyword? active-version)
                               (= active-version selected-version))
                          :active

                          :else
                          :default)
        selected-patch (when (map? versions)
                         (get versions selected-version))
        selected-protocol (if (map? selected-patch)
                            (-> (dissoc base-protocol :versions :rollout :artifact/version)
                                (deep-merge selected-patch)
                                (assoc :artifact/version selected-version))
                            (assoc (dissoc base-protocol :versions :rollout)
                                   :artifact/version selected-version))]
    {:protocol selected-protocol
     :artifact/version selected-version
     :artifact/source selected-source}))

(defn select-protocol-shadow-artifact
  "Selects optional shadow protocol artifact/version for side-by-side evaluation.

  Output:
  - `:protocol`: selected shadow protocol map or nil,
  - `:artifact/version`: selected version keyword or nil,
  - `:artifact/source`: one of `:request`, `:shadow`, `:disabled`,
  - `:shadow/enabled?`: shadow is enabled by config or explicit request override,
  - `:shadow/applied?`: concrete shadow artifact was selected."
  [protocol {:keys [trace-id requested-version]}]
  (let [base-protocol (if (map? protocol) protocol {})
        versions (if (map? (:versions base-protocol))
                   (:versions base-protocol)
                   {})
        rollout (if (map? (:rollout base-protocol))
                  (:rollout base-protocol)
                  {})
        requested-version' (keywordish requested-version)
        shadow-cfg (if (map? (:shadow rollout))
                     (:shadow rollout)
                     {})
        shadow-enabled? (true? (:enabled? shadow-cfg))
        shadow-version (keywordish (:version shadow-cfg))
        shadow-percent (let [n (:percent shadow-cfg)]
                         (if (integer? n) (max 0 (min 100 (int n))) 0))
        bucket (trace-bucket trace-id)
        shadow-hit? (and shadow-enabled?
                         (keyword? shadow-version)
                         (contains? versions shadow-version)
                         (int? bucket)
                         (< bucket shadow-percent))
        [selected-version selected-source]
        (cond
          (and (keyword? requested-version')
               (contains? versions requested-version'))
          [requested-version' :request]

          shadow-hit?
          [shadow-version :shadow]

          :else
          [nil :disabled])
        selected-patch (when (keyword? selected-version)
                         (get versions selected-version))
        selected-protocol (when (keyword? selected-version)
                            (if (map? selected-patch)
                              (-> (dissoc base-protocol :versions :rollout :artifact/version)
                                  (deep-merge selected-patch)
                                  (assoc :artifact/version selected-version))
                              (assoc (dissoc base-protocol :versions :rollout)
                                     :artifact/version selected-version)))]
    {:protocol selected-protocol
     :artifact/version selected-version
     :artifact/source selected-source
     :shadow/enabled? (or shadow-enabled? (keyword? requested-version'))
     :shadow/applied? (keyword? selected-version)}))

(defn normalize-protocol
  "Applies lightweight defaults to protocol config."
  [config]
  (-> config
      (update :prompts normalize-prompts)
      (update :intents normalize-intent-systems)
      (update :versions normalize-version-catalog)
      (update :rollout normalize-rollout)
      (update :proto/version #(or % 1))
      (update :transport/content-type #(or % :application/edn))
      (update :retry/max-attempts #(or % 3))))

(derive ::default :ferment.system/value)

(system/add-expand
 ::default
 [k config]
 {k (normalize-protocol config)})
