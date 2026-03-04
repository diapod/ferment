(ns

    ^{:doc    "Deterministic redaction pipeline for training events."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.training.redaction

  (:require [clojure.string :as str]))

(def ^:private default-placeholder
  "[REDACTED]")

(def ^:private default-deny-keys
  #{:password
    :secret
    :token
    :authorization
    :api-key
    :api/key
    :cookie
    :set-cookie
    :email
    :phone
    :phone-number})

(def ^:private default-deny-patterns
  ["(?i)bearer\\s+[a-z0-9._\\-]+"
   "(?i)api[_-]?key\\s*[:=]\\s*[^\\s,;]+"
   "(?i)[a-z0-9._%+\\-]+@[a-z0-9.\\-]+\\.[a-z]{2,}"
   "\\+?[0-9][0-9\\-\\s]{7,}[0-9]"])

(def ^:private default-config
  {:enabled? true
   :placeholder default-placeholder
   :deny/keys default-deny-keys
   :deny/paths []
   :deny/patterns default-deny-patterns})

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (trim-s v)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- ensure-regex
  [v]
  (cond
    (instance? java.util.regex.Pattern v) v
    (string? v) (try
                  (re-pattern v)
                  (catch Throwable _ nil))
    :else nil))

(defn normalize-config
  "Normalizes redaction config.

  Input keys:
  - `:enabled?`
  - `:placeholder`
  - `:deny/keys`
  - `:deny/paths`
  - `:deny/patterns`"
  [cfg]
  (let [cfg' (merge default-config (if (map? cfg) cfg {}))
        deny-keys (->> (or (:deny/keys cfg') [])
                       (keep keywordish)
                       set)
        deny-paths (->> (or (:deny/paths cfg') [])
                        (filter sequential?)
                        (mapv (fn [path]
                                (->> path
                                     (keep keywordish)
                                     vec)))
                        (filterv seq))
        deny-patterns (->> (or (:deny/patterns cfg') [])
                           (keep ensure-regex)
                           vec)]
    {:enabled? (true? (:enabled? cfg'))
     :placeholder (or (trim-s (:placeholder cfg')) default-placeholder)
     :deny/keys (if (seq deny-keys) deny-keys default-deny-keys)
     :deny/paths deny-paths
     :deny/patterns deny-patterns}))

(defn- redact-path
  [m path placeholder]
  (if (and (map? m) (seq path))
    (if (contains? m (first path))
      (if (next path)
        (assoc m (first path)
               (redact-path (get m (first path)) (next path) placeholder))
        (assoc m (first path) placeholder))
      m)
    m))

(defn- redact-by-paths
  [event deny-paths placeholder]
  (reduce (fn [{:keys [event count]} path]
            (if (and (seq path)
                     (map? event))
              (let [path-exists? (not= ::missing (get-in event path ::missing))
                    event' (if path-exists?
                             (redact-path event path placeholder)
                             event)]
                {:event event'
                 :count (if path-exists? (inc count) count)})
              {:event event :count count}))
          {:event event :count 0}
          deny-paths))

(defn- redact-string-patterns
  [s patterns placeholder]
  (reduce (fn [{:keys [value matches]} re]
            (if (re-find re value)
              {:value (str/replace value re placeholder)
               :matches (inc matches)}
              {:value value
               :matches matches}))
          {:value s :matches 0}
          patterns))

(defn- redact-value
  [v deny-keys patterns placeholder]
  (cond
    (map? v)
    (reduce-kv
     (fn [{:keys [out key-count pattern-count]} k item]
       (let [k' (keywordish k)]
         (if (and (keyword? k')
                  (contains? deny-keys k'))
           {:out (assoc out k placeholder)
            :key-count (inc key-count)
            :pattern-count pattern-count}
           (let [{child-value :out
                  child-key-count :key-count
                  child-pattern-count :pattern-count}
                 (redact-value item deny-keys patterns placeholder)]
             {:out (assoc out k child-value)
              :key-count (+ key-count child-key-count)
              :pattern-count (+ pattern-count child-pattern-count)}))))
     {:out (empty v) :key-count 0 :pattern-count 0}
     v)

    (vector? v)
    (reduce
     (fn [{:keys [out key-count pattern-count]} item]
       (let [{child-value :out
              child-key-count :key-count
              child-pattern-count :pattern-count}
             (redact-value item deny-keys patterns placeholder)]
         {:out (conj out child-value)
          :key-count (+ key-count child-key-count)
          :pattern-count (+ pattern-count child-pattern-count)}))
     {:out [] :key-count 0 :pattern-count 0}
     v)

    (sequential? v)
    (reduce
     (fn [{:keys [out key-count pattern-count]} item]
       (let [{child-value :out
              child-key-count :key-count
              child-pattern-count :pattern-count}
             (redact-value item deny-keys patterns placeholder)]
         {:out (conj out child-value)
          :key-count (+ key-count child-key-count)
          :pattern-count (+ pattern-count child-pattern-count)}))
     {:out (empty v) :key-count 0 :pattern-count 0}
     v)

    (string? v)
    (let [{value' :value matches :matches}
          (redact-string-patterns v patterns placeholder)]
      {:out value' :key-count 0 :pattern-count matches})

    :else
    {:out v :key-count 0 :pattern-count 0}))

(defn redact-event
  "Applies deterministic redaction policy to one training event.

  Returns:
  - `{:event <event'> :audit {...}}`."
  ([event]
   (redact-event event nil))
  ([event cfg]
   (let [{:keys [enabled? placeholder] :as cfg'}
         (normalize-config cfg)
         deny-keys (:deny/keys cfg')
         deny-paths (:deny/paths cfg')
         deny-patterns (:deny/patterns cfg')]
     (if (or (not enabled?) (not (map? event)))
       {:event event
        :audit {:enabled? false
                :config cfg'
                :redacted/paths 0
                :redacted/keys 0
                :redacted/patterns 0}}
       (let [{event1 :event path-count :count}
             (redact-by-paths event deny-paths placeholder)
             {event2 :out key-count :key-count pattern-count :pattern-count}
             (redact-value event1 deny-keys deny-patterns placeholder)]
         {:event event2
          :audit {:enabled? true
                  :config cfg'
                  :redacted/paths path-count
                  :redacted/keys key-count
                  :redacted/patterns pattern-count}})))))
