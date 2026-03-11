(ns

    ^{:doc    "Data-first response field extraction for HTTP model providers."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.providers.http.response

  (:require [clojure.string :as str]))

(defn- mapping-path
  [spec]
  (cond
    (vector? spec)
    (when (every? #(or (keyword? %)
                       (string? %)
                       (symbol? %)
                       (integer? %))
                  spec)
      spec)

    (keyword? spec) [spec]
    (string? spec)  [spec]
    (map? spec)     (let [path (:path spec)]
                      (when (vector? path)
                        path))
    :else nil))

(defn- mapping-default
  [spec]
  (when (map? spec)
    (:default spec)))

(defn- mapping-transform
  [spec]
  (when (map? spec)
    (:transform spec)))

(defn- resolve-transform-fn
  [transform]
  (let [sym (cond
              (symbol? transform) transform
              (keyword? transform) (symbol (namespace transform) (name transform))
              (string? transform)  (some-> transform str/trim not-empty symbol)
              (vector? transform)  (let [head (first transform)]
                                     (cond
                                       (symbol? head) head
                                       (keyword? head) (symbol (namespace head) (name head))
                                       (string? head)  (some-> head str/trim not-empty symbol)
                                       :else nil))
              :else nil)]
    (when sym
      (requiring-resolve sym))))

(defn- apply-transform
  [transform value]
  (if-not (some? transform)
    value
    (if-some [f (resolve-transform-fn transform)]
      (if (vector? transform)
        (apply f (concat (rest transform) [value]))
        (f value))
      (throw (ex-info "Response field transform could not be resolved."
                      {:error :invoke-http-response-transform-unresolved
                       :transform transform})))))

(defn- extract-field-value
  [response-map spec]
  (let [source  (if-some [path (mapping-path spec)]
                  (get-in response-map path)
                  response-map)
        value   (if (and (nil? source)
                         (map? spec)
                         (contains? spec :default))
                  (mapping-default spec)
                  source)
        result  (apply-transform (mapping-transform spec) value)]
    (when-not (nil? result)
      result)))

(defn extract-response-fields
  "Extracts mapped response fields from `response-map`.

  Mapping value forms:
  - `[:a :b 0 :c]` => `get-in` path
  - `{:path [...], :default x, :transform symbol-or-vector}`

  When `:transform` is a vector, its first element is the function symbol and the
  remaining items are default leading arguments. Extracted value is appended as the
  last argument."
  [response-map field-mapping]
  (if (and (map? response-map)
           (map? field-mapping))
    (reduce-kv (fn [acc out-k spec]
                 (let [v (extract-field-value response-map spec)]
                   (if (nil? v)
                     acc
                     (assoc acc out-k v))))
               {}
               field-mapping)
    {}))
