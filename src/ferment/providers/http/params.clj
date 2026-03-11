(ns

    ^{:doc    "Helpers for provider HTTP request parameter normalization."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.providers.http.params

  (:require [clojure.string :as str]))

(defn- normalize-param-key
  [k]
  (cond
    (keyword? k) k
    (symbol? k) (keyword (name k))
    (string? k) (let [s (some-> k str/trim not-empty)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else k))

(defn normalize-params
  "Recursively normalizes request params map keys to keywords when possible."
  [v]
  (cond
    (map? v)
    (reduce-kv (fn [acc k val]
                 (assoc acc (normalize-param-key k) (normalize-params val)))
               {}
               v)

    (vector? v) (mapv normalize-params v)
    (sequential? v) (map normalize-params v)
    :else v))

(defn merge-request-params
  "Merges user-provided `params` over provider `base` body map."
  [base params]
  (if (map? params)
    (merge base (normalize-params params))
    base))

(defn stringify-keys
  "Recursively converts map keys to strings for HTTP JSON payload."
  [v]
  (cond
    (map? v)
    (reduce-kv (fn [acc k val]
                 (let [ks (cond
                            (string? k) (some-> k str/trim not-empty)
                            (keyword? k) (name k)
                            (symbol? k) (name k)
                            :else (some-> k str str/trim not-empty))]
                   (if ks
                     (assoc acc ks (stringify-keys val))
                     acc)))
               {}
               v)

    (vector? v) (mapv stringify-keys v)
    (sequential? v) (map stringify-keys v)
    :else v))
