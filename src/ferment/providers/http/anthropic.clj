(ns

    ^{:doc    "Anthropic Messages API HTTP provider helpers for runtime model invoke."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.providers.http.anthropic

  (:require [clojure.string :as str]
            [ferment.providers.http.params :as params]))

(def ^:private default-max-tokens 512)

(defn content-text
  [content]
  (when (sequential? content)
    (some->> content
             (keep (fn [block]
                     (when (and (map? block)
                                (= "text" (:type block))
                                (string? (:text block)))
                       (some-> (:text block) str/trim not-empty))))
             seq
             (str/join "\n\n"))))

(defn content-types
  [content]
  (when (sequential? content)
    (some->> content
             (keep (fn [block]
                     (when (map? block)
                       (some-> (:type block) str not-empty))))
             seq
             vec)))

(defn text-block-count
  [content]
  (if (sequential? content)
    (reduce (fn [n block]
              (if (and (map? block)
                       (= "text" (:type block))
                       (string? (:text block)))
                (inc n)
                n))
            0
            content)
    0))

(defn- header-value
  [headers k]
  (when (map? headers)
    (some-> (or (get headers k)
                (get headers (str/lower-case k))
                (get headers (str/upper-case k)))
            str
            str/trim
            not-empty)))

(defn- parse-longish
  [v]
  (when (string? v)
    (try
      (Long/parseLong v)
      (catch Throwable _
        nil))))

(defn response-rate-limit
  [headers]
  (let [mappings {:retry-after              "retry-after"
                  :requests-limit           "anthropic-ratelimit-requests-limit"
                  :requests-remaining       "anthropic-ratelimit-requests-remaining"
                  :requests-reset           "anthropic-ratelimit-requests-reset"
                  :tokens-limit             "anthropic-ratelimit-tokens-limit"
                  :tokens-remaining         "anthropic-ratelimit-tokens-remaining"
                  :tokens-reset             "anthropic-ratelimit-tokens-reset"
                  :input-tokens-limit       "anthropic-ratelimit-input-tokens-limit"
                  :input-tokens-remaining   "anthropic-ratelimit-input-tokens-remaining"
                  :input-tokens-reset       "anthropic-ratelimit-input-tokens-reset"
                  :output-tokens-limit      "anthropic-ratelimit-output-tokens-limit"
                  :output-tokens-remaining  "anthropic-ratelimit-output-tokens-remaining"
                  :output-tokens-reset      "anthropic-ratelimit-output-tokens-reset"}]
    (reduce-kv
     (fn [acc out-k header-k]
       (let [raw (header-value headers header-k)
             parsed (or (parse-longish raw) raw)]
         (if (nil? parsed)
           acc
           (assoc acc out-k parsed))))
     {}
     mappings)))

(defn invoke-http-body
  [{:keys [request-params
           prompt
           system
           model-id
           temperature
           max-tokens
           top-p]}]
  (let [limit (if (and (number? max-tokens) (pos? (long max-tokens)))
                (long max-tokens)
                default-max-tokens)
        base (cond-> {:messages [{:role "user"
                                  :content prompt}]
                      :max_tokens limit}
               system (assoc :system system)
               model-id (assoc :model model-id)
               (number? temperature) (assoc :temperature temperature)
               (number? top-p) (assoc :top_p top-p))]
    (params/merge-request-params base request-params)))

(defn pick-response-text
  [{:keys [response-map response/path]}]
  (let [from-path (when (and (vector? path) (seq path))
                    (get-in response-map path))
        from-content (content-text (:content response-map))
        candidates [(when (string? from-path) from-path)
                    from-content
                    (get-in response-map [:content 0 :text])
                    (:completion response-map)
                    (:text response-map)
                    (:response response-map)]]
    (some->> candidates
             (filter string?)
             (map #(some-> % str/trim not-empty))
             (filter some?)
             first)))
