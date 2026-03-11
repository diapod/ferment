(ns

    ^{:doc    "Provider dispatch for runtime HTTP model invocations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.providers.http

  (:require [clojure.string :as str]
            [ferment.providers.http.anthropic :as anthropic]
            [ferment.providers.http.openai :as openai]))

(def ^:private default-provider-id :openai-compatible)

(def ^:private known-provider-ids*
  #{:openai-compatible
    :anthropic-messages})

(defn known-provider-ids
  []
  known-provider-ids*)

(defn normalize-provider-id
  [v]
  (let [k (cond
            (keyword? v) v
            (string? v) (let [s (some-> v str str/trim not-empty)]
                          (when s
                            (if (str/starts-with? s ":")
                              (keyword (subs s 1))
                              (keyword s))))
            :else nil)
        k' (case k
             :openai-chat :openai-compatible
             :openai-completions :openai-compatible
             k)]
    (or k' default-provider-id)))

(defn- unsupported-provider!
  [provider-id]
  (throw
   (ex-info "Unsupported HTTP model provider."
             {:error :invoke-http-provider-unsupported
             :provider/id provider-id
             :known/providers (sort (map name known-provider-ids*))})))

(defn invoke-http-body
  [{:keys [provider/id] :as opts}]
  (let [provider-id' (normalize-provider-id id)]
    (case provider-id'
      :openai-compatible (openai/invoke-http-body opts)
      :anthropic-messages (anthropic/invoke-http-body opts)
      (unsupported-provider! provider-id'))))

(defn pick-response-text
  [{:keys [provider/id] :as opts}]
  (let [provider-id' (normalize-provider-id id)]
    (case provider-id'
      :openai-compatible (openai/pick-response-text opts)
      :anthropic-messages (anthropic/pick-response-text opts)
      (unsupported-provider! provider-id'))))
