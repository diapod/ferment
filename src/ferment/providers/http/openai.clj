(ns

    ^{:doc    "OpenAI-compatible HTTP provider helpers for runtime model invoke."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.providers.http.openai

  (:require [clojure.string :as str]
            [ferment.providers.http.params :as params]))

(defn invoke-http-body
  [{:keys [request/format
           request-params
           prompt
           system
           model-id
           temperature
           max-tokens
           top-p]}]
  (let [base (case format
               :prompt-json
               (cond-> {:prompt prompt}
                 system (assoc :system system)
                 model-id (assoc :model model-id)
                 (number? temperature) (assoc :temperature temperature)
                 (number? max-tokens) (assoc :max_tokens max-tokens)
                 (number? top-p) (assoc :top_p top-p))

               :openai-completions
               (cond-> {:prompt prompt
                        :stream false}
                 model-id (assoc :model model-id)
                 (number? temperature) (assoc :temperature temperature)
                 (number? max-tokens) (assoc :max_tokens max-tokens)
                 (number? top-p) (assoc :top_p top-p))

               (let [messages (cond-> []
                                system (conj {:role "system"
                                              :content system})
                                true   (conj {:role "user"
                                              :content prompt}))]
                 (cond-> {:messages messages
                          :stream false}
                   model-id (assoc :model model-id)
                   (number? temperature) (assoc :temperature temperature)
                   (number? max-tokens) (assoc :max_tokens max-tokens)
                   (number? top-p) (assoc :top_p top-p))))]
    (params/merge-request-params base request-params)))

(defn pick-response-text
  [{:keys [response-map response/path]}]
  (let [from-path (when (and (vector? path) (seq path))
                    (get-in response-map path))
        candidates [(when (string? from-path) from-path)
                    (get-in response-map [:choices 0 :message :content])
                    (get-in response-map [:choices 0 :text])
                    (:text response-map)
                    (:response response-map)]]
    (some->> candidates
             (filter string?)
             (map #(some-> % str/trim not-empty))
             (filter some?)
             first)))
