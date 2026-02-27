(ns

    ^{:doc    "Protocol configuration branch for Ferment (meta-language and envelope)."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.protocol

  (:require [clojure.string :as str]
            [ferment.system :as system]
            [io.randomseed.utils :as utils]))

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

(defn normalize-protocol
  "Applies lightweight defaults to protocol config."
  [config]
  (-> config
      (update :prompts normalize-prompts)
      (update :intents normalize-intent-systems)
      (update :proto/version #(or % 1))
      (update :transport/content-type #(or % :application/edn))
      (update :retry/max-attempts #(or % 3))))

(derive ::default :ferment.system/value)

(system/add-expand
 ::default
 [k config]
 {k (normalize-protocol config)})
