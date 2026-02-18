(ns

    ^{:doc    "ferment service, lazy request map middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.middleware.lazy-req

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string          :as        str]
            [io.randomseed.lazy-map  :as   lazy-map]
            [ferment.logging       :as        log]
            [ferment.system        :as     system]))

;; Configuration initializers

(defn wrap
  "Lazy request map middleware."
  [{:keys [enabled?]
    :or   {enabled? true}}]
  (when enabled?
    (log/msg "Initializing lazy request map middleware")
    {:name    ::lazy-request
     :compile (fn [_ _]
                (fn [h]
                  (fn [req]
                    (h (lazy-map/->LazyMap req)))))}))

(system/add-init  ::default [_ config] (wrap config))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)
