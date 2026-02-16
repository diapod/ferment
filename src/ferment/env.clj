(ns

    ^{:doc    "Environment configuration branch of ferment."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.env

  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ferment :refer :all]
            [ferment.system :as system]))

(def ^:private hf-path-keys
  #{:ferment.env/hf.home
    :ferment.env/hf.hub.cache})

(defn project-root
  "Best-effort project root detection based on `deps.edn`/`.git` from `user.dir`."
  []
  (let [start (some-> (System/getProperty "user.dir") io/file .getCanonicalFile)]
    (loop [dir start]
      (if (nil? dir)
        (or (some-> start .getPath)
            ".")
        (let [deps? (.exists (io/file dir "deps.edn"))
              git?  (.exists (io/file dir ".git"))]
          (if (or deps? git?)
            (.getPath dir)
            (recur (.getParentFile dir))))))))

(defn- relative-path-string?
  [v]
  (and (string? v)
       (not (str/blank? v))
       (not (.isAbsolute (io/file v)))))

(defn preconfigure-env-value
  "Pre-configuration hook for env keys.

  For HF cache keys loaded from `.env`, converts relative paths to absolute
  paths by prefixing project root."
  [k config]
  (if (and (contains? hf-path-keys k)
           (relative-path-string? config))
    (.getPath (io/file (project-root) config))
    config))

(system/add-expand ::default [k config] {k (preconfigure-env-value k config)})
(system/add-init   ::default [_ config] config)
(system/add-halt!  ::default [_ _]      nil)
