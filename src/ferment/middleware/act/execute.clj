(ns

    ^{:doc    "ferment /v1/act middleware: execution stage."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.middleware.act.execute

  (:require [ferment.system :as system]))

(defn- module-name
  [k default]
  (cond
    (keyword? k) k
    (string? k)  (keyword k)
    :else        default))

(defn- required-fn
  [fns k middleware-k]
  (let [f (get fns k)]
    (when-not (fn? f)
      (throw (ex-info "Act middleware is missing required function in :act/fns."
                      {:error :act/middleware-missing-fn
                       :middleware middleware-k
                       :required k})))
    f))

(defn middleware
  ([]
   (middleware :act.middleware/execute nil))
  ([k _config]
   (let [name (module-name k :act.middleware/execute)]
     {:name name
      :compile (fn [_runtime _opts]
                 (fn [next]
                   (fn [{:keys [runtime] :as ctx}]
                     (let [fns           (if (map? (:act/fns ctx)) (:act/fns ctx) {})
                           execute-phase (required-fn fns :execute-phase name)]
                       (next (execute-phase runtime ctx))))))})))

(system/add-init  :ferment.http.act.middleware/execute [k config] (middleware k config))
(system/add-halt! :ferment.http.act.middleware/execute [_ _] nil)
