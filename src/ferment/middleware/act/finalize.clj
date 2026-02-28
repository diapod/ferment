(ns

    ^{:doc    "ferment /v1/act middleware: finalize stage."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.middleware.act.finalize

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
   (middleware :act.middleware/finalize nil))
  ([k _config]
   (let [name (module-name k :act.middleware/finalize)]
     {:name name
      :compile (fn [_runtime _opts]
                 (fn [next]
                   (fn [{:keys [runtime] :as ctx}]
                     (let [fns            (if (map? (:act/fns ctx)) (:act/fns ctx) {})
                           finalize-phase (required-fn fns :finalize-phase name)
                           phase          (finalize-phase runtime ctx)
                           response       (or (:response/final phase)
                                              (:response phase))]
                       (next (cond-> phase
                               (map? response) (assoc :response response)))))))})))

(system/add-init  :ferment.http.act.middleware/finalize [k config] (middleware k config))
(system/add-halt! :ferment.http.act.middleware/finalize [_ _] nil)
