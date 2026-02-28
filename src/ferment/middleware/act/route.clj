(ns

    ^{:doc    "ferment /v1/act middleware: routing stage."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.middleware.act.route

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
   (middleware :act.middleware/route nil))
  ([k _config]
   (let [name (module-name k :act.middleware/route)]
     {:name name
      :compile (fn [_runtime _opts]
                 (fn [next]
                   (fn [{:keys [runtime request accepted-mode? protocol resolver] :as ctx}]
                     (let [fns               (if (map? (:act/fns ctx)) (:act/fns ctx) {})
                           prepare-request   (required-fn fns :prepare-request name)
                           accepted-mode?*   (required-fn fns :accepted-mode? name)
                           resolver*         (required-fn fns :effective-resolver name)
                           route-phase       (required-fn fns :route-phase name)
                           request'          (if (contains? ctx :request)
                                               request
                                               (prepare-request runtime
                                                                (:payload ctx)
                                                                (:auth ctx)))
                           accepted-mode?'   (if (contains? ctx :accepted-mode?)
                                               accepted-mode?
                                               (and (map? request')
                                                    (accepted-mode?* request')))
                           protocol'         (or protocol (:protocol runtime) {})
                           resolver'         (or resolver (resolver* runtime))]
                       (next (merge ctx
                                    (route-phase runtime
                                                 request'
                                                 accepted-mode?'
                                                 protocol'
                                                 resolver')))))))})))

(system/add-init  :ferment.http.act.middleware/route [k config] (middleware k config))
(system/add-halt! :ferment.http.act.middleware/route [_ _] nil)
