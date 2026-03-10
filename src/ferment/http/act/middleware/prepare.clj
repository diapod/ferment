(ns

    ^{:doc    "ferment /v1/act middleware: request preparation stage."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.http.act.middleware.prepare

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

(defn- optional-fn
  [fns k]
  (let [f (get fns k)]
    (when (fn? f)
      f)))

(defn middleware
  ([]
   (middleware nil nil))
  ([k _config]
   (let [name (module-name k ::prepare)]
     {:name name
      :compile (fn [_runtime _opts]
                 (fn [next]
                   (fn [{:keys [runtime payload auth] :as ctx}]
                     (let [fns              (if (map? (:act/fns ctx)) (:act/fns ctx) {})
                           prepare-request  (required-fn fns :prepare-request name)
                           accepted-mode?*  (required-fn fns :accepted-mode? name)
                           resolver*        (required-fn fns :effective-resolver name)
                           select-runtime   (optional-fn fns :select-runtime)
                           request          (if (contains? ctx :request)
                                              (:request ctx)
                                              (prepare-request runtime payload auth))
                           runtime'         (if (fn? select-runtime)
                                              (select-runtime runtime request)
                                              runtime)
                           accepted-mode?   (if (contains? ctx :accepted-mode?)
                                              (:accepted-mode? ctx)
                                              (and (map? request)
                                                   (accepted-mode?* request)))
                           protocol         (if (contains? ctx :protocol)
                                              (:protocol ctx)
                                              (or (:protocol runtime') {}))
                           resolver         (if (contains? ctx :resolver)
                                              (:resolver ctx)
                                              (resolver* runtime'))]
                       (next (merge ctx
                                    {:runtime runtime'}
                                    {:request request
                                     :accepted-mode? accepted-mode?
                                     :protocol protocol
                                     :resolver resolver}))))))})))

(system/add-init  :ferment.http.act.middleware/prepare [k config] (middleware k config))
(system/add-halt! :ferment.http.act.middleware/prepare [_ _] nil)
