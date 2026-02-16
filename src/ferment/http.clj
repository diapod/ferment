(ns

    ^{:doc    "HTTP bridge for model runtimes exposed via bot command channel."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.http

  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [cheshire.core :as json]
            [ferment.model :as model]
            [ferment.system :as system])

  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io OutputStream)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)))

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- parse-port
  [v]
  (let [port (cond
               (integer? v) v
               (string? v) (try
                             (Long/parseLong (str/trim v))
                             (catch Throwable _ nil))
               :else nil)]
    (if (and (integer? port) (<= 1 port 65535))
      (int port)
      12002)))

(defn- normalize-endpoint
  [v]
  (when-some [endpoint (trim-s v)]
    (if (str/starts-with? endpoint "/")
      endpoint
      (str "/" endpoint))))

(defn- runtime-config
  [runtime]
  (cond
    (and (map? runtime) (map? (:config runtime))) (:config runtime)
    (map? runtime) runtime
    :else nil))

(defn- runtime-worker
  [runtime]
  (when (map? runtime)
    (:worker runtime)))

(defn model-http-routes
  "Builds endpoint routing table from initialized `:models` map.

  Output shape:
  {\"/solver/responses\" {:endpoint \"/solver/responses\"
                         :model    :ferment.model/solver
                         :worker   <bot-worker>
                         :worker-id :ferment.model.runtime/solver}}"
  [models]
  (reduce-kv
   (fn [acc model-k model-entry]
     (let [runtime      (when (map? model-entry) (:runtime model-entry))
           config       (runtime-config runtime)
           http-config  (when (map? config) (:http config))
           enabled?     (and (map? http-config) (:enabled? http-config))
           endpoint     (normalize-endpoint (:endpoint http-config))
           worker       (runtime-worker runtime)
           worker-id    (or (some-> runtime :id) model-k)]
       (if (and enabled? endpoint worker)
         (do
           (when (contains? acc endpoint)
             (throw (ex-info "Duplicate HTTP endpoint in model runtime config."
                             {:endpoint endpoint
                              :model model-k
                              :existing (get-in acc [endpoint :model])})))
           (assoc acc endpoint {:endpoint endpoint
                                :model model-k
                                :worker worker
                                :worker-id worker-id}))
         acc)))
   {}
   (or models {})))

(defn- read-body
  [^HttpExchange exchange]
  (with-open [in (.getRequestBody exchange)]
    (slurp in :encoding "UTF-8")))

(defn- content-type
  [^HttpExchange exchange]
  (some-> (.getRequestHeaders exchange)
          (.getFirst "Content-Type")
          trim-s
          str/lower-case))

(defn- decode-request-body
  [^String body ctype]
  (let [body (or body "")
        body' (str/trim body)]
    (cond
      (str/blank? body') {}
      (and ctype (str/includes? ctype "application/json"))
      (json/parse-string body' true)

      (and ctype (str/includes? ctype "application/edn"))
      (edn/read-string body')

      :else {:prompt body})))

(defn- encode-response
  [data]
  (json/generate-string
   (cond
     (map? data) data
     (string? data) {:ok? true :result {:text data}}
     :else {:ok? true :result data})))

(defn- write-response!
  [^HttpExchange exchange status ^String body]
  (let [bytes (.getBytes body StandardCharsets/UTF_8)
        headers (.getResponseHeaders exchange)]
    (.set headers "Content-Type" "application/json; charset=utf-8")
    (.sendResponseHeaders exchange (long status) (long (alength bytes)))
    (with-open [^OutputStream out (.getResponseBody exchange)]
      (.write out bytes))))

(defn- safe-invoke!
  [route payload]
  (try
    (let [worker (:worker route)]
      (if-not worker
        {:ok? false
         :error :runtime-worker-missing
         :model (:model route)}
        (or (model/command-bot-worker! worker :invoke payload)
            {:ok? false
             :error :empty-response
             :model (:model route)})))
    (catch Throwable t
      {:ok? false
       :error :invoke-exception
       :message (.getMessage t)
       :class (str (class t))
       :model (:model route)})))

(defn- invoke-handler
  [route]
  (reify HttpHandler
    (handle [_ exchange]
      (let [method (some-> (.getRequestMethod exchange) str/upper-case)]
        (if (not= "POST" method)
          (write-response! exchange 405 (encode-response {:ok? false
                                                          :error :method-not-allowed
                                                          :allowed ["POST"]}))
          (let [ctype   (content-type exchange)
                body    (read-body exchange)
                payload (decode-request-body body ctype)
                result  (safe-invoke! route payload)
                status  (if (= false (:ok? result)) 502 200)]
            (write-response! exchange status (encode-response result))))))))

(defn- health-handler
  [route-count]
  (reify HttpHandler
    (handle [_ exchange]
      (write-response! exchange 200 (encode-response {:ok? true
                                                      :service :ferment.http
                                                      :routes route-count})))))

(defn- routes-handler
  [routes]
  (let [public-routes
        (into {}
              (map (fn [[endpoint {:keys [model worker-id]}]]
                     [endpoint {:model model :worker-id worker-id}]))
              routes)]
    (reify HttpHandler
      (handle [_ exchange]
        (write-response! exchange 200 (encode-response {:ok? true
                                                        :routes public-routes}))))))

(defn preconfigure-http
  "Pre-configuration hook for HTTP bridge."
  [_k config]
  (let [cfg (if (map? config) config {})]
    (if (contains? cfg :models)
      cfg
      (assoc cfg :models (system/ref :ferment/models)))))

(defn init-http
  "Initializes HTTP bridge for model runtime workers."
  [_k config]
  (let [cfg      (preconfigure-http _k config)
        host     (or (trim-s (:host cfg)) "127.0.0.1")
        port     (parse-port (:port cfg))
        routes   (model-http-routes (:models cfg))
        server   (HttpServer/create (InetSocketAddress. ^String host (int port)) 0)]
    (doseq [[endpoint route] routes]
      (.createContext server endpoint (invoke-handler route)))
    (.createContext server "/health" (health-handler (count routes)))
    (.createContext server "/routes" (routes-handler routes))
    (.setExecutor server nil)
    (.start server)
    {:host host
     :port port
     :server server
     :routes (into {}
                   (map (fn [[endpoint {:keys [model worker-id]}]]
                          [endpoint {:model model :worker-id worker-id}]))
                   routes)}))

(defn stop-http
  "Stops HTTP bridge."
  [_k state]
  (when-some [^HttpServer server (:server state)]
    (.stop server 0))
  nil)

(derive ::service :ferment.system/value)
(derive :ferment.http/default ::service)

(system/add-expand ::service [k config] {k (preconfigure-http k config)})
(system/add-init   ::service [k config]    (init-http k config))
(system/add-halt!  ::service [k state]     (stop-http k state))
