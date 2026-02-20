(ns

    ^{:doc    "Runtime effect executors with hard scope enforcement."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.effects

  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ferment.roles :as roles])

  (:import (java.net HttpURLConnection URI URL)
           (java.nio.charset Charset StandardCharsets)
           (java.nio.file Path)))

(def ^:private tool->effect
  {:fs/write-file   :fs/write
   :fs/write        :fs/write
   :process/run     :process/run
   :process/exec    :process/run
   :net/http-request :net/http
   :net/http        :net/http})

(defn- trim-s
  [v]
  (some-> v str str/trim not-empty))

(defn- parse-non-negative-int
  [v default]
  (let [n (cond
            (integer? v) (int v)
            (number? v) (int v)
            (string? v) (try
                          (Integer/parseInt (str/trim v))
                          (catch Throwable _ nil))
            :else nil)]
    (if (and (some? n) (not (neg? n)))
      n
      default)))

(defn- keywordish
  [v]
  (cond
    (keyword? v) v
    (string? v) (let [s (trim-s v)]
                  (when s
                    (if (str/starts-with? s ":")
                      (keyword (subs s 1))
                      (keyword s))))
    :else nil))

(defn- keyword-set
  [v]
  (cond
    (set? v) (into #{} (keep keywordish) v)
    (sequential? v) (into #{} (keep keywordish) v)
    (some? v) (if-some [k (keywordish v)] #{k} #{})
    :else #{}))

(defn- string-set
  [v]
  (->> (cond
         (set? v) v
         (sequential? v) v
         (some? v) [v]
         :else [])
       (keep trim-s)
       (map str/lower-case)
       set))

(defn- ->path
  [v]
  (when-some [s (trim-s v)]
    (.normalize (.toPath (io/file s)))))

(defn- absolute-path
  [root path]
  (let [^Path root' (or (->path root)
                        (->path "."))
        ^Path path' (or (->path path)
                        (throw (ex-info "Missing path."
                                        {:error :effects/invalid-input
                                         :failure/type :effects/invalid-input
                                         :reason :path/missing
                                         :retryable? false})))]
    (.normalize
     (if (.isAbsolute path')
       path'
       (.resolve root' path')))))

(defn- effect-requested
  [tool-node]
  (keyword-set (or (:effects/allowed tool-node)
                   (get-in tool-node [:effects :allowed]))))

(defn- effect-scope
  [tool-node effect]
  (let [scope (if (map? (get-in tool-node [:effects :scope]))
                (get-in tool-node [:effects :scope])
                {})]
    (cond
      (map? (get scope effect)) (get scope effect)
      (keyword? effect)
      (or (get scope (keyword (name effect)))
          {})
      :else {})))

(defn- auth-user
  [tool-node env]
  (let [node' (if (map? tool-node) tool-node {})
        env'  (if (map? env) env {})]
    (or (when (map? (:auth/user node'))
          (:auth/user node'))
        (when (map? (:auth/user env'))
          (:auth/user env'))
        (when (map? (get-in env' [:workflow/auth :user]))
          (get-in env' [:workflow/auth :user])))))

(defn- roles-config
  [tool-node env]
  (let [node' (if (map? tool-node) tool-node {})
        env'  (if (map? env) env {})]
    (or (when (map? (:roles/config node'))
          (:roles/config node'))
        (when (map? (:roles/config env'))
          (:roles/config env'))
        (when (map? (:roles env'))
          (:roles env')))))

(defn- enforce-effects-authorization!
  [tool-node env]
  (let [requested (effect-requested tool-node)
        user      (auth-user tool-node env)
        roles-cfg (roles-config tool-node env)]
    (when (and (seq requested)
               (map? roles-cfg))
      (let [authz (roles/authorize-effects roles-cfg requested user)]
        (when-not (:ok? authz)
          (throw (ex-info "Tool node requested effects forbidden for authenticated principal."
                          {:error :auth/forbidden-effect
                           :failure/type :auth/forbidden-effect
                           :retryable? false
                           :requested-effects requested
                           :denied-effects (:denied authz)
                           :user (cond-> {}
                                   (some? (:user/id user)) (assoc :user/id (:user/id user))
                                   (some? (:user/email user)) (assoc :user/email (:user/email user))
                                   (some? (:user/account-type user)) (assoc :user/account-type (:user/account-type user))
                                   (seq (:user/roles user)) (assoc :user/roles (:user/roles user)))
                           :roles (:roles authz)})))))))

(defn- deny-scope!
  [effect reason details]
  (throw (ex-info "Runtime effect scope denied."
                  (merge {:error :effects/scope-denied
                          :failure/type :effects/scope-denied
                          :retryable? false
                          :effect effect
                          :reason reason}
                         details))))

(defn- invalid-input!
  [reason details]
  (throw (ex-info "Invalid tool input."
                  (merge {:error :effects/invalid-input
                          :failure/type :effects/invalid-input
                          :retryable? false
                          :reason reason}
                         details))))

(defn- unsupported-tool!
  [tool-id]
  (throw (ex-info "Unsupported runtime tool."
                  {:error :effects/unsupported-tool
                   :failure/type :effects/unsupported-tool
                   :retryable? false
                   :tool/id tool-id
                   :known-tools (vec (sort (keys tool->effect)))})))

(defn- assert-effect-requested!
  [tool-node tool-id effect]
  (let [requested (effect-requested tool-node)]
    (when (empty? requested)
      (invalid-input! :effects/not-declared
                      {:tool/id tool-id
                       :required-effect effect}))
    (when-not (contains? requested effect)
      (invalid-input! :effects/missing-required
                      {:tool/id tool-id
                       :required-effect effect
                       :requested-effects requested}))))

(defn- fs-write-policy
  [effects-cfg tool-node]
  (let [global-cfg (if (map? (get effects-cfg :fs/write))
                     (get effects-cfg :fs/write)
                     {})
        local-cfg  (effect-scope tool-node :fs/write)
        root       (or (trim-s (:root global-cfg))
                       ".")
        local-root (trim-s (:root local-cfg))
        global-allow (vec
                      (keep trim-s
                            (or (:allow global-cfg)
                                (:write-whitelist global-cfg)
                                [])))
        local-allow (vec
                     (keep trim-s
                           (or (:allow local-cfg)
                               (:write-whitelist local-cfg)
                               [])))]
    {:enabled? (and (boolean (:enabled? global-cfg))
                    (if (contains? local-cfg :enabled?)
                      (boolean (:enabled? local-cfg))
                      true))
     :root root
     :local-root local-root
     :global-allow global-allow
     :local-allow local-allow}))

(defn- enforce-fs-write!
  [effects-cfg tool-node path]
  (let [{:keys [enabled? root local-root global-allow local-allow]} (fs-write-policy effects-cfg tool-node)]
    (when-not enabled?
      (deny-scope! :fs/write :disabled {:path path}))
    (let [^Path root-path (absolute-path root ".")
          ^Path target-path (absolute-path root path)
          ^Path local-root-path (when local-root
                                  (absolute-path root local-root))
          global-prefixes (mapv #(absolute-path root %) global-allow)
          local-prefixes  (mapv #(absolute-path root %) local-allow)]
      (when-not (.startsWith target-path root-path)
        (deny-scope! :fs/write :path-outside-root
                     {:path (str target-path)
                      :root (str root-path)}))
      (when (and (instance? Path local-root-path)
                 (not (.startsWith target-path local-root-path)))
        (deny-scope! :fs/write :path-outside-local-root
                     {:path (str target-path)
                      :root (str root-path)
                      :local-root (str local-root-path)}))
      (when (and (seq global-prefixes)
                 (not-any? #(.startsWith target-path ^Path %) global-prefixes))
        (deny-scope! :fs/write :path-not-allowed
                     {:path (str target-path)
                      :allow (mapv str global-prefixes)}))
      (when (and (seq local-prefixes)
                 (not-any? #(.startsWith target-path ^Path %) local-prefixes))
        (deny-scope! :fs/write :path-not-allowed
                     {:path (str target-path)
                      :allow (mapv str local-prefixes)}))
      {:root root-path
       :path target-path})))

(defn- process-policy
  [effects-cfg tool-node]
  (let [global-cfg (if (map? (get effects-cfg :process/run))
                     (get effects-cfg :process/run)
                     {})
        local-cfg  (effect-scope tool-node :process/run)
        root       (or (trim-s (:root global-cfg))
                       ".")
        global-cwd (vec
                    (keep trim-s
                          (or (:allow-cwd global-cfg)
                              ["."])))
        local-cwd  (vec
                    (keep trim-s
                          (or (:allow-cwd local-cfg)
                              [])))
        global-cmds (string-set (or (:allow-commands global-cfg)
                                    []))
        local-cmds (string-set (or (:allow-commands local-cfg)
                                   []))]
    {:enabled? (and (boolean (:enabled? global-cfg))
                    (if (contains? local-cfg :enabled?)
                      (boolean (:enabled? local-cfg))
                      true))
     :root root
     :global-cwd global-cwd
     :local-cwd local-cwd
     :global-commands global-cmds
     :local-commands local-cmds}))

(defn- executable-name
  [command]
  (let [raw (trim-s (first command))]
    (or (some-> raw io/file .getName trim-s)
        raw)))

(defn- enforce-process-run!
  [effects-cfg tool-node command cwd]
  (let [{:keys [enabled? root global-cwd local-cwd global-commands local-commands]} (process-policy effects-cfg tool-node)
        command' (vec (keep trim-s command))
        exe      (str/lower-case (or (executable-name command') ""))]
    (when-not enabled?
      (deny-scope! :process/run :disabled {:command command'}))
    (when-not (seq command')
      (invalid-input! :command/missing {:tool/id :process/run}))
    (when (and (seq global-commands)
               (not (contains? global-commands exe)))
      (deny-scope! :process/run :command-not-allowed
                   {:command command'
                    :executable exe
                    :allow-commands (vec (sort global-commands))}))
    (when (and (seq local-commands)
               (not (contains? local-commands exe)))
      (deny-scope! :process/run :command-not-allowed
                   {:command command'
                    :executable exe
                    :allow-commands (vec (sort local-commands))}))
    (let [^Path cwd-path (absolute-path root (or cwd "."))
          global-prefixes (mapv #(absolute-path root %) global-cwd)
          local-prefixes (mapv #(absolute-path root %) local-cwd)]
      (when-not (some #(.startsWith cwd-path ^Path %) global-prefixes)
        (deny-scope! :process/run :cwd-not-allowed
                     {:cwd (str cwd-path)
                      :allow-cwd (mapv str global-prefixes)}))
      (when (and (seq local-prefixes)
                 (not-any? #(.startsWith cwd-path ^Path %) local-prefixes))
        (deny-scope! :process/run :cwd-not-allowed
                     {:cwd (str cwd-path)
                      :allow-cwd (mapv str local-prefixes)}))
      {:command command'
       :cwd cwd-path})))

(defn- net-policy
  [effects-cfg tool-node]
  (let [global-cfg (if (map? (get effects-cfg :net/http))
                     (get effects-cfg :net/http)
                     {})
        local-cfg  (effect-scope tool-node :net/http)
        global-schemes (keyword-set (or (:allow-schemes global-cfg)
                                        #{:https}))
        local-schemes  (keyword-set (or (:allow-schemes local-cfg)
                                        #{}))
        global-hosts   (string-set (or (:allow-hosts global-cfg)
                                       []))
        local-hosts    (string-set (or (:allow-hosts local-cfg)
                                       []))
        global-ports   (set
                        (keep #(parse-non-negative-int % nil)
                              (or (:allow-ports global-cfg)
                                  [])))
        local-ports    (set
                        (keep #(parse-non-negative-int % nil)
                              (or (:allow-ports local-cfg)
                                  [])))]
    {:enabled? (and (boolean (:enabled? global-cfg))
                    (if (contains? local-cfg :enabled?)
                      (boolean (:enabled? local-cfg))
                      true))
     :global-schemes global-schemes
     :local-schemes local-schemes
     :global-hosts global-hosts
     :local-hosts local-hosts
     :global-ports global-ports
     :local-ports local-ports}))

(defn- parse-uri
  [url]
  (try
    (URI. ^String url)
    (catch Throwable _
      nil)))

(defn- enforce-net-http!
  [effects-cfg tool-node url]
  (let [{:keys [enabled?
                global-schemes local-schemes
                global-hosts local-hosts
                global-ports local-ports]} (net-policy effects-cfg tool-node)
        uri (parse-uri (or (trim-s url) ""))]
    (when-not enabled?
      (deny-scope! :net/http :disabled {:url url}))
    (when-not (instance? URI uri)
      (invalid-input! :url/invalid {:tool/id :net/http-request
                                    :url url}))
    (let [scheme (some-> (.getScheme ^URI uri) str/lower-case keyword)
          host   (some-> (.getHost ^URI uri) str/lower-case)
          port   (let [p (.getPort ^URI uri)]
                   (when (not= -1 p) p))]
      (when-not (contains? global-schemes scheme)
        (deny-scope! :net/http :scheme-not-allowed
                     {:url url
                      :scheme scheme
                      :allow-schemes global-schemes}))
      (when (and (seq local-schemes)
                 (not (contains? local-schemes scheme)))
        (deny-scope! :net/http :scheme-not-allowed
                     {:url url
                      :scheme scheme
                      :allow-schemes local-schemes}))
      (when (empty? global-hosts)
        (deny-scope! :net/http :host-not-allowed
                     {:url url
                      :host host
                      :allow-hosts []}))
      (when-not (contains? global-hosts host)
        (deny-scope! :net/http :host-not-allowed
                     {:url url
                      :host host
                      :allow-hosts (vec (sort global-hosts))}))
      (when (and (seq local-hosts)
                 (not (contains? local-hosts host)))
        (deny-scope! :net/http :host-not-allowed
                     {:url url
                      :host host
                      :allow-hosts (vec (sort local-hosts))}))
      (when (and (seq global-ports)
                 (some? port)
                 (not (contains? global-ports port)))
        (deny-scope! :net/http :port-not-allowed
                     {:url url
                      :port port
                      :allow-ports (vec (sort global-ports))}))
      (when (and (seq local-ports)
                 (some? port)
                 (not (contains? local-ports port)))
        (deny-scope! :net/http :port-not-allowed
                     {:url url
                      :port port
                      :allow-ports (vec (sort local-ports))}))
      {:uri uri
       :url (str uri)})))

(defn- canonical-value
  [tool-id effect out]
  {:result {:type :value
            :out (cond-> {:tool/id tool-id
                          :effect effect}
                   (map? out) (merge out)
                   (not (map? out)) (assoc :value out))}})

(defn- fs-write-file!
  [effects-cfg tool-node]
  (let [input    (if (map? (:input tool-node)) (:input tool-node) {})
        path     (trim-s (:path input))
        content  (str (or (:content input) ""))
        append?  (true? (:append? input))
        mkdirs?  (true? (:mkdirs? input))
        encoding (or (trim-s (:encoding input)) "UTF-8")
        charset  (or (try (Charset/forName encoding)
                          (catch Throwable _ nil))
                     StandardCharsets/UTF_8)
        _        (when-not path
                   (invalid-input! :path/missing {:tool/id :fs/write-file}))
        {:keys [path root]} (enforce-fs-write! effects-cfg tool-node path)
        parent   (some-> ^Path path .getParent)]
    (when (and mkdirs? (instance? Path parent))
      (.mkdirs (io/file (str ^Path parent))))
    (spit (str ^Path path)
          content
          :append append?
          :encoding (.name ^Charset charset))
    (canonical-value
     :fs/write-file
     :fs/write
     {:path (str ^Path path)
      :root (str ^Path root)
      :bytes (alength (.getBytes ^String content ^Charset charset))
      :append? append?
      :wrote? true})))

(defn- process-run!
  [effects-cfg tool-node]
  (let [input      (if (map? (:input tool-node)) (:input tool-node) {})
        command    (cond
                     (sequential? (:command input)) (:command input)
                     (string? (:command input)) [(:command input)]
                     :else nil)
        cwd        (trim-s (:cwd input))
        env        (if (map? (:env input)) (:env input) {})
        stdin      (when (string? (:stdin input)) (:stdin input))
        fail?      (true? (:fail-on-nonzero? input))
        timeout-ms (parse-non-negative-int (:timeout-ms input) 0)
        {:keys [command cwd]} (enforce-process-run! effects-cfg tool-node command cwd)
        pb         (doto (ProcessBuilder. ^java.util.List (mapv str command))
                     (.redirectErrorStream false)
                     (.directory (io/file (str ^Path cwd))))
        _          (when (seq env)
                     (let [pb-env (.environment pb)]
                       (doseq [[k v] env]
                         (.put pb-env (str k) (str (or v ""))))))
        process    (.start pb)
        out-reader (future (slurp (.getInputStream process)))
        err-reader (future (slurp (.getErrorStream process)))
        _          (when (string? stdin)
                     (with-open [w (io/writer (.getOutputStream process))]
                       (.write ^java.io.Writer w ^String stdin)))
        exit       (if (pos? timeout-ms)
                     (if (.waitFor process timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
                       (.exitValue process)
                       (do
                         (.destroyForcibly process)
                         (deny-scope! :process/run :timeout
                                      {:command command
                                       :timeout-ms timeout-ms})))
                     (.waitFor process))
        stdout     @out-reader
        stderr     @err-reader]
    (when (and fail? (not (zero? exit)))
      (throw (ex-info "Process command failed."
                      {:error :effects/process-failed
                       :failure/type :effects/process-failed
                       :retryable? false
                       :command command
                       :cwd (str ^Path cwd)
                       :exit exit
                       :stdout stdout
                       :stderr stderr})))
    (canonical-value
     :process/run
     :process/run
     {:command command
      :cwd (str ^Path cwd)
      :exit exit
      :stdout stdout
      :stderr stderr})))

(def ^:private methods-with-body
  #{"POST" "PUT" "PATCH" "DELETE"})

(defn- header-map
  [^HttpURLConnection conn]
  (->> (.getHeaderFields conn)
       (remove (fn [[k _]] (nil? k)))
       (map (fn [[k vals]]
              [k (vec vals)]))
       (into {})))

(defn- net-http-request!
  [effects-cfg tool-node]
  (let [input      (if (map? (:input tool-node)) (:input tool-node) {})
        method     (-> (or (trim-s (:method input)) "GET")
                       str/upper-case)
        url        (trim-s (:url input))
        headers    (if (map? (:headers input)) (:headers input) {})
        body       (some-> (:body input) str)
        timeout-ms (parse-non-negative-int (:timeout-ms input) 10000)
        {:keys [url]} (enforce-net-http! effects-cfg tool-node url)
        ^HttpURLConnection conn (doto ^HttpURLConnection (.openConnection (URL. ^String url))
                                  (.setRequestMethod method)
                                  (.setConnectTimeout timeout-ms)
                                  (.setReadTimeout timeout-ms))
        _ (doseq [[k v] headers]
            (when (and (some? k) (some? v))
              (.setRequestProperty conn (str k) (str v))))
        _ (when (and (string? body)
                     (contains? methods-with-body method))
            (.setDoOutput conn true)
            (with-open [out (.getOutputStream conn)]
              (.write out (.getBytes ^String body StandardCharsets/UTF_8))))
        status (.getResponseCode conn)
        stream (if (<= 200 status 399)
                 (.getInputStream conn)
                 (.getErrorStream conn))
        resp-body (if stream
                    (with-open [in stream]
                      (slurp in))
                    "")]
    (canonical-value
     :net/http-request
     :net/http
     {:method method
      :url url
      :status status
      :headers (header-map conn)
      :body resp-body})))

(defn invoke-tool!
  "Invokes runtime tool with strict effect scope enforcement.

  Inputs:
  - `effects-cfg`: runtime effect policy map
  - `tool-node`:   workflow node map (expects `:tool/id`, `:input`, `:effects`)
  - `env`:         current workflow env (reserved for future use)"
  ([effects-cfg tool-node]
   (invoke-tool! effects-cfg tool-node nil))
  ([effects-cfg tool-node env]
   (let [tool-id (or (keywordish (:tool/id tool-node))
                     (keywordish (:tool tool-node)))
         effect  (get tool->effect tool-id)]
     (when-not (keyword? tool-id)
       (invalid-input! :tool/missing {:node tool-node}))
     (when-not (keyword? effect)
       (unsupported-tool! tool-id))
     (assert-effect-requested! tool-node tool-id effect)
     (enforce-effects-authorization! tool-node env)
     (case tool-id
       :fs/write-file    (fs-write-file! effects-cfg tool-node)
       :fs/write         (fs-write-file! effects-cfg tool-node)
       :process/run      (process-run! effects-cfg tool-node)
       :process/exec     (process-run! effects-cfg tool-node)
       :net/http-request (net-http-request! effects-cfg tool-node)
       :net/http         (net-http-request! effects-cfg tool-node)
       (unsupported-tool! tool-id)))))
