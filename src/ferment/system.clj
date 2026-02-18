(ns

    ^{:doc    "ferment system."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.system

  (:refer-clojure :exclude [ref])

  (:require [integrant.core           :as           ig]
            [maailma.core             :as         conf]
            [cambium.core             :as          log]
            [ferment.readers          :as      readers]
            [ferment.env.file         :as      envfile]
            [tick.core                :as            t]
            [clojure.java.io          :as           io]
            [clojure.string           :as          str]
            [io.randomseed.utils.var  :as          var]
            [io.randomseed.utils.fs   :as           fs]
            [io.randomseed.utils      :as        utils
                                      :refer [defdoc!]])

  (:import (java.util TimeZone)))

;; integrant wrappers

(defmacro add-init        [& more] `(defmethod ig/init-key     ~@more))
(defmacro add-expand      [& more] `(defmethod ig/expand-key   ~@more))
(defmacro add-suspend!    [& more] `(defmethod ig/suspend-key! ~@more))
(defmacro add-resume      [& more] `(defmethod ig/resume-key   ~@more))
(defmacro add-resolve     [& more] `(defmethod ig/resolve-key  ~@more))
(defmacro add-halt!       [& more] `(defmethod ig/halt-key!    ~@more))

(defmacro init-key        [& more] `(ig/init-key     ~@more))
(defmacro expand-key      [& more] `(ig/expand-key   ~@more))
(defmacro suspend-key!    [& more] `(ig/suspend-key! ~@more))
(defmacro resume-key      [& more] `(ig/resume-key   ~@more))
(defmacro resolve-key     [& more] `(ig/resolve-key  ~@more))
(defmacro halt-key!       [& more] `(ig/halt-key!    ~@more))

(defmacro ref             [& more] `(ig/ref          ~@more))
(defmacro refset          [& more] `(ig/refset       ~@more))

(defdoc! add-init
  "Defines an `integrant.core/init-key` multimethod implementation.
This is a thin wrapper over `(defmethod integrant.core/init-key ...)`
to keep system component registrations visually consistent.")

(defdoc! add-expand
  "Defines an `integrant.core/expand-key` multimethod implementation.
Thin wrapper over `(defmethod integrant.core/expand-key ...)`.")

(defdoc! add-suspend!
  "Defines an `integrant.core/suspend-key!` multimethod implementation.
Thin wrapper over `(defmethod integrant.core/suspend-key! ...)`.")

(defdoc! add-resume
  "Defines an `integrant.core/resume-key` multimethod implementation.
Thin wrapper over `(defmethod integrant.core/resume-key ...)`.")

(defdoc! add-resolve
  "Defines an `integrant.core/resolve-key` multimethod implementation.
Thin wrapper over `(defmethod integrant.core/resolve-key ...)`.")

(defdoc! add-halt!
  "Defines an `integrant.core/halt-key!` multimethod implementation.
Thin wrapper over `(defmethod integrant.core/halt-key! ...)`.")

(defdoc! init-key
  "Calls `integrant.core/init-key`.

Convenience macro mirroring Integrant’s API while keeping all calls under
this namespace.")

(defdoc! expand-key
  "Calls `integrant.core/expand-key`.

Convenience macro mirroring Integrant’s API while keeping all calls under
this namespace.")

(defdoc! suspend-key!
  "Calls `integrant.core/suspend-key!`.

Convenience macro mirroring Integrant’s API while keeping all calls under
this namespace.")

(defdoc! resume-key
  "Calls `integrant.core/resume-key`.

Convenience macro mirroring Integrant’s API while keeping all calls under
this namespace.")

(defdoc! resolve-key
  "Calls `integrant.core/resolve-key`.

Convenience macro mirroring Integrant’s API while keeping all calls under
this namespace.")

(defdoc! halt-key!
  "Calls `integrant.core/halt-key!`.

Convenience macro mirroring Integrant’s API while keeping all calls under
this namespace.")

(defdoc! ref
  "Creates an Integrant reference via `integrant.core/ref`.
Convenience macro used in configs and code to reference other components.")

(defdoc! refset
  "Creates an Integrant ref-set via `integrant.core/refset`.
Convenience macro used in configs and code to reference sets of components.")

(defmacro add-prep
  "Defines an `integrant.core/expand-key` method that computes a prepared value
  from `[k v]` and returns it wrapped as `{k prepared}`.

  This matches Integrant’s expectation that `expand-key` returns a map keyed by the
  expanded key."
  [dispatch-value argvec & body]
  (let [k#    (gensym "k")
        v#    (gensym "v")
        kform (nth argvec 0 '_)
        vform (nth argvec 1 '_)]
    `(defmethod ig/expand-key ~dispatch-value [~k# ~v#]
       (let [~kform ~k#
             ~vform ~v#
             prepared# (do ~@body)]
         {~k# prepared#}))))

(defmacro prep-key
  "Expands a single key/value pair using `integrant.core/expand-key` and returns
  the produced value for that key.

  Throws `ex-info` when `expand-key` does not return a map or does not include the
  requested key."
  [k v]
  `(let [k# ~k
         m# (ig/expand-key k# ~v)]
     (when-not (map? m#)
       (throw (ex-info "ig/expand-key must return a map" {:k k# :returned m#})))
     (when-not (contains? m# k#)
       (throw (ex-info "ig/expand-key did not produce value for key" {:k k# :returned m#})))
     (get m# k#)))

(defn expand
  "Wrapper over `integrant.core/expand` that preserves `::keys` and
  `::config-sources` metadata inside the returned config map.

  Accepts an optional `innerf` (as in Integrant) and optional `ks` restricting
  which top-level keys to expand."
  ([cfg]
   (expand cfg identity nil))
  ([cfg innerf]
   (expand cfg innerf nil))
  ([cfg innerf ks]
   (let [csrc     (get cfg ::config-sources)
         ksrc     (get cfg ::keys)
         pure-cfg (dissoc cfg ::keys ::config-sources)
         ks       (or (seq ks) (keys cfg))]
     (assoc (ig/expand pure-cfg innerf ks) ::keys ksrc ::config-sources csrc))))

(defn prep
  "Alias for `expand`. With one arg, expands the whole config; with `keys`,
  expands only the requested top-level keys."
  ([cfg]
   (expand cfg))
  ([cfg keys]
   (expand cfg identity keys)))

(defn init
  "Wrapper over `integrant.core/init` that strips `::keys` and
  `::config-sources` before initialization, then re-attaches them to the returned
  system map. Optionally accepts `ks` to initialize only selected top-level keys."
  ([cfg]
   (init cfg nil))
  ([cfg ks]
   (let [csrc     (get cfg ::config-sources)
         ksrc     (get cfg ::keys)
         pure-cfg (dissoc cfg ::keys ::config-sources)
         ks       (or (seq ks) (keys cfg))]
     (assoc (ig/init pure-cfg ks) ::keys ksrc ::config-sources csrc))))

(defn suspend!
  "Wrapper over `integrant.core/suspend!` that ignores `::keys` and
  `::config-sources` entries in the config. With `ks` provided, suspends only selected
  top-level keys."
  ([cfg]
   (suspend! cfg nil))
  ([cfg ks]
   (let [pure-cfg (dissoc cfg ::keys ::config-sources)]
     (if-some [ks (seq ks)]
       (ig/suspend! pure-cfg ks)
       (ig/suspend! pure-cfg)))))

(defn resume
  "Wrapper over `integrant.core/resume` that resumes a system using a config while
  preserving `::keys` and `::config-sources`. Accepts optional `ks` to resume only
  selected top-level keys."
  ([cfg system]
   (resume cfg system nil))
  ([cfg system ks]
   (let [csrc     (get cfg ::config-sources)
         ksrc     (get cfg ::keys)
         pure-cfg (dissoc cfg    ::keys ::config-sources)
         pure-sys (dissoc system ::keys ::config-sources)
         new-sys  (if (seq ks) (ig/resume pure-cfg pure-sys ks) (ig/resume pure-cfg pure-sys))]
     (assoc new-sys ::config-sources csrc ::keys ksrc))))

(defn halt!
  "Wrapper over `integrant.core/halt!` that ignores `::keys` and
  `::config-sources` entries in the config. With `ks` provided, halts only selected
  top-level keys."
  ([cfg]
   (halt! cfg nil))
  ([cfg ks]
   (let [pure-cfg (dissoc cfg ::keys ::config-sources)]
     (if-some [ks (seq ks)]
       (ig/halt! pure-cfg ks)
       (ig/halt! pure-cfg)))))

(defn ref?
  "Returns true if `v` is an Integrant reference (`integrant.core/ref?`).
  Delegates directly to Integrant."
  [v]
  (ig/ref? v))

;; var-object pre-processing (allows to dereference Vars by symbols or keywords);
;; functions (symbols/keywords in lists) will be called in the init phase

(defn expand-var-process [k v] {k (var/resolve v)})
(defn prep-var-process   [  v]    (var/resolve v))
(defn init-var-process   [  v]    (var/deref   v))

(defdoc! expand-var-process
  "Pre-processes a config value by resolving Vars (symbols/keywords)
into Var objects. Returns `{k resolved}` so it can be used from
`integrant.core/expand-key`.")

(defdoc! prep-var-process
  "Resolves a symbol/keyword into a Var object (without dereferencing it).
Useful for preparing config values before init.")

(defdoc! init-var-process
  "Dereferences a resolved Var (or Var-like reference) during init,
producing the runtime value to be inserted into the system.")

;;
;; configuration loading
;;

;; readers for a custom tags #ref and #refset
;; that can be placed in configuration file to reference
;; other keys or sets of keys

(defn- validate-ref
  "Validates that `ref` is a qualified keyword suitable for Integrant references.
  Returns an `ex-info` instance describing the problem (or nil when valid)."
  [ref]
  (when-not (qualified-keyword? ref)
    (ex-info (str "Invalid reference: " ref ". Must be a qualified keyword.")
             {:reason ::invalid-ref, :ref ref})))

(def ^:private integrant-readers
  {:readers {'ref    ig/ref
             'refset ig/refset
             're     readers/regex}})

;; parsing configuration files and returning a merged map
;; for the given profile

(defn profile-resource-dirs
  "Returns default resource directory scan order for a given profile.

  The order is:
  - base shared roots (`translations/ferment`, `config/common`, `config/common/env`),
  - `prod` overlays (`config/common/prod`, `config/local`, `config/local/env`, `config/local/prod`),
  - optional profile overlays (`config/common/<profile>`, `config/local/<profile>`).

  For `:prod` (or nil/blank) only base+prod directories are returned."
  [profile]
  (let [p        (some-> profile name str/lower-case str/trim not-empty)
        base     ["translations/ferment"
                  "config/common"
                  "config/common/env"
                  "config/common/prod"
                  "config/local"
                  "config/local/env"
                  "config/local/prod"]
        profile? (and p (not= "prod" p))]
    (if profile?
      (conj (conj base (str "config/common/" p))
            (str "config/local/" p))
      base)))

(defn conf-resource
  "Loads one or more configuration resources (classpath-based) using Maailma and the
  project’s custom tag readers.

  Returns a seq of loaded config maps (or nil when nothing could be resolved)."
  ([r]
   (when r (conf/resource r integrant-readers)))
  ([r & more]
   (->> (cons r more)
        (map conf-resource)
        (filter identity)
        seq)))

(defn conf-file
  "Loads a configuration file from the filesystem using Maailma and the project’s
  custom tag readers.

  Returns a config map (or nil when `f` is nil)."
  [f]
  (when f (conf/file f integrant-readers)))

(defn slurp-resource-or-file
  "Reads text from `p`.  If `p` resolves as a classpath resource it is slurped
  from the resource; otherwise `p` is treated as a filesystem path.

  Returns nil for blank/nil input."
  [p]
  (when-some [p (not-empty (str p))]
    (if-let [r (io/resource p)]
      (slurp r)
      (slurp p))))

(defn edn-path?
  "Returns true when `p` looks like an EDN path (string ends with `.edn`)."
  [p]
  (and p (str/ends-with? (str p) ".edn")))

(defn env-path?
  "Returns true when `p` looks like an ENV path (string ends with `.env`)."
  [p]
  (and p (str/ends-with? (str p) ".env")))

(defn- file->resource-name
  "Converts discovered file path to classpath resource name under root directory `d`."
  [d ^java.io.File root ^java.io.File f]
  (let [root-path (.toPath (.getCanonicalFile root))
        file-path (.toPath (.getCanonicalFile f))
        rel-path  (.toString (.relativize root-path file-path))
        rel-path  (str/replace rel-path "\\" "/")
        base-dir  (str/replace (str d) #"/+$" "")]
    (str base-dir "/" rel-path)))

(defn conf-dirs->resource-names
  "Given one or more resource directories, finds direct (non-recursive) `.edn`
  and `.env` files in each directory, sorts them, and returns a seq of resource
  names (directory-prefixed paths) suitable for loading via
  `clojure.java.io/resource`.

  For multiple directories, preserves directory order and removes duplicate
  resource names while keeping the first occurrence."
  ([d]
   (when-some [^java.io.File root (some-> d fs/resource-file)]
     (->> (seq (.listFiles root))
          (filter #(.isFile ^java.io.File %))
          (map (partial file->resource-name d root))
          (filter (some-fn edn-path? env-path?))
          sort
          seq)))
  ([d & more]
   (some->> (cons d more)
            (map conf-dirs->resource-names)
            (map seq)
            (filter identity)
            (apply concat)
            distinct
            seq)))

;; loading namespaces required by fully-qualified configuration keys

(defn load-with-namespaces
  "Returns `config` after loading namespaces implied by fully-qualified Integrant
  keys via `integrant.core/load-namespaces`.

  Intended as a post-step after reading configuration data."
  [config]
  (ig/load-namespaces config)
  config)

;; selecting subsystem(s) from a global configuration map

(defn subsystems
  "Selects subsystem roots from a global Integrant config.
  With one arg returns `config`; with `keys` returns `(select-keys config keys)`
  (i.e., only chosen top-level branches)."
  ([config]      config)
  ([config keys] (select-keys config keys)))

;; getting system configuration from file(s)

(defn read-configs
  "Reads configuration files in EDN or ENV format. Supports three modes:
  (1) given a map that already contains `::config-sources`, reloads using those sources;
  (2) given a seq of resource directories, scans and loads `.edn`/`.env` resources from them;
  (3) given `local-file` plus one or more resource directories, loads resources first and then
      overlays a local filesystem config (EDN or ENV).

  For 2 or more arguments it loads `local-file` from a filesystem (unless it's `nil`)
  and scans all resource directories specified as other arguments. For each directory
  it tries to find filenames ending with `.edn` or `.env` and loads them all in
  order. The local file is being loaded last.

  The function returns a single configuration map merged from all loaded
  maps (EDN-sourced) with a special key ::config-. The configuration sources are
  preserved in this map under a key `:ferment.app/config-sources`, containing the
  following keys: `:resource-dirs`, `:resource-files` and `:local-file`.

  When there is only 1 argument given and it is a map then it should be a valid
  config with `:ferment.app/config-sources` key present. The associated map will be
  then used as a sources list of loaded configuration.

  When there is only 1 argument given and it is not a map then it should be a
  sequential collection of resource directories to scan and load configuration
  from. In this case the local configuration file is considered to be `nil`.

  Also derives ENV keys and loads namespaces for fully-qualified Integrant keys."
  ([resource-config-dir-or-map]
   (if-not (map? resource-config-dir-or-map)
     (read-configs nil resource-config-dir-or-map)
     (let [config-sources   (::config-sources resource-config-dir-or-map)
           local-file       (:local-file config-sources)
           resource-dirs    (seq (filter identity (:resource-dirs  config-sources)))
           resource-files   (seq (filter identity (:resource-files config-sources)))
           resource-files   (or resource-files
                                (apply conf-dirs->resource-names resource-dirs))
           config-sources   (assoc config-sources
                                   :resource-dirs resource-dirs
                                   :resource-files resource-files)
           edn-resources    (seq (filter edn-path? resource-files))
           env-resources    (seq (filter env-path? resource-files))
           edn-file         (when (edn-path? local-file) local-file)
           env-file         (when (and (nil? edn-file) (env-path? local-file)) local-file)
           edn-res-confs    (when edn-resources (apply conf-resource edn-resources))
           edn-file-confs   (some-> edn-file not-empty conf-file (cons '()))
           env-file-confs   (some-> env-file not-empty (cons '()))
           edn-configs      (concat edn-res-confs edn-file-confs)
           env-configs      (concat env-resources env-file-confs)
           edn-config-data  (some->> edn-configs seq
                                     (apply conf/build-config)
                                     load-with-namespaces)
           env-config-data  (some->> env-configs seq (apply envfile/parse))]
       (envfile/derive-keys! env-config-data)
       (-> (merge edn-config-data env-config-data)
           (assoc ::config-sources config-sources)))))
  ([local-file resource-config-dir & more-dirs]
   (read-configs
    {::config-sources {:local-file    local-file
                       :resource-dirs (cons resource-config-dir more-dirs)}})))

;; initialization shortcuts

(add-init   ::key         [k _] k)
(add-init   ::function    [k v] (v k))
(add-init   ::nil         [_ _] nil)
(add-init   ::var         [_ v] (init-var-process v))
(add-expand ::prepped-var [k v] (expand-var-process k v))
(add-init   ::prepped-var [_ v] (init-var-process v))
(add-init   ::value       [_ v] v)
(add-halt!  ::value       [_ v] v)
(add-init   ::var-make    [k v] (var/make k v))
(add-halt!  ::var-make    [k _] (var/make k nil))

;; properties shortcut

(derive ::properties ::value)

;; time zone

(add-init
 ::timezone
 [_ tz]
 (let [tz (utils/valuable tz)
       tz (or (:timezone-id tz) tz)
       tz (if (or (string? tz) (ident? tz)) (utils/some-str tz) tz)]
   (when tz
     (let [^TimeZone tz (if (true? tz)
                          (TimeZone/getDefault)
                          (let [^TimeZone tznew (TimeZone/getTimeZone ^String (str tz))]
                            (TimeZone/setDefault ^TimeZone tznew) tznew))
           tz-id        (utils/some-str (.getID ^TimeZone tz))]
       (log/info (str "Setting default time zone to " tz-id))
       {:timezone    tz
        :timezone-id tz-id
        :zone-region (t/zone tz-id)}))))
