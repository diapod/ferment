(ns

 ^{:doc    "Environment files handling for ferment."
   :author "Paweł Wilk"
   :added  "1.0.0"}

 ferment.env.file

  (:refer-clojure :exclude [read])

  (:require [clojure.string      :as   str]
            [clojure.java.io     :as    io]
            [io.randomseed.utils :as utils]
            [ferment]))

(def ^:dynamic *default-ns* "ferment.env")

(defn- normalize-ns
  [x]
  (some-> x utils/named-to-str not-empty))

(defn get-default-ns
  ([]
   (normalize-ns *default-ns*))
  ([fallback]
   (or (normalize-ns *default-ns*)
       (normalize-ns fallback))))

(defn ns-or-default
  ([]
   (normalize-ns *default-ns*))
  ([nspace]
   (or (normalize-ns nspace)
       (normalize-ns *default-ns*))))

(defn key->ig-key
  [k nspace]
  (when (and k (some? k))
    (keyword (when nspace (not-empty (str nspace)))
             (-> (str k)
                 str/lower-case
                 (str/replace "/" ".")
                 (str/replace "_" ".")))))

(defn read
  ([p]
   (when (and p (some? p))
     (if-let [r (io/resource p)]
       (slurp r)
       (slurp (io/file p)))))
  ([f & more]
   (->> (cons f more)
        (map read)
        (apply str))))

(def ^:private ^java.util.regex.Pattern key-pattern
  (re-pattern "[A-Za-z_][A-Za-z0-9_\\-]*"))

(def ^:private ^java.util.regex.Pattern comment-pattern
  (re-pattern "^(.*?)(?:\\s+#.*)?$"))

(defn- valid-key?
  ^Boolean [sk]
  (and (string? sk)
       (.matches (.matcher key-pattern ^String sk))))

(defn- strip-inline-comment
  [s]
  (let [m (re-find comment-pattern s)]
    (if m (second m) s)))

(defn- unescape-double-quoted
  [s]
  (-> s
      (str/replace #"\\n" "\n")
      (str/replace #"\\r" "\r")
      (str/replace #"\\t" "\t")
      (str/replace #"\\\"" "\"")
      (str/replace #"\\\\" "\\")))

(defn- parse-value
  [raw]
  (let [v (some-> raw str/trim)]
    (cond
      (nil? v)                ""
      (str/blank? v)          ""
      (and (>= (count v) 2)
           (=  (first v) \')
           (=  (last  v) \')) (subs v 1 (dec (count v)))
      (and (>= (count v) 2)
           (=  (first v) \")
           (=  (last  v) \")) (unescape-double-quoted (subs v 1 (dec (count v))))
      :else (-> v strip-inline-comment str/trim))))

(defn- parse-line
  [line nspace]
  (when-some [s (some-> line str/trim)]
    (when (and (not (str/blank? s)) (not (str/starts-with? s "#")))
      (let [s   (if (str/starts-with? s "export ") (str/trim (subs s 7)) s)
            idx (.indexOf ^String s "=")]
        (when (neg? idx) (throw (ex-info "Invalid env file line (missing '=')" {:line line})))
        (let [k    (str/trim (subs s 0 idx))
              rawv (subs s (inc idx))]
          (when-not (valid-key? k)
            (throw (ex-info "Invalid env key (must match [A-Za-z_][A-Za-z0-9_\\-]*)"
                            {:key k :line line})))
          [(key->ig-key k nspace)
           (parse-value rawv)])))))

(defn parse
  "Parse a shell-style KEY=VALUE file(s) into a single map.

  Input:
  - content string OR filesystem path (string / java.io.File)

  Supports:
  - empty lines, comments (# ...)
  - optional leading `export `
  - whitespace around key and '='
  - values: unquoted, 'single-quoted', \"double-quoted\"
  - inline comments for unquoted values: `KEY=val # comment`

  Does NOT expand shell variables or multiline values."
  [path-or-resource & more]
  (when-some [content (apply read path-or-resource more)]
    (let [nspace (some-> *default-ns* utils/named-to-str not-empty)]
      (reduce (fn [m line]
                (if-let [[k v] (parse-line line nspace)]
                  (assoc m k v)
                  m))
              {}
              (str/split content #"\r?\n")))))

(defn derive-keys!
  ([config]        (derive-keys! config nil nil))
  ([config parent] (derive-keys! config parent nil))
  ([config parent filter-ns]
   (when (seq config)
     (let [filter-ns (normalize-ns filter-ns)
           parent    (utils/ensure-namespaced-keyword
                      (or parent :default)
                      (or filter-ns *default-ns*))]
       (if filter-ns
         (doseq [k (keys config)
                 :when (and (keyword? k)
                            (= filter-ns (namespace k))
                            (not (isa? k parent)))]
           (derive k parent))
         (doseq [k (keys config)
                 :when (and (keyword? k)
                            (not (isa? k parent)))]
           (derive k parent)))))))
