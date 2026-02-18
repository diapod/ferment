(ns

    ^{:doc    "ferment service, remote IP middleware."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.middleware.remote-ip

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string          :as         str]
            [clojure.core.memoize    :as         mem]
            [smangler.api            :as          sa]
            [ferment.logging       :as         log]
            [ferment.system        :as      system]
            [io.randomseed.utils.var :as         var]
            [io.randomseed.utils.ip  :as          ip]
            [io.randomseed.utils.map :as         map]
            [io.randomseed.utils.map :refer [qassoc]]
            [io.randomseed.utils     :refer     :all])

  (:import  (java.net         InetAddress
                              Inet4Address
                              Inet6Address)
            (inet.ipaddr      IPAddress
                              IPAddressString
                              IPAddressNetwork)
            (inet.ipaddr.ipv4 IPv4Address
                              IPv4AddressTrie)
            (inet.ipaddr.ipv6 IPv6Address
                              IPv6AddressTrie)))

(def ^:const proxy-header "x-forwarded-for")
(def ^:const ip-separator (re-pattern "[,\\s]+"))

(defn safe-ip-to-address
  [s]
  (try (ip/to-address s) (catch Exception _ nil)))

(defn remote-addr-parse
  "Parses remote address string to get the string representation of client's IP
  address."
  [addr]
  (if-some [a (some-str addr)]
    (if-some [a (not-empty (str/trim (str/trim-newline a)))]
      (not-empty (sa/trim-both {\space \space \[ \] \( \)} a)))))

(defn remote-addr-get
  "Parses :remote-addr key of a request to get the string representation of client's IP
  address."
  [req]
  (if req (remote-addr-parse (get req :remote-addr))))

(defn process-proxy
  "Prepares proxy header configuration for address-getting middleware."
  [p]
  (when p
    (if (true? p)
      proxy-header
      (if-some [p (some-str-simple-down p)]
        (not-empty (str/trim (str/trim-newline p)))))))

(defn process-proxy-for
  "Prepares configuration of proxy-detection. If the config contains a sequence it will
  turn it into two Trie trees of IP ranges and return a function which returns truthy value
  if a client IP from the given request is within any range."
  [t]
  (let [{t4 :ipv4 t6 :ipv6} (ip/preprocess-ip-list t)]
    (if (and t4 t6)
      (do (log/msg "Loaded" (+ (.size ^IPv4AddressTrie t4)
                               (.size ^IPv6AddressTrie t6))
                   "proxy detection IPv4 and IPv6 ranges")
          (fn [ipaddr-str]
            (when-some [remote-addr (remote-addr-parse ipaddr-str)]
              (log/dbg "Checking if IP" remote-addr "is on a proxy list (IPv4 and IPv6")
              (let [^IPAddressString ipa (IPAddressString. ^String remote-addr)
                    ip                   (ip/to-v6 (.getAddress ^IPAddressString ipa))]
                (and (or (ip/in6t? ^IPv6AddressTrie t6 ip) (ip/in4t? ^IPv4AddressTrie t4 ip))
                     (do (log/dbg "Detected proxy:" remote-addr)) true)))))
      (if t6
        (do (log/msg "Loaded" (.size ^IPv6AddressTrie t6) "proxy detection IPv6 ranges")
            (fn [ipaddr-str]
              (when-some [remote-addr (remote-addr-parse ipaddr-str)]
                (log/dbg "Checking if IP" remote-addr "is on a proxy list (IPv6)")
                (let [^IPAddressString ipa (IPAddressString. ^String remote-addr)
                      ip                   (ip/to-v6 (.getAddress ^IPAddressString ipa))]
                  (and (ip/in6t? ^IPv6AddressTrie t6 ^IPv6Address ip)
                       (do (log/dbg "Detected proxy:" remote-addr) true))))))
        (if t4
          (do (log/msg "Loaded" (.size ^IPv4AddressTrie t4) "proxy detection IPv4 ranges")
              (fn [ipaddr-str]
                (when-some [remote-addr (remote-addr-parse ipaddr-str)]
                  (log/dbg "Checking if IP" remote-addr "is on a proxy list (IPv4)")
                  (let [^IPAddressString ipa (IPAddressString. ^String remote-addr)
                        ip                   (ip/to-v4 (.getAddress ^IPAddressString ipa))]
                    (and (ip/in4t? ^IPv4AddressTrie t4 ^IPv4Address ip)
                         (do (log/dbg "Detected proxy:" remote-addr) true))))))
          (do (log/msg "Trusting any IP address to deliver correct proxy header")
              (constantly true)))))))

(defn get-proxy-header
  [headers proxy-setting]
  (if proxy-setting (some-str (get headers proxy-setting))))

(defn get-ips-from-req-data
  ([pheader addr-string]
   (get-ips-from-req-data pheader addr-string nil nil))
  ([pheader addr-string proxy-setting]
   (get-ips-from-req-data pheader addr-string proxy-setting nil))
  ([pheader addr-string proxy-setting proxy-for-fn]
   (let [raddr (-> addr-string remote-addr-parse str str/trim ip/to-address)]
     (if (and pheader (or (not proxy-for-fn) (proxy-for-fn addr-string)))
       (let [addr  (-> pheader (str/split ip-separator) first str str/trim safe-ip-to-address)
             paddr (ip/plain-ip-str addr)]
         [addr paddr raddr])
       [raddr (ip/plain-ip-str raddr) nil]))))

(def get-ips-from-req-data-lru
  (mem/lru get-ips-from-req-data {} :lru/threshold 2048))

(defn handler
  ([req]
   (handler req nil nil))
  ([req proxy-setting]
   (handler req proxy-setting nil))
  ([req proxy-setting proxy-for-fn]
   (let [addrs (delay (-> (get req :headers)
                          (get-proxy-header proxy-setting)
                          (get-ips-from-req-data-lru (get req :remote-addr)
                                                     proxy-setting
                                                     proxy-for-fn)))]
     (qassoc req
             :remote-ip        (delay (nth @addrs 0 nil))
             :remote-ip/str    (delay (nth @addrs 1 nil))
             :remote-ip/proxy? (delay (nth @addrs 3 nil))))))

;; Configuration initializers

(defn wrap-ip
  "Client IP getter middleware."
  [k ip-getter-sym proxy-setting proxy-for]
  (let [ip-getter     (var/deref-symbol ip-getter-sym)
        proxy-setting (process-proxy proxy-setting)
        proxy-for-fn  (process-proxy-for proxy-for)]
    (log/msg "Installing IP analyzer:" ip-getter-sym
             (if proxy-setting (str "(proxy header: " proxy-setting ")")))
    {:name    k
     :compile (fn [_ _]
                (fn [h]
                  (fn [req]
                    (h (ip-getter req proxy-setting proxy-for-fn)))))}))

(defn new-reserved
  [p]
  (if (system/ref? p)
    p
    (ip/preprocess-ip-list p)))

(defn expand-reserved
  [k p]
  {k (new-reserved p)})

(system/add-expand ::reserved [k config] (expand-reserved k config))
(system/add-init   ::reserved [_ config] config)
(system/add-halt!  ::reserved [_ config] nil)

(system/add-init  ::default  [k config] (wrap-ip k
                                                 (:handler      config)
                                                 (:proxy-header config)
                                                 (:proxy-for    config)))
(system/add-halt! ::default [_ config] nil)

(derive ::web ::default)
(derive ::api ::default)
(derive ::all ::default)

(derive ::web-reserved ::reserved)
(derive ::api-reserved ::reserved)
(derive ::all-reserved ::reserved)
