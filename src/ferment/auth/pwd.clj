(ns

    ^{:doc    "ferment service, passwords handling."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.pwd

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import  (java.security SecureRandom))

  (:require [clojure.spec.alpha      :as               s]
            [crypto.equality         :as          crypto]
            [jsonista.core           :as            json]
            [ferment.system        :as          system]
            [ferment.logging       :as             log]
            [io.randomseed.utils.crypto.codecs :as codecs]
            [io.randomseed.utils.var :as             var]
            [io.randomseed.utils     :as               u]
            [io.randomseed.utils.map :as             map]
            [io.randomseed.utils.map :refer    [qassoc
                                                qupdate]]
            [io.randomseed.utils     :refer         :all]
            [ferment.auth.specs    :refer         :all]
            [ferment.types.auth    :refer         :all]
            [ferment               :refer         :all])

  (:import (ferment Suites SuitesJSON)))

(defonce ^:private lock 'lock)

(def ^:const default-settings
  {:wait        1
   :wait-random [0 2]
   :wait-nouser 2})

;;
;; Helper functions
;;

(defn wait
  [wait-start wait-randmax]
  (s/assert number? wait-start)
  (s/assert number? wait-randmax)
  (if (pos? (+ wait-start wait-randmax))
    (Thread/sleep
     (long
      (* 1000
         (+ wait-start
            (if (pos? wait-randmax) (rand wait-randmax) 0)))))))

(defn salt-bytes
  ([] (salt-bytes 16))
  ([size]
   (s/assert pos-int? size)
   (let [sr (java.security.SecureRandom/getInstance "SHA1PRNG")
         buffer (byte-array size)]
     (.nextBytes sr buffer)
     buffer)))

(defn salt-string
  (^String [^java.lang.Number length
    ^clojure.lang.PersistentVector possible-chars]
   (s/assert :ferment.auth.settings.cipher/salt-length length)
   (s/assert :ferment.auth.settings.cipher/salt-charset possible-chars)
   (if (and (some? length)
            (some? possible-chars)
            (> length 0)
            (> (count possible-chars) 0))
     (apply str (repeatedly length #(get possible-chars (int (rand (count possible-chars)))))))))

(defn generate-salt
  [^java.lang.Number length
   ^clojure.lang.PersistentVector possible-chars
   prefix suffix]
  (s/assert (s/nilable nat-int?) length)
  (s/assert (s/nilable  vector?) possible-chars)
  (if (and (some? length) (> length 0))
    (let [prefix (if (some? prefix) (to-bytes prefix))
          suffix (if (some? suffix) (to-bytes suffix))
          score  (if (seq possible-chars)
                   (to-bytes (salt-string length possible-chars))
                   (salt-bytes length))]
      (bytes-concat prefix score suffix))))

(defn standard-check
  "Performs a standard authentication check based on the provided plain text password
  given as the second argument). The first argument should be an encryption function
  used to perform the encryption operation on the provided password and compare the
  result with the encrypted password that should be provided as third argument or as
  a value associated with the :password key if this argument is a map. The last
  argument should be settings map that will be passed to the encryption function.

  This is a low-level function that is intended to be used by different
  authentication modules which are implementing pretty standard way of checking the
  password."
  ([encrypt-fn plain encrypted salt settings]
   (standard-check encrypt-fn plain {:password encrypted :salt salt} settings))
  ([encrypt-fn plain opts encrypted salt settings]
   (standard-check encrypt-fn
                   plain
                   (qassoc opts :password encrypted :salt salt)
                   settings))
  ([encrypt-fn plain opts-or-enc settings]
   (s/assert :ferment.auth/encrypt-fn encrypt-fn)
   (s/assert (s/nilable :ferment.auth.settings/generic) settings)
   (s/assert (s/nilable (s/or :pwd-in-map (s/keys :req-un [:ferment.auth.plain/password])
                              :pwd-plain  :ferment.auth.plain/password)) opts-or-enc)
   (let [options   (if (or (nil? opts-or-enc) (map? opts-or-enc)) opts-or-enc {:password opts-or-enc})
         passwd    (get options :password)
         encrypted (if passwd (to-bytes passwd))
         provided  (get (encrypt-fn plain options settings) :password)]
     (if (and encrypted provided)
       (crypto/eq? encrypted provided)
       (crypto/eq? (to-bytes "@|-.-.-.-.-|@") (to-bytes "_ _ ! ! ! _ _"))))))

(defn find-handler
  "Tries to get an encryption handler from an entry map by accessing :handler key or
  using :handler-id as fallback (and dereferencing it)."
  [password-or-cipher]
  (s/assert (s/nilable (s/or :settings-cipher :ferment.auth.settings/cipher
                             :password-entry  :ferment.auth/password)) password-or-cipher)
  (if (some? password-or-cipher)
    (if-some [h (map/lazy-get password-or-cipher
                              :handler (var/deref (get password-or-cipher :handler-id)))]
      (if (and (map? h) (seq h)) h))))

(defn merge-suites
  ([^Suites crypto-suites-dual]
   (s/assert (s/nilable (s/or :settings-suite :ferment.auth.settings.suite/dual
                              :password-chain :ferment.auth.password-chain/dual)) crypto-suites-dual)
   (merge-suites (.shared ^Suites crypto-suites-dual) (.intrinsic ^Suites crypto-suites-dual)))
  ([defaults-crypto-suite user-crypto-suite & more]
   (let [suites (list* defaults-crypto-suite user-crypto-suite more)
         suites (filter identity suites)]
     (s/assert (s/coll-of
                (s/nilable
                 (s/coll-of
                  (s/or :settings-cipher-entry
                        (s/or :shared    :ferment.auth.settings.cipher/shared
                              :intrinsic :ferment.auth.settings.cipher/intrinsic)
                        :password-entry
                        (s/or :shared    :ferment.auth.password/shared
                              :intrinsic :ferment.auth.password/intrinsic))))) suites)
     (not-empty (apply map conj suites)))))

;;
;; Encryption handler processor
;;

(defn- entry-handler-wrapper
  [{:keys [^clojure.lang.Symbol  handler
           ^clojure.lang.Keyword name
           ^java.lang.Number salt-length
           ^java.lang.String salt-prefix
           ^java.lang.String salt-suffix
           ^java.lang.String salt-charset]
    :as   opts}                                      ; options for this handler (taken from configuration)
   settings]                                         ; authentication settings
  (locking lock
    (let [handler-id handler                         ; symbolic handler identifier (from options)
          handler    (assoc (var/deref handler-id)   ; actual handler (a map of two functions)
                            :id handler-id)          ; armored by its symbolic identifier (to find it later)
          encrypt-fn (get handler :encrypt-fn)       ; encryption function (from handler)
          check-fn   (get handler :check-fn)         ; checking function (from handler)
          opts       (-> (get handler :defaults)     ; encryption parameters (configuration options merged with handler defaults)
                         (or {}) (conj opts)
                         (qupdate :salt-charset vec) ; change string charset to vector (for random access)
                         map/remove-empty-values)
          pub-opts   (dissoc opts                    ; public options (enclosed in an encryption function)
                             :name :encrypt-fn       ; with removed keys which are only needed by this wrapper
                             :check-fn :salt-charset
                             :salt-length :salt-prefix :salt-suffix)]

      ;; add some required values to recalculated configuration options
      (assoc opts

             ;; a map of 2 functions armored by handler's symbolic id and a name
             ;; these functions (for encryption and decryption) are provided by the specific
             ;; authentication module (like scrypt or append).

             :handler handler

             ;; wrapper that encapsulates the original checking function
             ;; it is currently not used since the checker should be able
             ;; to act on a basis of parameters stored along with an encrypted password
             ;; (in order for it to not depend on a current configuration)

             :check-fn
             (fn checker
               ([plain, pass-opts] (check-fn plain pass-opts settings))
               ([plain, encr, salt] (checker plain {:salt salt :password encr} settings))
               ([plain, pass-opts, encr, salt] (checker plain (assoc pass-opts :salt salt :password encr) settings)))

             ;; wrapper that encapsulates the original encryption function
             ;; it receives public configuration options of handler (like salt parameters)
             ;; so it's able to use them (like to generate salt) and pass to the original
             ;; encryption function

             :encrypt-fn  (fn encryptor
                            ([plain] (encryptor plain settings))
                            ([plain settings]
                             (encrypt-fn
                              plain
                              (cond-> pub-opts
                                (number? salt-length)
                                (qassoc :salt (generate-salt
                                               salt-length
                                               (get opts :salt-charset)
                                               salt-prefix
                                               salt-suffix)))
                              settings)))))))

;;
;; Public interface
;;

(defn encrypt
  "Encrypts the given plain text password using all encryption functions in the given
  encryption suite."
  [plain local-settings]
  (s/assert :ferment.auth.plain/password plain)
  (let [settings   local-settings
        _          (s/assert (s/nilable :ferment.auth.pwd/settings) settings)
        used-suite (get settings :suite)]
    ;; extract suite from settings or use global as fallback
    ;; loop through all encryption steps in the provided suite
    ;; extracting encryption functions and using them
    ;; on passwords returned by calling previous ones
    ;; then store the results and important parameters (like salt)
    ;; in order (as a vector) while removing :password key from all
    ;; except the last one (which will be needed during checking)
    (loop [lastpass plain, todo used-suite, stack []]
      (let [opts    (first todo)
            encfn   (get opts :encrypt-fn)
            results (if (ifn? encfn)        ; if there is an encryption function
                      (-> lastpass          ; for the previously generated password
                          (encfn settings)  ; run an encryption function on it
                          (qassoc           ; to get the encrypted password and a bunch of parameters
                           :handler-id      ; armor the results (a map) with the handler-id
                           (get-in opts     ; containing names of encryption and decryption functions
                                   [:handler :id]))) ; for checking purposes
                      {:password lastpass})
            newpass (get results :password)]
        ;; if there are more items in our chain
        (if-some [nextf (next todo)]
          (recur newpass nextf (conj stack (dissoc results :password))) ; stack the current one and repeat the process
          (conj stack results))))))                                     ; otherwise stack the last results with a final encrypted password

(defn check
  "Checks if the given plain text password is correct by comparing it with the result
  of calling all checkers in the given encryption suite with memorized options
  applied."
  ([plain settings shared-suite intrinsic-suite & other-suites]
   (check plain (apply merge-suites shared-suite intrinsic-suite other-suites)))

  ([plain user-suite user-settings]
   (s/assert (s/nilable :ferment.auth.plain/password)   plain)
   (s/assert (s/nilable :ferment.auth/password-chain)   user-suite)
   (s/assert (s/nilable :ferment.auth.settings/generic) user-settings)
   (let [combo?   (instance? Suites user-suite) ; combined settings
         settings user-settings]

     ;; wait some time
     (if-not combo?
       (if-some [waitfn (get settings :wait-fn)] (waitfn user-suite)))

     (if combo? ; if the suite is a Suite then extract :shared and :intrinsic
       (check plain
              (.shared    ^Suites user-suite)
              (.intrinsic ^Suites user-suite)
              settings)

       ;; loop through all entries of a password suite
       ;; trying to resolve encryption handler
       ;; and calling encryption function to recreate hashing process

       (loop [lastpass plain, todo user-suite]
         (let [opts    (or (first todo) {})
               handler (-> opts :handler-id var/deref)
               opts    (-> opts                      ; preparing options for encryption function
                           (dissoc :handler-id)      ; encryption function doesn't have to know that
                           (qassoc :checking true))] ; informative flag for the encryption function
           (if-some [nextf (next todo)]
             (let [encrypt-fn (get handler :encrypt-fn)]
               (recur (get (if (ifn? encrypt-fn) (encrypt-fn lastpass opts settings) lastpass) :password) nextf))
             (if-some [checker-fn (get handler :check-fn)]
               (if (ifn? checker-fn) (checker-fn lastpass opts settings))))))))))

;;
;; Encode/decode as JSON
;;

(defn to-json
  "Converts the given suite to JSON format."
  [suite]
  (json/write-value-as-string suite json/keyword-keys-object-mapper))

(def json-translation
  {:prefix     codecs/base64-url-safe->bin
   :suffix     codecs/base64-url-safe->bin
   :infix      codecs/base64-url-safe->bin
   :salt       codecs/base64-url-safe->bin
   :password   codecs/base64-url-safe->bin
   :handler-id symbol})

(defn post-parse-json
  "Post-parses JSON data by transforming certain values with the given translation
  map."
  [tr-map m]
  (if-not (map? m) m
          (reduce-kv (fn [acc k f]
                       (if (contains? m k)
                         (qassoc acc k (f (get m k)))
                         acc))
                     m json-translation)))

(defn from-json
  "Converts JSON data to suite by applying transformations to keys described by
  tr-map. If no map is given the json-translation is used."
  ([suite]
   (from-json suite json-translation))
  ([suite tr-map]
   (mapv (partial post-parse-json tr-map)
         (json/read-value suite json/keyword-keys-object-mapper))))

;;
;; System suite
;;

(defn printable-suite
  [suite]
  (s/assert (s/nilable :ferment.auth.config/suite) suite)
  (map (comp normalize-name :name) suite))

(defn shared
  [crypto-entry]
  (s/assert (s/nilable :ferment.auth/crypto-entry) crypto-entry)
  ;; try :handler or dereferenced :handler-id to get a handler map
  ;; then try :handler-id of the given entry
  ;; then try :id from the obtained handler map (h)
  ;; then read shared configuration and select shared keys from handler
  (let [h   (find-handler crypto-entry)
        hid (map/lazy-get crypto-entry :handler-id
                          (map/lazy-get h :id (get-in crypto-entry [:handler :id])))
        dfl (select-keys  crypto-entry (get h :shared))]
    (qassoc dfl :handler-id hid)))

(defn shared-suite
  [suite]
  (s/assert :ferment.auth/crypto-suite suite)
  (map shared suite))

(def shared-chain shared-suite)

(defn split
  "Splits a cipher entry or a password into two parts and returns a Suite record with
  two fields `:shared` and `:intrinsic` with these parts."
  [crypto-entry]
  (s/assert :ferment.auth/crypto-entry crypto-entry)
  (let [shared-suite (shared crypto-entry)]
    (->Suites shared-suite (apply dissoc crypto-entry (keys shared-suite)))))

(defn split-suite
  [suite]
  (s/assert :ferment.auth/crypto-suite suite)
  (loop [todo     suite
         st-chain []
         va-chain []]
    (let [pwd-entry    (first todo)
          nexte        (next  todo)
          shared-pwd   (shared pwd-entry)
          new-st-chain (conj st-chain shared-pwd)
          new-va-chain (conj va-chain (apply dissoc pwd-entry (keys shared-pwd)))]
      (if (nil? nexte)
        (->Suites new-st-chain new-va-chain)
        (recur nexte new-st-chain new-va-chain)))))

(def split-chain split-suite)

(defn human-readable
  [pwd]
  (s/assert :ferment.auth/crypto-entry pwd)
  (-> pwd
      (map/update-bytes-to-strings :password :prefix :suffix :salt)
      (map/update-existing :name normalize-name)))

(defn human-readable-suite
  [suite]
  (s/assert :ferment.auth/crypto-suite suite)
  (map human-readable suite))

(def human-readable-chain human-readable-suite)

;;
;; Configuration
;;

(defn prepare-settings
  [config]
  (-> default-settings
      (merge config)
      map/remove-empty-values))

(defn- process-handlers
  [handlers settings]
  (map #(entry-handler-wrapper % settings) handlers))

(defn- process-suite
  [config]
  (log/msg "Registering password encryption suite:" (clojure.string/join ", " (printable-suite (:suite config))))
  (update config :suite process-handlers (dissoc config :suite)))

(defn init-wait
  [{:keys [wait-nouser], pwait :wait, [wmin wmax] :wait-random :as config}]
  (s/assert :ferment.auth.config/wait        pwait)
  (s/assert :ferment.auth.config/wait-min    wmin)
  (s/assert :ferment.auth.config/wait-max    wmax)
  (s/assert :ferment.auth.config/wait-nouser wait-nouser)
  (let [znil        (fnil identity 0)
        wait-nouser (znil wait-nouser) ; static delay when user is invalid
        pwait       (znil pwait)       ; static delay
        wmin        (znil wmin)        ; random delay min
        wmax        (znil wmax)        ; random delay max
        wmin        (if (> wmin wmax) wmax wmin)
        wstart      (+ pwait wmin)
        wrandm      (- wmax wmin)]
    (-> config
        (assoc  :wait-fn #(wait (+ wstart (if % 0 wait-nouser)) wrandm))
        (assoc  :wait-config {:wait        pwait
                              :wait-nouser wait-nouser
                              :wait-random wrandm
                              :wait-start  wstart})
        (dissoc :wait :wait-nouser :wait-random))))

(defn new-encryptor
  [settings]
  (fn password-encrypt
    [plain-text]
    (-> plain-text (encrypt settings) split-suite)))

(defn new-checker
  [settings]
  (fn password-check
    ([password suites]
     (password-check password (.shared ^Suites suites) (.intrinsic ^Suites suites)))
    ([password shared-suite user-suite]
     (if (or shared-suite user-suite)
       (check password (merge-suites shared-suite user-suite) settings)
       (check password nil settings)))))

(defn new-json-encryptor
  [settings]
  (fn password-encrypt-json
    [plain-text]
    (let [suites (-> plain-text (encrypt settings) split-suite)]
      (->SuitesJSON (to-json (.shared    ^Suites suites))
                    (to-json (.intrinsic ^Suites suites))))))

(defn new-json-checker
  [settings]
  (fn password-check-json
    ([password ^SuitesJSON json-suites]
     (password-check-json password
                          (.shared    ^SuitesJSON json-suites)
                          (.intrinsic ^SuitesJSON json-suites)))
    ([password shared-suite-json user-suite-json]
     (if (or shared-suite-json user-suite-json)
       (check password
              (merge-suites (from-json shared-suite-json) (from-json user-suite-json))
              settings)
       (check password nil settings)))))

(system/add-init
 ::pwd [k config]
 (s/assert :ferment.auth/config config)
 (log/msg "Configuring password authentication:" k)
 (let [config (-> config         ; processing configuration:
                  init-wait      ; initializing delay parameters
                  process-suite
                  (assoc :id k))
       config (assoc config :encrypt-fn      (new-encryptor      config))
       config (assoc config :check-fn        (new-checker        config))
       config (assoc config :encrypt-json-fn (new-json-encryptor config))
       config (assoc config :check-json-fn   (new-json-checker   config))]
   (s/assert :ferment.auth.pwd/settings config)
   config))

(defn expand-settings
  [k config]
  {k (prepare-settings config)})

(system/add-expand ::pwd [k config] (expand-settings k config))
(system/add-halt!  ::pwd [_ config] nil)

(derive ::settings.strong ::pwd)
(derive ::settings.simple ::pwd)
(derive ::suite.strong    ::system/value)
(derive ::suite.simple    ::system/value)
