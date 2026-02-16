(ns ferment.mlx

  (:require [clojure.data.json :as json])

  (:import (java.time Instant)
           (java.util UUID)))

;; --- podstawowy runner procesu (jak u Ciebie) -------------------------------

(defn sh!
  "Run a process, return {:exit int :out string :err string}."
  [& args]
  (let [pb (doto (ProcessBuilder. (into-array String args))
             (.redirectErrorStream false))
        p  (.start pb)
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))
        ec  (.waitFor p)]
    {:exit ec :out out :err err}))

(defn- ^String now-iso [] (str (Instant/now)))

(defn- ensure!
  [pred msg data]
  (when-not pred
    (throw (ex-info msg data))))

(defn- write-temp-json!
  "Writes EDN-ish map as JSON into a temp file; returns absolute path."
  ^String
  [m]
  (let [tmp (java.io.File/createTempFile "mlx" ".json")]
    (spit tmp (json/write-str m))
    (.getAbsolutePath tmp)))

(defn- http-post-json-via-curl!
  "POST JSON to URL using curl. Returns parsed JSON map.
   Options:
   - :timeout-s (default 120)
   - :headers (map of string->string)
   Throws ex-info on non-zero exit or invalid JSON."
  [url payload {:keys [timeout-s headers] :or {timeout-s 120 headers {}}}]
  (ensure! (string? url) "url must be string" {:url url})
  (ensure! (map? payload) "payload must be map" {:payload payload})

  (let [tmp-path (write-temp-json! payload)
        header-args (mapcat (fn [[k v]] ["-H" (str k ": " v)]) headers)
        ;; -m sets max time; --fail-with-body keeps body on 4xx/5xx in new curl,
        ;; but macOS curl might not have it; --fail will suppress body, so we avoid it.
        ;; Instead, we capture out and let server include error json if it wants.
        {:keys [exit out err]} (apply sh!
                                     (concat ["curl" "-sS"
                                              "-m" (str timeout-s)
                                              url
                                              "-H" "Content-Type: application/json"
                                              "--data-binary" (str "@" tmp-path)]
                                             header-args))]
    (ensure! (zero? exit)
             "MLX HTTP call failed"
             {:exit exit :stderr err :stdout (subs out 0 (min 2000 (count out))) :url url})
    (try
      (json/read-str out :key-fn keyword)
      (catch Exception e
        (throw (ex-info "MLX server returned non-JSON response"
                        {:url url
                         :stdout (subs out 0 (min 4000 (count out)))
                         :stderr err}
                        e))))))

;; --- public API --------------------------------------------------------------

(defn mlx-generate!
  "Calls MLX server and returns generated text.

  Required:
    - :prompt (string)

  Optional:
    - :base-url (default \"http://127.0.0.1:8088\")
    - :endpoint (default \"/generate\")
    - :max-tokens (default 512)
    - :temperature (default 0.6)
    - :top-p (if your server supports it)
    - :stop (vector of stop strings, if supported)
    - :timeout-s (default 120)
    - :meta (map) will be passed through (useful for tracing/logging)

  Expects server to return JSON with key :text (string)."
  [{:keys [prompt base-url endpoint max-tokens temperature top-p stop timeout-s meta]
    :or   {base-url "http://127.0.0.1:8088"
           endpoint "/generate"
           max-tokens 512
           temperature 0.6
           timeout-s 120}}]
  (ensure! (string? prompt) "mlx-generate!: :prompt must be a string" {:prompt prompt})
  (let [req-id (str (UUID/randomUUID))
        payload (cond-> {:id req-id
                         :ts (now-iso)
                         :prompt prompt
                         :max_tokens (int max-tokens)
                         :temperature (double temperature)}
                  top-p (assoc :top_p (double top-p))
                  (seq stop) (assoc :stop stop)
                  (map? meta) (assoc :meta meta))
        resp (http-post-json-via-curl! (str base-url endpoint)
                                       payload
                                       {:timeout-s timeout-s})
        text (:text resp)]
    (ensure! (string? text)
             "mlx-generate!: response missing :text"
             {:response resp :request-id req-id})
    text))

(defn- default-chat-system
  []
  (str "Jesteś głosem (VOICE) agenta. Mów po polsku, jasno, konkretnie.\n"
       "Nie konfabuluj faktów. Gdy brakuje danych, zadaj najwyżej 2 pytania.\n"
       "Jeśli użytkownik prosi o kod, możesz poprosić o kontekst lub zasugerować,"
       " że solver wygeneruje patch, a Ty go opiszesz."))

(defn- chat->prompt
  "Builds a simple prompt from messages.
   messages: vector of {:role \"system\"|\"user\"|\"assistant\" :content \"...\"}"
  [messages]
  (let [role->label {"system" "SYSTEM"
                     "user" "USER"
                     "assistant" "ASSISTANT"}]
    (apply str
           (for [{:keys [role content]} messages
                 :let [lbl (get role->label role (clojure.string/upper-case (str role)))]]
             (str lbl ":\n" content "\n\n"))
           "ASSISTANT:\n")))

(defn mlx-chat!
  "Chat wrapper on top of mlx-generate!.

  Input:
    - :messages  (vector) required, e.g. [{:role \"user\" :content \"...\"}]
  Optional:
    - :system    (string) if provided, prepended as system message; otherwise default system
    - :base-url, :endpoint, :max-tokens, :temperature, :top-p, :stop, :timeout-s, :meta
    - :return    :text (default) or :messages

  Returns:
    - when :return :text -> assistant string
    - when :return :messages -> updated messages vector including assistant turn"
  [{:keys [messages system return] :or {return :text}} opts]
  (ensure! (vector? messages) "mlx-chat!: :messages must be a vector" {:messages messages})
  (ensure! (every? #(and (map? %)
                         (string? (:role %))
                         (string? (:content %)))
                   messages)
           "mlx-chat!: each message must have :role and :content strings"
           {:messages messages})

  (let [sys (or system (default-chat-system))
        msgs (cond-> messages
               (not= "system" (:role (first messages)))
               (into [{:role "system" :content sys}]))
        prompt (chat->prompt msgs)
        text (mlx-generate! (merge opts {:prompt prompt}))]
    (case return
      :messages (conj msgs {:role "assistant" :content text})
      ;; default
      text)))
