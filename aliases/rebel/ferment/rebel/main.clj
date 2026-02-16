(ns ferment.rebel.main
  (:require
   rebel-readline.clojure.main
   rebel-readline.core
   io.aviso.ansi
   puget.printer))

(defn -main
  [& args]
  (rebel-readline.core/ensure-terminal
   (rebel-readline.clojure.main/repl
    :init (fn []
            (try
              (println "[ferment] Loading Clojure code, please wait...")
              (when (System/getProperty "nrepl.load")
                (try
                  (require 'ferment.nrepl)
                  (catch Exception e
                    (.printStackTrace e)
                    (println "[ferment] Failed to start nREPL (see exception above)."))))
              (require 'user)
              (in-ns 'user)
              (catch Exception e
                (.printStackTrace e)
                (println "[ferment] Failed to require user, this usually means there was a syntax error. See exception above.")))))))
