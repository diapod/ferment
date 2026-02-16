(ns ferment.readers.main
  (:require [ferment.utils.importer :as imp]))

(defn gen-readers
  "Generates tagged literal handler maps and handlers."
  []
  (imp/readers-export))

(defn -main
  [& args]
  (gen-readers))
