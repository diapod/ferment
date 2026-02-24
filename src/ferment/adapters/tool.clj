(ns

    ^{:doc    "Tool adapters facade for runtime effect executors."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.adapters.tool

  (:require [ferment.effects :as effects]))

(defn invoke!
  "Invokes a tool node through runtime effects executor."
  [effects-cfg tool-node env]
  (effects/invoke-tool! effects-cfg tool-node env))

