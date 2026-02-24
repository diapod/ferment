(ns

    ^{:doc    "Model adapters facade for runtime worker operations."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.adapters.model

  (:require [ferment.model :as model]))

(defn invoke-worker!
  "Invokes worker with payload over command channel."
  [worker payload]
  (model/command-bot-worker! worker :invoke payload))

(defn session-workers-state
  [runtime]
  (model/session-workers-state runtime))

(defn expire-session-workers!
  [runtime]
  (model/expire-session-workers! runtime))

(defn freeze-session-worker!
  [runtime model-id sid]
  (model/freeze-session-worker! runtime model-id sid))

(defn thaw-session-worker!
  [runtime model-id sid]
  (model/thaw-session-worker! runtime model-id sid))
