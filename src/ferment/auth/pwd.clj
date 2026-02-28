(ns

    ^{:doc    "ferment service, passwords handling."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.auth.pwd

  (:require [clojure.spec.alpha       :as       s]
            [ferment.auth.specs       :refer  :all]
            [ferment.logging          :as     log]
            [ferment.system           :as  system]
            [io.randomseed.utils.auth.pwd :as     pwd]))

(system/add-init
 ::pwd [k config]
 (s/assert :ferment.auth/config config)
 (log/msg "Configuring password authentication:" k)
 (let [config (pwd/init k config)]
   (s/assert :ferment.auth.pwd/settings config)
   config))

(system/add-expand ::pwd [k config] (pwd/expand-settings k config))
(system/add-halt!  ::pwd [_ config] nil)

(derive ::settings.strong ::pwd)
(derive ::settings.simple ::pwd)
(derive ::suite.strong    ::system/value)
(derive ::suite.simple    ::system/value)
