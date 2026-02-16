(ns

    ^{:doc    "Derivations for per-capability keys in Ferment capabilities registry."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    ferment.caps.registry

  (:require [ferment.caps]))

;; Per-capability keys are configuration-oriented and inherit lifecycle from
;; :ferment.caps/entry handlers defined in `ferment.caps`.
(derive ::llm-voice :ferment.caps/entry)
(derive ::llm-code  :ferment.caps/entry)
(derive ::llm-meta  :ferment.caps/entry)
(derive ::llm-mock  :ferment.caps/entry)
