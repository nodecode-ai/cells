;;;; cell.lisp --- the cell: Lightning AI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but lightningai, one
;;;; command and one manual topic:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Lightning AI's row: the chat
;;;;                          lane's package, this section's base, the
;;;;                          profile's key variables, and the models the
;;;;                          gateway last listed - so /connect offers it,
;;;;                          /models lists its models and a turn resolves its
;;;;                          lane and address the way it does any catalog
;;;;                          provider's
;;;;   :CREDENTIAL            a key from LIGHTNING_API_KEY, then
;;;;                          LIGHTNING_AI_API_KEY, and no other variable; a
;;;;                          key /connect saved answers before this point does
;;;;
;;;;   /lightning-ai [TEXT]  read GET <base>/models and list the models,
;;;;                          putting them in the picker (TEXT narrows the
;;;;                          listing)
;;;;
;;;; Config, a sibling top-level key:
;;;;   "lightning-ai": {"base_url": "https://platform.lightning.ai/api/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; The start reads the model list once, bounded, when a key is set: the
;;;; original's picker was that list, and a box with no key or no network
;;;; keeps whatever rows it has and reads again on /lightning-ai.

(in-package #:nodecode-lightning-ai)

(defparameter +manual+
  "Lightning AI serves an OpenAI-compatible gateway at
https://platform.lightning.ai/api/v1, which a turn reaches as provider
`lightningai' (/models lists it as Lightning AI). Make a key at
https://lightning.ai and save it with /connect or as LIGHTNING_API_KEY; the
profile's other names, LIGHTNING_AI_API_KEY and the LIGHTNING_BASE_URL
override, are read too. The models are whatever GET <base>/models answers:
the cell reads that list at start when a key is set, and /lightning-ai reads
it again, narrows the listing to the ids containing its argument, and puts the
rows in /models for the rest of the session."
  "The manual topic's text: what the provider is, where its key goes, and
where its models come from.")

(nle:define-cell lightning-ai
  (:section ("lightning-ai")
    (:guide "make a key at https://lightning.ai; save it with /connect or set LIGHTNING_API_KEY; base_url is where the OpenAI-compatible gateway is served")
    ("base_url" :string :default +base+
     :doc "the Lightning AI API base the chat lane appends /chat/completions to"))
  (:settings 'read-settings)
  (:start (lambda ()
            (forget-catalog)
            ;; one bounded read at start, so the first picker already lists
            ;; the models a key serves; no key means no request at all
            (refresh-live)
            (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:help :lightning-ai "Lightning AI: its gateway, its key and the model list it publishes" +manual+)
  (:command "lightning-ai" #'run-command
            :description "Lightning AI: read the model list the gateway publishes"
            :argument-hint "TEXT"))
