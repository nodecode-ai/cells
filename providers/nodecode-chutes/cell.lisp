;;;; cell.lisp --- the cell: Chutes among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but chutes, and one command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Chutes' row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the tool-capable models /chutes
;;;;                          read over the original's meta-models, so
;;;;                          /connect offers Chutes, /models lists its models
;;;;                          and a turn resolves its lane and address the way
;;;;                          it does any catalog provider's
;;;;   :CREDENTIAL            a key from CHUTES_API_KEY, and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;
;;;;   /chutes [TEXT]         read GET /models and list the models whose
;;;;                          supported_features name tools
;;;;
;;;; Config, a sibling top-level key:
;;;;   "chutes": {"base_url": "https://llm.chutes.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Chutes is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-chutes)

(defun start ()
  "Start clean, and leave no merged catalog behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog))

(nle:define-cell chutes
  (:section ("chutes")
    (:guide "make a key at https://chutes.ai/app/api and save it with /connect or set CHUTES_API_KEY; /chutes reads the live catalog and lists the models that take tool calls; base_url is where Chutes is served, else CHUTES_BASE_URL")
    ("base_url" :string
     :doc "the Chutes API base the chat lane appends /chat/completions to (else CHUTES_BASE_URL)"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:command "chutes" #'run-command
            :description "Chutes: read the catalog and list the models that take tool calls"))
