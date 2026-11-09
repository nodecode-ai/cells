;;;; cell.lisp --- the cell: Requesty among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but requesty, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Requesty's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the managed policies /requesty read
;;;;                          over the bundled rows, so /connect offers it,
;;;;                          /models lists its models and a turn resolves its
;;;;                          lane and address the way it does any catalog
;;;;                          provider's
;;;;   :CREDENTIAL            a key from REQUESTY_API_KEY, and no other
;;;;                          variable; a key /connect saved answers before
;;;;                          this point does
;;;;   REQUEST-BODY           reasoning_effort in Requesty's spelling and only
;;;;                          to a model the catalog marks reasoning, and the
;;;;                          session id as requesty.trace_id
;;;;
;;;;   /requesty [TEXT]       read GET /v1/models/managed and list the routes
;;;
;;;; Config, a sibling top-level key:
;;;;   "requesty": {"base_url": "https://router.requesty.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-requesty)

(defun start ()
  "Start clean, and leave no merged catalog behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog))

(nle:define-cell requesty
  (:section ("requesty")
    (:guide "make a key at https://app.requesty.ai/api-keys and save it with /connect or as REQUESTY_API_KEY; /requesty lists the managed routing policies; base_url is where Requesty is served")
    ("base_url" :string :default +base+
     :doc "the Requesty API base the chat lane appends /chat/completions to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:command "requesty" #'run-command
            :description "Requesty: list the managed routing policies"))
