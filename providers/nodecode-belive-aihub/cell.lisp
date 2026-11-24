;;;; cell.lisp --- the cell: BeLive AI Hub among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries BeLive AI Hub's row: the
;;;;                          chat lane's package, this section's base, the key
;;;;                          variable, and the routes a turn can run on, so
;;;;                          /connect offers it, /models lists its models and
;;;;                          a turn resolves its lane and address the way it
;;;;                          does any catalog provider's.
;;;;   :CREDENTIAL            a key from BELIVE_TOKENHUB_API_KEY, the variable
;;;;                          the hub issues, and no other one; a key /connect
;;;;                          saved in auth.json answers before this point does.
;;;;
;;;; A worker fetches the hub's model list once at start, and again after each
;;;; wait while it is unreachable; a landed fetch lets the worker end, and the
;;;; original plugin's fallback_models answer in the meantime.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "belive-aihub": {"base_url": "https://tokenhub-api.belive.sg/api/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and BeLive AI Hub is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-belive-aihub)

(defun credential (op next)
  "The :CREDENTIAL answer for belive-aihub: the workspace key
BELIVE_TOKENHUB_API_KEY holds, and no other variable. The core's derived name
(BELIVE_AIHUB_API_KEY) is not the one the hub issues, so without this the
ladder would answer the keyless default."
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let ((key (nle::credential-env +api-key-env+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(nle:define-cell belive-aihub
  (:section ("belive-aihub")
    (:guide "make a workspace inference key at https://aihub.belive.sg/; save it with /connect or set BELIVE_TOKENHUB_API_KEY; base_url is where the OpenAI-compatible gateway is served")
    ("base_url" :string :default +base+
     :doc "the OpenAI-compatible base the chat lane appends /chat/completions to"))
  (:start (lambda ()
            ;; a start begins from no rows and no merged catalog, so a restart
            ;; after a fix fetches again and no test inherits the last start's
            (setf *live-rows* nil)
            (forget-catalog)
            (let* ((base (setting :base-url))
                   (worker (nlk:worker-start "belive-aihub-catalog" (lambda () (refresh base))
                                             :wake 1800 :prime t)))
              (nle:on-stop (lambda ()
                             (nlk:worker-stop worker)
                             (setf *live-rows* nil)
                             (forget-catalog))))))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
