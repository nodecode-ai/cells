;;;; cell.lisp --- the cell: Telnyx among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but telnyx:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Telnyx's row: the chat
;;;;                          lane's package, this section's base, and the
;;;;                          original's curated models, so /connect offers
;;;;                          it, /models lists them and a turn resolves its
;;;;                          lane and address the way it does any catalog
;;;;                          provider's. The core lists the live catalog
;;;;                          itself (`GET <base>/models' with the lane's
;;;;                          Bearer key), so the bundled rows are the
;;;;                          fallback the picker shows before it answers.
;;;;   :CREDENTIAL            a key from TELNYX_API_KEY, the variable the
;;;;                          original reads, and no other variable; a key
;;;;                          /connect saved in auth.json answers before this
;;;;                          point does.
;;;;
;;;; No request-body hook: the chat lane already carries no output cap unless
;;;; a limit is known or configured, which is the original's default_max_tokens
;;;; of None (see provider.lisp).
;;;;
;;;; Config, a sibling top-level key:
;;;;   "telnyx": {"base_url": "https://api.telnyx.com/v2/ai/openai"}
;;;; A vetoed section ("enabled": false) installs nothing, and Telnyx is then
;;;; whatever models.dev alone makes of it.

(in-package #:nodecode-telnyx)

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Telnyx's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Telnyx's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun credential (op next)
  "The :CREDENTIAL answer for telnyx: the key TELNYX_API_KEY holds, else the
one the provider's own entry names, else none."
  ;; Never NEXT for telnyx: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Telnyx.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell telnyx
  (:section ("telnyx")
    (:guide "make a key at https://portal.telnyx.com/#/app/api-keys; save it with /connect or set TELNYX_API_KEY; base_url is where Telnyx inference is served")
    ("base_url" :string :default +base+
     :doc "the Telnyx API base the chat lane appends /chat/completions to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
