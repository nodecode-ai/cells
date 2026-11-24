;;;; cell.lisp --- the cell: Otari among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Three hooks, each declining for every provider but otari:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Otari's row: the chat
;;;;                          lane's package and this section's base, so
;;;;                          /connect offers it, /models lists the models
;;;;                          the gateway itself answers, and a turn resolves
;;;;                          its lane and address the way it does any
;;;;                          catalog provider's
;;;;   :CREDENTIAL            a key from OTARI_API_KEY, the variable the
;;;;                          original plugin reads, and no other variable; a
;;;;                          key /connect saved in auth.json answers before
;;;;                          this point does
;;;;   REQUEST-BODY           the round's reasoning_effort is spelled the way
;;;;                          Otari takes it: `max' as xhigh, and no rung the
;;;;                          gateway does not name
;;;;
;;;; Config, a sibling top-level key:
;;;;   "otari": {"base_url": "https://api.otari.ai/api/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Otari is then
;;;; whatever models.dev alone makes of it.

(in-package #:nodecode-otari)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an otari round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Otari's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Otari's row, made once per
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
  "The :CREDENTIAL answer for otari: the key OTARI_API_KEY holds, else the
core's own provider setting for it, else none."
  ;; Never NEXT for otari: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to the
  ;; gateway.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: an otari round asks for its effort in Otari's own
spelling, or leaves the field out."
  (when (and (ours-p config) (hash-table-p body))
    (multiple-value-bind (effort present) (gethash "reasoning_effort" body)
      (when present
        (alexandria:if-let (spelled (wire-effort effort))
          (setf (gethash "reasoning_effort" body) spelled)
          (remhash "reasoning_effort" body)))))
  body)

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell otari
  (:section ("otari")
    (:guide "make a key at https://otari.ai; save it with /connect or set OTARI_API_KEY; base_url is the gateway's own address, /api/v1 included — the EU host or a self-hosted Otari goes here")
    ("base_url" :string :default +base+
     :doc "the gateway's API base the chat lane appends /chat/completions to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body))
