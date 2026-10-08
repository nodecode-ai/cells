;;;; cell.lisp --- the cell: ClinePass among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but cline-pass:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries ClinePass's row: the chat
;;;;                          lane's package, this section's base, and omp's
;;;;                          bundled models over whatever models.dev
;;;;                          published, so /connect offers it, /models lists
;;;;                          its models and a turn resolves its lane and
;;;;                          address the way it does any catalog provider's
;;;;   :CREDENTIAL            a key from CLINE_API_KEY, the variable omp and
;;;;                          the Cline CLI read, then the core's own
;;;;                          CLINE_PASS_API_KEY, and no other variable; a key
;;;;                          /connect saved in auth.json answers before this
;;;;                          point does
;;;;   REQUEST-BODY           the chat body names the model by its wire id
;;;;   WALK-PROVIDER-STREAM   the request carries the Cline client headers
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cline-pass": {"base_url": "https://api.cline.bot/api/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and ClinePass is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-cline-pass)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a cline-pass round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
ClinePass's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with ClinePass's row, made once
per catalog the core reads."
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
  "The :CREDENTIAL answer for cline-pass: the key CLINE_API_KEY holds, else
the core's own CLINE_PASS_API_KEY, else none."
  ;; Never NEXT for cline-pass: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to
  ;; Cline.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a cline-pass round asks for its model's wire id."
  (when (and (ours-p config) (hash-table-p body))
    (setf (gethash "model" body) (wire-id (nle::effective-provider-config-model config))))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a cline-pass round carries the client headers."
  (if (ours-p config)
      (apply next fold :headers (append headers (client-headers))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell cline-pass
  (:section ("cline-pass")
    (:guide "make a key at https://app.cline.bot/dashboard/account (Settings, API Keys); save it with /connect or set CLINE_API_KEY; base_url is where ClinePass is served")
    ("base_url" :string :default +base+
     :doc "the ClinePass API base the chat lane appends /chat/completions to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk))
