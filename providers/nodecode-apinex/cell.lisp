;;;; cell.lisp --- the cell: APInex among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but the two this cell names:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries both APInex rows: the
;;;;                          Messages lane's package for `apinex', the
;;;;                          chat lane's for `apinex-openai', this section's
;;;;                          base, and the bundled models split by the rule
;;;;                          the original applied to /v1/models, so /connect
;;;;                          offers them, /models lists their models and a
;;;;                          turn resolves its lane and address the way it
;;;;                          does any catalog provider's
;;;;   :CREDENTIAL            the key APINEX_API_KEY holds, for both, and no
;;;;                          other variable; a key /connect saved in
;;;;                          auth.json answers before this point does
;;;;
;;;; Config, a sibling top-level key:
;;;;   "apinex": {"base_url": "https://api.apinex.bond/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and APInex is then
;;;; whatever models.dev alone makes of it.

(in-package #:nodecode-apinex)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an APInex round."
  (and config (member (nle::effective-provider-config-provider config) +providers+
                       :test #'equal)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
APInex's two rows in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with both APInex rows, made once
per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal))
            (previous (and (hash-table-p base) base)))
        (when previous
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) previous))
        (setf (gethash +anthropic-provider+ merged)
              (catalog-row t (and previous (gethash +anthropic-provider+ previous)))
              (gethash +openai-provider+ merged)
              (catalog-row nil (and previous (gethash +openai-provider+ previous))))
        (setf *catalog* (cons base merged))
        merged)))

(defun credential (op next)
  "The :CREDENTIAL answer for either APInex provider: the key APINEX_API_KEY
holds, else none."
  ;; Never NEXT for APInex: the ladder behind this point falls back to the
  ;; lane family's default variable, and would send OPENAI_API_KEY or
  ;; ANTHROPIC_API_KEY to APInex.
  (if (member (getf op :provider) +providers+ :test #'equal)
      (alexandria:if-let (key (or (env-key) (some #'nle::provider-env-key +providers+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell apinex
  (:section ("apinex")
    (:guide "make a key at https://apinex.bond/register; save it with /connect or set APINEX_API_KEY; base_url is where the gateway is served")
    ("base_url" :string :default +base+
     :doc "the APInex gateway base: the Messages lane appends /messages to it and the chat lane /chat/completions"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
