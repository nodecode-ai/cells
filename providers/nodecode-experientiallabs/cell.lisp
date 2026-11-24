;;;; cell.lisp --- the cell: Experiential Labs among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but experientiallabs, and
;;;; one slash command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Experiential Labs' row: the
;;;;                          chat lane's package, this section's base, and the
;;;;                          curated fallback models over whatever models.dev
;;;;                          published, so /connect offers it, /models lists
;;;;                          its models and a turn resolves its lane and
;;;;                          address the way it does any catalog provider's
;;;;   :CREDENTIAL            a key from EXPLABS_API_KEY, then
;;;;                          EXPERIENTIALLABS_API_KEY, and no other variable;
;;;;                          a key /connect saved in auth.json answers before
;;;;                          this point does
;;;;   /explabs                the wallet balance and the daily key cap from
;;;;                          the gateway's Cost API (usage.lisp)
;;;;
;;;; Config, a sibling top-level key:
;;;;   "experientiallabs": {"base_url": "https://api.experientiallabs.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Experiential
;;;; Labs is then whatever models.dev alone makes of it.

(in-package #:nodecode-experientiallabs)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an experientiallabs round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Experiential Labs' row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Experiential Labs' row, made
once per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test #'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun credential (op next)
  "The :CREDENTIAL answer for experientiallabs: the key EXPLABS_API_KEY holds,
else EXPERIENTIALLABS_API_KEY, else none."
  ;; Never NEXT for experientiallabs: the ladder behind this point falls back
  ;; to the chat family's default variable, and would send OPENAI_API_KEY to
  ;; the gateway.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell experientiallabs
  (:section ("experientiallabs")
    (:guide "make a key at https://platform.experientiallabs.ai/settings/api-keys; save it with /connect or set EXPLABS_API_KEY; base_url is where the gateway is served; /explabs reads the wallet and the daily key cap")
    ("base_url" :string :default +base+
     :doc "the gateway base the chat lane appends /chat/completions to")
    ("cost_api_base" :string :default +cost-base+
     :doc "the Cost API base /explabs reads GET /credits and GET /key from")
    ("usage_timeout_seconds" :integer :default 4 :min 1
     :doc "one Cost API read's ceiling, in seconds"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:command "explabs" #'run-command
            :description "Experiential Labs: wallet balance and daily key cap"))
