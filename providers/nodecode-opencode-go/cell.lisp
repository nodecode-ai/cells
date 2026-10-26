;;;; cell.lisp --- the cell: OpenCode Go among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but opencode-go:
;;;;
;;;;   MODELS-CATALOG-TABLE   models.dev's OpenCode Go row with this section's
;;;;                          base, the one every Go wire answers under
;;;;   :CREDENTIAL            a key from OPENCODE_API_KEY, then the core's own
;;;;                          OPENCODE_GO_API_KEY, and no other variable; a
;;;;                          key /connect saved in auth.json answers before
;;;;                          this point does
;;;;   RESOLVE-MODEL-LANE     each model on the wire Go serves it on, unless
;;;;                          the operator's config names an sdk
;;;;   WALK-PROVIDER-STREAM   every round carries x-opencode-session, which
;;;;                          Go refuses a request without
;;;;
;;;; Config, a sibling top-level key:
;;;;   "opencode-go": {"base_url": "https://opencode.ai/inference/go/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and OpenCode Go is
;;;; then whatever models.dev alone makes of it: every request refused.

(in-package #:nodecode-opencode-go)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an opencode-go round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
OpenCode Go's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with OpenCode Go's row, made once
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
  "The :CREDENTIAL answer for opencode-go: the key OPENCODE_API_KEY holds,
else the core's own OPENCODE_GO_API_KEY, else none."
  ;; Never NEXT for opencode-go: the ladder behind this point falls back to
  ;; the lane family's default variable, and would send OPENAI_API_KEY or
  ;; ANTHROPIC_API_KEY to OpenCode.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun operator-sdk-p (model &aux (entry (nle::configured-provider-entry +provider+)))
  "Whether the operator's config names the wire for MODEL on opencode-go: an
sdk on the provider, or on that model's row."
  (and entry
       (or (nle::trimmed-config-string entry "sdk")
           (alexandria:when-let (row (nle::configured-model-entry +provider+ model))
             (nle::trimmed-config-string row "sdk")))
       t))

(defun lane (next provider model)
  "RESOLVE-MODEL-LANE advice: an opencode-go model rides the wire Go serves
it on, unless the operator named one."
  (if (and (equal provider +provider+) (stringp model) (not (operator-sdk-p model)))
      (model-lane model)
      (funcall next provider model)))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an opencode-go round carries the session header."
  (if (ours-p config)
      (apply next fold :headers (session-headers headers)
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell opencode-go
  (:section ("opencode-go")
    (:guide "make a key at https://opencode.ai/auth (subscribe to Go first); save it with /connect or set OPENCODE_API_KEY; base_url is where Go is served")
    ("base_url" :string :default +base+
     :doc "the OpenCode Go base every lane appends its path to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::resolve-model-lane #'lane)
  (:hook 'nle::walk-provider-stream #'walk))
