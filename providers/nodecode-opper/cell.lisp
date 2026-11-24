;;;; cell.lisp --- the cell: Opper among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but opper:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Opper's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the roster, so /connect offers
;;;;                          it, /models lists its models and a turn
;;;;                          resolves its lane and address the way it does
;;;;                          any catalog provider's
;;;;   :CREDENTIAL            a key from OPPER_API_KEY, and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;   WALK-PROVIDER-STREAM   a round with no key refuses in words, naming
;;;;                          the variable to set, before it earns a 401
;;;;
;;;; The listing is the core's own: it GETs <base>/models, the catalogue the
;;;; plugin's docstring names, so no hook stands in front of it. A model id
;;;; rides the round as it was picked -- a bare pool name, or the vendor
;;;; prefix that pins one route -- so REQUEST-BODY has nothing to rewrite.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "opper": {"base_url": "https://api.opper.ai/v3/compat"}
;;;; A vetoed section ("enabled": false) installs nothing, and Opper is then
;;;; whatever models.dev alone makes of it.

(in-package #:nodecode-opper)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an opper round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog
with Opper's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Opper's row, made once per
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
  "The :CREDENTIAL answer for opper: the key OPPER_API_KEY holds, else
none."
  ;; Never NEXT for opper: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to the
  ;; gateway.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun walk (next fold &rest keys &key config &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an opper round with no key refuses in words,
naming the variable, instead of asking the gateway to say 401."
  (if (ours-p config)
      (progn
        (require-key (nle::effective-provider-config-api-key config))
        (apply next fold keys))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell opper
  (:section ("opper")
    (:guide "make a key at https://platform.opper.ai; save it with /connect or set OPPER_API_KEY; base_url is where the gateway is served")
    ("base_url" :string :default +base+
     :doc "the Opper gateway base the chat lane appends /chat/completions to"))
  (:settings 'read-settings)
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::walk-provider-stream #'walk))