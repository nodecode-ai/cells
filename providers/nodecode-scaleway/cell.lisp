;;;; cell.lisp --- the cell: Scaleway among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but scaleway:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Scaleway's row: the chat
;;;;                          lane's package, this section's base, and the
;;;;                          curated tool-calling models with the numbers
;;;;                          models.dev publishes for them, so /connect offers
;;;;                          it, /models lists its models and a turn resolves
;;;;                          its lane and address the way it does any catalog
;;;;                          provider's
;;;;   :CREDENTIAL            a key from SCW_SECRET_KEY (the one the Scaleway
;;;;                          CLI exports) or SCALEWAY_API_KEY, and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;
;;;; Config, a sibling top-level key:
;;;;   "scaleway": {"base_url": "https://api.scaleway.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and Scaleway is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-scaleway)

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Scaleway's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Scaleway's row, made once
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
  "The :CREDENTIAL answer for scaleway: the key SCW_SECRET_KEY holds, else
SCALEWAY_API_KEY, else none."
  ;; Never NEXT for scaleway: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Scaleway.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell scaleway
  (:section ("scaleway")
    (:guide "make a secret key at https://console.scaleway.com/iam/api-keys (the IAM principal needs GenerativeApisModelAccess); save it with /connect or set SCW_SECRET_KEY; base_url is where the API is served, and SCALEWAY_BASE_URL sets its default")
    ("base_url" :string :default +base+
     :doc "the base the chat lane appends /chat/completions to; SCALEWAY_BASE_URL sets the default"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
