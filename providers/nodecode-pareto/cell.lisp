;;;; cell.lisp --- the cell: Pareto Inference among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but pareto:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Pareto's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the three models the original
;;;;                          names with the capabilities it declares for
;;;;                          them, so /connect offers it, /models lists its
;;;;                          models and a turn resolves its lane and address
;;;;                          the way it does any catalog provider's
;;;;   REQUEST-BODY           the model goes out as the `vendor/model' id the
;;;;                          API requires, and reasoning_effort in Pareto's
;;;;                          own spelling -- `none' for Nodecode's `off',
;;;;                          `max' for `ultra', nothing for a rung the API
;;;;                          does not list
;;;;
;;;; Config, a sibling top-level key:
;;;;   "pareto": {"base_url": "https://api.paretoinference.com/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; The key is the core's own: the provider id derives PARETO_API_KEY, so no
;;;; :CREDENTIAL hook is needed, and no other family's variable rides here
;;;; because the endpoint is Pareto's own host.

(in-package #:nodecode-pareto)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a pareto round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Pareto's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Pareto's row, made once per
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

(defun body (next context &aux (request (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a pareto round names its model by the namespaced id
and carries reasoning_effort in Pareto's spelling."
  (when (and (ours-p config) (hash-table-p request))
    (setf (gethash "model" request)
          (wire-model (nle::effective-provider-config-model config)))
    (let ((level (wire-effort (nle::effective-provider-config-reasoning-effort config))))
      (if level
          (setf (gethash "reasoning_effort" request) level)
          (remhash "reasoning_effort" request))))
  request)

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell pareto
  (:section ("pareto")
    (:guide "make a key at https://paretoinference.com/dashboard; save it with /connect or set PARETO_API_KEY; base_url is where Pareto is served")
    ("base_url" :string :default +base+
     :doc "the Pareto Inference API base the chat lane appends /chat/completions to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::request-body #'body))
