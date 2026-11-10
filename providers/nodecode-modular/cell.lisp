;;;; cell.lisp --- the cell: Modular Cloud among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Two hooks, each declining for every provider but modular:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Modular's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the four models the original
;;;;                          describes with their windows, modalities and
;;;;                          ladders, so /connect offers Modular, /models
;;;;                          lists its models and a turn resolves its lane and
;;;;                          address the way it does any catalog provider's
;;;;   REQUEST-BODY           the chat lane's top-level reasoning_effort gives
;;;;                          way to Modular's nested `reasoning' object: the
;;;;                          model's own effort, lowered to a level it takes,
;;;;                          an `enabled' switch where it has no levels, and
;;;;                          `enabled: false' where turning thinking off is a
;;;;                          switch
;;;;
;;;; Config, a sibling top-level key:
;;;;   "modular": {"base_url": "https://api.modular.com/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; The key is the core's own: the provider id derives MODULAR_API_KEY, the
;;;; variable the original read, so no :CREDENTIAL hook is needed, and the
;;;; chat family's OPENAI_API_KEY never rides here because the endpoint is
;;;; Modular's own host.

(in-package #:nodecode-modular)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a modular round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Modular's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Modular's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row))
        (setf *catalog* (cons base merged))
        merged)))

(defun adjust-reasoning (body config)
  "BODY, a chat request for CONFIG's model, with Modular's nested `reasoning'
object in place of the chat lane's reasoning_effort: a level the model takes
for a model this cell describes, and no object at all where the model takes
none."
  (let* ((model (nle::effective-provider-config-model config))
         (effort (nle::effective-provider-config-reasoning-effort config))
         (row (model-row model)))
    ;; the wire has no top-level reasoning_effort: Modular reads its own object
    (remhash "reasoning_effort" body)
    (when row
      (destructuring-bind (id context efforts toggle off) row
        (declare (ignore id context))
        (let ((reasoning (reasoning-object efforts toggle off effort)))
          (when reasoning (setf (gethash "reasoning" body) reasoning)))))
    body))

(defun body (next context &aux (request (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a modular round asks in Modular's own reasoning
spelling."
  (when (and (ours-p config) (hash-table-p request))
    (adjust-reasoning request config))
  request)

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell modular
  (:section ("modular")
    (:guide "make a key at https://docs.modular.com/models; save it with /connect or set MODULAR_API_KEY; base_url is where Modular Cloud, or a gateway in front of it, is served")
    ("base_url" :string :default +base+
     :doc "the Modular Cloud API base the chat lane appends /chat/completions to"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::request-body #'body))
