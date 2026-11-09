;;;; cell.lisp --- the cell: Groq among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but groq:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Groq's row: the chat lane's
;;;;                          package, this section's base, the key variable,
;;;;                          and the models Groq last named (models.dev's own
;;;;                          Groq rows filtered while it has named none), so
;;;;                          /connect offers it, /models lists its models and
;;;;                          a turn resolves its lane and address the way it
;;;;                          does any catalog provider's
;;;;   REQUEST-BODY           reasoning_effort goes out in the form the model
;;;;                          takes it: lowered to the nearest level it
;;;;                          accepts, `none' where it takes that, and left
;;;;                          out of a model that takes none at all
;;;;
;;;; Config, a sibling top-level key:
;;;;   "groq": {"base_url": "https://api.groq.com/openai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; The key is the core's own: the provider id derives GROQ_API_KEY, so no
;;;; :CREDENTIAL hook is needed, and the family's OPENAI_API_KEY never rides
;;;; here because the endpoint is Groq's own host.

(in-package #:nodecode-groq)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a groq round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* nil
  "(CATALOG LIVE-STAMP . MERGED): the last root the core answered, the
/models read the row was built from, and that catalog with Groq's row in it.")

(defun catalog (next)
  "MODELS-CATALOG-TABLE advice: the catalog with Groq's row, made once per
catalog the core reads and once more when Groq's model list changes."
  (let ((root (funcall next)) (stamp (live-stamp)) (memo *catalog*))
    (if (and memo (eq (first memo) root) (eql (second memo) stamp))
        (third memo)
        (let ((merged (make-hash-table :test 'equal)))
          (when (hash-table-p root)
            (maphash (lambda (id provider) (setf (gethash id merged) provider)) root))
          (setf (gethash +provider+ merged)
                (catalog-row (and (hash-table-p root) (gethash +provider+ root))))
          (setf *catalog* (list root stamp merged))
          merged))))

(defun adjust-reasoning (body config)
  "BODY, a chat request for CONFIG's model, with reasoning_effort as Groq
takes it for that model: the request's own pick when the model accepts it,
the nearest weaker level it does, and no field at all for a model that takes
none."
  (let* ((model (nle::effective-provider-config-model config))
         (requested (gethash "reasoning_effort" body))
         (effort (nle::effective-provider-config-reasoning-effort config))
         (off (and (stringp effort) (string-equal effort "off")))
         (level (reasoning-effort-for model requested off)))
    (if level
        (setf (gethash "reasoning_effort" body) level)
        (remhash "reasoning_effort" body))
    body))

(defun body (next context &aux (request (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a groq round asks for a level its model takes."
  (when (and (ours-p config) (hash-table-p request))
    (adjust-reasoning request config))
  request)

(defun forget-catalog ()
  "Drop the merged catalog and the model list, so a restart with another base
builds both again."
  (setf *catalog* nil)
  (forget-live))

(nle:define-cell groq
  (:section ("groq")
    (:guide "make a key at https://console.groq.com/keys; save it with /connect or set GROQ_API_KEY; base_url is where Groq is served")
    ("base_url" :string :default +base+
     :doc "the Groq API base the chat lane appends /chat/completions to"))
  (:start (lambda ()
            (forget-catalog)
            ;; one bounded read at start, so the first picker already lists
            ;; Groq's models; a box with no key or no network keeps the
            ;; catalog's own rows
            (refresh-live)
            (nle:on-stop #'forget-catalog)
            (let ((worker (nlk:worker-start "groq-models" #'refresh-live
                                            :wake +refresh-seconds+)))
              (nle:on-stop (lambda () (nlk:worker-stop worker))))))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::request-body #'body))
