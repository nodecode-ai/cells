;;;; cell.lisp --- the cell: OpenPaths among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but openpaths:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries OpenPaths' row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variable, and the models OpenPaths last named
;;;;                          (its fallback list while it has named none), so
;;;;                          /connect offers it, /models lists its models
;;;;                          and a turn resolves its lane and address the
;;;;                          way it does any catalog provider's
;;;;   REQUEST-BODY           reasoning_effort goes out in the form OpenPaths
;;;;                          takes it: none/low/medium/high, `none' where
;;;;                          reasoning was turned off, and `auto' for an
;;;;                          auto-think route with no pick at all
;;;;
;;;; Config, a sibling top-level key:
;;;;   "openpaths": {"base_url": "https://openpaths.io/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and OpenPaths is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-openpaths)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an openpaths round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* nil
  "(CATALOG LIVE-STAMP . MERGED): the last root the core answered, the
/models read the row was built from, and that catalog with OpenPaths' row
in it.")

(defun catalog (next)
  "MODELS-CATALOG-TABLE advice: the catalog with OpenPaths' row, made once
per catalog the core reads and once more when OpenPaths' model list changes."
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

(defun body (next context &aux (request (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: an openpaths round sends the level the route takes."
  (when (and (ours-p config) (hash-table-p request))
    (let* ((model (nle::effective-provider-config-model config))
           (effort (nle::effective-provider-config-reasoning-effort config))
           (off (and (stringp effort) (string-equal effort "off")))
           (level (reasoning-effort-for model (gethash "reasoning_effort" request) off)))
      (if level
          (setf (gethash "reasoning_effort" request) level)
          (remhash "reasoning_effort" request))))
  request)

(defun forget-catalog ()
  "Drop the merged catalog and the model list, so a restart with another
base builds both again."
  (setf *catalog* nil)
  (forget-live))

(nle:define-cell openpaths
  (:section ("openpaths")
    (:guide "make a key at https://openpaths.io/; save it with /connect or set OPENPATHS_API_KEY; base_url is where OpenPaths is served")
    ("base_url" :string :default +base+
     :doc "the OpenPaths API base the chat lane appends /chat/completions to"))
  (:start (lambda ()
            (forget-catalog)
            ;; one bounded read at start, so the first picker already lists
            ;; OpenPaths' models; a box with no key or no network keeps the
            ;; fallback list
            (refresh-live)
            (nle:on-stop #'forget-catalog)
            (let ((worker (nlk:worker-start "openpaths-models" #'refresh-live
                                            :wake +refresh-seconds+)))
              (nle:on-stop (lambda () (nlk:worker-stop worker))))))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::request-body #'body))
