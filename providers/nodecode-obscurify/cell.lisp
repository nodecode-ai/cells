;;;; cell.lisp --- the cell: Obscurify.ai among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but obscurify:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Obscurify.ai's row: the
;;;;                          chat lane's package, this section's base, the
;;;;                          key variable and the models its public catalog
;;;;                          names (the plugin's featured list while it has
;;;;                          named none), so /connect offers it, /models lists
;;;;                          its models and a turn resolves its lane and
;;;;                          address the way it does any catalog provider's
;;;;   :CREDENTIAL            a key from OBSCURIFY_API_KEY and no other
;;;;                          variable; a key /connect saved in auth.json
;;;;                          answers before this point does
;;;;   REQUEST-BODY           reasoning_effort goes out as the API spells it,
;;;;                          `none' where the round turned reasoning off
;;;;
;;;; Config, a sibling top-level key:
;;;;   "obscurify": {"base_url": "https://obscurify.ai/v1",
;;;;                 "min_context_tokens": 64000, "timeout_seconds": 8}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-obscurify)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an obscurify round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* nil
  "(ROOT STAMP . MERGED): the last root the core answered, the /v1/models
read the row was built from, and that catalog with Obscurify.ai's row in it.")

(defun catalog (next &aux (root (funcall next)) (stamp (live-stamp)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Obscurify.ai's row, made
once per catalog the core reads and once more when its model list changes."
  (if (and memo (eq (first memo) root) (eql (second memo) stamp))
      (third memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p root)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) root))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p root) (gethash +provider+ root))))
        (setf *catalog* (list root stamp merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog and the rows read, so a restart with another
base, another floor, or another model list builds both again."
  (setf *catalog* nil)
  (forget-live))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: an obscurify round asks for reasoning_effort the way
the API spells it, so a round that turned reasoning off sends `none'."
  ;; The chat lane already writes the rung when one is chosen -- this
  ;; vocabulary's names are the API's own -- and leaves the field out for
  ;; `off', which is the one spelling this cell changes.
  (when (and (ours-p config) (hash-table-p body))
    (alexandria:when-let ((effort (wire-effort
                                    (nle::effective-provider-config-reasoning-effort config))))
      (setf (gethash "reasoning_effort" body) effort)))
  body)

(nle:define-cell obscurify
  (:section ("obscurify")
    (:guide "make a key at https://obscurify.ai/account; save it with /connect or set OBSCURIFY_API_KEY; base_url is where Obscurify.ai is served; min_context_tokens leaves out the models the catalog says are smaller")
    ("base_url" :string :default +base+
     :doc "the Obscurify.ai API base the chat lane appends /chat/completions to")
    ("min_context_tokens" :integer :default +min-context+ :min 0
     :doc "leave out catalog models whose context window is under this many tokens")
    ("timeout_seconds" :integer :default 8 :min 1
     :doc "one catalog read's ceiling, in seconds"))
  (:start (lambda ()
            (forget-catalog)
            ;; one bounded read at start, so the first picker already lists
            ;; Obscurify.ai's models; a box with no network keeps the
            ;; featured list
            (refresh-live)
            (nle:on-stop #'forget-catalog)
            (let ((worker (nlk:worker-start "obscurify-models" #'refresh-live
                                            :wake +refresh-seconds+)))
              (nle:on-stop (lambda () (nlk:worker-stop worker))))))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body))
