;;;; cell.lisp --- the cell: BharatCode among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks, each declining for every provider but bharatcode:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries BharatCode's row: the chat
;;;;                          lane's package, this section's base, and the
;;;;                          models the provider's own /models last named (the
;;;;                          original's fallback pair while it has named
;;;;                          none), so /connect offers it, /models lists its
;;;;                          models and a turn resolves its lane and address
;;;;                          the way it does any catalog provider's
;;;;   :CREDENTIAL            a key from BHARATCODE_API_KEY, the variable the
;;;;                          original reads, and no other variable; a key
;;;;                          /connect saved in auth.json answers before this
;;;;                          point does
;;;;
;;;; Config, a sibling top-level key:
;;;;   "bharatcode": {"base_url": "https://bharatcode.ai/api/model/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and BharatCode is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-bharatcode)

(defvar *catalog* nil
  "(ROOT STAMP . MERGED): the last catalog the core answered, the provider
model list that row was built from, and that catalog with BharatCode's row in
it.")

(defun catalog (next)
  "MODELS-CATALOG-TABLE advice: the catalog with BharatCode's row, made once
per catalog the core reads and once more when the provider's list changes."
  (let ((root (funcall next)) (stamp (car-safe *live*)) (memo *catalog*))
    (if (and memo (eq (first memo) root) (equal (second memo) stamp))
        (third memo)
        (let ((merged (make-hash-table :test 'equal)))
          (when (hash-table-p root)
            (maphash (lambda (id provider) (setf (gethash id merged) provider)) root))
          (setf (gethash +provider+ merged)
                (catalog-row (and (hash-table-p root) (gethash +provider+ root))))
          (setf *catalog* (list root stamp merged))
          merged))))

(defun credential (op next)
  "The :CREDENTIAL answer for bharatcode: the key BHARATCODE_API_KEY holds,
else the core's own BHARATCODE_API_KEY rung, else none."
  ;; Never NEXT for bharatcode: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to BharatCode.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun forget-catalog ()
  "Drop the merged catalog and the provider's list, so a restart with another
base builds both again."
  (setf *catalog* nil)
  (forget-live))

(nle:define-cell bharatcode
  (:section ("bharatcode")
    (:guide "make a key at https://bharatcode.ai (free for students with an .ac.in or .edu.in address); save it with /connect or set BHARATCODE_API_KEY; base_url is where BharatCode is served")
    ("base_url" :string :default +base+
     :doc "the BharatCode OpenAI-compatible base the chat lane appends /chat/completions to"))
  (:start (lambda ()
            (forget-catalog)
            ;; one bounded read at start, so the first picker already knows the
            ;; models the account may use; a provider that is down answers
            ;; nothing and start goes on
            (refresh-live)
            (nle:on-stop #'forget-catalog)
            (let ((worker (nlk:worker-start "bharatcode-catalog" #'refresh-live
                                            :wake +refresh-seconds+)))
              (nle:on-stop (lambda () (nlk:worker-stop worker))))))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
