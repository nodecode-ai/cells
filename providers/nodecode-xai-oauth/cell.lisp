;;;; cell.lisp --- the cell: xAI Grok OAuth among the organism's providers, and its sign-in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but xai-oauth, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries xai-oauth's row: the
;;;;                           Responses lane's package, this section's base,
;;;;                           and omp's bundled Grok models over whatever
;;;;                           models.dev published, so /models lists them and
;;;;                           a turn resolves the openai-responses lane and
;;;;                           https://api.x.ai/v1/responses
;;;;   :CREDENTIAL             the sign-in's token from auth.json, refreshed
;;;;                           first when it is about to expire, else
;;;;                           XAI_OAUTH_TOKEN; a key /connect saved answers
;;;;                           before this point does. It never falls through
;;;;                           to the lane's family default, which would send
;;;;                           OPENAI_API_KEY to xAI
;;;;   RESPONSES-REQUEST-BODY  the Grok dialect: reasoning asked only where a
;;;;                           model takes it, no summary, encrypted reasoning
;;;;                           kept, tool schemas xAI accepts
;;;;   WALK-PROVIDER-STREAM    the conversation in x-grok-conv-id
;;;;   /xai-oauth              login (a device code), logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "xai-oauth": {"base_url": "https://api.x.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and xai-oauth is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-xai-oauth)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an xai-oauth round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
xai-oauth's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with xai-oauth's row, made once
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
  "The :CREDENTIAL answer for xai-oauth: the sign-in's token, else
XAI_OAUTH_TOKEN, else the keyless placeholder."
  (if (equal (getf op :provider) +provider+)
      (or (token-credential op)
          (alexandria:when-let (key (env-key))
            (nle:make-credential key :env))
          (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "RESPONSES-REQUEST-BODY advice: an xai-oauth round speaks the Grok dialect."
  (if (and (ours-p config) (hash-table-p body))
      (grok-body body (nle::effective-provider-config-model config))
      body))

(defun session-id ()
  "The session the round on this thread belongs to, or NIL outside a turn."
  (let ((turn nle::*current-durable-turn*))
    (and turn (nlk:durable-turn-session-id turn))))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an xai-oauth round names its conversation
for xAI's prompt cache."
  (alexandria:if-let (session (and (ours-p config) (session-id)))
    (apply next fold :headers (append headers (list (cons +session-header+ session)))
           (alexandria:remove-from-plist keys :headers))
    (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Begin from a fresh catalog, and stop a sign-in in progress with the cell."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'cancel-flow))

(nle:define-cell xai-oauth
  (:section ("xai-oauth")
    (:guide "sign in with /xai-oauth login (a SuperGrok or X Premium+ account; open the address, enter the code), or set XAI_OAUTH_TOKEN; base_url is where xAI is served")
    ("base_url" :string :default +base+
     :doc "the xAI API base the Responses lane appends /responses to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::responses-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "xai-oauth" 'run-command
            :description "Sign in to xAI Grok (SuperGrok or X Premium+) with a device code"
            :argument-hint "login | logout | status"
            :session nil
            :complete 'complete-command))
