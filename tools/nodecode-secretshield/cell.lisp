;;;; cell.lisp --- the shield on the :TOOL point, the note, the tool, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT THE ORIGINAL DID, AND WHERE IT LANDS HERE. The plugin hooked four
;;;; phases of the harness: before a tool call, after one, before a model
;;;; request, and on the finished reply. Nodecode has one interception seam --
;;;; the core's hook chain on the :TOOL point, (lambda (op next)) -- where a
;;;; hook may rewrite the call by rebuilding OP, answer in its place by not
;;;; calling NEXT, or wrap it. So the shield lands there, on both sides of one
;;;; dispatch, and in two more places Nodecode has and Hermes did not:
;;;;   - enforce (the default): a call whose arguments carry credential
;;;;     material never runs. The model gets the refusal in the call's place,
;;;;     with the kinds found and where they sat, and the attempt is one line
;;;;     in the ledger.
;;;;   - redact: the call runs with the credentials replaced by
;;;;     [REDACTED by SecretShield].
;;;;   - warn: the call runs untouched and the attempt is recorded.
;;;;   - off: the cell observes nothing and changes nothing.
;;;; The original's pre_llm_call note is one :ROUND hook: the first round of a
;;;; session states what the shield does, so a model told to send a token
;;;; through a shell call knows why it was refused. The original's
;;;; transform_llm_output -- rewriting the finished reply -- has no seam here
;;;; that is not the streaming frame, so a credential quoted in a reply is
;;;; caught on its way back out as a tool call, or by /secretshield over text
;;;; the model has in hand.
;;;;
;;;; NOT a confidentiality guarantee: the detectors are the original's
;;;; patterns, and the engine's own masking still runs underneath. A call that
;;;; a format nobody wrote down slips past, slips past here too.

(in-package #:nodecode-secretshield)

(defparameter +modes+ '("enforce" "redact" "warn" "off")
  "What the `mode' setting may hold.")

(defparameter +tool-name+ "shield_status")

(defparameter *disabled* '()
  "The detector kinds the running cell skips.")

(defparameter *blocked* 0
  "How many calls were stopped since the cell started.")

(defparameter *redacted* 0
  "How many calls had credentials rewritten since the cell started.")

(defparameter *told* (make-hash-table :test #'equal)
  "Sessions the first-round note has already been stated in.")

(defparameter +note+
  "SecretShield is active in this profile: a tool call whose arguments carry
credential material (an API key, a token, a password, a private key) is stopped
or has it rewritten, and every attempt is one line in a local incident ledger
of metadata only. Put credentials in the tool's own configuration, never in a
call's arguments."
  "What the model reads in a session's first round.")

(defparameter +manual+
  "Credentials stop at the tool boundary. Every tool call's arguments are read
before the tool runs: a vendor key (OpenAI, Anthropic, GitHub, AWS, Google,
Slack, Stripe, SendGrid, npm, PyPI, Telegram, Discord), a JWT, a PEM private
key, a bearer credential or a `password = ...' assignment found there stops the
call in enforce mode, has it replaced in redact mode, and is recorded either
way. The ledger keeps metadata only -- the kind, its length and four masked
characters -- under ~/.nodecode/secretshield/incidents.jsonl, never the secret.
  /secretshield            the ledger, and the mode the shield runs in
  /secretshield TEXT       the same text as it would leave, with what was found
  shield_status             the same ledger as JSON, for the model
Modes: enforce (the default, calls stopped), redact (arguments rewritten),
warn (recorded, nothing changed), off."
  "What (help :secretshield) answers while the cell runs.")

;;; --- the mode, and the one incident line ---------------------------------------

(defun mode ()
  "The mode the shield runs in, `enforce' unless the section says otherwise."
  (let ((value (ignore-errors (setting :mode))))
    (if (and (stringp value) (member value +modes+ :test #'string=)) value "enforce")))

(defun active-p ()
  "Whether the shield looks at anything at all."
  (not (string= (mode) "off")))

;;; --- the :TOOL point -----------------------------------------------------------

(defun rebuild-op (op arguments)
  "OP with its :ARGUMENTS replaced: the way a point hook rewrites a call."
  (let ((copy (copy-list op)))
    (setf (getf copy :arguments) arguments)
    copy))

(defun refusal-text (tool findings)
  "What the model reads in place of a call the shield stopped."
  (format nil "ERROR: blocked by SecretShield: the arguments of this ~@[~a ~]call carry credential material (~a). Credentials must not be passed through tool calls: remove it, use the tool's own authenticated configuration, and retry. The attempt is one line in the local incident ledger (/secretshield)."
          tool (describe-findings findings)))

(defun scanner-error-text (tool)
  "What the model reads when the scanner itself failed in enforce mode."
  (format nil "ERROR: blocked by SecretShield: the arguments of this ~@[~a ~]call could not be scanned (an internal scanner error), so the call was stopped rather than run unchecked. /secretshield says how often this happens."
          tool))

(defun note-result (tool result)
  "A credential a tool PRINTED is an incident too, though the call already ran."
  (when (and (stringp result) (plusp (length result)) (active-p))
    (let ((findings (ignore-errors (scan-text result *disabled*))))
      (when findings (record-incident "tool_result" "detected" findings :tool tool)))))

(defun shield (op next)
  "The :TOOL point. OP is (:name TOOL-NAME :arguments HASH :call-id ID)."
  (if (not (active-p))
      (funcall next op)
      (let* ((tool (getf op :name))
             (arguments (getf op :arguments))
             (findings (handler-case (scan-arguments arguments *disabled*)
                         (error () :failed))))
        (cond
          ;; Fail closed in enforce mode: a call the scanner choked on is not run.
          ((eq findings :failed)
           (if (string= (mode) "enforce")
               (progn (record-incident "tool_call" "blocked_scanner_error" '() :tool tool)
                      (scanner-error-text tool))
               (let ((result (funcall next op))) (note-result tool result) result)))
          ((null findings)
           (let ((result (funcall next op))) (note-result tool result) result))
          ((string= (mode) "warn")
           (record-incident "tool_call" "warned" findings :tool tool)
           (let ((result (funcall next op))) (note-result tool result) result))
          ((string= (mode) "redact")
           (record-incident "tool_call" "redacted_args" findings :tool tool)
           (incf *redacted*)
           (let ((result (funcall next (rebuild-op op (redact-arguments arguments *disabled*)))))
             (note-result tool result) result))
          (t
           (record-incident "tool_call" "blocked" findings :tool tool)
           (incf *blocked*)
           (refusal-text tool findings))))))

;;; --- the note in a session's first round ------------------------------------------

(defun tell-model (op next)
  "The :ROUND point: state the shield once per session, in its first round."
  (when (and (active-p) (getf op :turn) (getf op :session-id)
             (= 1 (or (getf op :round) 0))
             (not (gethash (getf op :session-id) *told*)))
    (setf (gethash (getf op :session-id) *told*) t)
    (ignore-errors (nlk:record-turn-context-prompt (getf op :turn) :text +note+)))
  (funcall next op))

;;; --- the tool -------------------------------------------------------------------

(defparameter +schema+
  (list :description "Return SecretShield's recent incident ledger: the credentials blocked or rewritten in tool calls, and those seen in tool results. Metadata only -- kind, length, four masked characters."
        :parameters (nlk:make-json-object "type" "object"
                                          "properties" (nlk:make-json-object)))
  "shield_status takes nothing: an object with no members.")

(defun shield-status-tool (&optional arguments)
  "The shield_status tool: the ledger as JSON."
  (declare (ignore arguments))
  (let ((rows (recent-incidents)))
    (nlk:encode-json-object
     (json-object* "success" t
                   "mode" (mode)
                   "incidents_recent" (length rows)
                   "latest" (coerce (last rows (min 5 (length rows))) 'vector)
                   "ledger" (namestring (incident-path))
                   "note" "Metadata only: kind, length, four masked characters."))))

;;; --- /secretshield ---------------------------------------------------------------

(defun preview-text (text)
  "TEXT as it would leave, and what was found in it."
  (multiple-value-bind (redacted findings) (redact-text text *disabled*)
    (format nil "~a~%~a"
            (or redacted text)
            (if findings
                (format nil "found ~d credential~:p: ~a" (length findings)
                        (describe-findings findings))
                "found nothing: the text comes back as it was"))))

(defun run-slash (args session-id)
  "/secretshield: the ledger and the mode, or a preview of ARGS."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (values (if (plusp (length text))
                (preview-text text)
                (format nil "SecretShield is in ~a mode~@[ (skipping ~{~a~^, ~})~]; ~d call~:p stopped, ~d rewritten since the start.~%~a"
                        (mode) *disabled* *blocked* *redacted* (incident-text)))
            nil)))

;;; --- the section the cell runs on -------------------------------------------------

(defun read-settings (values table)
  "VALUES from the declaration, with an unknown detector kind refused before
the cell starts, so a typo is a REFUSED line and not a shield that skips
nothing."
  (declare (ignore table))
  (let ((kinds '()))
    (dolist (kind (getf values :disabled-kinds))
      (let ((name (nlk:trimmed kind)))
        (unless (member name (detector-kinds) :test #'string=)
          (nlk:config-error "secretshield.disabled_kinds names ~s, which is not a detector this cell has; it knows ~d of them, such as ~s and ~s"
                            kind (length (detector-kinds))
                            (first (detector-kinds)) (second (detector-kinds))))
        (pushnew name kinds :test #'string=)))
    (setf (getf values :disabled-kinds) (nreverse kinds))
    values))

(nlk:define-peripheral secretshield :not-running t)

(nle:define-cell secretshield
  (:section ("secretshield")
    (:guide "nothing needs setting: the default enforce stops a call whose arguments carry a credential; redact rewrites them, warn only records, off observes nothing; disabled_kinds names detectors to skip (openai_key, jwt, assigned_secret, ...)")
    ("mode" :choice :options '("enforce" "redact" "warn" "off") :default "enforce"
            :doc "enforce stops the call, redact replaces the credential, warn only records, off does nothing")
    ("disabled_kinds" :list
                      :doc "detector kinds to skip, as the table spells them (openai_key, jwt, assigned_secret)"))
  (:settings #'read-settings)
  (:hook :tool #'shield)
  (:hook :round #'tell-model)
  (:tool +tool-name+ +schema+ 'shield-status-tool)
  (:command "secretshield" 'run-slash
            :description "The credential incident ledger, or a preview of any text"
            :argument-hint "[TEXT]")
  (:help :secretshield
         "secretshield: stops credentials from passing through tool calls" +manual+)
  (:start (lambda ()
            (setf *disabled* (or (setting :disabled-kinds) '())
                  *blocked* 0
                  *redacted* 0
                  *told* (make-hash-table :test #'equal))
            (when (active-p)
              (nle:notice (format nil "secretshield: ~a mode, ~d detector~:p; the ledger is /secretshield"
                                  (mode) (- (length (detector-kinds)) (length *disabled*)))
                          :key "nodecode-secretshield"))
            (nle:on-stop (lambda ()
                           (setf *disabled* '() *blocked* 0 *redacted* 0
                                 *told* (make-hash-table :test #'equal)))))))
