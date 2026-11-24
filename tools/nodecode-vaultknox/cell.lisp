;;;; cell.lisp --- the hook on the wire fold, the tool, the command, the section.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; WHERE THE TEXT IS CAUGHT. The organism keeps its log and sends a
;;;; projection of it: WIRE-MESSAGES-FROM-EVENTS folds durable facts into the
;;;; provider-shaped messages of one turn, and the request is built from
;;;; those. The cell advises that fold, so a secret value written in a user
;;;; message, echoed in an assistant reply or printed by a tool result is
;;;; replaced with a placeholder on the way out -- and the log, and every
;;;; transcript read off it, keeps what was written. The copy carries the
;;;; folded message's log position, so the editable context still names it.
;;;;
;;;; An assistant turn is read twice: its secret values are redacted, and a
;;;; phrase that asks the operator to paste a key is rewritten into safe
;;;; guidance, which is what the plugin's transform_llm_output hook did to a
;;;; reply before it was shown.
;;;;
;;;; FAIL-OPEN, as the original was: a detector that throws passes the text
;;;; through, counting the failure and saying it once on the operator's board.

(in-package #:nodecode-vaultknox)

(defparameter +tool-name+ "scan_secrets")

(defvar *last-failure* nil
  "The class name of the last detector failure, or NIL.")

(defvar *failures* 0
  "How many times a detector failed since the cell started.")

(defvar *scrubbed* 0
  "How many messages went out with at least one placeholder in them.")

(defparameter +solicitation-patterns+
  '("drop\\s+(your|the|it)\\s+(api\\s+)?key"
    "paste\\s+(your|the|it)\\s+(api\\s+)?key"
    "send\\s+me\\s+(your|the|it)\\s+(api\\s+)?key"
    "provide\\s+your\\s+(api\\s+)?key"
    "share\\s+your\\s+(secret|key|token|password)"
    "give\\s+me\\s+your\\s+(api\\s+)?key"
    "enter\\s+your\\s+(password|secret|token)"
    "throw\\s+(it|the\\s+key|your\\s+key)\\s+(here|over)"
    "drop\\s+it\\s+here"
    "paste\\s+it\\s+here"
    "send\\s+it\\s+(over|here)"
    "your\\s+(api\\s+)?key\\s+(here|in\\s+chat|in\\s+the\\s+chat)")
  "Phrases that ask the operator for a secret, carried over from the plugin.")

(defparameter +solicitation-scanners+
  (loop for pattern in +solicitation-patterns+
        collect (ignore-errors (cl-ppcre:create-scanner (format nil "(?i)~a" pattern))))
  "The same phrases, compiled once at load.")

(defparameter +solicitation-rewrite+
  (concatenate 'string
               "Security notice: a secret should not be pasted in chat. "
               "Store it in a file only its owner reads, in the environment variable a service "
               "manager sets, or in the vault cell's /vault, and rotate any key that was pasted.")
  "What a secret-asking phrase becomes.")

(defparameter +manual+
  "Secret values in chat are replaced on their way to the model, never in the log:
an API key, a token, a password, a private key header, a connection string or a
JSON web token that matches one of the 28 detectors of the original plugin
becomes [REDACTED-SENSITIVE-VALUE] in every message of a request but the system
prompt, and an assistant turn that asks you to paste a key is rewritten into
safe guidance. Nothing leaves this machine: the detectors are regular
expressions, read against one string.
  /vaultknox        what is watched, how many messages were rewritten, the last failure
  /vaultknox TEXT   the same text as it would leave, with what was found
  scan_secrets      the tool: the same preview for text the model has in hand,
                    with a comma-separated list of detectors to narrow it"
  "What (help :vaultknox) answers while the cell runs.")

(defparameter +part-types+ '("text" "input_text" "output_text")
  "The content-part types whose `text' member carries words.")

(defparameter +skipped-roles+ '("system")
  "The roles whose text is never touched.")

;;; --- the detectors, behind one setting ------------------------------------------

(defun detector-names ()
  "The detector keys, in registry order, as one comma-separated line."
  (format nil "~{~a~^, ~}" +detector-keys+))

;;; --- asking for a secret -------------------------------------------------------

(defun solicitation-spans (text)
  "Every phrase in TEXT that asks for a secret, as merged (START . END) spans."
  (let ((hits '()))
    (dolist (scanner +solicitation-scanners+)
      (when scanner
        (dolist (span (match-spans scanner text))
          (push (make-hit (car span) (cdr span) "solicitation" "a secret request" "low") hits))))
    (mapcar (lambda (hit) (cons (hit-start hit) (hit-end hit)))
            (merge-hits hits (length text)))))

(defun detector-solicitation-p (text)
  "Whether TEXT asks the operator for a secret."
  (and (stringp text) (plusp (length text)) (not (null (solicitation-spans text)))))

(defun rewrite-solicitations (text)
  "TEXT with every secret-asking phrase replaced by safe guidance, or TEXT."
  (if (or (null text) (zerop (length text)))
      text
      (let ((spans (solicitation-spans text)))
        (if (null spans)
            text
            (with-output-to-string (out)
              (let ((cursor 0))
                (dolist (span spans)
                  (write-string (subseq text cursor (car span)) out)
                  (write-string +solicitation-rewrite+ out)
                  (setf cursor (cdr span)))
                (write-string (subseq text cursor) out)))))))

;;; --- the advice on the fold ---------------------------------------------------

(defun carry-source (from to)
  "Keep FROM's log position on the copy TO, so the editable context still names
it: MESSAGE-SOURCE is the fold's own side table, one entry a folded message."
  (ignore-errors
    (let ((source (nodecode.evolved::message-source from)))
      (when source (setf (nodecode.evolved::message-source to) source))))
  to)

(defun scrub-text (text role)
  "TEXT with its secret values redacted, and -- for an assistant turn -- its
secret-asking phrases rewritten. (values TEXT' HIT-COUNT)."
  (multiple-value-bind (redacted hits) (redact-text text :detectors *detectors*)
    (let ((rewritten (if (and (stringp role) (string-equal role "assistant"))
                         (rewrite-solicitations redacted)
                         redacted)))
      (values rewritten (length hits)))))

(defun scrub-content (content role)
  "CONTENT -- a string, or an array of parts -- with its words scrubbed.
(values CONTENT' HITS)."
  (cond
    ((stringp content) (scrub-text content role))
    ((vectorp content)
     (let ((copy (copy-seq content)) (changed nil) (found 0))
       (dotimes (i (length copy))
         (let ((part (aref copy i)))
           (when (hash-table-p part)
             (let ((text (nlk:json-value part :string "text"))
                   (type (nlk:json-value part :string "type")))
               (when (and text (plusp (length text))
                          (member type +part-types+ :test #'string=))
                 (multiple-value-bind (scrubbed hits) (scrub-text text role)
                   (incf found hits)
                   (unless (string= scrubbed text)
                     (setf (aref copy i) (nlk:copy-json-object part "text" scrubbed)
                           changed t))))))))
       (values (if changed copy content) found)))
    (t (values content 0))))

(defun scrub-message (message)
  "MESSAGE, a folded provider message, with its words scrubbed: a copy when that
changed anything and MESSAGE itself when it did not. (values MESSAGE' HITS)."
  (if (not (hash-table-p message))
      (values message 0)
      (let ((role (nlk:json-value message :string "role"))
            (type (nlk:json-value message :string "type")))
        (if (or (and (stringp role) (member role +skipped-roles+ :test #'string-equal))
                (not (nth-value 1 (gethash "content" message))))
            (values message 0)
            (multiple-value-bind (content hits)
                (scrub-content (gethash "content" message) (or role type))
              (if (eq content (gethash "content" message))
                  (values message hits)
                  (values (carry-source message (nlk:copy-json-object message "content" content))
                          hits)))))))

(defun scrub-messages (messages)
  "MESSAGES with every message's words scrubbed, in order. (values LIST HITS)."
  (let ((found 0))
    (values (mapcar (lambda (message)
                      (multiple-value-bind (scrubbed hits) (scrub-message message)
                        (incf found hits)
                        scrubbed))
                    messages)
            found)))

(defun note-failure (condition)
  "Count CONDITION and say its class once: no input, no message, no traceback."
  (incf *failures*)
  (let ((class (string-downcase (symbol-name (type-of condition)))))
    (unless (equal class *last-failure*)
      (setf *last-failure* class)
      (ignore-errors
        (nle:notice (format nil "vaultknox: redaction failed (~a); the text goes out unchanged. /vaultknox says how often."
                            class)
                    :level :warning :key "nodecode-vaultknox")))))

(defun redact-wire (next &rest args)
  "The advice on the fold: scrub the messages it answers with, and hand its
CLEAN-P back untouched -- the covered-prefix cache is licensed by it."
  (multiple-value-bind (messages clean-p) (apply next args)
    (handler-case
        (multiple-value-bind (scrubbed found) (scrub-messages messages)
          (when (plusp found) (incf *scrubbed*))
          (values scrubbed clean-p))
      (error (condition)
        (note-failure condition)
        (values messages clean-p)))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract scan-secrets-request
  (:doc "Scan TEXT here, on this machine, for API keys, tokens, passwords, private keys and connection strings: the 28 detectors of the VaultKnox registry run over the string and every match is replaced by [REDACTED-SENSITIVE-VALUE]. Use it before text goes anywhere, and before you repeat a key a user pasted. It answers the redacted text and what was found; nothing is sent to a service. A detector that fails returns the text as it was, so read the answer.")
  ("text" :text :required t :doc "the text to scan and redact")
  ("detectors" :text :default ""
               :doc "a comma-separated list of detector keys to run, empty for all of them"))

(defun requested-detectors (text)
  "The detector keys TEXT lists, comma separated, or all of them."
  (if (or (null text) (zerop (length (nlk:trimmed text))))
      *detectors*
      (remove "" (mapcar (lambda (one) (string-downcase (nlk:trimmed one)))
                         (uiop:split-string text :separator ","))
              :test #'string=)))

(defun scan-tool (request)
  "The scan_secrets tool: the redacted text and one line on what was found."
  (let* ((text (scan-secrets-request-text request))
         (detectors (requested-detectors (scan-secrets-request-detectors request)))
         (unknown (remove-if (lambda (key) (find-detector key)) detectors)))
    (multiple-value-bind (redacted hits) (redact-text text :detectors detectors)
      (format nil "~a~%~%~a~@[~%~a~]"
              redacted
              (found-summary hits)
              (cond ((and unknown hits)
                     (format nil "unknown detector(s) ignored: ~{~a~^, ~}; rotate a value that was live." unknown))
                    (unknown
                     (format nil "unknown detector(s) ignored: ~{~a~^, ~}" unknown))
                    (hits "Rotate a value that was live."))))))

;;; --- /vaultknox ------------------------------------------------------------------

(defun status-text ()
  "What /vaultknox answers with nothing typed."
  (format nil "vaultknox: ~d detector(s) watching every request, ~d message(s) rewritten since the start; the system prompt is never read, nothing leaves for a third party, and the log keeps the originals~@[; last detector failure: ~a~]."
          (length *detectors*) *scrubbed* *last-failure*))

(defun preview-text (text)
  "TEXT as it would leave, and what was found."
  (multiple-value-bind (redacted hits) (redact-text text :detectors *detectors*)
    (format nil "~a~%~a" redacted (found-summary hits))))

(defun run-slash (args session-id)
  "The /vaultknox command: the state, or a preview of ARGS."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (values (if (plusp (length text)) (preview-text text) (status-text)) nil)))

;;; --- the section the cell runs on ----------------------------------------------

(defun read-settings (values table)
  "VALUES from the declaration, with the detector keys lowercased and anything
unknown refused before the cell starts."
  (declare (ignore table))
  (let ((detectors (mapcar (lambda (name) (string-downcase (nlk:trimmed name)))
                            (getf values :detectors))))
    (dolist (detector detectors)
      (unless (member detector +detector-keys+ :test #'string=)
        (nlk:config-error "vaultknox.detectors names ~s; it holds any of ~{~a~^, ~}"
                          detector +detector-keys+)))
    (setf (getf values :detectors) detectors)
    values))

(nlk:define-peripheral vaultknox :not-running t)

(nle:define-cell vaultknox
  (:section ("vaultknox")
    (:guide "nothing needs setting: the defaults run every detector; narrow detectors to a comma-free list of keys, and /vaultknox previews any text")
    ("detectors" :list :default (copy-list +detector-keys+)
                 :doc "the detectors to run, by key: openai, anthropic, aws-key-id, github-pat, jwt, private-key, password and the rest; the registry is named in (help :vaultknox)"))
  (:settings #'read-settings)
  (:hook 'nodecode.evolved::wire-messages-from-events #'redact-wire)
  (:tool +tool-name+ (nlk:contract 'scan-secrets-request) 'scan-tool)
  (:command "vaultknox" 'run-slash
            :description "What is redacted: the detectors, how many messages, or a preview of any text"
            :argument-hint "[TEXT]")
  (:help :vaultknox "vaultknox: API keys, tokens and passwords are redacted from chat in both directions" +manual+)
  (:start (lambda ()
            (setf *detectors* (or (setting :detectors) (copy-list +detector-keys+))
                  *last-failure* nil
                  *failures* 0
                  *scrubbed* 0)
            (nle:on-stop (lambda ()
                           (setf *detectors* (copy-list +detector-keys+)))))))
