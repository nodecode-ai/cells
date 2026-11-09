;;;; cell.lisp --- the hook on the wire fold, the tool, the command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHERE THE TEXT IS CAUGHT. The organism keeps its log and sends a
;;;; projection of it: WIRE-MESSAGES-FROM-EVENTS folds durable facts into the
;;;; provider-shaped messages of one turn, and the request is built from those.
;;;; The cell advises that fold and nothing else, so the user text of every
;;;; message is scrubbed on the way out while the log -- and every transcript
;;;; read off it -- keeps what the operator wrote. Only user-role text is
;;;; touched: the system prompt, assistant turns, tool calls and tool results
;;;; pass byte for byte, as they did under the plugin this rewrites.
;;;;
;;;; The fold answers two values (MESSAGES CLEAN-P); the second licenses the
;;;; covered-prefix cache, so the advice hands it back untouched. A copy is
;;;; rewritten and never the folded message itself, and the copy keeps the
;;;; message's log position, so the editable context still names it.
;;;;
;;;; FAIL-OPEN, as the original was: a detector that throws passes the text
;;;; through, counting the failure and saying it once in the operator's board.
;;;; A redaction is a habit, not a confidentiality guarantee.

(in-package #:nodecode-pii)

(defparameter +tool-name+ "redact_text")

(defvar *last-failure* nil
  "The class name of the last detector failure, or NIL.")

(defvar *failures* 0
  "How many times a detector failed since the cell started.")

(defvar *scrubbed* 0
  "How many messages went out with at least one placeholder in them.")

(defparameter +manual+
  "The text is scrubbed on its way out, not on its way in: every user message of
every request has emails, phone numbers, card numbers (a checksum decides),
IBANs (a check remainder decides) and dotted IPv4 addresses replaced by
<EMAIL_ADDRESS>, <PHONE_NUMBER>, <CREDIT_CARD>, <IBAN_CODE> and <IP_ADDRESS>,
and each word listed in the cell's `terms' setting replaced by <PERSON>. No
model and no service is consulted: the detectors are local string rules, which
is why a name is masked only when the operator lists it. The log keeps the
originals -- /sessions and every transcript still show what was written -- and
a detector that fails passes the text through unchanged (/pii says so).
  /pii          what is masked, how many messages so far, the last failure
  /pii TEXT     the same text as it would leave, with what was found
  redact_text   the tool: the same preview for text the model has in hand"
  "What (help :pii) answers while the cell runs.")

(defparameter +part-types+ '("text" "input_text" "output_text")
  "The content-part types whose `text' member carries words a person wrote.")

(defparameter +nouns+ '(("EMAIL_ADDRESS" . "email address") ("PHONE_NUMBER" . "phone number")
                         ("CREDIT_CARD" . "card number") ("IBAN_CODE" . "IBAN")
                         ("IP_ADDRESS" . "IP address") ("PERSON" . "name"))
  "What a found span is called in an answer.")

(defun noun (type)
  (or (cdr (assoc type +nouns+ :test #'string=)) type))

(defun found-summary (hits)
  "One line on what HITS found, or that nothing was."
  (if (null hits)
      "nothing found: the text comes back as it was"
      (let ((counts '()))
        (dolist (hit hits)
          (let ((cell (assoc (hit-type hit) counts :test #'string=)))
            (if cell (incf (cdr cell)) (push (cons (hit-type hit) 1) counts))))
        (format nil "masked ~{~a~^, ~}"
                (loop for (type . count) in (sort counts #'string< :key #'car)
                      collect (format nil "~d ~a~a" count (noun type)
                                      (if (= count 1) "" "s")))))))

;;; --- the advice on the fold ---------------------------------------------------

(defun carry-source (from to)
  "Keep FROM's log position on the copy TO, so the editable context still names
it: MESSAGE-SOURCE is the fold's own side table, one entry a folded message."
  (ignore-errors
    (let ((source (nodecode.evolved::message-source from)))
      (when source (setf (nodecode.evolved::message-source to) source))))
  to)

(defun redact-content (content)
  "CONTENT -- a string, or an array of parts -- with the user's words redacted."
  (cond
    ((stringp content) (redact-text content))
    ((vectorp content)
     (let ((copy (copy-seq content)) (changed nil))
       (dotimes (i (length copy))
         (let ((part (aref copy i)))
           (when (hash-table-p part)
             (let ((text (nlk:json-value part :string "text"))
                   (type (nlk:json-value part :string "type")))
               (when (and text (plusp (length text))
                          (member type +part-types+ :test #'string=))
                 (let ((redacted (redact-text text)))
                   (unless (string= redacted text)
                     (setf (aref copy i) (nlk:copy-json-object part "text" redacted)
                           changed t))))))))
       (if changed copy content)))
    (t content)))

(defun redact-message (message)
  "MESSAGE, a folded provider message: its user text redacted, a copy when that
changed anything and MESSAGE itself when it did not."
  (if (not (hash-table-p message))
      message
      (let ((role (nlk:json-value message :string "role"))
            (type (nlk:json-value message :string "type")))
        (if (and (stringp role) (string= role "user")
                 (or (null type) (string= type "message"))
                 (nth-value 1 (gethash "content" message)))
            (let* ((content (gethash "content" message))
                   (redacted (redact-content content)))
              (if (eq redacted content)
                  message
                  (carry-source message
                                 (nlk:copy-json-object message "content" redacted))))
            message))))

(defun redact-messages (messages)
  "MESSAGES with every user-role text redacted, in order."
  (mapcar #'redact-message messages))

(defun note-failure (condition)
  "Count CONDITION and say its class once: no input, no message, no traceback."
  (incf *failures*)
  (let ((class (string-downcase (symbol-name (type-of condition)))))
    (unless (equal class *last-failure*)
      (setf *last-failure* class)
      (ignore-errors
        (nle:notice (format nil "pii: redaction failed (~a); the text goes out unchanged. /pii says how often." class)
                    :level :warning :key "nodecode-pii")))))

(defun redact-wire (next &rest args)
  "The advice on the fold: redact the messages it answers with, and hand its
CLEAN-P back untouched -- the covered-prefix cache is licensed by it."
  (multiple-value-bind (messages clean-p) (apply next args)
    (handler-case
        (let ((redacted (redact-messages messages)))
          (when (and redacted (not (eq redacted messages))) (incf *scrubbed*))
          (values redacted clean-p))
      (error (condition)
        (note-failure condition)
        (values messages clean-p)))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract redact-text-request
  (:doc "Redact personal data from TEXT here, on this machine, with no service and no model: email addresses, phone numbers, card numbers, IBANs and dotted IPv4 addresses become <TYPE> placeholders, and the words named in `terms' become <PERSON>. Use it before text is pasted anywhere; it answers the redacted text and what was found. A detector that fails returns the text as it was, so read the answer.")
  ("text" :text :required t :doc "the text to redact")
  ("terms" :text :default ""
           :doc "more words to mask as <PERSON>, comma separated: names, handles, a codename"))

(defun extra-terms (text)
  "The words TEXT lists, comma separated, or NIL."
  (when (and (stringp text) (plusp (length (nlk:trimmed text))))
    (remove "" (mapcar #'nlk:trimmed (split-on text #\,)) :test #'string=)))

(defun redact-tool (request)
  "The redact_text tool: the redacted text and one line on what was found."
  (let* ((text (redact-text-request-text request))
         (hits (detect-hits text :terms (append (extra-terms (redact-text-request-terms request))
                                                *terms*))))
    (format nil "~a~%~%~a" (mask-hits text hits) (found-summary hits))))

;;; --- /pii ----------------------------------------------------------------------

(defun status-text ()
  "What /pii answers with nothing typed."
  (format nil "pii: masking ~{~a~^, ~}~@[, plus ~d listed name(s)~] in the user text of every request; ~d message(s) rewritten since the start; nothing leaves for a third party, and the log keeps the originals~@[; last detector failure: ~a~]."
          (or *entities* '("nothing"))
          (and *terms* (length *terms*))
          *scrubbed*
          *last-failure*))

(defun preview-text (text)
  "TEXT as it would leave, and what was found."
  (let ((hits (detect-hits text)))
    (format nil "~a~%~a" (mask-hits text hits) (found-summary hits))))

(defun run-slash (args session-id)
  "The /pii command: the state, or a preview of ARGS."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (values (if (plusp (length text)) (preview-text text) (status-text)) nil)))

;;; --- the section the cell runs on ----------------------------------------------

(defun read-settings (values table)
  "VALUES from the declaration, with the entity names read as the detectors
spell them and anything unknown refused before the cell starts."
  (declare (ignore table))
  (let ((entities (mapcar (lambda (name) (string-downcase (nlk:trimmed name)))
                           (getf values :entities))))
    (dolist (entity entities)
      (unless (member entity +default-entities+ :test #'string=)
        (nlk:config-error "pii.entities names ~s; it holds any of ~{~a~^, ~}"
                          entity +default-entities+)))
    (setf (getf values :entities) entities)
    values))

(nlk:define-peripheral pii :not-running t)

(nle:define-cell pii
  (:section ("pii")
    (:guide "nothing needs setting: the defaults mask email addresses, phone numbers, card numbers, IBANs and IPv4 addresses in every user message before the request leaves; add names to terms, since no language model runs here; /pii previews any text, and the log keeps the originals")
    ("entities" :list :default '("email" "phone" "card" "iban" "ip")
                :doc "the detections to run: any of email, phone, card, iban, ip")
    ("terms" :list
             :doc "names and words masked as <PERSON> wherever they appear whole: no model runs here, so a name is masked because it is listed"))
  (:settings #'read-settings)
  (:hook 'nodecode.evolved::wire-messages-from-events #'redact-wire)
  (:tool +tool-name+ (nlk:contract 'redact-text-request) 'redact-tool)
  (:command "pii" 'run-slash
            :description "What leaves: the detectors, how many messages, or a preview of any text"
            :argument-hint "[TEXT]")
  (:help :pii "pii: masks emails, phones, cards, IBANs, IPs and listed names in the text you send" +manual+)
  (:start (lambda ()
            (setf *entities* (or (setting :entities) (copy-list +default-entities+))
                  *terms* (setting :terms)
                  *last-failure* nil
                  *failures* 0
                  *scrubbed* 0)
            (nle:on-stop (lambda ()
                           (setf *entities* (copy-list +default-entities+)
                                 *terms* nil))))))
