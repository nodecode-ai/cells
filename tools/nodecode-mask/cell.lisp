;;;; cell.lisp --- the hook on the wire fold, the tool, the command, the section.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; WHERE THE TEXT IS CAUGHT. The organism keeps its log and sends a
;;;; projection of it: WIRE-MESSAGES-FROM-EVENTS folds durable facts into the
;;;; provider-shaped messages of one turn, and the request is built from those.
;;;; The cell advises that fold and nothing else, so the user text of every
;;;; message goes out with its phones, emails, ID cards, bank cards, keys and
;;;; IP addresses replaced by placeholders while the log -- and every
;;;; transcript read off it -- keeps what the operator wrote.
;;;;
;;;; The original hooked the plugin's inbound step and, with its `tools' scope,
;;;; the tool results too; here the fold is the one seam, so tool results go
;;;; out whole and the IP detector is off unless the section names it.
;;;;
;;;; The fold answers two values (MESSAGES CLEAN-P); the second licenses the
;;;; covered-prefix cache, so the advice hands it back untouched. A copy is
;;;; rewritten and never the folded message itself, and the copy keeps the
;;;; message's log position, so the editable context still names it.
;;;;
;;;; FAIL-OPEN, as the original was: a detector that throws passes the text
;;;; through, counting the failure and saying it once in the operator's board.
;;;; A mask is a habit, not a confidentiality guarantee.

(in-package #:nodecode-mask)

(defparameter +tool-name+ "mask_test")

(defvar *enabled* t
  "Whether text is masked at all: the section's `enabled', or /mask on|off
for this run.")

(defvar *scrubbed* 0
  "How many messages went out with at least one placeholder in them.")

(defvar *failures* 0
  "How many times a detector failed since the cell started.")

(defvar *last-failure* nil
  "The class name of the last detector failure, or NIL.")

(defvar *restore-dir* nil
  "The folder /mask restore writes into, or NIL for the temporary folder.")

(defparameter +manual+
  "The text is masked on its way out, not on its way in: every user message of
every request has mainland phone numbers, email addresses, 18-character ID card
numbers, 16 to 19 digit bank card numbers and the visible shapes of an API key
(sk-..., gh[pousr]_..., xox[baprs]-..., AKIA..., a Bearer token) replaced by
numbered placeholders -- <PHONE_1>, <EMAIL_2>, <KEY_1> -- and the same words
always get the same number for as long as the cell runs. The log keeps the
originals: /sessions and every transcript still show what was written.
  /mask                 the state: on or off, how many replaced, by kind
  /mask on | off        mask, or stop masking, for this run
  /mask TEXT            that text as it would leave, with what was found
  /mask restore TEXT    the plaintext of a masked text, written to a file and
                        named -- never printed here, so it stays out of the log
  mask_test             the tool: the same preview for text the model holds"
  "What (help :mask) answers while the cell runs.")

(defparameter +part-types+ '("text" "input_text" "output_text")
  "The content-part types whose `text' member carries words a person wrote.")

;;; --- the advice on the fold ---------------------------------------------------

(defun carry-source (from to)
  "Keep FROM's log position on the copy TO, so the editable context still names
it: MESSAGE-SOURCE is the fold's own side table, one entry a folded message."
  (ignore-errors
    (let ((source (nodecode.evolved::message-source from)))
      (when source (setf (nodecode.evolved::message-source to) source))))
  to)

(defun mask-content (content)
  "CONTENT -- a string, or an array of parts -- with the user's words masked."
  (cond
    ((stringp content) (mask-text content))
    ((vectorp content)
     (let ((copy (copy-seq content)) (changed nil))
       (dotimes (i (length copy))
         (let ((part (aref copy i)))
           (when (hash-table-p part)
             (let ((text (nlk:json-value part :string "text"))
                   (type (nlk:json-value part :string "type")))
               (when (and text (plusp (length text))
                          (member type +part-types+ :test #'string=))
                 (let ((masked (mask-text text)))
                   (unless (string= masked text)
                     (setf (aref copy i) (nlk:copy-json-object part "text" masked)
                           changed t))))))))
       (if changed copy content)))
    (t content)))

(defun mask-message (message)
  "MESSAGE, a folded provider message: its user text masked, a copy when that
changed anything and MESSAGE itself when it did not."
  (if (not (hash-table-p message))
      message
      (let ((role (nlk:json-value message :string "role"))
            (type (nlk:json-value message :string "type")))
        (if (and (stringp role) (string= role "user")
                 (or (null type) (string= type "message"))
                 (nth-value 1 (gethash "content" message)))
            (let* ((content (gethash "content" message))
                   (masked (mask-content content)))
              (if (eq masked content)
                  message
                  (carry-source message
                                (nlk:copy-json-object message "content" masked))))
            message))))

(defun mask-messages (messages)
  "MESSAGES with every user-role text masked, in order."
  (mapcar #'mask-message messages))

(defun note-failure (condition)
  "Count CONDITION and say its class once: no input, no message, no traceback."
  (incf *failures*)
  (let ((class (string-downcase (symbol-name (type-of condition)))))
    (unless (equal class *last-failure*)
      (setf *last-failure* class)
      (ignore-errors
        (nle:notice (format nil "mask: masking failed (~a); the text goes out unchanged. /mask says how often." class)
                    :level :warning :key "nodecode-mask")))))

(defun mask-wire (next &rest args)
  "The advice on the fold: mask the messages it answers with, and hand its
CLEAN-P back untouched -- the covered-prefix cache is licensed by it."
  (multiple-value-bind (messages clean-p) (apply next args)
    (handler-case
        (if (not *enabled*)
            (values messages clean-p)
            (let ((masked (mask-messages messages)))
              (when (and masked (not (eq masked messages))) (incf *scrubbed*))
              (values masked clean-p)))
      (error (condition)
        (note-failure condition)
        (values messages clean-p)))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract mask-test-request
  (:doc "Mask personal data in TEXT here, on this machine, with no service and no model: mainland phone numbers, email addresses, 18-character ID card numbers, bank card numbers, API keys and dotted IPv4 addresses become numbered <TYPE_N> placeholders, the same words getting the same number for as long as the cell runs. It answers the masked text and what was found, never the values it replaced.")
  ("text" :text :required t :doc "the text to mask"))

(defun mask-test (request)
  "The mask_test tool: the masked text and one line on what it replaced."
  (let* ((text (mask-test-request-text request))
         (hits (resolve-overlaps (detect-hits text :entities *entities*)))
         (masked (mask-hits text hits)))
    (format nil "mask_test: replaced ~d PII value(s)~@[ (~{~a=~d~^, ~})~]~%masked:~%~a"
            (length hits)
            (loop for (label . count) in (hit-counts hits) collect label collect count)
            (if hits masked text))))

;;; --- the restore file ------------------------------------------------------------

(defun session-slug (session-id)
  "SESSION-ID as a file name: its letters and digits, at most sixteen of them."
  (let* ((raw (string-downcase (princ-to-string (or session-id ""))))
         (slug (string-trim "-" (substitute-if #\- (lambda (c) (not (alphanumericp c))) raw))))
    (if (plusp (length slug))
        (subseq slug 0 (min 16 (length slug)))
        "session")))

(defun restore-file (session-id)
  "The one file /mask restore writes for SESSION-ID: the next restore
overwrites it, so a session keeps at most one plaintext copy."
  (merge-pathnames (format nil "restore-~a.txt" (session-slug session-id))
                   (if (and (stringp *restore-dir*) (plusp (length *restore-dir*)))
                       (uiop:ensure-directory-pathname *restore-dir*)
                       (uiop:ensure-directory-pathname
                        (merge-pathnames "nodecode-mask-restore/" (uiop:temporary-directory))))))

(defun try-chmod (path)
  "Best effort: the plaintext is readable by its owner only, where the
implementation can say so."
  (ignore-errors (require :sb-posix))
  (ignore-errors (uiop:symbol-call "SB-POSIX" "CHMOD" (namestring path) #o600))
  path)

(defun write-restore (path text)
  "Write TEXT at PATH, 0600 where that can be asked for."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-string text out))
  (try-chmod path))

(defun restore-answer (text session-id)
  "The /mask restore answer: the placeholders put back, the file that holds
the plaintext, and never the plaintext itself."
  (let ((found (placeholders-in text)))
    (if (null found)
        "mask: no placeholder of this run stands in that text; nothing to restore."
        (let ((path (restore-file session-id)))
          (write-restore path (restore-text text))
          (format nil "mask: restored ~d placeholder(s): ~{~a~^, ~}.~%  file: ~a  (one file per session; the next restore overwrites it)~%  The plaintext is not in this text and not in the log: open the file to read it."
                  (length found) found (namestring path))))))

;;; --- /mask ----------------------------------------------------------------------

(defun status-text ()
  "What /mask answers with nothing typed."
  (format nil "mask: ~a; masking ~{~a~^, ~}~%  replaced: ~d total~%  distribution: ~a~%  remembered: ~d placeholder(s)~@[; last failure: ~a~]~@[; restore folder: ~a~]"
          (if *enabled* "enabled" "disabled (runtime off)")
          *entities*
          (stripper-total *stripper*)
          (distribution-words (stripper-distribution *stripper*))
          (hash-table-count (stripper-places *stripper*))
          *last-failure*
          *restore-dir*))

(defun preview-text (text)
  "TEXT as it would leave, and what was found."
  (let ((hits (resolve-overlaps (detect-hits text :entities *entities*))))
    (format nil "~a~%~a" (if hits (mask-hits text hits) text) (found-summary hits))))

(defun run-slash (args session-id)
  "The /mask command: the state, a switch, a restore to file, or a preview."
  (let* ((text (nlk:trimmed (or args "")))
         (break (position #\Space text))
         (verb (if break (subseq text 0 break) text))
         (rest (if break (nlk:trimmed (subseq text (1+ break))) "")))
    (values
     (cond
       ((zerop (length text))
        (let ((n (stripper-total *stripper*)))
          (if (zerop n)
              (format nil "~a~%  nothing to show yet: /mask TEXT previews any text, mask_test is the tool" (status-text))
              (status-text))))
       ((or (string= verb "status") (string= verb "st"))
        (status-text))
       ((string= verb "on")
        (setf *enabled* t)
        "mask: masking is on.")
       ((string= verb "off")
        (setf *enabled* nil)
        "mask: masking is off for this run; the text goes out as it was written.")
       ((string= verb "help")
        +manual+)
       ((string= verb "restore")
        (if (plusp (length rest))
            (restore-answer rest session-id)
            "mask: usage -- /mask restore TEXT  (the placeholders in TEXT are put back, and the plaintext is written to a file)"))
       ((and (string= verb "status") (plusp (length rest)))
        (status-text))
       (t (preview-text text)))
     nil)))

;;; --- the section the cell runs on ----------------------------------------------

(defun read-settings (values table)
  "VALUES from the declaration, with the detector names lowercased and
anything unknown refused before the cell starts."
  (declare (ignore table))
  (let ((entities (mapcar (lambda (name) (string-downcase (nlk:trimmed name)))
                           (getf values :entities))))
    (when (null entities)
      (nlk:config-error "mask.entities names no detector; name at least one of ~{~a~^, ~}"
                        +all-entities+))
    (dolist (entity entities)
      (unless (member entity +all-entities+ :test #'string=)
        (nlk:config-error "mask.entities names ~s; it holds any of ~{~a~^, ~}"
                          entity +all-entities+)))
    (setf (getf values :entities) entities)
    values))

(nlk:define-peripheral mask :not-running t)

(nle:define-cell mask
  (:section ("mask")
    (:guide "nothing needs setting: phones, emails, ID card numbers, bank card numbers and API keys are replaced by numbered placeholders in every user message before the request leaves, and the log keeps the originals; /mask previews any text, and /mask restore TEXT writes the plaintext of a masked text to a file")
    ("enabled" :boolean :default t
               :doc "mask at all; /mask off flips it for this run only")
    ("entities" :list :default '("phone" "email" "id-card" "bank-card" "key")
                :doc "the detections to run: any of phone, email, id-card, bank-card, key, ip")
    ("restore-dir" :string :default ""
                   :doc "the folder /mask restore writes into; empty means a folder in the temporary directory")
    ("max-entries" :integer :default 500 :min 1
                   :doc "how many placeholders one run remembers for /mask restore"))
  (:settings #'read-settings)
  (:hook 'nodecode.evolved::wire-messages-from-events #'mask-wire)
  (:tool +tool-name+ (nlk:contract 'mask-test-request) 'mask-test)
  (:command "mask" 'run-slash
            :description "What leaves: the state, a switch, a restore to file, or a preview of any text"
            :argument-hint "[status | on | off | restore TEXT | TEXT]")
  (:help :mask "mask: masks phones, emails, ID cards, bank cards, keys and IPs in the text you send" +manual+)
  (:start (lambda ()
            (setf *enabled* (if (setting :enabled) t nil)
                  *entities* (or (setting :entities) (copy-list +default-entities+))
                  *restore-dir* (setting :restore-dir)
                  *scrubbed* 0
                  *failures* 0
                  *last-failure* nil)
            (reset-stripper *stripper* (or (setting :max-entries) 500))
            (nle:on-stop (lambda ()
                           (setf *entities* (copy-list +default-entities+)
                                 *enabled* t
                                 *restore-dir* nil))))))
