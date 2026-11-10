;;;; cell.lisp --- the two tools, /gatekeeper, the manual, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's whole surface was three hooks and a local-owner
;;;; command. This cell keeps the ledger and the rules, and puts them on two
;;;; surfaces a Nodecode session reaches: the tools gatekeeper_check and
;;;; gatekeeper_record, which the model calls around a message it is about to
;;;; send, and /gatekeeper, where the owner records consent, an opt-out, a
;;;; reply or the wording templates. The eval verbs in the GATEKEEPER package
;;;; are the same code, for a layer or a test.

(in-package #:nodecode-gatekeeper)

;;; --- the manual ----------------------------------------------------------------

(defparameter +primer+
      "gatekeeper holds the owner's rules for outbound messages and answers, per
message, PASS, APPROVE or BLOCK. It sends nothing itself: the host does.

  gatekeeper_check channel recipient message [thread]
      the verdict before you send. channel is sms, email, telegram or another
      lower-case name; recipient is the stable address, id or number; thread is
      the thread id when the channel has one. PASS is routine; APPROVE means ask
      the owner first; BLOCK means do not send.
  gatekeeper_record channel recipient outcome [thread]
      what happened: sent, failed, or reply when they answered you. A thread with
      an unanswered send holds the next one for approval until a reply is noted.
  /gatekeeper [log [N] | optout R | consent CHANNEL R [SOURCE...] |
               revoke CHANNEL R | reply CHANNEL R [THREAD] |
               variables CHANNEL R NAME=VALUE;...]
      the owner's own surface: who consented, who opted out, the decisions taken.

The rules, in the order they decide: an opt-out (STOP) blocks absolutely, and
nothing overrides it; a missing consent record for this exact channel and
recipient blocks; quiet hours outside the owner's window block; an unanswered
thread, a send inside the minimum gap, and wording that matches none of the
owner's templates ask for approval. A consent record carries the recipient's
UTC offset, since quiet hours are the recipient's local time.
Remember: this cell gates the messages you bring to gatekeeper_check. Nothing
stops you sending without asking it -- so ask it first, every time, and note the
outcome after.
Settings: gatekeeper.owner_offset, require_consent, start, end,
min_gap_seconds, templates, stop_keywords. (help :gatekeeper) answers this
while the cell runs."
  "What (help :gatekeeper) answers while the cell runs.")

;;; --- the verbs ------------------------------------------------------------------

(defun check (channel recipient message &key thread)
  "The verdict for one message about to go out, recorded. => a line."
  (let* ((channel (channel-id channel))
         (recipient (canonical-recipient recipient))
         (message (and message (princ-to-string message)))
         (thread (or thread ""))
         (at (now)))
    (when (or (null message) (zerop (length (nlk:trimmed message))))
      (fail "a message needs words: it is the exact text about to go out"))
    (with-ledger (data)
      (multiple-value-bind (outcome rules)
          (evaluate data channel recipient thread message at)
        (note-decision data channel recipient (first rules) outcome at)
        (when (equal outcome "pass")
          (reserve data channel recipient thread at message))
        (verdict-text outcome channel recipient rules)))))

(defun consent (channel recipient &key source date offset region)
  "Record the owner's consent for RECIPIENT on CHANNEL. => a line."
  (let* ((channel (channel-id channel))
         (recipient (canonical-recipient recipient))
         (source (and source (nlk:trimmed (princ-to-string source))))
         (offset-text (or (and offset (princ-to-string offset)) (setting :owner-offset)))
         (offset (parse-offset offset-text))
         (date (or date (date-string (now) offset)))
         (region (or region "")))
    (when (or (null source) (zerop (length source)))
      (fail "a consent record names its source: where and how they agreed"))
    (when (> (length source) 512) (fail "a consent source is at most 512 characters"))
    (unless (date-text-p date)
      (fail "a consent date is YYYY-MM-DD, and ~s is not" date))
    (with-ledger (data)
      (let ((existing (contact data channel recipient)))
        (if existing
            (progn (setf (getf existing :source) source
                         (getf existing :date) date
                         (getf existing :offset) offset-text
                         (getf existing :region) region)
                   (setf (getf existing :channel) channel))
            (push (list :id (recipient-id recipient) :channel channel :source source
                        :date date :offset offset-text :region region :variables nil)
                  (getf data :consents))))
      (format nil "consent recorded: ~a on ~a, from ~a, dated ~a, at ~a. A suppression still applies."
              (short-id (recipient-id recipient)) channel source date offset-text))))

(defun optout (recipient)
  "Suppress RECIPIENT absolutely. => a line."
  (let ((recipient (canonical-recipient recipient)))
    (with-ledger (data)
      (pushnew (suppression-id recipient) (getf data :suppressions) :test #'equal)
      (format nil "opt-out recorded for ~a: a new consent and an owner's approval cannot override it."
              (short-id (recipient-id recipient))))))

(defun revoke (channel recipient)
  "Take back RECIPIENT's consent on CHANNEL. => a line."
  (let ((channel (channel-id channel)) (recipient (canonical-recipient recipient)))
    (with-ledger (data)
      (let ((id (recipient-id recipient)))
        (setf (getf data :consents)
              (remove-if (lambda (row) (and (equal (getf row :id) id)
                                            (equal (getf row :channel) channel)))
                         (getf data :consents)))
        (format nil "consent revoked for ~a on ~a." (short-id id) channel)))))

(defun note-reply (channel recipient &key thread)
  "RECIPIENT answered on CHANNEL: the thread's pending sends are answered.
=> a line."
  (let ((channel (channel-id channel)) (recipient (canonical-recipient recipient))
        (thread (or thread "")) (at (now)))
    (with-ledger (data)
      (dolist (row (thread-messages data channel recipient thread))
        (when (equal "pending" (getf row :state))
          (setf (getf row :state) "sent")))
      (push (list :id (recipient-id recipient) :channel channel :thread (thread-id thread)
                  :at at :direction "reply" :state "received")
            (getf data :messages))
      (setf (getf data :messages) (keep (getf data :messages) *messages-kept*))
      (format nil "reply noted for ~a on ~a: the thread is answered."
              (short-id (recipient-id recipient)) channel))))

(defun note-outcome (channel recipient thread outcome)
  "The newest pending send in the thread is OUTCOME, sent or failed. => a line."
  (let ((channel (channel-id channel)) (recipient (canonical-recipient recipient))
        (thread (or thread ""))
        (outcome (string-downcase (princ-to-string outcome))))
    (unless (member outcome '("sent" "failed") :test #'equal)
      (fail "an outcome is sent, failed or reply, and ~s is not" outcome))
    (with-ledger (data)
      (let ((row (find-if (lambda (row) (equal "pending" (getf row :state)))
                           (thread-messages data channel recipient thread))))
        (if row
            (progn (setf (getf row :state) outcome)
                   (format nil "~a noted for ~a on ~a."
                           outcome (short-id (recipient-id recipient)) channel))
            (format nil "no send was pending for ~a on ~a: the outcome is recorded as said."
                    (short-id (recipient-id recipient)) channel))))))

(defun variables (channel recipient values)
  "Store the owner's template variables for RECIPIENT on CHANNEL. => a line."
  (let ((channel (channel-id channel)) (recipient (canonical-recipient recipient))
        (values (mapcar (lambda (pair)
                          (let ((name (princ-to-string (car pair)))
                                (value (princ-to-string (cdr pair))))
                            (unless (and (plusp (length name)) (<= (length name) 40)
                                         (or (alpha-char-p (char name 0)) (char= (char name 0) #\_))
                                         (every (lambda (c)
                                                  (or (alpha-char-p c) (digit-char-p c) (char= c #\_)))
                                                name))
                              (fail "a variable name is letters, digits and _, a letter first: ~s" name))
                            (when (> (length value) 4096) (fail "a variable value is at most 4096 characters"))
                            (cons name value)))
                        values)))
    (with-ledger (data)
      (let ((existing (contact data channel recipient)))
        (unless existing
          (fail "record the consent for ~a on ~a first" (short-id (recipient-id recipient)) channel))
        (setf (getf existing :variables) values)
        (format nil "~d owner variable~:p stored for ~a on ~a."
                (length values) (short-id (recipient-id recipient)) channel)))))

(defun decisions (&optional (count 10))
  "The last COUNT decisions. => lines."
  (let ((count (cond ((integerp count) count)
                     ((and count (ignore-errors (parse-integer (princ-to-string count)))))
                     (t 10))))
    (unless (<= 1 count 50) (fail "a log is 1 to 50 decisions, not ~d" count))
    (with-ledger (data)
      (let ((rows (keep (getf data :decisions) count)))
        (if (null rows)
            "gatekeeper: no decisions yet."
            (format nil "~{~a~^~%~}"
                    (mapcar (lambda (row)
                              (format nil "~a | ~a | ~a | ~a | ~a"
                                      (date-string (getf row :at) 0)
                                      (getf row :channel)
                                      (short-id (getf row :id))
                                      (getf row :rule)
                                      (getf row :outcome)))
                            rows)))))))

;;; --- the command ---------------------------------------------------------------

(defparameter +usage+
  (concatenate 'string
               "/gatekeeper [log [1-50] | optout <recipient> | consent <channel> <recipient> [source...] | "
               "revoke <channel> <recipient> | reply <channel> <recipient> [thread] | "
               "variables <channel> <recipient> name=value;...]"))

(defun status-text ()
  (with-ledger (data)
    (format nil "gatekeeper: ~d contact~:p, ~d suppressed, ~d decision~:p; ~d send~:p held. ~a"
            (length (getf data :consents))
            (length (getf data :suppressions))
            (length (getf data :decisions))
            (count-if (lambda (row) (equal "pending" (getf row :state))) (getf data :messages))
            +usage+)))

(defun pairs-from (text)
  "NAME=VALUE;NAME=VALUE as an alist, or a refusal."
  (loop for entry in (remove "" (uiop:split-string (or text "") :separator ";") :test #'equal)
        for at = (position #\= entry)
        do (unless at (fail "a variable is NAME=VALUE, and ~s is not" entry))
        collect (cons (subseq entry 0 at) (subseq entry (1+ at)))))

(defun run-slash (args session-id)
  "/gatekeeper typed alone: the ledger's state and the usage. => the answer's text."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (uiop:split-string text :separator '(#\Space #\Tab)))
         (head (and words (string-downcase (first words))))
         (rest (rest words)))
    (cond
      ((null words) (status-text))
      ((equal head "log") (decisions (or (ignore-errors (parse-integer (first rest))) 10)))
      ((equal head "optout") (optout (or (first rest) (fail "optout needs a recipient"))))
      ((equal head "consent")
       (unless (and (first rest) (second rest)) (fail "consent needs a channel and a recipient"))
       (consent (first rest) (second rest)
                :source (if (cddr rest) (format nil "~{~a~^ ~}" (cddr rest)) "owner command")))
      ((equal head "revoke")
       (unless (and (first rest) (second rest)) (fail "revoke needs a channel and a recipient"))
       (revoke (first rest) (second rest)))
      ((equal head "reply")
       (unless (and (first rest) (second rest)) (fail "reply needs a channel and a recipient"))
       (note-reply (first rest) (second rest) :thread (or (third rest) "")))
      ((equal head "variables")
       (unless (and (first rest) (second rest) (third rest))
         (fail "variables needs a channel, a recipient and NAME=VALUE pairs"))
       (variables (first rest) (second rest) (pairs-from (third rest))))
      (t (fail "~a" +usage+)))))

;;; --- the tools -----------------------------------------------------------------

(nlk:define-contract gatekeeper-check
  (:doc "The owner's rules for an outbound message, answered before it is sent.
Returns PASS (routine: send it, then call gatekeeper_record with sent or
failed), APPROVE (ask the owner first) or BLOCK (do not send) with the reason.
Deterministic; no model involved.")
  ("channel" :text :required t
           :doc "the channel it goes out on: sms, email, telegram, or another lower-case name")
  ("recipient" :text :required t
             :doc "the stable address, id or phone number, one token")
  ("message" :text :required t
           :doc "the exact text about to go out")
  ("thread" :string :default ""
            :doc "the thread id, when the channel has one"))

(nlk:define-contract gatekeeper-record
  (:doc "What happened to the message gatekeeper_check passed: sent, failed, or
reply when the recipient answered. A thread whose last send is unanswered holds
the next one for approval, so record every outcome.")
  ("channel" :text :required t :doc "the channel it went out on")
  ("recipient" :text :required t :doc "the address, id or number it went to")
  ("outcome" :text :required t :options '("sent" "failed" "reply")
             :doc "sent, failed, or reply when they answered")
  ("thread" :string :default "" :doc "the thread id, when the channel has one"))

(defun check-tool (request)
  (check (gatekeeper-check-channel request)
         (gatekeeper-check-recipient request)
         (gatekeeper-check-message request)
         :thread (or (gatekeeper-check-thread request) "")))

(defun record-tool (request)
  (let ((channel (gatekeeper-record-channel request))
        (recipient (gatekeeper-record-recipient request))
        (outcome (string-downcase (gatekeeper-record-outcome request)))
        (thread (or (gatekeeper-record-thread request) "")))
    (if (equal outcome "reply")
        (note-reply channel recipient :thread thread)
        (note-outcome channel recipient thread outcome))))

;;; --- the declaration -----------------------------------------------------------

(nle:define-cell gatekeeper
  (:section ("gatekeeper")
    (:guide "owner_offset is your own UTC offset (+HH:MM) and dates a consent record; start and end are the permitted sending window in the recipient's local time; templates are the routine wording that needs no approval -- with none, every message asks")
    ("owner_offset" :string :required t
                    :doc "your own UTC offset, +HH:MM, used to date a consent record and as its default offset")
    ("require_consent" :boolean :default t
                        :doc "require a consent record for the exact recipient and channel; a suppression always applies")
    ("start" :string :default "08:00" :doc "start of the permitted window, recipient-local HH:MM, inclusive")
    ("end" :string :default "21:00" :doc "end of the permitted window, recipient-local HH:MM, exclusive")
    ("min_gap_seconds" :integer :default 0 :min 0
                        :doc "the least time between two sends in one thread, even after a reply")
    ("templates" :list
                 :doc "the exact routine wording approved in advance; $name is filled from the owner's stored variables; empty means every message needs approval")
    ("stop_keywords" :list
                     :doc "the whole-message opt-out words the owner records, such as STOP; documentation for /gatekeeper optout"))
  (:tool "gatekeeper_check" (nlk:contract 'gatekeeper-check) 'check-tool)
  (:tool "gatekeeper_record" (nlk:contract 'gatekeeper-record) 'record-tool)
  (:command "gatekeeper" 'run-slash
            :catches 'gatekeeper-error
            :description "Consent, opt-outs, quiet hours and wording for the messages this agent sends"
            :argument-hint "[log|optout|consent|revoke|reply|variables]")
  (:help :gatekeeper
         "gatekeeper: gatekeeper_check and gatekeeper_record hold the owner's consent, opt-out, quiet-hours and wording rules for outbound messages"
         +primer+))
