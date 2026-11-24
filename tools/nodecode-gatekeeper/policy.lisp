;;;; policy.lisp --- the ledger, and the rules the owner configured over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One file under the home holds everything: contacts (who consented, on
;;;; which channel, when, and at which UTC offset), suppressions (an opt-out,
;;;; absolute), the messages this cell reserved and the replies it observed,
;;;; and the decisions it took. Recipients are stored as a SHA-256 of their
;;;; canonical form, so the file holds no address book in clear.
;;;;
;;;; Four rules, three outcomes. A suppression or a missing consent BLOCKS.
;;;; Quiet hours, an unanswered thread (or a minimum gap), and wording that
;;;; matches none of the owner's templates ask for APPROVAL. Everything else
;;;; passes. The gate never sends anything: the host does.

(in-package #:nodecode-gatekeeper)

;;; --- the clock ------------------------------------------------------------------

(defvar *now* nil
  "The clock, in universal time; NIL reads the real one. A test binds it.")

(defun now () (or *now* (get-universal-time)))

;;; --- identities -----------------------------------------------------------------

(defun digest (text)
  "TEXT's SHA-256, hex only: what the ledger keeps instead of the text."
  (let ((hash (nlk:sha256-text text)))
    (if (and (> (length hash) 7) (char= (char hash 6) #\:)) (subseq hash 7) hash)))

(defun canonical-recipient (recipient)
  "RECIPIENT as the ledger's identity for it, or a refusal: one token, at
most 256 characters, a dialling prefix stripped of spaces, dots and dashes."
  (let ((text (and recipient (nlk:trimmed (princ-to-string recipient)))))
    (when (or (null text) (zerop (length text)))
      (fail "a recipient is required: the address, id or number the message goes to"))
    (when (> (length text) 256)
      (fail "a recipient is at most 256 characters"))
    (when (char= (char text 0) #\+)
      (setf text (remove-if (lambda (c) (find c " ()-")) text)))
    (when (find-if (lambda (c) (or (char= c #\Space) (char= c #\Tab) (< (char-code c) 32))) text)
      (fail "a recipient is one token with no spaces: ~s" text))
    text))

(defun recipient-id (recipient) (digest (canonical-recipient recipient)))

(defun suppression-id (recipient)
  "The identity an opt-out is filed under: case-folded, so a case variant of
an address cannot escape it."
  (digest (string-downcase (canonical-recipient recipient))))

(defun thread-id (thread) (digest (or thread "")))

(defun short-id (id) (subseq id 0 (min 12 (length id))))

(defun channel-id (channel)
  "CHANNEL as a channel name, or a refusal: lowercase, a letter first."
  (let ((text (and channel (string-downcase (nlk:trimmed (princ-to-string channel))))))
    (unless (and text (<= 1 (length text) 64)
                 (alpha-char-p (char text 0))
                 (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (find c "_-"))) text))
      (fail "a channel is a lowercase name such as sms, email or telegram, and ~s is none"
            channel))
    text))

;;; --- the ledger file ------------------------------------------------------------

(defparameter *decisions-kept* 200 "How many decisions the ledger keeps.")
(defparameter *messages-kept* 400 "How many reserved sends and replies it keeps.")

(defun ledger-path () (nlk:home "gatekeeper/ledger.sexp"))

(defun empty-ledger ()
  (list :consents '() :suppressions '() :messages '() :decisions '()))

(defun ledger ()
  "The ledger as a plist. A file that will not read is an empty ledger: the
next write replaces it."
  (let ((path (ledger-path)))
    (if (probe-file path)
        (handler-case
            (with-open-file (in path :direction :input :external-format :utf-8)
              (let ((*read-eval* nil) (*package* (find-package :keyword)))
                (let ((data (read in nil nil)))
                  (if (and (listp data) (member :consents data))
                      data
                      (empty-ledger)))))
          (error () (empty-ledger)))
        (empty-ledger))))

(defun save-ledger (data)
  (let ((text (with-output-to-string (out)
                (let ((*package* (find-package :keyword))
                      (*print-length* nil) (*print-level* nil) (*print-pretty* nil))
                  (prin1 data out)
                  (terpri out)))))
    (nlk:write-file-atomically (ledger-path) text :mode #o600 :directory-mode #o700)
    data))

(defmacro with-ledger ((var) &body body)
  "Run BODY over the ledger and write it back: (SETF (GETF VAR :X) ...) is
how a verb changes it."
  `(let ((,var (ledger)))
     (prog1 (progn ,@body)
       (save-ledger ,var))))

(defun keep (list limit) (subseq list 0 (min (length list) limit)))

;;; --- settings the declaration holds ---------------------------------------------

(defun setting-list (key)
  "KEY's value as a list, whatever the declaration answered: absent is ()"
  (let ((value (setting key)))
    (cond ((null value) '())
          ((listp value) value)
          ((vectorp value) (coerce value 'list))
          (t (list value)))))

;;; --- time -----------------------------------------------------------------------

(defun clock-minutes (text)
  "\"HH:MM\" as minutes after midnight, or a refusal."
  (let ((text (and text (princ-to-string text))))
    (unless (and text (= 5 (length text)) (char= (char text 2) #\:)
                 (every #'digit-char-p (concatenate 'string (subseq text 0 2) (subseq text 3))))
      (fail "a time is HH:MM, and ~s is not" text))
    (let ((hours (parse-integer text :end 2)) (minutes (parse-integer text :start 3)))
      (unless (and (<= 0 hours 23) (<= 0 minutes 59))
        (fail "a time is between 00:00 and 23:59, and ~s is not" text))
      (+ (* 60 hours) minutes))))

(defun parse-offset (text)
  "A UTC offset as minutes east of UTC: \"UTC\", \"+02:00\", \"-05:30\" or \"0\".
The original read an IANA zone; a rewrite without a zone database reads the
offset the owner writes, and does not follow a daylight-saving change."
  (let* ((text (and text (nlk:trimmed (string-upcase (princ-to-string text)))))
         (text (if (equal text "UTC") "+00:00" text))
         (text (if (equal text "Z") "+00:00" text)))
    (cond
      ((and text (member (char text 0) '(#\+ #\-))
            (= 6 (length text)) (char= (char text 3) #\:)
            (every #'digit-char-p (concatenate 'string (subseq text 1 3) (subseq text 4))))
       (let ((amount (+ (* 60 (parse-integer text :start 1 :end 3)) (parse-integer text :start 4))))
         (if (char= (char text 0) #\-) (- amount) amount)))
      ((and text (every #'digit-char-p text) (<= (length text) 5))
       (parse-integer text))
      (t (fail "a UTC offset is written +HH:MM or -HH:MM (UTC for +00:00), and ~s is not"
                text)))))

(defun minutes-of-day (universal offset)
  (floor (mod (+ universal (* 60 offset)) 86400) 60))

(defun date-string (universal &optional (offset 0))
  "YYYY-MM-DD for UNIVERSAL at OFFSET."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ universal (* 60 offset)) 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun date-text-p (text)
  (and (stringp text) (= 10 (length text)) (char= (char text 4) #\-) (char= (char text 7) #\-)
       (every #'digit-char-p (concatenate 'string (subseq text 0 4) (subseq text 5 7)
                                          (subseq text 8)))))

(defun window-open-p (minute start end)
  "START inclusive, END exclusive; a window that wraps past midnight is one
window."
  (if (<= start end)
      (and (>= minute start) (< minute end))
      (or (>= minute start) (< minute end))))

;;; --- the contact, and the four rules -------------------------------------------

(defun contact (data channel recipient)
  "The row recording RECIPIENT's consent on CHANNEL, or NIL."
  (find-if (lambda (row)
             (and (equal (getf row :id) (recipient-id recipient))
                  (equal (getf row :channel) (channel-id channel))))
           (getf data :consents)))

(defun contact-offset (row)
  "ROW's offset in minutes, or NIL when it names none that reads."
  (let ((text (getf row :offset)))
    (and text (plusp (length text)) (handler-case (parse-offset text) (gatekeeper-error () nil)))))

(defun suppressed-rule (data recipient)
  (when (member (suppression-id recipient) (getf data :suppressions) :test #'equal)
    "suppressed"))

(defun consent-rule (data channel recipient)
  (when (setting :require-consent)
    (let ((row (contact data channel recipient)))
      (unless (and row (getf row :source) (getf row :date))
        "no_consent"))))

(defun quiet-rule (data channel recipient universal)
  (let* ((row (contact data channel recipient))
         (offset (contact-offset row)))
    (if (null offset)
        "unknown_timezone"
        (unless (window-open-p (minutes-of-day universal offset)
                               (clock-minutes (setting :start))
                               (clock-minutes (setting :end)))
          "quiet_hours"))))

(defun thread-messages (data channel recipient thread)
  "This thread's messages, newest first."
  (remove-if-not (lambda (row)
                   (and (equal (getf row :id) (recipient-id recipient))
                        (equal (getf row :channel) (channel-id channel))
                        (equal (getf row :thread) (thread-id thread))))
                 (getf data :messages)))

(defun thread-rule (data channel recipient thread universal)
  "An unanswered thread always holds; so does a send inside the minimum gap."
  (let* ((rows (thread-messages data channel recipient thread))
         (ours (find "ours" rows :key (lambda (row) (getf row :direction)) :test #'equal))
         (gap (setting :min-gap-seconds)))
    (cond ((find "pending" rows :key (lambda (row) (getf row :state)) :test #'equal) "unanswered")
          ((and rows (equal "ours" (getf (first rows) :direction))) "unanswered")
          ((and ours (plusp (or gap 0)) (< (- universal (getf ours :at)) gap)) "minimum_gap"))))

(defun name-end (template start)
  (loop for index from start below (length template)
        while (let ((c (char template index))) (or (alphanumericp c) (char= c #\_)))
        finally (return index)))

(defun fill-template (template variables)
  "TEMPLATE with $name and ${name} filled from VARIABLES, an alist of
strings; NIL when the template names one that is not stored."
  (let ((out (make-string-output-stream)) (index 0) (size (length template)) (whole t))
    (loop while (< index size) do
      (let ((c (char template index)))
        (if (char/= c #\$)
            (progn (write-char c out) (incf index))
            (let* ((braced (and (< (1+ index) size) (char= (char template (1+ index)) #\{)))
                   (from (+ index (if braced 2 1)))
                   (end (name-end template from)))
              (if (= end from)
                  (progn (write-char c out) (incf index))
                  (let* ((name (subseq template from end))
                         (close (cond ((not braced) end)
                                      ((and (< end size) (char= (char template end) #\})) (1+ end))))
                         (pair (and close (assoc name variables :test #'equal))))
                    (if pair
                        (progn (princ (cdr pair) out) (setf index (or close end)))
                        (setf whole nil index size))))))))
    (when whole (get-output-stream-string out))))

(defun wording-rule (data channel recipient message)
  "The original's semantics: with no template stored, every message asks."
  (let* ((templates (setting-list :templates))
         (row (contact data channel recipient))
         (stored (getf row :variables)))
    (unless (find-if (lambda (template) (equal message (fill-template template stored))) templates)
      "unapproved_wording")))

(defun evaluate (data channel recipient thread message universal)
  "=> the outcome and the rules that named it, in the original's precedence:
a suppression, a missing consent and quiet hours block; an unanswered
thread, a minimum gap and unapproved wording ask."
  (let ((hard (or (suppressed-rule data recipient) (consent-rule data channel recipient))))
    (if hard
        (values "block" (list hard))
        (let ((soft (remove nil (list (quiet-rule data channel recipient universal)
                                      (thread-rule data channel recipient thread universal)
                                      (wording-rule data channel recipient message)))))
          (cond ((null soft) (values "pass" (list "routine")))
                ;; quiet hours block, as the original's quiet_hours rule did;
                ;; the rest of the soft rules ask.
                ((member "quiet_hours" soft :test #'equal) (values "block" soft))
                (t (values "approve" soft)))))))

;;; --- what the ledger keeps ------------------------------------------------------

(defun reserve (data channel recipient thread universal message)
  "Record this send as pending: a thread answers nothing until a reply comes."
  (push (list :id (recipient-id recipient) :channel channel :thread (thread-id thread)
              :at universal :direction "ours" :state "pending" :body (digest message))
        (getf data :messages))
  (setf (getf data :messages) (keep (getf data :messages) *messages-kept*)))

(defun note-decision (data channel recipient rule outcome universal)
  (push (list :at universal :channel channel :id (recipient-id recipient)
              :rule rule :outcome outcome)
        (getf data :decisions))
  (setf (getf data :decisions) (keep (getf data :decisions) *decisions-kept*)))

(defun rule-words (rules)
  (format nil "~{~a~^, ~}" (remove-duplicates rules :test #'equal)))

(defun verdict-text (outcome channel recipient rules)
  (let ((who (format nil "~a to ~a" channel (short-id (recipient-id recipient)))))
    (cond
      ((equal outcome "block")
       (format nil "BLOCK ~a: ~a -- do not send until the owner clears it.~%"
               who (rule-words rules)))
      ((equal outcome "approve")
       (format nil "APPROVE ~a: ~a -- ask the owner before sending.~%"
               who (rule-words rules)))
      (t
       (format nil "PASS ~a: routine -- send it, and tell gatekeeper_record how it went.~%" who)))))
