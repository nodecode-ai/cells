;;;; log.lisp --- what one refusal is, and what a digest says about them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure: a result's text in, the record's fields out; rows in, the alert's
;;;; text out. No file, no clock of its own, no settings -- so every rule the
;;;; original's core.py held is provable without a log on disk.

(in-package #:nodecode-tripwire)

(defparameter +reason-limit+ 160
  "How much of the refusal's own message one row keeps: the original kept 120
of it, and a refusal message here names the rule before it says why.")
(defparameter +tool-limit+ 60)
(defparameter +session-limit+ 80)
(defparameter +digest-lines+ 8
  "How many refused actions a digest spells out; the rest are counted.")
(defparameter +denied+ "chose deny"
  "What a refused approval says, from the cell that asked the operator.")
(defparameter +timeout+ "did not answer in time"
  "What an approval nobody answered says.")
(defparameter +refuser+ "refused by"
  "What a call another cell or plugin stopped says first.")

(defun flat (text)
  "TEXT as one line, or NIL when there is no text at all."
  (and text (nlk:one-line (if (stringp text) text (princ-to-string text)))))

(defun clipped (text limit)
  "TEXT on one line, at most LIMIT characters, or NIL."
  (let ((line (flat text)))
    (and line (subseq line 0 (min (length line) limit)))))

(defun refusal-how (text)
  "Whether TEXT, a tool call's result, says the call was stopped, and how: approval_denied,
approval_timeout or hook_refused; NIL when the call ran."
  (let* ((line (flat text))
         (folded (and line (string-downcase line))))
    (when folded
      (cond ((search +denied+ folded) "approval_denied")
            ((search +timeout+ folded) "approval_timeout")
            ((search +refuser+ folded) "hook_refused")
            (t nil)))))

(defun error-result-p (text)
  "Whether TEXT is a tool's own failure, which the core writes as ERROR: ..."
  (let ((line (flat text)))
    (and line (eql 0 (search "ERROR:" line)))))

(defun refusal-event (tool text session how now)
  "One row: when, which tool, how it was stopped and the reason's first line.
Never the arguments, the result or the message text -- the original's rule."
  (list :ts (or now (get-universal-time))
        :kind "refused"
        :tool (or (clipped tool +tool-limit+) "?")
        :how (or (clipped how 40) "stopped")
        :why (or (clipped text +reason-limit+) "")
        :session (or (clipped session +session-limit+) "?")))

(defun stamp-text (universal)
  "UNIVERSAL, a universal time, as YYYY-MM-DD HH:MM."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (or universal 0))
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun row-ts (row) (let ((ts (getf row :ts))) (if (integerp ts) ts 0)))
(defun row-field (row key fallback)
  (let ((value (getf row key)))
    (if (and value (stringp value) (plusp (length value))) value fallback)))

(defun event-line (row)
  "One refused action as the digest spells it: when, where, what, how, why."
  (format nil "- ~a ~a: ~a stopped (~a)~@[. ~a~]"
          (stamp-text (row-ts row))
          (row-field row :session "?")
          (row-field row :tool "?")
          (row-field row :how "stopped")
          (row-field row :why nil)))

(defun session-counts (rows)
  "ROWS by the session that was in, in the order the sessions first appear."
  (let ((counts (make-hash-table :test #'equal)) (order '()))
    (dolist (row rows)
      (let ((session (row-field row :session "?")))
        (unless (gethash session counts) (push session order))
        (incf (gethash session counts 0))))
    (format nil "~{~a~^, ~}"
            (loop for session in (nreverse order)
                  collect (format nil "~a ~d" session (gethash session counts))))))

(defun digest-body (fresh total &key peek)
  "The alert FRESH, the refusals since the last check, spells out, with TOTAL
the log's size; PEEK reads the newest ones without marking them read."
  (if (null fresh)
      (if peek
          "tripwire: the log holds no refusals yet"
          (format nil "tripwire: nothing refused since the last check (~d in the log)" total))
      (with-output-to-string (out)
        (format out "TRIPWIRE: ~d refused action(s) ~a~%"
                (length fresh) (if peek "in the log" "since the last check"))
        (dolist (row (last fresh (min +digest-lines+ (length fresh))))
          (format out "~a~%" (event-line row)))
        (when (> (length fresh) +digest-lines+)
          (format out "(+~d older; by session: ~a)~%"
                  (- (length fresh) +digest-lines+) (session-counts fresh)))
        (format out "Each of these was stopped. Say \"check the tripwire\" to look into it."))))
