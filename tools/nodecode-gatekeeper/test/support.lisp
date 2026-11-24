;;;; support.lisp --- the cell started over an empty ledger, and the two ways in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing reaches outside: the ledger is a file under the scratch home and
;;;; the clock is a constant, so a quiet-hours test does not depend on when it
;;;; runs. A tool call goes through the wire, exactly as a provider sends it;
;;;; a slash command goes through the registered entry, as the shell runs it.

(in-package #:nodecode.test)

(define-test-slice "gatekeeper" "GATEKEEPER-CELL-"
  :start nodecode-gatekeeper:start-cell
  :defaults (("templates" #("Routine notice: the build finished."))))

(defparameter +offset+ "+00:00")
(defparameter +routine+ "Routine notice: the build finished.")
(defparameter +recipient+ "+15550100")

(defparameter +noon+ (encode-universal-time 0 0 12 1 6 2026 0)
  "12:00 UTC: inside an 08:00-21:00 window at any offset within a few hours.")

(defparameter +night+ (encode-universal-time 0 0 3 1 6 2026 0)
  "03:00 UTC: outside an 08:00-21:00 window at offset +00:00.")

(defun ledger-path () (nlk:home "gatekeeper/ledger.sexp"))

(defun forget-ledger ()
  (ignore-errors (delete-file (ledger-path))))

(defun args (&rest pairs)
  "PAIRS as the JSON object a provider sends a tool."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun call-tool (name &optional (arguments "{}"))
  "NAME called as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun run-command (name &optional (argument ""))
  "NAME driven the way the shell drives a slash command; => its text."
  (let ((entry (nle::find-registered-command name)))
    (and entry (princ-to-string (funcall (nle::slash-command-headless entry)
                                         argument "gatekeeper-session")))))

(defmacro with-gatekeeper ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS -- pairs default to this suite's own
section -- over an empty ledger, the clock at noon, and stopped on unwind."
  `(let ((nodecode-gatekeeper::*now* +noon+))
     (forget-ledger)
     (unwind-protect
          (with-cell-stop ((gatekeeper-start ,@(if pairs pairs (list "owner_offset" +offset+))))
            (unwind-protect (progn ,@body)
              (forget-ledger)))
       (forget-ledger))))

(defun check-call (message &key (channel "sms") (recipient +recipient+) (thread ""))
  "gatekeeper_check as the model calls it; => the verdict text."
  (call-tool "gatekeeper_check"
              (args "channel" channel "recipient" recipient
                    "message" message "thread" thread)))

(defun mark-call (outcome &key (channel "sms") (recipient +recipient+) (thread ""))
  "gatekeeper_record as the model calls it; => the answer."
  (call-tool "gatekeeper_record"
              (args "channel" channel "recipient" recipient
                    "outcome" outcome "thread" thread)))

(defun consent-command (&optional (channel "sms") (recipient +recipient+))
  (run-command "gatekeeper" (format nil "consent ~a ~a signed the form on the site"
                                     channel recipient)))
