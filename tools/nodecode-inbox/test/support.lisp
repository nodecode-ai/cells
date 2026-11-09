;;;; support.lisp --- the inbox test runner, fixtures and stubbed sources.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Inbox tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under an INBOX-CELL-
;;;; name prefix; RUN-INBOX-TESTS runs exactly that slice.
;;;;
;;;; The cell reads four sources and every one of them sits in a variable, so
;;;; no test needs a store, a cron job or a background evaluation to exist:
;;;; WITH-STUBBED-SOURCES stands in for all four and names, per source, what
;;;; a read that did not happen looks like. The two tests that do go through
;;;; a real reader stub its one seam instead (NLK:SESSION-STATE-GET,
;;;; NLK:LIST-SESSIONS), so the parsing is proved against the shapes the cron
;;;; cell and the session directory actually write.

(in-package #:nodecode.test)

(define-test-slice "inbox" "INBOX-CELL-" :start nodecode-inbox:start-cell)

;;; --- fixtures ----------------------------------------------------------------

(defun inbox-test-notices ()
  "One standing notice, the shape the board answers."
  (list (list :kind "notice" :id "nodecode-cron" :level "error"
              :text "cron: job nightly-backup failed")))

(defun inbox-test-sessions ()
  "A session whose answer waits on the person, and one that does not."
  (list (list :kind "session" :id "s1" :title "Plan the offsite" :source "cli"
              :updated-at (- (get-universal-time) 300) :waits "which venue?")
        (list :kind "session" :id "s2" :title "Refactor billing" :source "tui"
              :updated-at (- (get-universal-time) 60) :waits nil)))

(defun inbox-test-jobs ()
  "A job whose last run failed, one paused with an answer still owed, and one
healthy job scheduled ahead."
  (list (list :kind "cron" :id "j1" :name "nightly-backup" :state "active"
              :next-at (+ (- (get-universal-time) 2208988800) 3600)
              :last-status "failed" :last-line "Script exited with code 1 stderr: no target"
              :report nil)
        (list :kind "cron" :id "j2" :name "morning-digest" :state "paused"
              :next-at nil :last-status "ok" :last-line nil :report "the digest")))

(defun inbox-test-evals ()
  "One background evaluation still running, one that exited."
  (list (list :kind "eval" :id 7 :status "running" :form "(sh \"make test\")")
        (list :kind "eval" :id 8 :status "exited" :form "(+ 1 2)")))

;;; --- stubbed sources ----------------------------------------------------------

(defmacro with-stubbed-sources ((&key notices sessions cron evals notes) &body body)
  "Run BODY with the cell's four readers standing in for their sources.
NOTICES, SESSIONS, CRON and EVALS are the item lists a source answers; NOTES
is an alist of source name to the note a failed read would carry, "read"
where it is silent."
  `(with-saved-globals (nodecode-inbox::*notices-reader* nodecode-inbox::*sessions-reader*
                        nodecode-inbox::*cron-reader* nodecode-inbox::*evals-reader*)
     (flet ((note (name) (or (cdr (assoc name ,notes :test #'equal)) "read")))
       (setf nodecode-inbox::*notices-reader* (lambda () (values ,notices (note "notices")))
             nodecode-inbox::*sessions-reader* (lambda () (values ,sessions (note "sessions")))
             nodecode-inbox::*cron-reader* (lambda () (values ,cron (note "cron")))
             nodecode-inbox::*evals-reader* (lambda () (values ,evals (note "evals")))))
     ,@body))

(defun inbox-view (&optional (section nil))
  "The inbox text as the tool and /inbox answer it, over the current stubs."
  (nodecode-inbox:inbox-text :only section))
