;;;; support.lisp --- the duty test runner, fixtures and stubbed sources.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Duty tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a DUTY-CELL-
;;;; name prefix; RUN-DUTY-TESTS runs exactly that slice.
;;;;
;;;; The cell reads three sources and asks one predicate, and all four sit in
;;;; variables, so no test needs a store, a cron job or a goal to exist:
;;;; WITH-DUTY-SOURCES stands in for them and names, per source, what a read
;;;; that did not happen looks like. The two tests that go through a real
;;;; reader stub its one seam instead (NLK:LIST-SESSIONS, NLK:SESSION-STATE-
;;;; GET, NLK:EVENTS), so the parsing is proved against the shapes the
;;;; session directory, the cron cell and the goal cell actually write.

(in-package #:nodecode.test)

(define-test-slice "duty" "DUTY-CELL-" :start nodecode-duty:start-cell)

;;; --- fixtures ----------------------------------------------------------------

(defun duty-test-sessions ()
  "One conversation waiting on the person, one turn running long enough to be
called stalled, one turn that started and left no answer, and one session
with nothing outstanding."
  (flet ((ago (seconds) (- (get-universal-time) 2208988800 seconds)))
    (list (list :id "s1" :title "Plan the offsite" :source "cli"
                :updated-at (ago 300)
                :turn-kind "turn.completed" :waits "which venue?")
          (list :id "s2" :title "Refactor billing" :source "tui"
                :updated-at (ago 7200)
                :turn-kind "turn.started" :waits nil)
          (list :id "s3" :title "Nightly notes" :source "discord"
                :updated-at (ago 900)
                :turn-kind "turn.started" :waits nil)
          (list :id "s4" :title "Nothing pending" :source "cli"
                :updated-at (ago 60)
                :turn-kind "turn.completed" :waits nil))))

(defun duty-test-active ()
  "The sessions with a turn running in the fixture above."
  '("s2"))

(defun duty-test-jobs ()
  "A job whose last run failed, one paused with an answer still owed, and one
healthy job with nothing to say."
  (list (list :kind "cron" :id "j1" :name "nightly-backup" :state "active"
              :next-at nil :last-status "failed"
              :last-line "code 1 stderr: no target" :report nil)
        (list :kind "cron" :id "j2" :name "morning-digest" :state "paused"
              :next-at nil :last-status "ok" :last-line nil :report "the digest")
        (list :kind "cron" :id "j3" :name "weekly-review" :state "active"
              :next-at (+ (- (get-universal-time) 2208988800) 3600)
              :last-status "ok" :last-line nil :report nil)))

(defun duty-test-goals ()
  "One goal still standing."
  (list (list :kind "goal" :id "g-1" :session "s1"
              :condition "the suite passes" :check "make test" :turns 8)))

;;; --- stubbed sources ---------------------------------------------------------

(defmacro with-duty-sources ((&key sessions cron goals active notes) &body body)
  "Run BODY with the cell's four seams standing in for their sources.
SESSIONS, CRON and GOALS are the item lists a source answers, ACTIVE the
session ids with a turn running; NOTES is an alist of source name to the note
a failed read would carry, \"read\" where it is silent."
  `(with-saved-globals (nodecode-duty::*sessions-reader* nodecode-duty::*cron-reader*
                        nodecode-duty::*goals-reader* nodecode-duty::*active-turn-p*)
     (flet ((note (name) (or (cdr (assoc name ,notes :test #'equal)) "read")))
       (setf nodecode-duty::*sessions-reader* (lambda () (values ,sessions (note "sessions")))
             nodecode-duty::*cron-reader* (lambda () (values ,cron (note "cron")))
             nodecode-duty::*goals-reader* (lambda () (values ,goals (note "goals")))
             nodecode-duty::*active-turn-p*
             (lambda (id) (and (member id ,active :test #'equal) t))))
     ,@body))

(defun duty-view (&optional (section nil))
  "The board as the tool and /duty answer it, over the current stubs."
  (nodecode-duty:duty-text :only section))
