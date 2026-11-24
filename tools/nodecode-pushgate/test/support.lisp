;;;; support.lisp --- push gate test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; PUSHGATE- name prefix; RUN-PUSHGATE-TESTS runs exactly that slice, so
;;;; this system's test-op never re-runs the core suite.
;;;;
;;;; Nothing here touches a real repository or a real process: the cell's two
;;;; seams -- *GIT-RUNNER* and *CHECK-RUNNER* -- are rebound to answers the
;;;; test writes, so the run is offline and leaves no machine state behind.

(in-package #:nodecode.test)

(define-test-slice "pushgate" "PUSHGATE" :start nodecode-pushgate:start-cell
  :defaults (("check" "make test")))

(defvar *git-answers* '()
  "What the stubbed git answers: (ARGS TEXT CODE), ARGS as git got them.")

(defun stub-git (directory arguments &key input environment)
  "The answer *GIT-ANSWERS* holds for ARGUMENTS, or a failure."
  (declare (ignore directory input environment))
  (let ((answer (assoc (format nil "~{~a~^ ~}" arguments) *git-answers* :test #'equal)))
    (if answer
        (values (second answer) (third answer))
        (values "" 1))))

(defvar *check-answer* '(0 "")
  "The (EXIT-CODE OUTPUT) the stubbed check answers.")

(defun stub-check (command directory)
  (declare (ignore command directory))
  (values (second *check-answer*) (first *check-answer*)))

(defun pushgate-temp-ledger ()
  "A ledger path of this run's own, so one test's receipts never reach another."
  (ensure-directories-exist (nlk:home "pushgate/test/"))
  (nlk:home (format nil "pushgate/test/ledger-~d-~d.json" (get-universal-time) (random 1000000))))

(defmacro with-stubs ((&key answers (check '(0 ""))) &body body)
  "Run BODY with git and the check stubbed, and a ledger of its own."
  `(let ((*git-answers* ,answers)
         (*check-answer* (list ,@check))
         (nodecode-pushgate::*git-runner* #'stub-git)
         (nodecode-pushgate::*check-runner* #'stub-check)
         (nodecode-pushgate::*ledger-file* (pushgate-temp-ledger))
         (nodecode-pushgate::*ledger* nil))
     (nodecode-pushgate::load-ledger)
     ,@body))
