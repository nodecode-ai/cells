;;;; support.lisp --- afterforge test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; afterforge tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under an AFTERFORGE-CELL-
;;;; name prefix; RUN-AFTERFORGE-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing the cell does is run a check, so that is what the
;;;; helpers stub: *RUN* is bound to STUB-RUN, which records each call and
;;;; answers *ANSWER* and *CODE*, so a test drives a pass or a failure without
;;;; running anything. One test calls the real runner, for the exit code a stub
;;;; cannot prove. No test touches the network, and each one gets a shelf of
;;;; its own under the temporary directory.

(in-package #:nodecode.test)

(defun afterforge-project ()
  "A folder the checks may run in: this run's scratch home."
  (let ((dir (merge-pathnames "afterforge-project/" (nlk:home))))
    (ensure-directories-exist (merge-pathnames "keep" dir))
    (namestring dir)))

(define-test-slice "afterforge" "AFTERFORGE-CELL-" :start nodecode-afterforge:start-cell
  :defaults (("project" (afterforge-project))))

(defvar *runs* '()
  "The (COMMAND DIRECTORY) of every stubbed check, newest first.")

(defvar *answer* ""
  "What the stubbed check prints.")

(defvar *code* 0
  "What the stubbed check exits with.")

(defun stub-run (command directory timeout)
  "The check seam the tests bind: record the call, answer *ANSWER* and *CODE*."
  (declare (ignore timeout))
  (push (list command (namestring directory)) *runs*)
  (values *answer* *code*))

(defun fresh-shelf ()
  "A folder for one test's cases, made now and removed with the test."
  (let ((dir (merge-pathnames (format nil "afterforge-shelf-~a/" (gensym)) (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-lab ((&key (answer "") (code 0)) &body body)
  "BODY with the cell started on a scratch shelf of its own, its check
stubbed to answer ANSWER and exit CODE, and the shelf removed afterwards."
  `(let* ((*runs* '())
          (*answer* ,answer)
          (*code* ,code)
          (shelf (fresh-shelf))
          (nodecode-afterforge::*run* #'stub-run))
     (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree shelf :validate t)))
       (with-cell-stop ((afterforge-start "home" (namestring shelf)))
         ,@body))))

(defun digest (case-id command &optional (directory ".") (expect-exit 0))
  "What afterforge_recipe answers as the digest of one check."
  (nodecode-afterforge::digest-of case-id command directory expect-exit))

(defun shelf-file (case-id)
  "The file a case is kept in, for a test that reads it back."
  (nodecode-afterforge::case-file case-id))
