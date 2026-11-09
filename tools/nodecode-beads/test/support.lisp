;;;; support.lisp --- beads test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; beads tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a BEADS-CELL-
;;;; name prefix; RUN-BEADS-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the bd CLI, so that is what the
;;;; tests stub: a special variable the tests bind to a Lisp function
;;;; (WITH-BD-STUB: fast, and it records every argv), and a real /bin/sh
;;;; script the cell accepts as bd at start. No test needs bd installed, a
;;;; store, or a network -- the store the calls name is a scratch directory
;;;; holding an empty .beads/.

(in-package #:nodecode.test)

(define-test-slice "beads" "BEADS-CELL-" :start nodecode-beads:start-cell
  :defaults (("bd" (beads-stub-program))))

(defvar *calls* '()
  "The command list of every run this test stubbed, newest first.")

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun executable-script (path &rest lines)
  "Write LINES to PATH, one per line, make it executable, and answer its
native namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line lines) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path))
                     :output nil :error-output nil)
  (uiop:native-namestring path))

(defun beads-stub-program ()
  "A program the cell accepts as bd: it exits 0 saying nothing, so a start
finds something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (executable-script (merge-pathnames "beads-stub/bd" (uiop:temporary-directory))
                               "exit 0"))))

(defun bd-command (argv)
  "ARGV past the program and the globals the cell binds before the verb."
  (let ((rest (rest argv)))
    (loop while (and rest (equal (first rest) "--readonly")) do (pop rest))
    (loop while (and rest (equal (first rest) "--actor")) do (setf rest (cddr rest)))
    rest))

(defmacro with-bd-stub ((&rest answers) &body body)
  "BODY with bd stubbed. Each answer is (VERB EXIT STDOUT STDERR), VERB the
subcommand it answers; a verb with no answer runs as a general failure. Every
run pushes its command list on *CALLS*."
  `(let ((*calls* '())
         (nodecode-beads::*run*
           (lambda (argv directory seconds)
             (declare (ignore seconds))
             (let* ((command (bd-command argv))
                    (verb (first command))
                    (answer (assoc verb ',answers :test #'string=)))
               (push command *calls*)
               (if answer
                   (values (second answer) (third answer) (or (fourth answer) ""))
                   (values 1 ""
                           (format nil "stub: no answer for ~a in ~a" verb directory)))))))
     ,@body))

(defun last-command ()
  "The command list of the newest stubbed run."
  (first *calls*))

(defun argv-of (verb)
  "The newest stubbed run whose verb is VERB, as a command list."
  (find verb *calls* :key #'first :test #'string=))

(defun beads-store ()
  "A scratch store directory: one holding an empty .beads/."
  (let ((dir (merge-pathnames (format nil "beads-store-~a/" (random 1000000))
                               (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames ".beads/" dir))
    (uiop:native-namestring dir)))

(defmacro with-store ((var) &body body)
  "BODY with VAR a fresh scratch store, removed on the way out."
  `(let ((,var (beads-store)))
     (nlk:with-cleanup ((ignore-errors
                          (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,var)
                                                       :validate t)))
       ,@body)))

(defun bead-json (&rest pairs)
  "One bead as bd prints it, a one-row JSON array: PAIRS alternating key and
text (a number stays a number when it parses as one)."
  (format nil "[~a]" (nlk:encode-json-object (apply #'nlk:make-json-object pairs))))

(defmacro refusal (form)
  "FORM's refusal text, or NIL when it did not refuse."
  `(handler-case (progn ,form nil)
     (beads:beads-error (condition) (princ-to-string condition))))

(defun tail-of (list n)
  "The last N items of LIST."
  (nthcdr (max 0 (- (length list) n)) list))
