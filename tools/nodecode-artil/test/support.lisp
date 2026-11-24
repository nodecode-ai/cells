;;;; support.lisp --- the artil test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's
;;;; (the core DEFTEST, with its hermetic machine-state posture) under an
;;;; ARTIL-CELL- name prefix; RUN-ARTIL-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the artil program, so that is what
;;;; the tests stub: a special variable they rebind to a Lisp function
;;;; (WITH-ARTIL-STUB: fast, and it records every run), or a real /bin/sh
;;;; script that records its argv and prints a listing
;;;; (ARTIL-RECORDING-PROGRAM), for the one test that proves the program is
;;;; really run. No test needs artil installed, and none touches the network.

(in-package #:nodecode.test)

(define-test-slice "artil" "ARTIL-CELL-" :start nodecode-artil:start-cell
  :defaults (("program" (artil-stub-program))))

(defvar *runs* '()
  "The (PROGRAM ARGS TIMEOUT) of every run this test stubbed, newest first.")

(defmacro with-artil-stub ((&key (stdout "") (stderr "") (exit 0) (timed-out nil)) &body body)
  "BODY with the artil program stubbed: it answers STDOUT without running anything,
and every run is pushed on *RUNS* as (PROGRAM ARGS TIMEOUT)."
  `(let ((*runs* '())
         (nodecode-artil::*run*
           (lambda (program args timeout)
             (push (list program args timeout) *runs*)
             (values ,stdout ,stderr ,exit ,timed-out))))
     ,@body))

(defun last-run ()
  "The (PROGRAM ARGS TIMEOUT) of the newest run."
  (first *runs*))

(defun last-args ()
  "The argv of the newest run."
  (second (last-run)))

(defun artil-refusal (function &rest args)
  "What calling FUNCTION with ARGS refuses with, as the text the model would read."
  (handler-case (apply function args)
    (error (condition) (princ-to-string condition))))

(defparameter +listing+
  "mail:7  alice.example  Re: the bridge  (unread)
mail:6  bob.example   the survey
tg:9    dana   hello"
  "What the tests read as a listing of messages.")

(defparameter +message+
  "mail:7
From: alice.example
Subject: Re: the bridge

The bridge closed on Friday."
  "What the tests read as one message.")

(defparameter +team+
  "bookkeeper  id agent_42  the ledger and the invoices
reception   id agent_51  the front desk"
  "What the tests read as a workspace's agents.")

(defun artil-script (path &rest lines)
  "Write LINES to PATH after a /bin/sh line, make it executable, and answer its
native namestring."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line lines) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path))
                     :output nil :error-output nil)
  (uiop:native-namestring path))

(defvar *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun artil-stub-program ()
  "A program the cell accepts as artil: it exits at once, so a start finds
something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (artil-script (merge-pathnames "artil-stub/artil"
                                            (uiop:temporary-directory))
                          "exit 0"))))

(defun artil-recording-program (directory)
  "An artil that records the argv it was run with and the ARTIL_CLIENT it was
given in DIRECTORY, and prints a listing."
  (artil-script
   (merge-pathnames "artil" directory)
   (format nil "printf '%s\\n' \"$@\" > ~s"
           (namestring (merge-pathnames "argv" directory)))
   (format nil "printf '%s' \"$ARTIL_CLIENT\" > ~s"
           (namestring (merge-pathnames "client" directory)))
   (format nil "printf '%s' ~s" +listing+)))
