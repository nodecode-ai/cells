;;;; support.lisp --- the defuddle test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; DEFUDDLE-CELL- name prefix; RUN-DEFUDDLE-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the defuddle program, so that is
;;;; what the tests stub: a special variable they rebind to a Lisp function
;;;; (WITH-DEFUDDLE-STUB: fast, and it records every run), or a real /bin/sh
;;;; script that records its argv and prints JSON (DEFUDDLE-RECORDING-PROGRAM),
;;;; for the one test that proves the program is really run. No test needs
;;;; defuddle installed, and none touches the network.

(in-package #:nodecode.test)

(define-test-slice "defuddle" "DEFUDDLE-CELL-" :start nodecode-defuddle:start-cell
  :defaults (("program" (defuddle-stub-program))))

(defvar *runs* '()
  "The (PROGRAM ARGS TIMEOUT) of every run this test stubbed, newest first.")

(defmacro with-defuddle-stub ((&key (stdout (defuddle-json)) (stderr "") (exit 0)
                                   (timed-out nil)) &body body)
  "BODY with the program stubbed: it answers STDOUT without running anything,
and every run is pushed on *RUNS* as (PROGRAM ARGS TIMEOUT)."
  `(let ((*runs* '())
         (nodecode-defuddle::*run*
           (lambda (program args timeout)
             (push (list program args timeout) *runs*)
             (values ,stdout ,stderr ,exit ,timed-out))))
     ,@body))

(defparameter +bridge+ "https://example.org/a-bridge"
  "The address the tests read.")

(defun last-run ()
  "The (PROGRAM ARGS TIMEOUT) of the newest run."
  (first *runs*))

(defun defuddle-refusal (url)
  "What reading URL refuses with, as the text the model would read."
  (handler-case (defuddle:extract url)
    (error (condition) (princ-to-string condition))))

(defun last-args ()
  "The argv of the newest run."
  (second (last-run)))

(defun defuddle-json (&key (title "A Bridge in the Fog")
                            (description "How one town rebuilt its crossing.")
                            (markdown "# A Bridge in the Fog

The river rose, and the old crossing went with it.")
                            (domain "example.org")
                            (site "Example")
                            (language "en")
                            (words 812)
                            (author "J. Doe")
                            (published "2024-03-01")
                            (omit '()))
  "The JSON defuddle prints for one page, as the cell reads it. OMIT names
members left out of the answer."
  (let ((table (nlk:make-json-object
                "title" title
                "description" description
                "contentMarkdown" markdown
                "domain" domain
                "site" site
                "language" language
                "wordCount" words
                "author" author
                "published" published)))
    (dolist (key omit) (remhash key table))
    (nlk:encode-json-object table)))

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun defuddle-script (path &rest lines)
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

(defun defuddle-stub-program ()
  "A program the cell accepts as defuddle: it exits at once, so a start finds
something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (defuddle-script (merge-pathnames "defuddle-stub/defuddle"
                                               (uiop:temporary-directory))
                              "exit 0"))))

(defun defuddle-recording-program (directory &optional (json (defuddle-json)))
  "A defuddle that records the argv it was run with in DIRECTORY/argv and
prints JSON."
  (defuddle-script
   (merge-pathnames "defuddle" directory)
   (format nil "printf '%s\\n' \"$@\" > ~s"
           (namestring (merge-pathnames "argv" directory)))
   (format nil "printf '%s' ~s" json)))
