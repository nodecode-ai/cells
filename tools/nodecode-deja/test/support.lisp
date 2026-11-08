;;;; support.lisp --- deja test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; deja tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a DEJA-CELL-
;;;; name prefix; RUN-DEJA-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the deja program, so that is
;;;; what the tests stub, two ways: a special variable the tests bind to a
;;;; Lisp function (WITH-RUN-STUB: fast, and it records every argv), and a
;;;; real /bin/sh script that records its arguments and standard input
;;;; (DEJA-RECORDING-PROGRAM), for the one test that proves the program is
;;;; really run. No test needs deja installed and none touches the network.

(in-package #:nodecode.test)

(define-test-slice "deja" "DEJA-CELL-" :start nodecode-deja:start-cell
  :defaults (("deja" (deja-stub-program))))

(defvar *calls* '()
  "The (ARGS . INPUT) of every run this test stubbed, newest first.")

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

(defun deja-stub-program ()
  "A program the cell accepts as deja: it answers one word, so a start finds
something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (executable-script (merge-pathnames "deja-stub/deja" (uiop:temporary-directory))
                               "cat >/dev/null"
                               "printf '%s' 'stub'"))))

(defun deja-recording-program (directory &optional (output "one line from deja"))
  "A deja that records what it was run with: DIRECTORY/argv holds one
argument per line, DIRECTORY/stdin its standard input, and it prints
OUTPUT."
  (executable-script
   (merge-pathnames "deja" directory)
   (format nil "printf '%s\\n' \"$@\" > ~s" (namestring (merge-pathnames "argv" directory)))
   (format nil "cat > ~s" (namestring (merge-pathnames "stdin" directory)))
   (format nil "printf '%s' ~s" output)))

(defmacro with-run-stub ((&key (context "") (prompt "") (answer "")) &body body)
  "BODY with deja stubbed: hook-context answers CONTEXT, hook-prompt PROMPT,
any other run ANSWER. Every run pushes (ARGS . INPUT) on *CALLS*."
  `(let ((*calls* '())
         (nodecode-deja::*run*
           (lambda (args input cwd)
             (declare (ignore cwd))
             (push (cons args input) *calls*)
             (cond ((equal (first args) "hook-context") ,context)
                   ((equal (first args) "hook-prompt") ,prompt)
                   (t ,answer)))))
     ,@body))

(defun last-call ()
  "The (ARGS . INPUT) of the newest stubbed run."
  (first *calls*))

(defun deja-context (reply)
  "What deja's hook reply REPLY gives the model."
  (nth-value 0 (nodecode-deja::split-reply reply)))

(defun deja-note (reply)
  "What deja's hook reply REPLY gives the person."
  (nth-value 1 (nodecode-deja::split-reply reply)))

(defun deja-standing (session)
  "The deja section SESSION's requests carry, or NIL."
  (cdr (assoc "deja" (nle::read-harness-sections session) :test #'equal)))

;;; --- a session, a turn, and what its requests were told ---------------------

(defun deja-turn (session &key (input "go") (source "cli"))
  "A turn admitted on SESSION the way a shell's turn is: its input is what
the round hook reads as the prompt."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command input
                    :invocation (nlk:default-invocation "turn.start" command
                                                         :source source :id command))))

(defun deja-round (session turn round)
  "Run the registered :ROUND chain as the engine does: OP through it."
  (funcall (cell-hook "nodecode-deja" :round)
           (list :session-id session :turn turn :round round)
           (lambda (op) (declare (ignore op)) nil)))

(defun deja-told (session)
  "Every reminder SESSION's turns were told, as texts, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt" :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when text collect text))
