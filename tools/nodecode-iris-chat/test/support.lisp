;;;; support.lisp --- iris-chat test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; iris-chat tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under an IRIS-CHAT-
;;;; CELL- name prefix; RUN-IRIS-CHAT-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the iris program, so that is
;;;; what the tests stub two ways: a special variable they bind to a Lisp
;;;; function (WITH-RUN-STUB: fast, and it records every argv), and a real
;;;; /bin/sh program that records its arguments (IRIS-RECORDING-PROGRAM), for
;;;; the one test that proves the program is really run. No test needs iris
;;;; installed, an identity, or a network.

(in-package #:nodecode.test)

(define-test-slice "iris-chat" "IRIS-CHAT-CELL-" :start nodecode-iris-chat:start-cell
  :defaults (("iris" (iris-stub-program))))

(defvar *calls* '()
  "The ARGS of every run this test stubbed, newest first.")

(defvar *stubbed-ok* t
  "What the stub answers as the run's exit status.")

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

(defun iris-stub-program ()
  "A program the cell accepts as iris: it answers one ok envelope, so a start
finds something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (executable-script (merge-pathnames "iris-stub/iris" (uiop:temporary-directory))
                               "cat >/dev/null"
                               "printf '%s' '{\"status\":\"ok\",\"command\":\"stub\",\"data\":{}}'"))))

(defun iris-recording-program (directory &key (data "{\"user_id\":\"u1\",\"name\":\"stub agent\"}"))
  "An iris that records what it was run with: DIRECTORY/argv holds one
argument per line, and it prints an ok envelope carrying DATA."
  (executable-script
   (merge-pathnames "iris" directory)
   (format nil "printf '%s\\n' \"$@\" > ~s" (namestring (merge-pathnames "argv" directory)))
   (format nil "printf '%s' ~s" (format nil "{\"status\":\"ok\",\"command\":\"stub\",\"data\":~a}" data))))

(defun envelope (data)
  "DATA, one JSON object's text, as the ok envelope the program prints."
  (format nil "{\"status\":\"ok\",\"command\":\"stub\",\"data\":~a}" data))

(defmacro with-run-stub ((&key (data "{}") stdout (ok t)) &body body)
  "BODY with iris stubbed: an ok envelope around DATA, or STDOUT exactly as
given, answered with OK as the run's status. Every run pushes its ARGS on
*CALLS*."
  `(let ((*calls* '())
         (*stubbed-ok* ,ok)
         (nodecode-iris-chat::*run*
           (lambda (args input)
             (declare (ignore input))
             (push args *calls*)
             (values (or ,stdout (envelope ,data)) *stubbed-ok*))))
     ,@body))

(defun last-call ()
  "The ARGS of the newest stubbed run."
  (first *calls*))

;;; --- a session, a turn, and what its requests were told ---------------------

(defun iris-turn (session &key (input "any messages?") (source "cli"))
  "A turn admitted on SESSION the way a shell's turn is."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command input
                    :invocation (nlk:default-invocation "turn.start" command
                                                         :source source :id command))))

(defun iris-round (session turn round)
  "The registered :ROUND chain run as the engine runs it: OP through it."
  (funcall (cell-hook "nodecode-iris-chat" :round)
           (list :session-id session :turn turn :round round)
           (lambda (op) (declare (ignore op)) nil)))

(defun iris-told (session)
  "Every reminder SESSION's turns were told, as texts, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt" :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when text collect text))

(defun iris-standing (session)
  "The iris section SESSION's requests carry, or NIL."
  (cdr (assoc "iris" (nle::read-harness-sections session) :test #'equal)))
