;;;; support.lisp --- a scripted aside CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test starts the cell for real; what is stubbed is the CLI. The
;;;; script answers one (values STDOUT STDERR STATUS) per call, so no test
;;;; needs Aside installed and nothing here runs a process. What the cell
;;;; builds is still there to assert on: ASIDE-ARGV is the argument vector
;;;; the CLI would have been given, one per call.

(in-package #:nodecode.test)

(define-test-slice "aside" "ASIDE-CELL-"
  :start nodecode-aside:start-cell
  :defaults (("bin" "/usr/local/bin/aside")))

(defvar *ran* '()
  "Every argument vector the scripted CLI was given, newest first.")

(defvar *script* nil
  "A function of one argument vector answering (values OUT ERR STATUS).")

(defun always-answering (out &optional (err "") (status 0))
  "A script answering the same thing to every call."
  (lambda (argv)
    (declare (ignore argv))
    (values out err status)))

(defun answering (&rest table)
  "A script answering per verb: (\"exec\" (OUT ERR STATUS) \"repl\" ...), the
verb being what follows the executable."
  (lambda (argv)
    (let ((answer (loop for (key value) on table by #'cddr
                        when (equal key (second argv)) return value)))
      (if answer (apply #'values answer) (values "" "" 0)))))

(defmacro with-aside-cli ((&key (answering '(always-answering "")) (binary "/usr/local/bin/aside")) &body body)
  "BODY with the executable and the process replaced: BINARY is what the cell
finds, ANSWERING answers each run."
  `(let ((*ran* '())
         (*script* ,answering)
         (nodecode-aside::*binary* (lambda () ,binary))
         (nodecode-aside::*run* (lambda (argv &key seconds)
                                   (declare (ignore seconds))
                                   (push argv *ran*)
                                   (funcall *script* argv)))
         (nodecode-aside::*ready* nil))
     ,@body))

(defun aside-argv ()
  "The argument vectors the cell built, oldest first."
  (reverse *ran*))

(defun aside-json (text)
  "TEXT decoded as the answer the tool contract carries."
  (nlk:decode-json text))

(defun aside-object (pairs)
  "A JSON object holding PAIRS, key then value."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun aside-refusal (name &rest pairs)
  "The preflight refusal of one call, or NIL when the call is accepted."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (nth-value 1 (nle::tool-call-preflight name (nlk:encode-json-object (aside-object pairs)) entry))))

(defun aside-tool (name &rest pairs)
  "Run the registered tool NAME as a model's call dispatches it: the arguments
as one JSON object, through the tool's own preflight."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (multiple-value-bind (arguments refusal)
        (nle::tool-call-preflight name (nlk:encode-json-object (aside-object pairs)) entry)
      (assert (null refusal) () "the call was refused before any run: ~a" refusal)
      (funcall (third entry) (nle::tool-body-arguments entry arguments)))))
