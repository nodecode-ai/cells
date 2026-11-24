;;;; support.lisp --- the metamask test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; METAMASK-CELL- name prefix; RUN-METAMASK-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the MetaMask CLI, so that is what
;;;; the tests stub: a special variable they rebind to a Lisp function
;;;; (WITH-METAMASK-STUB: fast, and it records every run and the argv it was
;;;; given), or a real /bin/sh script that records its argv and prints JSON
;;;; (METAMASK-RECORDING-PROGRAM), for the one test that proves a program is
;;;; really run. No test needs mm installed, and none touches the network or a
;;;; wallet.

(in-package #:nodecode.test)

(define-test-slice "metamask" "METAMASK-CELL-" :start nodecode-metamask:start-cell
  :defaults (("program" (metamask-stub-program))))

(defvar *runs* '()
  "The (PROGRAM ARGS TIMEOUT) of every run this test stubbed, newest first.")

(defvar *answers* '()
  "((MATCH . PLIST) ...): the first whose MATCH is in a command line answers it.")

(defun answer (match &rest plist)
  "One stubbed answer: the run whose command line holds MATCH answers PLIST
(:stdout, :stderr, :exit, :timed-out)."
  (cons match plist))

(defmacro with-metamask-stub ((&key (stdout (metamask-envelope)) (stderr "") (exit 0)
                                    (timed-out nil) (answers '())) &body body)
  "BODY with the CLI stubbed: no program runs, every run is pushed on *RUNS*,
and the run's answer is the first of ANSWERS whose match is in its command
line, else STDOUT and the rest of the defaults."
  `(let ((*runs* '())
         (*answers* ,answers)
         (nodecode-metamask::*run*
           (lambda (program args timeout)
             (push (list program args timeout) *runs*)
             (let ((stub (or (loop for (match . plist) in *answers*
                                   when (search match (format nil "~{~a~^ ~}" args))
                                     return plist)
                             (list :stdout ,stdout :stderr ,stderr :exit ,exit
                                   :timed-out ,timed-out))))
               (values (getf stub :stdout) (getf stub :stderr)
                       (getf stub :exit 0) (getf stub :timed-out))))))
     ,@body))

(defun last-run ()
  "The (PROGRAM ARGS TIMEOUT) of the newest run."
  (first *runs*))

(defun last-args ()
  "The argv of the newest run."
  (second (last-run)))

(defun ran (fragment)
  "The argv of the newest run whose arguments join to hold FRAGMENT, or NIL."
  (loop for (nil args nil) in *runs*
        when (search fragment (format nil "~{~a~^ ~}" args))
          return args))

;;; --- the JSON the CLI prints -------------------------------------------------------

(defun metamask-table (&rest pairs)
  "One JSON object as the decoder would build it."
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(defun metamask-envelope (&rest pairs)
  "The JSON mm prints when it worked: ok true, with PAIRS as its data members."
  (nlk:encode-json-object
   (nlk:make-json-object "ok" t "data" (apply #'metamask-table pairs))))

(defun metamask-refusal-json (code message &optional hint)
  "The JSON mm prints when it refused."
  (nlk:encode-json-object
   (nlk:make-json-object
    "ok" nil
    "error" (nlk:make-json-object "code" code "message" message "hint" (or hint "")))))

;;; --- calling a tool the way the model does -------------------------------------------

(defun call-tool (name &rest pairs)
  "NAME's answer, called with PAIRS through its declared contract, as the
model's call arrives."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun refusal (thunk)
  "What THUNK refuses with, as the text the model would read."
  (handler-case (funcall thunk)
    (error (condition) (princ-to-string condition))))

;;; --- a program that is really run ----------------------------------------------------

(defparameter *stub-program* nil
  "The stand-in program's path, made once a run.")

(defun metamask-script (path &rest lines)
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

(defun metamask-stub-program ()
  "A program the cell accepts as mm: it exits at once, so a start finds
something on the path and a test that binds *RUN* never runs it."
  (or *stub-program*
      (setf *stub-program*
            (metamask-script (merge-pathnames "metamask-stub/mm"
                                               (uiop:temporary-directory))
                             "exit 0"))))

(defun metamask-recording-program (directory &optional (json (metamask-envelope)))
  "An mm that records the argv it was run with in DIRECTORY/argv and prints JSON."
  (metamask-script
   (merge-pathnames "mm" directory)
   (format nil "printf '%s\\n' \"$@\" > ~s"
           (namestring (merge-pathnames "argv" directory)))
   (format nil "printf '%s' ~s" json)))

;;; --- the sample the tests answer with ------------------------------------------------

(defparameter +address+ "0x1111111111111111111111111111111111111111"
  "A destination address the tests send to.")

(defparameter +usdc+ "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913"
  "Circle's USDC on Base, as a token contract address.")

(defparameter +tx-hash+ "0x2222222222222222222222222222222222222222222222222222222222222222"
  "A transaction hash the tests look up.")
