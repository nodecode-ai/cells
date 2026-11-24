;;;; support.lisp --- smol test runner, the stubbed engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Smol tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under a SMOL-CELL- name prefix;
;;;; RUN-SMOL-TESTS runs exactly that slice.
;;;;
;;;; What is stubbed is the engine, never the cell: *RUN* answers what smolvm
;;;; would have printed, as the plist a run answers, and *WHICH* answers
;;;; whether the CLI is installed. Every argv the cell builds, every field of
;;;; its answer and every line /smol prints are the cell's own work.

(in-package #:nodecode.test)

(define-test-slice "smol" "SMOL-CELL-" :start nodecode-smol:start-cell)

(defun smol-exit (output &key (code 0) timed-out-p)
  "What a run answers: OUTPUT, its exit CODE, and whether its ceiling cut it short."
  (list :output output :exit-code code :timed-out-p timed-out-p))

(defun smol-answer-empty (argv)
  "An engine that printed nothing and exited zero, whatever it was asked."
  (declare (ignore argv))
  (smol-exit ""))

(defun smol-by-subcommand (answers)
  "An engine that keys on the CLI's subcommand: ANSWERS is an alist of subcommand
 to the plist answer it gives, and anything unnamed exits zero with no output."
  (lambda (argv)
    (or (cdr (assoc (third argv) answers :test #'string=))
        (smol-exit ""))))

(defmacro with-smol ((&key (answer ''smol-answer-empty) (which t) calls) &body body)
  "BODY with the engine stubbed: ANSWER is a function of ARGV answering a run's
plist, WHICH answers whether the CLI is installed, and CALLS names the variable the
macro binds to every ARGV asked, newest first."
  (let ((calls (or calls (gensym "CALLS"))))
    `(let* ((,calls '())
            (nodecode-smol:*run* (lambda (argv timeout)
                                    (declare (ignore timeout))
                                    (push argv ,calls)
                                    (funcall ,answer argv)))
            (nodecode-smol:*which* (lambda (name)
                                      (declare (ignore name))
                                      ,which)))
       ,@body)))

(defun smol-tool (tool arguments &rest pairs)
  "TOOL called through the wire as a provider sends it, ARGUMENTS its JSON body,
the cell started on PAIRS. => the decoded answer."
  (nlk:decode-json
   (with-cell-stop ((apply #'smol-start pairs))
     (princ-to-string (nle::message-content (execute-wire-call "call-1" tool arguments))))))

(defun answer-notes (answer)
  "Every note of ANSWER, as strings."
  (loop for note across (nlk:json-value answer :array "notes")
        collect (nlk:json-value note :string)))

(defun answer-machines (answer)
  "The machine names one smol_machines answer carries."
  (coerce (nlk:json-value answer :array "machines") 'list))
