;;;; support.lisp --- rex test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; REX-CELL- name prefix; RUN-REX-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell runs is the rex CLI, so that is what the
;;;; tests stub: NODECODE-REX::*RUN*, rebound to a Lisp function that answers
;;;; for an argv and records every call. No test needs Rex installed, and none
;;;; touches the network. The cell's settings name a program that is really
;;;; there (sh), so the section's own check passes without Rex.

(in-package #:nodecode.test)

(define-test-slice "rex" "REX-CELL-" :start nodecode-rex:start-cell
  :defaults (("binary" (or (nodecode-rex::program-file "sh") "/bin/sh"))
             ("session" "s-1")
             ("block" "b-1")))

(defvar *runs* '()
  "Every rex call this test stubbed, newest first, as
(PROGRAM ARGS TIMEOUT SESSION BLOCK).")

(defmacro with-rex-stub ((function) &body body)
  "BODY with the CLI stubbed: FUNCTION takes the argv and answers
(values EXIT STDOUT STDERR TIMED-OUT-P); every call is pushed on *RUNS*."
  `(let ((*runs* '())
         (nodecode-rex::*run*
           (lambda (program args timeout session block)
             (push (list program args timeout session block) *runs*)
             (funcall ,function args))))
     ,@body))

(defmacro rex-answers (&rest rules)
  "A stub from RULES, each (PREFIX EXIT STDOUT): the first whose PREFIX is
the call's argv's prefix answers with it; anything else fails loudly. PREFIX
is a literal list of strings; EXIT and STDOUT are forms, so a fixture's name
reads as its text."
  (let ((argv (gensym "ARGV")))
    `(lambda (,argv)
       (let ((,argv (if (equal (first ,argv) "--autostart=false") (rest ,argv) ,argv)))
         (loop for (prefix exit out err)
                 in (list ,@(loop for (prefix exit out err) in rules
                                  collect `(list ',prefix ,exit ,out ,err)))
               when (and (<= (length prefix) (length ,argv))
                         (equal prefix (subseq ,argv 0 (length prefix))))
                 return (values exit out (or err "") nil)
               finally (return (values 1 "" (format nil "no rex call answers ~s" ,argv)
                                       nil)))))))

(defun last-run ()
  "The newest stubbed call, (PROGRAM ARGS TIMEOUT SESSION BLOCK)."
  (first *runs*))

(defun last-argv ()
  "The argv of the newest stubbed call."
  (second (last-run)))

(defun last-pane ()
  "The REX_BLOCK the newest stubbed call was given."
  (fifth (last-run)))

(defun ran-p (argv)
  "Whether the stub was called with exactly ARGV -- one of the calls, not
only the newest, since a label reads the window back after renaming it."
  (loop for (nil args) in *runs* thereis (equal argv args)))

(defun rex-rename (label)
  "The argv of `rex window rename' for this session's window (w-1) with LABEL."
  (list "--autostart=false" "window" "rename" "-s" "s-1" "w-1" label))

(defun rex-capture-answer ()
  "The first line of the /rex capture 1 answer for this session's pane."
  (first (nlk:lines (cell-entry "nodecode-rex" "rex" "capture 1"))))

(defun rex-dialog-rows (dialog)
  "DIALOG's rows, as the answer carries them, list or JSON object."
  (let ((json (if (listp dialog) (nle::dialog-json dialog) dialog)))
    (nle::json-array json "rows")))

(defun rex-tool (name &rest pairs)
  "Call the registered tool NAME with PAIRS as its payload, as the model does."
  (let* ((entry (nle::find-tool name))
         (call (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs))))
    (funcall (third entry) call)))

(defun rex-refusal (&rest thunk)
  "What calling THUNK refuses with, as the text the model would read."
  (handler-case (apply (first thunk) (rest thunk))
    (error (condition) (princ-to-string condition))))

(defparameter +window+
  "{\"window_id\":\"w-1\",\"label\":\"fix-login\",\"session_id\":\"s-1\"}"
  "What `rex window inspect --json' answers for this session's pane.")

(defparameter +opened+
  "{\"block_id\":\"b-2\",\"label\":\"dev-server\"}"
  "What `rex run ... --json' answers when the pane opens.")

(defparameter +listing+
  "{\"sessions\":[{\"session_id\":\"s-1\",\"label\":\"rex\"},{\"session_id\":\"s-2\",\"label\":\"ops\"}]}"
  "What `rex ls --json' answers.")

(defparameter +windows+
  (format nil "{\"active_window_id\":\"w-1\",\"windows\":[{\"window_id\":\"w-1\",\"label\":\"fix-login\",\"focused_block_id\":\"b-1\"},{\"window_id\":\"w-2\",\"label\":\"ops\",\"focused_block_id\":\"b-9\"}]}")
  "What `rex window list -s s-1 --json' answers.")
