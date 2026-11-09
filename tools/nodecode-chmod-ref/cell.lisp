;;;; cell.lisp --- the tool, the command, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Permission bits are easy to misread: 4755 against 2755, an S against
;;;; an s, a directory where x means "enter", a symbolic change whose X only
;;;; sometimes adds execute. The engine in MODES.LISP works the bits out and
;;;; says them in words; this file hands it to the model as one tool and to
;;;; the operator as /chmod, and puts the reference on the manual.
;;;;
;;;; Nothing is read from disk and no network is dialled: a value is a string
;;;; and the answer is a string.

(in-package #:nodecode-chmod-ref)

;;; --- the manual ---------------------------------------------------------------
(defparameter +manual+
  "chmod-ref explains a file mode, a chmod change or a umask: the bits in
both forms (octal and ls -l), each class's rights in words, what setuid,
setgid and sticky do, what a chmod change does step by step, and what a umask
gives new files and directories. Nothing is read from disk and nothing is
changed. One value is read the way the original's @chmod: reference read it:

  755, 0755, 0o755, 4755        a mode, three to six octal digits
  100644, 040755                st_mode, with the file type
  rwxr-xr-x, -rw-r--r--, -rw-r--r--@   ls -l's bits, with or without the type
  u+x, go-w, a=r, ug+s, g=u     a symbolic change, on 644 and 755
  644 u+x,go-w                  one change applied to one starting mode
  644 755, 100644 100755        what changes between two modes
  umask 022, umask 027          what new files and directories get

A change is applied the way GNU chmod applies it, with a umask of 0: u, g, o
and a, the +, - and = operators, r, w, x, X, s and t, copies like u=g, and
several joined with commas. Ask for the plain text of any of them:

  (nodecode-chmod-ref:explain \"644 u+x,go-w\")   the whole answer
  (nodecode-chmod-ref:summarise \"4755\")          one line: 4755 = rwsr-xr-x
  (nodecode-chmod-ref:preview \"\")                the examples, one line each

The model has the same thing as the tool chmod_explain, and the operator as
/chmod VALUE. A value that is not a mode answers why: 788 is not octal, a
letter is in the wrong place of ls -l text, a clause is not a chmod change."
  "What (help :chmod-ref) answers while the cell runs.")

;;; --- the tool's one boundary -----------------------------------------------------
(nlk:define-contract chmod-value
  (:doc "One value to explain: a file mode (755, 4755, 0o644, 100644,
rwxr-xr-x, -rw-r--r--), a chmod change (u+x,go-w, a=r, 644 u+x,go-w), two
modes to compare (644 755), or a umask (umask 022).")
  ("target" :text :required t
            :doc "the mode, change, mode pair or umask, as it is written on a command line"))

;;; --- the verbs -----------------------------------------------------------------
(define-verb explain (target)
  "TARGET explained: the mode in octal and ls -l, each class's rights in
words, the special bits, the change applied step by step, or what a umask
gives. => a string, one statement a line."
  (let ((*max-target-chars* (setting :max-target-chars)))
    (expand target)))

(define-verb summarise (target)
  "TARGET as one line: 755 = rwxr-xr-x, 644 u+x -> 744 (rwxr--r--),
umask 027: new files 640, new directories 750."
  (let ((*max-target-chars* (setting :max-target-chars)))
    (one-line-text target)))

(define-verb preview (query)
  "Suggestions for QUERY: a list of (INSERTED SHOWN META), the way the
original previewed a mode while it was typed."
  (preview-rows query))

(defun answer-tool (request)
  "The model's call: TARGET explained, or the refusal as a failed call."
  (handler-case (explain (chmod-value-target request))
    (mode-error (condition)
      (nle:failure (format nil "ERROR: ~a" (mode-error-message condition))))))

;;; --- the command ----------------------------------------------------------------
(defun examples-text ()
  "What /chmod with nothing typed says: the examples, one line each."
  (with-output-to-string (out)
    (write-line "chmod-ref: explain a file mode, a chmod change or a umask." out)
    (dolist (row (preview ""))
      (format out "  ~a~%" (second row)))
    (write-string "Type one of those after /chmod, or put a mode in what you ask." out)))

(defun run-chmod (args session-id)
  "One /chmod invocation => the answer's text."
  (declare (ignore session-id))
  (let ((value (nlk:trimmed (or args ""))))
    (if (zerop (length value))
        (examples-text)
        (handler-case (explain value)
          (mode-error (condition)
            (format nil "ERROR: ~a~%~%~a" (mode-error-message condition) (examples-text)))))))

;;; --- the cell -------------------------------------------------------------------
(nle:define-cell chmod-ref
  (:section ("chmod-ref")
    (:guide "nothing here is required; max_target_chars bounds how long a value is read")
    ("max_target_chars" :integer :default 200 :min 20
                        :doc "the longest value read, in characters"))
  (:tool "chmod_explain" (nlk:contract 'chmod-value) 'answer-tool)
  (:help :chmod-ref "chmod-ref: a file mode, a chmod change or a umask explained - (nodecode-chmod-ref:explain \"755\"), /chmod 755"
         +manual+)
  (:command "chmod" 'run-chmod
            :description "Explain a file mode, a chmod change or a umask"
            :argument-hint "MODE"))
