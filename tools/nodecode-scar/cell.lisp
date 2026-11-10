;;;; cell.lisp --- the manual, the receipt, the tools, /scar, and two hooks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermes asked each risky command on a card with a Deny row, and kept
;;;; nothing; scar kept the denied shape and stopped it the next time it came
;;;; back. Nodecode asks its operator through one seam, ASK-OPERATOR (the
;;;; guard asks with it, and so does a block pattern), and the gates forget
;;;; the answer the moment the call decides.
;;;;
;;;; So this cell advises that seam: a Deny on a command that carries an rm
;;;; shape stores the shape -- the token set, never the path -- in this
;;;; home's scar.json, and one :TOOL hook reads the commands a call runs and
;;;; answers the block when a stored scar covers them. The general gates read
;;;; patterns a person wrote beforehand; this one remembers a decision a
;;;; person took.
;;;;
;;;; Config, a sibling top-level key next to `channels':
;;;;   "cells": ["nodecode-scar"]
;;;;   "scar": {"max_scars": 200, "ask_seconds": 120, "record": true}

(in-package #:nodecode-scar)

;;; --- the manual --------------------------------------------------------------

(defparameter +manual+
  "Scar keeps the rm shape an operator denied and stops that shape the next time it comes back. A
person answering Deny on a question a gate asked (the guard, a block pattern) is that decision: the
shape of the denied command -- the token set rm, recursive, force, never the path -- is written to
this home's scar.json, and a later call whose own command carries every token of a scar does not run.
  (scar:check \"rm -rf build/\")   one command, without running it: would a scar stop it
  (scar:deny \"rm -rf build/\")    store that shape now, when the operator said so in words
  (scar:list-scars)              every scar: its receipt, its token set, the redacted preview
  (scar:forget 3)                forget one, and only after the operator says so; nobody there to
                                  answer means the scar stays
  /scar list | check <command> | deny <command> | forget <id> | help
WHAT MATCHES. rm -rf, rm -fr, rm -r -f, rm --recursive --force, a flag after the path, /bin/rm, a
wrapper in front (nice, nohup, timeout, xargs, env with its assignments, up to two other leading
words such as git rm), find -exec rm -rf, and the same rm inside a shell -c body, eight shells deep.
A scar that kept force is not matched by rm -r.
WHAT DOES NOT MATCH. Only a command is read: echo rm -rf is not one, nor is a list of file names,
nor python -c, node -e, a script file, command substitution or a -- before -c. A call whose command
is hidden behind a variable runs, because it could not be read.
THE LEDGER. One file, capped at max_scars; a full ledger refuses a new scar rather than dropping an
old decision. A ledger that cannot be read blocks an rm command and nothing else. A Deny is the only
record: Allow once, Allow always and a timeout keep nothing. Nothing leaves this machine. A scar is a
speed bump, not a boundary: eval has this machine's full authority."
  "What (help :scar) answers while the cell runs.")

;;; --- the receipt ---------------------------------------------------------------

(defparameter +weekdays+
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "DECODE-UNIVERSAL-TIME's weekday, 0 is Monday.")

(defun zone-text (zone)
  "ZONE, hours east of UTC, as +09:00."
  (let* ((minutes (round (* 60 zone)))
         (sign (if (minusp minutes) "-" "+"))
         (size (abs minutes)))
    (format nil "~a~2,'0d:~2,'0d" sign (floor size 60) (mod size 60))))

(defun receipt (row)
  "The scar's receipt: its number, the weekday, the clock and the numeric
offset it was recorded at. The agent does not supply the time."
  (multiple-value-bind (second minute hour day month year weekday daylight-p zone)
      (decode-universal-time (row-ts row))
    (declare (ignore daylight-p))
    (format nil "scar #~d, ~a ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a"
            (row-id row) (aref +weekdays+ weekday)
            year month day hour minute second (zone-text zone))))

(defun tokens-text (tokens)
  "The token set as a person reads it: rm, recursive, force."
  (format nil "~{~a~^, ~}" tokens))

(defun block-text (row)
  "The words of a call a stored scar stopped."
  (format nil "ERROR: blocked by nodecode-scar (~a): the operator pressed deny on an rm shape (~a), and that shape came back. This call did not run. Do not paraphrase the command and do not run it again; ask the operator what they want instead."
          (receipt row) (tokens-text (row-tokens row))))

(defparameter +unreadable-text+
  "ERROR: blocked by nodecode-scar: the scar ledger could not be read, so this rm command was not run. Other calls still run. A person can move the ledger file aside in the nodecode home to start a new one."
  "What an rm call answers while the ledger does not parse.")

;;; --- asking the operator ---------------------------------------------------------

(defparameter +forget-choice+ "Forget it" "The answer that forgets a scar.")
(defparameter +keep-choice+ "Keep it" "The answer that keeps it.")

(defparameter +denial-words+
  '("deny" "denied" "reject" "rejected" "refuse" "refused" "no" "block" "cancel" "never")
  "The words an answer says no with.")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none. Read by name,
so the cell also loads in a core released before the question board."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun denial-p (answer)
  "True when ANSWER says no: one of its words is a denial, so `Forget it'
and `Keep it' are neither."
  (and (stringp answer)
       (let ((words (mapcar (lambda (word) (string-right-trim ",.;:!?" word))
                             (tokenize (string-downcase answer)))))
         (some (lambda (word) (member word +denial-words+ :test #'equal)) words))))

(defun record-deny (detail)
  "Keep the rm shape of a command a person denied. A detail that carries no
rm token is not stored. => the row, or NIL."
  (let ((tokens (and (stringp detail) (command-tokens detail))))
    (when tokens
      (ignore-errors (add-scar tokens (preview-of detail))))))

;;; --- what the tools do -----------------------------------------------------------

(defun check-one (command)
  "What a call carrying COMMAND would meet: the scar that covers it, or none."
  (multiple-value-bind (status rows) (read-scars)
    (let ((tokens (command-tokens command)))
      (cond
        ((null tokens)
         (format nil "scar: ~s carries no rm danger token (a bare rm, or rm without r and f, is not a shape), so no scar covers it: it would run." command))
        ((eq status :unreadable) +unreadable-text+)
        ((eq status :absent)
         (format nil "scar: the ledger file is absent, so nothing is stored; (~a) would run."
                 (tokens-text tokens)))
        (t (let ((row (matching-scar rows tokens)))
             (if row
                 (format nil "~a~%~%That shape is stored, so this call would be blocked." (block-text row))
                 (format nil "scar: the ledger was read and holds ~d scar~:p; none covers (~a): it would run."
                         (length rows) (tokens-text tokens)))))))))

(defun list-one (limit)
  "Every scar, oldest first, or the newest LIMIT of them."
  (multiple-value-bind (status rows) (read-scars)
    (cond
      ((eq status :absent) "The ledger file is absent.")
      ((eq status :unreadable) "The ledger file is present but could not be read. Leave it in place.")
      ((null rows) "The ledger was read and it holds no scars.")
      (t (let ((shown (if (and limit (plusp limit)) (last rows (min limit (length rows))) rows)))
           (with-output-to-string (out)
             (format out "the ledger was read and it holds ~d scar~:p~%" (length rows))
             (dolist (row shown)
               (format out "~a: (~a) ~a~%" (receipt row)
                       (tokens-text (row-tokens row)) (row-preview row)))))))))

(defun deny-one (command note)
  "Store COMMAND's shape now, on the operator's word."
  (let ((tokens (command-tokens command)))
    (unless tokens
      (fail "~s carries no rm danger token (a bare rm, or rm without r and f, is not a shape), so nothing was stored."
            command))
    (let ((row (add-scar tokens (preview-of command))))
      (format nil "scar #~d holds (~a) from ~s~@[ -- ~a~]. A later call that carries every one of those tokens is stopped before it runs."
              (row-id row) (tokens-text (row-tokens row)) (row-preview row) note))))

(defun find-row (rows id)
  (when (integerp id) (find id rows :key #'row-id :test #'=)))

(defun forget-one (id ask confirm)
  "Forget one scar. With CONFIRM, the operator is asked first, and anything
but a yes leaves the scar in place."
  (multiple-value-bind (status rows) (read-scars)
    (when (eq status :unreadable)
      (fail "the ledger file is present but could not be read, so nothing was forgotten."))
    (let ((row (find-row rows id)))
      (unless row
        (fail "no scar has id ~a; scar_list names the ids that exist." id))
      (when confirm
        (unless ask
          (fail "no surface here answers a question, so scar #~d was not forgotten." id))
        (multiple-value-bind (answer why)
            (funcall ask
                     (format nil "Forget ~a? After this, that shape is no longer stopped."
                             (receipt row))
                     :detail (format nil "(~a) ~a" (tokens-text (row-tokens row))
                                     (row-preview row))
                     :choices (list +forget-choice+ +keep-choice+)
                     :seconds (setting :ask-seconds))
          (unless (equal answer +forget-choice+)
            (return-from forget-one
              (format nil "scar #~d was not forgotten (~a)." (row-id row)
                      (or answer (string-downcase (string why))))))))
      (multiple-value-bind (removed left) (remove-scar (row-id row))
        (declare (ignore removed))
        (format nil "scar #~d forgotten: that shape is no longer stopped. The ledger holds ~d scar~:p."
                (row-id row) left)))))

;;; --- the verbs ---------------------------------------------------------------------

(define-verb check (command)
  "Test one shell command against the ledger without running it: would a stored scar stop it, and which."
  (handler-case (check-one command)
    (scar-error (condition) (format nil "ERROR: ~a" condition))))

(define-verb list-scars (&key limit)
  "Every stored scar, oldest first: its receipt, the token set, and the redacted preview of the command a person denied."
  (handler-case (list-one limit)
    (scar-error (condition) (format nil "ERROR: ~a" condition))))

(define-verb deny (command &key note)
  "Store COMMAND's rm shape as a scar, so a later call carrying every one of its tokens is stopped before it runs."
  (handler-case (deny-one command note)
    (scar-error (condition) (format nil "ERROR: ~a" condition))))

(define-verb forget (id &key (ask (ask-operator-function)) (confirm t))
  "Forget one scar, and only after the operator says so: nobody there to answer means the scar stays."
  (handler-case (forget-one id ask confirm)
    (scar-error (condition) (format nil "ERROR: ~a" condition))))

;;; --- the four tool contracts ---------------------------------------------------------

(nlk:define-contract check-arguments
  (:doc "Ask whether one shell command would be stopped by a scar an operator's Deny left. Nothing runs; the answer names the scar that covers it, or says that none does.")
  ("command" :text :required t
   :doc "the shell command to test, as it would be typed: rm -rf build/"))

(nlk:define-contract deny-arguments
  (:doc "Store the rm shape of a command as a scar, when the operator has said in words that it must not run again. A later call whose command carries every token of that shape does not run.")
  ("command" :text :required t
   :doc "the command whose shape is kept: rm -rf build/")
  ("note" :string
   :doc "one line on why, for the answer only; the ledger keeps the command's first line and the token set"))

(nlk:define-contract list-arguments
  (:doc "Read the scar ledger: every scar an operator's Deny left, oldest first, with its receipt and the redacted preview of the denied command. The file is not changed.")
  ("limit" :integer
   :doc "how many of the newest scars to show; omit for all of them"))

(nlk:define-contract forget-arguments
  (:doc "Forget one scar, and only after the operator says so: the question goes to them through the organism's own ask, and anything but a yes leaves the scar in place.")
  ("id" :integer :required t
   :doc "the scar's number, as scar_list prints it"))

(defun check-tool (args)
  (check (check-arguments-command args)))

(defun deny-tool (args)
  (deny (deny-arguments-command args) :note (deny-arguments-note args)))

(defun list-tool (args)
  (list-scars :limit (list-arguments-limit args)))

(defun forget-tool (args)
  (forget (forget-arguments-id args)))

;;; --- the commands a call runs ---------------------------------------------------------

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose arguments are a command the call runs.")

(defparameter +own-tools+ '("scar_check" "scar_deny" "scar_list" "scar_forget")
  "This cell's own tools: a check of a denied shape must not be stopped by that shape.")

(defun operator-name (x)
  "X's symbol name, lowercased, or NIL. Compared by name, so a form read in
any package is recognized."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun literal-command (x strings)
  "The text X alone names as a command, or NIL when X hides it."
  (cond
    ((stringp x) x)
    ((and (consp x) (equal "$" (operator-name (car x))) (stringp (second x))
          (hash-table-p strings))
     (let ((value (gethash (second x) strings)))
       (and (stringp value) value)))
    ((and (consp x) (member (operator-name (car x)) '("list" "quote") :test #'equal))
     (let ((items (if (equal "quote" (operator-name (car x))) (second x) (rest x))))
       (when (and (consp items)
                  (every (lambda (item) (literal-command item strings)) items))
         (format nil "~{~a~^ ~}"
                 (mapcar (lambda (item) (literal-command item strings)) items)))))
    (t nil)))

(defun spawn-command (call strings)
  "CALL's command text, or NIL when one of its arguments hides it."
  (let ((rest (rest call)) (parts '()) (hidden nil))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest))
                 (let ((text (literal-command (first rest) strings)))
                   (if text (push text parts) (setf hidden t))
                   (setf rest (rest rest)))))
    (when (and (not hidden) parts)
      (format nil "~{~a~^ ~}" (nreverse parts)))))

(defun form-commands (form strings)
  "Every command FORM runs, or NIL: a hidden one, or none at all, means the
call's command was not read."
  (let ((commands '()) (hidden nil) (saw nil))
    (labels ((walk (x)
               (cond
                 ((consp x)
                  (when (member (operator-name (car x)) +spawn-operators+ :test #'equal)
                    (setf saw t)
                    (let ((command (spawn-command x strings)))
                      (if command (push command commands) (setf hidden t))))
                  (walk (car x))
                  (walk (cdr x)))
                 ((member (operator-name x) +spawn-operators+ :test #'equal)
                  (setf saw t)))))
      (walk form))
    (and saw (not hidden) (nreverse commands))))

(defun read-forms (text)
  "TEXT's forms, read with nothing evaluated; NIL when it will not read."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil) (*read-circle* nil)
              (*package* (find-package :nodecode-scar)))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                collect form)))
    (error () nil)))

(defun call-tokens (arguments)
  "The rm danger tokens the call's own command carries. Empty when the call
shows no command, or hides one: such a call runs."
  (let* ((strings (and (hash-table-p arguments) (gethash "strings" arguments)))
         (command (and (hash-table-p arguments)
                       (or (gethash "command" arguments) (gethash "cmd" arguments))))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (texts (cond
                  ((and (stringp command) (plusp (length command))) (list command))
                  ((and (stringp form) (plusp (length form)))
                   (loop for one in (read-forms form) nconc (form-commands one strings)))
                  ((consp form) (form-commands form strings))
                  (t nil)))
         (tokens '()))
    (dolist (text texts tokens)
      (setf tokens (union tokens (command-tokens text) :test #'equal)))))

;;; --- the two hooks ------------------------------------------------------------------

(defun refuse-covered (op next)
  "The :TOOL point: a call whose own command carries every token of a stored
scar answers the block, and NEXT never runs."
  (let ((name (getf op :name)) (arguments (getf op :arguments)))
    (if (and (stringp name) (member name +own-tools+ :test #'equal))
        (funcall next op)
        (let ((tokens (and (hash-table-p arguments) (call-tokens arguments))))
          (if (null tokens)
              (funcall next op)
              (multiple-value-bind (status rows) (read-scars)
                (cond
                  ((eq status :unreadable) (nle:failure +unreadable-text+))
                  (t (let ((row (matching-scar rows tokens)))
                       (if row
                           (nle:failure (block-text row))
                           (funcall next op)))))))))))

(defun observe-question (next &rest args)
  "The ASK-OPERATOR advice: ask as usual, and when a person answered Deny to
a command that carries an rm shape, keep that shape. Answers what the
question answered, and never changes it."
  (multiple-value-bind (answer why) (apply next args)
    (when (and (eq why :answered) (denial-p answer) *scar*)
      (when (setting :record)
        (record-deny (getf (rest args) :detail))))
    (values answer why)))

;;; --- /scar ---------------------------------------------------------------------------

(defun first-word (text)
  "TEXT's first word and the rest after it."
  (let ((space (position-if (lambda (c) (or (char= c #\Space) (char= c #\Tab))) text)))
    (if space
        (values (subseq text 0 space) (string-left-trim '(#\Space #\Tab) (subseq text space)))
        (values text ""))))

(defun run-slash (args session-id)
  "/scar: the ledger from the operator's own hand. Every subcommand is one
of the verbs, and forget here needs no second question: typing it is the
answer."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (multiple-value-bind (word rest) (first-word text)
      (let ((word (string-downcase word)))
        (cond
          ((member word '("" "list" "ls" "scars") :test #'equal) (list-scars))
          ((member word '("check" "test") :test #'equal)
           (if (plusp (length rest))
               (check rest)
               "usage: /scar check <command>"))
          ((member word '("deny" "never") :test #'equal)
           (if (plusp (length rest))
               (deny rest)
               "usage: /scar deny <command>"))
          ((member word '("forget" "undo") :test #'equal)
           (let ((id (ignore-errors (parse-integer (nlk:trimmed rest)))))
             (if id (forget id :confirm nil) "usage: /scar forget <id>")))
          ((member word '("help" "-h" "--help") :test #'equal) +manual+)
          (t (format nil "Unknown option: ~a~%~%~a" word +manual+)))))))

;;; --- the cell -------------------------------------------------------------------------

(nle:define-cell scar
  (:section ("scar")
    (:guide "nothing is required: every Deny you give a gate is kept, and /scar lists them; max_scars caps the ledger and ask_seconds bounds a forget question")
    ("file" :string :default ""
     :doc "the ledger file; empty is scar.json in this nodecode home")
    ("max_scars" :integer :default 200 :min 1
     :doc "how many scars the ledger holds before a new one is refused")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long a scar_forget question waits for an answer")
    ("record" :boolean :default t
     :doc "keep the shape of a command the operator denies"))
  (:help :scar "scar: the rm shape you denied is stopped when it comes back; /scar lists, checks, denies and forgets them" +manual+)
  (:tool "scar_check" (nlk:contract 'check-arguments) #'check-tool)
  (:tool "scar_deny" (nlk:contract 'deny-arguments) #'deny-tool)
  (:tool "scar_list" (nlk:contract 'list-arguments) #'list-tool)
  (:tool "scar_forget" (nlk:contract 'forget-arguments) #'forget-tool)
  (:hook :tool #'refuse-covered)
  (:hook 'nle:ask-operator #'observe-question)
  (:command "scar" #'run-slash
            :description "The rm shapes you denied, and how to forget one"
            :argument-hint "[list | check <command> | deny <command> | forget <id>]"))
