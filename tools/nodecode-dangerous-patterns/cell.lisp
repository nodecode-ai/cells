;;;; cell.lisp --- custom dangerous patterns: the operator's own regex rules,
;;;; read before a shell command runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of scross01's custom-dangerous-patterns plugin for Hermes, in
;;;; which a YAML file of patterns extends the harness's approval system: a
;;;; `block' pattern escalates the command to a human Allow/Deny prompt, a
;;;; `deny' pattern stops it before any prompt.
;;;;
;;;; Here the same two actions ride one NLE:HOOK on the :TOOL point. A call
;;;; is READ, not grepped: the spawn calls ((sh ...), run-program,
;;;; launch-program, run-shell-command) are found in the form, each one's
;;;; command is taken from its literal arguments, and a shell command is cut
;;;; into the commands it runs, one per line. A rule whose "read" is "call"
;;;; instead reads the whole call, its strings included -- which is what a
;;;; rule about a file being written wants.
;;;;
;;;; The rules live in one config section:
;;;;
;;;;   "cells": ["nodecode-dangerous-patterns"],
;;;;   "dangerous-patterns": {
;;;;     "allow": ["\\brm\\s+-rf\\s+/tmp/scratch-"],
;;;;     "deny":  [{"id": "deploy", "pattern": "\\bmake\\s+deploy\\b",
;;;;                "reason": "deploy"}],
;;;;     "block": [{"id": "push", "pattern": "\\bgit\\s+push\\b",
;;;;                "reason": "push", "read": "commands"}]
;;;;   }
;;;;
;;;; Like guard, this is a speed bump, not a security boundary: eval has the
;;;; machine's full authority, and a command written in a way no pattern sees
;;;; passes.

(defpackage #:nodecode-dangerous-patterns
  (:use #:cl))

(in-package #:nodecode-dangerous-patterns)

(nlk:define-peripheral dangerous-patterns :not-running t)

;;; --- what a call runs ------------------------------------------------------

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose arguments are a command the call runs.")

(defun operator-name (x)
  "X's symbol name, lowercased, or NIL. Symbols are compared by name so a
form read in any package is recognized."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun dollar-name (x)
  "The strings-object name a ($ \"name\") form reads, or NIL."
  (and (consp x)
       (equal "$" (operator-name (car x)))
       (stringp (second x))
       (second x)))

(defun literal-command (x strings)
  "The text X alone names as a command, or NIL when X hides it."
  (cond ((stringp x) x)
        ((dollar-name x) (and strings (gethash (dollar-name x) strings)))
        ((and (consp x)
              (member (operator-name (car x)) '("list" "quote") :test #'equal))
         (let ((items (if (equal "quote" (operator-name (car x)))
                          (second x)
                          (rest x))))
           (when (and (listp items) items (every #'stringp items))
             (format nil "~{~a~^ ~}" items))))
        (t nil)))

(defun spawn-call-command (call strings)
  "CALL's command text, or NIL when one of its arguments hides it."
  (let ((rest (rest call)) (parts '()) (hidden nil))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest)) ; a keyword and its value are settings
                 (let ((text (literal-command (first rest) strings)))
                   (cond (text (push text parts))
                         (t (setf hidden t)))
                   (setf rest (cdr rest)))))
    (when (and (not hidden) parts)
      (format nil "~{~a~^ ~}" (nreverse parts)))))

(defun call-commands (form-text strings)
  "The commands FORM-TEXT runs, and how to read them.
=> (values TEXT MODE): MODE :COMMAND when the form shows them, :LOOSE when
one is hidden behind a variable or a function, NIL when the form runs nothing."
  (let ((commands '()) (hidden nil) (saw nil))
    (labels ((walk (x)
               (cond ((consp x)
                      (let ((name (operator-name (car x))))
                        (when (member name +spawn-operators+ :test #'equal)
                          (setf saw t)
                          (let ((command (spawn-call-command x strings)))
                            (if command (push command commands) (setf hidden t)))))
                      (walk (car x))
                      (walk (cdr x)))
                     (t (when (member (operator-name x) +spawn-operators+ :test #'equal)
                          (setf saw t))))))
      (when (stringp form-text)
        (ignore-errors
          (with-input-from-string (in form-text)
            (let ((*read-eval* nil) (*package* (find-package :nodecode-dangerous-patterns)))
              (loop for form = (read in nil :eof)
                    until (eq form :eof)
                    do (walk form))))))
      (cond ((and saw (not hidden))
             (values (format nil "~{~a~^~%~}" (nreverse commands)) :command))
            (saw (values form-text :loose))
            (t (values nil nil))))))

(defparameter +backslash+ (code-char 92) "The shell's escape character.")
(defparameter +quote-chars+ (format nil "~c~c" (code-char 34) (code-char 39))
  "The characters that open and close a shell quotation.")
(defparameter +command-breaks+ ";|&"
  "An unquoted one of these starts the next command.")
(defparameter +token-breaks+
  (format nil " ~c~c~c~c~c()[]{};|&,"
          #\Tab #\Newline #\Return (code-char 34) (code-char 39))
  "What separates one token of a loosely read call from the next.")

(defun command-lines (text)
  "TEXT's commands, one per line and trimmed: an unquoted ; & | or newline
starts the next one, and a quoted one is part of the word it sits in."
  (let ((lines '()) (start 0) (i 0) (n (length text)) (quote nil))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c +backslash+) (incf i 2))
                     (quote (when (char= c quote) (setf quote nil)) (incf i))
                     ((position c +quote-chars+) (setf quote c) (incf i))
                     ((or (find c +command-breaks+) (char= c #\Newline))
                      (push (subseq text start i) lines)
                      (setf start (1+ i))
                      (incf i))
                     (t (incf i)))))
    (push (subseq text start) lines)
    (format nil "~{~a~^~%~}"
            (remove "" (mapcar (lambda (line)
                                  (string-trim (list #\Space #\Tab #\Return) line))
                                (nreverse lines))))))

(defun loose-lines (text)
  "TEXT tokenized broadly, every token on its own line."
  (with-output-to-string (out)
    (loop for c across text
          do (if (find c +token-breaks+)
                 (terpri out)
                 (write-char c out)))))

(defun printed-forms (text)
  "TEXT's forms printed back with their comments gone, or TEXT itself when it
will not read."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil) (*package* (find-package :nodecode-dangerous-patterns))
              (parts '()))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push (write-to-string form :readably t :case :downcase) parts))
          (format nil "~{~a~^ ~}" (nreverse parts))))
    (error () text)))

(defun carried-strings (strings)
  "Every value STRINGS carries, space separated, or the empty string."
  (if (hash-table-p strings)
      (with-output-to-string (out)
        (loop for value being the hash-values of strings
              do (format out " ~a" value)))
      ""))

(defun whole-call-text (form-text strings)
  "FORM-TEXT with its comments gone, plus every string the call carries."
  (concatenate 'string (printed-forms form-text) (carried-strings strings)))

;;; --- the rules --------------------------------------------------------------

(defun read-patterns (table action)
  "The rules under ACTION (\"deny\" or \"block\") in TABLE, in file order.
A rule without a pattern string is skipped here; one of the wrong type
refused already, in NLK:CONFIG-STRING."
  (let ((rules '()))
    (loop for entry across (nlk:json-array table action)
          for index from 0
          do (when (hash-table-p entry)
               (let ((pattern (nlk:config-string entry "pattern")))
                 (when (and (stringp pattern) (plusp (length pattern)))
                   (push (list :id (nlk:config-string entry "id"
                                                      (format nil "~a-~a" action (1+ index)))
                               :reason (nlk:config-string entry "reason" "")
                               :pattern pattern
                               :action action
                               :read (if (equal "call" (nlk:config-string entry "read" "commands"))
                                         :call
                                         :commands))
                         rules)))))
    (nreverse rules)))

(defun compile-patterns (rules allow)
  "RULES with a compiled engine each, in the order they were written."
  (loop for rule in rules
        collect (append rule
                        (list :engine
                              (nlk:compile-text-rules
                               :rules (list (list :id (getf rule :id)
                                                  :pattern (getf rule :pattern)
                                                  :reason (getf rule :reason)))
                               :allow allow)))))

(defun compile-settings (values table)
  "The compiled engines are settings: one compile per start."
  (let ((allow (getf values :allow)))
    (append (list :deny (compile-patterns (read-patterns table "deny") allow)
                  :block (compile-patterns (read-patterns table "block") allow))
            values)))

(defun hit (rules command-text call-text)
  "The first rule in RULES that matches, and the text of its match."
  (dolist (rule rules (values nil nil))
    (let* ((read (getf rule :read))
           (text (if (eq read :call) call-text command-text))
           (refusal (and text (nlk:text-rules-refusal (getf rule :engine) text))))
      (when refusal
        (return (values rule refusal))))))

(defun call-texts (text kind strings)
  "What the rules read of a call: the commands it runs, and the whole call.
KIND is :FORM for an eval form read into its spawn calls, :COMMAND for a
text that is already a shell command."
  (if (eq kind :command)
      (values (command-lines text) (loose-lines text))
      (multiple-value-bind (commands mode) (call-commands text strings)
        (values (cond ((null commands) nil)
                      ((eq mode :command) (command-lines commands))
                      (t (loose-lines text)))
                (loose-lines (whole-call-text text strings))))))

(defun arguments-text (arguments)
  "The call's text and its kind, or NIL when the call carries none."
  (when (hash-table-p arguments)
    (let ((form (gethash "form" arguments))
          (command (gethash "command" arguments))
          (cmd (gethash "cmd" arguments)))
      (cond ((stringp form) (values form :form))
            ((and form (not (eq form :null))) (values (format nil "~s" form) :form))
            ((stringp command) (values command :command))
            ((stringp cmd) (values cmd :command))
            (t nil)))))

(defun call-shown (command-text call-text)
  "What a question shows of a call: the commands it runs, else the call."
  (let ((text (or command-text call-text "")))
    (if (> (length text) 1500) (concatenate 'string (subseq text 0 1500) " ...") text)))

;;; --- asking, and the refusal -------------------------------------------------

(defparameter +allow-once+ "Allow once")
(defparameter +deny+ "Deny")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun ask-before-run (rule command-text call-text)
  "A block rule asks the operator whether the call may run once.
=> the answer, or NIL and why: :NOBODY, :TIMEOUT."
  (let ((ask (ask-operator-function)))
    (if (null ask)
        (values nil :nobody)
        (funcall ask (format nil "A block pattern stopped a call: rule ~a~@[ (~a)~]"
                             (getf rule :id) (getf rule :reason))
                 :detail (call-shown command-text call-text)
                 :choices (list +allow-once+ +deny+)
                 :seconds (max 5 (or (setting :ask-seconds) 120))))))

(defun deny-text (refusal)
  "The model's words for a call a deny rule stopped without a prompt."
  (format nil "ERROR: refused by nodecode-dangerous-patterns: ~a; this call matches a deny pattern and was stopped without a prompt -- give the operator the command to run themselves, or ask them to retire the rule" refusal))

(defun block-text (refusal answer why)
  "The model's words for a call a block rule stopped."
  (cond ((equal answer +deny+)
         (format nil "ERROR: refused by nodecode-dangerous-patterns: ~a; the operator was asked and chose Deny; the call did not run -- do not retry it or work around it: ask them what they want instead" refusal))
        ((eq why :timeout)
         (format nil "ERROR: refused by nodecode-dangerous-patterns: ~a; the operator was asked and did not answer in time; the call did not run -- give the operator the command to run themselves" refusal))
        (t
         (format nil "ERROR: refused by nodecode-dangerous-patterns: ~a; the call did not run -- give the operator the command to run themselves" refusal))))

(defun refuse-or-run (op next)
  "The :TOOL point: deny stops the call, block asks first, anything else runs."
  (multiple-value-bind (text kind) (arguments-text (getf op :arguments))
    (if (null text)
        (funcall next op)
        (multiple-value-bind (command-text call-text)
            (call-texts text kind (and (hash-table-p (getf op :arguments))
                                       (gethash "strings" (getf op :arguments))))
          (multiple-value-bind (rule refusal) (hit (setting :deny) command-text call-text)
            (cond (rule (nle:failure (deny-text refusal)))
                  (t (multiple-value-bind (rule refusal)
                         (hit (setting :block) command-text call-text)
                       (if (null rule)
                           (funcall next op)
                           (multiple-value-bind (answer why)
                               (ask-before-run rule command-text call-text)
                             (if (equal answer +allow-once+)
                                 (funcall next op)
                                 (nle:failure (block-text refusal answer why)))))))))))))

;;; --- /patterns ---------------------------------------------------------------

(defun rule-line (rule)
  "One rule as the report shows it."
  (format nil "  ~a (~a)~@[ -- ~a~] [~a] ~a"
          (getf rule :id)
          (getf rule :action)
          (let ((reason (getf rule :reason)))
            (and (plusp (length reason)) reason))
          (if (eq (getf rule :read) :call) "the whole call" "the commands a call runs")
          (getf rule :pattern)))

(defun patterns-report ()
  "Every rule this cell holds, and how to try one."
  (let ((deny (setting :deny))
        (block (setting :block))
        (allow (setting :allow)))
    (with-output-to-string (out)
      (format out "custom dangerous patterns: ~a deny, ~a block"
              (length deny) (length block))
      (dolist (pair (list (cons "deny (stopped at once, no prompt)" deny)
                           (cons "block (asks Allow once or Deny)" block)))
        (when (cdr pair)
          (format out "~%~a" (car pair))
          (dolist (rule (cdr pair))
            (format out "~%~a" (rule-line rule)))))
      (when allow
        (format out "~%allow (exempt): ~{~a~^, ~}" allow))
      (unless (or deny block)
        (format out "~%no rules yet: add a \"deny\" or \"block\" array of {id, pattern, reason} to the \"dangerous-patterns\" section"))
      (format out "~%~%(/patterns test <command> says what that command would earn.)"))))

(defun patterns-test (command)
  "What COMMAND would earn from the rules in force."
  (multiple-value-bind (command-text call-text) (call-texts command :command nil)
    (multiple-value-bind (rule refusal) (hit (setting :deny) command-text call-text)
      (cond (rule (format nil "~a would be stopped at once by a deny rule: ~a" command refusal))
            (t (multiple-value-bind (rule refusal) (hit (setting :block) command-text call-text)
                 (if rule
                     (format nil "~a would ask the operator Allow once or Deny: ~a" command refusal)
                     (format nil "~a matches no rule: it would run" command))))))))

(defparameter +argument-breaks+ (list #\Space #\Tab)
  "What separates one /patterns argument from the next.")

(defun patterns-first-word (text)
  (let ((space (position-if (lambda (c) (find c +argument-breaks+)) text)))
    (if space
        (values (subseq text 0 space)
                (string-left-trim +argument-breaks+ (subseq text space)))
        (values text ""))))

(defun run-patterns (args session-id)
  "One /patterns invocation: list the rules, or test one command."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (multiple-value-bind (word rest) (patterns-first-word text)
      (let ((word (string-downcase word)))
        (cond ((member word '("" "list" "rules") :test #'equal) (patterns-report))
              ((member word '("test" "check") :test #'equal)
               (if (plusp (length rest))
                   (patterns-test rest)
                   "usage: /patterns test <command>"))
              (t (patterns-test text)))))))

(nle:define-cell dangerous-patterns
  (:section ("dangerous-patterns")
    (:guide "deny refuses a matching call at once; block asks Allow once or Deny; allow exempts what it matches; read is commands (the default) or call")
    ("allow" :list :doc "patterns whose matching text no rule reads")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long a block rule's question waits for an answer"))
  (:settings #'compile-settings)
  (:hook :tool #'refuse-or-run)
  (:command "patterns" 'run-patterns
            :description "List your dangerous-pattern rules, or test one command against them"
            :argument-hint "list | test <command>"))
