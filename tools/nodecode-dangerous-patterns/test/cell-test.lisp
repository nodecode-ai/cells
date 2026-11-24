;;;; cell-test.lisp --- lifecycle, both actions, what the rules read, and
;;;; /patterns.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The engine's own mechanics (span-scoped allow, skip-with-warn) are the
;;;; core waist's, proved in its own textrules test; what is proved here is
;;;; this cell's data and wiring: which text of a call a rule reads, deny
;;;; stopping a call before any prompt, block asking the operator first and
;;;; running only on Allow once, allow exempting what it names, and the
;;;; /patterns report and test.

(in-package #:nodecode.test)

(defun patterns-rule (id pattern &rest keys)
  "One rule as the config carries it: an object of the section's array."
  (let ((object (nlk:json-object "id" id "pattern" pattern)))
    (loop for (key value) on keys by #'cddr
          do (setf (gethash key object) value))
    object))

(defun patterns-verdict (form &rest strings)
  "The installed hook's answer for FORM with the strings object STRINGS
\(name text ...): :ALLOWED, or the refusal string."
  (funcall (cell-hook "nodecode-dangerous-patterns" :tool)
           (list :name "eval"
                 :arguments (if strings
                                (nlk:json-object
                                 "form" form
                                 "strings" (let ((object (nlk:json-object)))
                                             (loop for (name text) on strings by #'cddr
                                                   do (setf (gethash name object) text))
                                             object))
                                (nlk:json-object "form" form))
                 :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun patterns-command-verdict (command)
  "The hook's answer for a call whose tool carries COMMAND, not a form."
  (funcall (cell-hook "nodecode-dangerous-patterns" :tool)
           (list :name "shell"
                 :arguments (nlk:json-object "command" command)
                 :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun patterns-with-ask (stub thunk)
  "THUNK with the core's ASK-OPERATOR answered by STUB, put back after."
  (let* ((symbol (intern "ASK-OPERATOR" '#:nodecode.evolved))
         (saved (and (fboundp symbol) (fdefinition symbol))))
    (setf (fdefinition symbol) stub)
    (unwind-protect (funcall thunk)
      (if saved (setf (fdefinition symbol) saved) (fmakunbound symbol)))))

;;; --- lifecycle --------------------------------------------------------------

(define-cell-lifecycle-tests "dangerous-patterns"
  (:config "deny" (vector (patterns-rule "deploy" "\\bmake\\s+deploy\\b" "reason" "deploy")))
  (:hooks :tool)
  (:command "patterns")
  (:running (is (= 1 (length (nodecode-dangerous-patterns::setting :deny))) "the rules are compiled once, at start")
            (let ((report (cell-entry "nodecode-dangerous-patterns" "patterns" "list")))
              (is (search "deploy" report) "and the report names the rule")))
  (:refused ("deny" (vector (nlk:json-object "pattern" 42)))))

;;; --- deny: stopped at once, no prompt ---------------------------------------

(deftest dangerous-patterns-cell-stops-a-deny-match
    (with-cell-stop ((dangerous-patterns-start
                      "deny" (vector (patterns-rule "deploy" "\\bmake\\s+deploy\\b"
                                                     "reason" "deploy"))))
      (let ((verdict (patterns-verdict "(sh \"make deploy\")")))
        (is (stringp verdict) "a matching call is refused, not run")
        (is (search "refused by nodecode-dangerous-patterns" verdict) "naming the cell")
        (is (search "rule deploy (deploy) matched" verdict) "the rule and its reason")
        (is (search "without a prompt" verdict) "which deny never asks about"))
      (is (eq :allowed (patterns-verdict "(sh \"make build\")")) "another target runs")
      (is (eq :allowed (patterns-verdict "(sh \"rg -n deploy src\")")) "an argument that mentions the danger word passes")
      (is (eq :allowed (patterns-verdict "(+ 1 2)")) "a call that runs nothing passes")
      (is (stringp (patterns-command-verdict "make deploy")) "and a tool that carries a command argument is read the same way")))

(deftest dangerous-patterns-cell-reads-the-whole-call-only-when-asked
    (with-cell-stop ((dangerous-patterns-start
                      "deny" (vector (patterns-rule "no-delete" "\\bdelete-file\\b"
                                                     "read" "call" "reason" "file deletion"))))
      (is (search "rule no-delete" (patterns-verdict "(delete-file \"notes.txt\")")) "a read of the whole call sees a form that runs no command")
      (is (eq :allowed (patterns-verdict "(sh \"rm -f notes.txt\")")) "while a command the rule does not name passes")))

;;; --- block: the operator is asked first --------------------------------------

(deftest dangerous-patterns-cell-asks-before-a-block-match
    (let ((asked nil))
      (patterns-with-ask
       (lambda (title &rest keys) (setf asked (cons title keys)) (values "Allow once" :answered))
       (lambda ()
         (with-cell-stop ((dangerous-patterns-start
                           "block" (vector (patterns-rule "push" "\\bgit\\s+push\\b"
                                                           "reason" "push"))
                           "ask_seconds" 30))
           (is (eq :allowed (patterns-verdict "(sh \"git push origin main\")")) "Allow once runs the call")
           (is (search "rule push" (first asked)) "the question names the rule")
           (is (equal "git push origin main" (getf (rest asked) :detail)) "and shows the command the call runs")
           (is (equal '("Allow once" "Deny") (getf (rest asked) :choices)) "offering Allow once or Deny")
           (is (= 30 (getf (rest asked) :seconds)) "for ask_seconds")
           (is (eq :allowed (patterns-verdict "(sh \"git status\")")) "an unmatched call runs unasked"))))))

(deftest dangerous-patterns-cell-refuses-what-the-operator-denies-or-leaves
    (patterns-with-ask
     (lambda (&rest args) (declare (ignore args)) (values "Deny" :answered))
     (lambda ()
       (with-cell-stop ((dangerous-patterns-start
                         "block" (vector (patterns-rule "push" "\\bgit\\s+push\\b"
                                                         "reason" "push"))))
         (let ((verdict (patterns-verdict "(sh \"git push\")")))
           (is (stringp verdict) "Deny refuses the call")
           (is (search "chose Deny" verdict) "saying the operator chose it")
           (is (search "do not retry" verdict) "and that the model must not work around it"))))))
    (patterns-with-ask
     (lambda (&rest args) (declare (ignore args)) (values nil :timeout))
     (lambda ()
       (with-cell-stop ((dangerous-patterns-start
                         "block" (vector (patterns-rule "push" "\\bgit\\s+push\\b"
                                                         "reason" "push"))))
         (is (search "did not answer in time" (patterns-verdict "(sh \"git push\")")) "no answer refuses, saying so"))))
    (patterns-with-ask
     (lambda (&rest args) (declare (ignore args)) (values nil :nobody))
     (lambda ()
       (with-cell-stop ((dangerous-patterns-start
                         "block" (vector (patterns-rule "push" "\\bgit\\s+push\\b"
                                                         "reason" "push"))))
         (let ((verdict (patterns-verdict "(sh \"git push\")")))
           (is (search "operator the command" verdict) "nobody to ask is the refusal as before")
           (is (not (search "was asked" verdict)) "which claims no question")))))

;;; --- allow ------------------------------------------------------------------

(deftest dangerous-patterns-cell-allow-exempts-what-it-names
    (with-cell-stop ((dangerous-patterns-start
                      "deny" (vector (patterns-rule "rm-scratch" "\\brm\\s+-rf\\b"
                                                     "reason" "recursive delete"))
                      "allow" (vector "\\brm\\s+-rf\\s+/tmp/scratch-")))
      (is (search "rule rm-scratch" (patterns-verdict "(sh \"rm -rf /srv\")")) "a delete outside the exemption is refused")
      (is (eq :allowed (patterns-verdict "(sh \"rm -rf /tmp/scratch-1\")")) "the exempt path passes")))

;;; --- /patterns --------------------------------------------------------------

(deftest dangerous-patterns-command-lists-the-rules-and-tests-a-command
    (with-cell-stop ((dangerous-patterns-start
                      "deny" (vector (patterns-rule "deploy" "\\bmake\\s+deploy\\b"
                                                     "reason" "deploy"))
                      "block" (vector (patterns-rule "push" "\\bgit\\s+push\\b"
                                                      "reason" "push"))))
      (let ((listing (cell-entry "nodecode-dangerous-patterns" "patterns" "list")))
        (is (search "1 deny, 1 block" listing) "the report counts both families")
        (is (search "deploy" listing) "and names the deny rule")
        (is (search "push" listing) "and the block rule")
        (is (search "/patterns test" listing) "and says how to try one"))
      (is (search "would be stopped at once" (cell-entry "nodecode-dangerous-patterns" "patterns" "test make deploy")) "test says a deny rule would stop the command")
      (is (search "would ask the operator" (cell-entry "nodecode-dangerous-patterns" "patterns" "test git push origin main")) "and that a block rule would ask")
      (is (search "matches no rule" (cell-entry "nodecode-dangerous-patterns" "patterns" "test ls -la")) "and that an ordinary command runs")
      (is (search "usage" (cell-entry "nodecode-dangerous-patterns" "patterns" "test")) "test with no command says how to use it")))

(deftest dangerous-patterns-cell-with-no-rules-lets-everything-run
    (with-cell-stop ((dangerous-patterns-start))
      (is (eq :allowed (patterns-verdict "(sh \"rm -rf /srv\")")) "no rules, no refusals")
      (is (search "no rules yet" (cell-entry "nodecode-dangerous-patterns" "patterns" "list")) "and the report says so")))
