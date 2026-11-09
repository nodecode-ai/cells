;;;; cell-test.lisp --- the calling-mom cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a model: the side call is bound to a scripted
;;;; answer, so "mom was asked once", "the expensive tier was asked only
;;;; after she escalated" and "a token never left the machine" are facts a
;;;; test reads rather than a provider's mood. The tools are called the way
;;;; a model's call reaches them, through the real dispatch and contract.

(in-package #:nodecode.test)

(define-test-slice "calling-mom" "CALLING-MOM-CELL-"
  :start nodecode-calling-mom:start-cell)

(defvar *asked* '()
  "(SYSTEM USER) pairs the stubbed side call was asked with, newest first.")

(defmacro with-mom ((&rest answers) &body body)
  "Run BODY with the cell's one side call answering ANSWERS in turn, and
every (SYSTEM USER) pair it was asked with collected in *ASKED*."
  `(let* ((*asked* '())
          (answers (list ,@answers))
          (nodecode-calling-mom::*call*
            (lambda (system user)
              (push (list system user) *asked*)
              (pop answers))))
     ,@body))

(defun ask-tool (id arguments)
  "The second_opinion tool's answer to ARGUMENTS, as the model reads it."
  (nle::message-content (execute-wire-call id "second_opinion" arguments)))

(defun risk-tool (id arguments)
  "The risk_check tool's answer to ARGUMENTS."
  (nle::message-content (execute-wire-call id "risk_check" arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "calling-mom"
  (:hooks :tool)
  (:help :calling-mom)
  (:command "calling-mom")
  (:refused ("cooldown_seconds" -1)))

;;; --- the rules, which are data ------------------------------------------------------

(deftest calling-mom-cell-rules-name-their-tier ()
  (multiple-value-bind (tier id) (nodecode-calling-mom:classify "git push --force origin main")
    (is (eq :critical tier) "a force push is critical")
    (is (equal "force-push" id) (format nil "the rule that matched names itself: ~a" id)))
  (multiple-value-bind (tier id) (nodecode-calling-mom:classify "sudo kubectl -n prod delete pod web-1")
    (is (eq :critical tier) "a wrapped delete is still critical")
    (is (equal "kubectl" id) (format nil "read through the wrapper: ~a" id)))
  (is (eq :watch (nodecode-calling-mom:classify "ssh root@db-1 uptime")) "ssh is a watch, not a critical")
  (is (null (nodecode-calling-mom:classify "ls -la && git status")) "a routine command matches nothing")
  (is (eq :critical (nodecode-calling-mom:classify "deploy prod" :extra (list "deploy"))) "the operator's own regexes join the critical tier")
  (multiple-value-bind (file id) (nodecode-calling-mom:critical-file "infra/main.tf")
    (is (and file (equal "terraform" id)) "a terraform file is critical to change")))

;;; --- the cheap tier answers -----------------------------------------------------------

(deftest calling-mom-cell-the-tool-asks-mom-once ()
  (with-cell-stop ((calling-mom-start "mom" "openrouter/deepseek/cheap"))
    (with-mom ("Recommend option B: it keeps the old keys readable. Risk: the migration order.")
      (let ((answer (ask-tool "o1" "{\"question\":\"rename the table or add a view?\",\"context\":\"no downtime\"}")))
        (is (search "Recommend option B" answer) "the advice is the answer")
        (is (search "[advisor deepseek/cheap]" answer) "signed by the model that answered")
        (is (= 1 (length *asked*)) "one side call")
        (let ((system (first (first *asked*)))
              (user (second (first *asked*))))
          (is (search "design question from a coding agent" system) "the original's instruction, verbatim")
          (is (null (search "ESCALATE" system)) "no gate without an expensive tier")
          (is (search "rename the table or add a view?" user) "the question is what is asked")
          (is (search "no downtime" user) "with its context"))))))

(deftest calling-mom-cell-a-secret-never-leaves-the-machine ()
  (with-cell-stop ((calling-mom-start "mom" "openrouter/deepseek/cheap"))
    (with-mom ("fine")
      (ask-tool "o2" "{\"question\":\"rotate sk-abcdefghijklmnopqrstuvwxyz012345 or not?\"}")
      (let ((user (second (first *asked*))))
        (is (null (search "abcdefghijklmnopqrstuvwxyz012345" user)) "the key is blanked")
        (is (search "sk-***" user) "in place, so the sentence still reads")))
    (with-mom ("fine")
      (ask-tool "o3" "{\"question\":\"does https://deploy:hunter2@example.test/hook work?\"}")
      (let ((user (second (first *asked*))))
        (is (null (search "hunter2" user)) "a password inside a URL is blanked")
        (is (search "deploy:***@example.test" user) "and the host survives")))))

;;; --- the gate: olymp is asked only when mom escalates ---------------------------------

(deftest calling-mom-cell-olymp-is-asked-only-on-an-escalation ()
  (with-cell-stop ((calling-mom-start "mom" "openrouter/deepseek/cheap"
                                      "olymp" "claude/claude-opus"))
    (with-mom ("ESCALATE: a public release with no rollback"
               "Ask the operator before publishing; a yanked release cannot be unpublished.")
      (let ((answer (ask-tool "o4" "{\"question\":\"publish 2.0 now?\"}")))
        (is (search "claude-opus" answer) "the expensive tier answered")
        (is (search "cannot be unpublished" answer) "with its own words")
        (is (= 2 (length *asked*)) "two calls: mom first, then olymp")
        (is (search "You are the FIRST reviewer" (first (second *asked*))) "mom was told she is the first reviewer")
        (is (null (search "FIRST reviewer" (first (first *asked*)))) "and olymp was not")))
    (is (= 1 nodecode-calling-mom::*olymp-today*) "the escalation is counted")))

(deftest calling-mom-cell-the-expensive-tier-has-a-hard-limit ()
  (with-cell-stop ((calling-mom-start "mom" "openrouter/deepseek/cheap"
                                      "olymp" "claude/claude-opus"
                                      "olymp_calls_per_day" 1))
    (with-mom ("ESCALATE: risky" "go ahead" "ESCALATE: risky again")
      (ask-tool "o5" "{\"question\":\"ship it?\"}")
      (let ((second (ask-tool "o6" "{\"question\":\"and now?\"}")))
        (is (search "ESCALATE: risky again" second) "with the limit used up, mom's own word stands")
        (is (= 3 (length *asked*)) "olymp is not asked twice")))))

;;; --- the budget -----------------------------------------------------------------------

(deftest calling-mom-cell-a-used-budget-refuses ()
  (with-cell-stop ((calling-mom-start "mom" "openrouter/deepseek/cheap"
                                      "mom_calls_per_day" 1))
    (with-mom ("one answer")
      (is (search "one answer" (ask-tool "o7" "{\"question\":\"first\"}")) "the first is answered")
      (is (search "no second opinion today" (ask-tool "o8" "{\"question\":\"second\"}")) "the second says why it cannot")
      (is (= 1 (length *asked*)) "and never reaches a model"))))

;;; --- the watcher ------------------------------------------------------------------------

(defun hook-call (form result)
  "The :TOOL hook run around one call of FORM answering RESULT, as the
dispatch runs it: (lambda (OP NEXT))."
  (funcall (cell-hook "nodecode-calling-mom" :tool)
           (list :name "eval" :arguments (nlk:json-object "form" form) :call-id "c1")
           (lambda (op) (declare (ignore op)) result)))

(deftest calling-mom-cell-the-same-answer-three-times-calls-mom ()
  (with-cell-stop ((calling-mom-start "assist" t "cooldown_seconds" 0
                                      "mom" "openrouter/deepseek/cheap"))
    (with-mom ("the loop is in the retry: it never advances the cursor.")
      (is (null (search "[advisor" (hook-call "(search \"x\")" "no matches for x"))) "the first is silent")
      (is (null (search "[advisor" (hook-call "(search \"x\")" "no matches for x"))) "and so is the second")
      (let ((third (hook-call "(search \"x\")" "no matches for x")))
        (is (search "no matches for x" third) "the result is kept")
        (is (search "the loop is in the retry" third) "with mom's line after it")
        (is (search "Signal: same-result" (second (first *asked*))) "and she is told what fired")))))

(deftest calling-mom-cell-a-critical-command-is-reviewed-once ()
  (with-cell-stop ((calling-mom-start "assist" t "cooldown_seconds" 0
                                      "mom" "openrouter/deepseek/cheap"))
    (with-mom ("Biggest risk: the force push rewrites the shared branch.")
      (let ((answer (hook-call "(sh \"git push --force origin main\")" "pushed")))
        (is (search "Biggest risk" answer) "the review rides the result")
        (is (search "critical_change" (second (first *asked*))) "as the critical-change question"))
      (is (= 1 (length *asked*)) "one review for one command")
      (hook-call "(sh \"git push --force origin main\")" "pushed again, up to date")
      (is (= 1 (length *asked*)) "the same command is not reviewed again"))))

(deftest calling-mom-cell-with-the-watcher-off-nothing-is-said ()
  (with-cell-stop ((calling-mom-start "mom" "openrouter/deepseek/cheap"))
    (with-mom ("unasked")
      (let ((answer (hook-call "(search \"x\")" "the same answer")))
        (is (equal "the same answer" answer) "the result is unchanged")
        (is (null *asked*) "and no one was asked")))))

;;; --- risk_check --------------------------------------------------------------------------

(deftest calling-mom-cell-risk-check-reads-a-command ()
  (with-cell-stop ((calling-mom-start))
    (is (search "critical (force-push)" (risk-tool "r1" "{\"command\":\"git push --force origin main\"}")) "the tier and the rule")
    (is (search "watch (remote-shell)" (risk-tool "r2" "{\"command\":\"scp build.tgz host:/tmp\"}")) "privileged access is a watch")
    (is (search "routine" (risk-tool "r3" "{\"command\":\"npm test\"}")) "anything else is routine")
    (is (search "critical (terraform)" (risk-tool "r4" "{\"command\":\"apply\",\"path\":\"infra/main.tf\"}")) "a critical file is named on its own")))

;;; --- /calling-mom -----------------------------------------------------------------------

(deftest calling-mom-cell-the-command-answers-each-verb ()
  (with-cell-stop ((calling-mom-start "mom" "openrouter/deepseek/cheap"))
    (let ((status (cell-entry "nodecode-calling-mom" "calling-mom" "" "s1")))
      (is (search "calling-mom off" status) "it starts off unless the section says assist")
      (is (search "deepseek/cheap" status) "and names both tiers"))
    (is (search "calling-mom on" (cell-entry "nodecode-calling-mom" "calling-mom" "on" "s1")) "/calling-mom on turns the watcher on")
    (with-mom ("option B")
      (is (search "option B" (cell-entry "nodecode-calling-mom" "calling-mom" "ask which option?" "s1")) "ask answers the question"))
    (is (search "not one of its choices"
                (cell-entry "nodecode-calling-mom" "calling-mom" "bogus" "s1")) "an unknown verb names itself")))
