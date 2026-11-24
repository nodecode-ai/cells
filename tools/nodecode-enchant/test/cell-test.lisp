;;;; cell-test.lisp --- the enchant cell against a stubbed side call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one model call a rewrite makes is a stubbed NLE:COMPLETE: nothing
;;;; reaches a provider, and every call is kept so a test can read the system
;;;; message and the user message the model was actually given. The tools are
;;;; called the way a model's call reaches them - through the real dispatch,
;;;; contract and all - and the command the way a shell runs it, through its
;;;; own function with the composer's draft, reading the effect it answers.

(in-package #:nodecode.test)

(define-test-slice "enchant" "ENCHANT-CELL-"
  :start nodecode-enchant:start-cell)

(defvar *asked* '()
  "(SYSTEM USER) pairs the stubbed side call was asked with, newest first.")

(defmacro with-model ((answer) &body body)
  "BODY with the one side call answered by ANSWER, read in this scope, and
every (SYSTEM USER) pair it was asked with collected in *ASKED*."
  `(let ((*asked* '()))
     (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (push (list system user) *asked*)
                                ,answer)
       ,@body)))

(defun enchant-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real
dispatch, the contract's check and all."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun drafted (effects)
  "The draft an answer's first effect sets, or NIL when it sets none."
  (and effects (getf (first effects) :draft)))

;;; --- the review, a pure function of two texts --------------------------------------
(deftest enchant-cell-cuts-a-rewrite-into-groups ()
  (multiple-value-bind (groups whole) (nodecode-enchant:change-groups "make login fast" "Make login fast now")
    (is (not whole) "a short change is reviewed in groups")
    (is (= 2 (length groups)) "one group for the capital, one for what was added")
    (is (equal "make" (getf (first groups) :removed)))
    (is (equal "Make" (getf (first groups) :replacement)))
    (is (equal "" (getf (second groups) :removed)) "nothing is taken out to add an ending")
    (is (equal " now" (getf (second groups) :replacement)))
    (let ((whole (nodecode-enchant:compose "make login fast" groups)))
      (is (search "Make login fast" whole) "every group composed back in is the rewrite")
      (is (search "now" whole) "down to the change that ends it"))
    (is (equal "Make login fast" (nodecode-enchant:compose "make login fast" groups '("change-1"))) "the first group alone is only the capital")))

(deftest enchant-cell-calls-a-long-change-whole ()
  (let ((long (format nil "~{~a ~}" (loop for i below 600 collect (format nil "w~d" i)))))
    (multiple-value-bind (groups whole) (nodecode-enchant:change-groups long (concatenate 'string long "more"))
      (is whole "past the token ceiling the whole rewrite is offered instead")
      (is (null groups)))))

;;; --- the lifecycle, from the one declaration ---------------------------------------
(define-cell-lifecycle-tests "enchant"
  (:help :enchant)
  (:command "enchant")
  (:running (is (string= "clarify" (nodecode-enchant::setting :enhancer)) "the default enhancer"))
  (:refused ("enhancer" "wobble") ("history" 0)))

;;; --- the tools ------------------------------------------------------------------
(deftest enchant-cell-proposes-and-leaves-the-draft (with-cell-stop ((enchant-start))
  (with-model ("Make login fast now")
    (let ((answer (enchant-answer "e1" "enchant_propose" "{\"draft\": \"make login fast\"}")))
      (is (search "change-1" answer) "the answer lists the groups to pick from")
      (is (search "Make login fast now" answer) "and the rewrite whole")
      (is (= 1 (length *asked*)) "one side call, no turn")
      (is (search "Make the request unambiguous" (first (first *asked*))) "the enhancer's own instruction is the system message")
      (is (search "Return only the rewritten prompt." (first (first *asked*))))
      (is (string= "make login fast" (second (first *asked*))) "the draft is the user message")))))
(deftest enchant-cell-names-the-enhancer-it-runs (with-cell-stop ((enchant-start "enhancer" "tighten"))
  (with-model ("Make login fast")
    (enchant-answer "e2" "enchant_propose" "{\"draft\": \"make login fast\", \"enhancer\": \"grammar\"}")
    (is (search "Correct grammar and style" (first (first *asked*))) "the enhancer the call names wins")
    (enchant-answer "e3" "enchant_propose" "{\"draft\": \"make login fast\"}")
    (is (search "Remove filler" (first (first *asked*))) "an empty one takes the section's"))))
(deftest enchant-cell-takes-only-the-groups-named (with-cell-stop ((enchant-start))
  (with-model ("Make login fast now")
    (enchant-answer "e4" "enchant_propose" "{\"draft\": \"make login fast\"}")
    (let ((answer (enchant-answer "e5" "enchant_apply" "{\"groups\": [\"change-1\"]}")))
      (is (search "Make login fast" answer) "only the group named is composed in")
      (is (not (search "fast now" answer)) "the change left out is not there")
      (is (= 1 (length *asked*)) "taking a proposal spends no model call")))))
(deftest enchant-cell-undoes-and-redoes-without-asking-again (with-cell-stop ((enchant-start))
  (with-model ("Make login fast now")
    (enchant-answer "e6" "enchant_propose" "{\"draft\": \"make login fast\"}")
    (enchant-answer "e7" "enchant_apply" "{}")
    (let ((undone (enchant-answer "e8" "enchant_history" "{\"action\": \"undo\"}")))
      (is (search "make login fast" undone) "undo answers the draft it was")
      (is (not (search "now" undone))))
    (let ((redone (enchant-answer "e9" "enchant_history" "{\"action\": \"redo\"}")))
      (is (search "Make login fast now" redone) "redo answers the revision it was taken to"))
    (is (= 1 (length *asked*)) "undo and redo spend no model call"))))
(deftest enchant-cell-reports-a-failed-call (with-cell-stop ((enchant-start))
  (with-model ((error "the lane is down"))
    (let ((answer (enchant-answer "e10" "enchant_propose" "{\"draft\": \"make login fast\"}")))
      (is (search "enchant_propose failed" answer) "a refused provider is reported, not thrown at the model")
      (is (search "the lane is down" answer))))))
(deftest enchant-cell-refuses-a-call-with-no-draft (with-cell-stop ((enchant-start))
  (with-model ("never asked")
    (let ((answer (enchant-answer "e11" "enchant_propose" "{}")))
      (is (search "preflight" answer) "the contract refuses it before the body")
      (is (null *asked*))))))
;;; --- the command ------------------------------------------------------------------
(deftest enchant-cell-reviews-what-the-composer-holds (with-cell-stop ((enchant-start))
  (with-model ("Make login fast now")
    (multiple-value-bind (text dialog effects)
        (nodecode-enchant::run-slash "" "s-ench-1" :draft "make login fast")
      (is (search "change-1" text) "the review is the text a shell shows")
      (is (functionp (lambda () dialog)) "the dialog rides along for a shell that draws one")
      (is (null effects) "the composer's draft is untouched until it is taken")
      (is (string= "make login fast" (second (first *asked*))))))))
(deftest enchant-cell-applies-what-it-proposed (with-cell-stop ((enchant-start))
  (with-model ("Make login fast now")
    (nodecode-enchant::run-slash "" "s-ench-2" :draft "make login fast")
    (multiple-value-bind (text dialog effects)
        (nodecode-enchant::run-slash "apply all" "s-ench-2")
      (declare (ignore dialog))
      (is (search "applied" text))
      (is (equal "Make login fast now" (drafted effects)) "the accepted text is set in the composer")
      (is (eq :set (getf (first effects) :how)) "replacing the draft, not sending it")))))
(deftest enchant-cell-instants-a-rewrite (with-cell-stop ((enchant-start))
  (with-model ("Ship it today")
    (multiple-value-bind (text dialog effects)
        (nodecode-enchant::run-slash "instant tighten" "s-ench-3" :draft "ship it today")
      (declare (ignore dialog))
      (is (search "applied whole" text))
      (is (equal "Ship it today" (drafted effects)))))))
(deftest enchant-cell-refuses-to-overwrite-an-edited-draft (with-cell-stop ((enchant-start))
  (with-model ("Make login fast now")
    (nodecode-enchant::run-slash "" "s-ench-4" :draft "make login fast")
    (nodecode-enchant::run-slash "apply all" "s-ench-4")
    (multiple-value-bind (text dialog effects)
        (nodecode-enchant::run-slash "undo" "s-ench-4" :draft "something else I typed")
      (declare (ignore dialog))
      (is (search "changed since the last enchant" text) "a draft that moved on is not overwritten")
      (is (null effects))))))
(deftest enchant-cell-lists-the-enhancers (with-cell-stop ((enchant-start))
  (with-model ("never asked")
    (multiple-value-bind (text dialog effects)
        (nodecode-enchant::run-slash "list" "s-ench-5")
      (declare (ignore effects))
      (is (search "pick an enhancer" text))
      (is (= 5 (length (getf dialog :rows))) "a row per enhancer")
      (is (string= "/enchant summarize" (getf (getf (first (last (getf dialog :rows))) :run) :enter)) "and a row runs the enhancer it names")
      (is (null *asked*) "listing costs no model call")))))
