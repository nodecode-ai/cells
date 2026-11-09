;;;; cell-test.lisp --- the UX3 kernel and its cell against the core's seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here is stubbed, because nothing here reaches out: the verbs read
;;;; text and answer plists, the tool is the same code behind a declaration, and
;;;; the only file the cell touches is the config the test builds. What is proved
;;;; is the original's own behaviour: the route's three answers, the three lanes,
;;;; one verdict from the weakest lane, the deterministic weakest flow, the tier
;;;; floor, the self-contradicting boundary, and a call that leaves the prompt
;;;; out refused before any body runs.

(in-package #:nodecode.test)

(define-test-slice "ux3" "UX3-CELL-" :start nodecode-ux3:start-cell)

(define-cell-lifecycle-tests "ux3"
  (:config "evidence_tier" "t0" "working_language" "en")
  (:help :ux3)
  (:command "ux3")
  (:running (is (equal "t0" (nodecode-ux3::setting :evidence-tier)) "t0 is the honest default")
            (is (equal "en" (nodecode-ux3::setting :working-language))))
  (:refused ("evidence_tier" "t9") ("evidence_tier" 2))
  (:idle ux3:ux3-error
         (ux3:route "ship it")
         (ux3:evaluate "ship it")
         (ux3:decide '((:user "verify" "t2") (:evidence "verify" "t2") (:business "verify" "t2")))))

;;; --- helpers --------------------------------------------------------------------

(defun ux3-call (name &rest pairs)
  "Call the contract tool NAME as a model's call reaches it: the arguments
decoded through the declaration, then the registered body."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defmacro with-ux3 ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS (or on the defaults when it names none)."
  `(with-cell-stop ((ux3-start ,@(if pairs pairs '("evidence_tier" "t0"))))
     ,@body))

(defun ux3-lane (lane)
  "LANE as (flow verdict tier), the flow in its review spelling."
  (list (nodecode-ux3::flow-label (nodecode-ux3::lane-flow lane))
        (nodecode-ux3::lane-verdict lane)
        (nodecode-ux3::lane-tier lane)))

(defun ux3-lanes (gate)
  "The lanes GATE carries, each in its review spelling."
  (mapcar #'ux3-lane (getf gate :lanes)))

(defun ux3-gate (prompt &rest pairs)
  "The gate over PROMPT shown as prose, from the cell's own verbs."
  (nodecode-ux3:gate-text (apply #'ux3:evaluate prompt pairs)))

;;; --- route ----------------------------------------------------------------------

(deftest ux3-cell-route-reads-a-prompt-three-ways ()
  (with-ux3 ()
    (is (equal "routine" (getf (ux3:route "Fix typo in the README") :route)) "routine work is routine")
    (is (equal "quick" (getf (ux3:route "Fix typo in the README") :mode)) "a quick gate")
    (is (equal "commitment" (getf (ux3:route "Add a migration for billing") :route)) "a migration commits")
    (is (equal "commitment" (getf (ux3:route "Rename one helper" :files-changed 9) :route)) "past five files commits")
    (is (equal "discovery" (getf (ux3:route "Explore what a board could be") :route)) "nothing recognised is discovery")
    (is (search "routine" (getf (ux3:route "Fix typo in the README") :why)) "and it says why")
    (is (eq t (getf (ux3:route "anything") :review-required)) "a route always wants a review")))

;;; --- evaluate -------------------------------------------------------------------

(deftest ux3-cell-evaluate-reads-three-lanes-and-one-verdict ()
  (with-ux3 ("evidence_tier" "t1")
    (let* ((gate (ux3:evaluate "Ship a dashboard everyone can share"))
           (lanes (ux3-lanes gate)))
      (is (equal '(("user_flow" "continue" "t1") ("evidence_flow" "verify" "t1")
                   ("business_flow" "continue" "t1"))
                 lanes) "t1 is a verify on evidence and nothing else moves")
      (is (equal "verify" (getf gate :combined-verdict)) "one verdict, the worst lane's")
      (is (equal "evidence_flow" (getf gate :weakest-flow)) "and it names that lane")
      (is (search "proof step" (getf gate :next-step)) "with the step the verdict asks for")
      (is (eq t (getf gate :advisory)) "advisory is said, not implied"))
    (let* ((gate (ux3:evaluate "Maybe a fix for the payment page"))
           (lanes (ux3-lanes gate)))
      (is (equal '(("user_flow" "continue" "t0") ("evidence_flow" "stop_reframe" "t0")
                   ("business_flow" "verify" "t0"))
                 lanes) "a hedge reads as t0, and payment is a verify")
      (is (equal "stop_reframe" (getf gate :combined-verdict)) "the stop lane carries the review")
      (is (equal "evidence_flow" (getf gate :weakest-flow)) "the stop lane is the weakest")
      (is (search "better product question" (getf gate :next-step)) "and it reframes")
      (is (search "hedges" (format nil "~{~a~}" (getf gate :notes))) "the notes say what it read"))
    (is (search "t2" (ux3-gate "Ship a dashboard" :evidence-tier "t2")) "an explicit tier wins")
    (is (search "headline tier t3" (ux3-gate "Ship a dashboard" :evidence-tier "t3")) "a lifted tier reads back")))

;;; --- decide ---------------------------------------------------------------------

(deftest ux3-cell-decide-reduces-a-review-to-one-verdict ()
  (with-ux3 ()
    (let ((decision (ux3:decide '((:user "continue" "t3") (:evidence "stop_reframe" "t1")
                                  (:business "continue" "t2")))))
      (is (equal "stop_reframe" (getf decision :combined-verdict)) "one stop outranks two continues")
      (is (equal "evidence_flow" (getf decision :weakest-flow)) "the stop lane is named")
      (is (equal "t1" (getf decision :evidence-tier)) "the headline is the weakest lane's tier")
      (is (null (getf decision :authorized)) "and nothing is authorized"))
    ;; a tie on severity breaks to the lowest tier, then evidence, user, business
    (let ((decision (ux3:decide '((:business "verify" "t3") (:user "verify" "t1")
                                  (:evidence "verify" "t1")))))
      (is (equal "evidence_flow" (getf decision :weakest-flow)) "same severity and tier: evidence first")
      (is (equal "t1" (getf decision :evidence-tier)) "the lowest tier is the headline")
      (is (null (getf decision :problems)) "and a consistent review has none"))
    ;; t0 cannot authorize a continue, and t1 cannot return one
    (let ((decision (ux3:decide '(("user_flow" "continue" "t0") (:evidence "continue" "t2")
                                  (:business "continue" "t3")))))
      (is (equal "continue" (getf decision :combined-verdict)) "all three lanes continue")
      (is (= 1 (length (getf decision :problems))) "but t0 is one problem")
      (is (search "t0" (first (getf decision :problems))) "and it names the tier")
      (is (null (getf decision :authorized)) "so it authorizes nothing"))
    (let ((decision (ux3:decide '((:user "continue" "t1") (:evidence "continue" "t2")
                                  (:business "continue" "t2")))))
      (is (search "t1" (format nil "~{~a~}" (getf decision :problems))) "t1 can never continue"))
    ;; a boundary that contradicts itself, in any case and any spacing
    (let ((decision (ux3:decide '((:user "continue" "t3") (:evidence "continue" "t3")
                                  (:business "continue" "t3"))
                                :may-do '("Edit the settings page" "Run the tests")
                                :must-not-do '("  run the TESTS "))))
      (is (equal "continue" (getf decision :combined-verdict)) "three continues")
      (is (= 1 (length (getf decision :problems))) "one contradiction")
      (is (search "may_do" (first (getf decision :problems))) "said as the boundary states it")
      (is (null (getf decision :authorized)) "and it is not authorized"))
    ;; every bad lane shape is a refusal naming what it read
    (signals-error ux3:ux3-error
      (ux3:decide '((:user "continue" "t2") (:evidence "continue" "t2"))))
    (signals-error ux3:ux3-error
      (ux3:decide '((:user "ship" "t2") (:evidence "continue" "t2") (:business "continue" "t2"))))
    (signals-error ux3:ux3-error
      (ux3:decide '((:user "continue" "t2") (:user "continue" "t2") (:business "continue" "t2"))))
    (is (search "business_flow" (refusal-text ux3:ux3-error
                                (ux3:decide '((:user "continue" "t2") (:evidence "continue" "t2"))))) "a missing lane is named by its review spelling")))

;;; --- the tool, the command and the prose ----------------------------------------

(deftest ux3-cell-tool-command-and-primer-answer ()
  (with-ux3 ("evidence_tier" "t2")
    (let ((answer (ux3-call "ux3_gate" "prompt" "Let teams share one board")))
      (is (stringp answer) "the tool answers text")
      (is (search "UX3 gate" answer) "with the gate's own head")
      (is (search "user_flow" answer) "the user lane")
      (is (search "evidence_flow" answer) "the evidence lane")
      (is (search "business_flow" answer) "the business lane")
      (is (search "combined continue" answer) "t2 with nothing risky continues"))
    (let ((answer (ux3-call "ux3_gate" "prompt" "Disable security for the free tier exploit")))
      (is (search "combined stop_reframe" answer) "a risk shortcut stops the gate"))
    ;; the tool's own declaration refuses a call that leaves the prompt out
    (signals-error nlk:contract-refusal
      (nlk:contract-decode (second (nle::find-tool "ux3_gate"))
                            (nlk:make-json-object "evidence_tier" "t2")))
    (is (search "needs a prompt" (refusal-text ux3:ux3-error (ux3:route ""))) "an empty prompt is refused")
    (let ((answer (cell-entry "nodecode-ux3" "ux3" "Rename one helper function" "s1")))
      (is (search "UX3 gate" answer) "/ux3 runs the same gate")
      (is (search "routine" answer) "and reads the line it was given"))
    (is (search "takes the decision" (cell-entry "nodecode-ux3" "ux3" "" "s1")) "/ux3 with no argument says what it takes")
    (let ((manual (nle:help :ux3)))
      (is (search "stop_reframe" manual) "the primer carries the verdicts")
      (is (search "ux3:decide" manual) "and the verbs"))
    (is (search "write a context pack"
                (ux3:review-text (ux3:decide '((:user "continue" "t3") (:evidence "continue" "t3")
                                               (:business "continue" "t3"))))) "a clean continue review reads as an authorization")))
