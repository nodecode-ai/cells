;;;; cell-test.lisp --- the twelve actions, the plan on disk, /handflow, the
;;;; section every request carries.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing is stubbed and nothing dials: the cell's state is a folder under
;;;; the scratch home this run owns, so a test writes and reads real plan
;;;; files. What is proved is the wiring the loader makes -- the tool, the
;;;; command, the manual, the harness section, and the refusals -- and that a
;;;; plan outlives a stop and a start, as the original's JSON files did.

(in-package #:nodecode.test)

(define-test-slice "handflow" "HANDFLOW-CELL-" :recipe "handflow"
  :start nodecode-handflow:start-cell)

(define-cell-lifecycle-tests "handflow"
  (:hooks 'nle::read-harness-sections)
  (:help :handflow)
  (:command "handflow")
  (:refused ("state_dir" 42))
  (:idle nodecode-handflow:handflow-error
         (handflow:plan-text)
         (handflow:plans-text)))

;;; --- helpers ------------------------------------------------------------------

(defmacro with-fresh-plans (&body body)
  "BODY over an empty plan folder, so one test never reads another's plan."
  `(let ((dir (nlk:home "handflow/")))
     (when (uiop:directory-exists-p dir)
       (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))
     ,@body))

(defun handflow-call (&rest pairs)
  "The handflow tool as the registry dispatches it: PAIRS alternating key and
value, decoded through the tool's own contract, then run."
  (let ((entry (nle::find-tool "handflow")))
    (unless entry (error "the handflow tool is not registered"))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun handflow-command-text (&optional (argument ""))
  "/handflow ARGUMENT, run the way a shell with no model runs it."
  (let ((entry (nle::find-registered-command "handflow")))
    (unless entry (error "/handflow is not registered"))
    (funcall (nle::slash-command-headless entry) argument "s1")))

(defun handflow-sections ()
  "The harness sections a request would carry, as the advised chain answers
them for a session with one section already on it."
  (funcall (cell-hook "nodecode-handflow" 'nle::read-harness-sections)
           (lambda (session-id)
             (declare (ignore session-id))
             (list (cons "index" "a ranked index")))
           "s1"))

;;; --- the tool ------------------------------------------------------------------

(deftest handflow-cell-a-plan-starts-and-marks-its-steps-through-the-contract
  (with-fresh-plans
    (with-cell-stop ((handflow-start))
      (let ((text (handflow-call "action" "start" "goal" "ship the parser"
                                 "steps" (vector "read the old one" "port it" "prove it"))))
        (is (search "handflow start" text) "the answer names the act")
        (is (search "ship the parser" text) "and carries the goal back")
        (is (search "[>] read the old one" text) "the first step is in progress")
        (is (search "0/3 complete" text) "with the tally"))
      (let ((text (handflow-call "action" "complete" "note" "ported, tests pass")))
        (is (search "1/3 complete" text) "the current step completes")
        (is (search "note: ported, tests pass" text) "and keeps its note"))
      (let ((text (handflow-call "action" "begin" "step" "2")))
        (is (search "[>] port it" text) "a step can be begun by its number"))
      (let ((text (handflow-call "action" "begin" "step" "3")))
        (is (search "[x] port it" text) "beginning s03 auto-completes the step left in progress")
        (is (search "[>] prove it" text) "and opens s03")))))

(deftest handflow-cell-the-last-step-closes-the-plan-and-add-reopens-it
  (with-fresh-plans
    (with-cell-stop ((handflow-start))
      (handflow-call "action" "start" "goal" "one job" "steps" (vector "only step"))
      (let ((text (handflow-call "action" "complete" "step" "1")))
        (is (search "plan_" text) "the plan is still there")
        (is (search "(done)" text) "and closed itself when its last step completed"))
      (let ((text (handflow-call "action" "add" "title" "a second thought")))
        (is (search "(active)" text) "adding a step reopens the plan")
        (is (search "[x] only step" text) "the done step stays done")))))

(deftest handflow-cell-a-refusal-says-which-and-is-marked-failed
  (with-fresh-plans
    (with-cell-stop ((handflow-start))
      (is (search "no active plan" (handflow-call "action" "complete" "step" "1")) "a move with no plan says so")
      (handflow-call "action" "start" "goal" "a job" "steps" (vector "alpha" "beta"))
      (let ((text (handflow-call "action" "complete" "step" "9")))
        (is (search "ERROR: " text) "an unknown step is a failed call")
        (is (search "no step matches" text) "naming what it could not match"))
      (is (search "still open" (handflow-call "action" "finish")) "finish refuses while a step is open")
      (is (search "(done)" (handflow-call "action" "finish" "force" t)) "force closes it anyway"))))

(deftest handflow-cell-a-call-that-names-no-action-is-refused-by-the-contract
  (with-fresh-plans
    (with-cell-stop ((handflow-start))
      (let ((schema (second (nle::find-tool "handflow"))))
        (is (hash-table-p (nlk:contract-schema schema)) "the tool is registered against a declared contract")
        (is (signals-error nlk:contract-refusal
              (nlk:contract-decode schema (nlk:make-json-object "goal" "no action here"))) "a call without an action is refused before the body runs")
        (is (signals-error nlk:contract-refusal
              (nlk:contract-decode schema (nlk:make-json-object "action" "levitate"))) "and so is an action outside the vocabulary")))))

(deftest handflow-cell-plans-and-their-state-outlive-a-stop-and-a-start
  (with-fresh-plans
    (let ((stop (handflow-start)))
      (handflow-call "action" "start" "goal" "outlive the restart"
                      "steps" (vector "write it" "read it back"))
      (handflow-call "action" "complete" "note" "written")
      (funcall stop))
    (with-cell-stop ((handflow-start))
      (let ((text (handflow:plan-text)))
        (is (search "outlive the restart" text) "the plan is read off disk by the next start")
        (is (search "note: written" text) "with the note it was given")
        (is (search "1/2 complete" text) "and the progress it reached"))
      (let ((text (handflow:plans-text)))
        (is (search "handflow list" text) "the recent plans list answers")
        (is (search "(active)" text) "marking the one in hand")))))

;;; --- the plan in every request, and /handflow --------------------------------

(deftest handflow-cell-the-plan-rides-every-request
  (with-fresh-plans
    (with-cell-stop ((handflow-start))
      (is (functionp (cell-hook "nodecode-handflow" 'nle::read-harness-sections)) "the cell advises the harness sections")
      (is (equal (list (cons "index" "a ranked index")) (handflow-sections)) "with no plan the sections come back exactly as they went in")
      (handflow-call "action" "start" "goal" "ride every request"
                      "steps" (vector "plan it" "work it"))
      (let ((sections (handflow-sections)))
        (is (equal "handflow" (car (first sections))) "the plan's section comes first")
        (is (search "ride every request" (cdr (first sections))) "carrying the goal")
        (is (search "0/2 complete" (cdr (first sections))) "and the tally")
        (is (equal "index" (car (second sections))) "with the section it found kept behind it")))))

(deftest handflow-cell-slash-shows-the-plan-and-lists-the-recent-ones
  (with-fresh-plans
    (with-cell-stop ((handflow-start))
      (is (search "no plan yet" (handflow-command-text)) "with no plan, /handflow says so")
      (handflow-call "action" "start" "goal" "show it to the person"
                      "steps" (vector "write the plan"))
      (let ((text (handflow-command-text)))
        (is (search "[>] write the plan" text) "otherwise it shows the plan")
        (is (search "0/1 complete" text) "with its tally"))
      (is (search "handflow list" (handflow-command-text "list")) "and list, the recent plans")
      (is (search "handflow:plan-text" (handflow-command-text "help")) "and help, the manual line"))))

(deftest handflow-cell-show-plan-off-keeps-the-plan-out-of-the-request
  (with-fresh-plans
    (with-cell-stop ((handflow-start "show_plan" nil))
      (handflow-call "action" "start" "goal" "kept quiet" "steps" (vector "say nothing"))
      (is (null (assoc "handflow" (handflow-sections) :test #'equal)) "show_plan off leaves the request as it was"))))
