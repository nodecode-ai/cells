;;;; cell-test.lisp --- the lanes, a route's first turn, its answer, the refusals, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NLE:SUBMIT is stubbed, so a route's first turn is logged instead of run:
;;;; nothing here dials a provider or needs a key. What is proved is the cell's
;;;; own wiring -- the section and its two lanes, the model a purpose pins on
;;;; the job's own session, the standing instruction and the task its turn
;;;; carries, what a result says at each of the four states, each refusal, both
;;;; tools through the seam the model calls them by, and the operator's command
;;;; -- over real sessions in a real scratch store.

(in-package #:nodecode.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +heavy+ "prov-test/heavy-model"
    "A heavy route that reads like a real one and reaches nothing.")
  (defparameter +light+ "prov-test/light-model"
    "A light route, likewise."))

(defvar *submits* '()
  "Every stubbed submit of the test, (SESSION PROMPT COMMAND-ID SOURCE), newest first.")

(defmacro with-route-submits (&body body)
  "BODY with NLE:SUBMIT stubbed: a route's first turn is logged in *SUBMITS*
and admitted, never run."
  `(let ((*submits* '()))
     (with-stubbed-fdefinition (nle:submit (session prompt &key command-id source &allow-other-keys)
                                 (push (list session prompt command-id source) *submits*)
                                 (nlk::make-active-input-admission :disposition :started))
       ,@body)))

(defun route-submits ()
  "The stubbed submits, oldest first."
  (reverse *submits*))

(defmacro with-router ((&rest config) &body body)
  "BODY in a scratch store with a session \"origin\", the cell started on the
two test lanes -- or on CONFIG -- and the verbs run as origin's evaluation."
  `(with-temp-store ()
     (ensure-durable-session "origin")
     (with-cell-stop ((coding-route-start ,@(or config (list "heavy" "prov-test/heavy-model"
                                                             "light" "prov-test/light-model"))))
       (let ((nlk:*scribe-session-id* "origin"))
         ,@body))))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "coding-route" "CODING-ROUTE-CELL-" :start nodecode-coding-route:start-cell)

(define-cell-lifecycle-tests "coding-route"
  (:fixture with-route-submits)
  (:config "heavy" "prov-test/heavy-model" "light" "prov-test/light-model")
  (:help :coding-route)
  (:command "coding-route")
  (:running
   (is (nle::find-tool "coding_route") "coding_route is registered")
   (is (nle::find-tool "coding_route_result") "and the result reader")
   (is (= 2 (count-if (lambda (entry) (eql 0 (search "coding_route" (first entry)))) nle::*tools*)) "two of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "coding_route")) "and stop takes both back"))
  (:refused ("timeout_seconds" 0))
  (:idle coding-route:coding-route-error (coding-route:route "do it")))

;;; --- the lanes -----------------------------------------------------------------

(deftest coding-route-cell-a-plan-routes-heavy-and-the-work-routes-light (with-route-submits)
  (with-router ()
    (is-carrying (said (coding-route:route "rewrite the parser" :purpose "plan"))
                 ("route-1: plan on heavy (prov-test/heavy-model)") ("reads its answer")
                 (:absent "effort" "no effort member, so the model's own"))
    (multiple-value-bind (provider model) (nlk:session-model-selection "route-1")
      (is (equal "prov-test" provider) "the job's own session runs on the heavy lane's provider")
      (is (equal "heavy-model" model) "and on its model"))
    (let ((asked (first (route-submits))))
      (is (equal "route-1" (first asked)) "the first turn went to the job's own session")
      (is (search "rewrite the parser" (second asked)) "carrying the task whole")
      (is (search "Plan only" (second asked)) "and the standing instruction the purpose names")
      (is (equal "coding-route:route-1" (third asked)) "under a command id of its own")
      (is (equal "coding-route" (fourth asked)) "as its own source"))
    (is (equal (fixture-workspace) (nlk:find-session-cwd "route-1")) "the job works in the caller's folder")
    (is-carrying (said (coding-route:route "rename foo to bar" :purpose "mechanical"))
                 ("route-2: mechanical on light (prov-test/light-model)"))
    (is (equal "light-model" (nth-value 1 (nlk:session-model-selection "route-2"))) "the second route runs on the light lane")))

(deftest coding-route-cell-the-effort-member-reaches-the-job-s-own-selection (with-route-submits)
  (with-router ("heavy" "prov-test/heavy-model" "light" "prov-test/light-model" "heavy_effort" "high")
    (is-carrying (said (coding-route:route "review the change" :purpose "independent-review"))
                 ("route-1: independent-review on heavy (prov-test/heavy-model, effort high)"))
    (is (equal "high" (nlk:session-model-effort "route-1")) "the lane's effort is the job's own")
    (is (search "Independent review" (second (first (route-submits)))) "the review purpose's instruction reached the job")))

(deftest coding-route-cell-a-folder-the-caller-names-is-where-the-job-works (with-route-submits)
  (with-router ()
    (let ((folder (fixture-workspace)))
      (is-carrying (said (coding-route:route "count the tests" :purpose "implementation" :cwd folder))
                   ("route-1: implementation on light (prov-test/light-model)"))
      (is (equal folder (nlk:find-session-cwd "route-1")) "the job runs in the folder the call named"))))

;;; --- the answer -----------------------------------------------------------------

(deftest coding-route-cell-a-result-tells-where-the-route-stands (with-route-submits)
  (with-router ()
    (coding-route:route "do the thing" :purpose "implementation")
    (is-carrying (waiting (coding-route:result "route-1")) ("route-1 has not started its turn yet"))
    (with-stubbed-fdefinition (nlk:active-turn-p (session) (equal "route-1" session))
      (is-carrying (working (coding-route:result "route-1")) ("route-1 is working")))
    (with-stubbed-fdefinition (nlk:turn-answer (session &optional turn)
                                 (declare (ignore turn))
                                 (and (equal "route-1" session) "the parser is rewritten, tests green"))
      (is-carrying (answered (coding-route:result "route-1"))
                   ("route-1 answered") ("the parser is rewritten, tests green")
                   (:absent "has not started" "an answer is not a wait")))))

;;; --- the tools the model calls ------------------------------------------------------

(deftest coding-route-cell-coding-route-answers-through-its-own-tool (with-route-submits)
  (with-router ()
    (is-carrying (routed (nle::message-content
                           (execute-wire-call "route-call" "coding_route"
                                              "{\"task\": \"fix the failing build\", \"purpose\": \"plan\"}")))
                 ("route-1: plan on heavy (prov-test/heavy-model)") ("reads its answer"))
    (is (search "fix the failing build" (second (first (route-submits)))) "the tool's task reached the job")
    (is-carrying (late (nle::message-content
                        (execute-wire-call "result-call" "coding_route_result" "{\"route\": \"route-1\"}")))
                 ("route-1 has not started its turn yet"))))

(deftest coding-route-cell-a-tool-call-with-no-task-refuses-before-anything-starts (with-route-submits)
  (with-router ()
    (is-carrying (taskless (nle::message-content
                            (execute-wire-call "route-call" "coding_route" "{\"purpose\": \"plan\"}")))
                 ("a route needs a task"))
    (is (null (route-submits)) "nothing was started")))

;;; --- the refusals --------------------------------------------------------------------

(deftest coding-route-cell-a-missing-model-refuses-and-names-its-member (with-route-submits)
  (with-router ("light" "prov-test/light-model")
    (is-carrying (refusal (refusal-text coding-route:coding-route-error
                                         (coding-route:route "plan it" :purpose "plan")))
                 ("no heavy model is set") ("coding-route section" "it names where to put it"))
    (is (null (nlk:session-exists-p "route-1")) "no job was made")))

(deftest coding-route-cell-an-unknown-purpose-refuses-and-names-the-six (with-route-submits)
  (with-router ()
    (is-carrying (refusal (refusal-text coding-route:coding-route-error
                                         (coding-route:route "do it" :purpose "vibes")))
                 ("is no purpose") ("independent-review" "the six are named"))
    (is-carrying (elsewhere (refusal-text coding-route:coding-route-error (coding-route:result "job-1")))
                 ("is no route id") ("route-3" "the shape is named"))))

;;; --- the operator's command ------------------------------------------------------------

(deftest coding-route-cell-the-command-shows-the-lanes-and-routes-a-task (with-route-submits)
  (with-router ()
    (is-carrying (alone (nodecode-coding-route::run-command "" "origin"))
                 ("coding-route: two lanes") ("heavy: plan, semantic-final, independent-review")
                 ("-> prov-test/heavy-model") ("light: implementation, mechanical, preliminary-review")
                 ("-> prov-test/light-model") ("no route yet"))
    (is-carrying (typed (nodecode-coding-route::run-command "add a --dry-run flag" "origin"))
                 ("route-1: implementation on light (prov-test/light-model)"))
    (is-carrying (again (nodecode-coding-route::run-command "status" "origin"))
                 ("routes so far: route-1"))
    (is (equal (fixture-workspace) (nlk:find-session-cwd "route-1")) "typed at the keyboard, the job works in the shell's folder")))
