;;;; cell-test.lisp --- the routes, the tool over a stubbed subagent, the
;;;; policy, and the command: what reaches the model, what a routed task runs
;;;; on, every refusal, and what an exhausted route is told.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials a provider. A routed task goes through
;;;; NODECODE-ORQUEHELX::*RUN-TASK*, which a test binds to a stub answering the
;;;; OUTCOME a real subagent would have come to: what the cell resolves and
;;;; hands on is exactly what a real subagent would be given.

(in-package #:nodecode.test)

;;; --- the section, as a config holds it ----------------------------------------------

(defun orquehelx-route (name provider model)
  "One route object for the section's `routes' array."
  (nlk:make-json-object "name" name "provider" provider "model" model))

(defun orquehelx-two-routes ()
  "The two routes every test that wants routes starts on, as a JSON array."
  (vector (orquehelx-route "claude" "anthropic" "claude-haiku-4-5")
          (orquehelx-route "codex" "openai-codex" "gpt-5.5")))

(define-test-slice "orquehelx" "ORQUEHELX-CELL-" :start nodecode-orquehelx:start-cell
  :defaults (("routes" (orquehelx-two-routes))))

(defun orquehelx-tool ()
  "The registered `delegate_to' tool's function, or NIL."
  (third (nle::find-tool "delegate_to")))

(defun orquehelx-outcome (status &rest arguments)
  "An OUTCOME as the run-task stub answers one."
  (apply #'nodecode-orquehelx::make-outcome status arguments))

(defun orquehelx-forget-quota ()
  "Nothing recorded as out of quota: the state every test but the quota ones
starts from."
  (clrhash nodecode-orquehelx::*exhaustions*)
  (setf nodecode-orquehelx::*exhausted-turn* nil))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "orquehelx"
  (:config "routes" (orquehelx-two-routes))
  (:help :orquehelx)
  (:command "ohx")
  (:running
   (is (= 900 (nodecode-orquehelx::setting :seconds)) "seconds defaults to 900")
   (is (equal "ask" (nodecode-orquehelx::setting :policy)) "policy defaults to ask")
   (is (equal '("claude" "codex") (nodecode-orquehelx::route-names)) "routes in config order")
   (is (functionp (orquehelx-tool)) "the tool is registered"))
  (:refused ("seconds" 5) ("policy" "sometimes"))
  (:idle nodecode-orquehelx::orquehelx-error (nodecode-orquehelx::route-names)))

;;; --- the tool --------------------------------------------------------------

(deftest orquehelx-cell-the-tool-runs-the-goal-on-the-route-it-picks ()
  (orquehelx-forget-quota)
  (with-cell-stop ((orquehelx-start))
    (let* ((seen '())
           (nodecode-orquehelx::*run-task*
             (lambda (route goal context seconds)
               (push (list (nodecode-orquehelx::route-name route)
                           (nodecode-orquehelx::route-provider route)
                           (nodecode-orquehelx::route-model route)
                           goal context seconds)
                     seen)
               (orquehelx-outcome :completed :summary "three callers: src/parse.lisp:88"
                                              :session "delegate-1")))
           (tool (orquehelx-tool)))
      (let ((answer (funcall tool (nlk:make-json-object "route" "codex" "goal" "find every caller of parse_header"
                                                        "context" "src/parse.lisp"))))
        (is (search "\"status\":\"completed\"" answer) "the answer carries the status")
        (is (search "three callers" answer) "and the child's last words")
        (is (search "\"route\":\"codex\"" answer)))
      (is (equal '(("codex" "openai-codex" "gpt-5.5" "find every caller of parse_header"
                    "src/parse.lisp" 900))
                 seen) "the route resolved to provider and model, and the goal went whole"))))

(deftest orquehelx-cell-only-the-route-names-and-models-reach-the-model ()
  (with-cell-stop ((orquehelx-start))
    (let* ((parameters (getf (second (nle::find-tool "delegate_to")) :parameters))
           (route (gethash "route" (gethash "properties" parameters)))
           (description (getf (second (nle::find-tool "delegate_to")) :description)))
      (is (equal '("claude" "codex") (coerce (gethash "enum" route) 'list)) "the enum is the route names")
      (is (search "claude (claude-haiku-4-5)" (gethash "description" route)) "each name with its model")
      (is (search "claude, codex" description) "the tool says which routes it has")
      (is (equal '("route" "goal") (coerce (gethash "required" parameters) 'list)) "route and goal are required"))))

(deftest orquehelx-cell-a-route-that-is-not-configured-is-refused ()
  (with-cell-stop ((orquehelx-start))
    (let ((nodecode-orquehelx::*run-task*
            (lambda (&rest arguments) (declare (ignore arguments))
              (orquehelx-outcome :completed :summary "should never run"))))
      (is (search "unknown route ghost; the routes are claude, codex"
                  (refusal-text nodecode-orquehelx::orquehelx-error
                    (funcall (orquehelx-tool)
                             (nlk:make-json-object "route" "ghost" "goal" "look at this")))) "a name outside the routes names what is configured")
      (is (search "missing 'goal'"
                  (refusal-text nodecode-orquehelx::orquehelx-error
                    (funcall (orquehelx-tool)
                             (nlk:make-json-object "route" "claude" "goal" "  ")))) "a task with no words is refused"))))

(deftest orquehelx-cell-no-routes-is-said-and-no-route-is-offered ()
  (with-cell-stop ((orquehelx-start "routes" (vector)))
    (is (search "no routes configured: write them in the orquehelx section"
                (refusal-text nodecode-orquehelx::orquehelx-error
                  (funcall (orquehelx-tool) (nlk:make-json-object "route" "claude" "goal" "x")))))
    (let* ((parameters (getf (second (nle::find-tool "delegate_to")) :parameters))
           (route (gethash "route" (gethash "properties" parameters))))
      (is (null (gethash "enum" route)) "an empty enum is not offered"))))

;;; --- the policy ------------------------------------------------------------

(deftest orquehelx-cell-a-route-out-of-quota-is-told-with-the-ask-instruction ()
  (orquehelx-forget-quota)
  (with-cell-stop ((orquehelx-start))
    (let ((nodecode-orquehelx::*run-task*
            (lambda (&rest arguments) (declare (ignore arguments))
              (orquehelx-outcome :failed :error "429 Too Many Requests: usage limit reached"))))
      (let ((answer (funcall (orquehelx-tool)
                              (nlk:make-json-object "route" "claude" "goal" "read the diff"))))
        (is (search "\"status\":\"out_of_quota\"" answer))
        (is (search "reset time unknown" answer) "a reset the provider did not name is never estimated")
        (is (search "\"other_routes\":[\"codex\"]" answer) "the other routes are named")
        (is (search "Do not retry or switch routes on your own" answer) "the ask policy's instruction")
        (is (search "\"provider\":\"anthropic\"" answer) "the route's own provider")))))

(deftest orquehelx-cell-the-policy-holds-the-turn-until-the-user-answers ()
  (orquehelx-forget-quota)
  (with-cell-stop ((orquehelx-start))
    (let* ((nodecode-orquehelx::*current-turn* (lambda () "t1"))
           (nodecode-orquehelx::*run-task*
             (lambda (&rest arguments) (declare (ignore arguments))
               (orquehelx-outcome :failed :error "429 Too Many Requests: usage limit reached")))
           (tool (orquehelx-tool)))
      (funcall tool (nlk:make-json-object "route" "claude" "goal" "read the diff"))
      (is (search "a route ran out of quota in this turn"
                  (refusal-text nodecode-orquehelx::orquehelx-error
                    (funcall tool (nlk:make-json-object "route" "codex" "goal" "try the other one")))) "no second route in the same turn under ask")
      (is (search "policy: ask" (nle:slash "/ohx quota")) "and the command says which policy that is")
      (setf nodecode-orquehelx::*exhausted-turn* nil)
      (is (search "\"status\":\"out_of_quota\""
                  (funcall tool (nlk:make-json-object "route" "codex" "goal" "try the other one"))) "the user's answer opens a new turn, where another route may go"))))

(deftest orquehelx-cell-parent-decides-may-pick-another-route-at-once ()
  (orquehelx-forget-quota)
  (with-cell-stop ((orquehelx-start "policy" "parent_decides"))
    (let* ((nodecode-orquehelx::*current-turn* (lambda () "t1"))
           (calls 0)
           (nodecode-orquehelx::*run-task*
             (lambda (&rest arguments) (declare (ignore arguments))
               (incf calls)
               (orquehelx-outcome :failed :error "429 Too Many Requests: usage limit reached")))
           (tool (orquehelx-tool)))
      (let ((answer (funcall tool (nlk:make-json-object "route" "claude" "goal" "read the diff"))))
        (is (search "may resend the task to another route" answer) "the parent may choose"))
      (funcall tool (nlk:make-json-object "route" "codex" "goal" "read the diff"))
      (is (= 2 calls) "a second route runs in the same turn under parent_decides"))))

(deftest orquehelx-cell-a-failure-that-is-not-quota-is-not-called-quota ()
  (orquehelx-forget-quota)
  (with-cell-stop ((orquehelx-start))
    (let ((nodecode-orquehelx::*run-task*
            (lambda (&rest arguments) (declare (ignore arguments))
              (orquehelx-outcome :failed :error "connection reset by peer"))))
      (let ((answer (funcall (orquehelx-tool)
                              (nlk:make-json-object "route" "claude" "goal" "read the diff"))))
        (is (search "\"status\":\"failed\"" answer))
        (is (null (search "out_of_quota" answer)) "a plain failure is not quota")
        (is (null (nodecode-orquehelx::exhaustion-for "anthropic")) "and nothing is recorded")))))

;;; --- /ohx ------------------------------------------------------------------

(deftest orquehelx-cell-the-command-lists-the-routes-and-the-quota-it-knows ()
  (orquehelx-forget-quota)
  (with-cell-stop ((orquehelx-start))
    (let ((routes (nle:slash "/ohx routes")))
      (is (search "claude: anthropic (claude-haiku-4-5)" routes))
      (is (search "codex: openai-codex (gpt-5.5)" routes)))
    (let ((quota (nle:slash "/ohx quota")))
      (is (search "claude: no data" quota) "nothing measured, nothing invented")
      (is (search "policy: ask" quota)))
    (nodecode-orquehelx::record-exhaustion "anthropic" "429 usage limit")
    (let ((quota (nle:slash "/ohx quota")))
      (is (search "claude: out of quota since" quota))
      (is (search "reset: unknown" quota)))
    (is (search "usage: /ohx routes | /ohx quota" (nle:slash "/ohx")))
    (is (search "is not a /ohx word" (nle:slash "/ohx backwards")))))

(deftest orquehelx-cell-a-malformed-route-is-skipped-and-said-so ()
  (with-cell-stop ((orquehelx-start
                    "routes" (vector (orquehelx-route "claude" "anthropic" "claude-haiku-4-5")
                                     (nlk:make-json-object "name" "no-model" "provider" "somewhere")
                                     (nlk:make-json-object "name" "Mistyped" "provider" "a" "model" "b")
                                     (orquehelx-route "claude" "openai-codex" "gpt-5.5"))))
    (is (equal '("claude") (nodecode-orquehelx::route-names)) "the malformed and the repeated are skipped")
    (let ((problems (format nil "~{~a~^; ~}" (nodecode-orquehelx::setting :route-problems))))
      (is (search "route no-model needs a model" problems))
      (is (search "routes[2] needs a name" problems))
      (is (search "route claude is listed twice" problems)))))
