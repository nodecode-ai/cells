;;;; cell-test.lisp --- the routes, the tool over a stubbed subagent, and the
;;;; command: what reaches the model, what a routed task runs on, and every
;;;; refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials a provider. A routed task goes through
;;;; NODECODE-DELEGATION::*RUN-TASK*, which a test binds to a stub recording
;;;; (route, provider, model, effort, task, seconds): what the cell resolves and
;;;; hands on is exactly what a real subagent would be given.

(in-package #:nodecode.test)

;;; --- the section, as a config holds it ----------------------------------------------

(defun delegate-route (name description provider model &key effort)
  "One route object for the section's `routes' array."
  (apply #'nlk:make-json-object
         (append (list "name" name "description" description "provider" provider "model" model)
                 (when effort (list "reasoning_effort" effort)))))

(defun delegate-two-routes ()
  "The two routes every test that wants routes starts on, as a JSON array."
  (vector (delegate-route "fast" "Cheap and quick: lookups, summaries, simple edits."
                           "openrouter" "google/gemini-3-flash-preview")
          (delegate-route "deep" "Slow and careful: hard debugging and design work."
                           "anthropic" "claude-opus-5" :effort "high")))

(define-test-slice "delegation" "DELEGATION-CELL-" :start nodecode-delegation:start-cell
  :defaults (("backend" "model_map") ("routes" (delegate-two-routes))))

(defun delegate-tool ()
  "The registered `delegate' tool's function, or NIL."
  (third (nle::find-tool "delegate")))

(defun delegation-backend () nodecode-delegation::*backend*)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "delegation"
  (:config "backend" "model_map" "routes" (delegate-two-routes))
  (:help :delegation)
  (:command "delegation")
  (:running
   (is (= 900 (nodecode-delegation::setting :seconds)) "seconds defaults to 900")
   (is (equal '("fast" "deep") (nodecode-delegation::route-names)) "routes in config order")
   (is (functionp (delegate-tool)) "the tool is registered in model_map mode")
   (is (equal "model_map" (delegation-backend))))
  (:refused ("seconds" 5) ("backend" "minimal"))
  (:idle nodecode-delegation::delegation-error (nodecode-delegation::route-names)))

;;; --- the tool ----------------------------------------------------------------------

(deftest delegation-cell-the-tool-runs-the-task-on-the-route-it-picks ()
  (with-cell-stop ((delegation-start "backend" "model_map" "default_route" "fast"
                                     "routes" (delegate-two-routes)))
    (let* ((seen '())
           (nodecode-delegation::*run-task*
             (lambda (route task seconds)
               (push (list (nodecode-delegation::route-name route) task seconds
                           (nodecode-delegation::route-provider route)
                           (nodecode-delegation::route-model route)
                           (nodecode-delegation::route-effort route))
                     seen)
               (format nil "stubbed answer from ~a" (nodecode-delegation::route-name route))))
           (tool (delegate-tool)))
      (is (search "stubbed answer from deep"
                  (funcall tool (nlk:make-json-object "task" "debug the parser" "route" "deep"))) "the answer is the subagent's")
      (is (equal '(("deep" "debug the parser" 900 "anthropic" "claude-opus-5" "high")) seen) "the route resolved to provider, model and effort, and the task went whole")
      ;; no route: the mode's default, when one is set
      (funcall tool (nlk:make-json-object "task" "summarise this"))
      (is (equal "fast" (first (first seen))) "a task that picks none runs on the default when unset"))))

(deftest delegation-cell-only-names-and-lines-reach-the-model ()
  (with-cell-stop ((delegation-start "backend" "model_map" "routes" (delegate-two-routes)))
    (let* ((properties (gethash "properties" (getf (second (nle::find-tool "delegate")) :parameters)))
           (member (gethash "route" properties))
           (description (gethash "description" member)))
      (is (equal '("fast" "deep") (coerce (gethash "enum" member) 'list)) "the enum is the route names, in config order")
      (is (search "fast: Cheap and quick" description) "each name with its line")
      (is (search "deep: Slow and careful" description))
      (is (not (search "openrouter" description)) "no provider reaches the model")
      (is (not (search "claude-opus-5" description)) "no model id reaches the model")
      (is (equal '("task") (coerce (gethash "required" (getf (second (nle::find-tool "delegate")) :parameters))
                                    'list)) "only the task is required"))))

(deftest delegation-cell-a-route-that-is-not-configured-is-refused ()
  (with-cell-stop ((delegation-start "backend" "model_map" "routes" (delegate-two-routes)))
    (is (search "ghost is not a configured route"
                (refusal-text nodecode-delegation::delegation-error
                  (funcall (delegate-tool)
                           (nlk:make-json-object "task" "look at this" "route" "ghost")))) "a name outside the routes names what is configured")))

(deftest delegation-cell-a-route-outside-the-mode-is-refused-not-misrun ()
  (with-cell-stop ((delegation-start "backend" "model_map" "routes" (delegate-two-routes)))
    (let ((nodecode-delegation::*save-mode* (lambda (mode) (declare (ignore mode)) "saved"))
          (nodecode-delegation::*run-task*
            (lambda (&rest arguments) (declare (ignore arguments)) "should never run")))
      (is (search "saved" (nle:slash "/delegation default")))
      (is (equal "default" (delegation-backend)))
      (is (search "/delegation model_map"
                  (refusal-text nodecode-delegation::delegation-error
                    (funcall (delegate-tool)
                             (nlk:make-json-object "task" "look at this" "route" "fast")))) "the refusal names the way back"))))

;;; --- the command -------------------------------------------------------------------

(deftest delegation-cell-the-command-shows-the-mode-and-the-routes ()
  (with-cell-stop ((delegation-start "backend" "model_map" "default_route" "fast"
                                     "routes" (delegate-two-routes)))
    (let ((shown (nle:slash "/delegation")))
      (is (search "Delegation mode: Model routes (delegation.backend: model_map)" shown))
      (is (search "fast  Cheap and quick" shown) "a route's name and its line")
      (is (search "openrouter · google/gemini-3-flash-preview" shown) "and what it runs on")
      (is (search "(default)" shown) "the default is marked"))
    (let ((nodecode-delegation::*save-mode* (lambda (mode) (format nil "saved ~a" mode))))
      (is (search "saved model_map" (nle:slash "/delegation models")) "`models' is a mode word")
      (is (search "restart-cells" (nle:slash "/delegation default")) "says how to apply it"))
    (is (search "is not a delegation mode" (nle:slash "/delegation sideways")))))

;;; --- the section -------------------------------------------------------------------

(deftest delegation-cell-no-routes-means-no-tool-and-a-line-that-says-so ()
  (with-cell-stop ((delegation-start "backend" "model_map" "routes" (vector)))
    (is (null (delegate-tool)) "no route configured, no parameter offered")
    (is (search "(none yet: write them in the delegation section"
                (nle:slash "/delegation")))))

(deftest delegation-cell-a-malformed-route-is-skipped-and-said-so ()
  (with-cell-stop ((delegation-start
                    "backend" "model_map"
                    "routes" (vector (delegate-route "fast" "Cheap." "openrouter" "a-model")
                                     (nlk:make-json-object "name" "no-model" "description" "d"
                                                           "provider" "somewhere")
                                     (delegate-route "fast" "A second fast." "anthropic" "another"))))
    (is (equal '("fast") (nodecode-delegation::route-names)) "the malformed one and the repeated name are skipped")
    (let ((problems (format nil "~{~a~^; ~}" (nodecode-delegation::setting :route-problems))))
      (is (search "no-model needs a model" problems))
      (is (search "fast is listed twice" problems)))))
