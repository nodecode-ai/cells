;;;; cell-test.lisp --- lifecycle; the tool; /route; the judge; the catalog file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every answer here comes through the seam a model or a shell would use:
;;;; CALL-ROUTING dispatches model_route exactly as a provider's tool call
;;;; does, and RUN-COMMAND is the same function /route runs. Nothing dials:
;;;; the judge is the stub in support.lisp, and the catalog is data.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "model-router"
  (:help :model-router)
  (:command "route")
  (:running (is (nle::find-tool "model_route") "the tool is on the model"))
  (:stopped (is (null (nle::find-tool "model_route")) "and off it again"))
  (:refused ("max_candidates" 0)
            ("default_model" "gpt-4o")
            ("catalog_file" "/nonexistent/model-router-catalog.json"))
  (:idle nodecode-model-router:model-router-error
         (nodecode-model-router:route "summarise a letter")
         (nodecode-model-router:route-json "summarise a letter")
         (nodecode-model-router:catalog-text)))

(deftest model-router-cell-the-tool-answers-a-decision (with-model-router ())
  (let* ((answer (call-routing "task" "summarise this letter from the customer"))
         (selected (nlk:json-value answer :object "selected")))
    (is (string= "select" (answer-decision answer)) "a task the catalog can serve is selected")
    (is-present (base-url (nlk:json-value selected :string "base_url"))
      "the chosen row names the vendor's address"
      (is (search "https://" base-url)))
    (is (plusp (length (nlk:json-value selected :string "api_key_env"))) "it names its key variable")
    (is (null (gethash "api_key" selected)) "and never the key itself")
    (is (member "prefer=balanced" (selected-reasons answer) :test #'string=) "the reasons explain")
    (is (member "best_match" (answer-reasons answer) :test #'string=) "why it was chosen")
    (is (plusp (length (coerce (nlk:json-value answer :array "alternatives") 'list))))))

(deftest model-router-cell-the-tool-honours-the-constraints (with-model-router ())
  (let ((vision (call-routing "task" "describe this screenshot"
                                "required_capabilities" (vector "vision"))))
    (is (string= "select" (answer-decision vision)) "a capability the catalog has is served")
    (is (find "vision" (coerce (nlk:json-value (nlk:json-value vision :object "selected")
                                                 :array "capabilities")
                                 'list)
               :test #'string=))
    (is (some (lambda (line) (search "missing_capabilities" line)) (answer-rejections vision))))
  (let ((none (call-routing "task" "describe this screenshot"
                             "required_capabilities" (vector "vision" "coding")
                             "max_cost" 0.01)))
    (is (string= "abstain" (answer-decision none)) "a ceiling nothing sits under abstains")
    (is (string= "no_candidate" (answer-selected none))))
  (let ((pinned (call-routing "task" "anything" "prefer" "named")))
    (is (string= "named_without_pin" (answer-selected pinned)) "an unpinned named call abstains")
    (let ((named (call-routing "task" "anything" "prefer" "named" "model" "openai:gpt-4o")))
      (is (string= "openai:gpt-4o" (answer-selected named)) "a pin is taken whatever it scores")
      (is (member "named_pin" (answer-reasons named) :test #'string=)))))

(deftest model-router-cell-the-tool-answers-a-failure-with-one-hop (with-model-router ())
  (let ((answer (call-routing "task" "anything" "failed_model_id" "openai:gpt-4o")))
    (is (string= "fallback" (answer-decision answer)) "a failed row is answered, not re-picked")
    (is (not (string= "openai:gpt-4o" (answer-selected answer))) "not the row that failed")
    (is (member "openai:gpt-4o already_failed" (answer-rejections answer) :test #'string=))
    (is (member "bounded_fallback" (answer-reasons answer) :test #'string=) "one hop, and it says so"))
  (let ((answer (call-routing "task" "anything" "failed_model_id" "openai:gpt-4o"
                                "allow_fallback" nil)))
    (is (string= "fallback_disabled" (answer-selected answer)) "without a fallback it says so")))

(deftest model-router-cell-the-judge-is-asked-only-when-configured (with-model-router ())
  (setf *judge-answer* '(("openai:gpt-4o-mini" . 1.0)))
  (let ((answer (call-routing "task" "a quick note to my team")))
    (is (null *judge-calls*) "with no judge_url nothing is asked")
    (is (notany (lambda (reason) (search "judge=" reason)) (selected-reasons answer))))
  (nodecode-model-router::stop-cell)
  (with-model-router ("judge_url" "https://judge.test/v1/score"
                      "judge_key_env" "MODEL_ROUTER_JUDGE_KEY")
    (setf *judge-answer* '(("openai:gpt-4o-mini" . 1.0)))
    (let* ((answer (call-routing "task" "a quick note to my team"))
           (reasons (selected-reasons answer)))
      (is (= 1 (length *judge-calls*)) "one judge call, and one only")
      (is (equal "https://judge.test/v1/score" (first (first *judge-calls*))))
      (is (string= "openai:gpt-4o-mini" (answer-selected answer)) "its opinion outranks the numbers")
      (is (some (lambda (reason) (search "judge=1.00" reason)) reasons) "and it is reported"))
    (setf *judge-answer* :fail)
    (let ((answer (call-routing "task" "a quick note to my team")))
      (is (string= "select" (answer-decision answer)) "a judge that is down loses its opinion only")
      (is (some (lambda (reason) (search "judge_unavailable:" reason)) (answer-reasons answer))))))

(deftest model-router-cell-a-catalog-file-adds-a-vendor (with-model-router ("catalog_file" (catalogue-file)))
  (is (search "harbour:harbour-big" (nodecode-model-router:catalog-text)) "the vendor is listed")
  (let ((answer (call-routing "task" "anything" "provider" "harbour" "prefer" "best")))
    (is (string= "harbour:harbour-big" (answer-selected answer)) "and routed when its vendor is pinned")
    (is (search "https://models.test/v1"
                (nlk:json-value (nlk:json-value answer :object "selected") :string "base_url")))))

(deftest model-router-cell-the-command-answers-the-catalog-and-a-task (with-model-router ())
  (let ((text (nodecode-model-router::run-command "" nil)))
    (is (search "models in" text) "bare /route prints the catalog")
    (is (search "openrouter:anthropic/claude-sonnet-4" text) "every row of it")
    (is (not (search "(set)" text)) "and, in this keyless run, no key is set"))
  (multiple-value-bind (text panel) (nodecode-model-router::run-command "" nil)
    (is (search "model-router" (getf panel :title)) "titled for the cell")
    (is (plusp (length (getf panel :rows))) "with a row a vendor"))
  (let ((text (nodecode-model-router::run-command "a quick summary of the minutes" nil)))
    (is (search "decision: select" text) "/route TASK decides")
    (is (search "POST https://" text) "and says where to send it"))
  (let ((text (nodecode-model-router::run-command "-- cheapest a quick summary" nil)))
    (is (search "prefer=cheapest" text) "-- STRATEGY routes that way"))
  (let ((text (nodecode-model-router::run-command "-- provider openai" nil)))
    (is (search "openai:gpt-4o" text) "-- provider NAME opens one vendor's rows"))
  (is (search "not nonsense" (nodecode-model-router::run-command "-- nonsense" nil))))

(deftest model-router-cell-the-verbs-answer-the-same-decision (with-model-router ())
  (let ((json (nodecode-model-router:route-json "summarise a letter")))
    (is (string= "select" (nlk:json-value (nlk:decode-json json) :string "decision"))))
  (is (search "decision: select" (nodecode-model-router:route "summarise a letter")))
  (let ((rows (nodecode-model-router::model-catalog nil)))
    (is (string= "openrouter" (getf (first rows) :provider)) "the catalog is data a layer reads")
    (is (string= "OPENROUTER_API_KEY" (getf (first rows) :api-key-env)) "naming its key variable")
    (is (null (getf (first rows) :api-key)) "and never the key")))

(deftest model-router-cell-a-quiet-answer-can-omit-the-rejections ()
  (with-cell-stop ((nodecode-model-router:start-cell
                    (model-router-config "show_rejected" nil)))
    (let ((answer (call-routing "task" "anything")))
      (is (string= "select" (answer-decision answer)))
      (is (null (nlk:json-value answer :array "rejected")) "show_rejected off leaves it out")
      (is (plusp (length (nlk:json-value answer :array "alternatives")))))))
