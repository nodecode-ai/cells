;;;; cell-test.lisp --- the effort router against a stubbed decision endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every decision is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches OpenRouter, the environment or the
;;;; operator's files. The tools are called the way a model's call reaches
;;;; them -- through the real dispatch and contract -- so the schema the
;;;; provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "effort-router" "EFFORT-ROUTER-CELL-"
  :start nodecode-effort-router:start-cell
  :defaults (("api_key_env" "OPENROUTER_API_KEY")))

;;; --- fixtures ------------------------------------------------------------------

(defvar *asked* '()
  "Every stubbed request of the test, (METHOD URL . OPTIONS), newest first.")

(defvar *answer* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, OPTIONS =>
(values TEXT STATUS).")

(defmacro with-router-wire (&body body)
  "BODY with the decision endpoint stubbed and one key in the environment."
  `(let ((*asked* '())
         (*answer* (lambda (method url options)
                     (declare (ignore method url options))
                     (error "unexpected effort-router request"))))
     (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENROUTER_API_KEY") "test-key"))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list* method url options) *asked*)
                                   (funcall *answer* method url options))
         ,@body))))

(defmacro with-routed-provider (&body body)
  "BODY with the ambient provider the one this cell routes — the provider a
context build would freeze, so only a test that builds no context binds it."
  `(let ((nle::*provider* "ollama-cloud")) ,@body))

(defun answer-with (text &optional (status 200))
  "Make the stubbed endpoint answer TEXT with STATUS."
  (setf *answer* (lambda (method url options)
                    (declare (ignore method url options))
                    (values text status))))

(defun router-asked ()
  "The stubbed requests, oldest first."
  (reverse *asked*))

(defun router-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defparameter +decision+
  (concatenate
   'string
   "{\"answers\":{\"model_route\":{\"type\":\"choice\",\"choice\":\"glm-5.3\","
   "\"confidence\":0.87,\"probabilities\":{\"glm-5.3\":0.87,\"kimi-k3\":0.09}},"
   "\"reasoning_effort\":{\"type\":\"choice\",\"choice\":\"high\",\"confidence\":0.81}}}")
  "A confident route: the reasoning model, at high effort.")

(defparameter +uncertain+
  (concatenate
   'string
   "{\"answers\":{\"model_route\":{\"type\":\"choice\",\"choice\":\"kimi-k3\","
   "\"confidence\":0.22},\"reasoning_effort\":{\"type\":\"choice\",\"choice\":\"medium\","
   "\"confidence\":0.31}}}")
  "A route under the confidence floor: the turn keeps what it had.")

(defparameter +off-grid+
  "{\"answers\":{\"model_route\":{\"type\":\"choice\",\"choice\":\"gpt-9\",\"confidence\":0.9},\"reasoning_effort\":{\"type\":\"choice\",\"choice\":\"high\",\"confidence\":0.9}}}"
  "A choice this cell never offered.")

(defun router-message (&optional (text "refactor the parser"))
  "One wire-shaped user message, as a request carries it."
  (nlk:json-object "role" "user" "content" text))

(defun router-tool (name arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call "call-effort-1" name arguments)))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "effort-router"
  (:fixture with-router-wire)
  (:hooks 'nle::snapshot-context-request)
  (:help :effort-router)
  (:command "effort-router")
  (:running (is (nle::find-tool "effort_route") "effort_route is registered")
            (is (nle::find-tool "effort_status") "effort_status is registered")
            (is (equal "ollama-cloud" (nodecode-effort-router::routed-provider))
                "the default provider routed")
            (is (= 6 (length (nodecode-effort-router::grid))) "the measured grid, six deep")
            (is (= 1/2 (nodecode-effort-router::confidence-floor)) "the default floor"))
  (:stopped (is (null (nle::find-tool "effort_route")) "stop unregisters the tools")
            (is (null nodecode-effort-router::*last*) "and forgets the boot's decisions"))
  (:refused ("api_key_env" "NO_SUCH_VARIABLE_FOR_EFFORT_ROUTER")))

(deftest effort-router-cell-absent-key-refuses-with-the-variable-named ()
  (with-router-wire
    (let ((condition (signals-error nlk:config-refusal
                        (effort-router-start "api_key_env" "NO_SUCH_VARIABLE_FOR_EFFORT_ROUTER"))))
      (is (search "NO_SUCH_VARIABLE_FOR_EFFORT_ROUTER" (princ-to-string condition))
          "the refusal names the variable it looked in")
      (is (null (nle::find-tool "effort_route")) "a refused start installed nothing"))))

;;; --- the tools ------------------------------------------------------------------

(deftest effort-router-cell-route-tool-asks-the-endpoint ()
  (with-router-wire
    (with-cell-stop ((effort-router-start))
      (answer-with +decision+)
      (let ((answer (router-tool "effort_route" "{\"text\": \"prove this lemma about groups\"}")))
        (is (search "glm-5.3" answer) "the answer names the chosen model")
        (is (search "high" answer) "and the chosen effort"))
      (let ((request (first (router-asked))))
        (is (= 1 (length (router-asked))) "one decision, one request")
        (is (eq :post (first request)))
        (is (equal "https://openrouter.ai/api/alpha/decisions" (second request))
            "the Decisions API, not the chat endpoint")
        (is (equal "Bearer test-key" (cdr (assoc "authorization" (getf (cddr request) :headers)
                                                  :test #'string-equal))))
        (let ((body (router-body request)))
          (is (equal "typesafe/jev-1.13" (nlk:json-value body :string "model"))
              "the decision model is pinned")
          (is (equal "prove this lemma about groups"
                     (nlk:json-value (nlk:json-value body :object "state") :string "user_message")))
          (let* ((questions (nlk:json-value body :object "questions"))
                 (criteria (nlk:json-value (nlk:json-value questions :object "model_route")
                                            :object "criteria")))
            (is (= 6 (hash-table-count criteria)) "one criterion per grid entry")
            (is (search "deepseek-v4.1-flash" (gethash "1" criteria))
                "the first criterion is the grid's first model")))))))

(deftest effort-router-cell-a-low-confidence-answer-keeps-the-turn ()
  (with-router-wire
    (with-cell-stop ((effort-router-start))
      (answer-with +uncertain+)
      (let ((answer (router-tool "effort_route" "{\"text\": \"what is the capital of Norway\"}")))
        (is (search "no route" answer) "the route is dropped")
        (is (search "confidence" answer) "and the answer says why")))))

(deftest effort-router-cell-a-model-off-the-grid-keeps-the-turn ()
  (with-router-wire
    (with-cell-stop ((effort-router-start))
      (answer-with +off-grid+)
      (is (search "not in the grid"
                  (router-tool "effort_route" "{\"text\": \"hello\"}"))))))

(deftest effort-router-cell-status-names-the-wire-it-uses ()
  (with-router-wire
    (with-cell-stop ((effort-router-start))
      (answer-with +decision+)
      (let ((status (router-tool "effort_status" "{}")))
        (is (search "ollama-cloud" status) "the routed provider")
        (is (search "typesafe/jev-1.13" status) "the decision model")
        (is (search "set" status) "the key is present")
        (is (null (router-asked)) "the status asks the endpoint nothing")))))

;;; --- the turn's route ---------------------------------------------------------------

(deftest effort-router-cell-a-turn-takes-the-route-once ()
  (with-router-wire
    (with-cell-stop ((effort-router-start))
      (with-routed-provider
        (answer-with +decision+)
        (let ((seen '()))
          (flet ((next (&rest arguments)
                   (declare (ignore arguments))
                   (push (list nle::*model* nle::*reasoning-effort*) seen)
                   :context))
            ;; two rounds of one turn: the second reads the memo
            (nodecode-effort-router::route-around #'next "s1"
                                                   :prior-context nil
                                                   :messages (list (router-message)))
            (nodecode-effort-router::route-around #'next "s1"
                                                   :prior-context nil
                                                   :messages (list (router-message))))
          (is (= 2 (length seen)) "both rounds of the turn ran")
          (is (every (lambda (round) (equal '("glm-5.3" "high") round)) seen)
              "both on the routed model and effort, and one route was asked for them")
          (is (= 1 (length (router-asked))) "one decision for the turn"))))))

(deftest effort-router-cell-a-failed-decision-leaves-the-request-alone ()
  (with-router-wire
    (with-cell-stop ((effort-router-start))
      (setf *answer* (lambda (method url options)
                       (declare (ignore method url options))
                       (error "connection refused")))
      (with-routed-provider
        (let ((original nle::*model*))
          (is (eq :context
                  (nodecode-effort-router::route-around
                   (lambda (&rest arguments) (declare (ignore arguments))
                     (and (equal original nle::*model*) :context))
                   "s2" :prior-context nil :messages (list (router-message))))
              "the turn goes out on the model it already had")
          (is (search "no route" (nodecode-effort-router::last-decision-text))
              "and the failure is remembered with its reason"))))))

(deftest effort-router-cell-another-provider-is-not-routed ()
  (with-router-wire
    (with-cell-stop ((effort-router-start))
      (answer-with +decision+)
      (let ((nle::*provider* "openai"))
        (is (eq :untouched
                (nodecode-effort-router::route-around
                 (lambda (&rest arguments) (declare (ignore arguments)) :untouched)
                 "s3" :prior-context nil :messages (list (router-message))))
            "a turn on another provider is left alone")
        (is (null (router-asked)) "and asks for no decision")))))
