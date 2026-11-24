;;;; cell-test.lisp --- lifecycle; the tool through its seam; the policies; the decisions; /agents.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every answer here comes through the seam a model or a shell would use:
;;;; AR-CALL-ROUTING dispatches agent_route exactly as a provider's tool call
;;;; does, and the verbs are the same functions /agents runs. Nothing dials
;;;; anything: the cell has no network path at all, and the registry is data.

(in-package #:nodecode.test)

(define-test-slice "agent-router" "AGENT-ROUTER-CELL-" :start nodecode-agent-router:start-cell)

(defmacro with-agent-router ((&rest pairs) &body body)
  "BODY with the cell started under PAIRS as its `agent-router' section, and
stopped on unwind."
  `(with-cell-stop ((agent-router-start ,@pairs))
     ,@body))

;;; --- asking the tool what a model would ---------------------------------------------

(defun ar-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun ar-call-routing (&rest pairs)
  "agent_route with PAIRS as its JSON body; => the decoded answer object."
  (nlk:decode-json (ar-call-tool "agent_route"
                                  (nlk:encode-json-object
                                   (apply #'nlk:make-json-object pairs)))))

(defun ar-decision (answer)
  (nlk:json-value answer :string "decision"))

(defun ar-selected-id (answer)
  (nlk:json-value (nlk:json-value answer :object "selected") :string "id"))

(defun ar-abstain (answer)
  (nlk:json-value answer :string "abstain_reason"))

(defun ar-reasons (answer)
  (coerce (nlk:json-value answer :array "reasons") 'list))

(defun ar-selected-reasons (answer)
  (coerce (nlk:json-value (nlk:json-value answer :object "selected") :array "reasons") 'list))

(defun ar-rejected (answer)
  "The rejections as `id reason' lines, which is what a reader compares."
  (loop for row across (nlk:json-value answer :array "rejected")
        collect (format nil "~a ~a"
                        (nlk:json-value row :string "id")
                        (nlk:json-value row :string "reason"))))

(defun ar-alternatives (answer)
  (coerce (nlk:json-value answer :array "alternatives") 'list))

(defun ar-rejections-mentioning (answer word)
  (remove-if-not (lambda (line) (search word line)) (ar-rejected answer)))

;;; --- three agents a test can rank ----------------------------------------------------

(defun ar-agent (id description capabilities permissions &rest pairs)
  (apply #'nlk:make-json-object
         "id" id "name" id "description" description
         "capabilities" (coerce capabilities 'vector)
         "permissions" (coerce permissions 'vector)
         pairs))

(defun ar-three (&rest pairs)
  "Three agents: one that codes, one that researches, one that answers tickets."
  (vector (apply #'ar-agent "coding-agent" "Fixes CORS bugs and writes Python."
                 '("coding" "python") '("read_code" "write_code")
                 "cost" 0.4 "latency" 0.4 "reliability" 0.85
                 "fallback_id" "research-agent" pairs)
          (apply #'ar-agent "research-agent" "Searches the documentation and summarises."
                 '("research" "docs") '("read_docs")
                 "cost" 0.2 "latency" 0.3 "reliability" 0.9 pairs)
          (apply #'ar-agent "support-agent" "Answers the customer ticket and the billing questions."
                 '("support" "billing") '("read_tickets" "billing")
                 "cost" 0.3 "latency" 0.2 "reliability" 0.75 pairs)))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "agent-router"
  (:help :agent-router)
  (:command "agents")
  (:running (is (nle::find-tool "agent_route") "the tool is on the model")
            (is (= 15 (nodecode-agent-router::setting :min-confidence)) "the floor is fifteen percent by default"))
  (:stopped (is (null (nle::find-tool "agent_route")) "and off it again"))
  (:refused ("max_alternatives" -1)
            ("min_confidence" -1)
            ("registry" "/nonexistent/agent-router-registry.json"))
  (:idle nodecode-agent-router:agent-router-error
         (nodecode-agent-router:route "fix the CORS bug")
         (nodecode-agent-router:agents-text)))

;;; --- the tool -----------------------------------------------------------------------

(deftest agent-router-cell-the-tool-chooses-and-explains (with-agent-router ())
  (let ((answer (ar-call-routing "task" "fix the CORS bug in the FastAPI API"
                                  "agents" (ar-three))))
    (is (string= "select" (ar-decision answer)) "a task one agent fits is selected")
    (is (string= "coding-agent" (ar-selected-id answer)) "and it is the one whose words reach the task")
    (is (member "best_match" (ar-reasons answer) :test #'string=) "the answer says why")
    (is (some (lambda (reason) (search "relevance=" reason)) (ar-selected-reasons answer)) "and breaks the score down")
    (is (= 2 (length (ar-alternatives answer))) "the runners-up are listed")
    (is (null (ar-rejected answer)) "and nothing that fits was thrown out")))

(deftest agent-router-cell-the-policies-come-before-the-score (with-agent-router ())
  (let ((answer (ar-call-routing "task" "the customer cannot pay this month"
                                  "agents" (ar-three)
                                  "required_permissions" (vector "billing"))))
    (is (string= "support-agent" (ar-selected-id answer)) "a permission the caller requires decides")
    (is (member "coding-agent missing_permissions" (ar-rejected answer) :test #'string=) "and an agent without it is rejected, not merely outscored"))
  (let ((answer (ar-call-routing "task" "audit the code and the logs"
                                  "agents" (ar-three)
                                  "forbidden_permissions" (vector "write_code"))))
    (is (member "coding-agent forbidden_permissions" (ar-rejected answer) :test #'string=) "a permission the caller forbids rejects its holder")
    (is (not (string= "coding-agent" (ar-selected-id answer))) "whatever it would have scored"))
  (let ((answer (ar-call-routing "task" "fix the CORS bug"
                                  "agents" (ar-three "scope" "demo"))))
    (is (string= "abstain" (ar-decision answer)) "an agent of another scope is never chosen")
    (is (string= "no_candidate" (ar-abstain answer)))
    (is (= 3 (length (ar-rejections-mentioning answer "out_of_scope"))) "and every one of them says so"))
  (let ((answer (ar-call-routing "task" "fix the CORS bug"
                                  "agents" (ar-three "enabled" nil))))
    (is (string= "abstain" (ar-decision answer)) "a disabled agent is not a candidate")
    (is (= 3 (length (ar-rejections-mentioning answer "disabled"))))))

(deftest agent-router-cell-it-abstains-under-the-floor (with-agent-router ())
  (let ((answer (ar-call-routing "task" "fix the CORS bug"
                                  "agents" (ar-three)
                                  "min_confidence" 0.99)))
    (is (string= "abstain" (ar-decision answer)) "a floor nothing reaches abstains")
    (is (string= "low_confidence" (ar-abstain answer)))
    (is (some (lambda (reason) (search "low_confidence:" reason)) (ar-reasons answer)) "and names the score it fell short with")
    (is (plusp (length (ar-alternatives answer))) "while still showing what was close"))
  ;; The section's own floor is the default, so the same call without one is a choice.
  (let ((answer (ar-call-routing "task" "fix the CORS bug" "agents" (ar-three))))
    (is (string= "select" (ar-decision answer)))))

(deftest agent-router-cell-a-failure-is-answered-in-one-hop (with-agent-router ())
  (let ((answer (ar-call-routing "task" "fix the CORS bug in the FastAPI API"
                                  "agents" (ar-three)
                                  "failed_agent_id" "coding-agent")))
    (is (string= "fallback" (ar-decision answer)) "a failed agent is answered, not re-picked")
    (is (string= "research-agent" (ar-selected-id answer)) "its own fallback_id goes first when it is a candidate")
    (is (member "coding-agent already_failed" (ar-rejected answer) :test #'string=))
    (is (member "bounded_fallback" (ar-reasons answer) :test #'string=) "one hop, and it says so"))
  (let ((answer (ar-call-routing "task" "fix the CORS bug in the FastAPI API"
                                  "agents" (ar-three)
                                  "failed_agent_id" "coding-agent"
                                  "allow_fallback" nil)))
    (is (string= "abstain" (ar-decision answer)) "without a fallback it says nothing is left")
    (is (string= "fallback_disabled" (ar-abstain answer))))
  (let ((answer (ar-call-routing "task" "fix the CORS bug"
                                  "agents" (ar-three)
                                  "failed_agent_id" "no-such-agent")))
    (is (member "no-such-agent agent_absent" (ar-rejected answer) :test #'string=) "an agent the list never had is named as absent")
    (is (string= "coding-agent" (ar-selected-id answer)) "and the best remaining one answers")))

(deftest agent-router-cell-a-call-it-cannot-read-refuses (with-agent-router ())
  (let ((answer (ar-call-tool "agent_route"
                               (nlk:encode-json-object
                                (nlk:make-json-object "agents" (ar-three))))))
    (is (search "contract_refused" answer) "a call with no task is refused before the body")
    (is (search "task" answer) "naming the key it wanted"))
  (is (signals-error nodecode-agent-router:agent-router-error
                     (nodecode-agent-router:route "fix the bug" :agents (list (list :name "no id")))) "an agent with no id refuses when read"))

;;; --- the same decision from eval, and /agents ----------------------------------------

(deftest agent-router-cell-the-verbs-answer-the-same-decision (with-agent-router ())
  (let ((lines (nodecode-agent-router:route
                "fix the CORS bug in the FastAPI API"
                :agents (list (list :id "coding-agent"
                                    :description "Fixes CORS bugs and writes Python."
                                    :capabilities '("coding" "python")
                                    :permissions '("read_code"))))))
    (is (search "select coding-agent" lines) "an eval form gets the lines /agents prints")
    (is (search "relevance=" lines) "with the reasons"))
  (let ((json (nodecode-agent-router:route-json
               "fix the CORS bug"
               :agents (list (list :id "coding-agent"
                                   :description "Fixes CORS bugs and writes Python.")))))
    (is (search "\"decision\":\"select\"" json) "and the JSON the tool answers"))
  (let ((registry (nodecode-agent-router:agents-text)))
    (is (search "coding-agent" registry) "typed alone, the registry and its examples")
    (is (search "security-agent" registry))))

(deftest agent-router-cell-the-command-ranks-the-registry (with-agent-router ())
  (let ((answer (nodecode-agent-router::run-command "fix the CORS bug in the FastAPI API" nil)))
    (is (search "select coding-agent" answer) "a task after /agents routes it over the registry")
    (is (search "alternatives" answer) "and shows the runners-up"))
  (let ((answer (nodecode-agent-router::run-command "" nil)))
    (is (search "example registry" answer) "typed alone it names the registry it ranks")
    (is (search "devops-agent" answer))))

;;; --- a registry of the operator's own --------------------------------------------------

(defparameter +registry-json+
  "[{\"id\": \"harbour-agent\", \"name\": \"Harbour Agent\",
     \"description\": \"Keeps the harbour berths tidy and answers the boat questions.\",
     \"capabilities\": [\"berths\", \"boats\"], \"permissions\": [\"read_berths\"]},
    {\"id\": \"ledger-agent\", \"name\": \"Ledger Agent\",
     \"description\": \"Reconciles the ledger and the invoices.\",
     \"capabilities\": [\"ledger\", \"invoices\"], \"permissions\": [\"read_ledger\"]}]"
  "A registry file a test writes: two agents of the test's own.")

(defun ar-registry-file ()
  "A fresh file holding the test's registry, in the test's scratch home."
  (let ((path (nlk:home (format nil "ar-registry-~36r.json"
                                (random (expt 36 6) (make-random-state t))))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string +registry-json+ out))
    (namestring path)))

(deftest agent-router-cell-a-registry-file-is-the-operators-own ()
  (let ((file (ar-registry-file)))
    (with-agent-router ("registry" file)
      (let ((answer (nodecode-agent-router::run-command "reconcile the invoices" nil)))
        (is (search "select ledger-agent" answer) "the file's agents are the ones ranked")
        (is (not (search "coding-agent" answer)) "and the examples are gone"))
      (let ((registry (nodecode-agent-router:agents-text)))
        (is (search "harbour-agent" registry) "the registry prints the file's own")
        (is (search (namestring file) registry) "and names the file it read"))
      (let ((answer (ar-call-routing "task" "reconcile the invoices"
                                      "agents" (vector))))
        (is (string= "select" (ar-decision answer)) "an empty agents array ranks the file's own")
        (is (string= "ledger-agent" (ar-selected-id answer)))))))
