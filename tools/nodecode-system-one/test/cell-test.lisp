;;;; cell-test.lisp --- lifecycle, the three gates, the two tools, /system-one.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a service_url without a scheme or a timeout under its floor
;;;; refused), each gate through the seam it is called by -- its URL, its
;;;; headers, its body, and the line it builds -- the fail-open answer when
;;;; the sidecar is down, the gate switches, the bearer token, and the two
;;;; tools as a provider calls them. Every request is a stubbed NLK:HTTP, so
;;;; nothing reaches a sidecar that may not be running.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *system-one-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *system-one-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-system-one-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *SYSTEM-ONE-REQUESTS*
and answered by *SYSTEM-ONE-ANSWER-FN*, so no test dials a sidecar. UIOP:GETENV
answers nothing either, so a token in the machine's environment cannot reach a
test that did not ask for one."
  `(let ((*system-one-requests* '())
         (*system-one-answer-fn* (lambda (method url args)
                                    (declare (ignore method url args))
                                    (error "unexpected Local System One request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *system-one-requests*)
                                (funcall *system-one-answer-fn* method url args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun system-one-asked ()
  "The stubbed requests, oldest first."
  (reverse *system-one-requests*))

(defun system-one-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun system-one-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun system-one-answer (text &optional (status 200))
  "Every request answered with TEXT and STATUS."
  (setf *system-one-answer-fn* (lambda (method url args)
                                  (declare (ignore method url args))
                                  (values text status))))

(defmacro system-one-answer-by-path (&rest pairs)
  "Each request answered by the (SUFFIX TEXT) PAIR whose SUFFIX its url contains."
  `(setf *system-one-answer-fn*
         (lambda (method url args)
           (declare (ignore method args))
           (let ((pair (find-if (lambda (pair) (search (first pair) url)) ',pairs)))
             (if pair (values (second pair) 200) (values "{}" 404))))))

(defun system-one-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- the cell's config, the way the gateway calls it -------------------------

(define-test-slice "system-one" "SYSTEM-ONE-CELL-")

(defun system-one-config (&rest pairs)
  "A whole shared config with one `system-one' section built from PAIRS."
  (nlk:json-object "system-one" (apply #'nlk:make-json-object pairs)))

(defun system-one-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-system-one:start-cell (apply #'system-one-config pairs)))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "system-one"
  (:fixture with-system-one-wire)
  (:help :system-one)
  (:command "system-one")
  (:running (is (nle::find-tool "system_one_route") "system_one_route is registered")
            (is (nle::find-tool "system_one_notify") "system_one_notify is registered")
            (is (equal "http://127.0.0.1:8787" (nodecode-system-one::setting :service-url)) "the section's default is the sidecar's loopback port")
            (is (= 500 (nodecode-system-one::setting :timeout-ms)) "the original's 500 ms ceiling")
            (is (null (nodecode-system-one::setting :auth-token)) "no token: none is sent"))
  (:stopped (is (null (nle::find-tool "system_one_route")) "stop unregisters the tools")
            (is (null (nle::find-tool "system_one_notify")) "both of them"))
  (:refused ("service_url" "sidecar:8787") ("timeout_ms" 10))
  (:idle nodecode-system-one:system-one-error (nodecode-system-one::run-command "" nil)))

(deftest system-one-cell-a-named-but-empty-token-variable-refuses ()
  (with-system-one-wire
    (is (search "auth_token_env names SYSTEM_ONE_TEST_TOKEN"
                (refusal-text nlk:config-refusal
                              (system-one-start "auth_token_env" "SYSTEM_ONE_TEST_TOKEN"))) "the refusal names the variable")
    (is (null (nle::find-tool "system_one_route")) "a refused start installed nothing")))

;;; --- the two pre-turn gates --------------------------------------------------

(deftest system-one-cell-route-asks-both-gates-in-one-call ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start))
      (system-one-answer-by-path
       ("/v1/workflows/search-gate"
        "{\"decision\":\"no_search\",\"decision_source\":\"rule\",\"reason\":\"bounded_transform_task\",\"probability_search\":0.02,\"backend\":\"rule\"}")
       ("/v1/workflows/model-tier-gate"
        "{\"tier\":\"fast\",\"decision_source\":\"rule\",\"reason\":\"bounded_transform\",\"difficulty_score\":1,\"probability_strong\":0.08,\"backend\":\"rule\"}"))
      (multiple-value-bind (text decisions) (system-one:route "rewrite this paragraph"
                                                               :context "user: here it is")
        (is (search "search: no_search (rule, bounded_transform_task; p_web 0.020) - no public web needed" text) "the search decision on its own line")
        (is (search "model tier: fast (rule, bounded_transform; p_strong 0.080) - a fast, cheap tier is enough" text) "and the tier decision")
        (is (search "canary: an audited hard no-Web rule" text) "what Canary would have done")
        (is (search "canary: an audited hard-fast rule" text) "on both rules")
        (is (equal "bounded_transform_task" (getf (getf decisions :search) :reason)) "the decision plist keeps the reason")
        (let ((requests (system-one-asked)))
          (is (= 2 (length requests)) "one POST per gate")
          (is (search "/v1/workflows/search-gate" (second (first requests))) "search first")
          (is (search "/v1/workflows/model-tier-gate" (second (second requests))) "then the tier")
          (let ((body (system-one-request-body (first requests))))
            (is (equal "rewrite this paragraph" (nlk:json-value body :string "task")) "the task as asked")
            (is (equal "user: here it is" (nlk:json-value body :string "context")) "the context beside it"))
          (is (equal "application/json" (system-one-request-header (first requests) "content-type")) "JSON in")
          (is (null (system-one-request-header (first requests) "authorization")) "no token, no Authorization"))))))

(deftest system-one-cell-a-gate-switched-off-is-not-asked ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start "search_gate" nil))
      (system-one-answer "{\"tier\":\"strong\",\"decision_source\":\"rule\",\"reason\":\"high_risk\"}")
      (let ((text (system-one:route "decide the release")))
        (is (null (search "search:" text)) "the search gate was not asked")
        (is (search "model tier: strong" text) "the tier gate was")
        (is (= 1 (length (system-one-asked))) "one POST, not two")))))

(deftest system-one-cell-a-sidecar-that-is-down-is-undecided ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start))
      (setf *system-one-answer-fn* (lambda (method url args)
                                      (declare (ignore method url args))
                                      (error "connection refused")))
      (let ((text (system-one:route "anything")))
        (is (search "search: undecided - connection refused" text) "the search gate says so")
        (is (search "model tier: undecided" text) "and so does the tier gate")
        (is (search "fail-open" text) "nothing is blocked")))))

(deftest system-one-cell-an-answer-with-no-field-is-undecided ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start))
      (system-one-answer "{\"backend\":\"rule\"}")
      (let ((text (system-one:search-gate "review the diff")))
        (is (search "search: undecided - its answer carried no \"decision\" field" text) "a gate that answers the wrong shape fails open too")))))

(deftest system-one-cell-a-token-goes-as-a-bearer-header ()
  (with-system-one-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (if (string= name "SYSTEM_ONE_TEST_TOKEN") "t-1" nil))
      (with-cell-stop ((system-one-start "auth_token_env" "SYSTEM_ONE_TEST_TOKEN"))
        (system-one-answer "{\"decision\":\"no_search\",\"decision_source\":\"rule\",\"reason\":\"local_file_or_repo\"}")
        (system-one:search-gate "review the local diff")
        (is (equal "Bearer t-1" (system-one-request-header (first (system-one-asked)) "authorization")) "the section's token rides the request")))))

(deftest system-one-cell-canary-advice-can-be-turned-off ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start "canary_advice" nil))
      (system-one-answer "{\"decision\":\"no_search\",\"decision_source\":\"rule\",\"reason\":\"local_file_or_repo\"}")
      (let ((text (system-one:search-gate "review the local diff")))
        (is (search "no public web needed" text) "the decision still reads")
        (is (null (search "canary:" text)) "the advice line does not")))))

;;; --- the notification gate -----------------------------------------------------

(deftest system-one-cell-notify-answers-a-delivery-level ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start))
      (system-one-answer "{\"delivery\":\"notify_now\",\"notify_now\":true,\"decision_source\":\"rule\",\"reason\":\"blocking_failure\",\"priority_score\":4.0,\"confidence\":0.9,\"backend\":\"rule\"}")
      (multiple-value-bind (text decision) (system-one:notify-gate "the migration failed"
                                                                   :urgency "high"
                                                                   :blocking-failure t)
        (is (search "notification: notify_now (rule, blocking_failure; priority 4.000) - interrupt the user now" text) "the delivery level and why")
        (is (eq t (getf decision :notify-now)) "notify_now rides the decision")
        (let* ((request (first (system-one-asked)))
               (body (system-one-request-body request)))
          (is (search "/v1/workflows/notification-gate" (second request)) "its own endpoint")
          (is (equal "the migration failed" (nlk:json-value body :string "event")) "the event as asked")
          (is (equal "high" (nlk:json-value body :string "urgency")) "the urgency given")
          (is (eq t (nlk:json-value body :boolean "blocking_failure")) "and the flag")
          (multiple-value-bind (value present)
              (nlk:json-value body :string "deadline_minutes")
            (is (null value) "a deadline nobody gave")
            (is (null present) "is absent, not false")))))))

(deftest system-one-cell-notify-switched-off-says-so ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start "notification_gate" nil))
      (let ((text (system-one:notify-gate "everything is done")))
        (is (search "the gate is off in the system-one section" text) "nothing is asked")
        (is (null (system-one-asked)) "and no request goes out")))))

;;; --- the tools ----------------------------------------------------------------

(deftest system-one-cell-the-route-tool-answers-through-the-wire ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start))
      (system-one-answer "{\"decision\":\"search\",\"decision_source\":\"model\",\"probability_search\":0.91}")
      (let ((text (system-one-tool "system_one_route"
                                    (nlk:json-object "task" "weather in Oslo today"))))
        (is (search "search: search (model; p_web 0.910) - this task needs the public web" text) "the tool answers the model in the same words")
        (is (equal "weather in Oslo today"
                   (nlk:json-value (system-one-request-body (first (system-one-asked))) :string "task")) "and sent the task as given")))))

(deftest system-one-cell-the-notify-tool-answers-through-the-wire ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start))
      (system-one-answer "{\"delivery\":\"digest\",\"notify_now\":false,\"decision_source\":\"model\",\"priority_score\":1.8}")
      (let ((text (system-one-tool "system_one_notify"
                                    (nlk:json-object "event" "nightly backup finished"
                                                     "routine_update" t))))
        (is (search "notification: digest (model; priority 1.800) - add it to a digest, do not interrupt" text) "the tool answers the model")
        (let ((body (system-one-request-body (first (system-one-asked)))))
          (is (eq t (nlk:json-value body :boolean "routine_update")) "the flag it was given")
          (is (equal "auto" (nlk:json-value body :string "urgency")) "and the default urgency"))))))

;;; --- the command ----------------------------------------------------------------

(deftest system-one-cell-the-command-asks-about-a-task-or-says-status ()
  (with-system-one-wire
    (with-cell-stop ((system-one-start))
      (system-one-answer "{\"service\":\"ok\",\"ane\":{\"healthy\":true}}")
      (let ((status (nodecode-system-one::run-command "" nil)))
        (is (search "Local System One at http://127.0.0.1:8787 (a gate times out at 500ms): search on, model tier on, notification on"
                    status) "no argument: where it points and what is on")
        (is (search "health: answers (service ok), neural engine healthy" status) "and its health")
        (is (equal :get (first (first (system-one-asked)))) "a GET"))
      (system-one-answer "{\"decision\":\"no_search\",\"decision_source\":\"rule\",\"reason\":\"connected_app_data\"}")
      (let ((text (nodecode-system-one::run-command "summarize the ticket board" nil)))
        (is (search "no public web needed" text) "with a task: the gates answer about it")
        (is (search "canary: an audited hard no-Web rule" text) "and say what Canary would have done")))))
