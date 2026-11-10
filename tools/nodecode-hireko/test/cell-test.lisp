;;;; cell-test.lisp --- lifecycle, sign-in, the interview tools, /hireko, refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section and its
;;;; refusals, the session file of the home, the tenant page and Cognito read
;;;; for a sign-in, the facade call with its bearer token and tenant
;;;; headers, the poll that turns a job id into a candidate link, and
;;;; /hireko with and without a word. Every request is a stubbed NLK:HTTP,
;;;; so nothing reaches Hireko, Cognito or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *hireko-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *hireko-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-hireko-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *HIREKO-REQUESTS* and
answered by *HIREKO-ANSWER-FN*, so no test dials Hireko. UIOP:GETENV answers
nothing either, so a credential in the machine's environment cannot reach a
test that did not ask for one."
  `(let ((*hireko-requests* '())
         (*hireko-answer-fn* (lambda (method url args)
                                (declare (ignore method url args))
                                (error "unexpected Hireko request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *hireko-requests*)
                                (funcall *hireko-answer-fn* method url args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun hireko-asked ()
  "The stubbed requests, oldest first."
  (reverse *hireko-requests*))

(defun hireko-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun hireko-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun hireko-answer (text &optional (status 200))
  "The one answer every stubbed request gives."
  (setf *hireko-answer-fn* (lambda (method url args)
                              (declare (ignore method url args))
                              (values text status))))

(defun hireko-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun hireko-call-name (request)
  "The MCP tool name a facade REQUEST calls."
  (nlk:json-value (nlk:json-value (hireko-request-body request) :object "params")
                   :string "name"))

(defun hireko-call-arguments (request)
  "A facade REQUEST's tool arguments, decoded."
  (nlk:json-value (hireko-json-value-params request) :object "arguments"))

(defun hireko-json-value-params (request)
  (nlk:json-value (hireko-request-body request) :object "params"))

(defun hireko-session (tenant &key (token "access-1") (seconds 3600))
  "Save a session for TENANT, as a sign-in would."
  (nodecode-hireko::keep-session
   tenant
   (nlk:json-object "tenant" tenant "stage" "beta" "username" "recruiter@acme.test"
                    "access_token" token "id_token" "" "refresh_token" ""
                    "expires_at" (+ (get-universal-time) seconds))))

(defmacro with-forgotten-session ((tenant) &body body)
  "BODY, then TENANT's session deleted: a test leaves the home as it found it."
  `(progn ,@body (nodecode-hireko::forget-session ,tenant)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "hireko" "HIREKO-CELL-" :start nodecode-hireko:start-cell)

(define-cell-lifecycle-tests "hireko"
  (:fixture with-hireko-wire)
  (:help :hireko)
  (:command "hireko")
  (:running (is (nle::find-tool "hireko_auth_status") "the auth status tool is registered")
            (is (nle::find-tool "hireko_create_interview_link") "the interview tool is registered")
            (is (nle::find-tool "hireko_get_interview_link") "the retrieval tool is registered")
            (is (nle::find-tool "hireko_get_search_score") "the score tool is registered")
            (is (nle::find-tool "hireko_health") "the health tool is registered")
            (is (equal "test" (nodecode-hireko::setting :tenant)) "the default tenant")
            (is (equal "beta" (nodecode-hireko::setting :stage)) "the default stage")
            (is (null (nodecode-hireko::setting :username)) "no credential in the environment"))
  (:stopped (is (null (nle::find-tool "hireko_auth_status")) "stop unregisters the tools")
            (is (null (nle::find-tool "hireko_create_interview_link")) "both of them"))
  (:refused ("timeout_seconds" 0) ("tenant" "Not A Tenant") ("stage" "gamma")
            ("facade_url" "hireko.ai/mcp"))
  (:idle nodecode-hireko:hireko-error (nodecode-hireko::run-command "" nil)))

(deftest hireko-cell-a-named-but-empty-variable-refuses ()
  (with-hireko-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                       (hireko-start "tenant" "acme"
                                                     "username_env" "HIREKO_TEST_USER")))
                 ("HIREKO_TEST_USER" "the refusal names the variable")
                 ("HIREKO_USERNAME" "and the variable it falls back to"))
    (is (null (nle::find-tool "hireko_auth_status")) "a refused start installed nothing")))

;;; --- what the home holds ------------------------------------------------------

(deftest hireko-cell-auth-status-reads-the-home-and-asks-nothing ()
  (with-hireko-wire
    (with-cell-stop ((hireko-start))
      (is-carrying (answer (hireko-tool "hireko_auth_status" (nlk:make-json-object)))
                   ("Signed out of tenant \"test\"" "the section's tenant")
                   ("HIREKO_USERNAME" "and how to sign in"))
      (is (null (hireko-asked)) "no request is made to say what the home holds"))))

(deftest hireko-cell-a-saved-session-answers-auth-status ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (hireko-session "test")
      (with-cell-stop ((hireko-start))
        (is-carrying (answer (hireko-tool "hireko_auth_status" (nlk:make-json-object)))
                     ("Authenticated for tenant \"test\"" "the saved session")
                     ("recruiter@acme.test" "its recruiter")
                     ("good for 60 more minutes" "and the time it has left"))
        (is (null (hireko-asked)) "still no request")))))

(deftest hireko-cell-logout-forgets-the-saved-session ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (hireko-session "test")
      (with-cell-stop ((hireko-start))
        (is-carrying (answer (hireko-tool "hireko_logout" (nlk:make-json-object)))
                     ("Signed out of tenant \"test\"" "the tool signs out"))
        (is (null (nodecode-hireko::saved-session "test")) "the file is gone")
        (is-carrying (answer (nodecode-hireko::run-command "" nil))
                     ("signed out of tenant \"test\"" "/hireko agrees"))))))

;;; --- sign-in ------------------------------------------------------------------

(defun hireko-cognito-wire ()
  "The original's sign-in, answered: the tenant page, then Cognito."
  (setf *hireko-answer-fn*
        (lambda (method url args)
          (declare (ignore method args))
          (cond ((search "assets/Json/tenant_configuration.json" url)
                 (values "{\"cognito\":{\"cognito_client_id\":\"client-from-tenant\",\"cognito_user_pool_id\":\"eu-west-1_PoolAbc\"}}"
                         200))
                ((search "cognito-idp.eu-west-1.amazonaws.com" url)
                 (values "{\"AuthenticationResult\":{\"AccessToken\":\"access-9\",\"IdToken\":\"id-9\",\"RefreshToken\":\"refresh-9\",\"ExpiresIn\":3600}}"
                         200))
                (t (error "unexpected request to ~a" url))))))

(deftest hireko-cell-sign-in-reads-the-tenant-page-then-cognito ()
  (with-hireko-wire
    (with-forgotten-session ("acme")
      (hireko-cognito-wire)
      (with-stubbed-fdefinition (uiop:getenv (name)
                                 (cond ((string= name "HIREKO_USERNAME") "recruiter@acme.test")
                                       ((string= name "HIREKO_PASSWORD") "s3cret")
                                       (t nil)))
        (with-cell-stop ((hireko-start "tenant" "acme"))
          (is-carrying (answer (hireko-tool "hireko_login" (nlk:make-json-object)))
                       ("Signed in to tenant \"acme\"" "the sign-in answered")
                       ("recruiter@acme.test" "as the recruiter"))
          (let ((requests (hireko-asked)))
            (is (= 2 (length requests)) "the tenant's page, then Cognito")
            (let ((page (first requests)) (cognito (second requests)))
              (is (equal "https://recruiter-acme.beta.hireko.ai/assets/Json/tenant_configuration.json"
                         (second page)) "the page the original reads, on the tenant's own host")
              (is (equal "https://cognito-idp.eu-west-1.amazonaws.com/" (second cognito)) "Cognito in the region the pool id names")
              (is (equal "AWSCognitoIdentityProviderService.InitiateAuth"
                         (hireko-request-header cognito "X-Amz-Target")) "its InitiateAuth target")
              (let ((body (hireko-request-body cognito)))
                (is (equal "USER_PASSWORD_AUTH" (nlk:json-value body :string "AuthFlow")))
                (is (equal "client-from-tenant" (nlk:json-value body :string "ClientId")) "the tenant's own client id")
                (is (equal "recruiter@acme.test"
                           (nlk:json-value body :string "AuthParameters" "USERNAME")) "the email from the environment, never from an argument"))))
          (let ((session (nodecode-hireko::saved-session "acme")))
            (is (equal "access-9" (nlk:json-value session :string "access_token")) "the token is kept")
            (is (nodecode-hireko::fresh-session-p session) "and counts as live"))
          (is-carrying (answer (hireko-tool "hireko_auth_status" (nlk:make-json-object)))
                       ("Authenticated for tenant \"acme\"" "the status tool sees it")))))))

;;; --- the interview link -------------------------------------------------------

(defun hireko-interview-wire ()
  "The facade answering a create, then a poll that has the session."
  (setf *hireko-answer-fn*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((body (nlk:decode-json (getf args :content)))
                 (name (nlk:json-value body :string "params" "name")))
            (cond ((equal name "create_interview_link")
                   (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"{\\\"data\\\":{\\\"external_job_group_id\\\":\\\"ext-1\\\"}}\"}]}}" 200))
                  ((equal name "get_interview_link")
                   (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"structuredContent\":{\"jobId\":\"job_42\",\"status\":\"COMPLETED\"}}}" 200))
                  (t (error "unexpected tool ~a" name)))))))

(deftest hireko-cell-create-interview-answers-the-candidate-link ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (hireko-session "test" :token "token-1")
      (hireko-interview-wire)
      (with-cell-stop ((hireko-start "poll_interval_seconds" 0))
        (let ((text (hireko-tool "hireko_create_interview_link"
                                  (nlk:json-object "title" "Senior Backend Engineer"
                                                    "company_name" "Acme Corp"
                                                    "wait_seconds" 5))))
          (is (search "COMPLETED" text) (format nil "the poll found the session: ~a" text))
          (is (search "https://interview-test.beta.hireko.ai/anonymousInterviewSession/job_42?settings=eyJ"
                      text) "the candidate link the original builds")
          (is (search "job_id: job_42" text) "and the job id beside it"))
        (let ((requests (hireko-asked)))
          (is (= 2 (length requests)) "one create, one poll")
          (let ((create (first requests)) (poll (second requests)))
            (is (equal "create_interview_link" (hireko-call-name create)))
            (is (equal "get_interview_link" (hireko-call-name poll)))
            (is (equal "Bearer token-1" (hireko-request-header create "Authorization")) "the saved token rides the call")
            (is (equal "test" (hireko-request-header create "x-tenant")) "with its tenant")
            (is (equal "us-east-1" (hireko-request-header create "AuthorizationRegion")) "and the region the original names")
            (let ((arguments (hireko-call-arguments create)))
              (is (equal "Senior Backend Engineer" (nlk:json-value arguments :string "title")))
              (is (equal "test" (nlk:json-value arguments :string "tenant")) "the tenant is filled in")
              (is (equal "beta" (nlk:json-value arguments :string "stage")) "and the stage")
              (is (equal "AllFunnels" (nlk:json-value arguments :string "funnel")) "the original's funnel")
              (is (uiop:string-prefix-p "senior-backend-engineer-test-"
                                         (nlk:json-value arguments :string "external_job_group_id")) "the job group id the original mints")
              (is (integerp (nlk:json-value arguments :integer "start_date")) "dates are epoch milliseconds")
              (is (> (nlk:json-value arguments :integer "end_date")
                     (nlk:json-value arguments :integer "start_date")) "the end is after the start"))
            (is (equal "ext-1" (nlk:json-value (hireko-call-arguments poll) :string
                                                 "external_job_group_id")) "the id Hireko answered is the one polled")))))))

(deftest hireko-cell-a-refused-call-is-a-repair-not-a-crash ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (hireko-session "test" :token "stale-1")
      (hireko-answer "{\"message\":\"Invalid token\"}" 401)
      (with-cell-stop ((hireko-start))
        (is-carrying (answer (hireko-tool "hireko_health" (nlk:make-json-object)))
                     ("ERROR: " "the tool answers a refusal")
                     ("HTTP 401" "with the status")
                     ("Invalid token" "and Hireko's own words"))))))

(deftest hireko-cell-signed-out-create-says-what-to-set ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (with-cell-stop ((hireko-start))
        (is-carrying (answer (hireko-tool "hireko_create_interview_link"
                                           (nlk:json-object "title" "Data Engineer")))
                     ("ERROR: " "a refusal, not a crash")
                     ("HIREKO_PASSWORD" "naming what to set"))
        (is (null (hireko-asked)) "nothing is sent without a session")))))

(deftest hireko-cell-get-interview-link-polls-once-when-nothing-is-ready ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (hireko-session "test")
      (hireko-answer "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"structuredContent\":{\"status\":\"IN_PROGRESS\"}}}")
      (with-cell-stop ((hireko-start "poll_interval_seconds" 0))
        (is-carrying (answer (hireko-tool "hireko_get_interview_link"
                                           (nlk:json-object "external_job_group_id" "ext-7"
                                                             "wait_seconds" 1)))
                     ("STARTED" "the answer says it is still generating")
                     ("\"ext-7\"" "and names the id to ask again with"))
        (is (= 1 (length (hireko-asked))) "one look, then the wait ran out")))))

(deftest hireko-cell-search-score-passes-the-job-id ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (hireko-session "test")
      (hireko-answer "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"structuredContent\":{\"scores\":[{\"candidate\":\"A\",\"score\":90}]}}}")
      (with-cell-stop ((hireko-start))
        (let ((text (hireko-tool "hireko_get_search_score" (nlk:json-object "job_id" "job_7"))))
          (is-carrying (answer text)
                       ("\"score\":90" (format nil "the answer's own numbers, as JSON: ~a" text))))
        (let ((request (first (hireko-asked))))
          (is (equal "job_7" (nlk:json-value (hireko-call-arguments request) :string "job_id")) "the job the call named"))))))

;;; --- /hireko ------------------------------------------------------------------

(deftest hireko-cell-the-command-says-where-it-stands ()
  (with-hireko-wire
    (with-forgotten-session ("test")
      (with-cell-stop ((hireko-start))
        (is-carrying (answer (nodecode-hireko::run-command "" nil))
                     ("signed out of tenant \"test\"" "with nothing, the state"))
        (is-carrying (answer (nodecode-hireko::run-command "nonsense" nil))
                     ("signed out of tenant \"test\"" "an unknown word is the state too"))
        (is-carrying (answer (nodecode-hireko::run-command "out" nil))
                     ("had no saved session" "signing out of nothing says so"))))))
