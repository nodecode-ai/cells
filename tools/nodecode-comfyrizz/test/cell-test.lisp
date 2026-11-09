;;;; cell-test.lisp --- lifecycle, the twelve tools, /comfyrizz, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start with the default loopback URL, a non-loopback URL refused, a
;;;; token_env naming an empty variable refused), each tool through the seam
;;;; the model calls it by -- its method, its URL, its body and the answer it
;;;; builds -- the approval refusals the original sends without an engine
;;;; call, the stub the original answers when the engine is down, and
;;;; /comfyrizz with and without a brief. Every request is a stubbed
;;;; NLK:HTTP, so nothing reaches a Comfyrizz engine or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *comfyrizz-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *comfyrizz-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-comfyrizz-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *COMFYRIZZ-REQUESTS*
and answered by *COMFYRIZZ-ANSWER-FN*, so no test dials an engine.
UIOP:GETENV answers nothing either, so a token in the machine's environment
cannot reach a test that did not ask for one."
  `(let ((*comfyrizz-requests* '())
         (*comfyrizz-answer-fn* (lambda (method url args)
                                   (declare (ignore method url args))
                                   (error "unexpected Comfyrizz request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *comfyrizz-requests*)
                                (funcall *comfyrizz-answer-fn* method url args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun comfyrizz-asked ()
  "The stubbed requests, oldest first."
  (reverse *comfyrizz-requests*))

(defun comfyrizz-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun comfyrizz-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun comfyrizz-answer (text &optional (status 200))
  "Answer every request with TEXT at STATUS."
  (setf *comfyrizz-answer-fn* (lambda (method url args)
                                 (declare (ignore method url args))
                                 (values text status))))

(defun comfyrizz-routes (routes)
  "Answer each request by the first of ROUTES whose needle its URL carries:
(needle . text), with an optional third element as its status."
  (setf *comfyrizz-answer-fn*
        (lambda (method url args)
          (declare (ignore method args))
          (let ((route (find-if (lambda (route) (search (first route) url)) routes)))
            (unless route (error "unexpected request to ~a" url))
            (values (second route) (or (third route) 200))))))

(defun comfyrizz-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "comfyrizz" "COMFYRIZZ-CELL-")

(defun comfyrizz-config (&rest pairs)
  "A whole shared config with one `comfyrizz' section built from PAIRS."
  (nlk:json-object "comfyrizz" (apply #'nlk:make-json-object pairs)))

(defun comfyrizz-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-comfyrizz:start-cell (apply #'comfyrizz-config pairs)))

(define-cell-lifecycle-tests "comfyrizz"
  (:fixture with-comfyrizz-wire)
  (:help :comfyrizz)
  (:command "comfyrizz")
  (:running (is (nle::find-tool "comfyrizz_status") "comfyrizz_status is registered")
            (is (nle::find-tool "comfyrizz_accept_generation") "comfyrizz_accept_generation is registered")
            (is (nle::find-tool "comfyrizz_retake") "comfyrizz_retake is registered")
            (is (equal "http://127.0.0.1:4319" (nodecode-comfyrizz::setting :base-url)) "the default engine is this machine's port 4319")
            (is (null (nodecode-comfyrizz::setting :token)) "no token: the engine asks for none"))
  (:stopped (is (null (nle::find-tool "comfyrizz_status")) "stop unregisters the tools")
            (is (null (nle::find-tool "comfyrizz_export")) "all of them"))
  (:refused ("engine_url" "http://example.com:4319") ("engine_url" "127.0.0.1:4319")
            ("token_env" "COMFYRIZZ_TEST_TOKEN"))
  (:idle nodecode-comfyrizz:comfyrizz-error (nodecode-comfyrizz::status)))

(deftest comfyrizz-cell-a-remote-engine-refuses-at-start ()
  (with-comfyrizz-wire
    (is-carrying (answer (refusal-text nlk:config-refusal (comfyrizz-start "engine_url" "http://example.com:4319")))
                 ("is not a loopback URL" "the refusal says what is wrong")
                 ("127.0.0.1" "and names a loopback host"))
    (is (null (nle::find-tool "comfyrizz_status")) "a refused start installed nothing")))

(deftest comfyrizz-cell-the-token-rides-the-headers ()
  (with-comfyrizz-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (and (string= name "COMFYRIZZ_HOME_TOKEN") "t-secret"))
      (with-cell-stop ((comfyrizz-start "token_env" "COMFYRIZZ_HOME_TOKEN"))
        (is (equal "t-secret" (nodecode-comfyrizz::setting :token)) "token_env names the variable")
        (comfyrizz-answer "{\"ok\":true}")
        (comfyrizz-tool "comfyrizz_status" (nlk:json-object))
        (let ((request (first (comfyrizz-asked))))
          (is (equal "t-secret" (comfyrizz-request-header request "x-comfyrizz-token")) "the engine's own header")
          (is (equal "Bearer t-secret" (comfyrizz-request-header request "authorization")) "and the bearer beside it"))))))

;;; --- comfyrizz_status -----------------------------------------------------------

(deftest comfyrizz-cell-status-reads-the-engine ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"ok\":true,\"routes\":[\"brief\"]}")
      (let ((text (comfyrizz-tool "comfyrizz_status" (nlk:json-object))))
        (is (search "the engine at http://127.0.0.1:4319 answers" text))
        (is (search "\"ok\":true" text) "the health JSON rides the answer"))
      (let ((request (first (comfyrizz-asked))))
        (is (= 1 (length (comfyrizz-asked))) "one request")
        (is (eq :get (first request)))
        (is (equal "http://127.0.0.1:4319/api/health" (second request)))
        (is (null (comfyrizz-request-header request "x-comfyrizz-token")) "no token header without a token")))))

(deftest comfyrizz-cell-an-engine-that-does-not-answer-is-a-stub ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (setf *comfyrizz-answer-fn* (lambda (method url args)
                                     (declare (ignore method url args))
                                     (error "Connection refused")))
      (is-carrying (answer (comfyrizz-tool "comfyrizz_status" (nlk:json-object)))
                   ("stub: true" "the original's stub, not a failure")
                   ("the Comfyrizz engine did not answer on http://127.0.0.1:4319" "with the reason, and never the token")))))

(deftest comfyrizz-cell-a-refused-route-names-what-the-engine-said ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"error\":\"unknown production\",\"code\":\"NOT_FOUND\"}" 404)
      (is-carrying (answer (comfyrizz-tool "comfyrizz_job_status" (nlk:json-object "jobId" "j1")))
                   ("ERROR: engine_error" "the refusal is the model's to repair")
                   ("HTTP 404" "with the status")
                   ("unknown production" "and the engine's own words")))))

;;; --- projects, plans, the spend card ---------------------------------------------

(deftest comfyrizz-cell-create-project-posts-and-drafts ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-routes '(("/api/productions" "{\"productionId\":\"p9\"}")
                           ("/api/brief" "{\"shots\":[{\"id\":\"s1\"}]}")))
      (let ((text (comfyrizz-tool "comfyrizz_create_project"
                                   (nlk:json-object "brief" "a film about rain" "title" "Rain"))))
        (is (search "the project id every later call carries is p9" text) text)
        (is (search "s1" text) "the drafted plan rides the answer"))
      (let* ((requests (comfyrizz-asked))
             (create (comfyrizz-request-body (first requests)))
             (draft (comfyrizz-request-body (second requests))))
        (is (= 2 (length requests)) "a production, then its brief")
        (is (equal "production.create" (nlk:json-value create :string "kind")))
        (is (equal (nlk:json-value create :string "commandId")
                   (nlk:json-value create :string "idempotencyKey")) "one id, both keys")
        (is (= 0 (nlk:json-value (nlk:json-value create :object "expected") :integer "productionRevision")))
        (is (equal "a film about rain" (nlk:json-value draft :string "prompt")))
        (is (equal "Rain" (nlk:json-value draft :string "title")))))))

(deftest comfyrizz-cell-list-projects-asks-for-the-limit ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"packages\":[{\"id\":\"p1\"}]}")
      (is (search "p1" (comfyrizz-tool "comfyrizz_list_projects" (nlk:json-object "limit" 5))))
      (is (search "?limit=5" (second (first (comfyrizz-asked)))) "the call's limit rides the query")
      (is (equal "http://127.0.0.1:4319/api/packages?limit=200"
                 (progn (comfyrizz-tool "comfyrizz_list_projects" (nlk:json-object "limit" 500))
                        (second (second (comfyrizz-asked))))) "and the endpoint's own cap holds it at 200"))))

(deftest comfyrizz-cell-plan-drafts-asks-or-refuses ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-routes '(("/easy-plan" "{\"plan\":\"easy\"}") ("/api/brief" "{\"plan\":\"draft\"}")
                           ("/api/productions/p1" "{\"plan\":\"current\"}")))
      (is (search "draft" (comfyrizz-tool "comfyrizz_plan" (nlk:json-object "brief" "rain"))))
      (is (equal "http://127.0.0.1:4319/api/brief" (second (first (comfyrizz-asked)))))
      (is (search "easy" (comfyrizz-tool "comfyrizz_plan"
                                          (nlk:json-object "projectId" "p1" "level" "draft" "compute" "cpu"))))
      (is (equal "http://127.0.0.1:4319/api/productions/p1/easy-plan" (second (second (comfyrizz-asked)))) "the project's easy plan")
      (is (search "current" (comfyrizz-tool "comfyrizz_plan" (nlk:json-object "projectId" "p1"))))
      (is (= 3 (length (comfyrizz-asked))))
      (let ((text (comfyrizz-tool "comfyrizz_plan" (nlk:json-object))))
        (is (search "ERROR: PLAN_TARGET_REQUIRED" text) "nothing named, nothing planned")
        (is (= 3 (length (comfyrizz-asked))) "and no engine call for it")))))

(deftest comfyrizz-cell-get-plan-reads-the-latest-proposal ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"events\":[{\"type\":\"plan.claimed\",\"data\":{}},{\"type\":\"plan.proposed\",\"data\":{\"planHash\":\"h1\"}},{\"type\":\"plan.proposed\",\"data\":{\"planHash\":\"h2\"}}],\"lastSequence\":7}")
      (let ((text (comfyrizz-tool "comfyrizz_get_plan" (nlk:json-object "projectId" "p1"))))
        (is (search "a plan was proposed" text))
        (is (search "h2" text) "the last proposal, not the first")
        (is (search "last sequence 7" text)))
      (is (equal "http://127.0.0.1:4319/api/productions/p1/events?after=0"
                 (second (first (comfyrizz-asked))))))))

(deftest comfyrizz-cell-the-spend-card-carries-its-ceiling ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"ceiling\":\"12 units\",\"approvalId\":\"ap1\"}")
      (let ((text (comfyrizz-tool "comfyrizz_get_spend_card" (nlk:json-object "projectId" "p1"))))
        (is (search "ceiling 12 units" text))
        (is (search "ask for an unambiguous yes" text) "the rule rides the answer")
        (is (search "ap1" text) "and the approval id the acceptance needs"))
      (is (equal "http://127.0.0.1:4319/api/productions/p1/spend-cap"
                 (second (first (comfyrizz-asked))))))))

;;; --- acceptance, jobs, takes, assembly, export, retake -----------------------------

(deftest comfyrizz-cell-acceptance-needs-an-approval-and-a-target ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (is (search "ERROR: APPROVAL_REQUIRED"
                  (comfyrizz-tool "comfyrizz_accept_generation"
                                   (nlk:json-object "projectId" "p1" "actionId" "a1"))) "no approval id, no dispatch")
      (is (search "ERROR: APPROVAL_TARGET_INCOMPLETE"
                  (comfyrizz-tool "comfyrizz_accept_generation"
                                   (nlk:json-object "projectId" "p1" "actionId" "a1" "approvalId" "ap1"))) "and no plan to dispatch against")
      (is (null (comfyrizz-asked)) "neither refusal reached the engine"))))

(deftest comfyrizz-cell-acceptance-dispatches-one-approved-action ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"dispatched\":true}")
      (is (search "accepted with approval ap1"
                  (comfyrizz-tool "comfyrizz_accept_generation"
                                   (nlk:json-object "projectId" "p1" "actionId" "a1"
                                                    "planId" "pl1" "planRevision" 3
                                                    "approvalId" "ap1"))))
      (let* ((request (first (comfyrizz-asked)))
             (body (comfyrizz-request-body request))
             (payload (nlk:json-value body :object "payload")))
        (is (eq :post (first request)))
        (is (equal "http://127.0.0.1:4319/api/productions/p1/actions/a1" (second request)))
        (is (equal "action.dispatch" (nlk:json-value body :string "kind")))
        (is (equal "ap1" (nlk:json-value payload :string "approvalId")))
        (is (equal "pl1" (nlk:json-value payload :string "planId")))
        (is (= 3 (nlk:json-value payload :integer "planRevision")))
        (is (= 0 (nlk:json-value (nlk:json-value body :object "expected") :integer "productionRevision")) "a production revision not named reads as 0")))))

(deftest comfyrizz-cell-takes-and-jobs-read-their-own-routes ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"takes\":[]}")
      (comfyrizz-tool "comfyrizz_list_takes" (nlk:json-object "projectId" "p1"))
      (comfyrizz-tool "comfyrizz_list_takes" (nlk:json-object "projectId" "p1" "clipId" "c1"))
      (is (equal "http://127.0.0.1:4319/api/package/p1/clips"
                 (second (first (comfyrizz-asked)))))
      (is (equal "http://127.0.0.1:4319/api/package/p1/clips/c1/takes"
                 (second (second (comfyrizz-asked)))) "one clip, its own takes")
      (comfyrizz-tool "comfyrizz_job_status" (nlk:json-object "jobId" "j1"))
      (is (equal "http://127.0.0.1:4319/api/run/j1" (second (third (comfyrizz-asked))))))))

(deftest comfyrizz-cell-assemble-and-export-post-their-documents ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"timeline\":\"t\"}")
      (comfyrizz-tool "comfyrizz_assemble"
                       (nlk:json-object "projectId" "p1" "package" (nlk:json-object "a" 1) "revision" 4))
      (let ((body (comfyrizz-request-body (first (comfyrizz-asked)))))
        (is (equal "http://127.0.0.1:4319/api/compile" (second (first (comfyrizz-asked)))))
        (is (equal "p1" (nlk:json-value body :string "packageId")))
        (is (= 4 (nlk:json-value body :integer "revision")))
        (is (= 1 (nlk:json-value body :integer "doc" "package" "a")) "the package document the call carried")
        (is (null (nlk:json-value body :object "doc" "editor")) "editor stays absent when the call names none"))
      (comfyrizz-answer "{\"path\":\"/tmp/out.mp4\"}")
      (is (search "/tmp/out.mp4" (comfyrizz-tool "comfyrizz_export"
                                                  (nlk:json-object "projectId" "p1"
                                                                    "variants" (nlk:json-object "k" "web")))))
      (let ((body (comfyrizz-request-body (second (comfyrizz-asked)))))
        (is (equal "http://127.0.0.1:4319/api/export" (second (second (comfyrizz-asked)))))
        (is (equal "web" (nlk:json-value (nlk:json-value body :object "variants") :string "k")) "the variants the call named")))))

(deftest comfyrizz-cell-retake-carries-its-change-and-no-approval ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"queued\":true}")
      (let ((text (comfyrizz-tool "comfyrizz_retake"
                                   (nlk:json-object "projectId" "p1" "clipId" "c1"
                                                    "change" (nlk:json-object "why" "too dark")))))
        (is (search "a paid retake still needs a fresh spend card" text)))
      (let ((body (comfyrizz-request-body (first (comfyrizz-asked)))))
        (is (equal "http://127.0.0.1:4319/api/package/p1/clips/c1/regenerate"
                   (second (first (comfyrizz-asked)))))
        (is (equal "too dark" (nlk:json-value (nlk:json-value body :object "change") :string "why")))
        (is (null (nlk:json-value body :string "approvalId")) "no approval of its own")))))

;;; --- /comfyrizz ------------------------------------------------------------------

(deftest comfyrizz-cell-the-command-shows-health-or-plans-a-brief ()
  (with-comfyrizz-wire
    (with-cell-stop ((comfyrizz-start))
      (comfyrizz-answer "{\"ok\":true}")
      (is (search "the engine at http://127.0.0.1:4319 answers" (cell-entry "nodecode-comfyrizz" "comfyrizz" "")))
      (is (equal "http://127.0.0.1:4319/api/health" (second (first (comfyrizz-asked)))) "a bare command asks only for health")
      (comfyrizz-answer "{\"plan\":\"draft\"}")
      (is (search "draft" (cell-entry "nodecode-comfyrizz" "comfyrizz" "a film about rain")))
      (is (equal "a film about rain"
                 (nlk:json-value (comfyrizz-request-body (second (comfyrizz-asked))) :string "prompt")) "a brief is free to propose"))))
