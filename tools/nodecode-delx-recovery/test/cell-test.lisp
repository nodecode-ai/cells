;;;; cell-test.lisp --- lifecycle, the five tools, /delx, and the wire they share.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a refused endpoint, a refused language, an integer under its
;;;; floor), the five tools through the seam the model calls them by -- their
;;;; URL, their JSON-RPC body, their headers and their answer -- /delx with
;;;; and without an argument, registration, and a private call with no
;;;; credential. Every request is a stubbed NLK:HTTP, so nothing reaches Delx.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-delx-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Delx."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected delx request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun delx-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun delx-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun delx-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun delx-argument (request &rest path)
  "REQUEST's JSON-RPC arguments, read at PATH."
  (apply #'nlk:json-value (delx-request-body request) :string "params" "arguments" path))

(defun delx-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun delx-envelope (text &key (failed nil))
  "TEXT as the MCP server answers a tools/call, its content block carrying TEXT."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 1
                     "result" (nlk:json-object
                               "content" (coerce (list (nlk:json-object "type" "text" "text" text)) 'vector)
                               "isError" (and failed t)))))

(defun delx-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun delx-tool-answer (name &rest pairs)
  "Run tool NAME with PAIRS as its arguments, the way a model call arrives."
  (delx-tool name (apply #'nlk:make-json-object pairs)))

(defun delx-registered (&rest pairs)
  "The delx section a test starts with, holding PAIRS."
  (apply #'delx-recovery-start (append (list "endpoint" "https://delx.test/mcp") pairs)))

;;; --- lifecycle ----------------------------------------------------------------

(defun delx-config (&rest pairs)
  "A whole shared config with one `delx' section built from PAIRS."
  (nlk:json-object "delx" (apply #'nlk:make-json-object pairs)))

(defun delx-recovery-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-delx-recovery::start-cell (apply #'delx-config pairs)))

(define-test-slice "delx-recovery" "DELX-")

(define-cell-lifecycle-tests "delx-recovery"
  (:fixture with-delx-wire)
  (:config "endpoint" "https://delx.test/mcp" "agent_id" "ag-7" "agent_token" "tok-7"
           "session_id" "s-1")
  (:help :delx)
  (:command "delx")
  (:running (is (nle::find-tool "delx_check") "delx_check is registered")
            (is (nle::find-tool "delx_resume"))
            (is (nle::find-tool "delx_remember"))
            (is (nle::find-tool "delx_seal") "delx_seal is registered")
            (is (nle::find-tool "delx_close"))
            (is (equal "https://delx.test/mcp" (nodecode-delx-recovery::endpoint)))
            (is (equal "en" (nodecode-delx-recovery::setting :language)) "the default language")
            (is (= 30 (nodecode-delx-recovery::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "delx_check")) "stop unregisters the tools"))
  (:refused ("endpoint" "http://delx.test/mcp") ("language" "fr") ("timeout_seconds" 0))
  (:idle nodecode-delx-recovery:delx-recovery-error (nodecode-delx-recovery::run-command "" nil)))

;;; --- the tools -----------------------------------------------------------------

(deftest delx-cell-public-triage-carries-no-credential ()
  (with-delx-wire
    (with-cell-stop ((delx-recovery-start "endpoint" "https://delx.test/mcp"))
      (delx-answer (delx-envelope "One step: restart the worker."))
      (is-carrying (answer (delx-tool-answer "delx_check" "problem" "the worker died"))
                   ("One step: restart the worker."))
      (let ((request (first (delx-asked))))
        (is (= 1 (length (delx-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://delx.test/mcp" (second request)))
        (is (equal "tools/call" (nlk:json-value (delx-request-body request) :string "method")))
        (is (equal "2.0" (nlk:json-value (delx-request-body request) :string "jsonrpc")))
        (is (equal "discovery_self_check"
                   (nlk:json-value (delx-request-body request) :string "params" "name")))
        (is (equal "2" (delx-argument request "entry_version")))
        (is (equal "the worker died" (delx-argument request "problem")))
        (is (equal "en" (delx-argument request "language")) "the section's language")
        (is (null (delx-request-header request "x-delx-agent-token")) "triage carries no credential")))))

(deftest delx-cell-a-private-call-without-a-credential-refuses ()
  (with-delx-wire
    (with-cell-stop ((delx-recovery-start "endpoint" "https://delx.test/mcp"))
      (is-carrying (answer (delx-tool "delx_resume" (nlk:json-object)))
                   ("ERROR: delx:" "no agent credential yet") ("/delx register"))
      (is (null (delx-asked)) "nothing was sent"))
    (with-cell-stop ((delx-registered "session_id" "s-1"))
      (is-carrying (answer (delx-tool-answer "delx_close"
                                             "goal" "g" "done" "d" "next" "n"))
                   ("no agent credential yet" "the close refuses before it seals"))
      (is (null (delx-asked)) "and sent nothing"))))

(deftest delx-cell-resume-reads-the-capsule-back ()
  (with-delx-wire
    (with-cell-stop ((delx-registered "agent_id" "ag-7" "agent_token" "tok-7"))
      (delx-answer (delx-envelope "{\"capsule\":{\"next\":\"run the tests\"}}"))
      (is-carrying (answer (delx-tool "delx_resume" (nlk:json-object)))
                   ("run the tests"))
      (let ((request (first (delx-asked))))
        (is (equal "resume_session"
                   (nlk:json-value (delx-request-body request) :string "params" "name")))
        (is (equal "ag-7" (delx-argument request "agent_id")) "the section's agent id selects the lineage")
        (is (equal "ag-7" (delx-request-header request "x-delx-agent-id")))
        (is (equal "tok-7" (delx-request-header request "x-delx-agent-token")) "the credential rides the headers")))))

(deftest delx-cell-seal-sends-the-capsule-it-was-given ()
  (with-delx-wire
    (with-cell-stop ((delx-registered "agent_id" "ag-7" "agent_token" "tok-7"
                                      "session_id" "s-1"))
      (delx-answer (delx-envelope "Sealed."))
      (is (equal "Sealed."
                 (delx-tool-answer "delx_seal"
                                    "goal" "ship the cell" "done" "the wire" "next" "run the tests"
                                    "refuted" "the REST path, which has no capsule")))
      (let* ((request (first (delx-asked)))
             (body (delx-request-body request))
             (capsule (nlk:json-value body :object "params" "arguments" "capsule")))
        (is (equal "leave_hive_note" (nlk:json-value body :string "params" "name")))
        (is (equal "s-1" (nlk:json-value body :string "params" "arguments" "session_id")))
        (is (equal "1" (nlk:json-value capsule :string "version")) "the schema's version")
        (is (equal "ship the cell" (nlk:json-value capsule :string "goal")))
        (is (equal "the wire" (nlk:json-value capsule :string "done")))
        (is (equal "run the tests" (nlk:json-value capsule :string "next")))
        (is (equal "the REST path, which has no capsule" (nlk:json-value capsule :string "refuted")))
        (is (null (nlk:json-value capsule :string "blockers")) "an unanswered field is left out")))))

(deftest delx-cell-close-reports-a-rating-then-seals ()
  (with-delx-wire
    (with-cell-stop ((delx-registered "agent_id" "ag-7" "agent_token" "tok-7"
                                      "session_id" "s-1"))
      (delx-answer (delx-envelope "Closed."))
      (is (equal "Closed."
                 (delx-tool-answer "delx_close"
                                    "goal" "ship" "done" "all of it" "next" "review"
                                    "rating" 9)))
      (let ((first (first (delx-asked)))
            (second (second (delx-asked))))
        (is (= 2 (length (delx-asked))) "feedback, then the close")
        (is (equal "provide_feedback"
                   (nlk:json-value (delx-request-body first) :string "params" "name")))
        (let ((rating (nlk:json-value (delx-request-body first) :integer
                                       "params" "arguments" "rating")))
          (is (= 5 rating) "a rating above five reads as five"))
        (is (equal "close_session"
                   (nlk:json-value (delx-request-body second) :string "params" "name")))
        (is (equal "review"
                   (nlk:json-value (delx-request-body second) :string
                                   "params" "arguments" "capsule" "next")))))))

(deftest delx-cell-remember-needs-a-session ()
  (with-delx-wire
    (with-cell-stop ((delx-registered "agent_id" "ag-7" "agent_token" "tok-7"))
      (is-carrying (answer (delx-tool-answer "delx_remember" "key" "hive.next" "value" "x"))
                   ("no session yet"))
      (is (null (delx-asked)) "nothing was sent"))
    (with-cell-stop ((delx-registered "agent_id" "ag-7" "agent_token" "tok-7"
                                      "session_id" "s-1"))
      (delx-answer (delx-envelope "Stored."))
      (is (equal "Stored." (delx-tool-answer "delx_remember" "key" "hive.next" "value" "run the tests")))
      (is (equal "add_context_memory"
                 (nlk:json-value (delx-request-body (first (delx-asked))) :string "params" "name")))
      (is (equal "run the tests" (delx-argument (first (delx-asked)) "value"))))))

(deftest delx-cell-an-event-stream-answer-is-read ()
  (with-delx-wire
    (with-cell-stop ((delx-recovery-start "endpoint" "https://delx.test/mcp"))
      (delx-answer (format nil "event: message~%data: ~a~%~%" (delx-envelope "Warm resume.")))
      (is (equal "Warm resume." (delx-tool-answer "delx_check" "problem" "compacted")))
      (delx-answer (format nil "event: message~%data: [DONE]~%~%"))
      (is-carrying (answer (delx-tool-answer "delx_check" "problem" "compacted"))
                   ("the server answered no result" "an empty stream is a refusal, not a blank")))))

(deftest delx-cell-a-refused-call-is-a-repair-not-a-crash ()
  (with-delx-wire
    (with-cell-stop ((delx-registered "agent_id" "ag-7" "agent_token" "tok-7"))
      (delx-answer (delx-envelope "the session is closed" :failed t))
      (is-carrying (answer (delx-tool "delx_resume" (nlk:json-object)))
                   ("ERROR: delx:" "resume_session: the session is closed"))
      (delx-answer "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"unknown tool\"}}")
      (is-carrying (answer (delx-tool "delx_resume" (nlk:json-object)))
                   ("resume_session: unknown tool"))
      (delx-answer "{\"message\":\"slow down\"}" 429)
      (is-carrying (answer (delx-tool "delx_resume" (nlk:json-object)))
                   ("HTTP 429" "slow down")))))

;;; --- /delx ---------------------------------------------------------------------

(deftest delx-cell-the-command-shows-the-session-or-triages ()
  (with-delx-wire
    (with-cell-stop ((delx-registered))
      (let ((answer (cell-entry "nodecode-delx-recovery" "delx" "")))
        (is (search "Delx Protocol recovery at https://delx.test/mcp" answer))
        (is (search "no agent credential yet" answer))
        (is (null (delx-asked)) "a status read asks the server nothing"))
      (delx-answer (delx-envelope "Name the failing step."))
      (is (equal "Name the failing step."
                 (cell-entry "nodecode-delx-recovery" "delx" "the build looped")))
      (is (equal "the build looped" (delx-argument (first (delx-asked)) "problem"))))))

(deftest delx-cell-register-asks-for-an-identity-and-says-what-to-keep ()
  (with-delx-wire
    (with-cell-stop ((delx-recovery-start "endpoint" "https://delx.test/mcp"))
      (delx-answer "{\"agent_id\":\"ag-9\",\"session_id\":\"s-9\",\"identity_auth\":{\"token\":\"tok-9\"}}")
      (is-carrying (answer (cell-entry "nodecode-delx-recovery" "delx" "register"))
                   ("Registered with Delx as agent ag-9") ("session s-9") ("tok-9")
                   ("(config-set '(\"delx\" \"agent_token\")")
                   ("token is shown once" "the answer says the token is shown once"))
      (let ((request (first (delx-asked))))
        (is (equal "https://delx.test/api/v1/agents/register" (second request)) "the endpoint's own host")
        (is (uiop:string-prefix-p "rk1_" (nlk:json-value (delx-request-body request) :string
                                                          "registration_key")))
        (is (= 47 (length (nlk:json-value (delx-request-body request) :string "registration_key"))) "rk1_ and 32 unpadded bytes")
        (is (equal "skill" (nlk:json-value (delx-request-body request) :string "source")))))))
