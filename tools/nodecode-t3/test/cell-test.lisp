;;;; cell-test.lisp --- lifecycle, the six tools, /t3, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a choice outside its options or an integer under its floor
;;;; refused, a start with no token refused), each tool through the seam the
;;;; model calls it by -- its method, its URL, its headers, its body and the
;;;; answer it builds -- and /t3 with and without an argument. Every request
;;;; is a stubbed NLK:HTTP, so nothing reaches T3 or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-t3-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials T3."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected T3 request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun t3-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun t3-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun t3-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun t3-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun t3-ends-with-p (suffix text)
  "T when TEXT ends in SUFFIX."
  (let ((start (- (length text) (length suffix))))
    (and (>= start 0) (string= suffix text :start2 start))))

(defun t3-routes (table)
  "Answer each request from TABLE, a list of (PATH-ENDING . TEXT): the first
entry whose path the URL ends in answers it, so one test can serve a thread
read and the dispatch that follows it."
  (setf *answer-fn*
        (lambda (method url args)
          (declare (ignore method args))
          (let ((entry (find-if (lambda (entry) (t3-ends-with-p (first entry) url)) table)))
            (if entry
                (values (rest entry) 200)
                (values "{\"error\":\"no stub\"}" 404))))))

(defun t3-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- what T3 answers -------------------------------------------------------------

(defparameter +shell+
  "{\"snapshotSequence\":7,\"updatedAt\":\"2026-11-08T10:00:00Z\",
    \"projects\":[{\"id\":\"p1\",\"title\":\"shop\",\"workspaceRoot\":\"/w/shop\",
                  \"defaultModelSelection\":{\"instanceId\":\"inst-1\",\"model\":\"model-1\",\"options\":[]}}],
    \"threads\":[
      {\"id\":\"thread-1\",\"projectId\":\"p1\",\"title\":\"Fix the login test\",
       \"modelSelection\":{\"instanceId\":\"inst-1\",\"model\":\"model-1\",\"options\":[]},
       \"runtimeMode\":\"approval-required\",\"interactionMode\":\"default\",
       \"branch\":null,\"worktreePath\":null,\"archivedAt\":null,
       \"createdAt\":\"2026-11-08T09:00:00Z\",\"updatedAt\":\"2026-11-08T09:30:00Z\",
       \"hasPendingApprovals\":true,\"hasPendingUserInput\":false,\"hasActionableProposedPlan\":false,
       \"latestTurn\":{\"turnId\":\"turn-1\",\"state\":\"running\",
                      \"requestedAt\":\"2026-11-08T09:20:00Z\",\"startedAt\":\"2026-11-08T09:20:01Z\",
                      \"completedAt\":null,\"assistantMessageId\":null},
       \"session\":{\"threadId\":\"thread-1\",\"status\":\"running\",\"providerName\":\"provider\",
                   \"runtimeMode\":\"approval-required\",\"activeTurnId\":\"turn-1\",
                   \"lastError\":null,\"updatedAt\":\"2026-11-08T09:30:00Z\"}},
      {\"id\":\"thread-2\",\"projectId\":\"p1\",\"title\":\"Ship the release\",
       \"modelSelection\":{\"instanceId\":\"inst-1\",\"model\":\"model-1\",\"options\":[]},
       \"runtimeMode\":\"auto\",\"interactionMode\":\"default\",
       \"branch\":\"main\",\"worktreePath\":null,\"archivedAt\":null,
       \"createdAt\":\"2026-11-07T09:00:00Z\",\"updatedAt\":\"2026-11-07T12:00:00Z\",
       \"hasPendingApprovals\":false,\"hasPendingUserInput\":true,\"hasActionableProposedPlan\":false,
       \"latestTurn\":{\"turnId\":\"turn-9\",\"state\":\"completed\",
                      \"requestedAt\":\"2026-11-07T10:00:00Z\",\"startedAt\":\"2026-11-07T10:00:01Z\",
                      \"completedAt\":\"2026-11-07T10:04:00Z\",\"assistantMessageId\":\"m-2\"},
       \"session\":{\"threadId\":\"thread-2\",\"status\":\"idle\",\"providerName\":\"provider\",
                   \"runtimeMode\":\"auto\",\"activeTurnId\":null,
                   \"lastError\":null,\"updatedAt\":\"2026-11-07T12:00:00Z\"}}]}"
  "A shell snapshot with one running thread that waits on an approval and one idle one.")

(defparameter +thread-1+
  "{\"snapshotSequence\":7,\"thread\":{\"id\":\"thread-1\",\"projectId\":\"p1\",
     \"title\":\"Fix the login test\",
     \"modelSelection\":{\"instanceId\":\"inst-1\",\"model\":\"model-1\",\"options\":[]},
     \"runtimeMode\":\"approval-required\",\"interactionMode\":\"default\",
     \"branch\":null,\"worktreePath\":null,\"archivedAt\":null,
     \"createdAt\":\"2026-11-08T09:00:00Z\",\"updatedAt\":\"2026-11-08T09:30:00Z\",
     \"settledAt\":null,\"hasPendingApprovals\":true,\"hasPendingUserInput\":false,
     \"hasActionableProposedPlan\":false,
     \"planProgress\":{\"step\":\"write the failing test\",\"completedSteps\":1,\"totalSteps\":3},
     \"latestTurn\":{\"turnId\":\"turn-1\",\"state\":\"running\",
                    \"requestedAt\":\"2026-11-08T09:20:00Z\",\"startedAt\":\"2026-11-08T09:20:01Z\",
                    \"completedAt\":null,\"assistantMessageId\":null},
     \"session\":{\"threadId\":\"thread-1\",\"status\":\"running\",\"providerName\":\"provider\",
                 \"runtimeMode\":\"approval-required\",\"activeTurnId\":\"turn-1\",
                 \"lastError\":null,\"updatedAt\":\"2026-11-08T09:30:00Z\"},
     \"messages\":[{\"id\":\"m-1\",\"role\":\"user\",\"text\":\"the login test is flaky\",
                   \"turnId\":\"turn-1\",\"streaming\":false,
                   \"createdAt\":\"2026-11-08T09:20:00Z\",\"updatedAt\":\"2026-11-08T09:20:00Z\"},
                  {\"id\":\"m-2\",\"role\":\"assistant\",\"text\":\"Reproduced it: the fixture leaks a session.\",
                   \"turnId\":\"turn-1\",\"streaming\":true,
                   \"createdAt\":\"2026-11-08T09:21:00Z\",\"updatedAt\":\"2026-11-08T09:21:00Z\"}],
     \"proposedPlans\":[]}}"
  "One thread mid-turn, with a plan and two messages.")

(defparameter +idle-thread+
  "{\"snapshotSequence\":8,\"thread\":{\"id\":\"thread-2\",\"projectId\":\"p1\",
     \"title\":\"Ship the release\",
     \"modelSelection\":{\"instanceId\":\"inst-1\",\"model\":\"model-1\",\"options\":[]},
     \"runtimeMode\":\"auto\",\"interactionMode\":\"default\",\"branch\":\"main\",
     \"worktreePath\":null,\"archivedAt\":null,
     \"createdAt\":\"2026-11-07T09:00:00Z\",\"updatedAt\":\"2026-11-07T12:00:00Z\",
     \"latestTurn\":null,
     \"session\":{\"threadId\":\"thread-2\",\"status\":\"idle\",\"providerName\":\"provider\",
                 \"runtimeMode\":\"auto\",\"activeTurnId\":null,\"lastError\":null,
                 \"updatedAt\":\"2026-11-07T12:00:00Z\"},
     \"messages\":[]}}"
  "A thread between turns: nothing running to interrupt.")

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "t3" "T3-CELL-" :start nodecode-t3:start-cell)

(define-cell-lifecycle-tests "t3"
  (:fixture with-t3-wire)
  (:config "token" "k-test")
  (:help :t3)
  (:command "t3")
  (:running (is (nle::find-tool "t3_threads") "t3_threads is registered")
            (is (nle::find-tool "t3_thread_read") "t3_thread_read is registered")
            (is (nle::find-tool "t3_thread_create") "t3_thread_create is registered")
            (is (nle::find-tool "t3_thread_send") "t3_thread_send is registered")
            (is (nle::find-tool "t3_turn_interrupt") "t3_turn_interrupt is registered")
            (is (nle::find-tool "t3_session_stop") "t3_session_stop is registered")
            (let ((root (nodecode-t3::setting :base-url)))
              (is (equal "http://127.0.0.1:3773" root) "the original's loopback origin"))
            (let ((mode (nodecode-t3::setting :default-runtime-mode)))
              (is (equal "approval-required" mode) "a created thread asks before it writes")))
  (:stopped (is (null (nle::find-tool "t3_threads")) "stop unregisters the tools")
            (is (null (nle::find-tool "t3_session_stop")) "all six of them"))
  (:refused ("token" "k-test" "timeout_seconds" 0)
            ("token" "k-test" "default_runtime_mode" "yolo")
            ("token" "k-test" "base_url" "127.0.0.1:3773"))
  (:idle nodecode-t3::t3-error (nodecode-t3::status-line)))

(deftest t3-cell-a-missing-token-refuses-the-start ()
  (with-t3-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (t3-start)))
                   ("no token") ("T3_ORCHESTRATION_TOKEN" "the refusal names what to set"))
      (is (null (nle::find-tool "t3_threads")) "a refused start installed nothing"))))

(deftest t3-cell-the-token-may-come-from-the-environment ()
  (with-t3-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (cond ((string= name "T3_HOME_TOKEN") "k-env")
                                     ((string= name "T3_ORCHESTRATION_TOKEN") "k-fallback")
                                     (t nil)))
      (with-cell-stop ((t3-start "token_env" "T3_HOME_TOKEN"))
        (is (equal "k-env" (nodecode-t3::setting :token)) "token_env names the variable"))
      (with-cell-stop ((t3-start))
        (is (equal "k-fallback" (nodecode-t3::setting :token)) "T3_ORCHESTRATION_TOKEN is read either way")))))

;;; --- t3_threads ----------------------------------------------------------------

(deftest t3-cell-threads-are-listed-with-their-state ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test"))
      (t3-answer +shell+)
      (let ((answer (t3-tool "t3_threads" (nlk:json-object))))
        (is (search "2 T3 threads:" answer))
        (is (search "Fix the login test" answer))
        (is (search "thread-1" answer))
        (is (search "running" answer) "the session state is named")
        (is (search "turn running" answer) "the turn state is named")
        (is (search "project shop" answer) "the thread's project is named by title")
        (is (search "needs an approval" answer) "a pending approval is a fact of the row"))
      (let* ((asked (t3-asked))
             (request (first asked)))
        (is (= 1 (length asked)) "one request")
        (is (eq :get (first request)))
        (is (equal "http://127.0.0.1:3773/api/orchestration/shell" (second request)))
        (is (equal "Bearer k-test" (t3-request-header request "authorization")))
        (is (equal "application/json" (t3-request-header request "accept")))))))

(deftest t3-cell-a-limit-clips-the-list-and-all-lifts-it ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test" "max_threads" 1))
      (t3-answer +shell+)
      (let ((answer (t3-tool "t3_threads" (nlk:json-object))))
        (is (search "newest first" answer) "a clipped list says it is clipped")
        (is (search "1 more not shown" answer))
        (is (not (search "Ship the release" answer)) "the second thread is past the limit"))
      (let ((answer (t3-tool "t3_threads" (nlk:json-object "all" t))))
        (is (search "Ship the release" answer) "all lifts the limit")
        (is (not (search "more not shown" answer)))))))

;;; --- t3_thread_read -------------------------------------------------------------

(deftest t3-cell-one-thread-is-read-with-its-messages-and-plan ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test"))
      (t3-answer +thread-1+)
      (let ((answer (t3-tool "t3_thread_read" (nlk:json-object "thread_id" "thread-1"))))
        (is (search "Fix the login test" answer))
        (is (search "latest turn turn-1: running" answer))
        (is (search "plan: write the failing test (1/3 steps)" answer))
        (is (search "messages: 2:" answer))
        (is (search "user: the login test is flaky" answer))
        (is (search "assistant: Reproduced it" answer) "the newest message is carried"))
      (let ((request (first (t3-asked))))
        (is (eq :get (first request)))
        (is (equal "http://127.0.0.1:3773/api/orchestration/threads/thread-1" (second request)))))))

(deftest t3-cell-a-failed-read-is-the-answer-not-an-error ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test"))
      (t3-answer "{\"error\":\"thread not found\"}" 404)
      (let ((answer (t3-tool "t3_thread_read" (nlk:json-object "thread_id" "thread-9"))))
        (is (search "ERROR: T3 answered HTTP 404" answer))
        (is (search "thread not found" answer) "T3's own words are repeated"))
      (let ((answer (t3-tool "t3_thread_read" (nlk:json-object "thread_id" "a/b"))))
        (is (search "ERROR:" answer) "a slash in a thread id never becomes a path")))))

;;; --- t3_thread_send, interrupt, stop -----------------------------------------------

(deftest t3-cell-a-send-reads-the-thread-then-dispatches-one-turn ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test"))
      (t3-routes (list (cons "/api/orchestration/shell" +shell+)
                       (cons "/api/orchestration/threads/thread-1" +thread-1+)
                       (cons "/api/orchestration/dispatch" "{\"sequence\":42}")))
      (let ((answer (t3-tool "t3_thread_send"
                              (nlk:json-object "thread_id" "thread-1" "message" "and the fixture?"))))
        (is (search "Queued one turn on T3 thread thread-1" answer))
        (is (search "dispatch #42" answer)))
      (let ((asked (t3-asked)))
        (is (= 2 (length asked)) "one read, one dispatch")
        (is (eq :get (first (first asked))))
        (let ((dispatch (second asked)))
          (is (eq :post (first dispatch)))
          (is (equal "http://127.0.0.1:3773/api/orchestration/dispatch" (second dispatch)))
          (is (equal "Bearer k-test" (t3-request-header dispatch "authorization")))
          (let ((body (t3-request-body dispatch)))
            (is (equal "thread.turn.start" (nlk:json-value body :string "type")))
            (is (equal "thread-1" (nlk:json-value body :string "threadId")))
            (is (equal "and the fixture?" (nlk:json-value body :string "message" "text")))
            (is (equal "user" (nlk:json-value body :string "message" "role")))
            (let ((selection (nlk:json-value body :string "modelSelection" "instanceId")))
              (is (equal "inst-1" selection) "the thread's own model selection rides along"))
            (is (equal "approval-required" (nlk:json-value body :string "runtimeMode")))))))))

(deftest t3-cell-full-access-is-said-out-loud ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test"))
      (t3-routes (list (cons "/api/orchestration/threads/thread-1" +thread-1+)
                       (cons "/api/orchestration/dispatch" "{\"sequence\":7}")))
      (let ((answer (t3-tool "t3_thread_send"
                              (nlk:json-object "thread_id" "thread-1" "message" "go"
                                               "runtime_mode" "full-access"))))
        (is (search "Full access lets the provider change files without asking" answer)))
      (is (equal "full-access"
                 (nlk:json-value (t3-request-body (second (t3-asked))) :string "runtimeMode"))))))

(deftest t3-cell-interrupt-and-stop-post-their-own-commands ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test"))
      (t3-routes (list (cons "/api/orchestration/threads/thread-1" +thread-1+)
                       (cons "/api/orchestration/threads/thread-2" +idle-thread+)
                       (cons "/api/orchestration/dispatch" "{\"sequence\":9}")))
      (let ((answer (t3-tool "t3_turn_interrupt" (nlk:json-object "thread_id" "thread-1"))))
        (is (search "Interrupted turn turn-1" answer))
        (is (equal "turn-1"
                   (nlk:json-value (t3-request-body (second (t3-asked))) :string "turnId"))))
      (setf *requests* '())
      (let ((answer (t3-tool "t3_turn_interrupt" (nlk:json-object "thread_id" "thread-2"))))
        (is (search "ERROR:" answer) "an idle thread has no turn to interrupt")
        (is (= 1 (length (t3-asked))) "and nothing was dispatched"))
      (setf *requests* '())
      (let ((answer (t3-tool "t3_session_stop" (nlk:json-object "thread_id" "thread-1"))))
        (is (search "Stopped the session of T3 thread thread-1 (was running" answer))
        (is (equal "thread.session.stop"
                   (nlk:json-value (t3-request-body (second (t3-asked))) :string "type")))))))

;;; --- t3_thread_create and /t3 ----------------------------------------------------

(deftest t3-cell-create-names-a-project-and-its-default-model ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test"))
      (t3-routes (list (cons "/api/orchestration/shell" +shell+)
                       (cons "/api/orchestration/dispatch" "{\"sequence\":11}")))
      (let ((answer (t3-tool "t3_thread_create"
                              (nlk:json-object "title" "Retry the flaky test" "project" "shop"))))
        (is (search "Created T3 thread" answer))
        (is (search "model inst-1/model-1" answer) "the project's default model is used"))
      (let ((body (t3-request-body (second (t3-asked)))))
        (is (equal "thread.create" (nlk:json-value body :string "type")))
        (is (equal "p1" (nlk:json-value body :string "projectId"))))
      (setf *requests* '())
      (let ((answer (t3-tool "t3_thread_create"
                              (nlk:json-object "title" "x" "project" "/elsewhere"))))
        (is (search "ERROR:" answer))
        (is (search "/w/shop" answer) "the refusal names the projects T3 holds")
        (is (= 1 (length (t3-asked))) "and dispatched nothing")))))

(deftest t3-cell-the-command-answers-alone-and-with-a-thread-id ()
  (with-t3-wire
    (with-cell-stop ((t3-start "token" "k-test" "max_threads" 10))
      (t3-routes (list (cons "/api/orchestration/shell" +shell+)
                       (cons "/api/orchestration/threads/thread-1" +thread-1+)))
      (let ((answer (cell-entry "nodecode-t3" "t3" "" nil)))
        (is (search "T3 Code at http://127.0.0.1:3773: 1 project, 2 threads, 1 running." answer))
        (is (search "Fix the login test" answer)))
      (let ((answer (cell-entry "nodecode-t3" "t3" "thread-1" nil)))
        (is (search "the login test is flaky" answer) "a thread id reads that thread")))))
