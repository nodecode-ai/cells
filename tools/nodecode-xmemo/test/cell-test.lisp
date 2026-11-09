;;;; cell-test.lisp --- lifecycle, the six tools, /xmemo, the filters, and the
;;;; opt-in timeline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a keyless start refused, an integer under its floor refused), the
;;;; six tools through the seam the model calls them by -- their URL, their
;;;; headers, their params or body, and their answer -- /xmemo with and
;;;; without a query, the time filters refused before anything leaves, and a
;;;; finished turn recorded when capture_timeline asks for it. Every request
;;;; is a stubbed NLK:HTTP, so nothing reaches XMemo or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-xmemo-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials XMemo."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected xmemo request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun xmemo-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun xmemo-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun xmemo-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun xmemo-request-param (request name)
  "REQUEST's URL query parameter NAME, decoded."
  (let ((query (quri:uri-query (quri:uri (second request)))))
    (cdr (assoc name (and query (quri:url-decode-params query)) :test #'string=))))

(defun xmemo-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun xmemo-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defparameter +preferences+
  "{\"results\":[{\"id\":\"m1\",\"content\":\"drinks green tea, never coffee\",\"path\":\"notes/preferences\",\"memory_type\":\"semantic\",\"similarity\":0.912,\"agent_boundary\":\"self\"},{\"id\":\"m2\",\"content\":\"lives in Oslo\"}]}"
  "A search answer: two facts, one with provenance and a score.")

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "xmemo" "XMEMO-CELL-" :start nodecode-xmemo:start-cell)

(define-cell-lifecycle-tests "xmemo"
  (:fixture with-xmemo-wire)
  (:config "api_key" "k-test" "base_url" "https://xmemo.test")
  (:help :xmemo)
  (:command "xmemo")
  (:running (is (nle::find-tool "xmemo_search") "xmemo_search is registered")
            (is (nle::find-tool "xmemo_get"))
            (is (nle::find-tool "xmemo_list"))
            (is (nle::find-tool "xmemo_recall_context"))
            (is (nle::find-tool "xmemo_remember"))
            (is (nle::find-tool "xmemo_update_state"))
            (is (equal "https://xmemo.test" (nodecode-xmemo::root)))
            (is (equal "nodecode" (nodecode-xmemo::agent-id)) "the default agent")
            (is (equal "work" (nodecode-xmemo::write-bucket)))
            (is (equal "%" (nodecode-xmemo::read-bucket)))
            (is (= 10 (nodecode-xmemo::setting :search-limit)))
            (is (= 32 (length nodecode-xmemo::*instance-id*)) "an opaque instance id is made"))
  (:stopped (is (null (nle::find-tool "xmemo_search")) "stop unregisters the tools")
            (is (equal "" nodecode-xmemo::*instance-id*) "and drops the instance id"))
  (:refused ("api_key" "k-test" "base_url" "https://xmemo.test" "max_items" 0)
            ("api_key" "k-test" "base_url" "https://xmemo.test" "search_limit" 0)
            ("api_key" "k-test" "base_url" "https://xmemo.test" "timeout_seconds" 0))
  (:idle nodecode-xmemo:xmemo-error (nodecode-xmemo::run-command "" nil)))

(deftest xmemo-cell-a-start-without-a-key-is-refused ()
  (with-xmemo-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (xmemo-start "base_url" "https://xmemo.test")))
                   ("needs a key" "XMEMO_KEY"))
      (is (null (nle::find-tool "xmemo_search")) "a refused start installed nothing"))))

;;; --- search -------------------------------------------------------------------

(deftest xmemo-cell-search-answers-and-widens-the-query ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"))
      (xmemo-answer +preferences+)
      (let ((answer (xmemo-tool "xmemo_search"
                                 (nlk:json-object "query" "memory of what I drink" "limit" 3))))
        (is (search "2 memories for \"memory of what I drink\"" answer))
        (is (search "[semantic] [self] notes/preferences (sim 0.912) id=m1" answer))
        (is (search "drinks green tea, never coffee" answer))
        (is (search "id=m2" answer)))
      (let ((request (first (xmemo-asked))))
        (is (= 1 (length (xmemo-asked))) "one request")
        (is (eq :get (first request)))
        (is (search "https://xmemo.test/v1/memories/search?" (second request)))
        (is (equal "k-test" (xmemo-request-header request "x-api-key")))
        (is (equal "nodecode" (xmemo-request-header request "x-memory-os-agent-id")))
        (is (= 32 (length (xmemo-request-header request "x-memory-os-agent-instance-id"))))
        (let ((query (xmemo-request-param request "query")))
          (is (search "memory of what I drink" query))
          (is (search "memories" query) "the alias table widened it")
          (is (search "recall" query)))
        (is (equal "3" (xmemo-request-param request "limit")))
        (is (equal "%" (xmemo-request-param request "bucket")))
        (is (null (xmemo-request-param request "scope")) "an unset scope is dropped")))))

(deftest xmemo-cell-a-failed-read-is-a-repair-not-a-crash ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"))
      (xmemo-answer "{\"message\":\"Invalid API key\"}" 401)
      (is-carrying (answer (xmemo-tool "xmemo_search" (nlk:json-object "query" "x")))
                   ("ERROR: xmemo:" "the refusal is the model's repair")
                   ("HTTP 401") ("Invalid API key"))
      (xmemo-answer "{\"results\":[]}")
      (is (equal "No relevant XMemo memories found for \"x\"."
                 (xmemo-tool "xmemo_search" (nlk:json-object "query" "x")))))))

(deftest xmemo-cell-a-bad-time-filter-is-refused-before-the-wire ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"))
      (is-carrying (answer (xmemo-tool "xmemo_search"
                                        (nlk:json-object "query" "x" "since" "yesterday")))
                   ("since must be an RFC 3339 timestamp"))
      (is (null (xmemo-asked)) "nothing left the process")
      (is-carrying (answer (xmemo-tool "xmemo_search"
                                        (nlk:json-object "query" "x"
                                                         "since" "2026-01-02T00:00:00Z"
                                                         "until" "2026-01-01T00:00:00Z")))
                   ("until must be strictly later than since"))
      (is-carrying (answer (xmemo-tool "xmemo_search"
                                        (nlk:json-object "query" "x" "time_order" "sideways")))
                   ("time_order must be one of"))
      (is (null (xmemo-asked)))
      (xmemo-answer "{\"results\":[]}")
      (xmemo-tool "xmemo_search" (nlk:json-object "query" "x"
                                                   "since" "2026-01-01T00:00:00+09:00"
                                                   "time_order" "desc"))
      (let ((request (first (xmemo-asked))))
        (is (equal "2026-01-01T00:00:00+09:00" (xmemo-request-param request "since")))
        (is (equal "desc" (xmemo-request-param request "time_order")))))))

;;; --- get and list ----------------------------------------------------------------

(deftest xmemo-cell-get-reads-one-memory-whole ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"))
      (xmemo-answer "{\"memory\":{\"id\":\"m7\",\"content\":\"a decision, whole and untruncated\",\"path\":\"notes/decisions\",\"memory_type\":\"semantic\",\"created_at\":\"2026-02-03T04:05:06Z\"}}")
      (let ((answer (xmemo-tool "xmemo_get" (nlk:json-object "memory_id" "m7"))))
        (is (search "a decision, whole and untruncated" answer))
        (is (search "path: notes/decisions" answer))
        (is (search "created: 2026-02-03T04:05:06Z" answer)))
      (is (equal "https://xmemo.test/v1/memories/m7/explain"
                 (second (first (xmemo-asked)))))
      (is-carrying (answer (xmemo-tool "xmemo_get" (nlk:json-object)))
                   ("memory_id is required")))))

(deftest xmemo-cell-list-browses-a-path ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"))
      (xmemo-answer "{\"results\":[{\"id\":\"m1\",\"content\":\"prefers window seats\"}]}")
      (let ((answer (xmemo-tool "xmemo_list" (nlk:json-object "path" "notes"))))
        (is (search "1 memories under \"notes\"" answer))
        (is (search "m1 prefers window seats" answer)))
      (let ((request (first (xmemo-asked))))
        (is (equal "*" (xmemo-request-param request "query")) "a browse asks for every path")
        (is (equal "notes%" (xmemo-request-param request "path")) "the prefix is widened")))))

;;; --- writes ----------------------------------------------------------------------

(deftest xmemo-cell-remember-writes-a-fact-with-an-idempotency-key ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"
                                  "bucket" "personal" "scope" "nodecode/me" "agent_id" "laptop"))
      (xmemo-answer "{\"id\":\"mem-9\"}")
      (is (equal "Stored under \"notes/decisions\" as memory mem-9."
                 (xmemo-tool "xmemo_remember" (nlk:json-object "content" "ships on Fridays"
                                                                "path" "notes/decisions"
                                                                "importance" 0.8))))
      (let* ((request (first (xmemo-asked)))
             (body (xmemo-request-body request)))
        (is (eq :post (first request)))
        (is (equal "https://xmemo.test/v1/remember" (second request)))
        (is (equal "ships on Fridays" (nlk:json-value body :string "content")))
        (is (equal "notes/decisions" (nlk:json-value body :string "path")))
        (is (equal "personal" (nlk:json-value body :string "bucket")) "the section's bucket")
        (is (equal "nodecode/me" (nlk:json-value body :string "scope")))
        (is (eq t (nlk:json-value body :boolean "dedupe")))
        (let ((importance (nlk:json-value body :number "importance")))
          (is (and importance (< 0.79 importance 0.81)) "the call's importance"))
        (let ((key (nlk:json-value body :string "idempotency_key")))
          (is (stringp key))
          (is (equal key (xmemo-request-header request "idempotency-key")))
          (is (equal key (xmemo-request-header request "x-idempotency-key")))))
      (is-carrying (answer (xmemo-tool "xmemo_remember" (nlk:json-object "content" "x")))
                   ("path is required")))))

(deftest xmemo-cell-update-state-saves-the-active-task ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"))
      (xmemo-answer "{}")
      (is (search "lapses after 3600 seconds"
                  (xmemo-tool "xmemo_update_state"
                               (nlk:json-object "current_task" "port xmemo"
                                                "next_action" "run the tests"
                                                "ttl_seconds" 3600))))
      (let ((body (xmemo-request-body (first (xmemo-asked)))))
        (is (equal "https://xmemo.test/v1/update_state" (second (first (xmemo-asked)))))
        (is (equal "active_task" (nlk:json-value body :string "state_key")))
        (is (equal "port xmemo" (nlk:json-value body :string "current_task")))
        (is (equal "run the tests" (nlk:json-value body :string "next_action")))
        (is (= 3600 (nlk:json-value body :integer "ttl_seconds"))))
      (is-carrying (answer (xmemo-tool "xmemo_update_state" (nlk:json-object)))
                   ("at least one of current_task")))))

;;; --- the context pack --------------------------------------------------------------

(deftest xmemo-cell-recall-builds-a-bounded-pack ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"
                                  "max_tokens" 500))
      (xmemo-answer "{\"context_text\":\"- drinks green tea\\n- lives in Oslo\",\"items\":[]}")
      (is (search "drinks green tea"
                  (xmemo-tool "xmemo_recall_context"
                               (nlk:json-object "query" "tea" "max_items" 4))))
      (let* ((request (first (xmemo-asked)))
             (body (xmemo-request-body request)))
        (is (eq :post (first request)))
        (is (equal "https://xmemo.test/v1/recall/context" (second request)))
        (is (= 4 (nlk:json-value body :integer "max_items")))
        (is (= 500 (nlk:json-value body :integer "max_tokens")) "the section's budget")
        (is (eq t (nlk:json-value body :boolean "prefer_working")))
        (is (equal "auto" (nlk:json-value body :string "memory_type"))))
      (xmemo-answer "{\"items\":[{\"content\":\"prefers tea\",\"agent_boundary\":\"other_agent\"}]}")
      (is (search "[other_agent] prefers tea"
                  (xmemo-tool "xmemo_recall_context" (nlk:json-object "query" "tea")))))))

;;; --- /xmemo --------------------------------------------------------------------------

(deftest xmemo-cell-the-command-shows-the-backend-or-searches-it ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"
                                  "agent_id" "laptop"))
      (let ((answer (cell-entry "nodecode-xmemo" "xmemo" "")))
        (is (search "XMemo: https://xmemo.test, agent \"laptop\"" answer))
        (is (search "new memories go to work/nodecode/default" answer))
        (is (null (xmemo-asked)) "a status read asks the backend nothing"))
      (xmemo-answer "{\"results\":[{\"id\":\"m1\",\"content\":\"lives in Oslo\"}]}")
      (is (search "m1" (cell-entry "nodecode-xmemo" "xmemo" "where do I live")))
      (is (equal "where do I live"
                 (xmemo-request-param (first (xmemo-asked)) "query"))))))

;;; --- the opt-in timeline ----------------------------------------------------------------

(deftest xmemo-cell-capture-timeline-records-a-finished-turn ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"
                                  "capture_timeline" t))
      (xmemo-answer "{}")
      (cell-publish "nodecode-xmemo"
                    (cell-frame-op "turn.input_committed" :turn "t4"
                                   :payload (nlk:json-object "role" "user"
                                                              "message" "Remember that I drink green tea.")))
      (cell-publish "nodecode-xmemo" (cell-frame-op "turn.completed" :turn "t4"))
      (let* ((request (first (xmemo-asked)))
             (body (xmemo-request-body request)))
        (is (eq :post (first request)))
        (is (equal "https://xmemo.test/v1/timeline/events" (second request)))
        (is (equal "Remember that I drink green tea." (nlk:json-value body :string "content")))
        (is (equal "session_event" (nlk:json-value body :string "event_type")))
        (is (equal "s1" (nlk:json-value body :string "session_id"))))
      (is (null (nodecode-xmemo::take-turn-input "t4")) "the turn is forgotten once recorded"))))

(deftest xmemo-cell-capture-off-sends-nothing ()
  (with-xmemo-wire
    (with-cell-stop ((xmemo-start "api_key" "k-test" "base_url" "https://xmemo.test"))
      (cell-publish "nodecode-xmemo"
                    (cell-frame-op "turn.input_committed" :turn "t5"
                                   :payload (nlk:json-object "message" "transient chat")))
      (cell-publish "nodecode-xmemo" (cell-frame-op "turn.completed" :turn "t5"))
      (is (null (xmemo-asked)) "without capture_timeline the frame point is silent"))))
