;;;; cell-test.lisp --- lifecycle, the tools over a stubbed wire, and /innernet.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor refused, a start with no token
;;;; refused), the MCP handshake and one tools/call through the seam the
;;;; model calls a tool by -- their URL, their headers, their JSON-RPC body
;;;; and the answer rendered back -- the two shapes a server answers in (JSON
;;;; and an event stream), the two ways a call fails, and /innernet with and
;;;; without a project. Every request is a stubbed NLK:HTTP, so nothing
;;;; reaches innernet or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values TEXT STATUS HEADERS).")

(defmacro with-innernet-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials innernet."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected innernet request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun innernet-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun innernet-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun innernet-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun innernet-request-method (request)
  (nlk:json-value (innernet-request-body request) :string "method"))

(defun innernet-answer (text &optional (status 200) headers)
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status headers))))

(defparameter +handshake+
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"serverInfo\":{\"name\":\"innernet\"}}}"
  "What the stub answers the initialize request: the protocol version it takes.")

(defun innernet-server (&key text (status 200) headers (session "s-1"))
  "A stub endpoint: the handshake answers +HANDSHAKE+ and hands back SESSION,
the ready notification a 202, and every tools/call TEXT at STATUS."
  (setf *answer-fn*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((object (and (getf args :content) (nlk:decode-json (getf args :content))))
                 (rpc-method (nlk:json-value object :string "method")))
            (cond ((equal rpc-method "initialize")
                   (values +handshake+ 200 (and session (list (cons "mcp-session-id" session)))))
                  ((equal rpc-method "notifications/initialized") (values "" 202 nil))
                  (t (values text status headers)))))))

(defun result-text (text)
  "The JSON-RPC answer a server sends for TEXT content."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 2
                     "result" (nlk:json-object
                               "content" (vector (nlk:json-object "type" "text" "text" text))))))

(defun innernet-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun innernet-configuration ()
  (list "token" "t-test" "base_url" "https://innernet.test/api/mcp"))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "innernet" "INNERNET-CELL-" :start nodecode-innernet:start-cell)

(define-cell-lifecycle-tests "innernet"
  (:fixture with-innernet-wire)
  (:config "token" "t-test" "base_url" "https://innernet.test/api/mcp")
  (:help :innernet)
  (:command "innernet")
  (:running (is (nle::find-tool "innernet_list_projects") "innernet_list_projects is registered")
            (is (nle::find-tool "innernet_load_project"))
            (is (nle::find-tool "innernet_context"))
            (is (nle::find-tool "innernet_handoff_write"))
            (is (nle::find-tool "search"))
            (is (nle::find-tool "fetch"))
            (is (equal "https://innernet.test/api/mcp" (nodecode-innernet::root)))
            (is (null nodecode-innernet::*session*) "no session before the first call"))
  (:stopped (is (null (nle::find-tool "innernet_list_projects")) "stop unregisters the tools")
            (is (null nodecode-innernet::*session*) "and forgets the session"))
  (:refused ("timeout_seconds" 0))
  (:idle nodecode-innernet:innernet-error (nodecode-innernet::run-command "" nil)))

(deftest innernet-cell-a-token-is-required ()
  (with-innernet-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal
                                         (innernet-start "base_url" "https://innernet.test/api/mcp")))
                   ("needs a sign-in token" "INNERNET_TOKEN"))
      (is (null (nle::find-tool "innernet_list_projects")) "a refused start installed nothing"))))

;;; --- the handshake and one call -----------------------------------------------

(deftest innernet-cell-a-tool-call-goes-through-the-handshake ()
  (with-innernet-wire
    (with-cell-stop ((innernet-start "token" "t-test" "base_url" "https://innernet.test/api/mcp"))
      (innernet-server :text (result-text "No projects yet."))
      (is (equal "No projects yet."
                 (innernet-tool "innernet_list_projects" (nlk:make-json-object))))
      (let ((asked (innernet-asked)))
        (is (= 3 (length asked)) "initialize, the ready notification, then the call")
        (is (equal '("initialize" "notifications/initialized" "tools/call")
                   (mapcar #'innernet-request-method asked)))
        (dolist (request asked)
          (is (equal "https://innernet.test/api/mcp" (second request)) "one endpoint only")
          (is (equal "Bearer t-test" (innernet-request-header request "authorization")))
          (is (search "text/event-stream" (innernet-request-header request "accept")) "streamable"))
        (let ((call (third asked)))
          (is (equal "s-1" (innernet-request-header call "mcp-session-id")) "the session the handshake gave")
          (let ((body (innernet-request-body call)))
            (is (equal "innernet_list_projects" (nlk:json-value body :string "params" "name")))
            (is (hash-table-p (nlk:json-value body :object "params" "arguments")) "an arguments object")))))))

(deftest innernet-cell-a-call-carries-its-arguments-and-reuses-the-session ()
  (with-innernet-wire
    (with-cell-stop ((innernet-start "token" "t-test" "base_url" "https://innernet.test/api/mcp"))
      (innernet-server :text (result-text "Saved."))
      (is (equal "Saved."
                 (innernet-tool "innernet_save_context"
                                 (nlk:json-object "slug" "pricing" "content" "we ship on Fridays"))))
      (is (equal "Saved."
                 (innernet-tool "innernet_handoff_read" (nlk:json-object "project_slug" "pricing"))))
      (let ((asked (innernet-asked)))
        (is (= 4 (length asked)) "one handshake for the run, one request per call")
        (let ((body (innernet-request-body (third asked))))
          (is (equal "pricing" (nlk:json-value body :string "params" "arguments" "slug")))
          (is (equal "we ship on Fridays"
                     (nlk:json-value body :string "params" "arguments" "content"))))
        (is (equal "pricing"
                   (nlk:json-value (innernet-request-body (fourth asked))
                                   :string "params" "arguments" "project_slug")))))))

(deftest innernet-cell-an-event-stream-is-read-to-its-last-message ()
  (with-innernet-wire
    (with-cell-stop ((innernet-start "token" "t-test" "base_url" "https://innernet.test/api/mcp"))
      (innernet-server
       :text (format nil "event: message~%data: ~a~%~%" (result-text "Where you left off: the pricing page.")))
      (is (equal "Where you left off: the pricing page."
                 (innernet-tool "innernet_handoff_read" (nlk:json-object "project_slug" "pricing")))))))

;;; --- what a failure looks like ------------------------------------------------

(deftest innernet-cell-a-refused-call-is-a-repair-not-a-crash ()
  (with-innernet-wire
    (with-cell-stop ((innernet-start "token" "t-test" "base_url" "https://innernet.test/api/mcp"))
      (innernet-server :text "{\"message\":\"token expired\"}" :status 401)
      (is-carrying (answer (innernet-tool "innernet_save_context"
                                           (nlk:json-object "slug" "pricing" "content" "x")))
                   ("ERROR: innernet:" "HTTP 401") ("token expired"))
      (innernet-server :text "{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32602,\"message\":\"unknown tool\"}}")
      (is-carrying (answer (innernet-tool "innernet_rooms" (nlk:make-json-object)))
                   ("ERROR: innernet:" "innernet_rooms was refused") ("unknown tool"))
      (innernet-server :text (nlk:encode-json-object
                               (nlk:json-object "jsonrpc" "2.0" "id" 2
                                                "result" (nlk:json-object
                                                          "isError" t
                                                          "content" (vector (nlk:json-object
                                                                             "type" "text"
                                                                             "text" "no project called that"))))))
      (is-carrying (answer (innernet-tool "innernet_load_project" (nlk:json-object "slug" "nope")))
                   ("ERROR: innernet:" "no project called that")))))

(deftest innernet-cell-a-forgotten-session-is-renewed-once ()
  (with-innernet-wire
    (with-cell-stop ((innernet-start "token" "t-test" "base_url" "https://innernet.test/api/mcp"))
      (let ((calls 0))
        (setf *answer-fn*
              (lambda (method url args)
                (declare (ignore method url))
                (let* ((object (nlk:decode-json (getf args :content)))
                       (rpc-method (nlk:json-value object :string "method")))
                  (cond ((equal rpc-method "initialize")
                         (incf calls)
                         (values +handshake+ 200 (list (cons "mcp-session-id" (format nil "s-~d" calls)))))
                        ((equal rpc-method "notifications/initialized") (values "" 202 nil))
                        ((= calls 1) (values "{\"message\":\"session gone\"}" 404 nil))
                        (t (values (result-text "Reloaded.") 200 nil))))))
        (is (equal "Reloaded." (innernet-tool "innernet_list_projects" (nlk:make-json-object))))
        (is (= 2 calls) "one more handshake, never a loop")
        (is (= 6 (length (innernet-asked))) "two handshakes, two notifications, two calls")))))

;;; --- /innernet ------------------------------------------------------------------

(deftest innernet-cell-the-command-shows-the-connection-or-loads-a-project ()
  (with-innernet-wire
    (with-cell-stop ((innernet-start "token" "t-test" "base_url" "https://innernet.test/api/mcp"))
      (let ((answer (cell-entry "nodecode-innernet" "innernet" "")))
        (is (search "innernet: https://innernet.test/api/mcp" answer))
        (is (search "a token is set" answer))
        (is (null (innernet-asked)) "a status read dials nothing"))
      (innernet-server :text (result-text "Pricing: one line per dimension."))
      (is (search "one line per dimension"
                  (cell-entry "nodecode-innernet" "innernet" "pricing")))
      (is (equal "pricing"
                 (nlk:json-value (innernet-request-body (car (last (innernet-asked))))
                                 :string "params" "arguments" "slug"))))))
