;;;; cell-test.lisp --- lifecycle, the three tools, /gurusup-brain.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor refused, a start with no token
;;;; refused), a token that may come from a variable, each tool through the
;;;; seam the model calls it by -- its endpoint, its bearer header, its
;;;; JSON-RPC messages and the answer it reads back -- and /gurusup-brain.
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Brain or the
;;;; network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values BODY STATUS HEADERS).")

(defmacro with-gurusup-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Brain."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected gurusup-brain request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun gurusup-headers (&optional (content-type "application/json") (session "s-1"))
  "A response's header table, as the transport hands one over."
  (let ((headers (nlk:json-object "content-type" content-type)))
    (when session (setf (gethash "mcp-session-id" headers) session))
    headers))

(defun gurusup-message (id result)
  "The JSON-RPC reply to ID carrying RESULT, as the text of one answer."
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defvar *gurusup-request* nil
  "The JSON-RPC message the stubbed wire is answering, so a reply carries
its own id.")

(defun gurusup-reply-id ()
  "The id of the request being answered: a reply echoes it, as a server does."
  (nlk:json-value *gurusup-request* :any "id"))

(defun gurusup-initialized ()
  "What an initialize answers, for the request under way."
  (gurusup-message (gurusup-reply-id)
                    (nlk:json-object "protocolVersion" "2025-06-18"
                                      "serverInfo" (nlk:json-object "name" "brain"))))

(defun gurusup-tool-object (name description)
  (nlk:json-object "name" name "description" description
                    "inputSchema" (nlk:json-object "type" "object")))

(defun gurusup-tools-result (&rest tools)
  "A tools/list result holding TOOLS, as one SSE frame."
  (format nil "event: message~%data: ~a~%~%"
          (gurusup-message (gurusup-reply-id)
                            (nlk:json-object "tools" (coerce tools 'vector)))))

(defun gurusup-tool-result (text &key (is-error nil))
  "A tools/call result holding TEXT, as one SSE frame."
  (format nil "event: message~%data: ~a~%~%"
          (gurusup-message (gurusup-reply-id) (nlk:json-object
                              "content" (vector (nlk:json-object "type" "text" "text" text))
                              "isError" is-error))))

(defmacro with-gurusup-answers ((&rest routes) &body body)
  "BODY with the stubbed wire answering ROUTES, (FRAGMENT FORM): the first
whose FRAGMENT the request's body holds answers FORM's (values BODY STATUS
HEADERS); a body no route names is an error."
  `(with-gurusup-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore method))
                           (let* ((body (getf args :content))
                                  (*gurusup-request* (ignore-errors (nlk:decode-json body))))
                             (cond ,@(loop for (fragment form) in routes
                                           collect `((search ,fragment body) ,form))
                                   (t (error "unexpected request to ~a: ~a" url body)))))))
       (with-cell-stop ((gurusup-brain-start "token" "b-token"))
         ,@body))))

(defun gurusup-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun gurusup-request-body (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun gurusup-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun gurusup-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "gurusup-brain" "GURUSUP-BRAIN-CELL-"
  :start nodecode-gurusup-brain:start-cell)

(define-cell-lifecycle-tests "gurusup-brain"
  (:fixture with-gurusup-wire)
  (:config "token" "b-token")
  (:help :gurusup-brain)
  (:command "gurusup-brain")
  (:running (is (nle::find-tool "gurusup_brain_init") "gurusup_brain_init is registered")
            (is (nle::find-tool "gurusup_brain_tools"))
            (is (nle::find-tool "gurusup_brain_call") "all three tools are registered")
            (is (equal "https://mcp.brain.gurusup.com/mcp" (nodecode-gurusup-brain::endpoint)))
            (is (= 45 (nodecode-gurusup-brain::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "gurusup_brain_call")) "stop unregisters the tools"))
  (:refused ("timeout_seconds" 4))
  (:idle gurusup-brain:gurusup-brain-error (gurusup-brain:call "search")))

(deftest gurusup-brain-cell-a-token-is-required ()
  (with-gurusup-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (progn name nil))
      (is-carrying (answer (refusal-text nlk:config-refusal (gurusup-brain-start)))
        "gurusup-brain: no token"
        "gurusup.com/brain/mcp"
        "GURUSUP_BRAIN_TOKEN")
      (is (null (nle::find-tool "gurusup_brain_init")) "a refused start installed nothing"))))

(deftest gurusup-brain-cell-a-token-may-come-from-a-variable ()
  (with-gurusup-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                                (if (equal name "GURUSUP_BRAIN_TOKEN") "env-token" nil))
      (with-cell-stop ((gurusup-brain-start))
        (is (equal "env-token" (nodecode-gurusup-brain::token)) "a token exported in a shell needs no section")
        (is (search "GURUSUP_BRAIN_TOKEN" (nodecode-gurusup-brain::status)))))))

;;; --- /gurusup-brain -----------------------------------------------------------

(deftest gurusup-brain-cell-says-which-token-it-uses ()
  (with-gurusup-answers ()
    (let ((text (cell-entry "nodecode-gurusup-brain" "gurusup-brain" "")))
      (is (search "gurusup-brain:" text))
      (is (search "the section's token" text))
      (is (search "https://mcp.brain.gurusup.com/mcp" text) "the endpoint it talks to"))))

;;; --- the wire -----------------------------------------------------------------

(deftest gurusup-brain-cell-asks-for-the-init-contract ()
  (with-gurusup-answers (("\"method\":\"initialize\"" (values (gurusup-initialized) 200
                                                             (gurusup-headers)))
                         ("notifications/initialized" (values "" 202 (gurusup-headers)))
                         ("tools/list" (values (gurusup-tools-result
                                                (gurusup-tool-object "init" "the contract")
                                                (gurusup-tool-object "search" "find decisions and people"))
                                               200 (gurusup-headers "text/event-stream")))
                         ("tools/call" (values (gurusup-tool-result
                                                "Brain contract: ask with the source behind each answer")
                                               200 (gurusup-headers "text/event-stream"))))
    (is (search "Brain contract" (gurusup-tool "gurusup_brain_init" (nlk:json-object))) "the init tool's text is the answer")
    (let ((asked (gurusup-asked)))
      (is (= 4 (length asked)) "one session: initialize, its notification, tools/list, the call")
      (destructuring-bind (method url . args) (first asked)
        (declare (ignore args))
        (is (eq :post method))
        (is (equal "https://mcp.brain.gurusup.com/mcp" url) "the endpoint the original's manifest names")
        (is (equal "Bearer b-token" (gurusup-request-header (first asked) "authorization")) "the token rides as a bearer, never in the url")
        (is (search "text/event-stream" (gurusup-request-header (first asked) "accept")))
        (is (null (gurusup-request-header (first asked) "mcp-session-id")) "no session before initialize")
        (let ((body (gurusup-request-body (first asked))))
          (is (equal "initialize" (nlk:json-value body :string "method")))
          (is (equal "2025-06-18" (nlk:json-value body :string "params" "protocolVersion")))))
      (is (equal "notifications/initialized"
                 (nlk:json-value (gurusup-request-body (second asked)) :string "method")))
      (is (equal "s-1" (gurusup-request-header (second asked) "mcp-session-id")) "the session the server issued")
      (let* ((request (fourth asked))
             (body (gurusup-request-body request)))
        (is (equal "tools/call" (nlk:json-value body :string "method")))
        (is (equal "init" (nlk:json-value body :string "params" "name")) "the tool the skill names")
        (is (equal "s-1" (gurusup-request-header request "mcp-session-id")))))))

(deftest gurusup-brain-cell-lists-what-the-account-may-call ()
  (with-gurusup-answers (("\"method\":\"initialize\"" (values (gurusup-initialized) 200
                                                             (gurusup-headers)))
                         ("notifications/initialized" (values "" 202 (gurusup-headers)))
                         ("tools/list" (values (gurusup-tools-result
                                                (gurusup-tool-object "init" "the contract")
                                                (gurusup-tool-object "search"
                                                                      "find decisions and customers"))
                                               200 (gurusup-headers "text/event-stream"))))
    (let ((answer (gurusup-tool "gurusup_brain_tools" (nlk:json-object))))
      (is (search "init - the contract" answer) "a tool a line, with what it is for")
      (is (search "search - find decisions and customers" answer)))))

(deftest gurusup-brain-cell-passes-a-tools-arguments ()
  (with-gurusup-answers (("\"method\":\"initialize\"" (values (gurusup-initialized) 200
                                                             (gurusup-headers)))
                         ("notifications/initialized" (values "" 202 (gurusup-headers)))
                         ("tools/call" (values (gurusup-tool-result
                                                "Acme renewed in March (Slack #acme)")
                                               200 (gurusup-headers "text/event-stream"))))
    (is (search "renewed in March"
                (gurusup-tool "gurusup_brain_call"
                               (nlk:json-object "tool" "search"
                                                "arguments" (nlk:json-object "query" "acme")))))
    (let* ((body (gurusup-request-body (third (gurusup-asked))))
           (arguments (nlk:json-value body :object "params" "arguments")))
      (is (equal "search" (nlk:json-value body :string "params" "name")))
      (is (equal "acme" (nlk:json-value arguments :string "query")) "the arguments ride as the object the tool takes")))
  ;; an arguments member sent as JSON text is read as the same object
  (with-gurusup-answers (("\"method\":\"initialize\"" (values (gurusup-initialized) 200
                                                             (gurusup-headers)))
                         ("notifications/initialized" (values "" 202 (gurusup-headers)))
                         ("tools/call" (values (gurusup-tool-result "ok") 200
                                               (gurusup-headers "text/event-stream"))))
    (gurusup-tool "gurusup_brain_call"
                   (nlk:json-object "tool" "search" "arguments" "{\"query\":\"acme\"}"))
    (is (equal "acme" (nlk:json-value (gurusup-request-body (third (gurusup-asked)))
                                       :string "params" "arguments" "query")))))

(deftest gurusup-brain-cell-reads-a-refusal-the-server-reports ()
  (with-gurusup-answers (("\"method\":\"initialize\"" (values (gurusup-initialized) 200
                                                             (gurusup-headers)))
                         ("notifications/initialized" (values "" 202 (gurusup-headers)))
                         ("tools/call" (values (gurusup-tool-result
                                                "no evidence in the connected sources"
                                                :is-error t)
                                               200 (gurusup-headers "text/event-stream"))))
    (let ((answer (gurusup-tool "gurusup_brain_call" (nlk:json-object "tool" "search"))))
      (is (search "ERROR: no evidence" answer) "a refusal is the answer's text, prefixed"))))

(deftest gurusup-brain-cell-says-a-refused-token ()
  (with-gurusup-answers (("initialize" (values (gurusup-message 1 (nlk:json-object))
                                               401 (gurusup-headers))))
    (let ((answer (gurusup-tool "gurusup_brain_tools" (nlk:json-object))))
      (is (search "ERROR: gurusup-brain:" answer))
      (is (search "HTTP 401" answer))
      (is (search "gurusup.com/brain/mcp" answer) "and where a token is issued"))))
