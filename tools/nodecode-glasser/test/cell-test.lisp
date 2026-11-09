;;;; cell-test.lisp --- lifecycle, the seven tools, /glasser.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, an integer under its floor refused, a start with no key refused),
;;;; each tool through the seam the model calls it by -- its endpoint, its
;;;; bearer header, its JSON-RPC messages and the answer it reads back -- and
;;;; /glasser with and without an argument. Every request is a stubbed
;;;; NLK:HTTP, so nothing reaches Glasser or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values BODY STATUS HEADERS).")

(defmacro with-glasser-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Glasser."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected glasser request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun glasser-headers (&optional (content-type "application/json") (session "s-1"))
  "A response's header table, as the transport hands one over."
  (let ((headers (nlk:json-object "content-type" content-type)))
    (when session (setf (gethash "mcp-session-id" headers) session))
    headers))

(defun glasser-message (id result)
  "The JSON-RPC reply to ID carrying RESULT, as the text of one answer."
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defvar *glasser-request* nil
  "The JSON-RPC message the stubbed wire is answering, so a reply carries its own id.")

(defun glasser-reply-id ()
  "The id of the request being answered: a reply echoes it, as a server does."
  (nlk:json-value *glasser-request* :any "id"))

(defun glasser-initialized ()
  "What an initialize answers, for the request under way."
  (glasser-message (glasser-reply-id)
                    (nlk:json-object "protocolVersion" "2025-06-18"
                                      "serverInfo" (nlk:json-object "name" "glasser"))))

(defun glasser-tool-result (text &key (is-error nil))
  "A tools/call result holding TEXT, as one SSE frame."
  (format nil "event: message~%data: ~a~%~%"
          (glasser-message (glasser-reply-id) (nlk:json-object
                              "content" (vector (nlk:json-object "type" "text" "text" text))
                              "isError" is-error))))

(defmacro with-glasser-answers ((&rest routes) &body body)
  "BODY with the stubbed wire answering ROUTES, (FRAGMENT FORM): the first
whose FRAGMENT the request's body holds answers FORM's (values BODY STATUS
HEADERS); a body no route names is an error."
  `(with-glasser-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore method))
                           (let* ((body (getf args :content))
                                  (*glasser-request* (ignore-errors (nlk:decode-json body))))
                             (cond ,@(loop for (fragment form) in routes
                                           collect `((search ,fragment body) ,form))
                                   (t (error "unexpected request to ~a: ~a" url body)))))))
       (with-cell-stop ((glasser-start "api_key" "g-key"))
         ,@body))))

(defun glasser-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun glasser-request-body (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun glasser-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun glasser-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "glasser" "GLASSER-CELL-" :start nodecode-glasser:start-cell)

(define-cell-lifecycle-tests "glasser"
  (:fixture with-glasser-wire)
  (:config "api_key" "g-key")
  (:help :glasser)
  (:command "glasser")
  (:running (is (nle::find-tool "glasser_search") "glasser_search is registered")
            (is (nle::find-tool "glasser_inspect"))
            (is (nle::find-tool "glasser_run"))
            (is (nle::find-tool "glasser_runs_get"))
            (is (nle::find-tool "glasser_runs_list"))
            (is (nle::find-tool "glasser_runs_stop"))
            (is (nle::find-tool "glasser_balance") "all seven tools are registered")
            (is (equal "https://api.glasser.ai/mcp" (nodecode-glasser::endpoint)))
            (is (= 60 (nodecode-glasser::setting :timeout-seconds))))
  (:stopped (is (null (nle::find-tool "glasser_balance")) "stop unregisters the tools"))
  (:refused ("timeout_seconds" 4))
  (:idle glasser:glasser-error (glasser:call "balance")))

(deftest glasser-cell-a-key-is-required ()
  (with-glasser-wire
    (with-stubbed-fdefinition (uiop:getenv (name) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (glasser-start)))
                   ("glasser: no key" "GLASSER_API_KEY"))
      (is (null (nle::find-tool "glasser_search")) "a refused start installed nothing"))))

(deftest glasser-cell-a-key-may-come-from-a-variable ()
  (with-glasser-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (if (equal name "GLASSER_API_KEY") "env-key" nil))
      (with-cell-stop ((glasser-start))
        (is (equal "env-key" (nodecode-glasser::api-key)) "a key exported in a shell needs no section")
        (is (search "GLASSER_API_KEY" (nodecode-glasser::status)))))))

;;; --- /glasser -----------------------------------------------------------------

(deftest glasser-cell-says-which-key-it-uses ()
  (with-glasser-answers ()
    (let ((text (cell-entry "nodecode-glasser" "glasser" "")))
      (is (search "glasser:" text))
      (is (search "the section's api_key" text))
      (is (search "https://api.glasser.ai/mcp" text) "the endpoint it talks to")
      (is (search "published price" text) "and that a call costs"))))

;;; --- the wire ----------------------------------------------------------------

(deftest glasser-cell-asks-the-server-for-the-balance ()
  (with-glasser-answers (("\"method\":\"initialize\"" (values (glasser-initialized) 200 (glasser-headers)))
                         ("notifications/initialized" (values "" 202 (glasser-headers)))
                         ("tools/call" (values (glasser-tool-result "available 12.00 USD, held 0.40") 200
                                               (glasser-headers "text/event-stream"))))
    (is (search "available 12.00 USD" (glasser-tool "glasser_balance" (nlk:json-object))))
    (let ((asked (glasser-asked)))
      (is (= 3 (length asked)) "one session: initialize, the notification, the call")
      (destructuring-bind (method url . args) (first asked)
        (declare (ignore args))
        (is (eq :post method))
        (is (equal "https://api.glasser.ai/mcp" url) "the endpoint the original's manifest names")
        (is (equal "Bearer g-key" (glasser-request-header (first asked) "authorization")) "the key rides as a bearer, never in the url")
        (is (search "text/event-stream" (glasser-request-header (first asked) "accept")))
        (is (null (glasser-request-header (first asked) "mcp-session-id")) "no session before initialize")
        (let ((body (glasser-request-body (first asked))))
          (is (equal "initialize" (nlk:json-value body :string "method")))
          (is (equal "2025-06-18" (nlk:json-value body :string "params" "protocolVersion")))))
      (is (equal "notifications/initialized"
                 (nlk:json-value (glasser-request-body (second asked)) :string "method")))
      (is (equal "s-1" (glasser-request-header (second asked) "mcp-session-id")) "the session the server issued")
      (let* ((request (third asked))
             (body (glasser-request-body request)))
        (is (equal "tools/call" (nlk:json-value body :string "method")))
        (is (equal "balance" (nlk:json-value body :string "params" "name")))
        (is (hash-table-p (nlk:json-value body :object "params" "arguments")))
        (is (equal "s-1" (glasser-request-header request "mcp-session-id")))))))

(deftest glasser-cell-runs-an-endpoint-with-an-idempotency-key ()
  (with-glasser-answers (("\"method\":\"initialize\"" (values (glasser-initialized) 200 (glasser-headers)))
                         ("notifications/initialized" (values "" 202 (glasser-headers)))
                         ("tools/call" (values (glasser-tool-result "run r-1 COMPLETED, charge_usd 0.05") 200
                                               (glasser-headers "text/event-stream"))))
    (is (search "charge_usd 0.05"
                (glasser-tool "glasser_run" (nlk:json-object "endpoint" "seo.keywords"
                                                              "input" (nlk:json-object "query" "ai agents")))))
    (let* ((body (glasser-request-body (third (glasser-asked))))
           (arguments (nlk:json-value body :object "params" "arguments"))
           (key (nlk:json-value arguments :string "idempotency_key")))
      (is (equal "seo.keywords" (nlk:json-value arguments :string "endpoint")))
      (is (equal "ai agents" (nlk:json-value arguments :string "input" "query")) "the input rides as an object")
      (is (and key (stringp key) (= 36 (length key))) "a UUID was made when the model left one out")))
  ;; a key the caller gives is the one sent, so a retry replays the first run
  (with-glasser-answers (("\"method\":\"initialize\"" (values (glasser-initialized) 200 (glasser-headers)))
                         ("notifications/initialized" (values "" 202 (glasser-headers)))
                         ("tools/call" (values (glasser-tool-result "replayed") 200
                                               (glasser-headers "text/event-stream"))))
    (glasser-tool "glasser_run" (nlk:json-object "endpoint" "seo.keywords" "idempotency_key" "k-1"))
    (is (equal "k-1" (nlk:json-value (glasser-request-body (third (glasser-asked)))
                                      :string "params" "arguments" "idempotency_key")))))

(deftest glasser-cell-reads-a-refusal-the-server-reports ()
  (with-glasser-answers (("\"method\":\"initialize\"" (values (glasser-initialized) 200 (glasser-headers)))
                         ("notifications/initialized" (values "" 202 (glasser-headers)))
                         ("tools/call" (values (glasser-tool-result "insufficient_balance: 0.12 USD available"
                                                                    :is-error t)
                                               200 (glasser-headers "text/event-stream"))))
    (let ((answer (glasser-tool "glasser_balance" (nlk:json-object))))
      (is (search "ERROR: insufficient_balance" answer) "a refusal is the answer's text, prefixed"))))

(deftest glasser-cell-says-a-refused-key ()
  (with-glasser-answers (("initialize" (values (glasser-message 1 (nlk:json-object))
                                               401 (glasser-headers))))
    (let ((answer (glasser-tool "glasser_balance" (nlk:json-object))))
      (is (search "ERROR: glasser:" answer))
      (is (search "HTTP 401" answer))
      (is (search "app.glasser.ai/keys" answer) "and where a new one is made"))))
