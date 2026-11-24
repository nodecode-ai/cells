;;;; cell-test.lisp --- lifecycle, the eight tools, the wire, /makernice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a field refused, a start with no token refused), the MCP handshake
;;;; it runs before its first tool call, what each tool sends the server and
;;;; what it hands back -- through the seam the model calls it by -- the
;;;; consent gate generate enforces locally, an event-stream answer, a refused
;;;; token and a forgotten session, and /makernice. Every request is a stubbed
;;;; NLK:HTTP, so nothing reaches Makernice.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS =>
(values BODY STATUS HEADERS).")

(defvar *tool-answer* nil
  "What the stubbed server answers a tools/call: a function of TOOL and
ARGUMENTS => (values TEXT ERROR-P).")

(defvar *events* nil
  "Whether the stubbed server answers a tool call as an event stream.")

(defmacro with-mk-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected makernice request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun mk-headers (&rest pairs)
  "A response header table, as dexador hands one back."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name . value) in pairs do (setf (gethash name table) value))
    table))

(defun mk-message (args)
  "The JSON-RPC message the stubbed request ARGS carried."
  (nlk:decode-json (or (getf args :content) "{}")))

(defun mk-json (value)
  (nlk:encode-json-object value))

(defun mk-answer (message text &key (is-error nil) (events nil))
  "TEXT as the reply a stubbed server makes to MESSAGE."
  (let ((body (mk-json (nlk:json-object
                         "jsonrpc" "2.0"
                         "id" (gethash "id" message)
                         "result" (nlk:json-object
                                   "content" (vector (nlk:json-object "type" "text" "text" text))
                                   "isError" (if is-error t nil))))))
    (if events
        (values (format nil "event: message~%data: ~a~%~%" body) 200
                (mk-headers (cons "content-type" "text/event-stream")))
        (values body 200 (mk-headers (cons "content-type" "application/json"))))))

(defun mk-server-answer (method url args)
  "The protocol a stubbed Makernice server speaks, whatever the test answers
a tool call with."
  (declare (ignore method url))
  (let* ((message (mk-message args))
         (name (nlk:json-value message :string "method"))
         (id (gethash "id" message)))
    (cond
      ((equal name "initialize")
       (values (mk-json (nlk:json-object
                          "jsonrpc" "2.0" "id" id
                          "result" (nlk:json-object "protocolVersion" "2025-06-18"
                                                     "capabilities" (nlk:json-object)
                                                     "serverInfo" (nlk:json-object "name" "makernice" "version" "0.2.0"))))
               200 (mk-headers (cons "content-type" "application/json")
                               (cons "mcp-session-id" "sess-1"))))
      ((equal name "notifications/initialized")
       (values "" 202 (mk-headers (cons "content-type" "application/json"))))
      ((equal name "tools/call")
       (multiple-value-bind (text error-p)
           (funcall *tool-answer* (nlk:json-value message :string "params" "name")
                    (nlk:json-value message :object "params" "arguments"))
         (mk-answer message text :is-error error-p :events *events*)))
      (t (error "unexpected ~a" name)))))

(defmacro with-mk-server ((&key answer) &body body)
  "BODY with the cell started on a test token and a stubbed Makernice server
answering ANSWER -- a function of TOOL and ARGUMENTS => (values TEXT ERROR-P)
-- or \"ok\" for every call."
  `(with-mk-wire
     (let ((*answer-fn* #'mk-server-answer)
           (*events* nil)
           (*tool-answer* (or ,answer
                              (lambda (tool arguments)
                                (declare (ignore tool arguments))
                                (values "ok" nil)))))
       (with-cell-stop ((makernice-start "token" "test-token"))
         ,@body))))

(defun mk-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun mk-last-message ()
  "The JSON-RPC message the last stubbed request carried."
  (mk-message (cddr (first *requests*))))

(defun mk-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun mk-initialize-count ()
  "How many handshakes the stubbed server was asked for."
  (count "initialize" (mk-asked)
         :key (lambda (request) (nlk:json-value (mk-message (cddr request)) :string "method"))
         :test #'equal))

(defun mk-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "makernice" "MAKERNICE-CELL-" :start nodecode-makernice:start-cell)

(define-cell-lifecycle-tests "makernice"
  (:fixture with-mk-wire)
  (:config "token" "test-token")
  (:help :makernice)
  (:command "makernice")
  (:running (is (nle::find-tool "makernice_generate") "the generate tool is registered")
            (is (nle::find-tool "makernice_estimate_cost") "and estimate_cost")
            (is (equal "https://mcp.makernice.com/mcp"
                       (nodecode-makernice::endpoint)) "the server the plugin's mcp.json names")
            (is (equal "test-token" (nodecode-makernice::token)) "the section's token"))
  (:stopped (is (null (nle::find-tool "makernice_generate")) "stop unregisters the tool"))
  (:refused ("request_timeout" 0) ("url" 5))
  (:idle makernice:makernice-error (makernice:call "get_balance") (makernice:account)))

(deftest makernice-cell-a-token-is-required ()
  (with-mk-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (makernice-start)))
        ("makernice: no token") ("MAKERNICE_TOKEN") ("token"))
      (is (null (nle::find-tool "makernice_get_balance")) "a refused start installed nothing"))))

(deftest makernice-cell-a-token-in-the-environment-is-enough ()
  (with-mk-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                                (if (equal name "MAKERNICE_TOKEN") "env-token" nil))
      (with-cell-stop ((makernice-start))
        (is (equal "env-token" (nodecode-makernice::token)) "the variable token_env names")
        (is (nle::find-tool "makernice_estimate_cost") "the tools are up")))))

;;; --- the handshake, and the tools ----------------------------------------------

(deftest makernice-cell-get-balance-speaks-the-protocol ()
  (with-mk-server (:answer (lambda (tool arguments)
                              (declare (ignore arguments))
                              (values (format nil "{\"tool\":\"~a\",\"available_usd\":1.5}" tool) nil)))
    (let ((text (mk-tool "makernice_get_balance" (nlk:json-object))))
      (is (search "available_usd" text) "the server's text is the answer"))
    (let ((asks (mk-asked)))
      (is (= 3 (length asks)) "initialize, its notification, then the call")
      (destructuring-bind (method url . args) (first asks)
        (declare (ignore args))
        (is (eq :post method))
        (is (equal "https://mcp.makernice.com/mcp" url))
        (is (equal "Bearer test-token" (mk-request-header (first asks) "authorization")))
        (is (search "text/event-stream" (mk-request-header (first asks) "accept")) "an event stream is accepted")
        (is (equal "2025-06-18" (mk-request-header (first asks) "mcp-protocol-version"))))
      (is (equal "initialize" (nlk:json-value (mk-message (cddr (first asks))) :string "method")))
      (is (equal "notifications/initialized"
                 (nlk:json-value (mk-message (cddr (second asks))) :string "method")))
      (is (equal "sess-1" (mk-request-header (third asks) "mcp-session-id")) "the session the handshake issued")
      (let ((body (mk-message (cddr (third asks)))))
        (is (equal "tools/call" (nlk:json-value body :string "method")))
        (is (equal "get_balance" (nlk:json-value body :string "params" "name")))))))

(deftest makernice-cell-estimate-cost-passes-the-parameters-through ()
  (with-mk-server (:answer (lambda (tool arguments)
                              (declare (ignore tool))
                              (values (format nil "{\"amount_usd\":0.35,\"model\":\"~a\"}"
                                              (nlk:json-value arguments :string "input" "model"))
                                      nil)))
    (let ((text (mk-tool "makernice_estimate_cost"
                          (nlk:json-object
                           "input" (nlk:json-object "model" "seedance" "mode" "t2v"
                                                     "prompt" "a paper boat")))))
      (is (search "0.35" text) "the price the server answered")
      (is (search "seedance" text)))
    (let ((body (mk-last-message)))
      (is (equal "estimate_cost" (nlk:json-value body :string "params" "name")))
      (let ((input (nlk:json-value body :object "params" "arguments" "input")))
        (is (equal "seedance" (nlk:json-value input :string "model")) "the parameters ride unchanged")
        (is (equal "t2v" (nlk:json-value input :string "mode")))))))

(deftest makernice-cell-estimate-cost-needs-the-input-object ()
  (with-mk-server ()
    (is-carrying (answer (mk-tool "makernice_estimate_cost" (nlk:json-object)))
      ("ERROR: makernice") ("input is required"))
    (is (null (mk-asked)) "nothing was sent")))

(deftest makernice-cell-generate-needs-the-approved-price-and-an-id ()
  (with-mk-server ()
    (is-carrying (answer (mk-tool "makernice_generate"
                           (nlk:json-object "input" (nlk:json-object "model" "m" "mode" "t2v" "prompt" "p"))))
      ("ERROR: makernice") ("max_cost_usd is required") ("estimate_cost"))
    (is-carrying (answer (mk-tool "makernice_generate"
                           (nlk:json-object "input" (nlk:json-object "model" "m" "mode" "t2v" "prompt" "p")
                                            "max_cost_usd" 2 "request_id" "has a space")))
      ("ERROR: makernice") ("request_id must be"))
    (is (null (mk-asked)) "a job is never started without the price the user agreed to")))

(deftest makernice-cell-generate-carries-the-price-and-a-stable-id ()
  (with-mk-server (:answer (lambda (tool arguments)
                              (declare (ignore tool))
                              (values (format nil "{\"job_id\":\"job-~a\"}"
                                              (nlk:json-value arguments :string "request_id"))
                                      nil)))
    (let ((text (mk-tool "makernice_generate"
                          (nlk:json-object "input" (nlk:json-object "model" "m" "mode" "t2v" "prompt" "p")
                                           "max_cost_usd" 2 "request_id" "setup-test-1"))))
      (is (search "job-setup-test-1" text) "the job id the server answered"))
    (let ((body (mk-last-message)))
      (is (equal "generate" (nlk:json-value body :string "params" "name")))
      (is (equal "setup-test-1" (nlk:json-value body :string "params" "arguments" "request_id")))
      (is (= 2 (nlk:json-value body :integer "params" "arguments" "max_cost_usd")))
      (is (equal "p" (nlk:json-value body :string "params" "arguments" "input" "prompt"))))))

(deftest makernice-cell-a-server-refusal-is-the-error-line ()
  (with-mk-server (:answer (lambda (tool arguments)
                              (declare (ignore tool arguments))
                              (values "the price is above max_cost_usd" t)))
    (is-carrying (answer (mk-tool "makernice_check_job" (nlk:json-object "job_id" "job-1")))
      ("ERROR: makernice check_job") ("above max_cost_usd"))))

(deftest makernice-cell-an-event-stream-answer-is-read ()
  (with-mk-server (:answer (lambda (tool arguments)
                              (declare (ignore tool arguments))
                              (values "{\"per_job_limit_usd\":5,\"available_usd\":9}" nil)))
    (let ((*events* t))
      (is-carrying (answer (mk-tool "makernice_get_balance" (nlk:json-object)))
        ("available_usd") ("9")))
    (is (equal "tools/call"
               (nlk:json-value (mk-last-message) :string "method")
               ) "the answer really came as events")))

(deftest makernice-cell-a-refused-token-names-where-it-came-from ()
  (with-mk-wire
    (let ((*answer-fn* (lambda (method url args)
                          (declare (ignore method url args))
                          (values "{\"error\":{\"code\":\"invalid_token\",\"message\":\"Missing or invalid access token\"}}"
                                  401 (mk-headers (cons "content-type" "application/json"))))))
      (with-cell-stop ((makernice-start "token" "stale"))
        (is-carrying (answer (mk-tool "makernice_list_models" (nlk:json-object)))
          ("ERROR: makernice") ("refused the token") ("the section's token"))))))

(deftest makernice-cell-a-forgotten-session-is-opened-again ()
  (with-mk-server (:answer (lambda (tool arguments)
                              (declare (ignore tool arguments))
                              (values "{\"available_usd\":2}" nil)))
    (let* ((forgotten nil)
           (inner *answer-fn*)
           (*answer-fn*
             (lambda (method url args)
               (declare (ignore method url))
               (let ((name (nlk:json-value (mk-message args) :string "method")))
                 (if (and (equal name "tools/call") (not forgotten))
                     (progn (setf forgotten t)
                            (values "{\"error\":\"session not found\"}" 404
                                    (mk-headers (cons "content-type" "application/json"))))
                     (funcall inner method url args))))))
      (is-carrying (answer (mk-tool "makernice_get_balance" (nlk:json-object)))
        ("available_usd") ("2"))
      (is (= 2 (mk-initialize-count)) "the handshake ran again for the forgotten session"))))

;;; --- the verb, and /makernice ----------------------------------------------------

(deftest makernice-cell-the-verb-takes-a-plist ()
  (with-mk-server (:answer (lambda (tool arguments)
                              (declare (ignore tool))
                              (values (format nil "{\"amount_usd\":1,\"model\":\"~a\"}"
                                              (nlk:json-value arguments :string "input" "model"))
                                      nil)))
    (is-carrying (answer (makernice:call "estimate_cost"
                                  (list :input (list :model "kling" :mode "t2v" :prompt "a boat"))))
      ("kling"))
    (let ((sent (nlk:json-value (mk-last-message) :object "params" "arguments")))
      (is (equal "kling" (nlk:json-value sent :string "input" "model")) "a keyword becomes the wire name"))))

(deftest makernice-cell-the-command-reports-what-it-runs-with ()
  (with-mk-server ()
    (is-carrying (answer (cell-entry "nodecode-makernice" "makernice" ""))
      ("https://mcp.makernice.com/mcp") ("token: the section's token")
      ("makernice_estimate_cost") ("none yet"))
    (is-carrying (answer (cell-entry "nodecode-makernice" "makernice" "balance"))
      ("ok" "the balance the server answered"))))
