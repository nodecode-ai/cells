;;;; cell-test.lisp --- lifecycle, the two wires, the tools and /priors.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a bad endpoint and a wallet with no key refused), the tools
;;;; through the seam the model calls them by -- the JSON-RPC they post, the
;;;; session header they carry, the arguments they fold -- /priors with and
;;;; without a word, and that the wallet tools stay shut until the section
;;;; opens them. Both seams are stubbed: NLK:HTTP answers the hosted server's
;;;; JSON, or its SSE, and the wallet seam answers the local command, so
;;;; nothing reaches Priors, the chain, or a child process.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed HTTP request of the test, (METHOD URL . ARGS), newest first.")

(defvar *run-calls* '()
  "Every stubbed wallet run of the test, (ARGV INPUT), newest first.")

(defvar *tool-answer* nil
  "What a stubbed MCP tools/call answers: a function of TOOL-NAME and its
ARGUMENTS object => the text the server would send back.")

(defvar *tool-error* nil
  "When true, the stubbed server marks its tool answer as an error.")

(defvar *sse* nil
  "When true, a stubbed tools/call answer arrives as an SSE stream.")

(defvar *run-answer* nil
  "What a stubbed wallet run answers: a function of ARGV and INPUT =>
(values STDOUT STDERR STATUS), or NIL for the ordinary success.")

(defun priors-default-tool-answer (name arguments)
  (declare (ignore arguments))
  (format nil "~a: ok" name))

(defun priors-json-message (object &optional (status 200) headers)
  (values (nlk:encode-json-object object) status headers))

(defun priors-sse-message (object &optional (status 200))
  (values (format nil "event: message~%data: ~a~%~%" (nlk:encode-json-object object))
          status))

(defun priors-session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "session-1")
    headers))

(defun priors-result (id text)
  (nlk:json-object "jsonrpc" "2.0" "id" id
                    "result" (nlk:json-object "content" (vector (nlk:json-object "type" "text"
                                                                                    "text" text))
                                               "isError" (and *tool-error* t))))

(defun priors-answer (method url args)
  "The hosted server, stubbed: initialize, the notification, tools/call."
  (declare (ignore method url))
  (let* ((body (nlk:decode-json (getf args :content)))
         (id (nlk:json-value body :integer "id"))
         (name (nlk:json-value body :string "method")))
    (cond ((equal name "initialize")
           (priors-json-message
            (nlk:json-object "jsonrpc" "2.0" "id" id
                             "result" (nlk:json-object
                                       "protocolVersion" "2025-06-18"
                                       "capabilities" (nlk:json-object)
                                       "serverInfo" (nlk:json-object "name" "priors")))
            200 (priors-session-headers)))
          ((equal name "notifications/initialized") (values "" 202 nil))
          ((equal name "tools/call")
           (let* ((params (nlk:json-value body :object "params"))
                  (tool (nlk:json-value params :string "name"))
                  (arguments (nlk:json-value params :object "arguments"))
                  (message (priors-result id (funcall *tool-answer* tool arguments))))
             (if *sse* (priors-sse-message message) (priors-json-message message))))
          (t (values "{}" 400 nil)))))

(defun priors-stub-run (argv input)
  "The wallet command, stubbed: one JSON-RPC answer line on its stdout."
  (push (list argv input) *run-calls*)
  (cond (*run-answer* (funcall *run-answer* argv input))
        (t (let* ((messages (remove "" (uiop:split-string input
                                                         :separator (list #\Newline))
                                    :test #'string=))
                  (messages (mapcar #'nlk:decode-json messages))
                  (params (and (third messages) (nlk:json-value (third messages) :object "params")))
                  (text (format nil "wallet ran ~a"
                                (or (and params (nlk:json-value params :string "name"))
                                    "nothing"))))
             (values (format nil "~a~%" (nlk:encode-json-object (priors-result 2 text)))
                     "" 0)))))

(defmacro with-priors-wire (&body body)
  "BODY with both seams stubbed: NLK:HTTP is the hosted server and the
wallet seam is PRIORS-STUB-RUN, so no socket and no child process is opened."
  `(let ((*requests* '())
         (*run-calls* '())
         (*sse* nil)
         (*tool-error* nil)
         (*tool-answer* #'priors-default-tool-answer)
         (*run-answer* nil)
         (nodecode-priors::*run* 'priors-stub-run))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (priors-answer method url args))
       ,@body)))

(defun priors-asked ()
  "The stubbed HTTP requests, oldest first."
  (reverse *requests*))

(defun priors-request-body (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun priors-request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun priors-method (request)
  (nlk:json-value (priors-request-body request) :string "method"))

(defun priors-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun priors-wallet-messages (call)
  "The JSON-RPC lines a wallet run was handed, decoded."
  (mapcar #'nlk:decode-json
          (remove "" (uiop:split-string (second call) :separator (list #\Newline))
                  :test #'string=)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "priors" "PRIORS-CELL-" :start nodecode-priors:start-cell)

(define-cell-lifecycle-tests "priors"
  (:fixture with-priors-wire)
  (:config)
  (:help :priors)
  (:command "priors")
  (:running (is (nle::find-tool "priors_agent_record") "the record tool is registered")
            (is (nle::find-tool "priors_find_sellers"))
            (is (nle::find-tool "priors_pay_url") "and the payment one, shut until the wallet is on")
            (is (equal "https://mcp.priors.trade/mcp" (nodecode-priors::endpoint)) "the hosted server's default address")
            (is (not (nodecode-priors::wallet-wanted-p)) "the wallet is off by default")

            (is (stringp (nodecode-priors::status-line))))
  (:stopped (is (null (nle::find-tool "priors_agent_record")) "stop unregisters the tools")
            (is (null nodecode-priors::*session*) "and the MCP session is forgotten"))
  (:refused ("base_url" "priors.trade") ("wallet" t)))

(deftest priors-cell-the-wallet-seam-is-the-command-runner ()
  ;; Outside the fixture the seam is its default: the wallet's runner and not
  ;; the /priors command, which is another function of the same package.
  (is (eq 'nodecode-priors::run-wallet-command nodecode-priors::*run*) "*RUN* runs the wallet command")
  (is (fboundp 'nodecode-priors::run-wallet-command))
  (is (not (eq 'nodecode-priors::run-command nodecode-priors::*run*)) "and not the slash command of the same name"))

(deftest priors-cell-a-wallet-with-no-key-is-refused-by-name ()
  (with-priors-wire
    (is-carrying (said (refusal-text nlk:config-refusal
                                     (nodecode-priors:start-cell (priors-config "wallet" t))))
                 ("key_file" "read tools work without one"))
    (is-carrying (said (refusal-text nlk:config-refusal
                                     (nodecode-priors:start-cell
                                      (priors-config "wallet" t "key_file" "/k/agent.key"))))
                 ("agent_id"))
    (is (null (nle::find-tool "priors_agent_record")) "a refused start installed nothing")))

;;; --- the hosted server ------------------------------------------------------------

(deftest priors-cell-a-record-is-one-session-then-one-call ()
  (with-priors-wire
    (with-cell-stop ((nodecode-priors:start-cell (priors-config)))
      (setf *tool-answer* (lambda (name arguments)
                             (format nil "~a for agent #~a"
                                     name (nlk:json-value arguments :integer "agent_id"))))
      (is (equal "agent_record for agent #9233"
                 (priors-tool "priors_agent_record" (nlk:json-object "agent_id" "9233"))) "the server's text is the tool's answer")
      (let ((requests (priors-asked)))
        (is (= 3 (length requests)) "initialize, the notification, then the call")
        (is (equal "initialize" (priors-method (first requests))))
        (is (equal "notifications/initialized" (priors-method (second requests))))
        (is (equal "tools/call" (priors-method (third requests))))
        (is (equal "application/json, text/event-stream"
                   (priors-request-header (first requests) "accept")) "the transport the MCP spec asks for")
        (let* ((body (priors-request-body (first requests)))
               (params (nlk:json-value body :object "params")))
          (is (equal "nodecode-priors" (nlk:json-value params :string "clientInfo" "name")))
          (is (stringp (nlk:json-value params :string "protocolVersion"))))
        (is (null (priors-request-header (first requests) "mcp-session-id")) "initialize opens the session")
        (is (equal "session-1" (priors-request-header (third requests) "mcp-session-id")) "every later message carries it")
        (let* ((body (priors-request-body (third requests)))
               (params (nlk:json-value body :object "params")))
          (is (equal "agent_record" (nlk:json-value params :string "name")))
          (is (eq 9233 (nlk:json-value params :integer "arguments" "agent_id")) "a digit string is sent as the number the server declares")))
      (priors-tool "priors_score_of" (nlk:json-object "agent_id" 7))
      (is (= 4 (length (priors-asked))) "the session is reused, not opened again"))))

(deftest priors-cell-an-sse-answer-is-read-and-an-empty-call-still-asks ()
  (with-priors-wire
    (with-cell-stop ((nodecode-priors:start-cell (priors-config)))
      (setf *sse* t)
      (setf *tool-answer* (lambda (name arguments)
                             (declare (ignore name arguments))
                             "Agent #9233: score 812/1000, no defaults."))
      (is (search "score 812/1000"
                  (priors-tool "priors_agent_record" (nlk:json-object "agent_id" 9233))) "an SSE stream is read as its data lines")
      (setf *sse* nil
            *tool-answer* #'priors-default-tool-answer)
      (is (search "find_sellers: ok"
                  (priors-tool "priors_find_sellers" (nlk:json-object))) "no argument is an empty object, not a refusal"))))

(deftest priors-cell-a-tools-own-error-is-the-models-repair ()
  (with-priors-wire
    (with-cell-stop ((nodecode-priors:start-cell (priors-config)))
      (setf *tool-error* t
            *tool-answer* (lambda (name arguments)
                             (declare (ignore name arguments))
                             "agent 9233 has no Priors record yet"))
      (is-carrying (answer (priors-tool "priors_agent_record" (nlk:json-object "agent_id" 9233)))
                   ("ERROR: priors:" "no Priors record yet")))))

;;; --- the wallet ---------------------------------------------------------------------

(deftest priors-cell-a-payment-runs-the-local-command-once ()
  (with-priors-wire
    (with-cell-stop ((nodecode-priors:start-cell
                      (priors-config "wallet" t "agent_id" "1234"
                                     "key_file" "/k/agent.key"
                                     "pay_hosts" (vector "check.example")
                                     "state_dir" "/s")))
      (is (search "wallet ran pay_url"
                  (priors-tool "priors_pay_url"
                                (nlk:json-object "url" "https://check.example/x"
                                                 "max_price_usd" 0.1))) "the command's answer is the tool's")
      (is (null (priors-asked)) "the wallet tools open no HTTP session of their own")
      (let* ((call (first *run-calls*))
             (argv (first call))
             (messages (priors-wallet-messages call)))
        (is (= 1 (length *run-calls*)) "one child run per call")
        (is (equal '("priors-mcp") argv) "the section's command, as configured")
        (is (= 3 (length messages)) "initialize, the notification, the call")
        (is (equal "initialize" (nlk:json-value (first messages) :string "method")))
        (is (equal "tools/call" (nlk:json-value (third messages) :string "method")))
        (let* ((params (nlk:json-value (third messages) :object "params"))
               (arguments (nlk:json-value params :object "arguments")))
          (is (equal "pay_url" (nlk:json-value params :string "name")))
          (is (equal "https://check.example/x" (nlk:json-value arguments :string "url")))
          (is (< (abs (- (nlk:json-value arguments :number "max_price_usd") 0.1)) 1d-6) "the call's own price ceiling, as the number it was given")))
      (let ((environment (nodecode-priors::wallet-environment)))
        (is (member "PRIORS_KEY_FILE=/k/agent.key" environment :test #'equal) "the key file it names reaches the command")
        (is (member "PRIORS_AGENT_ID=1234" environment :test #'equal))
        (is (member "PRIORS_PAY_HOSTS=check.example" environment :test #'equal) "the host list pay_url is bounded by reaches it too")))))

(deftest priors-cell-the-wallet-tools-stay-shut-while-it-is-off ()
  (with-priors-wire
    (with-cell-stop ((nodecode-priors:start-cell (priors-config)))
      (is-carrying (answer (priors-tool "priors_credit_status" (nlk:json-object)))
                   ("ERROR: priors:") ("wallet") ("npm install -g @priors/mcp"))
      (is (null *run-calls*) "and no command was run")
      (is (null (priors-asked)) "nor any request sent"))))

(deftest priors-cell-a-command-that-cannot-start-is-words-not-a-crash ()
  (with-priors-wire
    (with-cell-stop ((nodecode-priors:start-cell
                      (priors-config "wallet" t "agent_id" "1234" "key_file" "/k/agent.key")))
      (setf *run-answer* (lambda (argv input)
                           (declare (ignore argv input))
                           (values nil "spawn: no such command" 127)))
      (is-carrying (answer (priors-tool "priors_wallet_balance" (nlk:json-object)))
                   ("ERROR: priors:") ("exited 127") ("no such command")))))


;;; --- /priors ------------------------------------------------------------------------

(deftest priors-cell-the-command-reads-a-record-or-lists-sellers ()
  (with-priors-wire
    (with-cell-stop ((nodecode-priors:start-cell (priors-config)))
      (is-carrying (answer (cell-entry "nodecode-priors" "priors" ""))
                   ("Priors: records from https://mcp.priors.trade/mcp" "the wallet is off"))
      (is (null (priors-asked)) "a status read asks the server nothing")
      (setf *tool-answer* (lambda (name arguments)
                             (declare (ignore arguments))
                             (format nil "~a says: line 5 USDG, 0 defaults" name)))
      (is-carrying (answer (cell-entry "nodecode-priors" "priors" "9233"))
                   ("Agent #9233" "agent_record says"))
      (let* ((body (priors-request-body (third (priors-asked))))
             (params (nlk:json-value body :object "params")))
        (is (equal "agent_record" (nlk:json-value params :string "name")) "the command reads the agent it names"))
      (setf *requests* '())
      (is-carrying (answer (cell-entry "nodecode-priors" "priors" "check")) ("find_sellers says"))
      (let* ((body (priors-request-body (first (priors-asked))))
             (params (nlk:json-value body :object "params")))
        (is (equal "find_sellers" (nlk:json-value params :string "name")))
        (is (equal "check" (nlk:json-value params :string "arguments" "query")) "a word is the search it names")))))
