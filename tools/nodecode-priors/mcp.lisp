;;;; mcp.lisp --- the two wires Priors speaks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was two MCP servers plus a skill: the hosted,
;;;; read-only one at mcp.priors.trade (records, sellers, pool data) and the
;;;; local `priors-mcp` command that signs with the agent's own wallet.
;;;; Neither is bundled here. The hosted one is spoken over MCP's streamable
;;;; HTTP transport -- one POST per JSON-RPC message, an `Mcp-Session-Id'
;;;; header carried back, a body that is JSON or an SSE stream of it -- and
;;;; the wallet one over the same JSON-RPC, written down a child command's
;;;; stdin and read back a line at a time. *RUN* is that second seam, so a
;;;; test answers a wallet call with no command, no key and no chain.

(in-package #:nodecode-priors)

(defparameter +protocol+ "2025-06-18"
  "The MCP revision this client speaks.")

(defparameter +client-name+ "nodecode-priors"
  "What this client calls itself in `initialize'.")

(defvar *session* nil
  "The hosted server's Mcp-Session-Id while it is known, or NIL.")

(defvar *session-url* nil
  "The endpoint *SESSION* was opened on: a section change opens another.")

(defvar *call-id* 0
  "The last JSON-RPC id this client sent; every call takes the next one.")

(defparameter *run* 'run-wallet-command
  "How a wallet tool runs the local command: (funcall *RUN* ARGV INPUT) =>
(values STDOUT STDERR STATUS), STATUS an exit code, :TIMEOUT, or 127 for a
command that could not be started.")

;;; --- reading a wire answer ------------------------------------------------

(defun endpoint ()
  "The hosted MCP endpoint the section names, without a trailing slash."
  (string-right-trim "/" (or (setting :base-url) "")))

(defun header-value (headers name)
  "NAME's value in HEADERS -- dexador's hash table, or a test's alist."
  (let ((key (string-downcase name)))
    (typecase headers
      (hash-table (or (gethash key headers) (gethash name headers)))
      (list (cdr (assoc key headers :test #'string-equal))))))

(defun sse-p (text)
  "TEXT as an SSE stream rather than one JSON body."
  (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (or (uiop:string-prefix-p "data:" trimmed)
        (uiop:string-prefix-p "event:" trimmed)
        (uiop:string-prefix-p ":" trimmed))))

(defun decode-line (text)
  "TEXT as a decoded JSON value, or NIL when it is not JSON."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (and (plusp (length trimmed))
         (ignore-errors (nlk:decode-json trimmed)))))

(defun wire-objects (text)
  "Every JSON-RPC message TEXT carries: one JSON body, or the payloads of an
SSE stream's `data:' lines."
  (cond ((or (null text) (not (stringp text))) '())
        ((sse-p text)
         (loop for raw in (uiop:split-string text :separator (list #\Newline))
               for line = (string-right-trim '(#\Return) raw)
               for plain = (string-trim '(#\Space #\Tab) line)
               when (uiop:string-prefix-p "data:" plain)
                 collect (let ((payload (string-trim '(#\Space #\Tab) (subseq plain 5))))
                           (decode-line payload))))
        ((uiop:string-prefix-p "{" (string-left-trim '(#\Space #\Tab #\Newline #\Return) text))
         (list (decode-line text)))
        (t '())))

(defun json-lines (text)
  "Every JSON object among TEXT's lines: an MCP stdio server writes one per
line, and may write other lines around them."
  (loop for line in (uiop:split-string (or text "") :separator (list #\Newline #\Return))
        for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
        when (and (plusp (length trimmed)) (char= #\{ (char trimmed 0)))
          collect (decode-line trimmed)))

(defun message-id (object)
  (and (hash-table-p object) (nlk:json-value object :integer "id")))

(defun message-answer (objects id)
  "The message among OBJECTS answering ID, else the last one that carries a
result or an error."
  (or (find id objects :key #'message-id :test #'eql)
      (find-if (lambda (object)
                 (and (hash-table-p object)
                      (or (nlk:json-value object :object "result")
                          (nlk:json-value object :object "error"))))
               (reverse objects))))

(defun message-error (object)
  "OBJECT's JSON-RPC error, as the one line the model reads."
  (let ((error (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (and error
         (or (nlk:json-value error :string "message")
             (format nil "JSON-RPC error ~a" (nlk:json-value error :integer "code"))))))

;;; --- the hosted server, over streamable HTTP -------------------------------

(defun request-body (id method params)
  "One JSON-RPC message as its wire text; NO ID makes it a notification."
  (nlk:encode-json-object
   (if id
       (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)
       (nlk:json-object "jsonrpc" "2.0" "method" method "params" params))))

(defun initialize-params ()
  (nlk:json-object "protocolVersion" +protocol+
                    "capabilities" (nlk:json-object)
                    "clientInfo" (nlk:json-object "name" +client-name+
                                                   "version" "0.1.0")))

(defun post-rpc (body session)
  "POST one JSON-RPC message to the endpoint => (values OBJECT STATUS HEADERS)."
  (let ((limit (setting :timeout-seconds)))
    (multiple-value-bind (answer status headers)
        (nlk:http :post (endpoint)
                  :headers (append (list (cons "content-type" "application/json")
                                         (cons "accept" "application/json, text/event-stream"))
                                   (and session (list (cons "mcp-session-id" session))))
                  :content body
                  :timeout limit
                  :connect-timeout (min 10 limit))
      (values (message-answer (wire-objects answer) (message-id (decode-line body)))
              status headers))))

(defun open-session ()
  "Initialize the hosted server, send the initialized notification, and answer
the session id it handed back."
  (let ((id (incf *call-id*)))
    (multiple-value-bind (object status headers)
        (post-rpc (request-body id "initialize" (initialize-params)) nil)
      (let ((session (header-value headers "mcp-session-id")))
        (declare (ignore session))
        (cond ((message-error object)
               (fail "the Priors server refused initialize: ~a" (message-error object)))
              ((or (null status) (>= status 400))
               (fail "the Priors server at ~a answered initialize with HTTP ~a"
                     (endpoint) status)))
        (setf *session* (header-value headers "mcp-session-id")
              *session-url* (endpoint))
        ;; The notification is answered with 202 and no body; a server that
        ;; does not care about it must not fail the call over it.
        (handler-case
            (post-rpc (request-body nil "notifications/initialized" (nlk:json-object))
                      *session*)
          (error () nil))
        *session*))))

(defun ensure-session ()
  (unless (and *session* (equal *session-url* (endpoint)))
    (open-session))
  *session*)

(defun call-http (method params)
  "METHOD with PARAMS on the hosted server => its result object. A session
the server has forgotten (HTTP 404) is opened once more."
  (loop repeat 2
        do (ensure-session)
           (let ((id (incf *call-id*)))
             (multiple-value-bind (object status)
                 (post-rpc (request-body id method params) *session*)
               (cond ((message-error object)
                      (if (and (= (or status 0) 404))
                          (setf *session* nil)
                          (fail "~a: ~a" method (message-error object))))
                     ((hash-table-p object)
                      (return (nlk:json-value object :object "result")))
                     ((and (>= (or status 0) 400) *session*)
                      (setf *session* nil))
                     (t (return nil)))))
        finally (fail "~a: the Priors server at ~a ended the session twice" method (endpoint))))

(defun result-text (result)
  "RESULT's content blocks as the one text a tool answers."
  (let* ((blocks (nlk:json-value result :array "content"))
         (text (with-output-to-string (out)
                 (loop for block across (or blocks #())
                       when (equal "text" (nlk:json-value block :string "type"))
                         do (write-string (or (nlk:json-value block :string "text") "") out)))))
    (setf text (string-right-trim '(#\Newline #\Space) text))
    (cond ((nlk:json-value result :boolean "isError")
           (fail "~a" (if (plusp (length text)) text "the Priors tool failed")))
          ((plusp (length text)) text)
          ((nlk:json-value result :object "structuredContent")
           (nlk:encode-json-object (nlk:json-value result :object "structuredContent")))
          (t "(the Priors tool answered nothing)"))))

;;; --- the wallet, over the local command's stdio ----------------------------

(defun wallet-wanted-p ()
  (and (setting :wallet) t))

(defun check-wallet (what)
  "Refuse a money-moving call the section has not enabled."
  (unless (wallet-wanted-p)
    (fail "~a moves real USDG and needs the agent's wallet: set \"wallet\": true in the priors section, install the command once (npm install -g @priors/mcp@0.14.1) and name key_file and agent_id" what)))

(defun wallet-environment ()
  "The variables the local command reads, as ENVIRONMENT-WITH's strings."
  (flet ((named (variable value)
           (and value (stringp value) (plusp (length value))
                (format nil "~a=~a" variable value))))
    (remove nil
            (list (named "PRIORS_KEY_FILE" (setting :key-file))
                  (named "PRIORS_AGENT_ID" (setting :agent-id))
                  (named "PRIORS_STATE_DIR" (setting :state-dir))
                  (named "PRIORS_RPC" (setting :rpc))
                  (named "PRIORS_MAX_PRICE_USD" (setting :max-price-usd))
                  (let ((hosts (setting :pay-hosts)))
                    (and hosts (plusp (length hosts))
                         (named "PRIORS_PAY_HOSTS"
                                (format nil "~{~a~^,~}" hosts))))))))

(defun run-wallet-command (argv input)
  "Run the local wallet command with INPUT on its stdin => (values STDOUT
STDERR STATUS); a command that is not installed is 127, as a shell says."
  (handler-case
      (nlk:run-bounded argv
                       :seconds (setting :timeout-seconds)
                       :environment (wallet-environment)
                       :input input)
    (error (condition)
      (declare (ignore condition))
      (values nil
              (format nil "~a could not be run: install the wallet command (npm install -g @priors/mcp@0.14.1), or set the priors section's wallet_command"
                      (first argv))
              127))))

(defun wallet-call (name arguments)
  "NAME with ARGUMENTS through the local command: one child run per call,
`initialize', the initialized notification, then the call, all on its stdin."
  (let ((input (with-output-to-string (out)
                 (dolist (body (list (request-body 1 "initialize" (initialize-params))
                                      (request-body nil "notifications/initialized"
                                                    (nlk:json-object))
                                      (request-body 2 "tools/call"
                                                    (nlk:json-object "name" name
                                                                     "arguments" arguments))))
                   (write-string body out)
                   (write-char #\Newline out)))))
    (multiple-value-bind (out err status)
        (funcall *run* (setting :wallet-command) input)
      (let ((object (message-answer (json-lines out) 2)))
        (cond ((message-error object)
               (fail "~a: ~a" name (message-error object)))
              ((hash-table-p object)
               (nlk:json-value object :object "result"))
              ((and (integerp status) (plusp status))
               (let ((said (string-trim '(#\Space #\Tab #\Newline #\Return) (or err ""))))
                 (fail "~a: the wallet command exited ~d~@[: ~a~]" name status
                       (and (plusp (length said)) (subseq said 0 (min 300 (length said)))))))
              (t (fail "~a: the wallet command answered nothing" name)))))))

;;; --- one call, either wire ------------------------------------------------

(defun tool-call (name arguments &key local)
  "MCP tool NAME with the JSON object ARGUMENTS, on the hosted server or
( LOCAL) through the agent's wallet => the text it answered."
  (result-text (if local
                   (progn (check-wallet name) (wallet-call name arguments))
                   (call-http "tools/call" (nlk:json-object "name" name
                                                             "arguments" arguments)))))

(defun digit-string-p (text)
  (and (stringp text)
       (plusp (length text))
       (every #'digit-char-p text)))

(defun arguments-object (args integers)
  "ARGS -- the dispatch's hash table -- as the JSON object an MCP tool takes.
INTEGERS names the members a server declares as integers: their digit text
is sent as the number it writes. An absent member is absent, not empty."
  (let ((object (nlk:json-object)))
    (maphash (lambda (key value)
               (cond ((null value))
                     ((and (stringp value) (zerop (length (string-trim '(#\Space) value)))))
                     ((and (member key integers :test #'string=) (digit-string-p value))
                      (setf (gethash key object) (parse-integer value)))
                     (t (setf (gethash key object) value))))
             args)
    object))
