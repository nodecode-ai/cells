;;;; wire.lisp --- BoxCompute's MCP server, spoken to directly.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original shipped no code: a manifest, a skill, and a line the user
;;;; ran to add BoxCompute's hosted MCP server to Hermes. The server is still
;;;; the service, and this cell is still a client of it -- but the client is
;;;; here, not in another program: JSON-RPC over streamable HTTP, nlk:http
;;;; doing the POST, an API key as the bearer. So the tools work with nothing
;;;; installed but the cell.
;;;;
;;;; Three messages open a session: `initialize' answers the protocol version
;;;; and, in a header, the session id; `notifications/initialized' closes the
;;;; handshake; every later `tools/call' echoes the session id. A server that
;;;; answers a request with text/event-stream instead of a JSON body is read
;;;; too, and its first `data:' carrying our reply is the reply.
;;;;
;;;; Every byte goes through POST-RPC, so a test stubs one name and reaches
;;;; nothing. A refused status or a JSON-RPC error becomes BOXCOMPUTE-ERROR
;;;; with the service's own words in it, which is what the model repairs from.

(in-package #:nodecode-boxcompute)

(defparameter +default-endpoint+ "https://mcp.boxcompute.ai/mcp"
  "BoxCompute's MCP server, where every tool call goes unless the section
names another address.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks: the one the server documents.")

(defvar *session-id* nil
  "The Mcp-Session-Id the initialize answer issued, echoed on every later
request, or NIL before the handshake.")

(defvar *session-open-p* nil
  "Whether this process has completed the handshake on *SESSION-ID*'s session.")

(defvar *next-id* 0
  "The JSON-RPC id of the next request; ids are this client's own and never
repeat inside one session.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names inline or by variable, else BOXCOMPUTE_API_KEY."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "BOXCOMPUTE_API_KEY"))))

(defun check-config ()
  "Refuse a section that could reach BoxCompute with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "boxcompute: this cell needs a key: set BOXCOMPUTE_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the boxcompute section; a key is made in the "
                                 "BoxCompute app under Menu -> API keys"))))

(defun endpoint ()
  "The MCP address this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :url) ""))))
    (if (plusp (length given)) given +default-endpoint+)))

(defun auth-headers ()
  "The plugin's own authentication: the account's key as a bearer."
  (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
        (cons "x-client-name" "nodecode boxcompute cell")))

(defun header-value (headers name)
  "NAME's value from the response headers, or NIL: both of dexador's
backends lowercase every name."
  (and (hash-table-p headers) (gethash name headers)))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :text "error")
        (nlk:json-value body :text "message")
        (nlk:json-value body :text "detail")
        (nlk:json-value body :text "code")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun post-rpc (message session)
  "POST one JSON-RPC MESSAGE to the endpoint, on SESSION when it is a string
=> (values BODY STATUS HEADERS). A transport failure is BOXCOMPUTE-ERROR."
  (let* ((limit (setting :timeout-seconds))
         (headers (append (list (cons "content-type" "application/json")
                                (cons "accept" "application/json, text/event-stream")
                                (cons "mcp-protocol-version" +protocol-version+))
                          (and session (list (cons "mcp-session-id" session)))
                          (auth-headers)))
         (answer nil)
         (status nil)
         (response-headers nil))
    (handler-case
        (multiple-value-setq (answer status response-headers)
          (nlk:http :post (endpoint)
                    :headers headers
                    :content (nlk:encode-json-object message)
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "boxcompute: ~a" (nle:transport-failure-label condition (endpoint)))))
    (values answer status response-headers)))

(defun check-status (method status answer)
  "Refuse a refused STATUS, saying what the service said."
  (cond ((and (integerp status) (< status 300)) t)
        ((and (integerp status) (= status 401))
         (fail "boxcompute ~a: HTTP 401, the key was refused; check api_key in the boxcompute section"
               method))
        ((and (integerp status) (= status 429))
         (fail "boxcompute ~a: HTTP 429, rate limited; wait a moment and try again" method))
        (t (fail "boxcompute ~a: HTTP ~a~@[: ~a~]" method status (body-error-text answer)))))

(defun event-data (text)
  "TEXT -- a text/event-stream body -- as the payloads of its `data:' lines,
one string per event."
  (let ((events '())
        (current '()))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((line (string-right-trim '(#\Return) line)))
        (cond ((uiop:string-prefix-p "data:" line)
               (push (string-left-trim " " (subseq line 5)) current))
              ((zerop (length line))
               (when current (push (format nil "~{~a~^~%~}" (nreverse current)) events)
                     (setf current '()))))))
    (when current (push (format nil "~{~a~^~%~}" (nreverse current)) events))
    (nreverse events)))

(defun reply-message (answer content-type)
  "The JSON-RPC message ANSWER carries: a whole JSON body, or the first event
of a text/event-stream body that is one."
  (flet ((message-of (text)
           (let ((value (handler-case (nlk:decode-json text) (error () nil))))
             (and (hash-table-p value) value))))
    (if (and (stringp content-type) (search "text/event-stream" content-type))
        (loop for event in (event-data (or answer ""))
              for message = (message-of event)
              when message return message)
        (message-of (if (stringp answer) answer "")))))

(defun request-message (method params)
  "One JSON-RPC request object."
  (nlk:make-json-object "jsonrpc" "2.0" "id" (incf *next-id*) "method" method
                         "params" (or params (nlk:json-object))))

(defun notify (method params)
  "POST one JSON-RPC notification: no id, and the server answers 202 with no body."
  (handler-case (post-rpc (nlk:make-json-object "jsonrpc" "2.0" "method" method
                                                 "params" (or params (nlk:json-object)))
                           *session-id*)
    (error () nil))
  nil)

(defun open-session ()
  "The handshake: initialize, keep the session id it issues, then say initialized."
  (let* ((params (nlk:make-json-object
                  "protocolVersion" +protocol-version+
                  "capabilities" (nlk:json-object)
                  "clientInfo" (nlk:make-json-object "name" "nodecode" "version" "0.1.0")))
         (message (request-message "initialize" params)))
    (multiple-value-bind (answer status headers) (post-rpc message nil)
      (check-status "initialize" status answer)
      (setf *session-id* (header-value headers "mcp-session-id")
            *session-open-p* t)
      (notify "notifications/initialized" nil)
      *session-id*)))

(defun ensure-session ()
  "The session a request rides, opening one when there is none."
  (unless *session-open-p* (open-session))
  *session-id*)

(defun forget-session ()
  (setf *session-id* nil *session-open-p* nil)
  nil)

(defun rpc (method params)
  "One JSON-RPC request => its result object. A session the server forgot is
opened once more and the request posted again; a JSON-RPC error is
BOXCOMPUTE-ERROR carrying the server's message."
  (ensure-session)
  (multiple-value-bind (answer status headers)
      (post-rpc (request-message method params) *session-id*)
    (when (and (integerp status) (= status 404) *session-open-p*)
      (forget-session)
      (ensure-session)
      (multiple-value-setq (answer status headers)
        (post-rpc (request-message method params) *session-id*)))
    (check-status method status answer)
    (let ((message (reply-message answer (and (hash-table-p headers)
                                              (header-value headers "content-type")))))
      (unless message
        (fail "boxcompute ~a: the server answered nothing" method))
      (let ((problem (nlk:json-value message :object "error")))
        (when problem
          (fail "boxcompute ~a: ~a" method
                (or (nlk:json-value problem :text "message")
                    (nlk:pretty-json problem)))))
      (nlk:json-value message :object "result"))))

(defun result-text (result)
  "RESULT -- a tools/call result -- as the text the model reads: its content
blocks, in order; the JSON object when it carries none."
  (let ((blocks (nlk:json-value result :array "content")))
    (let ((text (and blocks
                      (format nil "~{~a~^~%~}"
                              (loop for block across blocks
                                    for text = (nlk:json-value block :text "text")
                                    when text collect text)))))
      (cond ((and text (plusp (length text)))
             (if (eq t (nlk:json-value result :boolean "isError"))
                 (format nil "ERROR: ~a" text)
                 text))
            (t (or (nlk:pretty-json result) ""))))))

(defun tool-call (name arguments)
  "Call the server's tool NAME with ARGUMENTS -- a JSON object -- => its text."
  (result-text (rpc "tools/call"
                    (nlk:make-json-object "name" name
                                           "arguments" (or arguments (nlk:json-object))))))
