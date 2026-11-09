;;;; wire.lisp --- the one seam to MAQAMI's hosted MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original was a Node stdio bridge that forwarded every MCP request to
;;;; https://mcp.maqami.co/ over Streamable HTTP; the tool list is the
;;;; server's, not the repository's. This carries the same one direction of
;;;; that bridge in Lisp: a JSON-RPC request is one POST, and the answer is
;;;; read out of a plain JSON body or out of the server-sent events the
;;;; endpoint answers with. The session is joined at the first call after a
;;;; start and forgotten at a stop; nothing about it is written down.

(in-package #:nodecode-maqami-travel)

(defparameter +endpoint+ "https://mcp.maqami.co/"
  "The address the plugin's mcp.json names. The section's base_url outranks it,
which is what a mirror or a local mock is pointed at.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision asked for at initialize. The server's own answer is the one
sent on every request after, so a server on another revision is believed.")

(defvar *session-id* nil
  "The session the server opened (Mcp-Session-Id), sent on every request after
it. NIL while the server keeps no session, which a stateless endpoint may.")

(defvar *protocol-version* nil
  "The revision the server agreed to, sent on every request after initialize.")

(defvar *joined* nil
  "Whether this cell has joined a session since it started.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request; each one takes the next.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun endpoint ()
  "The endpoint this cell talks to: the section's base_url, else MAQAMI's own."
  (or (nonempty (setting :base-url)) +endpoint+))

(defun forget-session ()
  "Forget the joined session and the ids spent on it: the next call joins a
fresh one. Called at start and at stop, so a restarted cell never reuses a
session the server has dropped."
  (setf *session-id* nil *protocol-version* nil *joined* nil *request-id* 0))

(defun clipped (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))

;;; --- one exchange ------------------------------------------------------------

(defun request-headers ()
  "The headers every POST carries: JSON in, a JSON body or an event stream out,
and the session and revision once they are known."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream"))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (when *protocol-version*
            (list (cons "mcp-protocol-version" *protocol-version*)))))

(defun exchange (message)
  "POST the JSON-RPC MESSAGE to the endpoint => (values BODY STATUS HEADERS). A
transport failure is this cell's own error, naming the endpoint."
  (let* ((limit (setting :timeout-seconds))
         (answer (list nil nil nil nil)))
    ;; HANDLER-CASE keeps one value, so the four a dial answers are carried
    ;; through a list and given back whole.
    (handler-case
        (setf answer (multiple-value-list
                      (nlk:http :post (endpoint)
                                :headers (request-headers)
                                :content (nlk:encode-json-object message)
                                :timeout limit
                                :connect-timeout (min 10 limit))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "maqami: ~a" (nle:transport-failure-label condition (endpoint)))))
    (values-list answer)))

;;; --- what came back -----------------------------------------------------------

(defun event-payloads (body)
  "BODY's JSON messages: one from a plain JSON body, or one per event of a
server-sent stream. An empty body carries none."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or body ""))))
    (cond ((zerop (length text)) '())
          ((char= (char text 0) #\{)
           (list text))
          (t (let ((events '())
                   (data '()))
               (labels ((flush ()
                          (when data
                            (push (format nil "~{~a~}" (nreverse data)) events)
                            (setf data '()))))
                 (dolist (line (nlk:lines text))
                   (cond ((uiop:string-prefix-p "data:" line)
                          (push (string-left-trim " " (subseq line 5)) data))
                         ((zerop (length line)) (flush))))
                 (flush))
               (nreverse events))))))

(defun message-of (body id)
  "The JSON-RPC message BODY carries: the one whose id is ID, else the last
one. A notice a stream carries before the answer has an id of its own."
  (let ((found nil)
        (last nil))
    (dolist (payload (event-payloads body) (or found last))
      (let ((message (ignore-errors (nlk:decode-json payload))))
        (when (hash-table-p message)
          (setf last message)
          (when (and id (equal id (nlk:json-value message :integer "id")))
            (setf found message)))))))

(defun rpc-error-text (message)
  "What MESSAGE's error object says, or NIL when it carries none."
  (let ((error (nlk:json-value message :object "error")))
    (and error
         (or (nlk:json-value error :text "message")
             (format nil "code ~a" (nlk:json-value error :integer "code"))))))

(defun read-answer (body status id)
  "BODY of a 2xx answer as its JSON-RPC message. A refused status, an error
object, or an answer carrying no message is this cell's error, in the server's
own words, which is what the model repairs from."
  (unless (and (integerp status) (<= 200 status) (< status 300))
    (fail "maqami: HTTP ~a~@[ - ~a~]"
          status (clipped (and (stringp body) body) 300)))
  (let ((message (message-of body id)))
    (unless message
      (fail "maqami: the server answered without a JSON-RPC message"))
    (let ((error (rpc-error-text message)))
      (when error (fail "maqami: ~a" error)))
    message))

(defun note-session (headers)
  "Keep the session the server opened on this answer, if it opened one."
  (when (hash-table-p headers)
    (let ((session (gethash "mcp-session-id" headers)))
      (when (and (stringp session) (plusp (length session)))
        (setf *session-id* session)))))

;;; --- the two shapes of request ------------------------------------------------

(defun request (method &optional (params (nlk:json-object)))
  "One JSON-RPC request => its result object, an empty one for a result the
server left out."
  (let* ((id (incf *request-id*))
         (message (nlk:make-json-object "jsonrpc" "2.0" "id" id
                                         "method" method "params" params)))
    (multiple-value-bind (body status headers) (exchange message)
      (note-session headers)
      (let ((answer (read-answer body status id)))
        (or (nlk:json-value answer :object "result") (nlk:json-object))))))

(defun notify (method &optional (params (nlk:json-object)))
  "One JSON-RPC notification: the server acknowledges it and says nothing."
  (exchange (nlk:make-json-object "jsonrpc" "2.0" "method" method "params" params))
  nil)

(defun join-session ()
  "Join a session with the server when this cell holds none: initialize, keep
what it says, then tell it the client is ready. => the revision in force."
  (unless *joined*
    (let ((result (request "initialize"
                           (nlk:make-json-object
                            "protocolVersion" +protocol-version+
                            "capabilities" (nlk:json-object)
                            "clientInfo" (nlk:make-json-object
                                          "name" "nodecode-maqami-travel"
                                          "version" "0.1.0")))))
      (setf *protocol-version* (or (nlk:json-value result :text "protocolVersion")
                                   +protocol-version+)
            *joined* t)
      (notify "notifications/initialized")))
  *protocol-version*)

(defun ask (method &optional (params (nlk:json-object)))
  "One request in a joined session, joining a fresh one once when the server
says the old session is gone (HTTP 404), which is how an endpoint that expired
an idle session answers."
  (join-session)
  (handler-case (request method params)
    (maqami-travel-error (condition)
      (unless (and *session-id*
                   (search "HTTP 404" (or (maqami-travel-error-detail condition) "")))
        (error condition))
      (forget-session)
      (join-session)
      (request method params))))

(defun call-tool (name arguments)
  "Call the server's tool NAME with ARGUMENTS (a JSON object, or NIL for none)
=> its result object, which carries the content blocks and ISERROR."
  (ask "tools/call" (nlk:make-json-object
                      "name" name
                      "arguments" (or arguments (nlk:json-object)))))
