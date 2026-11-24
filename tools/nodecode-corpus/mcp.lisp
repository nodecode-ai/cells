;;;; mcp.lisp --- Corpus's hosted MCP server, spoken to over JSON-RPC.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original package carries no code: it points a harness at one remote
;;;; streamable-HTTP MCP server and lets the harness's own client do the rest.
;;;; This file is that client, reduced to what these tools need: one POST per
;;;; JSON-RPC message to the endpoint, an `initialize' that claims a session
;;;; (Mcp-Session-Id) when the server offers one, the
;;;; `notifications/initialized' note, and `tools/call'.
;;;;
;;;; A server may answer a POST with plain JSON or with a text/event-stream;
;;;; both are read here, so a conforming server and a stateless one both work.
;;;; A session that goes stale (400, 404, 410 on tools/call) is claimed once
;;;; more and the call asked again once -- never in a loop.
;;;;
;;;; An operator's key, when the section has one, rides every POST as a bearer
;;;; token. Nothing here prints a header or a body back whole: a refusal
;;;; carries the status and the server's own sentence, bounded to one line.

(in-package #:nodecode-corpus)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks.")

(defparameter +client-name+ "nodecode")
(defparameter +client-version+ "0.1.0")

(defvar *session-id* nil
  "The Mcp-Session-Id this start holds, or NIL before an initialize.")

(defvar *stateless-p* nil
  "True once the server answered initialize with a refusal: then each call
stands alone, which is what a stateless server wants.")

(defvar *rpc-counter* 0
  "The last JSON-RPC id this start used; ids only have to be distinct.")

(defun endpoint ()
  "The MCP server's address, as the section gives it."
  (setting :endpoint))

(defun api-key ()
  "The Corpus key this start sends, or NIL: the section's inline secret,
else the variable api_key_env names. A key is optional -- the anonymous floor
answers, and formation is free."
  (or (let ((inline (nlk:trimmed (or (setting :api-key) ""))))
        (and (plusp (length inline)) inline))
      (let ((variable (nlk:trimmed (or (setting :api-key-env) ""))))
        (and (plusp (length variable)) (nle::credential-env variable)))))

(defun rpc-id ()
  (incf *rpc-counter*))

(defun request-headers ()
  "What every POST carries: JSON in, JSON or an event stream out, the
protocol revision, the session once one is claimed, and the key when one is set."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream"))
          (list (cons "mcp-protocol-version" +protocol-version+))
          (when *session-id*
            (list (cons "mcp-session-id" *session-id*)))
          (let ((key (api-key)))
            (when key (list (cons "authorization" (format nil "Bearer ~a" key)))))))

(defun one-line (value &key (cap 200))
  "VALUE's printed text on one line, at most CAP characters: what a refusal
says about a condition without pouring a whole backtrace into a tool result."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (princ-to-string value)))
         (flat (substitute #\Space #\Return
                            (substitute #\Space #\Tab
                                        (substitute #\Space #\Newline text)))))
    (if (> (length flat) cap)
        (concatenate 'string (subseq flat 0 (max 0 (- cap 3))) "...")
        flat)))

(defun header-value (headers name)
  "HEADERS' value for NAME, case-insensitively, when it is a non-empty string."
  (let ((pair (assoc name headers :test #'string-equal)))
    (and pair (stringp (cdr pair)) (plusp (length (cdr pair))) (cdr pair))))

(defun sse-payload (line)
  "LINE's payload when it is an event-stream data line, else NIL."
  (when (uiop:string-prefix-p "data:" line)
    (string-left-trim " " (subseq line 5))))

(defun messages-of (text)
  "Every JSON-RPC message TEXT carries: one object, a batch, or the data
lines of an event stream. A body that is neither answers NIL."
  (when text
    (let ((whole (ignore-errors (nlk:decode-json text))))
      (cond
        ((hash-table-p whole) (list whole))
        ((and (vectorp whole) (plusp (length whole))) (coerce whole 'list))
        (t (loop for line in (uiop:split-string text :separator (list (code-char 10)))
                 for payload = (sse-payload line)
                 for message = (and payload
                                    (plusp (length payload))
                                    (ignore-errors (nlk:decode-json payload)))
                 when (hash-table-p message) collect message))))))

(defun post (message)
  "POST one JSON-RPC MESSAGE. => (values MESSAGES STATUS HEADERS), every
status a value; a transport that never arrived refuses with CORPUS-ERROR."
  (handler-case
      (multiple-value-bind (text status headers)
          (nlk:http :post (endpoint)
                    :headers (request-headers)
                    :content (nlk:encode-json-object message)
                    :timeout (setting :timeout-seconds))
        (values (messages-of (and (stringp text) text)) status headers))
    (corpus-error (condition) (error condition))
    (error (condition)
      (fail "~a could not be reached: ~a" (endpoint) (one-line condition :cap 200)))))

(defun notify-initialized ()
  "Tell the server the handshake is done. Its answer is never read: a
refused note changes nothing about the calls that follow."
  (ignore-errors
    (nlk:http :post (endpoint)
              :headers (request-headers)
              :content (nlk:encode-json-object
                        (nlk:json-object "jsonrpc" "2.0"
                                         "method" "notifications/initialized"))
              :timeout (setting :timeout-seconds)))
  nil)

(defun initialize ()
  "Claim a session. A server that wants none answers with a client-error
status, and the calls then go out alone; a server that fails hard leaves the
next call to say why."
  (multiple-value-bind (messages status headers)
      (post (nlk:json-object "jsonrpc" "2.0"
                              "id" (rpc-id)
                              "method" "initialize"
                              "params" (nlk:json-object
                                         "protocolVersion" +protocol-version+
                                         "capabilities" (nlk:json-object)
                                         "clientInfo" (nlk:json-object
                                                        "name" +client-name+
                                                        "version" +client-version+))))
    (declare (ignore messages))
    (if (and (integerp status) (<= 200 status 299))
        (progn
          (setf *session-id* (header-value headers "mcp-session-id"))
          (notify-initialized)
          t)
        (progn (setf *session-id* nil
                     *stateless-p* (and (integerp status) (< status 500)))
               nil))))

(defun message-for (messages id)
  "The message answering ID; failing that, the last one carrying a result or
an error -- some servers answer with an id of their own."
  (or (find-if (lambda (message) (equal (gethash "id" message) id)) messages)
      (find-if (lambda (message) (or (gethash "result" message) (gethash "error" message)))
               messages)))

(defun error-text (error)
  "The server's own sentence for ERROR, one line."
  (cond
    ((hash-table-p error)
     (or (nlk:json-value error :string "message")
         (let ((data (gethash "data" error)))
           (and data (one-line (nlk:encode-json-object data) :cap 200)))
         "the server refused the call"))
    ((stringp error) (one-line error :cap 200))
    (t "the server refused the call")))

(defun content-text (result)
  "RESULT's text content joined line by line, or NIL when it carries none."
  (let* ((parts (and (hash-table-p result) (nlk:json-array result "content")))
         (texts (loop for part across (or parts #())
                      for text = (nlk:json-value part :string "text")
                      when text collect text)))
    (and texts (format nil "~{~a~^~%~}" texts))))

(defun refusal-text (name status messages)
  "What to say about STATUS on a tools/call for NAME."
  (let* ((answer (message-for messages nil))
         (error (and answer (gethash "error" answer))))
    (cond
      ((eql status 429)
       (format nil "~a: Corpus is rate limiting the anonymous floor (HTTP 429) -- wait, or ~
                    put a free key from https://corpuslaw.us/settings in api_key_env for a ~
                    higher limit" name))
      (error (format nil "~a: the server answered HTTP ~a: ~a" name status (error-text error)))
      (t (format nil "~a: the server answered HTTP ~a" name status)))))

(defun rpc-call (name arguments &key (retry t))
  "Call the MCP tool NAME with ARGUMENTS -- a JSON object this client built.
=> the text its content carries, which is what the model reads."
  (unless (or *session-id* *stateless-p*) (initialize))
  (let ((id (rpc-id)))
    (multiple-value-bind (messages status headers)
        (post (nlk:json-object "jsonrpc" "2.0"
                                "id" id
                                "method" "tools/call"
                                "params" (nlk:json-object "name" name
                                                           "arguments" arguments)))
      (declare (ignore headers))
      (unless (and (integerp status) (<= 200 status 299))
        (when (and retry *session-id* (member status '(400 404 410)))
          ;; The session went stale under us: claim another and ask once more.
          (setf *session-id* nil *stateless-p* nil)
          (return-from rpc-call (rpc-call name arguments :retry nil)))
        (fail "~a" (refusal-text name status messages)))
      (let* ((answer (message-for messages id))
             (error (and answer (gethash "error" answer)))
             (result (and answer (gethash "result" answer)))
             (text (content-text result)))
        (when error (fail "~a: ~a" name (error-text error)))
        (when (and (hash-table-p result) (gethash "isError" result))
          (fail "~a: ~a" name (if text (one-line text :cap 400) "the server reported the call failed")))
        (or text
            (and (hash-table-p result) (nlk:encode-json-object result))
            (fail "~a answered nothing this client could read" name))))))
