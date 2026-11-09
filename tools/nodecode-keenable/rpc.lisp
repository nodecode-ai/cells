;;;; rpc.lisp --- the MCP wire: one JSON-RPC message a POST, one answer back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin did not implement MCP: it handed its harness a
;;;; connection entry -- https://api.keenable.ai/mcp, streamable HTTP, one
;;;; static label header -- and Hermes's own MCP client spoke the protocol.
;;;; Nodecode has such a client too (the nodecode-mcp cell), but a cell
;;;; never depends on another entry, so this is the one exchange the two
;;;; tools need, written here: POST the JSON-RPC message, read the answer as
;;;; one JSON object or as an event stream, and lift the result's text.
;;;;
;;;; The seal: every byte leaves through HTTP, and HTTP is one function a
;;;; test stubs. Nothing here touches a file, the environment or a process.

(in-package #:nodecode-keenable)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks; the server answers with its own.")

(defparameter +label+ "nodecode-cell"
  "The value of X-Keenable-Title: which client is calling, as the original
plugin sent hermes-plugin. A label, never an identity or a bypass.")

(defparameter *request-id* 0
  "The JSON-RPC request id counter; one call, one id.")

(defparameter +detail-chars+ 400
  "How much of a failed answer's body a refusal quotes.")

(defun clipped (text)
  "TEXT at most +DETAIL-CHARS+ characters; NIL is the empty string."
  (let ((text (or text "")))
    (if (<= (length text) +detail-chars+)
        text
        (format nil "~a [~d more characters]" (subseq text 0 +detail-chars+)
                (- (length text) +detail-chars+)))))

;;; --- the one seam ---------------------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS HEADERS), every status a value."
  (nlk:http method url :headers headers :content content :timeout (or timeout 30)))

(defun configured-key ()
  "The Keenable API key the section carries, or NIL: keyless is the shape
the original shipped, and a key only lifts its rate limits."
  (let ((key (setting :api-key)))
    (and (stringp key) (plusp (length (nlk:trimmed key))) (nlk:trimmed key))))

(defun redact (text)
  "TEXT with the configured key replaced by [redacted]; the detail of every
refusal passes through here."
  (let ((key (ignore-errors (configured-key))))
    (if key
        (uiop:frob-substrings (princ-to-string text) (list key) "[redacted]")
        (princ-to-string text))))

(defun request-headers ()
  "The headers every POST carries: JSON in, JSON or an event stream back,
the static label, and the key when the operator set one."
  (append `(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("x-keenable-title" . ,+label+))
          (nlk:when-let (key (configured-key))
            (list (cons "x-api-key" key)))))

;;; --- one message out, one back ----------------------------------------------------

(defun envelope (method params id)
  "The JSON-RPC message for METHOD with PARAMS; no ID makes it a notification."
  (nlk:json-object "jsonrpc" "2.0" :opt "id" id
                 "method" method "params" (or params (nlk:json-object))))

(defun event-payloads (text)
  "TEXT's messages as JSON strings: one per `data:' line when the body is an
event stream, the whole body when it is one JSON object."
  (let ((data (remove-if-not (lambda (line) (uiop:string-prefix-p "data:" line))
                              (nlk:lines (or text "")))))
    (if data
        (loop for line in data
              for payload = (nlk:trimmed (subseq line 5))
              when (plusp (length payload)) collect payload)
        (let ((text (nlk:trimmed (or text ""))))
          (and (plusp (length text)) (list text))))))

(defun messages-of (text)
  "TEXT as JSON objects, dropping a payload that is not one."
  (loop for payload in (event-payloads text)
        for value = (ignore-errors (nlk:decode-json payload))
        when (hash-table-p value) collect value))

(defun message-id (message)
  "MESSAGE's id, or NIL."
  (ignore-errors (nlk:json-value message :integer "id")))

(defun reply-of (messages id)
  "The message answering request ID, else the newest answer in MESSAGES."
  (or (find-if (lambda (message)
                 (let ((own (message-id message)))
                   (and own (= own id))))
               messages)
      (find-if (lambda (message)
                 (or (nlk:json-value message :object "result")
                     (nlk:json-value message :object "error")))
               messages)
      (first messages)))

(defun rpc (method &optional params &key id)
  "One JSON-RPC request, POSTed to the section's endpoint. => its answer
message, with a server error already signalled."
  (let ((id (or id (incf *request-id*)))
        (url (setting :endpoint))
        (timeout (setting :timeout-seconds)))
    (multiple-value-bind (body status)
        (http :post url :headers (request-headers)
               :content (nlk:encode-json-object (envelope method params id))
               :timeout timeout)
      (let* ((messages (messages-of body))
             (reply (and messages (reply-of messages id)))
             (error (and reply (nlk:json-value reply :object "error"))))
        (cond
          (error
           (fail "Keenable answered error ~a: ~a~@[, HTTP ~d~]"
                 (or (ignore-errors (nlk:json-value error :integer "code")) "?")
                 (or (nlk:json-value error :string "message") (clipped body))
                 (and (>= status 400) status)))
          ((>= status 400)
           (fail "the Keenable MCP server answered HTTP ~d: ~a" status (clipped (or body ""))))
          ((null reply)
           (fail "the Keenable MCP server answered HTTP ~d with no JSON-RPC message: ~a"
                 status (clipped (or body ""))))
          (t reply))))))

(defun notify (method &optional params)
  "One JSON-RPC notification; no answer is read."
  (ignore-errors
   (http :post (setting :endpoint) :headers (request-headers)
         :content (nlk:encode-json-object (envelope method params nil))
         :timeout (setting :timeout-seconds)))
  nil)

(defun handshake ()
  "initialize, then notifications/initialized, as MCP asks before the first
tools/call. => the server's initialize result."
  (let* ((reply (rpc "initialize"
                      (nlk:json-object
                       "protocolVersion" +protocol-version+
                       "capabilities" (nlk:json-object)
                       "clientInfo" (nlk:json-object "name" "nodecode-cell" "version" "0.1.0"))))
         (result (nlk:json-value reply :object "result")))
    (notify "notifications/initialized")
    result))

;;; --- the answer as text ------------------------------------------------------------

(defun result-text (result)
  "RESULTS's text blocks joined by a blank line; an answer that carries no
content falls back to its structured content and then to the result itself."
  (let ((content (nlk:json-value result :array "content")))
    (cond
      ((and content (plusp (length content)))
       (let ((blocks (loop for block across content
                           for text = (nlk:json-value block :string "text")
                           when text collect text)))
         (format nil "~{~a~^~2%~}" blocks)))
      ((nlk:json-value result :object "structuredContent")
       (nlk:encode-json-object (nlk:json-value result :object "structuredContent")))
      ((hash-table-p result) (nlk:encode-json-object result))
      (t ""))))

(defun tools-call-once (name arguments)
  "tools/call NAME with the JSON object ARGUMENTS against Keenable. => the
text its result carries, as the model reads it."
  (let* ((reply (rpc "tools/call"
                      (nlk:json-object "name" name "arguments" arguments)))
         (result (nlk:json-value reply :object "result")))
    (unless result
      (fail "Keenable answered ~a with no result" name))
    (let ((text (nlk:trimmed (result-text result))))
      (when (nlk:json-value result :boolean "isError")
        (fail "~a" (if (plusp (length text)) text "Keenable refused the call")))
      (unless (plusp (length text))
        (fail "Keenable answered ~a with no text" name))
      text)))

(defun call-tool (name arguments)
  "tools/call NAME; a server that wants an initialize first is handshaked once and asked again."
  (handler-case (tools-call-once name arguments)
    (keenable-error (condition)
      (if (cl:search "initialize" (princ-to-string condition))
          (progn (handshake) (tools-call-once name arguments))
          (error condition)))))
