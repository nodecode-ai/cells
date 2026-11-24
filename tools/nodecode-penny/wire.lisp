;;;; wire.lisp --- the one network seam: JSON-RPC over MCP's streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin pointed Hermes at Penny's hosted MCP server; a cell has no MCP
;;;; client, so this file is the small one it needs. Every JSON-RPC message is
;;;; POSTed to the one endpoint URL. A request is answered with one JSON
;;;; message or with an SSE body carrying the reply; the Mcp-Session-Id
;;;; header initialize issues is echoed on the rest of that call's messages.
;;;; Nothing is kept between calls -- one tool call is initialize,
;;;; notifications/initialized, tools/call -- so a restart never holds a stale
;;;; session, and a tool call never borrows another account's.
;;;;
;;;; Every byte goes through POST, so a test stubs NLK:HTTP and never touches
;;;; the network. A refused status or a transport failure is PENNY-ERROR with
;;;; a line that never carries the token, which is what the model repairs from.

(in-package #:nodecode-penny)

(defparameter +endpoint+ "https://app.mypenny.ai/mcp"
  "Penny's public MCP endpoint: where every call goes.")

(defparameter +protocol-version+ "2025-03-26"
  "The MCP revision Penny speaks; it refuses a session negotiated at another.")

(defparameter +client-name+ "nodecode-penny")
(defparameter +client-version+ "0.1.0")

(defparameter +max-response-bytes+ (* 2 1024 1024)
  "One answer's ceiling, as the plugin bounded it: a memory answer is text.")

(defvar *request-id* 0
  "The JSON-RPC id of the last message this cell sent.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun home-expanded (path)
  "PATH with a leading ~/ read as the home directory, else PATH."
  (let ((trimmed (nlk:trimmed path)))
    (if (uiop:string-prefix-p "~/" trimmed)
        (let ((home (uiop:getenv "HOME")))
          (if (and home (plusp (length home)))
              (merge-pathnames (subseq trimmed 2) (concatenate 'string home "/"))
              trimmed))
        trimmed)))

(defun path-string (value)
  "VALUE -- a config path -- as a string: a :path setting may arrive either way."
  (typecase value
    (string value)
    (pathname (uiop:native-namestring value))
    (t nil)))

(defun file-token (path)
  "The access token PATH holds: the file's own text, or the access_token a
JSON file written by a Penny CLI keeps (top level, under tokens, or camel
cased). NIL when the file is missing or holds none."
  (let ((text (handler-case (uiop:read-file-string (home-expanded path))
                (error () nil))))
    (when text
      (let ((trimmed (nlk:trimmed text)))
        (cond
          ((zerop (length trimmed)) nil)
          ((char= #\{ (char trimmed 0))
           (let ((json (handler-case (nlk:decode-json trimmed) (error () nil))))
             (and (hash-table-p json)
                  (or (nlk:json-value json :string "access_token")
                      (nlk:json-value json :string "accessToken")
                      (nlk:json-value json :string "tokens" "access_token")
                      (nlk:json-value json :string "tokens" "accessToken")))))
          (t (nonempty trimmed)))))))

(defun api-token ()
  "The Penny access token: the section's own, the variable it names, the file
it names, else PENNY_TOKEN."
  (or (nonempty (setting :token))
      (let ((variable (nonempty (setting :token-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (let ((path (path-string (setting :token-file))))
        (and (nonempty path) (file-token path)))
      (nonempty (uiop:getenv "PENNY_TOKEN"))))

(defun check-config ()
  "Refuse a section that would reach Penny with no token, naming the member."
  (let ((token (api-token)))
    (unless token
      (error 'nlk:config-refusal
             :detail (concatenate 'string
                                  "penny: this cell needs a Penny access token: set \"token\" in the penny "
                                  "section, or name the variable holding it in \"token_env\", or the file "
                                  "holding it in \"token_file\" (a Penny CLI's auth.json works as it is); "
                                  "PENNY_TOKEN is read too")))
    (when (uiop:string-prefix-p "mpt_" token)
      (error 'nlk:config-refusal
             :detail (concatenate 'string
                                  "penny: an mpt_ key is Penny's API key, not the access token the MCP "
                                  "endpoint takes; set the token Penny's sign-in hands out")))))

(defun endpoint ()
  "The endpoint URL this cell talks to."
  (let ((given (setting :endpoint)))
    (or (and (stringp given) (nonempty (nlk:trimmed given))) +endpoint+)))

(defun message-headers (session-id)
  "The headers every message carries: the token, JSON, both answer shapes,
and the session id once initialize answered with one."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (or (api-token) "")))
                (cons "mcp-protocol-version" +protocol-version+)
                (cons "user-agent" (format nil "~a/~a" +client-name+ +client-version+)))
          (when (nonempty session-id)
            (list (cons "mcp-session-id" session-id)))))

(defun response-header (headers name)
  "NAME's value from a decoded header table."
  (and (hash-table-p headers) (gethash name headers)))

(defun decode-message (text)
  "TEXT as one JSON-RPC message, or NIL when it is not a JSON object."
  (let ((value (and (stringp text)
                    (plusp (length (nlk:trimmed text)))
                    (handler-case (nlk:decode-json text) (error () nil)))))
    (and (hash-table-p value) value)))

(defun reply-message-p (message)
  "Whether MESSAGE answers a request: it carries a result or an error."
  (and message (or (nth-value 1 (gethash "result" message))
                   (nth-value 1 (gethash "error" message)))))

(defun sse-messages (text)
  "The `data:' events of an SSE body TEXT, as decoded messages, in order."
  (let ((messages '())
        (data '()))
    (flet ((dispatch ()
             (when data
               (let ((message (decode-message
                               (format nil "~{~a~^~%~}" (nreverse (shiftf data '()))))))
                 (when message (push message messages))))))
      (with-input-from-string (stream text)
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((line (string-right-trim '(#\Return) line)))
                   (cond ((zerop (length line)) (dispatch))
                         ((uiop:string-prefix-p "data:" line)
                          (push (string-left-trim " " (subseq line 5)) data))))))
      (dispatch))
    (nreverse messages)))

(defun post (message &key session-id)
  "POST one JSON-RPC MESSAGE to the endpoint => (values MESSAGES STATUS
SESSION-ID). An SSE answer is read up to its reply; a notification's 202
carries none."
  (let ((timeout (setting :timeout-seconds))
        (body nil)
        (status nil)
        (headers nil))
    (handler-case
        (multiple-value-setq (body status headers)
          (nlk:http :post (endpoint)
                    :headers (message-headers session-id)
                    :content message
                    :timeout timeout
                    :connect-timeout (min 10 timeout)
                    :stream t))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "penny: ~a" (nle:transport-failure-label condition (endpoint)))))
    (let ((messages '())
          (type (string-downcase (or (response-header headers "content-type") ""))))
      (flet ((collect (message) (when message (push message messages))))
        (cond
          ((streamp body)
           (unwind-protect
                (cond ((member status '(202 204)) nil)
                      ((uiop:string-prefix-p "text/event-stream" type)
                       (nlk:read-events body
                                         (lambda (text)
                                           (let ((message (decode-message text)))
                                             (collect message)
                                             (reply-message-p message)))
                                         :idle timeout
                                         :limit +max-response-bytes+))
                      (t (collect (decode-message (nlk:body-text body +max-response-bytes+)))))
             (ignore-errors (close body :abort t))))
          ((stringp body)
           (if (uiop:string-prefix-p "text/event-stream" type)
               (mapc #'collect (sse-messages body))
               (collect (decode-message body))))
          ((vectorp body) (collect (decode-message (nlk:decode-octets body type))))))
      (values (nreverse messages) status
              (response-header headers "mcp-session-id")))))

(defun json-line (value limit)
  "VALUE -- decoded JSON -- as one clipped line, for a failure's report."
  (let ((text (handler-case (nlk:encode-json-object value) (error () (format nil "~s" value)))))
    (if (and (stringp text) (> (length text) limit))
        (concatenate 'string (subseq text 0 limit) "...")
        (format nil "~a" text))))

(defun error-text (error)
  "ERROR -- a JSON-RPC error object -- as the one line the model reads."
  (or (and (hash-table-p error) (nlk:json-value error :text "message"))
      (and (hash-table-p error) (nlk:json-value error :text "data"))
      (json-line error 300)))

(defun refused-text (status)
  "One line for a refused STATUS: 401 and 403 say the token, not the server, is the problem."
  (cond ((member status '(401 403))
         (format nil "penny: the token was refused (HTTP ~a): connect Penny again, then set the new token in the penny section" status))
        (t (format nil "penny: the request failed (HTTP ~a)" status))))

(defun request (method params &key session-id)
  "One JSON-RPC request METHOD => (values RESULT SESSION-ID)."
  (let* ((id (incf *request-id*))
         (text (nlk:encode-json-object
                (nlk:make-json-object "jsonrpc" "2.0" "id" id
                                       "method" method "params" params))))
    (multiple-value-bind (messages status session)
        (post text :session-id session-id)
      (let ((reply (find-if #'reply-message-p messages)))
        (cond
          ((null reply)
           (if (and status (>= status 400))
               (fail "~a" (refused-text status))
               (fail "penny ~a: HTTP ~a and no JSON-RPC answer~@[: ~a~]"
                     method status
                     (and messages (json-line (first messages) 300)))))
          ((nth-value 1 (gethash "error" reply))
           (fail "penny ~a: ~a" method (error-text (gethash "error" reply))))
          (t (values (gethash "result" reply) session)))))))

(defun notify (method)
  "One JSON-RPC notification: the server answers 202 and nothing else."
  (post (nlk:encode-json-object
         (nlk:make-json-object "jsonrpc" "2.0" "method" method "params" (nlk:json-object)))))

(defun open-session ()
  "initialize, then notifications/initialized => the session id Penny issued."
  (multiple-value-bind (result session)
      (request "initialize"
               (nlk:make-json-object
                "protocolVersion" +protocol-version+
                "capabilities" (nlk:json-object)
                "clientInfo" (nlk:make-json-object "name" +client-name+
                                                    "version" +client-version+)))
    (declare (ignore result))
    (notify "notifications/initialized")
    session))

(defun call-tool (name arguments)
  "Call Penny's tool NAME with ARGUMENTS -- a JSON object -- => its MCP result."
  (let ((session (open-session)))
    (request "tools/call"
             (nlk:make-json-object "name" name
                                    "arguments" (or arguments (nlk:json-object)))
             :session-id session)))

(defun result-value (result)
  "RESULT -- an MCP tool result -- as the value Penny answered: its
structuredContent, else the first text block that parses as JSON, else that
block's text."
  (when (nlk:json-value result :boolean "isError")
    (fail "penny: the server refused the memory request~@[: ~a~]"
          (let ((first-block (aref (or (nlk:json-value result :array "content") #()) 0)))
            (and (hash-table-p first-block) (nlk:json-value first-block :text "text")))))
  (let ((structured (nlk:json-value result :object "structuredContent")))
    (if structured
        (multiple-value-bind (value present) (gethash "value" structured)
          (if present value structured))
        (let ((blocks (nlk:json-value result :array "content")))
          (or (loop for block across (or blocks #())
                    when (and (hash-table-p block)
                              (equal "text" (nlk:json-value block :string "type")))
                      return (let ((text (nlk:json-value block :text "text")))
                               (or (and text (handler-case (nlk:decode-json text) (error () nil)))
                                   text)))
              (fail "penny: the server answered a memory request in no shape this cell reads"))))))

(defun call (name arguments)
  "NAME called the way Penny's tools are called: the value its answer carries."
  (result-value (call-tool name arguments)))
