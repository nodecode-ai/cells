;;;; wire.lisp --- the athletedata MCP server, spoken to over Streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server is someone else's, at the section's url, and it speaks MCP:
;;;; every JSON-RPC message is one POST to the one endpoint. A request's POST
;;;; is answered either by one JSON message or by a server-sent stream whose
;;;; last event carries the reply; a notification's POST is answered 202 and
;;;; no body. The Mcp-Session-Id the initialize answer issues is echoed on
;;;; every later request, and the whole session is dropped and made again
;;;; once when a request comes back 400 or 404.
;;;;
;;;; The server's tool set is served at run time, not here: TOOLS answers
;;;; what tools/list said, kept for this process, and a call names one of
;;;; them. Every byte this cell moves goes through HTTP, so a test stubs that
;;;; one name and never opens a socket. A header value is a secret and never
;;;; appears in an answer: a failed status reports the code only.

(in-package #:nodecode-athletedata)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this cell asks for.")

(defparameter +client-version+ "0.1.0")

(defvar *session-id* nil
  "The Mcp-Session-Id the server issued this process, or NIL before one has.")

(defvar *initialized* nil
  "Whether this process has initialized a session with the server.")

(defvar *tools* nil
  "The server's tools/list answer, as decoded objects, kept for this process.")

(defvar *counter* 0
  "The JSON-RPC id the next request carries.")

;;; --- the section's values ----------------------------------------------------

(defun environment-value (name)
  "The value of environment variable NAME, or NIL. A test stubs this name."
  (uiop:getenv name))

(defun api-key ()
  "The key the section's api_key_env names, or NIL when that variable is unset or empty."
  (let ((name (setting :api-key-env)))
    (when (and name (plusp (length name)))
      (let ((value (ignore-errors (environment-value name))))
        (when (and value (plusp (length (nlk:trimmed value))))
          (nlk:trimmed value))))))

(defun check-config ()
  "Refuse a section the server could never be reached with, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (format nil "athletedata: no API key: set the environment variable ~a to the ~
sk_soma_... key from the athletedata dashboard, then (restart-cells \"nodecode-athletedata\")"
                           (or (setting :api-key-env) "ATHLETEDATA_API_KEY")))))

;;; --- one exchange -------------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network seam -- NLK:HTTP. A test stubs this name."
  (apply #'nlk:http method url arguments))

(defun request-headers ()
  "The headers every POST carries; the key is one of them and is never reported."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . "2025-06-18"))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (list (cons "authorization" (format nil "Bearer ~a" (api-key))))))

(defun header (headers name)
  "NAME's value in HEADERS, a dexador table with lowercase names, or NIL."
  (and (hash-table-p headers) (gethash name headers)))

(defun sse-events (text)
  "TEXT, a server-sent stream, as its data payloads, oldest first."
  (let ((events '()) (data '()))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((line (string-right-trim '(#\Return) line)))
        (cond ((uiop:string-prefix-p "data:" line)
               (push (string-left-trim " " (subseq line 5)) data))
              ((zerop (length (nlk:trimmed line)))
               (when data
                 (push (format nil "~{~a~^~%~}" (nreverse data)) events)
                 (setf data '()))))))
    (when data (push (format nil "~{~a~^~%~}" (nreverse data)) events))
    (nreverse events)))

(defun message-text (body headers)
  "BODY as the JSON text of one JSON-RPC message: the body itself, or an
event stream's last data payload."
  (let ((type (or (header headers "content-type") ""))
        (text (or body "")))
    (if (or (search "text/event-stream" type :test #'char-equal)
            (uiop:string-prefix-p "data:" (string-left-trim '(#\Space #\Tab #\Newline #\Return) text))
            (uiop:string-prefix-p "event:" (string-left-trim '(#\Space #\Tab #\Newline #\Return) text)))
        (or (car (last (sse-events text))) "")
        text)))

(defun message (body headers)
  "The decoded JSON-RPC message BODY carries, or NIL for an empty body."
  (let ((text (message-text body headers)))
    (when (plusp (length (nlk:trimmed text)))
      (handler-case (nlk:decode-json text :whole t) (error () nil)))))

(defun post (object)
  "POST OBJECT, one JSON-RPC message. => (values MESSAGE STATUS), MESSAGE NIL
for a 202 or a body that is not JSON."
  (multiple-value-bind (body status headers)
      (http :post (setting :url) :headers (request-headers)
            :content (nlk:encode-json-object object)
            :timeout (setting :timeout-seconds))
    (let ((session (header headers "mcp-session-id")))
      (when (and session (stringp session) (plusp (length session)))
        (setf *session-id* session)))
    (values (message body headers) status)))

(defun describe-status (status)
  "What a bare STATUS means, in one clause, without a header or a body. The
code always rides, so a bare refusal is still a fact the operator can act on."
  (format nil "the server answered ~d~@[ (~a)~]" status
          (case status
            (401 "the key was refused: check the variable api_key_env names")
            (403 "the key may not use this server")
            (404 "the endpoint or the session is gone")
            (429 "too many calls for now")
            (t nil))))

(defun request (method &optional params)
  "One JSON-RPC request. => (values RESULT ERROR-TEXT STATUS)."
  (let* ((id (incf *counter*))
         (object (nlk:json-object "jsonrpc" "2.0" "id" id "method" method :opt "params" params)))
    (multiple-value-bind (reply status) (post object)
      (cond ((null reply) (values nil (format nil "athletedata: ~a" (describe-status status)) status))
            (t (let ((error (nlk:json-value reply :object "error")))
                 (if error
                     (values nil (format nil "athletedata: the server refused ~a: ~a~@[ (~d)~]"
                                         method
                                         (or (nlk:json-value error :string "message") "no reason given")
                                         (nlk:json-value error :integer "code"))
                             status)
                     (values (nlk:json-value reply :object "result") nil status))))))))

(defun notify (method &optional params)
  "One JSON-RPC notification, whose answer there is nothing to read."
  (ignore-errors
    (post (nlk:json-object "jsonrpc" "2.0" "method" method :opt "params" params))))

(defun initialize ()
  "Make this process's session once. => ERROR-TEXT or NIL."
  (if *initialized*
      nil
      (multiple-value-bind (result error) (request
                                            "initialize"
                                            (nlk:json-object
                                             "protocolVersion" +protocol-version+
                                             "capabilities" (nlk:json-object)
                                             "clientInfo" (nlk:json-object
                                                           "name" "nodecode-athletedata"
                                                           "version" +client-version+)))
        (declare (ignore result))
        (if error
            error
            (progn (setf *initialized* t)
                   (notify "notifications/initialized")
                   nil)))))

(defun call (method &optional params retry)
  "Call METHOD once through an initialized session, and again from a fresh
one when the server says the session is gone. => (values RESULT ERROR-TEXT)."
  (let ((error (initialize)))
    (when error (return-from call (values nil error)))
    (multiple-value-bind (result error status) (request method params)
      (if (and error retry (member status '(400 404)))
          (progn (setf *initialized* nil *session-id* nil *tools* nil)
                 (call method params nil))
          (values result error)))))

;;; --- the server's tools ----------------------------------------------------------

(defun tools-result (&optional refresh)
  "The server's tools, asked once per process unless REFRESH.
=> (values TOOLS ERROR-TEXT)."
  (when (or refresh (null *tools*))
    (multiple-value-bind (result error) (call "tools/list" (nlk:json-object) t)
      (if error
          (return-from tools-result (values nil error))
          (setf *tools* (coerce (nlk:json-array result "tools") 'list)))))
  (values *tools* nil))

(defun tool-name (tool)
  (or (nlk:json-value tool :string "name") "?"))

(defun read-only-p (tool)
  "Whether TOOL says it only reads: an annotation that is absent reads as a
tool that writes, which is the side to be wrong on."
  (and (nth-value 0 (nlk:json-value tool :boolean "annotations" "readOnlyHint")) t))

(defun tool-line (tool)
  "One line for TOOL: its name, a * when it writes, and its first sentence."
  (let ((description (or (nlk:json-value tool :string "description") "")))
    (format nil "~a~:[~; *~] - ~a"
            (tool-name tool)
            (not (read-only-p tool))
            (nlk:one-line description :cap 220))))

(defun format-tools (tools)
  "TOOLS as one answer for the model."
  (if (null tools)
      "athletedata: the server lists no tools for this account."
      (format nil "~d athletedata tools, a * marking one that writes to the athlete's accounts:~%~{~a~%~}"
              (length tools)
              (loop for tool in tools collect (tool-line tool)))))

(defun format-result (result)
  "RESULT, a tools/call answer, as text the model reads."
  (let ((parts '()))
    (loop for part across (nlk:json-array result "content")
          do (push (or (nlk:json-value part :string "text")
                       (format nil "[~a content]" (or (nlk:json-value part :string "type") "unreadable")))
                   parts))
    (format nil "~@[error: ~]~a"
            (nth-value 0 (nlk:json-value result :boolean "isError"))
            (if parts
                (format nil "~{~a~^~%~}" (nreverse parts))
                (nlk:encode-json-object result)))))
