;;;; wire.lisp --- the FlipLetters server, spoken to over Streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server is FlipLetters' own, at the section's url, and it speaks MCP:
;;;; every JSON-RPC message is one POST to the one endpoint. A request's POST
;;;; is answered either by one JSON message or by a server-sent stream whose
;;;; last event carries the reply; a notification's POST is answered 202 and
;;;; no body. The Mcp-Session-Id the initialize answer issues is echoed on
;;;; every later request, and a session the server has dropped (400 or 404 on
;;;; a call) is made again once before the call is given up on.
;;;;
;;;; The tool set is the server's, at run time: TOOLS keeps what tools/list
;;;; answered for this process, and a call names one of those tools and
;;;; passes its own arguments through. Every byte this cell moves goes
;;;; through HTTP, so a test stubs one name and never opens a socket. The key
;;;; is a header value and never appears in an answer.

(in-package #:nodecode-flipletters)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this cell asks for.")

(defparameter +client-version+ "0.1.0"
  "What this cell calls itself in the initialize handshake.")

(defvar *session-id* nil
  "The Mcp-Session-Id the server issued this process, or NIL before one has.")

(defvar *initialized* nil
  "Whether this process has initialized a session with the server.")

(defvar *tools* nil
  "The server's tools/list answer as decoded objects, kept for this process.")

(defvar *counter* 0
  "The JSON-RPC id the next request carries.")

;;; --- the section's values ------------------------------------------------------

(defun environment-value (name)
  "The value of environment variable NAME, or NIL. A test stubs this name."
  (uiop:getenv name))

(defun api-key ()
  "The key: api_key written inline, else the value api_key_env names. NIL
when neither is set or what was named is empty."
  (let ((inline (let ((key (ignore-errors (setting :api-key))))
                  (and (stringp key) (plusp (length (nlk:trimmed key)))
                       (nlk:trimmed key)))))
    (or inline
        (let ((name (ignore-errors (setting :api-key-env))))
          (when (and name (plusp (length name)))
            (let ((value (ignore-errors (environment-value name))))
              (and value (plusp (length (nlk:trimmed value)))
                   (nlk:trimmed value))))))))

(defun forget-session ()
  "Drop the handshake and the tool list: the next call makes a new session."
  (setf *initialized* nil *session-id* nil *tools* nil))

;;; --- one exchange ---------------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network seam -- NLK:HTTP. A test stubs NLK:HTTP itself."
  (apply #'nlk:http method url arguments))

(defun request-headers ()
  "The headers every POST carries; the key is one of them."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . "2025-06-18"))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (list (cons "authorization" (format nil "Bearer ~a" (api-key))))))

(defun header (headers name)
  "NAME's value in HEADERS, a response-header table with lowercase names."
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
  "POST OBJECT, one JSON-RPC message. => (values MESSAGE STATUS)."
  (multiple-value-bind (body status headers)
      (http :post (setting :url) :headers (request-headers)
            :content (nlk:encode-json-object object)
            :timeout (setting :timeout-seconds)
            :connect-timeout (min 15 (setting :timeout-seconds)))
    (let ((session (header headers "mcp-session-id")))
      (when (and session (stringp session) (plusp (length session)))
        (setf *session-id* session)))
    (values (message body headers) status)))

(defun describe-status (status)
  "What a bare STATUS means, in one clause, without a header or a body."
  (format nil "the server answered ~d~@[ (~a)~]" status
          (case status
            (401 "the key was refused: check the variable api_key_env names, or api_key")
            (403 "the key may not use this account")
            (404 "the endpoint or the session is gone")
            (429 "too many calls for now")
            (t nil))))

(defun request (method &optional params)
  "One JSON-RPC request. => (values RESULT ERROR-TEXT STATUS)."
  (let* ((id (incf *counter*))
         (object (nlk:json-object "jsonrpc" "2.0" "id" id "method" method :opt "params" params)))
    (multiple-value-bind (reply status) (post object)
      (let ((reported (and reply (nlk:json-value reply :object "error"))))
        (cond
          (reported
           (values nil (format nil "FlipLetters refused ~a: ~a~@[ (~d)~]"
                               method
                               (or (nlk:json-value reported :string "message") "no reason given")
                               (nlk:json-value reported :integer "code"))
                   status))
          ((and (integerp status) (>= status 300))
           (let ((said (and reply (nlk:one-line
                                   (or (nlk:json-value reply :string "message") "")))))
             (values nil (format nil "FlipLetters: ~a~@[: ~a~]" (describe-status status)
                                 (and said (plusp (length said)) said))
                     status)))
          ((null reply)
           (values nil (format nil "FlipLetters: ~a" (describe-status status)) status))
          (t (values (nlk:json-value reply :object "result") nil status)))))))

(defun notify (method &optional params)
  "One JSON-RPC notification, whose answer there is nothing to read."
  (ignore-errors
    (post (nlk:json-object "jsonrpc" "2.0" "method" method :opt "params" params))))

(defun initialize ()
  "Make this process's session once. => ERROR-TEXT or NIL."
  (if *initialized*
      nil
      (multiple-value-bind (result error)
          (request "initialize"
                   (nlk:json-object
                    "protocolVersion" +protocol-version+
                    "capabilities" (nlk:json-object)
                    "clientInfo" (nlk:json-object
                                  "name" "nodecode-flipletters"
                                  "version" +client-version+)))
        (declare (ignore result))
        (if error
            error
            (progn (setf *initialized* t)
                   (notify "notifications/initialized")
                   nil)))))

(defun call (method &optional params retry)
  "Call METHOD through an initialized session, and again from a fresh one when
the server says the session is gone. => (values RESULT ERROR-TEXT)."
  (let ((error (initialize)))
    (when error (return-from call (values nil error)))
    (multiple-value-bind (result error status) (request method params)
      (if (and error retry (member status '(400 404)))
          (progn (forget-session)
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
  "TOOL's name as the server lists it."
  (or (nlk:json-value tool :string "name") "?"))

(defun tool-description (tool)
  "TOOL's description, or the empty string."
  (or (nlk:json-value tool :string "description") ""))

(defun required-arguments (tool)
  "The argument names TOOL's input schema marks required, in its order."
  (coerce (nlk:json-array (nlk:json-value tool :object "inputSchema") "required") 'list))

(defun invoke (name arguments)
  "One tools/call of NAME with ARGUMENTS, an object. => (values RESULT ERROR-TEXT)."
  (call "tools/call"
        (nlk:json-object "name" name "arguments" (or arguments (nlk:json-object)))
        t))
