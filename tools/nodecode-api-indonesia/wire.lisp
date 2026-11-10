;;;; wire.lisp --- the two ways API Indonesia serves its data, with NLK:HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The first is the product the plugin wraps: a hosted MCP server at the
;;;; section's url, which serves its own tool set at run time, so this file
;;;; keeps no copy of it -- TOOLS asks, and CALL names one of the answers.
;;;; Streamable HTTP: every JSON-RPC message is one POST to the one endpoint;
;;;; a request is answered by one JSON message or by a server-sent stream
;;;; whose last event carries the reply, a notification by a bare 202. The
;;;; Mcp-Session-Id the initialize answer issues rides every later request,
;;;; and the whole session is dropped and made again once when a request comes
;;;; back 400 or 404.
;;;;
;;;; The second is the REST API the original's skill documents, same data and
;;;; same key at the section's rest_url: GET <rest_url><path>?<query>. A tool
;;;; set served by someone else's server can gain or lose a verb between two
;;;; launches; the documented paths are the fallback that does not.
;;;;
;;;; Every byte this cell moves goes through HTTP, so a test stubs that one
;;;; name and never opens a socket. A header value is a secret and never
;;;; appears in an answer: a failed status reports the code only.

(in-package #:nodecode-api-indonesia)

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
           :detail (format nil "api-indonesia: no API key: set the environment variable ~a to the ~
aip_live_... key from the API Indonesia dashboard, then (restart-cells \"nodecode-api-indonesia\")"
                           (or (setting :api-key-env) "API_INDONESIA_KEY")))))

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

(defun rest-headers ()
  "The headers the REST fallback carries: the same key, as the original's examples send it."
  (list (cons "x-api-key" (api-key))
        (cons "accept" "application/json")))

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
            (401 "the key is missing or wrong: check the variable api_key_env names")
            (402 "the account is out of credits: top up at the API Indonesia dashboard")
            (403 "the key may not use this server, or a proxy refused it")
            (404 "the endpoint or the session is gone")
            (429 "too many calls for now")
            (t nil))))

(defun request (method &optional params)
  "One JSON-RPC request. => (values RESULT ERROR-TEXT STATUS)."
  (let* ((id (incf *counter*))
         (object (nlk:json-object "jsonrpc" "2.0" "id" id "method" method :opt "params" params)))
    (multiple-value-bind (reply status) (post object)
      (cond ((null reply) (values nil (format nil "api-indonesia: ~a" (describe-status status)) status))
            (t (let ((error (nlk:json-value reply :object "error")))
                 (if error
                     (values nil (format nil "api-indonesia: the server refused ~a: ~a~@[ (~d)~]"
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
                                                           "name" "nodecode-api-indonesia"
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

(defun tool-line (tool)
  "One line for TOOL: its name and its first sentence. Every API Indonesia
tool only reads, so none carries a mark."
  (format nil "~a - ~a"
          (tool-name tool)
          (nlk:one-line (or (nlk:json-value tool :string "description") "") :cap 220)))

(defun format-tools (tools)
  "TOOLS as one answer for the model."
  (if (null tools)
      "api-indonesia: the server lists no tools for this key."
      (format nil "~d API Indonesia tools, as the server lists them now:~%~{~a~%~}~%~
call one with api_indonesia_call (tool, arguments), or reach a documented REST path ~
with api_indonesia_get."
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

;;; --- the REST fallback -------------------------------------------------------------

(defun value-text (value)
  "VALUE, one JSON value of a query object, as the text of a query parameter."
  (typecase value
    (string value)
    (integer (princ-to-string value))
    (real (princ-to-string value))
    ((eql t) "true")
    (vector (format nil "~{~a~^,~}" (loop for item across value collect (value-text item))))
    (hash-table (nlk:encode-json-object value))
    (t (and value (princ-to-string value)))))

(defun query-pairs (query)
  "QUERY, a decoded JSON object of parameters, as (key . text) pairs to encode."
  (let ((pairs '()))
    (when (hash-table-p query)
      (maphash (lambda (key value)
                 (let ((text (value-text value)))
                   (when (and (stringp text) (plusp (length text)))
                     (push (cons key text) pairs))))
               query))
    (nreverse pairs)))

(defun without-base (path)
  "PATH without the base path the section's rest_url already carries, so a URL
copied from the docs lands under it once, not twice."
  (let* ((base (ignore-errors (quri:uri-path (quri:uri (setting :rest-url)))))
         (base (and base (string-right-trim "/" base))))
    (if (and base (plusp (length base)) (uiop:string-prefix-p base path))
        (let ((rest (subseq path (length base))))
          (if (plusp (length rest)) rest "/"))
        path)))

(defun rest-target (path query)
  "PATH as a URL under the section's rest_url: a full URL contributes its own
path and query, and QUERY's members are added to either. Nothing but the
section's host is ever addressed."
  (let* ((path (or path "/"))
         (given (and (search "://" path) (ignore-errors (quri:uri path))))
         (pairs (append (when (and given (quri:uri-query given))
                          (quri:url-decode-params (quri:uri-query given) :lenient t))
                        (query-pairs query)))
         (path (without-base (if given (or (quri:uri-path given) "/") path))))
    (format nil "~a~a~@[?~a~]"
            (string-right-trim "/" (setting :rest-url))
            (if (uiop:string-prefix-p "/" path) path (concatenate 'string "/" path))
            (and pairs (quri:url-encode-params pairs)))))

(defun format-envelope (path body status)
  "One REST answer as text: the path, the status, and the JSON the API
answered with (its data and its source metadata), or the status alone."
  (let ((text (nlk:trimmed (or body ""))))
    (if (>= status 400)
        (format nil "api-indonesia: GET ~a -> ~a~@[~%~a~]" path (describe-status status)
                (and (plusp (length text)) (nlk:one-line text :cap 400)))
        (format nil "GET ~a -> ~d~%~a" path status
                (if (plusp (length text)) text "(an empty answer)")))))

(defun rest-get (path query)
  "GET one documented REST PATH with QUERY. => the answer as text."
  (let ((url (rest-target path query)))
    (multiple-value-bind (body status)
        (http :get url :headers (rest-headers) :timeout (setting :timeout-seconds))
      (format-envelope (or path "/") body (or status 0)))))
