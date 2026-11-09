;;;; mcp.lisp --- the wire: Birta's MCP server, one JSON-RPC exchange at a time.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was a portable manifest naming one streamable-HTTP MCP server
;;;; (mcp.json beside plugin.json). This cell speaks that protocol itself --
;;;; initialize, notifications/initialized, tools/list, tools/call -- so every
;;;; byte it moves goes through POST-RPC, the one name a test stubs. A
;;;; server-sent-event answer and a plain JSON one read the same way, and the
;;;; session id the server hands out is carried back on every later request.

(in-package #:nodecode-birta)

(defparameter +endpoint+ "https://birta.dev/mcp"
  "Birta's MCP server, where every call goes unless the section names another.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this cell asks for; a server may answer an older one.")

(defparameter +token-variable+ "BIRTA_TOKEN"
  "The environment variable read for the bearer token when the section names none.")

(defparameter +client-name+ "nodecode-birta")
(defparameter +client-version+ "0.1.0")

(defvar *session* nil
  "The Mcp-Session-Id the last initialize answered, or NIL before the first.")

(defvar *opened* nil
  "True once this start has handshaked, so later calls reuse the session.")

(defvar *next-id* 0
  "The JSON-RPC id the next request carries.")

;;; --- the section, read ------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun token ()
  "The bearer token the section names inline or by variable, else BIRTA_TOKEN."
  (or (nonempty (setting :token))
      (let ((variable (nonempty (setting :token-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv +token-variable+))))

(defun check-config ()
  "Refuse a section that would reach Birta with no token, naming the member."
  (unless (token)
    (error 'nlk:config-refusal
           :detail (format nil "birta: this cell needs a token: set \"token\" (or \"token_env\") in the birta section, or export ~a; the token is the bearer token Birta's MCP server accepts after the browser sign-in at birta.dev"
                           +token-variable+))))

(defun endpoint ()
  "The MCP endpoint this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :url) ""))))
    (if (plusp (length given)) given +endpoint+)))

(defun limit ()
  "One MCP exchange's ceiling in seconds."
  (or (setting :timeout-seconds) 120))

;;; --- one request ------------------------------------------------------------

(defun request-headers ()
  "The headers every request carries: the token, the protocol revision, and
the session once there is one."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (or (token) "")))
                (cons "mcp-protocol-version" +protocol-version+))
          (when *session* (list (cons "mcp-session-id" *session*)))))

(defun body-text (body)
  "BODY -- what NLK:HTTP answered -- as a string."
  (cond ((null body) "")
        ((stringp body) body)
        ((and (vectorp body) (not (stringp body)))
         (handler-case (sb-ext:octets-to-string body :external-format :utf-8)
           (error () "")))
        (t (format nil "~a" body))))

(defun sse-payload (text)
  "TEXT with any server-sent-event framing taken off: the JSON of its last
data line, or TEXT itself when it carries none."
  (let ((data (loop for line in (uiop:split-string text :separator '(#\Newline))
                     for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
                     when (and (> (length trimmed) 5)
                               (string-equal "data:" (subseq trimmed 0 5)))
                       collect (string-trim '(#\Space #\Tab)
                                            (subseq trimmed 5)))))
    (if data (car (last data)) text)))

(defun decoded-body (body)
  "BODY as a decoded JSON object, or NIL when it carries none."
  (let ((payload (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (sse-payload (body-text body)))))
    (when (plusp (length payload))
      (handler-case (let ((value (nlk:decode-json payload)))
                      (and (hash-table-p value) value))
        (error () nil)))))

(defun post-rpc (payload)
  "POST PAYLOAD to the endpoint => the decoded answer, the status and the headers.
A transport failure signals BIRTA-ERROR."
  (handler-case
      (multiple-value-bind (body status headers)
          (nlk:http :post (endpoint)
                    :headers (request-headers)
                    :content (nlk:encode-json-object payload)
                    :timeout (limit)
                    :connect-timeout (min 30 (limit)))
        (values (decoded-body body) status headers))
    ((or error sb-sys:deadline-timeout) (condition)
      (fail "birta: ~a" (nle:transport-failure-label condition (endpoint))))))

(defun rpc-payload (method params notify)
  "The JSON-RPC request for METHOD: with PARAMS when there are any, and with
no id at all when it is a NOTIFY notification."
  (if notify
      (nlk:make-json-object "jsonrpc" "2.0" "method" method)
      (if params
          (nlk:make-json-object "jsonrpc" "2.0" "id" (incf *next-id*) "method" method "params" params)
          (nlk:make-json-object "jsonrpc" "2.0" "id" (incf *next-id*) "method" method))))

(defun header-value (headers name)
  "NAME's value in HEADERS -- an alist or a hash table -- or NIL."
  (cond
    ((hash-table-p headers)
     (or (gethash name headers)
         (loop for key being the hash-keys of headers using (hash-value value)
               when (and (stringp key) (string-equal key name)) return value)))
    ((listp headers)
     (loop for entry in headers
           when (and (consp entry) (stringp (car entry)) (string-equal (car entry) name))
             return (cdr entry)))
    (t nil)))

(defun session-gone-p (status)
  "True when STATUS says the session this cell carried no longer exists."
  (and *session* (integerp status) (member status '(404 400))))

(defun result-of (object method)
  "OBJECT's result member, a JSON-RPC error in it signalled as BIRTA-ERROR."
  (let ((problem (nlk:json-value object :object "error")))
    (when problem
      (fail "birta ~a: ~a~@[ (code ~a)~]"
            method
            (or (nlk:json-value problem :text "message") "the server refused the call")
            (nlk:json-value problem :integer "code"))))
  (nlk:json-value object :object "result"))

(defun exchange (method params notify)
  "One exchange => (values RESULT HEADERS), or :SESSION-LOST when the server
closed the session this cell was carrying."
  (multiple-value-bind (object status headers) (post-rpc (rpc-payload method params notify))
    (cond
      ((session-gone-p status) (values :session-lost headers))
      ((and (integerp status) (<= 200 status) (< status 300))
       (values (result-of object method) headers))
      ((member status '(401 403))
       (fail "birta ~a: HTTP ~a, the token was refused; refresh \"token\" in the birta section and run (restart-cells \"nodecode-birta\")"
             method status))
      (t (fail "birta ~a: HTTP ~a~@[ - ~a~]" method status
               (or (nlk:json-value (nlk:json-value object :object "error") :text "message")
                   (nlk:json-value object :text "message")))))))

(declaim (ftype (function () t) handshake))

(defun rpc (method &key params notify)
  "One JSON-RPC exchange => (values RESULT HEADERS). A NOTIFY notification
answers NIL. When the server says the session is gone, this cell handshakes
again and repeats the call once."
  (multiple-value-bind (result headers) (exchange method params notify)
    (if (not (eq result :session-lost))
        (values result headers)
        (progn
          (close-session)
          (handshake)
          (multiple-value-bind (result headers) (exchange method params notify)
            (when (eq result :session-lost)
              (fail "birta: the server keeps closing the session; check the token in the birta section"))
            (values result headers))))))

(defun close-session ()
  "Forget the session this start opened, so the next call handshakes again."
  (setf *session* nil
        *opened* nil))

(defun handshake ()
  "The protocol's opening: initialize, then the initialized notification.
The session id the answer names rides every later request."
  (let ((params (nlk:make-json-object
                 "protocolVersion" +protocol-version+
                 "capabilities" (nlk:json-object)
                 "clientInfo" (nlk:make-json-object "name" +client-name+
                                                     "version" +client-version+))))
    (multiple-value-bind (result headers) (rpc "initialize" :params params)
      (setf *session* (header-value headers "mcp-session-id")
            *opened* t)
      (rpc "notifications/initialized" :notify t)
      result)))

(defun ensure-session ()
  "Handshake once per start; every call after that reuses the session."
  (unless *opened* (handshake)))

(defun list-tools ()
  "The tools the connected server offers, each a decoded JSON object."
  (ensure-session)
  (coerce (or (nlk:json-value (rpc "tools/list") :array "tools") #()) 'list))

(defun call-tool (name arguments)
  "Call NAME on the connected server with ARGUMENTS -- a JSON object -- and
answer its result object."
  (ensure-session)
  (rpc "tools/call"
       :params (nlk:make-json-object "name" name
                                      "arguments" (or arguments (nlk:json-object)))))
