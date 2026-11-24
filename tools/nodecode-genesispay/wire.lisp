;;;; wire.lisp --- the one network seam: JSON-RPC over the catalog's MCP endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through POST-RPC or POST-NOTIFICATION, so
;;;; a test stubs NLK:HTTP once and nothing here reaches the catalog. The
;;;; endpoint speaks MCP over Streamable HTTP: a JSON-RPC POST in, the answer
;;;; as JSON or as one server-sent event out. There is no MCP runtime between
;;;; the tool and the wire -- a hello, then one tools/call per tool call -- so
;;;; the four tools stay ordinary Nodecode tools.
;;;;
;;;; The catalog keeps no account and no key: what leaves is the search text
;;;; and the filters, which is what the plugin's README says the server sees.

(in-package #:nodecode-genesispay)

(defparameter +endpoint+ "https://catalog.genesispay.finance/mcp/catalog"
  "The public catalog endpoint, where every call goes unless the section names
another.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks; the server answers its own, which is
what the later requests carry.")

(defparameter +client-version+ "0.1.0"
  "This cell's version, sent in the hello.")

(defvar *request-id* 0
  "The JSON-RPC id of the next call. Reset by START-CELL.")

(defvar *session* nil
  "The session id the server handed back, when it keeps sessions at all.")

(defvar *protocol* nil
  "The protocol version the server answered the hello with.")

(defvar *initialized* nil
  "Whether this start has said hello. Reset by START-CELL.")

(defun reset-session ()
  "Forget the hello and the session: the next tool call starts a new one."
  (setf *session* nil
        *protocol* nil
        *initialized* nil
        *request-id* 0))

(defun endpoint ()
  "The catalog address this cell talks to, without a trailing slash."
  (string-right-trim "/" (or (setting :base-url) +endpoint+)))

;;; --- the request ---------------------------------------------------------------

(defun header-value (headers name)
  "HEADERS -- a hash table or an alist, as the HTTP seam answers -- read at
NAME, case-insensitively, or NIL."
  (cond ((hash-table-p headers)
         (loop for key being the hash-keys of headers using (hash-value value)
               when (and (stringp key) (string-equal key name))
                 return value))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))))

(defun request-headers ()
  "What every request carries. No credential rides here: the catalog has no
account, no key and no sign-in."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("user-agent" . "nodecode-genesispay"))
          (when *session*
            (list (cons "mcp-session-id" *session*)))
          (when (and *initialized* *protocol*)
            (list (cons "mcp-protocol-version" *protocol*)))))

(defun snippet (body)
  "BODY as the first 300 characters of itself, for a failure's one line."
  (and (stringp body) (subseq body 0 (min 300 (length body)))))

(defun sse-payloads (text)
  "TEXT as the payloads it carries: one per `data:' block when it is a
server-sent event stream, the whole body otherwise."
  (if (null (search "data:" text))
      (list text)
      (let ((payloads '())
            (lines '()))
        (flet ((flush ()
                 (when lines
                   (push (format nil "~{~a~^~%~}" (nreverse lines)) payloads)
                   (setf lines '()))))
          (with-input-from-string (in text)
            (loop for line = (read-line in nil nil)
                  while line
                  for trimmed = (string-right-trim '(#\Return) line)
                  do (cond ((zerop (length (string-trim '(#\Space #\Tab) trimmed)))
                            (flush))
                           ((and (>= (length trimmed) 5)
                                 (string-equal "data:" (subseq trimmed 0 5)))
                            (push (string-left-trim " " (subseq trimmed 5)) lines)))))
          (flush))
        (nreverse payloads))))

(defun decode-envelope (body)
  "BODY -- the answer's text, JSON or an event stream carrying it -- as the
JSON-RPC envelope it holds, or NIL when it holds none."
  (when (stringp body)
    (loop for payload in (sse-payloads body)
          for value = (handler-case (nlk:decode-json payload) (error () nil))
          when (and (hash-table-p value)
                    (or (nlk:json-value value :object "result")
                        (nlk:json-value value :object "error")))
            return value)))

(defun post-rpc (method params)
  "One JSON-RPC POST to the catalog: METHOD with PARAMS => the answer's
RESULT object. A transport failure, a refused status or a JSON-RPC error
signals GENESISPAY-ERROR, whose one line is what the model reads."
  (let* ((object (nlk:json-object "jsonrpc" "2.0" "id" (incf *request-id*)
                                   "method" method "params" params))
         (body (nlk:encode-json-object object))
         (answer nil)
         (status nil)
         (answer-headers nil))
    (handler-case
        (multiple-value-setq (answer status answer-headers)
          (nlk:http :post (endpoint)
                    :headers (request-headers)
                    :content body
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 15 (setting :timeout-seconds))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "genesispay: the catalog at ~a could not be reached: ~a"
              (endpoint) (nle:transport-failure-label condition (endpoint)))))
    (setf *session* (or (header-value answer-headers "mcp-session-id") *session*))
    (unless (and (integerp status) (< status 300))
      (fail "genesispay ~a: HTTP ~a from the catalog~@[: ~a~]"
            method status (snippet answer)))
    (let ((envelope (decode-envelope answer)))
      (unless envelope
        (fail "genesispay ~a: the catalog answered something that is not JSON-RPC~@[: ~a~]"
              method (snippet answer)))
      (let ((error-object (nlk:json-value envelope :object "error")))
        (when error-object
          (fail "genesispay ~a: ~a" method
                (or (nlk:json-value error-object :text "message")
                    "the catalog refused the call"))))
      (or (nlk:json-value envelope :object "result")
          (nlk:json-object)))))

(defun post-notification (method)
  "One JSON-RPC notification, which the server answers with no body at all."
  (nlk:http :post (endpoint)
            :headers (request-headers)
            :content (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0"
                                                               "method" method))
            :timeout (setting :timeout-seconds)
            :connect-timeout (min 15 (setting :timeout-seconds)))
  t)

(defun ensure-session ()
  "Say hello once per start, the way an MCP client does: the protocol
version and the client's name, then the notification that the answer was
heard. A server that keeps no session still wants the first; a server that
keeps one hands back the id every later request carries."
  (unless *initialized*
    (let ((result (post-rpc "initialize"
                            (nlk:json-object
                             "protocolVersion" +protocol-version+
                             "capabilities" (nlk:json-object)
                             "clientInfo" (nlk:json-object
                                           "name" "nodecode-genesispay"
                                           "version" +client-version+)))))
      (setf *protocol* (or (nlk:json-value result :string "protocolVersion")
                           +protocol-version+)))
    (handler-case (post-notification "notifications/initialized")
      (genesispay-error () nil))
    (setf *initialized* t))
  t)

;;; --- the answer ------------------------------------------------------------------

(defun result-text (result)
  "RESULT -- a tools/call answer -- as the text its content blocks carry."
  (let ((parts '()))
    (loop for block across (nlk:json-array result "content")
          when (and (hash-table-p block)
                    (equal "text" (nlk:json-value block :string "type")))
            do (push (nlk:json-value block :string "text") parts))
    (let ((text (format nil "~{~a~^~%~}" (remove nil (nreverse parts)))))
      (and (plusp (length text)) text))))

(defun call-tool (name arguments)
  "NAME -- one tool of the catalog -- called with ARGUMENTS, a JSON object =>
the text it answers. A call the server refuses signals GENESISPAY-ERROR with
the server's own words."
  (ensure-session)
  (let* ((params (nlk:json-object "name" name
                                   "arguments" (or arguments (nlk:json-object))))
         (result (post-rpc "tools/call" params))
         (text (result-text result)))
    (if (nlk:json-value result :boolean "isError")
        (fail "genesispay ~a: ~a" name (or text "the catalog refused the call"))
        (or text (format nil "genesispay ~a: the catalog answered nothing~%" name)))))
