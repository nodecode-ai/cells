;;;; mcp.lisp --- the hosted MCP server, spoken over Streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was configuration only: it pointed its harness at
;;;; https://mcp.dyscalculia.io/mcp and let the harness speak MCP. Nodecode
;;;; speaks it here, in Lisp: one POST per JSON-RPC message, an initialize and
;;;; its notification per call, then tools/call. The server answers a plain
;;;; JSON body or a server-sent stream; both are read.
;;;;
;;;; One place dials: HTTP below, which no test calls -- a test stubs it and
;;;; the whole cell runs offline.

(in-package #:nodecode-dyscalculia)

;;; --- where it dials ---------------------------------------------------------------

(defparameter +default-endpoint+ "https://mcp.dyscalculia.io/mcp"
  "The one address the plugin pointed at, and the default here.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks: initialize's protocolVersion.")

(defparameter +user-agent+ "nodecode-dyscalculia/0.1.0"
  "What every request says it is.")

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS HEADERS), every status a value."
  (nlk:http method url :headers headers :content content :timeout (or timeout 30)))

(defun request-headers ()
  "What every request carries. No credential: the server needs no key."
  (list (cons "content-type" "application/json")
        (cons "accept" "application/json, text/event-stream")
        (cons "user-agent" +user-agent+)))

(defun loopback-host-p (host)
  "Whether HOST names this machine."
  (member (string-downcase (or host "")) '("localhost" "127.0.0.1" "::1" "[::1]")
          :test #'string=))

(defun endpoint ()
  "The configured MCP endpoint, https or loopback http, or a refusal."
  (let ((url (nlk:trimmed (or (setting :base-url) ""))))
    (when (zerop (length url))
      (fail "the dyscalculia endpoint is empty: write base_url in the dyscalculia section, ~
~a unless you run your own server" +default-endpoint+))
    (let* ((uri (ignore-errors (quri:uri url)))
           (scheme (and uri (string-downcase (or (quri:uri-scheme uri) ""))))
           (host (and uri (quri:uri-host uri))))
      (unless (and uri host (plusp (length host)))
        (fail "~s is not a URL this cell can dial" url))
      (when (or (quri:uri-userinfo uri) (quri:uri-fragment uri))
        (fail "base_url must carry no credentials and no fragment"))
      (unless (or (string= scheme "https")
                  (and (string= scheme "http") (loopback-host-p host)))
        (fail "base_url must be https, or http on localhost, and ~s is neither" url))
      (string-right-trim "/" url))))

;;; --- one message ---------------------------------------------------------------

(defun header-value (headers name)
  "NAME's value in the response HEADERS nlk:http answers, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun sse-result (text)
  "TEXT as a server-sent stream's one result or error message, or NIL."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for trimmed = (string-left-trim '(#\Space #\Tab #\Return) line)
        when (uiop:string-prefix-p "data:" trimmed)
          do (let ((data (nlk:trimmed (subseq trimmed 5))))
               (unless (or (zerop (length data)) (string= data "[DONE]"))
                 (let ((message (ignore-errors (nlk:decode-json data))))
                   (when (and (hash-table-p message)
                              (or (nlk:json-value message :any "result")
                                  (nlk:json-value message :any "error")))
                     (return message)))))))

(defun parse-message (text)
  "TEXT as one JSON-RPC message object, or a refusal."
  (let ((message (or (ignore-errors (nlk:decode-json text))
                     (ignore-errors (sse-result text)))))
    (if (hash-table-p message)
        message
        (fail "the dyscalculia server answered something that was not JSON"))))

(defun exchange (base timeout method params &key id)
  "One POST of one JSON-RPC message => (values TEXT STATUS)."
  (let* ((message (apply #'nlk:make-json-object
                         (append (list "jsonrpc" "2.0" "method" method)
                                 (when params (list "params" params))
                                 (when id (list "id" id)))))
         (body (nlk:encode-json-object message)))
    (handler-case
        (http :post base :headers (request-headers) :content body :timeout timeout)
      (error () (values nil 0 nil)))))

(defvar *next-id* 0
  "This image's JSON-RPC request ids: unique across calls, as the protocol asks.")

(defun rpc (base timeout method &key params notify)
  "One request, one exchange. => the message object, or :SENT for a
notification, which answers nothing."
  (let ((id (unless notify (incf *next-id*))))
    (multiple-value-bind (text status)
        (exchange base timeout method params :id id)
      (cond ((or (null status) (zerop status))
             (fail "the dyscalculia server could not be reached"))
            ((<= 200 status 299)
             (if notify
                 :sent
                 (let ((answer (parse-message text)))
                   (let ((error (nlk:json-value answer :any "error")))
                     (when error
                       (fail "the dyscalculia server refused the call: ~a"
                             (or (nlk:json-value error :string "message") "no reason given"))))
                   answer)))
            (t (fail "the dyscalculia server answered HTTP ~d" status))))))

(defun initialize-session (base timeout)
  "Open one JSON-RPC session: initialize, then its notification."
  (rpc base timeout "initialize"
       :params (nlk:make-json-object
                "protocolVersion" +protocol-version+
                "capabilities" (nlk:make-json-object)
                "clientInfo" (nlk:make-json-object
                               "name" "nodecode-dyscalculia" "version" "0.1.0")))
  (ignore-errors (rpc base timeout "notifications/initialized"
                      :params (nlk:make-json-object) :notify t))
  t)

;;; --- the tool result ---------------------------------------------------------------

(defun tool-result-text (answer)
  "ANSWER's tool result as the server's own text, or a refusal."
  (let* ((result (nlk:json-value answer :any "result"))
         (content (and result (nlk:json-array result "content")))
         (first (and content (plusp (length content)) (aref content 0)))
         (text (and first (nlk:json-value first :string "text"))))
    (when (and result (nlk:json-value result :any "isError"))
      (fail "the dyscalculia server could not answer: ~a" (or text "no reason given")))
    (unless (and text (plusp (length text)))
      (fail "the dyscalculia server returned an empty answer"))
    text))

(defun clip (text)
  "TEXT, cut at the section's max_chars with a line saying so."
  (let ((max (or (ignore-errors (setting :max-chars)) 8000)))
    (if (> (length text) max)
        (concatenate 'string (subseq text 0 max)
                     (format nil "~%[... cut at ~d characters; ask for one entry or one study for the rest]" max))
        text)))

(defun call-tool (name arguments)
  "NAME with ARGUMENTS on the hosted server, as the server's own text."
  (let ((base (endpoint))
        (timeout (setting :timeout-seconds)))
    (initialize-session base timeout)
    (clip (tool-result-text
           (rpc base timeout "tools/call"
                :params (nlk:make-json-object "name" name "arguments" arguments))))))

(defun arguments (&rest pairs)
  "A JSON object of PAIRS, skipping a member whose value is NIL: an absent
filter and an empty one are the same to the server."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          when (not (null value)) do (setf (gethash key object) value))
    object))
