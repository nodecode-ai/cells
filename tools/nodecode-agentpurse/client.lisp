;;;; client.lisp --- the AgentPurse streamable-HTTP MCP, spoken from Lisp.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One place dials: DEX:REQUEST through EXCHANGE, which no test calls --
;;;; a test stubs EXCHANGE and the whole plugin runs offline. Reads retry a
;;;; 429 or a 5xx three times; a purchase never retries, because a timeout
;;;; after the server took the payment must not send it twice. Redirects are
;;;; refused (:MAX-REDIRECTS 0), so the bearer key cannot leave the
;;;; configured origin.

(in-package #:nodecode-agentpurse)

;;; --- the origin ---------------------------------------------------------------

(defun loopback-host-p (host)
  "Whether HOST names this machine."
  (member (string-downcase (or host "")) '("localhost" "127.0.0.1" "::1" "[::1]") :test #'string=))

(defun normalize-base-url (url)
  "URL as an origin this cell may send the key to: https anywhere, http on
loopback only, no credentials and no fragment. => the origin without its
trailing slash."
  (let ((url (and url (nlk:trimmed url))))
    (unless (and url (plusp (length url)))
      (fail "the agentpurse base_url is empty"))
    (let* ((uri (ignore-errors (quri:uri url)))
           (scheme (and uri (string-downcase (or (quri:uri-scheme uri) ""))))
           (host (and uri (quri:uri-host uri))))
      (unless (and uri host (plusp (length host)))
        (fail "~s is not a URL the AgentPurse client can dial" url))
      (when (or (quri:uri-userinfo uri) (quri:uri-fragment uri))
        (fail "base_url must not carry credentials or a fragment"))
      (unless (or (and (string= scheme "https"))
                  (and (string= scheme "http") (loopback-host-p host)))
        (fail "base_url must be https, or http on localhost, and ~s is neither" url))
      (string-right-trim "/" url))))

;;; --- one exchange ----------------------------------------------------------------

(defun exchange (method url &key headers content (timeout 20))
  "METHOD URL as one exchange of at most TIMEOUT seconds => (values BODY
STATUS HEADERS) with BODY a string, or (values NIL 0 NIL) when the host
could not be reached. A 3xx is answered, not followed: the caller refuses it."
  (handler-case
      (let ((body (dex:request url :method method :headers headers :content content
                                    :connect-timeout (min 10 timeout)
                                    :read-timeout timeout
                                    :max-redirects 0
                                    :force-string t
                                    :keep-alive nil)))
        (values body 200 nil))
    (dex:http-request-failed (condition)
      (let ((body (dex:response-body condition)))
        (values (and (stringp body) body) (dex:response-status condition)
                (dex:response-headers condition))))
    (error () (values nil 0 nil))))

(defun header-value (headers name)
  "HEADERS' value for NAME, an alist of (name . value), case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

;;; --- the client ------------------------------------------------------------------

(defstruct (client (:constructor make-client (base-url api-key &key (timeout 20) (read-attempts 3)))
                    (:copier nil))
  base-url api-key timeout read-attempts (session-id nil) (initialized nil) (next-id 0))

(defun settings-integer (key fallback)
  "SETTING KEY as an integer >= 0, FALLBACK when it is not a number."
  (let ((value (ignore-errors (setting key))))
    (if (and (integerp value) (not (minusp value))) value fallback)))

(defun api-key ()
  "The agent key: the environment variable api_key_env names, else api_key
written in the section. A secret never comes from a file of this folder."
  (flet ((given (text) (and (stringp text) (plusp (length (nlk:trimmed text)))
                            (nlk:trimmed text))))
    (let ((name (given (setting :api-key-env))))
      (or (and name (given (uiop:getenv name)))
          (given (setting :api-key))
          (fail "no AgentPurse key: set ~@[the variable ~a~:*, ~]or write api_key in the \
agentpurse section of the config" (and name (list name)))))))

(defun open-client (&key (timeout nil))
  "A client for this call: the configured origin, this call's own JSON-RPC
session, the key read at the call."
  (make-client (normalize-base-url (setting :base-url))
               (api-key)
               :timeout (or timeout 20)
               :read-attempts 3))

(defun client-headers (client)
  (let ((headers (list (cons "authorization" (format nil "Bearer ~a" (client-api-key client)))
                       (cons "accept" "application/json, text/event-stream")
                       (cons "content-type" "application/json")
                       (cons "user-agent" "nodecode-agentpurse/0.1"))))
    (if (client-session-id client)
        (cons (cons "mcp-session-id" (client-session-id client)) headers)
        headers)))

(defun rpc-once (client method params &key (id nil id-given))
  "One JSON-RPC message => (values TEXT STATUS HEADERS)."
  (let* ((message (apply #'nlk:make-json-object
                         (append (list "jsonrpc" "2.0" "method" method "params" params)
                                 (when id-given (list "id" id)))))
         (body (nlk:encode-json-object message)))
    (exchange :post (format nil "~a/mcp" (client-base-url client))
              :headers (client-headers client)
              :content body
              :timeout (client-timeout client))))

(defun sse-result (text)
  "TEXT as a server-sent stream's one result or error message, or NIL."
  (loop for line in (nlk:lines text)
        when (uiop:string-prefix-p "data:" (string-left-trim '(#\Space) line))
          do (let ((data (nlk:trimmed (subseq line (+ 1 (search "data:" line))))))
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
        (fail "AgentPurse answered something that was not JSON"))))

(defun rpc (client method params &key notify)
  "One request, one retry-free exchange, a read's retry handled by the caller.
=> the message object; refuses a 3xx, a 401 and any 400+."
  (let ((id (if notify nil (incf (client-next-id client)))))
    (loop
      (multiple-value-bind (text status headers)
          (if notify (rpc-once client method params) (rpc-once client method params :id id))
        (cond ((zerop status) (fail "AgentPurse could not be reached"))
              ((<= 300 status 399) (fail "AgentPurse redirected the request (HTTP ~d); \
the key was not forwarded" status))
              ((= status 401) (fail "AgentPurse refused the API key"))
              ((= status 404) (fail "AgentPurse has no such thing (HTTP 404)"))
              ((>= status 400) (fail "AgentPurse answered HTTP ~d" status))
              (notify (return :sent))
              ((null text) (fail "AgentPurse answered an empty MCP response"))
              (t (let ((message (parse-message text)))
                   (when (nlk:json-value message :any "error")
                     (fail "AgentPurse rejected the call: ~a"
                           (or (nlk:json-value message :string "error" "message")
                               "no reason given")))
                   (return message))))))))

(defun ensure-session (client)
  "Initialize this client's JSON-RPC session once, keeping the Mcp-Session-Id
the server hands back."
  (when (client-initialized client)
    (return-from ensure-session t))
  (let ((answer (rpc client "initialize"
                     (nlk:make-json-object
                      "protocolVersion" "2025-11-25"
                      "capabilities" (nlk:make-json-object)
                      "clientInfo" (nlk:make-json-object "name" "nodecode-agentpurse" "version" "0.1")))))
    (declare (ignore answer))
    (ignore-errors
     (rpc client "notifications/initialized" (nlk:make-json-object) :notify t))
    (setf (client-initialized client) t)))

(defparameter +read-tools+
  '("search_merchants" "search_products" "get_product" "check_price"
    "find_alternatives" "list_watches" "get_budget" "list_receipts")
  "The tools a retry cannot double-spend: everything else moves money.")

(defparameter +retry-statuses+ '(429 502 503 504)
  "The statuses one more read attempt can plausibly answer.")

(defun tool-result-text (message)
  "MESSAGE's tool result as the server's own JSON text, or a refusal."
  (let* ((result (nlk:json-value message :any "result"))
         (content (and result (nlk:json-array result "content")))
         (first (and content (plusp (length content)) (aref content 0)))
         (text (and first (nlk:json-value first :string "text"))))
    (when (and result (nlk:json-value result :any "isError"))
      (fail "AgentPurse could not do that: ~a" (or text "no reason given")))
    (unless (and text (plusp (length text)))
      (fail "AgentPurse returned an empty tool result"))
    text))

(defun call-tool (client name &key arguments (retry nil))
  "NAME on the MCP server, over this client => (values TEXT OBJECT): the
server's own JSON text, and what it decodes to. RETRY (a read) retries a 429
or a 5xx; a purchase never does."
  (ensure-session client)
  (let ((arguments (or arguments (nlk:make-json-object)))
        (delay 0.2)
        (attempts (if retry (max 1 (client-read-attempts client)) 1)))
    (loop for attempt from 1
          do (handler-case
                 (let* ((message (rpc client "tools/call"
                                       (nlk:make-json-object "name" name "arguments" arguments)))
                        (text (tool-result-text message)))
                   (return (values text (or (ignore-errors (nlk:decode-json text)) text))))
               (agentpurse-error (condition)
                 (let ((retryable (and retry (< attempt attempts)
                                       (search "HTTP 5" (agentpurse-error-detail condition)))))
                   (if (and retryable)
                       (progn (sleep delay) (setf delay (* 2 delay)))
                       (error condition))))))))

(defun read-tool (name &key arguments)
  "TEXT a read tool answered, its JSON as the server wrote it."
  (call-tool (open-client) name :arguments arguments :retry t))

(defun get-spend (client intent-id)
  "GET /api/spends/INTENT-ID => the payment's object, or a refusal."
  (unless (and (stringp intent-id) (plusp (length intent-id))
               (null (find #\/ intent-id)) (null (search ".." intent-id)))
    (fail "~s is not a payment id this client can read" intent-id))
  (multiple-value-bind (text status _headers)
      (exchange :get (format nil "~a/api/spends/~a" (client-base-url client)
                             (quri:url-encode intent-id))
                :headers (client-headers client) :timeout (client-timeout client))
    (cond ((= status 404) (fail "this AgentPurse has no payment status endpoint"))
          ((zerop status) (fail "AgentPurse could not be reached"))
          ((<= 300 status 399) (fail "AgentPurse redirected the request (HTTP ~d)" status))
          ((>= status 400) (fail "AgentPurse answered HTTP ~d reading the payment" status)))
    (let ((message (parse-message text)))
      (unless (and (equal t (nlk:json-value message :any "ok"))
                   (hash-table-p (nlk:json-value message :any "intent")))
        (fail "AgentPurse returned an unexpected payment status"))
      message)))
