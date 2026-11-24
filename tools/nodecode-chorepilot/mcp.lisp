;;;; mcp.lisp --- ChorePilot's MCP server, over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Stdlib-only in the original and dependency-free here: one JSON-RPC POST
;;;; per exchange to the address the section names, the bearer token in the
;;;; Authorization header, an Mcp-Session-Id reused until the server forgets
;;;; it. Nothing here decides anything: every policy check (a water or heat
;;;; action waiting for the person's own yes, a kid's token going to the
;;;; approval queue, unlocking refused) happens inside ChorePilot, from the
;;;; token, never from what is sent.

(in-package #:nodecode-chorepilot)

;;; --- what the wire says -------------------------------------------------------

(defparameter +default-url+ "http://127.0.0.1:8770/mcp"
  "Where ChorePilot's MCP server listens unless the section names another address.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks, as the original plugin did.")

(defparameter +default-timeout+ 150
  "Seconds one call may wait, a water/heat confirmation included; ChorePilot
itself gives up after 120.")

(defparameter +start-timeout+ 2
  "How long the start's look at the house may take, so a house that is down
never holds a boot.")

(defvar *session* nil
  "The Mcp-Session-Id the house gave this process, or NIL before the first call.")

;;; --- where, and as whom -------------------------------------------------------

(defun expand-home (text)
  "TEXT as a pathname, ~/ read under the home directory."
  (if (uiop:string-prefix-p "~/" text)
      (merge-pathnames (subseq text 2) (user-homedir-pathname))
      (pathname text)))

(defun token ()
  "The bearer token the section points at: the file it names, else the
environment variable it names."
  (or (let ((path (setting :token-file)))
        (when path
          (let ((text (ignore-errors (uiop:read-file-string (expand-home path)))))
            (and text (let ((trimmed (nlk:trimmed text)))
                        (and (plusp (length trimmed)) trimmed)))) ))
      (let* ((name (setting :token-env))
             (value (and name (uiop:getenv name)))
             (trimmed (and value (nlk:trimmed value))))
        (and trimmed (plusp (length trimmed)) trimmed))
      (fail "no ChorePilot token: on the ChorePilot machine run `chorepilot mcp token add <person>` (it prints one once), then put it in the section's token_env variable or token_file")))

(defun check-token ()
  "The token, refusing at start what no call could do without: a section that
names neither a variable nor a file, or one whose file is unreadable."
  (unless (or (setting :token-file) (setting :token-env))
    (nlk:config-error
     "no token: set token_env to the environment variable holding the ChorePilot token, or token_file to the file holding it (the ChorePilot machine prints one once)"))
  (handler-case (token)
    (chorepilot-error (condition) (nlk:config-error "~a" (princ-to-string condition))))
  t)

(defun timeout-seconds ()
  (or (setting :timeout-seconds) +default-timeout+))

(defun endpoint-of (url)
  "URL with the /mcp path ChorePilot's streamable-HTTP server answers on."
  (let ((trimmed (string-right-trim "/" (or url +default-url+))))
    (if (uiop:string-suffix-p trimmed "/mcp")
        trimmed
        (concatenate 'string trimmed "/mcp"))))

(defun endpoint () (endpoint-of (setting :url)))

;;; --- plain http only to this machine or a private network ---------------------

(defun url-parts (url)
  "URL's (values SCHEME HOST PORT), the host lowercased, or NIL."
  (let ((mark (search "://" url)))
    (when mark
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (c) (find c "/?#")) rest) (length rest)))
             (authority (subseq rest 0 end))
             (at (position #\@ authority :from-end t))
             (hostport (if at (subseq authority (1+ at)) authority)))
        (multiple-value-bind (host port)
            (cond ((and (plusp (length hostport)) (char= #\[ (char hostport 0)))
                   (let ((close (position #\] hostport)))
                     (values (subseq hostport 1 close)
                             (let ((colon (and close (position #\: hostport :start close))))
                               (and colon (subseq hostport (1+ colon)))))))
                  ((let ((colon (position #\: hostport :from-end t)))
                     (and colon (every #'digit-char-p (subseq hostport (1+ colon)))))
                   (let ((colon (position #\: hostport :from-end t)))
                     (values (subseq hostport 0 colon) (subseq hostport (1+ colon)))))
                  (t (values hostport nil)))
          (values scheme (string-downcase host) port))))))

(defun ipv4-octets (host)
  "HOST as four octets when it is a dotted quad, else NIL."
  (let ((parts (uiop:split-string host :separator ".")))
    (when (and (= 4 (length parts))
               (every (lambda (part)
                        (and (plusp (length part)) (<= (length part) 3)
                             (every #'digit-char-p part)))
                      parts))
      (mapcar #'parse-integer parts))))

(defun private-ipv4-p (octets)
  (destructuring-bind (a b c d) octets
    (declare (ignore d))
    (or (= a 127) (= a 10) (= a 0)
        (and (= a 192) (= b 168))
        (and (= a 172) (<= 16 b 31))
        (and (= a 100) (<= 64 b 127))          ; Tailscale's 100.64/10
        (and (= a 169) (= b 254)))))           ; link-local

(defun private-ipv6-p (host)
  (let ((host (string-downcase (string-trim "[]" host))))
    (or (equal host "::1") (equal host "::")
        (uiop:string-prefix-p "fd" host)       ; unique local
        (uiop:string-prefix-p "fc" host)
        (uiop:string-prefix-p "fe80" host)))) ; link-local

(defun plain-http-ok-p (url)
  "Plain http:// only to this machine or a private network (LAN, Tailscale,
*.local, *.ts.net, *.lan, *.home.arpa); anything else needs https, or the
bearer token would cross the internet in the clear."
  (multiple-value-bind (scheme host) (url-parts url)
    (cond ((equal scheme "https") t)
          ((not (equal scheme "http")) nil)
          ((or (null host) (zerop (length host))) nil)
          ((equal host "localhost") t)
          ((some (lambda (suffix) (uiop:string-suffix-p host suffix))
                 '(".local" ".lan" ".home.arpa" ".ts.net" ".internal"))
           t)
          ((not (find #\. host)) (plusp (length host)))   ; a bare LAN name
          ((let ((octets (ipv4-octets host)))
             (and octets (private-ipv4-p octets)))
           t)
          ((private-ipv6-p host) t)
          (t nil))))

(defun check-endpoint ()
  "The address, refusing at the start what the original refused at every call."
  (let ((url (endpoint)))
    (unless (plain-http-ok-p url)
      (nlk:config-error
       "~a: plain http:// only to this machine or a private network (LAN, Tailscale, *.local, *.ts.net); use an https address or a LAN one, the token must not cross the internet in the clear"
       url))
    url))

;;; --- JSON on the wire ---------------------------------------------------------

(defun trimmed-text (text)
  (and (stringp text) (let ((trimmed (nlk:trimmed text)))
                        (and (plusp (length trimmed)) trimmed))))

(defun json-value-of (text)
  "TEXT decoded as JSON, or NIL."
  (when (trimmed-text text)
    (handler-case (nlk:decode-json text) (error () nil))))

(defun sse-payload (text)
  "The last `data: {...}' payload of an SSE body, or NIL."
  (let ((last nil))
    (when (stringp text)
      (dolist (line (uiop:split-string text :separator (list #\Newline)))
        (let ((line (string-left-trim '(#\Space #\Tab #\Return) line)))
          (when (uiop:string-prefix-p "data:" line)
            (setf last (trimmed-text (subseq line 5)))))))
    last))

(defun answer-value (text)
  "TEXT as the JSON value it carries, a JSON body or an SSE frame."
  (or (json-value-of text)
      (let ((payload (sse-payload text)))
        (and payload (json-value-of payload)))))

(defun encode (value) (nlk:encode-json-object value))

(defun header-value (headers name)
  (let ((pair (find name headers
                    :key (lambda (entry) (if (consp entry) (car entry) entry))
                    :test #'string-equal)))
    (and (consp pair) (cdr pair))))

;;; --- one exchange -------------------------------------------------------------

(defun request-headers (&optional session)
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("mcp-protocol-version" . "2025-06-18"))
          (list (cons "authorization" (format nil "Bearer ~a" (token))))
          (and session (list (cons "mcp-session-id" session)))))

(defun post (body &key session (timeout (timeout-seconds)))
  "One JSON-RPC POST => (values TEXT STATUS HEADERS)."
  (handler-case
      (multiple-value-bind (text status headers)
          (nlk:http :post (endpoint) :content body :timeout timeout
                    :connect-timeout (min timeout 10)
                    :headers (request-headers session))
        (values (if (stringp text) text "") status headers))
    ((or error sb-sys:deadline-timeout) (condition)
      (declare (ignore condition))
      (fail "can't reach ChorePilot at ~a. Is `chorepilot run` (or `chorepilot mcp --http`) running there?" (endpoint)))))

(defun initialize (&key (timeout (timeout-seconds)))
  "Open the MCP session, keeping its id; answer it."
  (let ((body (encode (nlk:json-object
                       "jsonrpc" "2.0" "id" 0 "method" "initialize"
                       "params" (nlk:json-object
                                 "protocolVersion" +protocol-version+
                                 "capabilities" (nlk:json-object)
                                 "clientInfo" (nlk:json-object "name" "nodecode" "version" "0"))))))
    (multiple-value-bind (text status headers) (post body :timeout timeout)
      (let ((value (answer-value text))
            (session (header-value headers "mcp-session-id")))
        (cond ((= status 401)
               (fail "ChorePilot refused the token (401). Make a new one on the ChorePilot machine and put it in the section"))
              ((and (= status 200) session (hash-table-p value) (gethash "result" value))
               (setf *session* session)
               (ignore-errors
                 (post (encode (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized"))
                       :session session :timeout timeout))
               session)
              (t (fail "ChorePilot did not start an MCP session (HTTP ~d: ~a)" status text)))))))

(defun rpc (method &optional params &key (timeout (timeout-seconds)))
  "One MCP request, starting the session as needed and once more after the
server forgot it; answer its result object."
  (loop with first = t
        repeat 2
        do (progn
             (unless *session* (initialize :timeout timeout))
             (let ((body (encode (nlk:json-object
                                   "jsonrpc" "2.0" "id" 1 "method" method
                                   "params" (or params (nlk:json-object))))))
               (multiple-value-bind (text status)
                   (post body :session *session* :timeout timeout)
                 (cond
                   ((and first (member status '(400 404)))
                    ;; the house forgot this session (a restart): start over once
                    (setf *session* nil))
                   ((= status 401)
                    (fail "ChorePilot refused the token (401); it may have been revoked"))
                   ((/= status 200)
                    (fail "ChorePilot answered HTTP ~d~@[: ~a~]" status text))
                   (t
                    (let ((value (answer-value text)))
                      (unless (hash-table-p value)
                        (fail "ChorePilot answered something that is not JSON: ~a" text))
                      (let ((error (gethash "error" value)))
                        (when error
                          (fail "~a" (if (hash-table-p error)
                                         (gethash "message" error)
                                         error))))
                      (let ((result (gethash "result" value)))
                        (return (or result (nlk:json-object)))))))))
             (setf first nil))))


;;; --- the surface --------------------------------------------------------------

(defun house-tools (&key (timeout (timeout-seconds)))
  "Every tool the house offers, each a decoded object, in its own order."
  (let ((tools (gethash "tools" (rpc "tools/list" nil :timeout timeout))))
    (cond ((vectorp tools) (coerce tools 'list))
          ((listp tools) tools)
          (t '()))))

(defun content-text (result)
  "RESULT's text content parts, joined."
  (let ((content (gethash "content" result))
        (out (make-string-output-stream)))
    (when (vectorp content)
      (loop for part across content
            when (and (hash-table-p part) (equal (gethash "type" part) "text"))
              do (write-string (or (gethash "text" part) "") out)))
    (get-output-stream-string out)))

(defun call-house (name arguments)
  "tools/call NAME with ARGUMENTS; ChorePilot's result object (ok, policy,
tier, confirmation, ...) as the object it structured, else its text read as
one."
  (let* ((result (rpc "tools/call" (nlk:json-object "name" name
                                                     "arguments" (or arguments (nlk:json-object)))))
         (structured (gethash "structuredContent" result)))
    (if (hash-table-p structured)
        structured
        (let ((text (content-text result)))
          (or (let ((value (json-value-of text)))
                (and (hash-table-p value) value))
              (nlk:json-object "ok" (not (gethash "isError" result)) "result" text))))))
