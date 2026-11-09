;;;; frihet.lisp --- one MCP endpoint, one operation, and the gate over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's frihet.py, in Lisp: the same endpoint, the same
;;;; unauthenticated initialize probe, the same tri-state report, the same
;;;; redaction of a key in a failure's text, and the same classifier over
;;;; operation names. Two things differ, both deliberate. The plugin read the
;;;; host's MCP client for its authenticated evidence; a cell has no host
;;;; client, so it speaks JSON-RPC itself -- initialize, then tools/list or
;;;; tools/call -- and answers an operation's own text, not a JSON envelope.
;;;;
;;;; Nothing here touches a file: the only way out is HTTP, the one seam a
;;;; test stubs, and the only secret is the one the section carries.

(in-package #:nodecode-frihet)

;;; --- the canonical surface ---------------------------------------------------

(defparameter +default-url+ "https://mcp.frihet.io/mcp"
  "Frihet's canonical MCP endpoint, as the original named it.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP protocol version the handshake states.")

(defparameter +client-name+ "nodecode-frihet"
  "What the handshake calls this client; the original called itself hermes-frihet-plugin.")

(defparameter +client-version+ "0.1.0"
  "This cell's own version, sent in the handshake.")

(defparameter +tool-name+ "frihet_mcp"
  "The one tool the cell registers, and the only name the gate reads.")

(defparameter +result-chars+ 20000
  "How much of an operation's answer reaches the model before it is cut.")

(defparameter +detail-chars+ 600
  "How much of a failed answer's body a refusal quotes.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (and text (plusp (length text)) text)))

(defun clipped (text &optional (limit +detail-chars+))
  "TEXT at most LIMIT characters, with a note when it was cut."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a ... (~d more characters)" (subseq text 0 limit) (- (length text) limit)))))

;;; --- what a key looks like in a stranger's words --------------------------------

(defparameter +redaction-rules+
  (list (cons "fri_[A-Za-z0-9_-]{8,}" "[REDACTED:FRIHET_API_KEY]")
        (cons "(?i)(authorization\\s*:?\\s*bearer\\s+)[A-Za-z0-9_.=-]+" "\\1[REDACTED:TOKEN]"))
  "What a key looks like in text on its way to the model, as the original's
REDACT_RULES had it: a Frihet key, and a bearer token.")

(defun redact (text)
  "TEXT with a key in it masked, as the original's redact() did before an answer left it."
  (if (null text)
      ""
      (reduce (lambda (string rule)
                (cl-ppcre:regex-replace-all (car rule) string (cdr rule)))
              +redaction-rules+ :initial-value (princ-to-string text))))

;;; --- reading the section --------------------------------------------------------

(defun endpoint-scheme (url)
  "URL's scheme, lowercased, or NIL."
  (let ((colon (and url (position #\: url))))
    (and colon (string-downcase (subseq url 0 colon)))))

(defun read-settings (values table)
  "The declared VALUES, with the key and the endpoint checked here, once: the
inline secret, else the variable api_key_env names, else a refusal."
  (declare (ignore table))
  (let* ((key (nonblank (getf values :api-key)))
         (env (nonblank (getf values :api-key-env)))
         (url (nonblank (getf values :mcp-url)))
         (plist (copy-list values)))
    (unless (member (endpoint-scheme url) '("http" "https") :test #'equal)
      (nlk:config-error "mcp_url must be a full https:// URL, not ~s" (getf values :mcp-url)))
    (cond (key (setf (getf plist :key-source) "api_key"))
          ((and env (nle::credential-env env))
           (setf (getf plist :api-key) (nle::credential-env env)
                 (getf plist :key-source) (format nil "api_key_env, the variable ~a" env)))
          (t (nlk:config-error
              "no key: paste a Frihet API key into api_key, or name the variable holding it in api_key_env~@[ (~a is unset or empty)~]"
              env)))
    plist))

;;; --- the classifier the gate reads ------------------------------------------------

(defparameter +irreversible-patterns+
  '("send" "markpaid" "mark_paid" "issue" "delete" "destroy" "revoke" "void"
    "cancelinvoice" "registerverifactu" "applycreditnote" "pay" "paid" "mark"
    "charge" "refund"
    "submit" "finalize" "approve" "archive" "publish")
  "What an operation name says when it reaches a tax authority, moves money or
deletes state, as the original's IRREVERSIBLE_PATTERNS read it.")

(defparameter +draft-patterns+
  '("create" "draft" "add" "update" "upsert" "edit" "set")
  "What an operation name says when it leaves a draft behind: shown to the
operator before anything is sent.")

(defparameter +read-patterns+
  '("get" "list" "search" "describe" "schema" "fetch" "find" "show" "preview" "count" "export")
  "What an operation name says when it only reads.")

(defun classify-operation (name)
  "NAME as :READ, :DRAFT-WRITE, :IRREVERSIBLE-WRITE or :UNKNOWN -- the
original's classify_tool_call, with the vendor contract's table folded into
the local pattern lists. A name nothing matches is unclassified, and the gate
fails closed on it as the original did."
  (let* ((name (string-downcase (or name "")))
         (mark (search "__" name :from-end t))
         (bare (if mark (subseq name (+ 2 mark)) name)))
    (cond ((zerop (length bare)) :unknown)
          ((some (lambda (pattern) (search pattern bare)) +irreversible-patterns+)
           :irreversible-write)
          ((some (lambda (pattern) (search pattern bare)) +draft-patterns+) :draft-write)
          ((some (lambda (pattern) (uiop:string-prefix-p pattern bare)) +read-patterns+) :read)
          (t :unknown))))

;;; --- the one seam ------------------------------------------------------------

(defun http (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS HEADERS), every status a value."
  (nlk:http method url :headers headers :content content :timeout (or timeout 15)))

(defun header-value (headers name)
  "HEADERS' NAME, case-insensitively, whether they arrive as an alist or a hash table."
  (typecase headers
    (hash-table (or (gethash name headers)
                    (loop for key being the hash-keys of headers using (hash-value value)
                          when (string-equal (princ-to-string key) name) return value)))
    (list (cdr (assoc name headers :test #'string-equal)))
    (t nil)))

(defun decode-answer (body)
  "BODY as one decoded JSON object: a plain answer, or the first data: line of
a server-sent event stream. NIL for an empty body."
  (let ((text (and body (string-trim '(#\Space #\Tab #\Newline #\Return) body))))
    (when (and text (plusp (length text)))
      (if (char= (char text 0) #\{)
          (ignore-errors (nlk:decode-json text))
          (loop for line in (uiop:split-string text :separator '(#\Newline))
                for data = (and (> (length line) 5) (uiop:string-prefix-p "data:" line)
                                (string-trim '(#\Space) (subseq line 5)))
                when (and data (plusp (length data)))
                  return (ignore-errors (nlk:decode-json data)))))))

(defun request-headers (authorization session)
  "What one exchange carries, credential and session apart."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "user-agent" (format nil "~a/~a" +client-name+ +client-version+)))
          (when authorization
            (list (cons "authorization" (format nil "Bearer ~a" authorization))))
          (when session
            (list (cons "mcp-session-id" session)
                  (cons "mcp-protocol-version" +protocol-version+)))))

(defun rpc (method &key id params session authorization)
  "One JSON-RPC exchange with the endpoint. => (values PAYLOAD STATUS HEADERS BODY-TEXT)."
  (let* ((params (or params (nlk:json-object)))
         (object (if id
                     (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)
                     (nlk:json-object "jsonrpc" "2.0" "method" method "params" params))))
    (multiple-value-bind (body status headers)
        (http :post (setting :mcp-url)
              :headers (request-headers authorization session)
              :content (nlk:encode-json-object object)
              :timeout (setting :timeout-seconds))
      (values (decode-answer body) status headers (or body "")))))

(defun rpc-error (payload)
  "PAYLOAD's JSON-RPC error message, or NIL."
  (let ((error (and (hash-table-p payload) (nlk:json-value payload :object "error"))))
    (and error
         (or (nlk:json-value error :string "message")
             (nlk:json-value error :string "code")
             "the endpoint answered an error"))))

;;; --- the handshake ----------------------------------------------------------

(defun initialize-params ()
  "The initialize parameters the handshake sends: no credential rides them."
  (nlk:json-object "protocolVersion" +protocol-version+
                    "capabilities" (nlk:json-object)
                    "clientInfo" (nlk:json-object "name" +client-name+
                                                   "version" +client-version+)))

(defun server-of (payload)
  "PAYLOAD's serverInfo as (values NAME VERSION)."
  (let ((server (and (hash-table-p payload)
                     (nlk:json-value payload :object "result" "serverInfo"))))
    (if server
        (values (nlk:json-value server :string "name")
                (nlk:json-value server :string "version"))
        (values nil nil))))

(defun open-session (&key authorization)
  "The handshake, and the session id it hands back. => (values SESSION PAYLOAD)."
  (multiple-value-bind (payload status headers body)
      (rpc "initialize" :id 1 :params (initialize-params) :authorization authorization)
    (when (>= status 400)
      (fail "the Frihet MCP refused the handshake: HTTP ~d~@[ ~a~]"
            status (nonblank (clipped (redact body)))))
    (when (rpc-error payload)
      (fail "the Frihet MCP refused the handshake: ~a" (redact (rpc-error payload))))
    (let ((session (header-value headers "mcp-session-id")))
      (when session
        ;; A notification carries no id and answers 202: nothing to read.
        (ignore-errors (rpc "notifications/initialized" :session session
                            :authorization authorization)))
      (values session payload))))

(defun probe-reachability ()
  "The unauthenticated initialize the original's probe_mcp sent: no key, no
session, no claim about authentication. => a plist."
  (let ((started (get-internal-real-time)))
    (handler-case
        (multiple-value-bind (payload status headers body)
            (rpc "initialize" :id 1 :params (initialize-params))
          (declare (ignore headers))
          (multiple-value-bind (name version) (server-of payload)
            (list :status status
                  :reachable (and status (< status 500) t)
                  :server name :version version
                  :protocol (nlk:json-value payload :string "result" "protocolVersion")
                  :error (or (rpc-error payload)
                             (and status (>= status 400)
                                  (nonblank (clipped (redact body)))))
                  :ms (round (* 1000 (- (get-internal-real-time) started))
                             internal-time-units-per-second))))
      (error (condition)
        (list :status nil :reachable nil :error (redact (princ-to-string condition)))))))

(defun list-operations (&key authorization)
  "tools/list through an authenticated session. => (values NAMES SESSION)."
  (multiple-value-bind (session payload) (open-session :authorization authorization)
    (declare (ignore payload))
    (multiple-value-bind (answer status headers body)
        (rpc "tools/list" :id 2 :session session :authorization authorization)
      (declare (ignore headers))
      (when (>= status 400)
        (fail "the Frihet MCP refused tools/list: HTTP ~d~@[ ~a~]"
              status (nonblank (clipped (redact body)))))
      (when (rpc-error answer)
        (fail "the Frihet MCP answered tools/list with an error: ~a" (redact (rpc-error answer))))
      (values (loop for tool across (or (nlk:json-value answer :array "result" "tools") #())
                    for name = (nlk:json-value tool :string "name")
                    when name collect name)
              session))))

;;; --- what an operation answers ------------------------------------------------

(defun content-text (result)
  "RESULT (an MCP CallToolResult object) as the model reads it: its text
content joined, else the whole object."
  (let* ((items (nlk:json-value result :array "content"))
         (texts (loop for item across (or items #())
                       for text = (nlk:json-value item :string "text")
                       when (nonblank text) collect text)))
    (cond ((plusp (length texts)) (format nil "~{~a~^~2%~}" texts))
          ((hash-table-p result) (nlk:encode-json-object result))
          (t (princ-to-string result)))))

(defun answer-text (result)
  "RESULT as one text, cut at +RESULT-CHARS+ when it is longer."
  (let ((text (content-text result)))
    (if (<= (length text) +result-chars+)
        text
        (format nil "~a~2%; ... (~d more characters)"
                (subseq text 0 +result-chars+) (- (length text) +result-chars+)))))

;;; --- the reports ----------------------------------------------------------------

(defun status ()
  "What the cell is set to, without a network call."
  (format nil "frihet: ~a ~a~%  endpoint: ~a~a~%  key: set, from ~a~%  timeout: ~ds~%  /frihet doctor proves the handshake, the key and the tool list; (frihet:operations) lists them~%"
          +client-name+ +client-version+
          (setting :mcp-url)
          (if (string-equal (setting :mcp-url) +default-url+) " (canonical)" "")
          (setting :key-source) (setting :timeout-seconds)))

(defun setup ()
  "Where the key goes, and what to do once it is there."
  (format nil "frihet: this cell talks to the Frihet MCP at ~a.~2% 1. Make an API key for your Frihet workspace.~% 2. Paste it into api_key in the frihet section of the config, or leave that empty and export the variable api_key_env names (~a).~% 3. Leave mcp_url alone unless you run a private mirror; the canonical endpoint is ~a.~% 4. (restart-cells \"nodecode-frihet\"), or set it in /settings, then /frihet doctor.~2%A key pasted here is the section's, never the transcript's: unlike a slash argument it cannot land in a chat log.~%"
          (setting :mcp-url) (setting :api-key-env) +default-url+))

(defun doctor ()
  "The live probe: reachable, authenticated, and how many operations answered.
Every state stays apart -- reachable is not authenticated, and neither is ready."
  (let* ((probe (probe-reachability))
         (reachable (getf probe :reachable))
         (evidence nil)
         (authenticated "unknown")
         (available "unknown"))
    (when (setting :api-key)
      (handler-case
          (let ((names (list-operations :authorization (setting :api-key))))
            (setf evidence (length names)
                  authenticated t
                  available (and (plusp evidence) t)))
        (frihet-error (condition)
          (setf evidence (redact (princ-to-string condition))
                authenticated nil
                available nil))))
    (flet ((state (value)
             (cond ((eq value t) "true") ((null value) "false") (t value))))
      (with-output-to-string (out)
        (format out "frihet: ~a~%" (setting :mcp-url))
        (format out "  endpoint_reachable: ~a" (if reachable "true" "false"))
        (when reachable
          (format out " (HTTP ~a" (getf probe :status))
          (when (getf probe :server) (format out ", server ~a" (getf probe :server)))
          (when (getf probe :version) (format out " ~a" (getf probe :version)))
          (when (getf probe :protocol) (format out "; ~a" (getf probe :protocol)))
          (format out "; ~d ms)" (or (getf probe :ms) 0)))
        (format out "~%  authenticated: ~a" (state authenticated))
        (when (and (integerp evidence) (plusp evidence))
          (format out " (tools/list answered ~d operation~:p)" evidence))
        (format out "~%  tools_available: ~a~%" (state available))
        (when (getf probe :error) (format out "  evidence: ~a~%" (getf probe :error)))
        (when (stringp evidence) (format out "  evidence: ~a~%" evidence))))))

(defun operations ()
  "Every operation the endpoint advertises, one a comma."
  (let ((names (list-operations :authorization (setting :api-key))))
    (if (null names)
        "frihet: the endpoint advertises no operation"
        (format nil "frihet: ~d operation~:p~%~{~a~^, ~}"
                (length names) (sort (copy-list names) #'string-lessp)))))

;;; --- the one verb ----------------------------------------------------------------

(defun call-operation (operation &key arguments confirm)
  "Call one Frihet MCP operation and answer what it answered. CONFIRM says the
operator already agreed to an irreversible effect, so the gate lets it through."
  (declare (ignore confirm))
  (let ((operation (nonblank operation)))
    (unless operation
      (fail "call-operation needs an operation: one of (frihet:operations), e.g. list_invoices"))
    (multiple-value-bind (session payload) (open-session :authorization (setting :api-key))
      (declare (ignore payload))
      (multiple-value-bind (answer status headers body)
          (rpc "tools/call" :id 2 :session session :authorization (setting :api-key)
               :params (nlk:json-object "name" operation
                                        "arguments" (if (hash-table-p arguments)
                                                        arguments
                                                        (nlk:json-object))))
        (declare (ignore headers))
        (when (member status '(401 403))
          (fail "the Frihet MCP refused the key: HTTP ~d~@[ ~a~]; check api_key, or the variable api_key_env names"
                status (nonblank (clipped (redact body)))))
        (when (and status (>= status 400))
          (fail "the Frihet MCP answered HTTP ~d~@[ ~a~]" status (nonblank (clipped (redact body)))))
        (when (rpc-error answer)
          (fail "the Frihet MCP refused ~a: ~a" operation (redact (rpc-error answer))))
        (let ((result (and (hash-table-p answer) (nlk:json-value answer :object "result"))))
          (unless result
            (fail "the Frihet MCP answered ~a with no result: ~a" operation
                  (nonblank (clipped (redact body)))))
          (let ((text (answer-text result)))
            (if (nlk:json-value result :boolean "isError")
                (fail "~a" (redact text))
                text)))))))

;;; --- the tool the model calls --------------------------------------------------

(defun operation-tool (request)
  "The frihet_mcp tool: one operation, its arguments, answered as it answered."
  (call-operation (operation-request-operation request)
                  :arguments (operation-request-arguments request)
                  :confirm (operation-request-confirm request)))

;;; --- the gate over an irreversible operation ---------------------------------------

(defparameter +allow-once+ "Allow once")
(defparameter +deny+ "Deny")

(defvar *ask-operator* nil
  "A test's stand-in for the core's question board; NIL asks the core.")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (or *ask-operator*
      (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
        (and symbol (fboundp symbol) (symbol-function symbol)))))

(defun ask-before-run (operation kind)
  "Ask whether OPERATION, classified KIND, may run once. => (values ANSWER WHY),
WHY :OFF when nobody can be asked."
  (let ((ask (and (setting :ask) (ask-operator-function))))
    (if (null ask)
        (values nil :off)
        (funcall ask (format nil "Frihet: run the ~(~a~) operation ~a?"
                             (string-downcase (string kind)) operation)
                 :detail "It reaches a tax authority, moves money, or deletes state; a read and a draft-write need no question."
                 :choices (list +allow-once+ +deny+)
                 :seconds 120))))

(defparameter +mcp-prefix+ "mcp__frihet__"
  "What a host's MCP client prefixes a Frihet tool with, as the original's hook read it.")

(defun ours-p (name)
  "Whether NAME is a Frihet tool: this cell's own, or one a host MCP client serves
under mcp__frihet__."
  (let ((name (string-downcase (or name ""))))
    (or (string= name +tool-name+)
        (uiop:string-prefix-p +mcp-prefix+ name))))

(defun operation-of (name arguments)
  "The Frihet operation a call names: the member this cell's tool takes, else the
last segment of a host client's tool name."
  (cond ((and (stringp name) (uiop:string-prefix-p +mcp-prefix+ (string-downcase name)))
         (subseq (string-downcase name) (length +mcp-prefix+)))
        ((hash-table-p arguments) (gethash "operation" arguments))))

(defun gate (op next)
  "The :TOOL point: an irreversible or unclassified Frihet operation runs only
once a person agreed to it. Every other tool is let straight through."
  (if (not (ours-p (getf op :name)))
      (funcall next op)
      (let* ((arguments (getf op :arguments))
             (operation (operation-of (getf op :name) arguments))
             (confirm (and (hash-table-p arguments) (gethash "confirm" arguments)))
             (kind (classify-operation operation)))
        (if (member kind '(:read :draft-write))
            (funcall next op)
            (multiple-value-bind (answer why) (ask-before-run (or operation "?") kind)
              (cond ((or confirm (equal answer +allow-once+)) (funcall next op))
                    ((equal answer +deny+)
                     (nle:failure
                      (format nil "ERROR: refused by nodecode-frihet: the operator was asked about ~a and chose Deny; the call did not run -- do not retry it or work around it: ask them what they want instead"
                              operation)))
                    (t
                     (nle:failure
                      (format nil "ERROR: ~a is ~(~a~): it reaches a tax authority, moves money, or deletes state, and nobody was asked~@[ (~(~a~))~]. Re-read the record, confirm with the operator, then call it again with \"confirm\": true -- never retry it blindly"
                              operation kind why)))))))))
