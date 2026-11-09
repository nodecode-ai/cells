;;;; cell.lisp --- DSers's dropshipping service, as tools the model can call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original Hermes plugin is a connection, not a program: one remote
;;;; MCP server (streamable HTTP, OAuth) plus eight Markdown skills that say
;;;; how to use the tools it exposes. Here the connection is two verbs --
;;;; DSERS-TOOLS, which reads the tool list the authorized account gets, and
;;;; DSERS-CALL, which calls one by name -- and the skills are a short
;;;; standing note plus the /dsers manual topic, which is where their
;;;; guardrails live.
;;;;
;;;; Every byte this cell sends goes through HTTP, one name, so a test stubs
;;;; that name and never opens a socket.

(defpackage #:nodecode-dsers
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:dsers-tools #:dsers-call #:dsers-command))

(in-package #:nodecode-dsers)

(nlk:define-peripheral dsers :not-running t)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this cell speaks: initialize, tools/list, tools/call.")

(defparameter +default-endpoint+ "https://ai.dsers.com/mcp"
  "Producer's own server; the section's endpoint overrides it.")

(defparameter +max-text+ 4000
  "How much of one tool's text answer reaches the model, in characters.")

(defparameter *session* nil
  "The open MCP session this cell holds: a plist of :ID, :ENDPOINT, :TOKEN.
One is opened on the first call and dropped when the server disowns it.")

(defparameter *tools* nil
  "The last TOOLS/LIST answer, so a listing is not a round trip each time.")

(defparameter *skill*
  "DSers tools come from the DSers MCP server: (dsers_tools) lists what this account may call and (dsers_call \"name\" arguments) calls one. Read first (list_stores, list_orders, get_order), then say exactly what will change and wait for the user's yes before anything that publishes a listing, changes a price or places a supplier order. Place orders only from a preview the user has seen. Report only what a call returned: never invent an id, price, stock or status. UNAUTHORIZED and PLAN_RESTRICTION are the DSers sign-in and plan, not the arguments; do not work around them."
  "What the model is told every round, which the original said in eight skills.")

(defparameter *primer*
  "DSers is a dropshipping service: it connects supplier platforms (AliExpress, Alibaba.com, 1688, the DSers Verified Agent Network) to sales channels (Shopify, WooCommerce, Wix, eBay, TikTok Shop, Amazon, Walmart and others). A DSers Advanced, Pro or Enterprise plan is required, and an MCP connection is authorized with OAuth against the endpoint in the dsers section (default https://ai.dsers.com/mcp).

The tools are the ones the account's server exposes; this cell does not fix a list. (dsers_tools) lists each name with its first line of description; (dsers_call name arguments) calls one and answers its text. Arguments are the tool's own input schema; read it in the (dsers_tools) output before calling.

Reads: list_stores, get_account_profile, get_plan_and_credits, search_supplier_products, list_pre_publish_products, get_pre_publish_product, get_store_product_mapping, list_orders, get_order, diagnose_order_issue, list_packages, get_package_details.

Writes (say what changes, then wait for a yes): publishing a listing, changing a price, mapping a supplier, placing a supplier order (it spends the seller's money: only from a preview the user has seen), cancelling a supplier order, sending tracking to the store.

Errors come back as a code, a message, a retryable flag and often a hint. UNAUTHORIZED or INSUFFICIENT_SCOPE: the sign-in expired or the account lacks the permission. PLAN_RESTRICTION: the plan, a quota or the AI Credits balance, not the arguments. RATE_LIMITED: do not call that tool again before the reset the hint names. VALIDATION_ERROR: fix the arguments, never resend them. UPSTREAM_UNKNOWN after a change: read the record again before anything else, and do not resend."
  "The /dsers manual topic: the original's working-with-dsers reference, condensed.")

;;; --- the one network seam ---------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub this name."
  (apply #'nlk:http method url arguments))

(defun header-value (headers name)
  "The value HEADERS carries for NAME, case-insensitively: an alist, a plist
or a hash table, whichever the call handed back."
  (cond ((hash-table-p headers)
         (loop for key being the hash-keys of headers using (hash-value value)
               when (string-equal (string key) name) return value))
        ((listp headers)
         (loop for entry in headers
               when (and (consp entry) (string-equal (string (car entry)) name))
                 return (cdr entry)))
        (t nil)))

(defun session-id (headers)
  (let ((value (header-value headers "mcp-session-id")))
    (and (stringp value) (plusp (length value)) value)))

;;; --- settings ---------------------------------------------------------------

(defun setting-of (key &optional default)
  (getf (running-settings) key default))

(defun endpoint ()
  (let ((named (setting-of :endpoint)))
    (if (and named (plusp (length named))) named +default-endpoint+)))

(defun token ()
  "The access token: written inline, or read from the named environment
variable. A missing one is a refusal, not a request without a header."
  (let* ((direct (setting-of :token))
         (named (setting-of :token-env))
         (resolved (and named (nle::credential-env named))))
    (or direct resolved named
        (fail "no DSers access token: set token or token_env in the dsers section of ~~/.nodecode/config.jsonc, ~
               then (restart-cells \"nodecode-dsers\")"))))

(defun timeout ()
  (or (setting-of :timeout-seconds) 60))

;;; --- JSON-RPC over streamable HTTP ------------------------------------------

(defun body-text (body)
  "BODY as text, whether the call answered a string or octets."
  (cond ((null body) "")
        ((stringp body) body)
        ((vectorp body)
         (or (ignore-errors (sb-ext:octets-to-string body :external-format :utf-8)) ""))
        (t (princ-to-string body))))

(defun event-data (text)
  "TEXT, or the last `data:' payload when the server answered a server-sent
stream. DSers answers one JSON-RPC message per stream, on one data line."
  (let ((answer nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (when (uiop:string-prefix-p "data:" line)
        (setf answer (string-trim " " (subseq line 5)))))
    (or answer text)))

(defun post (payload &key session)
  "One JSON-RPC PAYLOAD to the endpoint. => (values TEXT STATUS HEADERS)."
  (let* ((headers (list (cons "Content-Type" "application/json")
                         (cons "Accept" "application/json, text/event-stream")
                         (cons "Authorization" (format nil "Bearer ~a" (token)))
                         (cons "MCP-Protocol-Version" +protocol-version+)))
         (headers (if session (cons (cons "Mcp-Session-Id" session) headers) headers)))
    (multiple-value-bind (body status answer-headers)
        (http :post (endpoint)
              :headers headers
              :content (nlk:encode-json-object payload)
              :timeout (timeout))
      (values (body-text body) status answer-headers))))

(defun answer-json (text status)
  "The JSON-RPC message in TEXT, refusing a failed status in the server's words."
  (let ((json (ignore-errors (nlk:decode-json (event-data text)))))
    (unless (and (<= 200 status 299) (hash-table-p json))
      (fail "the DSers MCP server answered ~a~@[: ~a~]" status
            (nlk:one-line (if (hash-table-p json)
                               (or (gethash "message" json) (event-data text))
                               (event-data text))
                          :cap 300)))
    (let ((error (gethash "error" json)))
      (when (hash-table-p error)
        (fail "the DSers MCP server refused ~a: ~a"
              (or (gethash "code" error) "the call")
              (or (gethash "message" error) (event-data text)))))
    json))

(defun rpc (method &key params session notification)
  "One JSON-RPC exchange. A NOTIFICATION carries no id and answers the text."
  (let* ((payload (let ((object (make-hash-table :test #'equal)))
                    (setf (gethash "jsonrpc" object) "2.0"
                          (gethash "method" object) method)
                    (when params (setf (gethash "params" object) params))
                    (unless notification (setf (gethash "id" object) (random 1000000)))
                    object)))
    (multiple-value-bind (text status headers) (post payload :session session)
      (if notification
          (values text headers)
          (values (gethash "result" (answer-json text status)) headers)))))

(defun open-session ()
  "Initialize against the endpoint, keep the session id it answers, and tell
the server we are ready. => SESSION, the plist this cell then reuses."
  (multiple-value-bind (result headers)
      (rpc "initialize"
           :params (let ((params (make-hash-table :test #'equal)))
             (setf (gethash "protocolVersion" params) +protocol-version+
                   (gethash "capabilities" params) (make-hash-table :test #'equal)
                   (gethash "clientInfo" params)
                   (let ((client (make-hash-table :test #'equal)))
                     (setf (gethash "name" client) "nodecode"
                           (gethash "version" client) "0.1.0")
                     client))
             params))
    (declare (ignore result))
    (let ((session (list :id (session-id headers) :endpoint (endpoint) :token (token))))
      (setf *session* session)
      (ignore-errors (rpc "notifications/initialized" :session (getf session :id) :notification t))
      session)))

(defun session-for (method &rest arguments &key params notification)
  "RPC against the open session, opening one first; a server that disowned
the session (a 4xx on a call we thought was open) costs one re-initialize."
  (declare (ignorable params notification))
  (let ((session (or *session* (open-session))))
    (handler-case
        (apply #'rpc method :session (getf session :id) arguments)
      (dsers-error (condition)
        (unless (search "404" (dsers-error-detail condition))
          (signal condition))
        (setf *session* nil *tools* nil)
        (let ((fresh (open-session)))
          (apply #'rpc method :session (getf fresh :id) arguments))))))

;;; --- reading answers --------------------------------------------------------

(defun field (object name)
  "NAME read off OBJECT: a JSON object or a plist the model's call carried."
  (cond ((hash-table-p object) (gethash name object))
        ((listp object) (or (getf object (intern (string-upcase name) :keyword))
                            (cdr (assoc name object :test #'string-equal))))
        (t nil)))

(defun tool-objects ()
  (let ((result (session-for "tools/list" :params (make-hash-table :test #'equal))))
    (setf *tools* (or (and (hash-table-p result) (gethash "tools" result)) #()))))

(defun tools-list ()
  "Each tool the account's server exposes: a plist of :NAME, :DESCRIPTION, :SCHEMA."
  (let ((tools (tool-objects)))
    (loop for tool across tools
          collect (list :name (field tool "name")
                        :description (field tool "description")
                        :schema (field tool "inputSchema")))))

(defun render-tools (tools)
  "TOOLS as text: a name, its first sentence, and its arguments' names."
  (if (null tools)
      "the DSers server exposed no tools for this account."
      (with-output-to-string (out)
        (format out "~d DSers tool~:p on this account:~%" (length tools))
        (dolist (tool tools)
          (let ((schema (getf tool :schema)))
            (format out "~%- ~a~@[ -- ~a~]~@[~%    arguments: ~{~a~^, ~}~]"
                    (getf tool :name)
                    (nlk:one-line (or (getf tool :description) "") :cap 160)
                    (let ((properties (and (hash-table-p schema) (gethash "properties" schema)))
                          (required (and (hash-table-p schema) (gethash "required" schema))))
                      (when (hash-table-p properties)
                        (loop for key being the hash-keys of properties
                              collect (if (and required (member key (coerce required 'list)
                                                                :test #'string=))
                                          (format nil "~a (required)" key)
                                          key))))))))))

;;; --- the verbs --------------------------------------------------------------

(defun dsers-tools (arguments)
  "List the DSers tools the connected account may call."
  (declare (ignore arguments))
  (render-tools (tools-list)))

(defun content-text (result)
  "RESULT's text content, capped, and the server's own error note if any."
  (let* ((content (and (hash-table-p result) (gethash "content" result)))
         (text (format nil "~{~a~^~%~}"
                       (loop for item across (if (vectorp content) content #())
                             when (string= "text" (or (field item "type") ""))
                               collect (or (field item "text") "")))))
    (if (plusp (length text))
        (if (> (length text) +max-text+)
            (concatenate 'string (subseq text 0 +max-text+) " ...")
            text)
        (nlk:one-line (nlk:encode-json-object (or result (make-hash-table :test #'equal))) :cap +max-text+))))

(defun dsers-call (arguments)
  "Call one DSers tool by name with its own arguments."
  (let* ((name (field arguments "tool"))
         (named-arguments (field arguments "arguments")))
    (unless (and (stringp name) (plusp (length name)))
      (fail "dsers_call needs the tool's name, as (dsers_tools) writes it: {\"tool\": \"list_stores\"}"))
    (let ((params (make-hash-table :test #'equal)))
      (setf (gethash "name" params) name
            (gethash "arguments" params) (or named-arguments (make-hash-table :test #'equal)))
      (let ((result (session-for "tools/call" :params params)))
        (if (and (hash-table-p result) (gethash "isError" result))
            (format nil "~a answered an error: ~a" name (content-text result))
            (content-text result))))))

(defun dsers-command (args session-id)
  "One /dsers invocation: the connection as it stands, or the tool list."
  (declare (ignore session-id))
  (let ((argument (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member argument '("tools" "list") :test #'string=)
           (render-tools (tools-list)))
          (t
           (format nil "DSers: ~a~%token: ~:[not set~;set~]~%~%(dsers_tools) lists the tools this account may call; (help :dsers) has the workflow."
                   (endpoint)
                   (and (ignore-errors (token)) t))))))

;;; --- the always-on note -----------------------------------------------------

(defun standing-skill (next session-id)
  "Supply the DSers note beside whatever standing sections the harness reads."
  (acons "dsers" *skill*
         (remove "dsers" (funcall next session-id) :key #'car :test #'equal)))

;;; --- the cell ---------------------------------------------------------------

(nle:define-cell dsers
  (:section ("dsers")
    (:guide "a token authorizes the connection, then /dsers tools says what the account may call")
    ("endpoint" :string :default +default-endpoint+ :doc "the DSers MCP server this account uses")
    ("token" :secret :doc "an access token for that server; token_env names a variable holding one")
    ("token_env" :env :doc "environment variable holding the access token")
    ("timeout_seconds" :integer :default 60 :min 1 :doc "one exchange with DSers's ceiling, in seconds"))
  (:tool "dsers_tools"
   '(:description "List the DSers tools this account may call: supplier search, listings, prices, orders, parcels. Read it before dsers_call, which takes a tool's name and input schema from it."
    :parameters (:type "object" :properties ()))
   'dsers-tools)
  (:tool "dsers_call"
   '(:description "Call one DSers tool by name with its own arguments, as dsers_tools lists them. Writes (publish a listing, change a price, place or cancel a supplier order, send tracking) need the user's yes first; placing a supplier order spends the seller's money."
    :parameters (:type "object"
                 :properties (:tool (:type "string" :description "the tool's name, as dsers_tools writes it")
                              :arguments (:type "object" :description "that tool's own arguments"))
                 :required ("tool")))
   'dsers-call)
  (:command "dsers" 'dsers-command
            :description "The DSers connection and the tools it offers"
            :argument-hint "[tools]")
  (:help :dsers "dsers: the DSers dropshipping tools - (dsers_tools), (dsers_call \"list_stores\")" *primer*)
  (:hook 'nle::read-harness-sections #'standing-skill))
