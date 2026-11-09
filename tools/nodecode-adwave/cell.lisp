;;;; cell.lisp --- the skill's rules, the section, the wire, two tools and one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin had no code: an MCP server declaration
;;;; (https://waverunner.adwave.com/api/mcp, Streamable HTTP) and one skill
;;;; that taught a model when to use it and how to spend safely. This cell
;;;; speaks that server's JSON-RPC itself -- initialize, the Mcp-Session-Id,
;;;; tools/list, tools/call, an answer that is either a JSON object or a
;;;; one-event stream -- so every tool the server offers is reachable by name,
;;;; and the skill is the manual topic (help :adwave).
;;;;
;;;; The credential is an Adwave API key (account menu, API & agents), pasted
;;;; into the section or named by an environment variable or a file. A person
;;;; who signed in to the same server in /mcp needs nothing: the shipped mcp
;;;; cell keeps that token, and this cell reuses it.
;;;;
;;;; Config, a top-level key of its own:
;;;;   "adwave": {"server_url": "https://waverunner.adwave.com/api/mcp",
;;;;              "token_file": "~/.nodecode/secrets/adwave-key",
;;;;              "timeout_seconds": 60, "max_chars": 6000}

(in-package #:nodecode-adwave)

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "Adwave runs paid advertising for a business in the US and Canada. Give it a website and it
builds the business and buyer profiles, makes the ads, buys the media on streaming TV, Meta
(Facebook and Instagram), Google Search, YouTube, Reddit and the open web from one prepaid daily
budget, and keeps optimizing; then it reports spend, visits, conversions, cost per result and
return on ad spend. This cell reaches Adwave's own MCP server: adwave_tools lists every tool it
offers, what each does, whether it is read-only and which arguments it takes; adwave_call runs one
by name with that tool's own arguments object.

Use it when the person wants to advertise a business, product, store, app or event; launch, pause,
resume, end or re-budget a campaign; see results; or install conversion tracking. Not for organic
social posting, email newsletters or SEO.

Money
1. adwave_call refuses any tool not marked read-only until confirmed is true. That flag is one
   person's yes to one call: say the amount in dollars first, and get a clear yes in this
   conversation. It covers launch_campaign, resume_campaign, generate_creatives, create_campaign
   when it generates ads, retry_campaign_launch, bulk_campaign_lifecycle with resume,
   apply_recommendation and every budget increase. Never pass it on your own initiative.
2. Each morning (Eastern time) a live campaign sets aside that day's budget from the wallet; the
   next morning the person is charged for what ran, never more than the amount set aside. A paused
   day is never charged.
3. Amounts are integer cents; show dollars to the person.
4. Never ask for card numbers: create_wallet_checkout answers a link, and the person pays on
   Stripe's page.

First campaign
1. add_business with the website URL: the analysis is free. Poll list_businesses until the
   business is ready.
2. get_business: summarize the profile and the buyer profiles, and let the person correct them
   (update_persona, create_persona).
3. Agree the goal, the daily budget (from $30 a day), the dates and the area; get_creation_quote,
   then create_campaign for a draft.
4. get_wallet: if availableCents is short of day one, create_wallet_checkout and wait for the
   payment.
5. launch_campaign_dry_run, explain anything that blocks, get the yes, then launch_campaign with
   confirmed true.
6. Suggest get_tracking_setup so conversions are measured on their site.

Results and changes
get_campaign_report answers spend, sessions, conversions, CPA and ROAS by channel; net spend is
spendCents + unchargedSpend.cents. get_campaign_analytics has trends and breakdowns,
get_campaign_ads per ad. update_campaign_budget, update_campaign_budget_mix,
update_campaign_end_date, set_ad_serving, pause_campaign, resume_campaign and end_campaign
(permanent) change a campaign; list_ad_reviews then approve_ad handle ads waiting for approval.

Errors
401: the key or the sign-in expired or was disconnected; ask the person to sign in again, or set a
new key in the adwave section. 402 insufficient_funds: offer create_wallet_checkout, which the
answer names in nextTool. 409 or 422: read error and message, explain it in plain words, fix the
named field, and do not retry a spending call without a new yes. 429: wait a minute, then retry
once. This topic is the skill the original plugin shipped, whole."
  "What (help :adwave) answers while the cell runs: the skill, as one workflow.")

(defparameter +default-url+ "https://waverunner.adwave.com/api/mcp"
  "Adwave's hosted MCP server: the one host this cell talks to.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client offers at initialize; the server may answer another.")

(defparameter +client-version+ "0.1.0")

(defparameter +spending+
  '("launch_campaign" "resume_campaign" "generate_creatives" "create_campaign"
    "retry_campaign_launch" "bulk_campaign_lifecycle" "apply_recommendation"
    "update_campaign_budget" "update_campaign_budget_mix")
  "The server's tools that spend from the wallet, by the skill's own list.")

;;; --- answers, and how a refusal reaches the model ---------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on, as the call's failure."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal or a dead network becomes that text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (adwave-error (condition)
       (nle:failure (format nil "ERROR: ~a" (adwave-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: adwave: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun clip (text limit)
  "TEXT, cut at LIMIT characters with a note of how much is left."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a... (~d more characters)" (subseq text 0 limit) (- (length text) limit)))))

;;; --- reading the section ---------------------------------------------------------

(defun json-member (object key)
  "OBJECT's member KEY => (values VALUE PRESENT-P), a false member counting as present."
  (if (hash-table-p object)
      (gethash key object)
      (values nil nil)))

(defun read-token-file (path)
  "The token in PATH's first non-empty line, or NIL when the file is not there."
  (let ((truename (probe-file path)))
    (when truename
      (handler-case
          (with-open-file (in truename :external-format :utf-8)
            (loop for line = (read-line in nil nil)
                  while line
                  for text = (nonblank line)
                  when text return text))
        (error () nil)))))

(defun url-host (url)
  "URL's host, lowercased, or NIL."
  (let* ((start (or (search "//" url) -1))
         (rest (subseq url (min (length url) (+ start 2))))
         (end (or (position #\/ rest) (length rest))))
    (string-downcase (subseq rest 0 end))))

(defun mcp-token (url)
  "The access token the shipped mcp cell keeps for URL, when it has signed in,
else NIL. That store is its sign-in, not this cell's: it is only read."
  (let ((path (nlk:home "mcp/oauth.json")))
    (when (probe-file path)
      (let* ((host (ignore-errors (url-host url)))
             (store (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
        (when (and host (hash-table-p store))
          (loop for key being the hash-keys of store using (hash-value entry)
                when (and (stringp key) (search host (string-downcase key)))
                  return (nonblank (nlk:json-value entry :text "tokens" "access_token"))))))))

(defun environment-token (name)
  "The value of the environment variable NAME, or NIL."
  (and name (ignore-errors (uiop:getenv name)) (nonblank (uiop:getenv name))))

(defun read-settings (values table)
  "The declared VALUES, with the key resolved once, here: the section's, the
environment's, a file's, else the sign-in the mcp cell keeps."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :server-url) (or (nonblank (getf values :server-url)) +default-url+)
          (getf plist :timeout-seconds) (or (getf values :timeout-seconds) 60)
          (getf plist :max-chars) (or (getf values :max-chars) 12000))
    (let* ((inline (nonblank (getf values :token)))
           (from-env (environment-token (nonblank (getf values :token-env))))
           (file (nonblank (getf values :token-file)))
           (from-file (and file (read-token-file file)))
           (stored (mcp-token (getf plist :server-url))))
      (setf (getf plist :token) (or inline from-env from-file stored))
      (setf (getf plist :token-source)
            (cond (inline "the key in the token member of the adwave section")
                  (from-env "the environment variable token_env names")
                  (from-file "the file token_file names")
                  (stored "the sign-in the mcp cell keeps for this server")
                  (t "none"))))
    plist))

(defun token-source ()
  "Where the key comes from, in words: never the key itself."
  (or (setting :token-source) "none"))

(defun check-config ()
  "Refuse a section that cannot reach Adwave, naming the member to set."
  (unless (search "https://" (setting :server-url))
    (error 'nlk:config-refusal
           :detail "adwave: server_url must be an https address; it defaults to https://waverunner.adwave.com/api/mcp"))
  (unless (nonblank (setting :token))
    (error 'nlk:config-refusal
           :detail (format nil "adwave: no API key. Create one in Adwave (account menu, API & agents) and paste it into the token member of the adwave section, name it in token_env or token_file, or sign in to ~a once in /mcp (that sign-in is reused); then (restart-cells \"nodecode-adwave\")"
                            (setting :server-url)))))

;;; --- the wire: MCP over streamable HTTP -------------------------------------------
;; One POST per JSON-RPC message, and an answer that is either the bare object
;; or a one-event stream (data: {...}). The server hands a session id at
;; initialize and wants it back on every request after that; a server that
;; hands none is stateless, and a 404 means the session it gave is gone.

(defvar *session-id* nil
  "The Mcp-Session-Id this image was given at initialize, or NIL.")

(defvar *negotiated* nil
  "The MCP revision the server answered at initialize, sent with every request.")

(defvar *initialized* nil
  "Whether this image signed in to the server since the cell started.")

(defvar *offered* nil
  "The tools the server last listed: ((:name :description :read-only) ...).")

(defvar *request-id* 0)

(defun reset-session ()
  (setf *session-id* nil *negotiated* nil *initialized* nil *offered* nil *request-id* 0))

(defun next-id ()
  (incf *request-id*))

(defun header-value (headers name)
  "NAME's value in the response HEADERS nlk:http answers, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun request-headers ()
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (or (setting :token) ""))))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (when *negotiated* (list (cons "mcp-protocol-version" *negotiated*)))))

(defun post-json (body)
  "One HTTPS POST of BODY to the server => (values TEXT STATUS HEADERS)."
  (nlk:http :post (setting :server-url)
            :headers (request-headers)
            :content body
            :timeout (setting :timeout-seconds)
            :connect-timeout (min 20 (setting :timeout-seconds))))

(defun stream-json (text)
  "The last JSON object a one-event stream body carries, or NIL."
  (let ((last nil))
    (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
          do (let ((line (string-right-trim '(#\Return) line)))
               (when (and (> (length line) 5) (string-equal "data:" (subseq line 0 5)))
                 (let ((payload (nlk:trimmed (subseq line 5))))
                   (when (and (plusp (length payload)) (char= (char payload 0) #\{))
                     (setf last payload))))))
    last))

(defun rpc-object (text)
  "The JSON-RPC object in TEXT: a bare body, or the last data line of a stream."
  (let* ((text (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
         (text (cond ((null text) nil)
                     ((zerop (length text)) nil)
                     ((char= (char text 0) #\{) text)
                     (t (stream-json text)))))
    (when (and text (plusp (length text)))
      (ignore-errors (nlk:decode-json text)))))

(defun rpc-error-text (object)
  "The words a JSON-RPC error object carries, or NIL."
  (let ((error (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (when error
      (or (nlk:json-value error :text "message")
          (nlk:one-line (nlk:encode-json-object error))))))

(defun fail-on-status (status object)
  (cond
    ((member status '(401 403))
     (fail "Adwave refused the API key (HTTP ~a): it is missing, expired or revoked. Create a new one in Adwave (account menu, API & agents) and set it in the adwave section, or sign in again in /mcp, then (restart-cells \"nodecode-adwave\")." status))
    (t
     (fail "Adwave's MCP server answered HTTP ~a~@[: ~a~]" status (rpc-error-text object)))))

(defun send-rpc (method params)
  "One JSON-RPC request of METHOD with PARAMS => (values OBJECT STATUS)."
  (let ((id (next-id)))
    (multiple-value-bind (text status headers)
        (post-json (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                     "params" (or params (nlk:make-json-object)))))
      (let ((session (header-value headers "mcp-session-id")))
        (when session
          (setf *session-id* (if (stringp session) session (princ-to-string session)))))
      (values (rpc-object text) status))))

(defun send-note (method)
  "The one no-answer message MCP wants after initialize; a 202 is the whole answer."
  (ignore-errors
    (post-json (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0" "method" method
                                 "params" (nlk:make-json-object))))
    t))

(defun initialize ()
  (multiple-value-bind (object status)
      (send-rpc "initialize"
                (nlk:json-object "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:make-json-object)
                                  "clientInfo" (nlk:json-object "name" "nodecode-adwave"
                                                                 "version" +client-version+)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (setf *negotiated*
          (or (nlk:json-value (nlk:json-value object :object "result") :text "protocolVersion")
              +protocol-version+))
    (send-note "notifications/initialized")
    (setf *initialized* t)
    t))

(defun ensure-session ()
  (unless *initialized* (initialize)))

(defun mcp-call (method &optional params)
  "One MCP method, signing in first and once more after a session the server
forgot => the result object."
  (ensure-session)
  (multiple-value-bind (object status) (send-rpc method params)
    (when (equal status 404)
      (reset-session)
      (ensure-session)
      (multiple-value-bind (again again-status) (send-rpc method params)
        (setf object again status again-status)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (let ((reason (rpc-error-text object)))
      (when reason
        (fail "Adwave refused ~a: ~a" method reason)))
    (or (nlk:json-value object :object "result")
        (nlk:make-json-object))))

;;; --- what one tools/call answers ---------------------------------------------------

(defun content-text (result)
  "The text parts of one tools/call RESULT, joined by a newline."
  (with-output-to-string (out)
    (let ((first t))
      (loop for part across (nlk:json-array result "content")
            when (equal "text" (nlk:json-value part :text "type"))
              do (let ((text (nlk:json-value part :string "text")))
                   (when text
                     (unless first (terpri out))
                     (setf first nil)
                     (write-string text out)))))))

(defun render-result (result)
  "One tools/call RESULT as the model reads it => (values TEXT ERROR-P)."
  (let* ((structured (nlk:json-value result :object "structuredContent"))
         (errorp (and (nlk:json-value result :boolean "isError") t))
         (text (or (and structured (nlk:encode-json-object structured))
                   (content-text result))))
    (values (if (and text (plusp (length text))) text (nlk:encode-json-object result))
            errorp)))

(defun call-tool-raw (tool arguments)
  "One tools/call of TOOL => the result object."
  (mcp-call "tools/call"
            (nlk:json-object "name" tool
                              "arguments" (or arguments (nlk:make-json-object)))))

(defun tool-answer (tool arguments)
  "One MCP tool call as the model reads it: its text, clipped, or the failure it reported."
  (multiple-value-bind (text errorp) (render-result (call-tool-raw tool arguments))
    (if errorp
        (nle:failure (format nil "ERROR: Adwave said ~a" text))
        (clip text (setting :max-chars)))))

;;; --- the tools the server offers ----------------------------------------------------
;; The server changes its tool list without this cell moving, so the list is
;; read once per session and read again when a name it does not carry is
;; asked for. A tool's own readOnlyHint -- the annotation the original's
;; README says marks what may run without asking -- decides whether a call
;; needs the person's yes first; a tool that carries none is treated as one
;; that changes the account.

(defun read-only-hint (tool)
  "T, NIL, or :UNKNOWN when the tool carries no readOnlyHint annotation."
  (let ((annotations (nlk:json-value tool :object "annotations")))
    (multiple-value-bind (value present) (json-member annotations "readOnlyHint")
      (if present (and value t) :unknown))))

(defun schema-arguments (tool)
  "The property names TOOL's input schema carries, and the required ones.
That schema is what an MCP client would hand its model; this is the same
thing, said in one line the listing can carry."
  (let* ((schema (nlk:json-value tool :object "inputSchema"))
         (properties (nlk:json-value schema :object "properties"))
         (required (nlk:json-value schema :array "required"))
         (names nil))
    (when (hash-table-p properties)
      (maphash (lambda (name value)
                 (declare (ignore value))
                 (push name names))
               properties))
    (values (sort names #'string<)
            (and required (coerce required 'list)))))

(defun tool-entry (tool)
  (multiple-value-bind (arguments required) (schema-arguments tool)
    (list :name (nlk:json-value tool :text "name")
          :description (or (nlk:json-value tool :text "description") "")
          :read-only (read-only-hint tool)
          :arguments arguments
          :required required)))

(defun read-tool-list ()
  "Every tool the server offers, read from tools/list."
  (let ((result (mcp-call "tools/list"))
        (entries nil))
    (loop for tool across (nlk:json-array result "tools")
          for entry = (tool-entry tool)
          when (getf entry :name)
            do (push entry entries))
    (nreverse entries)))

(defun offered-tools (&key refresh)
  "The tools the server offers, read once and kept until REFRESH."
  (when (or refresh (null *offered*))
    (setf *offered* (read-tool-list)))
  *offered*)

(defun first-sentence (text)
  "TEXT up to its first sentence's end, at most 300 characters."
  (let* ((text (or text ""))
         (end (or (search ". " text) (position #\Newline text) (length text))))
    (clip (subseq text 0 (min end 300)) 300)))

(defun tool-line (entry)
  (format nil "~a~a~%~a~@[~%  arguments: ~{~a~^, ~}~@[ (required: ~{~a~^, ~})~]~]"
          (getf entry :name)
          (if (eq t (getf entry :read-only)) " [read-only]" " [needs confirmed true]")
          (first-sentence (getf entry :description))
          (getf entry :arguments)
          (let ((required (getf entry :required)))
            (and required (intersection required (getf entry :arguments) :test #'string=)))))

(defun listing (entries filter)
  "The list of ENTRIES as the model reads it, or what to say when FILTER found none."
  (if (null entries)
      (if filter
          (format nil "No tool from ~a names or describes ~s; adwave_tools with no filter lists all ~d."
                  (setting :server-url) filter (length (offered-tools)))
          (format nil "~a offered no tools." (setting :server-url)))
      (with-output-to-string (out)
        (format out "~d tools from ~a~2%" (length entries) (setting :server-url))
        (dolist (entry entries)
          (format out "~a~%" (tool-line entry))))))

(defparameter +list-schema+
  (list :description
        (concatenate 'string
                     "List the tools the Adwave server offers: each name, what it does, whether it is "
                     "read-only or needs the person's yes (confirmed true) before it runs, and the "
                     "argument names it takes. Call it before the first adwave_call, and again when a "
                     "name is refused as unknown.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "filter" (nlk:make-json-object
                                  "type" "string"
                                  "description" "only the tools whose name or description contains this word")))))

(defun list-tools (arguments)
  "adwave_tools: the server's own tools/list, as one list the model reads."
  (answering
    (let* ((filter (nonblank (nlk:json-value arguments :text "filter")))
           (entries (if filter
                        (remove-if-not
                         (lambda (entry)
                           (search (string-downcase filter)
                                   (string-downcase (format nil "~a ~a"
                                                            (getf entry :name)
                                                            (getf entry :description)))))
                         (offered-tools))
                        (offered-tools))))
      (clip (listing entries filter) (setting :max-chars)))))

(defparameter +call-schema+
  (list :description
        (concatenate 'string
                     "Run one Adwave tool by the name adwave_tools lists, with that tool's own "
                     "arguments object. A tool not marked read-only changes the account or spends "
                     "from the prepaid wallet and is refused until confirmed is true: say the amount "
                     "in dollars first and get the person's clear yes in this conversation, for this "
                     "call. Read-only tools need no confirmation.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "name" (nlk:make-json-object
                               "type" "string"
                               "description" "the tool's name, such as add_business or get_campaign_report")
                       "arguments" (nlk:make-json-object
                                    "type" "object"
                                    "additionalProperties" t
                                    "description" "the tool's own arguments object, as the server's schema describes them")
                       "confirmed" (nlk:make-json-object
                                    "type" "boolean"
                                    "description" "true only after the person said yes to this exact call, with its amount said first"))
         "required" (vector "name"))))

(defun spending-p (name)
  (and (member name +spending+ :test #'string=) t))

(defun spend-refusal (name)
  (refuse "~a ~a. Say the amount in dollars first and get the person's clear yes in this conversation, then call adwave_call again with confirmed true."
          name
          (if (spending-p name)
              "spends from the Adwave wallet and needs the person's yes"
              "changes the Adwave account and needs the person's yes")))

(defun find-offered (name)
  (find name (offered-tools) :test #'string= :key (lambda (entry) (getf entry :name))))

(defun call-tool (arguments)
  "adwave_call: one tools/call of a name the server listed, gated on the person's yes."
  (answering
    (let* ((name (or (nonblank (nlk:json-value arguments :text "name"))
                     (return-from call-tool (refuse "name is required: the tool to run, as adwave_tools lists it"))))
           (payload (or (nlk:json-value arguments :object "arguments") (nlk:make-json-object)))
           (confirmed (and (nlk:json-value arguments :boolean "confirmed") t))
           (entry (or (find-offered name)
                      (find name (offered-tools :refresh t) :test #'string=
                            :key (lambda (entry) (getf entry :name))))))
      (unless entry
        (return-from call-tool
          (refuse "~a is not a tool this Adwave server offers; adwave_tools lists the ~d it does"
                  name (length (offered-tools)))))
      (when (and (not confirmed) (not (eq t (getf entry :read-only))))
        (return-from call-tool (spend-refusal name)))
      (tool-answer name payload))))

;;; --- the operator's command ----------------------------------------------------------

(defun run-command (args session-id)
  "One /adwave invocation: with no argument, the tools the server offers and
where the key comes from; with a word, the tools that word names; with login,
only where the key comes from."
  (declare (ignore session-id))
  (let ((wanted (nlk:trimmed (or args ""))))
    (cond ((zerop (length wanted))
           (format nil "~a~%The key in use comes from ~a." (list-tools nil) (token-source)))
          ((member wanted '("login" "key" "signin" "sign-in") :test #'string-equal)
           (format nil "adwave: the key in use comes from ~a. ~
Create one in Adwave (account menu, API & agents) and paste it into the token member of the adwave ~
section, or sign in to ~a once in /mcp and this cell reuses that sign-in."
                   (token-source) (setting :server-url)))
          (t (list-tools (nlk:make-json-object "filter" wanted))))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell adwave
  (:section ("adwave")
    (:guide "create an API key in Adwave (account menu, API & agents) and paste it into token, or name it in token_env or token_file; server_url defaults to https://waverunner.adwave.com/api/mcp; then /adwave lists the tools the server offers")
    ("server_url" :string :default "https://waverunner.adwave.com/api/mcp"
     :doc "Adwave's MCP server, streamable HTTP")
    ("token" :secret
     :doc "an Adwave API key; empty reuses an environment variable, a file, or the sign-in the mcp cell keeps")
    ("token_env" :env
     :doc "the environment variable holding an API key instead of the token member")
    ("token_file" :path
     :doc "a file holding an API key instead of the token member")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one call's ceiling: a website analysis can be slow")
    ("max_chars" :integer :default 12000 :min 500
     :doc "how much of one answer the model reads: a full tool list runs long"))
  (:settings 'read-settings)
  (:help :adwave
         "adwave: a business website into ads on TV, Meta, Google, YouTube and Reddit, on a prepaid budget - (help :adwave)"
         +primer+)
  (:command "adwave" 'run-command
            :description "adwave: the tools the Adwave server offers, or where the key came from"
            :argument-hint "FILTER"
            :session nil)
  (:tool "adwave_tools" +list-schema+ 'list-tools)
  (:tool "adwave_call" +call-schema+ 'call-tool)
  (:start (lambda ()
            (check-config)
            (reset-session)
            (nle:on-stop #'reset-session))))
