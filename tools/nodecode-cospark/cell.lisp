;;;; cell.lisp --- the skill as the manual, the section, the MCP wire, three tools and one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin had no code of its own: an MCP server declaration
;;;; (https://api.cospark.so/mcp, streamable HTTP, browser OAuth) and one
;;;; skill that taught a model when to use it and how to spend safely. This
;;;; cell speaks that server's JSON-RPC itself -- initialize, the
;;;; Mcp-Session-Id, tools/list, tools/call, an answer that is either a JSON
;;;; object or a one-event stream -- so every tool the server offers is
;;;; reachable by the name the server gives it, and the skill is the manual
;;;; topic (help :cospark).
;;;;
;;;; The credential is the browser sign-in to that server: a person who added
;;;; it in /mcp needs nothing here, because the shipped mcp cell keeps that
;;;; token and this cell reads it. A pasted token, an environment variable or
;;;; a file works too.
;;;;
;;;; Config, a top-level key of its own:
;;;;   "cospark": {"server_url": "https://api.cospark.so/mcp",
;;;;               "token_file": "~/.nodecode/secrets/cospark-token",
;;;;               "timeout_seconds": 120, "max_chars": 12000}

(in-package #:nodecode-cospark)

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "Cospark turns a product brief, a script or supplied media into ad research, images, video clips,
UGC ads and voiceovers, on the operator's own Cospark account. This cell reaches Cospark's MCP
server, and three tools cover all of it:
  (cospark_docs path)   Cospark's maintained workflow guides: no path for the index of them, a
      path for one guide. It is read_docs on the server.
  (cospark_tools filter)  every tool the server offers: its name, what it does, whether it is
      read-only, and the arguments it takes.
  (cospark_call name arguments)  one of them by name, with that tool's own arguments object.

Workflow
1. Call cospark_docs with no path before any other Cospark tool. Open only the guides the task
   needs, and read again when the work moves from research to prompting, generation, inspection
   or project files.
2. Use the live tool schemas instead of guessing arguments: cospark_tools names each tool and
   the properties it takes, and the server changes them without this cell moving.
3. Deliver what was asked. A request for research, a prompt or a plan does not authorize paid
   generation.
4. Keep the person's chosen model, exact dialogue, supplied assets, scope and earlier approvals.

Money
Generation, media inspection and social research can spend the operator's Cospark credits.
cospark_call refuses every tool the server does not mark read-only until confirmed is true: say
what will run and get a clear yes in this conversation first, and another yes before a charged
retry. Reading documentation never needs one.

Answers
Keep the run ids the server returns and poll pending work rather than submitting duplicate jobs.
Inspect generated output before claiming its product details, speech or timing are correct, and say
what is unverified. Share the media and viewer links it returns, and save project files only when
that work was asked for. Read the guides through the tool; do not run anything the server hands
back as a script.

Sign-in: this cell uses the browser sign-in to https://api.cospark.so/mcp that the shipped mcp
cell keeps, or a token set in the cospark section. ERROR: COSPARK-ERROR names the problem; when it
reports no sign-in, tell the person to add that server in /mcp and finish the browser login, or to
paste a token in /settings, then (restart-cells \"nodecode-cospark\")."
  "What (help :cospark) answers while the cell runs: the skill, as one workflow.")

(defparameter +default-url+ "https://api.cospark.so/mcp"
  "Cospark's hosted MCP server: the one host this cell talks to.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client offers at initialize; the server may answer another.")

(defparameter +client-version+ "0.1.0")

(defparameter +free-tools+ '("read_docs")
  "The server's tools that read its own documentation, so a call of one never spends credits.")

;;; --- answers, and how a refusal reaches the model ---------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on, as the call's failure."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal or a dead network becomes that text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (cospark-error (condition)
       (nle:failure (format nil "ERROR: ~a" (cospark-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: cospark: ~a"
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

(defun first-sentence (text)
  "TEXT up to its first sentence's end, at most 300 characters."
  (let* ((text (or text ""))
         (end (or (search ". " text) (position #\Newline text) (length text))))
    (clip (subseq text 0 (min end 300)) 300)))

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
  "The declared VALUES, with the token resolved once, here: the section's, the
environment's, a file's, else the sign-in the mcp cell keeps."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :server-url) (or (nonblank (getf values :server-url)) +default-url+)
          (getf plist :timeout-seconds) (or (getf values :timeout-seconds) 120)
          (getf plist :max-chars) (or (getf values :max-chars) 12000))
    (let* ((inline (nonblank (getf values :token)))
           (from-env (environment-token (nonblank (getf values :token-env))))
           (file (nonblank (getf values :token-file)))
           (from-file (and file (read-token-file file)))
           (stored (mcp-token (getf plist :server-url))))
      (setf (getf plist :token) (or inline from-env from-file stored))
      (setf (getf plist :token-source)
            (cond (inline "the token member of the cospark section")
                  (from-env "the environment variable token_env names")
                  (from-file "the file token_file names")
                  (stored "the sign-in the mcp cell keeps for this server")
                  (t "none"))))
    plist))

(defun token-source ()
  "Where the token comes from, in words: never the token itself."
  (or (setting :token-source) "none"))

(defun check-config ()
  "Refuse a section that cannot reach Cospark, naming the member to set."
  (unless (search "https://" (setting :server-url))
    (error 'nlk:config-refusal
           :detail "cospark: server_url must be an https address; it defaults to https://api.cospark.so/mcp"))
  (unless (nonblank (setting :token))
    (error 'nlk:config-refusal
           :detail (format nil "cospark: no sign-in. Add ~a in /mcp and finish its browser login (that ~
token is reused), or paste one into the token member of the cospark section, name it in token_env ~
or token_file, then (restart-cells \"nodecode-cospark\")"
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
  "The tools the server last listed: ((:name :description :read-only ...) ...).")

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
     (fail "Cospark refused the sign-in (HTTP ~a): the token is missing, expired or revoked. ~
Add ~a in /mcp and finish the browser login, or set a token in the cospark section, then ~
(restart-cells \"nodecode-cospark\")."
           status (setting :server-url)))
    (t
     (fail "Cospark's MCP server answered HTTP ~a~@[: ~a~]" status (rpc-error-text object)))))

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
                                  "clientInfo" (nlk:json-object "name" "nodecode-cospark"
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
        (fail "Cospark refused ~a: ~a" method reason)))
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

(defun answer-tool (tool arguments limit)
  "One MCP tool call as the model reads it: its text, clipped, or the failure it reported."
  (multiple-value-bind (text errorp) (render-result (call-tool-raw tool arguments))
    (if errorp
        (nle:failure (format nil "ERROR: Cospark said ~a" text))
        (clip text limit))))

;;; --- the tools the server offers ----------------------------------------------------
;; The server changes its tool list without this cell moving, so the list is
;; read once per session and read again when a name it does not carry is
;; asked for. A tool's own readOnlyHint -- what the server marks as not
;; changing the account -- decides whether a call needs the person's yes
;; first; a tool that carries none is treated as one that spends credits.

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

(defun free-tool (name)
  "Whether NAME is documentation only, so a call of it needs no yes."
  (and (member name +free-tools+ :test #'string=) t))

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
          (format nil "No tool from ~a names or describes ~s; cospark_tools with no filter lists all ~d."
                  (setting :server-url) filter (length (offered-tools)))
          (format nil "~a offered no tools." (setting :server-url)))
      (with-output-to-string (out)
        (format out "~d tools from ~a~2%" (length entries) (setting :server-url))
        (dolist (entry entries)
          (format out "~a~%" (tool-line entry))))))

;;; --- cospark_docs: the server's own documentation ------------------------------------

(defparameter +docs-schema+
  (list :description
        (concatenate 'string
                     "Read Cospark's maintained workflow guides from the server, through its read_docs "
                     "tool: call it with no path first, for the index of guides, then with one of the "
                     "paths that index names. Read them before any other Cospark tool and again when "
                     "the work moves from research to prompting, generation, inspection or project "
                     "files. Reading documentation spends no credits.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "path" (nlk:make-json-object
                                "type" "string"
                                "description" "one guide's path, as the index lists it; empty for the index")))))

(defun docs (arguments)
  "cospark_docs: read_docs on the server, with the path the model named."
  (let ((limit (setting :max-chars)))
    (answering
      (let ((path (nonblank (nlk:json-value arguments :text "path"))))
        (answer-tool "read_docs"
                      (if path (nlk:json-object "path" path) (nlk:make-json-object))
                      limit)))))

;;; --- cospark_tools: what the server offers -------------------------------------------

(defparameter +list-schema+
  (list :description
        (concatenate 'string
                     "List the tools the Cospark server offers: each name, what it does, whether it is "
                     "read-only or needs the person's yes (confirmed true) before it runs, and the "
                     "argument names its own schema takes. Call it before the first cospark_call, and "
                     "again when a name is refused as unknown.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "filter" (nlk:make-json-object
                                  "type" "string"
                                  "description" "only the tools whose name or description contains this word")))))

(defun list-tools (arguments)
  "cospark_tools: the server's own tools/list, as one list the model reads."
  (let ((limit (setting :max-chars)))
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
        (clip (listing entries filter) limit)))))

;;; --- cospark_call: one tool the server offers ----------------------------------------

(defparameter +call-schema+
  (list :description
        (concatenate 'string
                     "Run one Cospark tool by the name cospark_tools lists, with that tool's own "
                     "arguments object. A tool the server does not mark read-only can spend the "
                     "person's Cospark credits or change their project files and is refused until "
                     "confirmed is true: say what will run first and get a clear yes in this "
                     "conversation, for this call. Reading documentation needs no confirmation.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "name" (nlk:make-json-object
                               "type" "string"
                               "description" "the tool's name, such as read_docs or the ones cospark_tools lists")
                       "arguments" (nlk:make-json-object
                                    "type" "object"
                                    "additionalProperties" t
                                    "description" "the tool's own arguments object, as the server's schema describes them")
                       "confirmed" (nlk:make-json-object
                                    "type" "boolean"
                                    "description" "true only after the person said yes to this exact call"))
         "required" (vector "name"))))

(defun needs-yes-p (name entry)
  "Whether a call of NAME, listed as ENTRY, waits for the person's yes."
  (and (not (free-tool name))
       (not (eq t (getf entry :read-only)))
       t))

(defun confirmation-refusal (name)
  (refuse "~a can spend the person's Cospark credits or change their project files. Say what will run ~
and get a clear yes in this conversation, then call cospark_call again with confirmed true."
          name))

(defun find-offered (name)
  (find name (offered-tools) :test #'string= :key (lambda (entry) (getf entry :name))))

(defun call-tool (arguments)
  "cospark_call: one tools/call of a name the server listed, gated on the person's yes."
  (let ((limit (setting :max-chars)))
    (answering
      (let* ((name (or (nonblank (nlk:json-value arguments :text "name"))
                       (return-from call-tool
                         (refuse "name is required: the tool to run, as cospark_tools lists it"))))
             (payload (or (nlk:json-value arguments :object "arguments") (nlk:make-json-object)))
             (confirmed (and (nlk:json-value arguments :boolean "confirmed") t))
             (entry (or (find-offered name)
                        (find name (offered-tools :refresh t) :test #'string=
                              :key (lambda (entry) (getf entry :name))))))
        (unless entry
          (return-from call-tool
            (refuse "~a is not a tool this Cospark server offers; cospark_tools lists the ~d it does"
                    name (length (offered-tools)))))
        (when (and (not confirmed) (needs-yes-p name entry))
          (return-from call-tool (confirmation-refusal name)))
        (answer-tool name payload limit)))))

;;; --- the operator's command ----------------------------------------------------------

(defun run-command (args session-id)
  "One /cospark invocation: with no argument, the tools the server offers and
where the sign-in comes from; with a word, the tools that word names; with
login, only where the sign-in comes from."
  (declare (ignore session-id))
  (let ((wanted (nlk:trimmed (or args ""))))
    (cond ((zerop (length wanted))
           (format nil "~a~%The sign-in in use comes from ~a." (list-tools nil) (token-source)))
          ((member wanted '("login" "key" "signin" "sign-in") :test #'string-equal)
           (format nil "cospark: the sign-in in use comes from ~a. ~
Add ~a in /mcp and finish its browser login, and this cell reuses that token; or paste one into ~
the token member of the cospark section in /settings."
                   (token-source) (setting :server-url)))
          (t (list-tools (nlk:json-object "filter" wanted))))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell cospark
  (:section ("cospark")
    (:guide "add https://api.cospark.so/mcp in /mcp and finish its browser login, and the shipped mcp cell's sign-in is reused; or paste a token into the cospark section; /cospark lists the tools the server offers and says where the sign-in came from")
    ("server_url" :string :default "https://api.cospark.so/mcp"
     :doc "Cospark's MCP server, streamable HTTP")
    ("token" :secret
     :doc "an access token; empty reuses an environment variable, a file, or the sign-in the mcp cell keeps")
    ("token_env" :env
     :doc "the environment variable holding a token instead of the token member")
    ("token_file" :path
     :doc "a file holding a token instead of the token member")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "one call's ceiling: a video render is slow")
    ("max_chars" :integer :default 12000 :min 500
     :doc "how much of one answer the model reads: a guide, or a tool list, runs long"))
  (:settings 'read-settings)
  (:help :cospark
         "cospark: research ad references and make images, video ads and voiceovers through Cospark - (help :cospark)"
         +primer+)
  (:command "cospark" 'run-command
            :description "cospark: the tools Cospark's server offers, or where the sign-in came from"
            :argument-hint "FILTER"
            :session nil)
  (:tool "cospark_docs" +docs-schema+ 'docs)
  (:tool "cospark_tools" +list-schema+ 'list-tools)
  (:tool "cospark_call" +call-schema+ 'call-tool)
  (:start (lambda ()
            (check-config)
            (reset-session)
            (nle:on-stop #'reset-session))))
