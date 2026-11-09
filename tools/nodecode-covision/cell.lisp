;;;; cell.lisp --- Covision: the client's tasks, meetings and published
;;;; knowledge, read-only, through the vendor's own MCP connector.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of Covision-Consultants/covision-agent-plugin for Nodecode (see
;;;; NOTICE). The original is a portable bundle of two things: an MCP
;;;; declaration naming Covision's production Streamable HTTP endpoint, and a
;;;; skill holding the model to what the signed-in user may read.
;;;;
;;;; Both are kept. The endpoint is this section's default; the connector's
;;;; own tools are listed during the handshake and registered as Nodecode
;;;; tools, so the model calls the same approved read-only tools the original's
;;;; runtime would have listed; and the skill's operating rules ride every
;;;; request as a standing section named `covision'.
;;;;
;;;; The wire is MCP's Streamable HTTP transport, JSON-RPC 2.0: one POST per
;;;; message, the session id the connector answers `initialize' with carried
;;;; back on `Mcp-Session-Id', the reply in a JSON body or in the last
;;;; `data:' line of an event stream. Tokens come from the section; the
;;;; browser sign-in the original leaves to its runtime is the operator's.

(defpackage #:nodecode-covision
  (:use #:cl)
  (:export #:covision-error #:covision-error-detail))

(in-package #:nodecode-covision)

(nlk:define-peripheral covision :not-running t :lock "covision")

(defparameter +endpoint+ "https://connector.covisionperformance.com/mcp"
  "Covision's production connector, the original's mcp.json, unless the
section names another.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks.")

(defparameter +client-version+ "0.1.0"
  "What the initialize message calls this client.")

(defvar *session-id* nil
  "The session the connector handed back at the handshake, or NIL.")

(defvar *initialized-p* nil
  "Whether the connector has accepted an initialize this start.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request: one per message, from this counter.")

(defvar *connector-tools* '()
  "The connector's tools as of the last listing: (NAME DESCRIPTION SCHEMA) ...")

(defvar *registered* '()
  "The Nodecode tool names this cell registered from the last listing.")

;;; --- the section, read back --------------------------------------------------

(defun token ()
  "The OAuth access token, inline or by environment variable."
  (or (let ((inline (setting :access-token)))
        (and inline (plusp (length (string-trim '(#\Space) inline))) inline))
      (let ((variable (setting :access-token-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      ""))

(defun endpoint ()
  "The connector's address, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :endpoint) ""))))
    (if (plusp (length given)) given +endpoint+)))

(defun check-config ()
  "Refuse a section that could never reach the connector, naming the member."
  (when (zerop (length (string-trim '(#\Space) (token))))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                "covision: the section needs a Covision access token: set \"access_token\", "
                                "or \"access_token_env\" naming the variable that holds one; a token carries the scopes "
                                "tasks:read, meetings:read and knowledge:read"))))

;;; --- the wire -----------------------------------------------------------------

(defun next-id ()
  (incf *request-id*))

(defun header-value (headers name)
  "HEADERS' NAME, case-insensitively, however the transport hands them over."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-capitalize name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun wire-headers ()
  "What every message carries: the token, and the session once there is one."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (token))))
          (when *session-id*
            (list (cons "mcp-session-id" *session-id*)
                  (cons "mcp-protocol-version" +protocol-version+)))))

(defun stream-payload (text)
  "TEXT's last `data:' payload when it is an event stream, else TEXT."
  (if (not (and (stringp text) (search "data:" text)))
      text
      (let ((payload nil))
        (dolist (line (uiop:split-string text :separator '(#\Newline)))
          (let ((line (string-trim '(#\Return #\Tab #\Space) line)))
            (when (uiop:string-prefix-p "data:" line)
              (let ((data (string-trim '(#\Space) (subseq line 5))))
                (unless (equal data "[DONE]")
                  (setf payload data))))))
        (or payload text))))

(defun body-value (text)
  "TEXT -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (if (and (stringp text)
           (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
      (handler-case (nlk:decode-json (stream-payload text))
        (error () (nlk:json-object)))
      (nlk:json-object)))

(defun wire-failure (condition)
  (fail "covision: ~a" (nle:transport-failure-label condition (endpoint))))

(defun exchange (method params &key notification)
  "One JSON-RPC message to the connector.
=> (values RESULT STATUS HEADERS BODY TEXT), RESULT and BODY decoded."
  (let ((request (nlk:make-json-object "jsonrpc" "2.0" "method" method)))
    (unless notification
      (setf (gethash "id" request) (next-id)))
    (when params
      (setf (gethash "params" request) params))
    (let (text status headers)
      (handler-case
          (multiple-value-setq (text status headers)
            (nlk:http :post (endpoint)
                      :headers (wire-headers)
                      :content (nlk:encode-json-object request)
                      :timeout (setting :timeout-seconds)
                      :connect-timeout (min 10 (setting :timeout-seconds))))
        ((or error sb-sys:deadline-timeout) (condition)
          (wire-failure condition)))
      (let ((body (body-value text)))
        (values (and (hash-table-p body) (gethash "result" body))
                status headers body text)))))

(defun rpc-error-text (body text)
  "What a refused message says, for the one line the model reads."
  (let ((error (and (hash-table-p body) (gethash "error" body))))
    (or (and (hash-table-p error) (nlk:json-value error :string "message"))
        (and (stringp text) (plusp (length text))
             (subseq text 0 (min 200 (length text))))
        "no detail")))

(defun refused-status (status body text)
  "The HTTP STATUS the connector refused with, its own words when it gave any."
  (if (eql status 401)
      (fail "covision: the connector refused the access token (HTTP 401): it may have expired or been revoked, so paste a fresh one in the covision section")
      (fail "covision: HTTP ~a from the connector~@[: ~a~]" status (rpc-error-text body text))))

(defun handshake ()
  "initialize, then the initialized notification: keeps the session it answers."
  (setf *session-id* nil
        *initialized-p* nil)
  (multiple-value-bind (result status headers body text)
      (exchange "initialize"
                (nlk:json-object "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:json-object)
                                  "clientInfo" (nlk:json-object "name" "nodecode-covision"
                                                                 "version" +client-version+)))
    (declare (ignore result))
    (unless (and (integerp status) (< status 300))
      (refused-status status body text))
    (setf *session-id* (header-value headers "mcp-session-id")
          *initialized-p* t))
  (exchange "notifications/initialized" (nlk:json-object) :notification t)
  t)

(defun mcp (method &optional params)
  "METHOD on the connector => its result object. A session the connector has
forgotten is handshaken again and the call made once more."
  (labels ((once ()
             (unless *initialized-p* (handshake))
             (multiple-value-bind (result status body text) (exchange method params)
               (declare (ignore body))
               (cond ((and (hash-table-p result) (gethash "error" result))
                      (fail "covision: ~a" (rpc-error-text result text)))
                     ((not (and (integerp status) (< status 300)))
                      (if (and (member status '(400 404)) *session-id*)
                          :again
                          (refused-status status (body-value text) text)))
                     (t (or result (nlk:json-object)))))))
    (let ((answer (once)))
      (if (eq answer :again)
          (progn (setf *initialized-p* nil
                       *session-id* nil)
                 (once))
          answer))))

;;; --- the connector's own tools --------------------------------------------------

(defun connector-tools ()
  "The connector's tools, refreshed: (NAME DESCRIPTION SCHEMA) ..."
  (let ((result (mcp "tools/list" (nlk:json-object)))
        (tools '()))
    (let ((listed (and (hash-table-p result) (nlk:json-value result :array "tools"))))
      (loop for tool across (or listed #())
            when (hash-table-p tool)
              do (let ((name (nlk:json-value tool :string "name")))
                   (when name
                     (push (list name
                                 (nlk:json-value tool :string "description")
                                 (nlk:json-value tool :object "inputSchema"))
                           tools)))))
    (setf *connector-tools* (nreverse tools))))

(defun tool-name-for (name)
  "NAME's Nodecode spelling: its own when it already says covision."
  (if (uiop:string-prefix-p "covision" name)
      name
      (format nil "covision_~a" name)))

(defun content-text (result)
  "RESULT's content as one text: every text block in order."
  (let ((content (and (hash-table-p result) (nlk:json-value result :array "content")))
        (parts '()))
    (loop for block across (or content #())
          when (and (hash-table-p block)
                    (equal (nlk:json-value block :string "type") "text"))
            do (let ((text (nlk:json-value block :string "text")))
                 (when text (push text parts))))
    (when parts
      (format nil "~{~a~^~%~}" (nreverse parts)))))

(defun call-text (name arguments)
  "NAME called on the connector with ARGUMENTS => the text the model reads."
  (let* ((result (mcp "tools/call"
                       (nlk:json-object "name" name
                                         "arguments" (or (and (hash-table-p arguments) arguments)
                                                         (nlk:json-object)))))
         (text (content-text result)))
    (cond ((nlk:json-value result :boolean "isError")
           (fail "covision: ~a" (or text (format nil "the tool ~a reported an error" name))))
          (text text)
          (t (format nil "The connector answered ~a with no text: ~a"
                     name (nlk:encode-json-object result))))))

(defun register-connector-tool (name description schema)
  "Register the connector's NAME as a Nodecode tool, once per start."
  (let ((tool-name (tool-name-for name)))
    (unless (member tool-name *registered* :test #'string=)
      (nle:register-tool
       tool-name
       (list :description (or description
                              (format nil "Covision: ~a, on the signed-in user's connector." name))
             :parameters (or (and (hash-table-p schema) schema)
                             (nlk:json-object "type" "object")))
       (lambda (arguments) (tool-answer name arguments)))
      (push tool-name *registered*))
    tool-name))

(defun register-connector-tools ()
  "Every tool the connector lists, as a Nodecode tool; the listing itself."
  (dolist (tool (connector-tools))
    (apply #'register-connector-tool tool))
  *connector-tools*)

(defun unregister-connector-tools ()
  (dolist (name *registered*)
    (ignore-errors (nle:unregister-tool name)))
  (setf *registered* '()))

;;; --- what the model calls --------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: covision: ~a" condition)))))

(defun tool-answer (name arguments)
  "NAME run on the connector, its refusal as the one line the model repairs from."
  (answering (call-text name arguments)))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (and (hash-table-p args) (gethash name args))))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "covision: ~a is required" (substitute #\_ #\- name)))))

(defun argument-names (schema)
  "SCHEMA's declared property names, in order."
  (let ((properties (and (hash-table-p schema) (nlk:json-value schema :object "properties")))
        (names '()))
    (when (hash-table-p properties)
      (maphash (lambda (name value) (declare (ignore value)) (push name names)) properties))
    (sort names #'string<)))

(defun one-line (text limit)
  "TEXT on one line, at most LIMIT characters: a tool description in a list."
  (let ((flat (substitute #\Space #\Newline (string-trim '(#\Space) (or text "")))))
    (if (<= (length flat) limit) flat (concatenate 'string (subseq flat 0 (1- limit)) "…"))))

(defun tool-tools (arguments)
  "covision_tools: what the signed-in user's connector approves, and takes."
  (declare (ignore arguments))
  (answering
    (let ((tools (register-connector-tools)))
      (if (null tools)
          (format nil "The Covision connector at ~a answers no tools: the grant may not include one yet."
                  (endpoint))
          (with-output-to-string (out)
            (format out "The Covision connector at ~a answers ~d tool~:p:" (endpoint) (length tools))
            (dolist (tool tools)
              (destructuring-bind (name description schema) tool
                (format out "~% - ~a~@[ - ~a~]" name (and description (one-line description 160)))
                (let ((names (argument-names schema)))
                  (when names
                    (format out "~%   arguments: ~{~a~^, ~}" names))))))))))

(defparameter *tools-schema*
  (list :description "List the read-only Covision tools the signed-in user's connector approves (tasks, meetings, published knowledge), each with the arguments it takes. Call it before covision_call when you do not already know the tool names."
        :parameters (nlk:json-object "type" "object"))
  "The covision_tools tool.")

(defparameter *call-schema*
  (list :description "Run one of the Covision connector's read-only tools by name, with its arguments as an object (covision_tools names them). Returns the record text the connector answers: tasks, meetings and briefs, or published Covision knowledge the signed-in user may see."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:make-json-object
                                   "tool" (nlk:json-object "type" "string"
                                                            "description" "The connector's tool name, as covision_tools lists it.")
                                   "arguments" (nlk:json-object "type" "object"
                                                                 "description" "That tool's arguments; an empty object when it takes none."))
                     "required" (vector "tool")))
  "The covision_call tool.")

(defun tool-call (arguments)
  "covision_call: one named tool on the connector."
  (answering
    (let ((name (argument-text arguments "tool")))
      (call-text name (and (hash-table-p arguments) (gethash "arguments" arguments))))))

;;; --- the command, the manual, the standing skill --------------------------------

(defun status-line ()
  (format nil "Covision: ~a, ~:[no token in the covision section~;signed in~]. ~d tool~:p listed~@[, the first: ~{~a~^, ~}~]."
          (endpoint) (plusp (length (token))) (length *connector-tools*)
          (and *connector-tools* (mapcar #'first (subseq *connector-tools* 0 (min 4 (length *connector-tools*)))))))

(defun describe-tool (name)
  "NAME's entry on the last listing, one tool per line, or a refusal naming it."
  (let ((entry (assoc name (register-connector-tools) :test #'string=)))
    (if (null entry)
        (format nil "The Covision connector lists no tool named ~s." name)
        (destructuring-bind (listed description schema) entry
          (format nil "~a~@[ - ~a~]~@[~% arguments: ~{~a~^, ~}~]"
                  listed (and description (one-line description 400))
                  (argument-names schema))))))

(defun run-command (args session-id)
  "One /covision invocation: no argument answers the connection, a tool name
describes that tool."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (plusp (length argument))
        (describe-tool argument)
        (progn (unless *connector-tools* (register-connector-tools))
               (status-line)))))

(defparameter +primer+
  "Covision holds a client-services team's assigned tasks, accessible meetings and briefs, and
published operating knowledge; the connector is read-only, and answers only what the signed-in
user's Covision permissions allow.
  covision_tools {}                       the tools the connector approves, with their arguments
  covision_call {tool, arguments}          one of them: tasks, meetings, briefs, knowledge
List the tools first unless you already know their names, then call the one that answers.
State when access is unavailable instead of guessing, and do not claim a record was created,
changed, assigned or sent: nothing here writes. Cite the returned record's title, date or URL,
separate what the source says from what you recommend, and ask a focused follow-up when the
user has named no meeting, organization or time range and the results do not settle it."
  "What (help :covision) answers while the cell runs.")

(defparameter *skill*
  "Covision is a client-services team's own record: assigned tasks, meetings and briefs the signed-in user may access, and published operating knowledge. Reach it with covision_tools (what the connector approves) and covision_call (run one, by name). The grant is read-only: never imply a task, meeting or other record was created, changed, assigned or sent. Treat what it returns as confidential to the user and their organization, cite the record's title, date or URL, keep source facts apart from your recommendations, and say so plainly when access is unavailable rather than guessing."
  "The original's skill, as the standing section every request carries.")

(defun standing-skill (next session-id)
  "Always supply the rules in protected standing context, never the ranked index."
  (acons "covision" *skill*
         (remove "covision" (funcall next session-id) :key #'car :test #'equal)))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell covision
  (:section ("covision")
    (:guide "the connector needs a Covision OAuth access token: paste it as access_token, or name the variable holding it in access_token_env; a token carries the scopes tasks:read, meetings:read and knowledge:read; endpoint is Covision's production connector unless your organization gave another; next step: /covision lists the tools the token may call")
    (:one-of "access_token" "access_token_env")
    ("endpoint" :string :default "https://connector.covisionperformance.com/mcp"
     :doc "the Covision MCP endpoint: the production connector unless your organization gave another")
    ("access_token" :secret
     :doc "the OAuth access token Covision issued, sent as the connector's Bearer token")
    ("access_token_env" :env
     :doc "the environment variable holding that access token")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one request's ceiling"))
  (:help :covision "covision: read-only Covision tasks, meetings and published knowledge through the vendor's connector" +primer+)
  (:command "covision" 'run-command
            :description "Show the Covision connection, or describe one of its tools"
            :argument-hint "TOOL")
  (:tool "covision_tools" *tools-schema* 'tool-tools)
  (:tool "covision_call" *call-schema* 'tool-call)
  (:hook 'nle::read-harness-sections #'standing-skill)
  (:start (lambda ()
            (check-config)
            (setf *session-id* nil
                  *initialized-p* nil
                  *request-id* 0
                  *connector-tools* '())
            (nle:on-stop (lambda ()
                           (unregister-connector-tools)
                           (setf *session-id* nil
                                 *initialized-p* nil
                                 *connector-tools* '())))
            ;; The listing is best effort: a connector that is away, or a token
            ;; the operator has not pasted yet, leaves the two tools above, and
            ;; covision_tools lists again when it can.
            (handler-case (register-connector-tools)
              ((or error sb-sys:deadline-timeout) (condition)
                (nle:notice (format nil "covision: ~a" condition) :level :warning
                            :key "nodecode-covision"))))))
