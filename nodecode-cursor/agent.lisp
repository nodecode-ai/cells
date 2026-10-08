;;;; agent.lisp --- the agent.v1 messages this client writes and reads, and its answers to the server's asks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): the message shapes of ai/src/
;;;; providers/cursor/proto/agent.proto and catalog/src/discovery/cursor-
;;;; models.proto, in the field order omp's generated codecs (catalog/src/
;;;; discovery/cursor-proto.ts) write them; the exec answers of ai/src/
;;;; providers/cursor.ts (handleExecServerMessage with no local handlers and
;;;; an external tool executor, which is what this client is), cursor/exec-
;;;; modern.ts (buildMcpStateResult, buildNeutralHookResult, the pi_* error
;;;; builders), cursor/interaction-query.ts and cursor-external-tool-
;;;; handoff.md. Pure transforms, no I/O.
;;;;
;;;; Only the messages a run uses are written out, field by field, with the
;;;; numbers agent.proto gives them. Every writer answers the message's
;;;; octets; every reader takes PB-DECODE's field list.
;;;;
;;;; The service runs the agent loop and asks the client, over the exec
;;;; channel, to run tools. This client runs none of them during a round:
;;;; Nodecode's own turn loop runs its tools, between rounds. So an MCP call
;;;; (one of Nodecode's tools, advertised as an MCP tool) is answered with
;;;; omp's handoff text, which tells the model to end the turn, and becomes a
;;;; tool call of the round's message; the next round carries its result in
;;;; the history. Every native exec ask (shell, read, write, grep, ls,
;;;; delete, diagnostics, the pi_* family, ...) is refused in its own typed
;;;; result, as omp refuses it when no local handler is installed.

(in-package #:nodecode-cursor)

(defparameter +handoff+
  (string-trim '(#\Space #\Newline #\Return #\Tab)
               (uiop:read-file-string (asdf:system-relative-pathname "nodecode-cursor" "external-tool-handoff.md")))
  "What an MCP call handed to the turn loop is answered with (cursor-external-
tool-handoff.md): the model ends its turn and finds the result in the next request.")

(defparameter +provider-identifier+ "pi-agent"
  "The MCP server name the advertised tools ride under, omp's.")

(defparameter +not-implemented-suffix+ "not implemented by this client")

(defparameter +not-implemented+ "Not implemented by this client")

(defparameter +tool-not-available+ "Tool not available"
  "resolveExecHandler's refusal when no local handler is installed.")

;;; --- the client's envelope -------------------------------------------------------

(defun client-message (field octets)
  "An AgentClientMessage carrying OCTETS as member FIELD: 1 runRequest,
2 execClientMessage, 5 execClientControlMessage, 3 kvClientMessage,
6 interactionResponse, 7 clientHeartbeat."
  (pb-message field octets))

(defun heartbeat-message ()
  "A ClientHeartbeat, sent every five seconds while a run is open."
  (client-message 7 (pb)))

(defun bidi-request-id (request-id)
  "A BidiRequestId naming the run."
  (pb (pb-string 1 request-id)))

(defun bidi-append-request (request-id seqno data)
  "One BidiAppend: the run's id, the append's sequence number, and DATA, an
AgentClientMessage."
  (pb (pb-message 2 (bidi-request-id request-id))
      (pb-int 3 seqno)
      (pb-bytes 4 data)))

;;; --- the run request ---------------------------------------------------------------

(defun model-details (details-id display-id display-name max-mode)
  "ModelDetails: the account-usable slug, the picker's id and name, max mode
only when on."
  (pb (pb-string 1 details-id)
      (pb-string 3 display-id)
      (pb-string 4 display-name)
      (pb-bool* 7 (and max-mode t))))

(defun requested-model (model-id max-mode parameters)
  "RequestedModel: the base id, max mode, and PARAMETERS ((ID . VALUE) ...)."
  (pb (pb-string 1 model-id)
      (pb-bool 2 max-mode)
      (pb-messages 3 (mapcar (lambda (parameter)
                               (pb (pb-string 1 (car parameter)) (pb-string 2 (cdr parameter))))
                             parameters))))

(defun selected-image (mime-type data)
  "A SelectedImage carrying DATA inline."
  (pb (pb-string 2 (uuid))
      (pb-string 7 mime-type)
      (pb-bytes* 8 data)))

(defun user-message (text message-id images)
  "A UserMessage; IMAGES, ((MIME . OCTETS) ...), ride as its selected context."
  (pb (pb-string 1 text)
      (pb-string 2 message-id)
      (when images
        (pb-message 3 (pb (pb-messages 1 (mapcar (lambda (image) (selected-image (car image) (cdr image)))
                                                 images)))))))

(defun conversation-action (user-message)
  "A ConversationAction: the user's message when there is one, else a resume."
  (if user-message
      (pb (pb-message 1 (pb (pb-message 1 user-message))))
      (pb (pb-message 2 (pb)))))

(defparameter +state-field-order+ '(2 1 8 3 4 5 6 7 9 10 11 12 15 13 14 16 17 18)
  "ConversationStateStructure's fields in the order its codec writes them.")

(defun conversation-state (root-ids turn-ids &optional cached)
  "A ConversationStateStructure whose prompt (root_prompt_messages_json) and
turns are ROOT-IDS and TURN-IDS, blob ids; CACHED, a checkpoint's field list,
lends every other field it holds (todos, file states, summaries)."
  (let ((rest (remove-if (lambda (field) (member (first field) '(1 8))) cached)))
    (pb (loop for number in +state-field-order+
              collect (case number
                        (1 (mapcar (lambda (id) (len-field 1 id)) root-ids))
                        (8 (mapcar (lambda (id) (len-field 8 id)) turn-ids))
                        (t (loop for field in rest
                                 when (= (first field) number) collect (pb-raw field)))))
        ;; fields this client does not name ride last, as a codec keeps them
        (loop for field in rest
              unless (member (first field) +state-field-order+) collect (pb-raw field)))))

(defun run-request (state action details requested conversation-id)
  "The AgentClientMessage that opens a run."
  (client-message 1 (pb (pb-message 1 state)
                        (pb-message 2 action)
                        (pb-message 3 details)
                        (pb-message 9 requested)
                        (pb-string* 5 conversation-id))))

;;; --- the history's own structures -------------------------------------------------

(defun agent-turn (user-blob step-blobs)
  "A ConversationTurnStructure holding one agent turn of blob ids."
  (pb (pb-message 1 (pb (pb-bytes 1 user-blob) (mapcar (lambda (id) (len-field 2 id)) step-blobs)))))

(defun assistant-step (text)
  "A ConversationStep of assistant text."
  (pb (pb-message 1 (pb (pb-string 1 text)))))

(defun thinking-step (text)
  "A ConversationStep of thinking."
  (pb (pb-message 3 (pb (pb-string 1 text)))))

(defun mcp-args (name arguments tool-call-id)
  "McpArgs of one call of the tool NAME: ARGUMENTS, an alist of (KEY . JSON),
each a google.protobuf.Value."
  (pb (pb-string 1 name)
      (pb-bytes-map 2 (mapcar (lambda (entry) (cons (car entry) (json-value-octets (cdr entry))))
                              arguments))
      (pb-string 3 tool-call-id)
      (pb-string 4 +provider-identifier+)
      (pb-string 5 name)))

(defun mcp-text-item (text)
  "An McpToolResultContentItem of text."
  (pb (pb-message 1 (pb (pb-string 1 text)))))

(defun mcp-image-item (mime-type data)
  "An McpToolResultContentItem of an image."
  (pb (pb-message 2 (pb (pb-bytes 1 data) (pb-string 2 mime-type)))))

(defun tool-call-step (tool-call-id name arguments result-items)
  "A ConversationStep of one MCP call; RESULT-ITEMS, its result's content
items, when the history holds the result."
  (pb (pb-message 2 (pb (pb-string* 57 tool-call-id)
                        (pb-message 15 (pb (pb-message 1 (mcp-args name arguments tool-call-id))
                                           (when result-items
                                             (pb-message 2 (pb (pb-message 1 (pb (pb-messages 1 result-items))))))))))))

;;; --- the request context: the rules and the tools ---------------------------------

(defun cursor-rule (path content)
  "A CursorRule that always applies: the system prompt rides as one, since
the service rebuilds the model's prompt from the rules."
  (pb (pb-string 1 path)
      (pb-string 2 content)
      (pb-message 3 (pb (pb-message 1 (pb))))
      (pb-int 4 2)))

(defun mcp-tool-definition (name description schema)
  "An McpToolDefinition of one of Nodecode's tools; SCHEMA its JSON schema."
  (pb (pb-string 1 name)
      (pb-string 4 +provider-identifier+)
      (pb-string 5 name)
      (pb-string 2 description)
      (pb-bytes 3 (json-value-octets schema))))

(defun request-context (rules tools)
  "A RequestContext of RULES and TOOLS, encoded; everything else empty."
  (pb (pb-messages 2 rules) (pb-messages 7 tools)))

;;; --- reading the server -------------------------------------------------------------

(defparameter +server-members+ '(1 2 5 3 4 7)
  "AgentServerMessage's members: 1 interactionUpdate, 2 execServerMessage,
5 execServerControlMessage, 3 conversationCheckpointUpdate, 4 kvServerMessage,
7 interactionQuery.")

(defparameter +update-members+ '(1 7 15 2 3 4 5 6 8 9 10 11 12 13 14 16 17)
  "InteractionUpdate's members: 1 textDelta, 7 partialToolCall, 15
toolCallDelta, 2 toolCallStarted, 3 toolCallCompleted, 4 thinkingDelta, 5
thinkingCompleted, 8 tokenDelta, 13 heartbeat, 14 turnEnded, 17
stepCompleted, and others this client lets pass.")

(defparameter +tool-call-members+
  '(1 3 4 5 8 9 10 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
    61 62 63 64 65 66 67 68 69 37)
  "ToolCall's tool oneof.")

(defparameter +exec-owned-tool-calls+ '(61 62 63 64 65 66 67 20 21)
  "The ToolCall variants whose block the exec channel owns (the pi_* family
and the MCP resource frames).")

(defparameter +server-resolved-tool-calls+ '(9 10 12 24 37 68)
  "The ToolCall variants the service resolves itself: todos, edit, hosted
fetch, connect_scm. omp shows each as a call already paired with its result.")

(defun server-member (octets)
  "(values MEMBER FIELDS) of the AgentServerMessage OCTETS."
  (multiple-value-bind (member value) (pb-oneof (pb-decode octets) +server-members+)
    (values member (and (vectorp value) (pb-decode value)))))

(defun update-member (fields)
  "(values MEMBER FIELDS) of the InteractionUpdate FIELDS."
  (multiple-value-bind (member value) (pb-oneof fields +update-members+)
    (values member (and (vectorp value) (pb-decode value)))))

(defun tool-call-variant (tool-call)
  "(values VARIANT FIELDS) of the ToolCall TOOL-CALL's tool oneof."
  (multiple-value-bind (variant value) (pb-oneof tool-call +tool-call-members+)
    (values variant (and (vectorp value) (pb-decode value)))))

(defun mcp-call-of (tool-call)
  "The McpArgs fields of TOOL-CALL when it is an MCP call, else NIL."
  (multiple-value-bind (variant fields) (tool-call-variant tool-call)
    (and (eql variant 15) (pb-sub fields 1))))

(defun decode-mcp-arg (octets)
  "One MCP argument as JSON (decodeMcpArgValue): a Value, a string that is
itself JSON parsed, anything else the text it spells."
  (flet ((parse (text)
           (let ((trimmed (nlk:trimmed text)))
             (or (and (plusp (length trimmed)) (ignore-errors (nlk:decode-json trimmed)))
                 text))))
    (let ((value (handler-case (octets-json-value octets)
                   (error () (return-from decode-mcp-arg (parse (text-of octets)))))))
      (if (and (stringp value)
               (find (char (string-left-trim '(#\Space #\Tab #\Newline #\Return) (concatenate 'string value " ")) 0)
                     "{[\""))
          (parse value)
          value))))

(defun mcp-call-arguments (args)
  "The McpArgs ARGS's argument map as a JSON object, or NIL when it has none."
  (let ((entries (pb-all args 2)))
    (when entries
      (let ((object (make-hash-table :test #'equal)))
        (dolist (entry entries object)
          (let ((fields (pb-decode entry)))
            (setf (gethash (pb-text* fields 1) object)
                  (decode-mcp-arg (or (pb-get fields 2) (make-array 0 :element-type '(unsigned-byte 8)))))))))))

(defun mcp-call-name (args)
  "The tool an McpArgs names: its tool_name, else its name."
  (let ((tool-name (pb-text* args 5)))
    (if (plusp (length tool-name)) tool-name (pb-text* args 1))))

;;; --- answering the exec channel -----------------------------------------------------

(defun exec-reply (exec member result)
  "The AgentClientMessage answering the ExecServerMessage EXEC with RESULT
as ExecClientMessage member MEMBER."
  (client-message 2 (pb (pb-int 1 (pb-get exec 1))
                        (pb-string 15 (pb-text exec 15))
                        (pb-message member result))))

(defun exec-control (control)
  "The AgentClientMessage carrying the ExecClientControlMessage CONTROL."
  (client-message 5 control))

(defun exec-throw (exec error code)
  "Fail EXEC in band (sendExecClientThrow): a throw, then the stream's close."
  (let ((id (pb-get exec 1)))
    (list (exec-control (pb (pb-message 2 (pb (pb-int 1 id) (pb-string 2 error) (pb-string* 4 code)))))
          (exec-stream-close exec))))

(defun exec-stream-close (exec)
  "Close EXEC's stream."
  (exec-control (pb (pb-message 1 (pb (pb-int 1 (pb-get exec 1)))))))

(defun result (member &rest fields)
  "A result oneof set to MEMBER, a message of FIELDS."
  (pb (pb-message member (apply #'pb fields))))

(defun path-reason (path reason)
  "The {path, reason} (or {path, error}) a refusal names."
  (pb (pb-string 1 path) (pb-string 2 reason)))

(defun shell-rejected (command directory reason)
  "A ShellRejected."
  (pb (pb-string 1 command) (pb-string 2 directory) (pb-string 3 reason)))

(defun empty-grep-pattern-rejection (pattern glob)
  "The refusal of a grep with an empty pattern, or NIL when it has one."
  (unless (and pattern (plusp (length (nlk:trimmed pattern))))
    (if (and glob (plusp (length glob)))
        (format nil "grep pattern is required (received an empty pattern). To list files matching \"~a\", pass a non-empty regex (e.g. \".\") and set path to that glob, or use the ls/read tool instead." glob)
        "grep pattern is required (received an empty pattern).")))

(defun shell-stream-refusal (exec args directory)
  "What a refused streaming shell answers (handleShellStreamArgs with no
handler): start, the refusal, the exit, the shellResult, the close."
  (let ((rejected (shell-rejected (pb-text* args 1) directory +tool-not-available+)))
    (list (exec-reply exec 14 (result 4 (pb)))
          (exec-reply exec 14 (pb (pb-message 5 rejected)))
          (exec-reply exec 14 (result 3 (pb-int 1 1) (pb-string 2 directory)))
          (exec-reply exec 2 (pb (pb-message 4 rejected)))
          (exec-stream-close exec))))

(defun mcp-state-result (tools identifiers)
  "McpStateExecResult from the advertised TOOLS ((NAME . DEFINITION) ...),
regrouped under their one server (buildMcpStateResult)."
  (let ((wanted (and identifiers (member +provider-identifier+ identifiers :test #'equal))))
    (result 1 (when (and tools (or (null identifiers) wanted))
                (pb-message 1 (pb (pb-string 1 +provider-identifier+)
                                  (pb-string 2 +provider-identifier+)
                                  (pb-messages 5 (mapcar #'cdr tools))
                                  (pb-string* 7 "connected")))))))

(defparameter +exec-members+
  '(2 3 4 5 7 8 9 10 11 14 16 17 18 20 21 22 23 29 36 27 28 30 31 37 38 40 41 42 43 44
    45 46 47 48 49 50 51 52 53 54)
  "ExecServerMessage's members.")

(defun exec-member (exec)
  "(values MEMBER ARGS) of the ExecServerMessage EXEC."
  (multiple-value-bind (member value) (pb-oneof exec +exec-members+)
    (values member (and (vectorp value) (pb-decode value)))))

(defun exec-answer (exec &key rules tools (directory "") handoff)
  "The AgentClientMessages answering the ExecServerMessage EXEC, in order.
RULES and TOOLS are the request context's ((NAME . DEFINITION) for a tool),
DIRECTORY the working directory a shell refusal names. HANDOFF, called with
(TOOL-CALL-ID NAME ARGUMENTS) for an MCP call, files the call for the turn
loop; it answers whether the call is a tool this client advertises."
  (multiple-value-bind (member args) (exec-member exec)
    (flet ((reply (field octets) (list (exec-reply exec field octets)))
           (refuse-path (field variant) (list (exec-reply exec field (result variant (path-reason (pb-text* args 1) +tool-not-available+))))))
      (case member
        ((nil) (exec-throw exec "Unknown exec message variant" "unknown_exec_variant"))
        (10 (reply 10 (result 1 (pb-message 1 (request-context rules (mapcar #'cdr tools))))))
        (7 (refuse-path 7 3))           ; read: rejected
        (8 (refuse-path 8 3))           ; ls: rejected
        (5 (reply 5 (result 2 (pb-string 1 (or (empty-grep-pattern-rejection (pb-text args 1) (pb-text args 3))
                                                +tool-not-available+)))))
        (3 (refuse-path 3 6))           ; write: rejected
        (4 (refuse-path 4 6))           ; delete: rejected
        (2 (reply 2 (pb (pb-message 4 (shell-rejected (pb-text* args 1)
                                                      (let ((dir (pb-text* args 2))) (if (plusp (length dir)) dir directory))
                                                      +tool-not-available+)))))
        (14 (shell-stream-refusal exec args (let ((dir (pb-text* args 2))) (if (plusp (length dir)) dir directory))))
        (16 (reply 16 (pb (pb-message 3 (shell-rejected (pb-text* args 1) (pb-text* args 2) "Not implemented")))))
        (23 (reply 23 (result 2 (pb-string 1 "Not implemented"))))
        (20 (reply 20 (result 2 (pb-string 1 (pb-text* args 1)) (pb-string 2 "Not implemented"))))
        (9 (refuse-path 9 3))           ; diagnostics: rejected
        (11 (let* ((call-id (pb-text* args 3))
                   (name (mcp-call-name args)))
              (if (pb-get args 7)
                  ;; an approval probe, not a call: nothing here can approve
                  (reply 11 (result 3 (pb-string 1 (format nil "Tool \"~a\" is not approved to run without asking." name))))
                  (progn
                    (when handoff
                      (funcall handoff call-id name (or (mcp-call-arguments args) (make-hash-table :test #'equal))))
                    (reply 11 (result 1 (pb-message 1 (mcp-text-item +handoff+))))))))
        (17 (reply 17 (result 1)))      ; no MCP servers of its own: an empty listing
        (18 (reply 18 (result 4 (pb-string 1 (pb-text* args 2)))))
        (21 (reply 21 (result 4 (pb-string 1 +not-implemented+))))
        (22 (reply 22 (result 2 (pb-string 1 +not-implemented+))))
        ((45 46 49 50 51)               ; pi_read, pi_bash, pi_grep, pi_find, pi_ls: error
         (reply (1+ member) (result 2 (pb-string 1 +tool-not-available+))))
        ((47 48)                        ; pi_edit, pi_write: rejected
         (reply (1+ member) (result 3 (pb-string 1 +tool-not-available+))))
        (52 (reply 55 (pb (pb-message 4 (shell-rejected (pb-text* args 1)
                                                        (let ((dir (pb-text* args 2))) (if (plusp (length dir)) dir directory))
                                                        +tool-not-available+)))))
        (29 (reply 29 (result 2 (path-reason (pb-text* args 1) "Secret redaction is not implemented by this client"))))
        (36 (reply 36 (mcp-state-result tools (mapcar #'text-of (pb-all args 1)))))
        (27 (let ((case* (pb-oneof (pb-sub args 1) '(1 2 3 4 5 6 7 8 9 11))))
              (if case*
                  (reply 27 (pb (pb-message 1 (pb (pb-message case* (pb))))))
                  (exec-throw exec (format nil "Unsupported hook request: ~a" "unset") "unknown_hook_request"))))
        (28 (reply 28 (result 2 (pb-string 2 (format nil "Subagents are ~a" +not-implemented-suffix+)))))
        (37 (reply 37 (result 3 (pb-string 1 (pb-text* args 1)))))
        (30 (reply 30 (pb (pb-int 1 2))))   ; NOT_FOUND
        (31 (reply 31 (pb (pb-int 1 2))))   ; NOT_FOUND
        (38 (reply 38 (result 2 (pb-string 1 (format nil "Smart-mode classification is ~a" +not-implemented-suffix+)))))
        (40 (reply 40 (result 2 (pb-string 1 (pb-text* args 1))
                              (pb-string 2 (format nil "Canvas diagnostics are ~a" +not-implemented-suffix+)))))
        ((41 42 43) (reply member (pb)))    ; allowlisted: false
        (53 (reply 53 (result 2 (pb-string 1 (format nil "Conversation search is ~a" +not-implemented-suffix+)))))
        (54 (reply 54 (result 2 (pb-string 1 (format nil "Agent store conflicts are ~a" +not-implemented-suffix+)))))
        (44 (exec-throw exec (format nil "Git diff is ~a" +not-implemented-suffix+) "exec_variant_unsupported"))
        (t (exec-throw exec (format nil "No handler for exec message of type ~a" member) "exec_variant_unsupported"))))))

;;; --- answering an interaction query --------------------------------------------------

(defun interaction-answer (query)
  "The AgentClientMessage answering the InteractionQuery QUERY, or NIL when
it is left unanswered (handleInteractionQuery): hosted web search, Exa and
web fetch are approved, a question, a mode switch and a plan refused, VM
setup left alone."
  (let ((id (pb-get query 1)))
    (flet ((respond (member octets)
             (client-message 6 (pb (pb-int 1 id) (pb-message member octets))))
           (refusal (text) (pb (pb-message 2 (pb (pb-string 1 text))))))
      (multiple-value-bind (member) (pb-oneof query '(2 3 4 5 6 7 8 9))
        (case member
          ((2 5 6 9) (respond member (result 1)))
          (3 (respond 3 (pb (pb-message 1 (pb (pb-message 3 (pb (pb-string 1 (format nil "Interactive questions are ~a" +not-implemented-suffix+)))))))))
          (4 (respond 4 (refusal (format nil "Mode switches are ~a" +not-implemented-suffix+))))
          (7 (respond 7 (pb (pb-message 1 (pb (pb-message 2 (pb (pb-string 1 (format nil "Plan files are ~a" +not-implemented-suffix+)))))))))
          (8 nil)
          (t
           ;; a query this build does not model: an `approved {}' on the
           ;; matching response member, when it carries one at all
           (let ((unknown (find-if (lambda (field) (and (= 2 (second field)) (>= (first field) 2)))
                                   query)))
             (and unknown (respond (first unknown) (result 1))))))))))

;;; --- the key-value store ---------------------------------------------------------------

(defun kv-answer (kv blobs)
  "The AgentClientMessage answering the KvServerMessage KV against the blob
store BLOBS (hex id -> octets): a read answers the blob when it is held, a
write keeps it."
  (let ((id (pb-get kv 1)))
    (multiple-value-bind (member value) (pb-oneof kv '(2 3))
      (let ((args (and (vectorp value) (pb-decode value))))
        (case member
          (2 (let ((data (gethash (hex (or (pb-get args 1) #())) blobs)))
               (client-message 3 (pb (pb-int 1 id) (pb-message 2 (pb (pb-bytes* 1 data)))))))
          (3 (setf (gethash (hex (or (pb-get args 1) #())) blobs)
                   (or (pb-get args 2) (make-array 0 :element-type '(unsigned-byte 8))))
             (client-message 3 (pb (pb-int 1 id) (pb-message 3 (pb))))))))))

(defun store-blob (blobs octets)
  "Keep OCTETS in BLOBS under its SHA-256: the blob id the request names."
  (let ((id (sha256-octets octets)))
    (setf (gethash (hex id) blobs) octets)
    id))

;;; --- the model roster ---------------------------------------------------------------------

(defun usable-models (octets)
  "The ModelDetails a GetUsableModelsResponse OCTETS lists, each a plist
(:ID :NAME :MAX-MODE :ALIASES :THINKING)."
  (loop for model in (pb-all (pb-decode octets) 1)
        for fields = (pb-decode model)
        for id = (nlk:trimmed (pb-text* fields 1))
        when (plusp (length id))
          collect (list :id id
                        :name (let ((names (append (list (pb-text fields 4) (pb-text fields 5) (pb-text fields 3))
                                                   (mapcar #'text-of (pb-all fields 6)))))
                                (nlk:trimmed (or (find-if (lambda (name) (and name (plusp (length (nlk:trimmed name))))) names)
                                                 id)))
                        :names (remove nil (append (list (pb-text fields 4) (pb-text fields 5) (pb-text fields 3))
                                                   (mapcar #'text-of (pb-all fields 6))))
                        :max-mode (let ((value (pb-get fields 7))) (and value (/= value 0)))
                        :thinking (pb-has fields 2))))

;;; --- a Connect error -----------------------------------------------------------------

(defparameter +cursor-errors+
  '((0 "UNSPECIFIED") (1 "BAD_API_KEY" 401) (2 "NOT_LOGGED_IN" 401) (3 "INVALID_AUTH_ID" 401)
    (4 "NOT_HIGH_ENOUGH_PERMISSIONS" 403) (5 "BAD_MODEL_NAME" 404) (6 "USER_NOT_FOUND" 404)
    (7 "FREE_USER_RATE_LIMIT_EXCEEDED" 429) (8 "PRO_USER_RATE_LIMIT_EXCEEDED" 429)
    (9 "FREE_USER_USAGE_LIMIT" 429) (10 "PRO_USER_USAGE_LIMIT" 429) (11 "AUTH_TOKEN_NOT_FOUND" 401)
    (12 "AUTH_TOKEN_EXPIRED" 401) (13 "OPENAI" nil t) (14 "OPENAI_RATE_LIMIT_EXCEEDED" 429)
    (18 "AGENT_REQUIRES_LOGIN" 401) (20 "MAX_TOKENS" 413) (21 "USER_ABORTED_REQUEST")
    (22 "GENERIC_RATE_LIMIT_EXCEEDED" 429) (23 "PRO_USER_ONLY" 403) (25 "TIMEOUT" nil t)
    (28 "GPT_4_VISION_PREVIEW_RATE_LIMIT" 429) (29 "CUSTOM_MESSAGE") (30 "OUTDATED_CLIENT")
    (31 "CLAUDE_IMAGE_TOO_LARGE") (33 "FILE_NOT_FOUND" 404) (34 "API_KEY_RATE_LIMIT" 429)
    (35 "DEBOUNCED" 429) (36 "BAD_REQUEST") (37 "REPOSITORY_SERVICE_REPOSITORY_IS_NOT_INITIALIZED")
    (38 "UNAUTHORIZED" 401) (39 "NOT_FOUND" 404) (40 "DEPRECATED") (41 "RESOURCE_EXHAUSTED" 429)
    (42 "BAD_USER_API_KEY" 401) (43 "CONVERSATION_TOO_LONG" 413) (44 "USAGE_PRICING_REQUIRED" 429)
    (45 "USAGE_PRICING_REQUIRED_CHANGEABLE" 429) (46 "GITHUB_NO_USER_CREDENTIALS" 403)
    (47 "GITHUB_USER_NO_ACCESS" 403) (48 "GITHUB_APP_NO_ACCESS" 403) (49 "GITHUB_MULTIPLE_OWNERS" 403)
    (50 "RATE_LIMITED" 429) (51 "RATE_LIMITED_CHANGEABLE" 429) (52 "CUSTOM") (53 "HOOKS_BLOCKED")
    (54 "SUSPICIOUS_USAGE_BLOCKED" 403) (55 "EXTENSION_HOST_TIMEOUT" nil t) (56 "NETWORK_ERROR" nil t)
    (57 "PROVIDER_ERROR" nil t) (58 "MODEL_BLOCKED" 403) (59 "INTERNAL" nil t)
    (60 "MAX_MODE_REQUIRED") (61 "MODEL_NO_LONGER_SUPPORTED" 404) (62 "PRICING_WARNING")
    (63 "SLOW_POOL" nil t) (64 "UNSUPPORTED_REGION" 403) (65 "ACCOUNT_CLOSED" 401))
  "aiserver.v1.ErrorDetails.CursorError: (CODE NAME STATUS RETRYABLE), the
HTTP status omp classifies each as, and the codes it retries.")

(defparameter +connect-statuses+
  '(("canceled" . 499) ("unknown" . 500) ("invalid_argument" . 400) ("deadline_exceeded" . 504)
    ("not_found" . 404) ("already_exists" . 409) ("permission_denied" . 403)
    ("resource_exhausted" . 429) ("failed_precondition" . 400) ("aborted" . 409)
    ("out_of_range" . 400) ("unimplemented" . 501) ("internal" . 500) ("unavailable" . 503)
    ("data_loss" . 500) ("unauthenticated" . 401))
  "The Connect protocol's own mapping of its codes onto HTTP statuses.")

(defun structured-error (error)
  "(values CODE NAME MESSAGE RETRYABLE) of the aiserver.v1.ErrorDetails among
the Connect ERROR object's details, or NIL (decodeCursorStructuredError)."
  (loop for entry across (or (nlk:json-value error :array "details") #())
        for type = (nlk:json-value entry :string "type")
        for encoded = (nlk:json-value entry :string "value")
        when (and (member type '("aiserver.v1.ErrorDetails" "type.googleapis.com/aiserver.v1.ErrorDetails")
                          :test #'equal)
                  encoded (plusp (length encoded)))
          do (let ((detail (ignore-errors (pb-decode (unbase64 encoded)))))
               (when detail
                 (let* ((code (or (pb-get detail 1) 0))
                        (name (or (second (assoc code +cursor-errors+)) (format nil "ERROR_~d" code)))
                        (custom (pb-sub detail 2))
                        (title (nlk:trimmed (pb-text* custom 1)))
                        (body (nlk:trimmed (pb-text* custom 2)))
                        (message (format nil "~{~a~^: ~}" (remove "" (list title body) :test #'equal))))
                   (return (values code name (if (plusp (length message)) message name)
                                   (let ((flag (pb-get custom 4))) (and flag (if (/= flag 0) :true :false))))))))))

(defun summarize-details (details)
  "The Connect error DETAILS as one line (summarizeConnectErrorDetails), or NIL."
  (let ((parts (loop for entry across (or details #())
                     for type = (let ((type (nlk:json-value entry :string "type")))
                                  (and type (plusp (length type)) type))
                     for debug = (multiple-value-bind (value present) (gethash "debug" entry)
                                   (and present (nlk:encode-json-object value)))
                     for value = (multiple-value-bind (value present) (gethash "value" entry)
                                   (and present (if (stringp value) value (nlk:encode-json-object value))))
                     for diagnostic = (or debug value)
                     when (and (hash-table-p entry) (or type diagnostic))
                       collect (cond ((and type diagnostic) (format nil "~a: ~a" type diagnostic))
                                     (type type)
                                     (t diagnostic)))))
    (and parts (nlk:clip (format nil "~{~a~^; ~}" parts) 400))))

(defun retryable-detail-p (details)
  "Whether a Cursor ErrorDetails entry's debug form says the failure is
retryable (hasRetryableCursorErrorDetail)."
  (loop for entry across (or details #())
        thereis (and (hash-table-p entry)
                     (equal "aiserver.v1.ErrorDetails" (nlk:json-value entry :string "type"))
                     (eq t (nlk:json-value entry :boolean "debug" "details" "isRetryable")))))

(defun connect-error-text (error)
  "The Connect ERROR object as a diagnosable line (formatConnectEndStreamError)."
  (let* ((code (let ((code (nlk:json-value error :string "code"))) (if (and code (plusp (length code))) code "unknown")))
         (message (or (nlk:json-value error :string "message") ""))
         (detail (summarize-details (nlk:json-value error :array "details"))))
    (format nil "Connect error ~a: ~a~@[ ~a~]" code (if (plusp (length message)) message "Unknown error")
            (cond (detail (format nil "[details: ~a]" detail))
                  ((member (string-downcase (nlk:trimmed message))
                           '("" "error" "unknown" "unknown error" "internal" "internal error") :test #'equal)
                   (let ((extras (make-hash-table :test #'equal)))
                     (maphash (lambda (key value) (unless (member key '("code" "message") :test #'equal)
                                                    (setf (gethash key extras) value)))
                              error)
                     (and (plusp (hash-table-count extras))
                          (format nil "[trailer: ~a]" (nlk:clip (nlk:encode-json-object extras) 400)))))))))

(defun connect-error (error &key (scope :stream))
  "The NLE::PROVIDER-ERROR a Connect ERROR object is (classifyConnectError):
Cursor's own error detail by its code, else the Connect code."
  (multiple-value-bind (code name message retryable) (structured-error error)
    (if code
        (let* ((row (assoc code +cursor-errors+))
               (status (cond ((= code 21) 499)
                             ((third row))
                             ((or (eq retryable :true) (and (null retryable) (fourth row))) 503)
                             (t 400))))
          (make-condition 'nle::provider-error :status status :scope scope
                                               :detail (format nil "Cursor ~a: ~a" name message)))
        (let ((code (nlk:json-value error :string "code")))
          (make-condition 'nle::provider-error
                          :status (if (retryable-detail-p (nlk:json-value error :array "details"))
                                      503
                                      (cdr (assoc code +connect-statuses+ :test #'equal)))
                          :scope scope
                          :detail (connect-error-text error))))))

(defun end-stream-error (payload)
  "The error the end-of-stream trailer PAYLOAD carries (parseConnectEndStream),
or NIL for a clean end."
  (handler-case
      (let ((error (nlk:json-value (nlk:decode-json (text-of payload)) :object "error")))
        (and error (connect-error error)))
    (error ()
      (make-condition 'nle::provider-error :scope :stream :detail "Failed to parse Connect end stream"))))
