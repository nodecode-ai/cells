;;;; wire.lisp --- a Cursor run: the request from the organism's history, the transport, the stream folded.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): ai/src/providers/cursor.ts --
;;;; buildGrpcRequestForWireMode, buildCursorConversationState,
;;;; buildConversationTurns, buildRootPromptMessagesJson and their helpers
;;;; (the request); createHttp1RunTransport (RunSSE and BidiAppend, the
;;;; HTTP/1.1 transport); streamCursorWithWireMode, handleServerMessage and
;;;; processInteractionUpdate (the stream); buildMcpToolDefinitions and
;;;; buildCursorRequestContextRules (what the service is told it may call);
;;;; and a compact form of ai/src/utils/schema/normalize.ts's
;;;; sanitizeSchemaForCursor.
;;;;
;;;; A run, over HTTP/1.1, is two kinds of call against one request id:
;;;;
;;;;   POST /agent.v1.AgentService/RunSSE         body: one Connect frame of
;;;;        the BidiRequestId; answer: the run's AgentServerMessages, each a
;;;;        Connect frame, then the end-of-stream trailer
;;;;   POST /aiserver.v1.BidiService/BidiAppend   one per client message, in
;;;;        sequence: the run request first, then every answer to the
;;;;        server's asks and a heartbeat every five seconds
;;;;
;;;; The appends ride a thread of their own while the round's thread reads
;;;; the stream, since the service answers RunSSE only as the appends arrive.
;;;;
;;;; The request carries the whole conversation each time: the system prompt
;;;; and the history as JSON blobs (root_prompt_messages_json, which the model
;;;; reads) and as turn structures (which the service keeps), each referenced
;;;; by its SHA-256 and served from this process's blob store when the
;;;; service asks for it over the key-value channel. A checkpoint the service
;;;; sent lends the next request the state that is not history (todos, file
;;;; states, summaries) while the system prompt is unchanged.

(in-package #:nodecode-cursor)

;;; --- conversations this process keeps --------------------------------------------
;;; Per conversation: the last checkpoint (its field list) and the blob store
;;; its blob ids point into, evicted together, at most 128 of them. An
;;; evicted conversation rebuilds from the history like the first request
;;; after a restart.

(defstruct (conversation (:conc-name conv-) (:copier nil))
  (state nil)
  (blobs (make-hash-table :test #'equal :synchronized t)))

(defvar *conversations* (make-hash-table :test #'equal :synchronized t)
  "Conversation id -> its CONVERSATION.")

(defvar *conversation-order* '()
  "Conversation ids, most recently used first.")

(defvar *conversation-lock* (bt2:make-lock :name "cursor conversations"))

(defparameter +conversations-limit+ 128)

(defun conversation (id)
  "The CONVERSATION kept under ID, made the first time, and now the most recent."
  (bt2:with-lock-held (*conversation-lock*)
    (setf *conversation-order* (cons id (remove id *conversation-order* :test #'equal)))
    (loop while (> (length *conversation-order*) +conversations-limit+)
          do (remhash (car (last *conversation-order*)) *conversations*)
             (setf *conversation-order* (butlast *conversation-order*)))
    (or (gethash id *conversations*)
        (setf (gethash id *conversations*) (make-conversation)))))

(defun forget-conversation (id)
  "Drop what is kept of conversation ID."
  (bt2:with-lock-held (*conversation-lock*)
    (remhash id *conversations*)
    (setf *conversation-order* (remove id *conversation-order* :test #'equal))))

;;; A conversation the service poisoned (a bare resource_exhausted with no
;;; output, #8345 in omp) is given a new wire id: the next attempt rebuilds
;;; it fresh from the history. One rotation per failure streak; a rotated id
;;; that completed a turn may rotate again.

(defvar *rotated* (make-hash-table :test #'equal :synchronized t)
  "Base conversation id -> the wire id it was rotated to.")

(defvar *rotated-good* (make-hash-table :test #'equal :synchronized t)
  "Rotated ids that completed a turn.")

(defvar *rotated-fresh* (make-hash-table :test #'equal :synchronized t)
  "Rotated ids not yet used: they rebuild from the history alone.")

(defun forget-conversations ()
  "Drop every kept conversation and rotation."
  (bt2:with-lock-held (*conversation-lock*)
    (clrhash *conversations*)
    (setf *conversation-order* '()))
  (clrhash *rotated*)
  (clrhash *rotated-good*)
  (clrhash *rotated-fresh*))

;;; --- the organism's history, as omp's messages ------------------------------------

(defun message-role (message)
  "MESSAGE's role, `user' standing for the developer and system roles that
ride as user turns."
  (let ((role (nlk:json-value message :string "role")))
    (if (member role '("user" "developer" "system") :test #'equal) "user" role)))

(defun data-url-image (url)
  "(values MIME BASE64) of a data: URL, or NIL for any other address."
  (ppcre:register-groups-bind (mime data) ("(?s)^data:([^;,]+);base64,(.*)$" (or url ""))
    (values mime data)))

(defun content-items (content)
  "CONTENT as omp's content: a string stays one; parts become a list of
(:TEXT TEXT) and (:IMAGE MIME BASE64)."
  (if (stringp content)
      content
      (loop for part in (nle::message-content-parts content)
            for type = (nle::content-part-type part)
            if (equal type "text")
              collect (list :text (or (nlk:json-value part :string "text") ""))
            else if (equal type "image_url")
                   append (multiple-value-bind (mime data)
                              (data-url-image (nlk:json-value part :string "image_url" "url"))
                            (and mime (list (list :image mime data)))))))

(defun items-text (items &key (images nil))
  "The text of ITEMS joined by newlines; an image as `[MIME image]' when IMAGES."
  (if (stringp items)
      items
      (format nil "~{~a~^~%~}"
              (loop for item in items
                    if (eq (first item) :text) collect (second item)
                    else if images collect (format nil "[~a image]" (second item))))))

(defun items-images (items)
  "The images of ITEMS: ((MIME . BASE64) ...)."
  (and (listp items)
       (loop for item in items when (eq (first item) :image) collect (cons (second item) (third item)))))

(defun user-text (items)
  "extractUserMessageText: the text, trimmed."
  (nlk:trimmed (items-text items)))

(defun content-key (items)
  "cursorUserContentKey: the trimmed text of a string, the SHA-256 of a part list."
  (if (stringp items)
      (nlk:trimmed items)
      (subseq (nlk::sha256-text
               (format nil "~{~a~}" (loop for item in items
                                          append (if (eq (first item) :text)
                                                     (list "text" (second item))
                                                     (list "image" (second item) (third item))))))
              7)))

(defun prompt-content (items)
  "buildCursorRootPromptContent: the user content as the JSON prompt parts."
  (if (stringp items)
      (let ((text (nlk:trimmed items)))
        (if (plusp (length text)) (list (nlk:json-object "type" "text" "text" text)) '()))
      (loop for item in items
            if (eq (first item) :text)
              append (let ((text (nlk:trimmed (second item))))
                       (and (plusp (length text)) (list (nlk:json-object "type" "text" "text" text))))
            else collect (nlk:json-object "type" "image"
                                          "image" (format nil "data:~a;base64,~a" (second item) (third item))
                                          "mediaType" (second item)))))

(defun normalize-call-id (id)
  "A tool-call id within Cursor's charset (normalizeToolCallId)."
  (let ((clean (ppcre:regex-replace-all "[^a-zA-Z0-9_-]" (or id "") "_")))
    (if (> (length clean) 64) (subseq clean 0 64) clean)))

(defun call-arguments (call)
  "A chat tool call's arguments as a JSON object, an empty one when they do not parse."
  (let* ((text (nlk:json-value call :string "function" "arguments"))
         (parsed (and text (ignore-errors (nlk:decode-json text)))))
    (if (hash-table-p parsed) parsed (make-hash-table :test #'equal))))

(defun object-alist (object)
  "OBJECT's members in order, as (KEY . VALUE)."
  (let ((pairs '()))
    (maphash (lambda (key value) (push (cons key value) pairs)) object)
    (nreverse pairs)))

(defun assistant-calls (message)
  "MESSAGE's tool calls: ((ID NAME ARGUMENTS) ...)."
  (loop for call across (or (nlk:json-value message :array "tool_calls") #())
        collect (list (or (nlk:json-value call :string "id") "")
                      (or (nlk:json-value call :string "function" "name") "")
                      (call-arguments call))))

;;; Which Cursor model wrote a round. omp keeps the provider and model on
;;; every assistant message; the organism's message carries neither, and a
;;; field the lane added would ride every other lane's replay of the history
;;; (the chat lane sends assistant messages verbatim). So the cell keeps it
;;; here, in this process: the round's digest (its text, its thinking, its
;;; call ids, which the stored message replays unchanged) -> the model. A
;;; round this process did not write, or wrote before a restart, has no
;;; known writer.

(defvar *round-models* (make-hash-table :test #'equal :synchronized t)
  "A round's digest -> the Cursor model that wrote it.")

(defparameter +round-models-limit+ 4096
  "The most rounds whose writer is kept; past it the table starts over.")

(defun round-key (message)
  "The digest an assistant MESSAGE is known by: what it replays."
  (nlk::sha256-text (format nil "~a~c~a~c~{~a~^,~}"
                            (nle::message-content message) #\Nul
                            (or (nlk:json-value message :string "reasoning_content") "") #\Nul
                            (mapcar #'first (assistant-calls message)))))

(defun note-round-model (message model-id)
  "Remember that MODEL-ID wrote the round MESSAGE."
  (when (>= (hash-table-count *round-models*) +round-models-limit+)
    (clrhash *round-models*))
  (setf (gethash (round-key message) *round-models*) model-id))

(defun round-model (message)
  "The Cursor model that wrote the round MESSAGE, when this process knows it."
  (gethash (round-key message) *round-models*))

(defun replay-thinking-p (message target)
  "canReplayCursorThinking: only a Kimi K3 round's own thinking replays, to the same model."
  (and target (k3-p target) (equal target (round-model message))))

(defun result-text (message)
  "A tool result's text (toolResultToText)."
  (items-text (content-items (gethash "content" message)) :images t))

(defun tool-history (messages end)
  "(values RESULTS PAIRED): the tool results of the first END MESSAGES by
call id, and the ids of the calls the assistant made there."
  (let ((results (make-hash-table :test #'equal)) (paired (make-hash-table :test #'equal)))
    (loop for message in messages
          for index below end
          do (cond ((equal (message-role message) "tool")
                    (setf (gethash (or (nlk:json-value message :string "tool_call_id") "") results) message))
                   ((equal (message-role message) "assistant")
                    (dolist (call (assistant-calls message))
                      (setf (gethash (first call) paired) t)))))
    (values results paired)))

(defun call-name (messages id)
  "The tool the call ID of MESSAGES names."
  (dolist (message messages "")
    (when (equal (message-role message) "assistant")
      (let ((call (find id (assistant-calls message) :key #'first :test #'equal)))
        (when call (return (second call)))))))

(defun orphan-result-text (message)
  "A tool result with no call before it, as text (cursorOrphanToolResultText)."
  (let ((text (result-text message)))
    (format nil "[Tool Result]~%~a" (if (plusp (length text)) text "(empty result)"))))

(defun check-k3-history (messages end target)
  "Refuse to continue a Kimi K3 conversation another Cursor model wrote
(assertCursorKimiK3HistoryReplayable): K3 replays its own thinking, and
another model's turns carry none. A round whose writer this process does not
know (another provider's, or one written before a restart) cannot be told
apart, and is let through."
  (when (and target (k3-p target))
    (loop for message in messages
          for index below end
          when (equal (message-role message) "assistant")
            do (let ((model (round-model message)))
                 (when (and model (not (equal model target)))
                   (error 'nle::provider-config-error
                          :detail (format nil "Cursor ~a cannot continue history from a different model (~a/~a); start a new session."
                                          target +provider+ model)))))))

(defun json-blob (blobs object)
  "OBJECT's compact JSON kept in BLOBS: its blob id."
  (store-blob blobs (coerce (nlk:encode-json-octets object) 'octets)))

(defun system-blobs (blobs system)
  "buildCursorSystemPromptJsons: the system prompt as one system message blob,
a stock greeting when there is none."
  (list (json-blob blobs (nlk:json-object "role" "system"
                                          "content" (if (plusp (length (nlk:trimmed (or system ""))))
                                                        system
                                                        "You are a helpful assistant.")))))

(defun root-prompt-blobs (messages blobs system-ids end target)
  "buildRootPromptMessagesJson: the first END MESSAGES as the prompt's JSON
blobs behind SYSTEM-IDS, each tool round's results right after it."
  (check-k3-history messages end target)
  (multiple-value-bind (results paired) (tool-history messages end)
    (let ((entries (reverse system-ids)) (emitted (make-hash-table :test #'equal)))
      (labels ((push-json (object) (push (json-blob blobs object) entries))
               (push-result (message)
                 (let ((id (or (nlk:json-value message :string "tool_call_id") "")))
                   (push-json (nlk:json-object
                               "role" "tool"
                               "id" (normalize-call-id id)
                               "content" (vector (nlk:json-object "type" "tool-result"
                                                                  "toolName" (call-name messages id)
                                                                  "toolCallId" (normalize-call-id id)
                                                                  "result" (result-text message)))))
                   (setf (gethash id emitted) t))))
        (loop for message in messages
              for index below end
              for role = (message-role message)
              do (cond
                   ((equal role "user")
                    (let ((content (prompt-content (content-items (gethash "content" message)))))
                      (when content
                        (push-json (nlk:json-object "role" "user" "content" (coerce content 'vector))))))
                   ((equal role "assistant")
                    ;; one model round: the thinking it may replay, its text, its calls
                    (let ((parts (append
                                  (let ((thinking (nlk:json-value message :string "reasoning_content")))
                                    (and thinking (plusp (length thinking)) (replay-thinking-p message target)
                                         (list (nlk:json-object
                                                "type" "reasoning" "text" thinking
                                                "providerOptions" (nlk:json-object
                                                                   "cursor" (nlk:json-object
                                                                             "modelName" (round-model message)))))))
                                  (let ((text (nle::message-content message)))
                                    (and (plusp (length text)) (list (nlk:json-object "type" "text" "text" text))))
                                  (mapcar (lambda (call)
                                            (nlk:json-object "type" "tool-call"
                                                             "toolCallId" (normalize-call-id (first call))
                                                             "toolName" (second call)
                                                             "args" (third call)))
                                          (assistant-calls message)))))
                      (when parts
                        (push-json (nlk:json-object "role" "assistant" "content" (coerce parts 'vector))))))
                   ((equal role "tool")
                    (let ((id (or (nlk:json-value message :string "tool_call_id") "")))
                      (cond ((gethash id emitted))
                            ((not (gethash id paired))
                             (push-json (nlk:json-object "role" "assistant"
                                                         "content" (vector (nlk:json-object "type" "text"
                                                                                            "text" (orphan-result-text message))))))
                            (t (push-result (gethash id results message))))))))
        (nreverse entries)))))

(defun result-items (message)
  "A tool result's content as MCP content items."
  (let ((items (content-items (gethash "content" message))))
    (if (stringp items)
        (list (mcp-text-item items))
        (loop for item in items
              collect (if (eq (first item) :text)
                          (mcp-text-item (second item))
                          (mcp-image-item (second item) (or (unbase64 (third item)) #())))))))

(defun user-images (items)
  "ITEMS's images as ((MIME . OCTETS) ...)."
  (loop for (mime . data) in (items-images items)
        for octets = (unbase64 data)
        when octets collect (cons mime octets)))

(defun turn-blobs (messages blobs active target)
  "buildConversationTurns: each user message before ACTIVE (the index of the
message the action carries, or NIL) and what answered it, as turn blobs."
  (multiple-value-bind (results paired) (tool-history messages (or active (length messages)))
    (let ((turns '()) (vector (coerce messages 'vector)) (i 0))
      (loop while (< i (length vector))
            do (let ((message (aref vector i)))
                 (cond
                   ((not (equal (message-role message) "user")) (incf i))
                   ((eql i active) (return))
                   (t
                    (let* ((items (content-items (gethash "content" message)))
                           (text (user-text items)))
                      (if (and (zerop (length text)) (null (items-images items)))
                          (incf i)
                          (let ((user (store-blob blobs (user-message
                                                         text
                                                         (deterministic-uuid (format nil "u:~d:~a" (length turns) (content-key items)))
                                                         (user-images items))))
                                (steps '()))
                            (incf i)
                            (loop while (and (< i (length vector)) (not (equal (message-role (aref vector i)) "user")))
                                  do (let ((step (aref vector i)))
                                       (cond
                                         ((equal (message-role step) "assistant")
                                          (let ((thinking (nlk:json-value step :string "reasoning_content"))
                                                (text (nle::message-content step)))
                                            (when (and thinking (plusp (length thinking)) (replay-thinking-p step target))
                                              (push (store-blob blobs (thinking-step thinking)) steps))
                                            (when (plusp (length text))
                                              (push (store-blob blobs (assistant-step text)) steps))
                                            (dolist (call (assistant-calls step))
                                              (let ((result (gethash (first call) results)))
                                                (push (store-blob blobs (tool-call-step (normalize-call-id (first call))
                                                                                        (second call)
                                                                                        (object-alist (third call))
                                                                                        (and result (result-items result))))
                                                      steps)))))
                                         ((and (equal (message-role step) "tool")
                                               (not (gethash (or (nlk:json-value step :string "tool_call_id") "") paired)))
                                          (push (store-blob blobs (assistant-step (orphan-result-text step))) steps))))
                                     (incf i))
                            (push (store-blob blobs (agent-turn user (nreverse steps))) turns))))))))
      (nreverse turns))))

(defun state-root-ids (state)
  "The root prompt blob ids a checkpoint STATE holds."
  (pb-all state 1))

(defun conversation-state-for (messages blobs system-ids history-end target cached)
  "buildCursorConversationState: the history rebuilt over the cached
checkpoint's other fields while its system prompt is the same."
  (let* ((turns (turn-blobs messages blobs history-end target))
         (root (root-prompt-blobs messages blobs system-ids (or history-end (length messages)) target))
         (head (subseq (state-root-ids cached) 0 (min (length system-ids) (length (state-root-ids cached)))))
         (matching (and (= (length head) (length system-ids))
                        (every #'equalp head system-ids))))
    (conversation-state root turns (and matching cached))))

;;; --- the tools and the rules the service is told -----------------------------------

(defparameter +native-tool-names+ '("bash" "read" "write" "delete" "ls" "grep" "todo")
  "Tools Cursor drives natively over the exec channel, which advertising as
MCP tools would offer twice.")

(defparameter +schema-value-keys+
  '("additionalProperties" "unevaluatedProperties" "unevaluatedItems" "items" "additionalItems"
    "contains" "propertyNames" "contentSchema" "if" "then" "else" "not" "anyOf" "oneOf" "allOf"
    "prefixItems"))

(defparameter +schema-map-keys+ '("properties" "patternProperties" "$defs" "definitions" "dependentSchemas"))

(defun schema-ref (root ref depth)
  "The local definition REF (#/$defs/NAME or #/definitions/NAME) names in ROOT."
  (when (< depth 32)
    (ppcre:register-groups-bind (bag name) ("^#/(\\$defs|definitions)/(.+)$" ref)
      (nlk:json-value root :object bag name))))

(defun dereference (value root &optional (depth 0))
  "VALUE with every local $ref inlined from ROOT (dereferenceJsonSchema, local refs)."
  (cond ((hash-table-p value)
         (let ((target (and (stringp (gethash "$ref" value)) (schema-ref root (gethash "$ref" value) depth))))
           (if target
               (dereference target root (1+ depth))
               (let ((copy (make-hash-table :test #'equal)))
                 (maphash (lambda (key item)
                            (unless (member key '("$defs" "definitions") :test #'equal)
                              (setf (gethash key copy) (dereference item root depth))))
                          value)
                 copy))))
        ((and (vectorp value) (not (stringp value))) (map 'vector (lambda (item) (dereference item root depth)) value))
        (t value)))

(defun combiner-p (value &optional inside-map)
  "Whether VALUE carries anyOf, oneOf or allOf in a schema position."
  (cond ((and (vectorp value) (not (stringp value))) (some #'combiner-p value))
        ((hash-table-p value)
         (or (and (not inside-map)
                  (some (lambda (key) (vectorp (gethash key value))) '("anyOf" "oneOf" "allOf")))
             (loop for key being the hash-keys of value using (hash-value item)
                   thereis (cond (inside-map (combiner-p item))
                                 ((member key +schema-map-keys+ :test #'equal) (combiner-p item t))
                                 ((member key +schema-value-keys+ :test #'equal) (combiner-p item))))))))

(defun object-schema-p (schema)
  "Whether SCHEMA describes an object by its properties."
  (and (hash-table-p schema)
       (or (hash-table-p (gethash "properties" schema)) (equal "object" (gethash "type" schema)))))

(defun fold-combiners (node)
  "NODE with anyOf, oneOf and allOf folded out, only ever widening what it
accepts: object branches merge their properties into the node (allOf their
required lists, the others what every branch requires), any other combiner
is dropped."
  (dolist (combiner '("anyOf" "oneOf" "allOf") node)
    (let ((branches (gethash combiner node)))
      (when (vectorp branches)
        (remhash combiner node)
        (when (and (plusp (length branches)) (every #'object-schema-p branches))
          (let ((properties (or (gethash "properties" node) (make-hash-table :test #'equal)))
                (required (coerce (or (gethash "required" node) #()) 'list))
                (branch-required (map 'list (lambda (branch) (coerce (or (gethash "required" branch) #()) 'list))
                                      branches)))
            (loop for branch across branches
                  do (alexandria:when-let (own (gethash "properties" branch))
                       (maphash (lambda (name schema)
                                  (unless (nth-value 1 (gethash name properties))
                                    (setf (gethash name properties) (project-schema schema))))
                                own)))
            (setf (gethash "type" node) "object"
                  (gethash "properties" node) properties)
            (let ((merged (remove-duplicates
                           (append required
                                   (if (equal combiner "allOf")
                                       (reduce #'append branch-required)
                                       (reduce (lambda (a b) (intersection a b :test #'equal)) branch-required)))
                           :test #'equal :from-end t)))
              (if merged
                  (setf (gethash "required" node) (coerce merged 'vector))
                  (remhash "required" node)))))))))

(defun project-schema (value &optional inside-map)
  "VALUE projected onto what Cursor's MCP catalog accepts (a compact
projectSchemaForCursor): no composition keyword anywhere, no negation of one."
  (cond ((and (vectorp value) (not (stringp value))) (map 'vector #'project-schema value))
        ((hash-table-p value)
         (let ((result (make-hash-table :test #'equal)))
           (maphash (lambda (key item)
                      (unless (and (not inside-map) (equal key "not") (combiner-p item))
                        (setf (gethash key result)
                              (cond (inside-map (project-schema item))
                                    ((and (member key +schema-map-keys+ :test #'equal) (hash-table-p item))
                                     (project-schema item t))
                                    ((member key +schema-value-keys+ :test #'equal) (project-schema item))
                                    (t item)))))
                    value)
           (if inside-map result (fold-combiners result))))
        (t value)))

(defun tool-definitions (tools model-id)
  "buildMcpToolDefinitions: Nodecode's TOOLS (chat wrappers) as ((NAME .
McpToolDefinition) ...), the natively driven ones left out (write kept
whenever any tool is advertised), schemas projected for a model that needs it."
  (let* ((functions (map 'list (lambda (wrapper) (gethash "function" wrapper)) (or tools #())))
         (advertised (remove-if (lambda (fn) (member (gethash "name" fn) +native-tool-names+ :test #'equal))
                                functions))
         (write (find "write" functions :key (lambda (fn) (gethash "name" fn)) :test #'equal)))
    (when advertised
      (loop for fn in (if write (append advertised (list write)) advertised)
            for name = (gethash "name" fn)
            for schema = (let ((raw (gethash "parameters" fn)))
                           (cond ((not (hash-table-p raw))
                                  (nlk:json-object "type" "object" "properties" (make-hash-table :test #'equal)
                                                   "required" #()))
                                 ((tool-schema-projection-p model-id) (project-schema (dereference raw raw)))
                                 (t raw)))
            collect (cons name (mcp-tool-definition name (or (gethash "description" fn) "") schema))))))

(defun request-rules (system)
  "buildCursorRequestContextRules: the system prompt as an always-applied rule."
  (when (plusp (length (nlk:trimmed (or system ""))))
    (list (cursor-rule "/omp/system-prompt/0.mdc" system))))

;;; --- the run request ------------------------------------------------------------------

(defstruct (request (:copier nil))
  octets          ; the AgentClientMessage that opens the run
  fallback        ; the discovery id a not-found may be retried with, or NIL
  model-id)

(defun build-request (context model-id wire-id mode conversation-id conversation &key fresh)
  "The run request of the compiled CONTEXT for MODEL-ID routed to WIRE-ID
(buildGrpcRequestForWireMode): the trailing user message as the action, the
rest of the history as the conversation state. FRESH builds without the
cached checkpoint (a rotated conversation)."
  (let* ((blobs (conv-blobs conversation))
         (system (nle::compiled-turn-context-system-prompt context))
         (messages (coerce (nle::request-messages context) 'list))
         (last (car (last messages)))
         (active (and last (equal (message-role last) "user") (1- (length messages))))
         (items (and active (content-items (gethash "content" last))))
         (text (cond ((null active) "")
                     ((stringp items) (nlk:trimmed items))
                     (t (items-text items))))
         (images (and active (user-images items)))
         (system-ids (system-blobs blobs system))
         (state (conversation-state-for messages blobs system-ids active model-id
                                        (and (not fresh) (conv-state conversation))))
         (action (conversation-action (and active (or (plusp (length (nlk:trimmed text))) images)
                                           (user-message text (uuid) images)))))
    (multiple-value-bind (base-id details-id parameters max-mode) (resolve-wire-model model-id wire-id mode)
      (let ((row (find-row +models+ model-id)))
        (make-request
         :octets (run-request state action
                              (model-details details-id model-id
                                             (or (nlk:json-value row :string "name") model-id) max-mode)
                              (requested-model base-id max-mode parameters)
                              conversation-id)
         :fallback (and (eq mode :normalized) parameters (not (equal base-id wire-id)) wire-id)
         :model-id model-id)))))

;;; --- the transport ------------------------------------------------------------------

(defparameter +heartbeat-seconds+ 5)

(defparameter +append-base-timeout+ 60
  "Seconds one BidiAppend may take, plus one per 128 KiB it carries.")

(defstruct (pipe (:copier nil))
  "The appends of one run, sent in order by a thread of their own."
  (lock (bt2:make-lock :name "cursor appends"))
  (ready (bt2:make-condition-variable))
  (queue '())
  (seqno 0)
  (closed nil)
  (ended nil)
  (failure nil)
  (thread nil)
  (body nil)
  url headers request-id proxy)

(defun pipe-send (pipe message)
  "Queue the AgentClientMessage MESSAGE; nothing once the run has ended."
  (bt2:with-lock-held ((pipe-lock pipe))
    (unless (or (pipe-closed pipe) (pipe-ended pipe))
      (setf (pipe-queue pipe) (append (pipe-queue pipe) (list message)))
      (bt2:condition-notify (pipe-ready pipe)))))

(defun pipe-end (pipe)
  "Stop sending: the server's end frame arrived, so what is queued is moot."
  (bt2:with-lock-held ((pipe-lock pipe))
    (setf (pipe-ended pipe) t (pipe-queue pipe) '())))

(defun pipe-close (pipe)
  "Stop the appends and let the thread go."
  (bt2:with-lock-held ((pipe-lock pipe))
    (setf (pipe-closed pipe) t (pipe-queue pipe) '())
    (bt2:condition-notify (pipe-ready pipe)))
  (let ((thread (pipe-thread pipe)))
    (when (and thread (not (eq thread (bt2:current-thread))))
      (loop repeat 40 while (bt2:thread-alive-p thread) do (sleep 0.05)))))

(defun append-failure (status body)
  "The error a refused BidiAppend is: its Connect error when it sent one."
  (let ((error (ignore-errors (nlk:json-value (nlk:decode-json (nlk:body-text body)) :object "error"))))
    (if (hash-table-p error)
        (connect-error error)
        (make-condition 'nle::provider-error :status status :scope :stream
                                             :detail (format nil "Cursor BidiAppend failed with HTTP ~a" status)))))

(defun post-append (pipe seqno data)
  "Send one append; NIL when the service took it, else the error it is."
  (let ((timeout (+ +append-base-timeout+ (ceiling (length data) (* 128 1024)))))
    (handler-case
        (multiple-value-bind (body status)
            (dex:post (pipe-url pipe)
                      :headers (cons '("content-type" . "application/proto")
                                     (remove "content-type" (pipe-headers pipe) :key #'car :test #'string-equal))
                      :content (bidi-append-request (pipe-request-id pipe) seqno data)
                      :connect-timeout timeout :read-timeout timeout
                      :proxy (or (pipe-proxy pipe) dex:*default-proxy*))
          (and (not (and (integerp status) (<= 200 status 299)))
               (append-failure status body)))
      (dex:http-request-failed (e)
        (append-failure (dex:response-status e) (ignore-errors (dex:response-body e))))
      (error (e)
        (make-condition 'nle::provider-error :scope :stream :detail (format nil "Cursor BidiAppend failed: ~a" e))))))

(defun pipe-loop (pipe)
  "The append thread: each queued message in turn, a heartbeat every five
seconds; the first refusal fails the run."
  (let ((next-beat (+ (get-internal-real-time) (* +heartbeat-seconds+ internal-time-units-per-second))))
    (loop
      (let ((message nil))
        (bt2:with-lock-held ((pipe-lock pipe))
          (loop until (or (pipe-closed pipe) (pipe-queue pipe)
                          (>= (get-internal-real-time) next-beat))
                do (bt2:condition-wait (pipe-ready pipe) (pipe-lock pipe)
                                       :timeout (max 0.01 (/ (- next-beat (get-internal-real-time))
                                                              internal-time-units-per-second))))
          (when (pipe-closed pipe) (return))
          (cond ((pipe-queue pipe) (setf message (pop (pipe-queue pipe))))
                ((not (pipe-ended pipe))
                 (setf message (heartbeat-message)
                       next-beat (+ (get-internal-real-time) (* +heartbeat-seconds+ internal-time-units-per-second))))
                (t (setf next-beat (+ (get-internal-real-time) (* +heartbeat-seconds+ internal-time-units-per-second))))))
        (when message
          (let ((failure (post-append pipe (prog1 (pipe-seqno pipe) (incf (pipe-seqno pipe))) message)))
            (when failure
              (bt2:with-lock-held ((pipe-lock pipe))
                (setf (pipe-failure pipe) failure (pipe-closed pipe) t))
              ;; the round's thread may be parked on the stream: wake it
              (alexandria:when-let (body (pipe-body pipe))
                (nle::close-provider-stream-body body))
              (return))))))))

(defun read-octet (stream seconds)
  "The next octet of STREAM, NIL at its end, waiting at most SECONDS."
  (sb-sys:with-deadline (:seconds seconds) (read-byte stream nil nil)))

(defun read-frame (stream seconds)
  "(values FLAGS PAYLOAD) of the next Connect frame on STREAM, NIL at a clean end."
  (let ((head (make-array 5 :element-type '(unsigned-byte 8))))
    (dotimes (i 5)
      (let ((octet (read-octet stream seconds)))
        (cond (octet (setf (aref head i) octet))
              ((zerop i) (return-from read-frame nil))
              (t (error 'nle::provider-stream-incomplete :detail "Cursor stream ended inside a Connect frame header")))))
    (let* ((length (loop for i from 1 to 4 sum (ash (aref head i) (* 8 (- 4 i)))))
           (payload (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (i length)
        (setf (aref payload i)
              (or (read-octet stream seconds)
                  (error 'nle::provider-stream-incomplete :detail "Cursor stream ended inside a Connect frame"))))
      (values (aref head 0) payload))))

;;; --- folding the stream ------------------------------------------------------------------

(defstruct (fold (:copier nil))
  "One attempt's state: the assembly, the calls in flight, what the stream said."
  asm
  (spans 0)
  ;; envelope call id -> (KIND INDEX CALL-ID): KIND :mcp or :resolved (a call
  ;; the service settles itself), INDEX the tool buffer an MCP call fills
  (open (make-hash-table :test #'equal))
  (current nil)
  ;; tool-call id -> its tool buffer index, every call this round emits
  (emitted (make-hash-table :test #'equal))
  ;; tool buffer index -> the argument text streamed so far
  (partial (make-hash-table))
  (next-index 0)
  (last-kind nil)
  (turn-ended nil)
  (step-latest nil)
  (progress nil)
  (token-delta nil)
  (checkpoint-terminal nil)
  (end-error nil)
  (usage (nle::make-provider-usage))
  tools rules directory conversation send)

(defun next-span (fold prefix)
  (format nil "~a-~d" prefix (incf (fold-spans fold))))

(defun close-blocks (fold)
  "End the open text and thinking spans (endCurrentTextBlock, endCurrentThinkingBlock)."
  (nle::lifecycle-close-text (fold-asm fold))
  (nle::lifecycle-close-reasoning (fold-asm fold)))

(defun emit-call (fold id name arguments)
  "File the call ID of the tool NAME on the round's message: the turn loop
runs it. ARGUMENTS, a JSON object or NIL while they stream."
  (close-blocks fold)
  (let ((index (fold-next-index fold)))
    (incf (fold-next-index fold))
    (setf (gethash id (fold-emitted fold)) index
          (fold-last-kind fold) :tool)
    (nle::open-tool-buffer (fold-asm fold) index :id id :name name
                                                 :arguments (if arguments (nlk:encode-json-object arguments) ""))
    index))

(defun settle-arguments (fold index arguments)
  "Set the call at INDEX's arguments to the JSON object ARGUMENTS."
  (let ((cell (assoc index (nle::lane-assembly-tool-buffers (fold-asm fold)))))
    (when cell
      (setf (getf (cdr cell) :arguments) (nlk:encode-json-object arguments)))))

(defun rewritten-snapshot-p (text)
  "Whether TEXT holds a whole JSON value with more after it (a snapshot
appended to another, `{...}{...}'), as against a value cut short."
  (let* ((start (ignore-errors (nlk::jsonc-trivia-end text 0)))
         (end (and start (< start (length text)) (ignore-errors (nlk::jsonc-value-end text start)))))
    (and end (< (or (ignore-errors (nlk::jsonc-trivia-end text end)) end) (length text)))))

(defun merge-arguments (streamed completion)
  "mergeCursorMcpToolCallArgs: the completion frame's arguments over the
streamed ones, unless it downgraded a structured value to a string."
  (let ((merged (make-hash-table :test #'equal)))
    (when (hash-table-p streamed)
      (maphash (lambda (key value) (setf (gethash key merged) value)) streamed))
    (when (hash-table-p completion)
      (maphash (lambda (key value)
                 (let ((old (gethash key merged)))
                   (unless (and (stringp value) (or (hash-table-p old) (and (vectorp old) (not (stringp old)))))
                     (setf (gethash key merged) value))))
               completion))
    merged))

(defun retain (fold call-id kind &optional index)
  "Keep the streamed call CALL-ID open until its completion."
  (let ((entry (list kind index call-id)))
    (when (plusp (length call-id))
      (setf (gethash call-id (fold-open fold)) entry))
    (setf (fold-current fold) entry)))

(defun resolve-open (fold call-id)
  "The open call a streamed update addresses (resolveStreamedCall), or NIL."
  (if (zerop (length call-id))
      (fold-current fold)
      (or (gethash call-id (fold-open fold))
          (let ((current (fold-current fold)))
            (and current (zerop (length (third current))) current)))))

(defun release (fold entry)
  "Let a settled call go."
  (when (plusp (length (third entry)))
    (remhash (third entry) (fold-open fold)))
  (when (eq (fold-current fold) entry)
    (setf (fold-current fold) nil)))

(defun tool-call-started (fold update)
  "toolCallStarted: an MCP call opens a call of the round's message; a call the
service resolves itself, or the exec channel owns, files nothing here."
  (let* ((call-id (pb-text* update 1))
         (tool-call (pb-sub update 2)))
    (close-blocks fold)
    (setf (fold-last-kind fold) :tool)
    (multiple-value-bind (variant) (tool-call-variant tool-call)
      (cond
        ((member variant +exec-owned-tool-calls+))
        ((eql variant 15)
         (let* ((args (mcp-call-of tool-call))
                (id (let ((id (pb-text* args 3))) (if (plusp (length id)) id (uuid)))))
           (unless (gethash id (fold-emitted fold))
             (retain fold call-id :mcp (emit-call fold id (mcp-call-name args) nil)))))
        ((member variant +server-resolved-tool-calls+)
         (retain fold call-id :resolved))))))

(defun partial-tool-call (fold update)
  "partialToolCall: the cumulative argument text of an open MCP call."
  (let ((entry (resolve-open fold (pb-text* update 1))))
    (when (and entry (eq (first entry) :mcp))
      (let* ((index (second entry))
             (snapshot (pb-text* update 3))
             (current (gethash index (fold-partial fold) ""))
             (chunk (if (uiop:string-prefix-p current snapshot) (subseq snapshot (length current)) snapshot)))
        (when (plusp (length chunk))
          (setf (gethash index (fold-partial fold)) (concatenate 'string current chunk))
          (nle::assembly-tool-fragment (fold-asm fold) index chunk))))))

(defun tool-call-completed (fold update)
  "toolCallCompleted: an MCP call's arguments settle, its streamed text read
whole and the completion's own map laid over it."
  (let ((entry (resolve-open fold (pb-text* update 1))))
    (when entry
      (when (eq (first entry) :mcp)
        (let* ((index (second entry))
               (partial (gethash index (fold-partial fold)))
               (streamed (and partial (ignore-errors (nlk:decode-json partial))))
               (completion (mcp-call-arguments (mcp-call-of (pb-sub update 2)))))
          (cond ((or (null partial) (hash-table-p streamed))
                 (settle-arguments fold index (merge-arguments streamed completion)))
                ((and completion (plusp (hash-table-count completion)) (rewritten-snapshot-p partial))
                 ;; a buffer that does not parse but is no cut-off prefix: the
                 ;; completion frame alone; a cut-off one stays as it came,
                 ;; and the turn loop refuses it rather than run a call that
                 ;; may lack what was cut
                 (settle-arguments fold index completion)))))
      (release fold entry))))

(defun apply-turn-ended (fold update)
  "applyTurnEndedUsage: the turn's own counters replace the streamed estimate."
  (let ((usage (fold-usage fold))
        (input (pb-signed (pb-get update 1))) (output (pb-signed (pb-get update 2)))
        (read (pb-signed (pb-get update 3))) (write (pb-signed (pb-get update 4)))
        (reasoning (pb-signed (pb-get update 5))))
    (flet ((n (value) (or value 0)))
      (when (some #'plusp (list (n input) (n output) (n read) (n write)))
        (when (plusp (n input))
          (setf (nle::provider-usage-input-tokens usage) (max 0 (- (n input) (n read) (n write)))))
        (when (plusp (n output)) (setf (nle::provider-usage-output-tokens usage) (n output)))
        (when (plusp (n read)) (setf (nle::provider-usage-cached-input-tokens usage) (n read)))
        (when (plusp (n write)) (setf (nle::provider-usage-cache-write-tokens usage) (n write)))
        (when (plusp (n reasoning)) (setf (nle::provider-usage-reasoning-tokens usage) (n reasoning)))
        (setf (nle::provider-usage-total-tokens usage)
              (+ (or (nle::provider-usage-input-tokens usage) 0) (or (nle::provider-usage-output-tokens usage) 0)
                 (or (nle::provider-usage-cached-input-tokens usage) 0)
                 (or (nle::provider-usage-cache-write-tokens usage) 0)))))))

(defun interaction-update (fold update)
  "processInteractionUpdate over the round's assembly."
  (multiple-value-bind (member fields) (update-member update)
    (let ((asm (fold-asm fold)))
      (case member
        (1 (let ((text (pb-text* fields 1)))
             (unless (nle::lane-assembly-text-id asm)
               (setf (fold-last-kind fold) :text))
             (nle::assembly-text-delta asm (or (nle::lane-assembly-text-id asm) (next-span fold "txt")) text
                                       :close-reasoning t)))
        (4 (let ((text (pb-text* fields 1)))
             (unless (nle::lane-assembly-reasoning-id asm)
               (setf (fold-last-kind fold) :thinking))
             (nle::assembly-reasoning-delta asm (or (nle::lane-assembly-reasoning-id asm) (next-span fold "rsn")) text
                                            :close-text t)))
        (5 (nle::lifecycle-close-reasoning asm))
        (2 (tool-call-started fold fields))
        (7 (partial-tool-call fold fields))
        (3 (tool-call-completed fold fields))
        (14 (apply-turn-ended fold fields))
        (8 (let ((usage (fold-usage fold)))
             (setf (fold-token-delta fold) t)
             (setf (nle::provider-usage-output-tokens usage)
                   (+ (or (nle::provider-usage-output-tokens usage) 0) (or (pb-signed (pb-get fields 1)) 0)))))))
    member))

(defun handoff (fold call-id name arguments)
  "An MCP call the exec channel asks for: filed on the round's message, with
the arguments the frame carries, unless the stream already opened it."
  (unless (gethash call-id (fold-emitted fold))
    (emit-call fold call-id name arguments)))

(defparameter +exec-call-blocks+ '(2 3 4 5 7 8 9 14 45 46 47 48 49 50 51 52 53)
  "The exec asks omp files as a call block of the round (synthesizeCursorExecToolCall)
even when nothing here runs them; this client files none, but a stream that
ends after one did not end on answer text.")

(defun exec-call-block-p (exec)
  "Whether omp files the exec ask EXEC as a call block: every one in
+EXEC-CALL-BLOCKS+, but a grep it refuses for want of a pattern."
  (multiple-value-bind (member args) (exec-member exec)
    (and (member member +exec-call-blocks+)
         (not (and (eql member 5) (empty-grep-pattern-rejection (pb-text args 1) (pb-text args 3)))))))

(defun server-message (fold octets)
  "handleServerMessage: fold one AgentServerMessage, answer what it asks."
  (multiple-value-bind (member fields) (server-member octets)
    (let ((update (and (eql member 1) (nth-value 0 (update-member fields)))))
      ;; a heartbeat is no progress
      (unless (eql update 13) (setf (fold-progress fold) t))
      (cond ((eql update 17) (setf (fold-step-latest fold) t))
            ((and (not (eql update 13)) (not (member member '(3 4))))
             (setf (fold-step-latest fold) nil)))
      (case member
        (1 (when (eql (interaction-update fold fields) 14)
             (setf (fold-turn-ended fold) t)))
        (3 (setf (fold-checkpoint-terminal fold) (fold-turn-ended fold)
                 (conv-state (fold-conversation fold)) fields))
        (4 (alexandria:when-let (answer (kv-answer fields (conv-blobs (fold-conversation fold))))
             (funcall (fold-send fold) answer)))
        (2 (when (exec-call-block-p fields)
             ;; omp files the asked call as a block of its own, ending the text before it
             (close-blocks fold)
             (setf (fold-last-kind fold) :tool))
           (dolist (answer (exec-answer fields :rules (fold-rules fold) :tools (fold-tools fold)
                                               :directory (fold-directory fold)
                                               :handoff (lambda (id name arguments)
                                                          (handoff fold id name arguments))))
             (funcall (fold-send fold) answer)))
        (7 (alexandria:when-let (answer (interaction-answer fields))
             (funcall (fold-send fold) answer)))))))

(defun complete-p (fold)
  "Whether the stream that ended said all of the turn: its turnEnded, or the
final step's completion after answer text with no call left open."
  (or (fold-turn-ended fold)
      (and (fold-step-latest fold)
           (eq (fold-last-kind fold) :text)
           (null (fold-current fold))
           (zerop (hash-table-count (fold-open fold))))))

;;; --- one attempt -----------------------------------------------------------------------

(defun round-directory ()
  "The working directory a refused shell names: the session's, else this process's."
  (let* ((turn (nle:turn))
         (cwd (and turn (ignore-errors (nlk:find-session-cwd (getf turn :session-id))))))
    (or cwd (namestring (uiop:getcwd)))))

(defun model-not-found-p (condition)
  "isCursorModelNotFound."
  (and (typep condition 'nle::provider-error)
       (let ((detail (or (nle::provider-error-detail condition) "")))
         (or (search "BAD_MODEL_NAME" detail)
             (ppcre:scan "(?i)^(?:Connect error not_found:|gRPC error 5:)" detail)))))

(defun attempt (config request fold token base)
  "Send REQUEST and fold the run's stream into FOLD."
  (let* ((request-id (uuid))
         ;; the appends carry the run's headers too, their content type their own
         (headers (append (client-headers token :content-type "application/connect+proto")
                          `(("connect-protocol-version" . "1")
                            ("x-request-id" . ,request-id)
                            ("x-cursor-streaming" . "true"))))
         (proxy (nle::effective-provider-config-proxy config))
         (seconds (nle::effective-provider-config-request-timeout config))
         (pipe (make-pipe :url (format nil "~a~a" base +bidi-append-path+)
                          :headers headers :request-id request-id :proxy proxy))
         (turn nle::*current-durable-turn*)
         (body nil))
    (setf (fold-send fold) (lambda (message) (pipe-send pipe message)))
    (pipe-send pipe (request-octets request))
    (setf (pipe-thread pipe) (bt2:make-thread (lambda () (pipe-loop pipe)) :name "cursor appends"))
    (unwind-protect
         (progn
           (setf body
                 (handler-case
                     (sb-sys:with-deadline (:seconds seconds)
                       (nlk:with-cancellable-wait (turn)
                         (multiple-value-bind (stream status)
                             (dex:post (format nil "~a~a" base +run-sse-path+)
                                       :headers headers
                                       :content (connect-frame (bidi-request-id request-id))
                                       :want-stream t :force-binary t
                                       :connect-timeout seconds :read-timeout (* 4 seconds)
                                       :proxy (or proxy dex:*default-proxy*))
                           (unless (eql status 200)
                             (error 'nle::provider-error :status status :scope :request
                                                         :detail (format nil "Cursor RunSSE failed with HTTP ~a" status)))
                           stream)))
                   (nlk:turn-cancelled-condition (c) (error c))
                   (nle::provider-error (e) (error e))
                   (dex:http-request-failed (e)
                     (error 'nle::provider-error :status (dex:response-status e) :scope :request
                                                 :detail (format nil "Cursor RunSSE failed with HTTP ~a" (dex:response-status e))))
                   (sb-sys:deadline-timeout ()
                     (error 'nle::provider-error :scope :request
                                                 :detail (format nil "no response for ~a s" seconds)))
                   (error (e)
                     (error 'nle::provider-error :scope :request :detail (format nil "Cursor RunSSE failed: ~a" e)))))
           (setf (pipe-body pipe) body)
           (nle::emit-stream-part (nle::lane-assembly-on-part (fold-asm fold)) :stream-start)
           (handler-case
               (loop
                 (when turn (nlk:ensure-turn-not-cancelled turn))
                 (multiple-value-bind (flags payload)
                     (nlk:with-cancellable-wait (turn) (read-frame body seconds))
                   (unless flags (return))
                   (cond ((logtest flags +connect-end-stream+)
                          (let ((failure (end-stream-error payload)))
                            (if failure
                                (progn (setf (fold-end-error fold) failure) (return))
                                (pipe-end pipe))))
                         (t (handler-case (server-message fold payload)
                              (malformed-proto () nil))))))
             (sb-sys:deadline-timeout ()
               (error 'nle::provider-stream-incomplete :detail (format nil "no data from Cursor for ~a s" seconds)))
             (nlk:turn-cancelled-condition (c) (error c))
             (nle::provider-error (e) (error (or (pipe-failure pipe) e)))
             (error (e)
               (error (or (pipe-failure pipe)
                          (make-condition 'nle::provider-stream-incomplete
                                          :detail (format nil "Cursor stream failed: ~a" e))))))
           (alexandria:when-let (failure (pipe-failure pipe)) (error failure))
           (let ((end-error (fold-end-error fold)))
             (cond ((and end-error (not (and (fold-turn-ended fold) (fold-checkpoint-terminal fold))))
                    (error end-error))
                   ((not (complete-p fold))
                    (error 'nle::provider-stream-incomplete :detail "Cursor stream ended before turnEnded")))))
      (pipe-close pipe)
      (when body (nle::close-provider-stream-body body)))))

;;; --- the lane ----------------------------------------------------------------------------

(defun round-token (config)
  "The token a round on CONFIG sends: the sign-in's, refreshed when a long
turn outlived the one the turn froze; else the frozen credential's."
  (let ((key (nle::effective-provider-config-api-key config))
        (path (nle::credential-attribute config :auth-path)))
    (or (and path (ignore-errors
                   (let ((stored (stored-entry (nle::read-auth-file path))))
                     (and stored (nlk:json-value (fresh-entry stored path) :text "access_token")))))
        (and (stringp key) (plusp (length key)) (not (equal key "public")) key))))

(defun base-conversation-id ()
  "The conversation the round belongs to: one per Nodecode session, as a UUID;
NIL outside a session."
  (let ((turn (nle:turn)))
    (and turn (getf turn :session-id)
         (deterministic-uuid (format nil "nodecode-cursor:~a" (getf turn :session-id))))))

(defun rotate (base conversation-id)
  "Give BASE a new wire id after the service poisoned CONVERSATION-ID."
  (let ((current (gethash base *rotated*)))
    (when (or (null current) (gethash current *rotated-good*))
      (let ((rotated (uuid)))
        (when current (remhash current *rotated-good*))
        (setf (gethash base *rotated*) rotated
              (gethash rotated *rotated-fresh*) t)
        (forget-conversation conversation-id)))))

(defun round-message (fold)
  "The round, chat-shaped: text, reasoning, the calls the turn loop runs;
nothing of the cell's own, since every lane replays it."
  (let ((asm (fold-asm fold)))
    (nle::assembly-close-spans asm :order '(:tools :reasoning :text))
    (nle::flush-thinking-tag asm)
    (let ((content (get-output-stream-string (nle::lane-assembly-content asm))))
      (nlk:json-object
       "role" "assistant"
       "content" (if (string= content "") :null content)
       :when (nle::lane-assembly-reasoning-seen-p asm) "reasoning_content"
       (get-output-stream-string (nle::lane-assembly-reasoning asm))
       :when (nle::lane-assembly-tool-buffers asm) "tool_calls"
       (map 'vector (lambda (pair &aux (buf (cdr pair)))
                      (nle::chat-tool-call-object (or (getf buf :id) "") (getf buf :name)
                                                  (let ((arguments (getf buf :arguments)))
                                                    (if (plusp (length arguments)) arguments "{}"))))
            (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car))))))

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The cursor lane: one Cursor run over the compiled CONTEXT, its stream
folded into the round's message, every MCP call the service asks for handed
to the turn loop. => (values MESSAGE USAGE FINISH-REASON REQUEST-OCTETS)"
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model-id (nle::effective-provider-config-model config))
         (effort (round-effort model-id (nle::effective-provider-config-reasoning-effort config)))
         (wire-id (wire-model-id model-id effort))
         (token (round-token config))
         (base (string-right-trim "/" (nle::effective-provider-config-endpoint config)))
         (system (nle::compiled-turn-context-system-prompt context))
         (tools (tool-definitions (nle::compiled-turn-context-tools context) model-id))
         (rules (request-rules system))
         (directory (round-directory))
         (base-id (base-conversation-id))
         (ephemeral (null base-id))
         (base-id (or base-id (uuid))))
    (unless token
      (error 'nle::provider-config-error
             :status 401
             :detail (format nil "Cursor is not signed in: run /~a login, or set CURSOR_ACCESS_TOKEN" +provider+)))
    (unwind-protect
         (let ((mode :normalized))
           (loop
             (let* ((conversation-id (or (gethash base-id *rotated*) base-id))
                    (fresh (gethash conversation-id *rotated-fresh*))
                    (conversation (conversation conversation-id))
                    (request (build-request context model-id wire-id mode conversation-id conversation :fresh fresh))
                    (fold (make-fold :asm (nle::make-lane-assembly :on-part on-part)
                                     :tools tools :rules rules :directory directory
                                     :conversation conversation)))
               (handler-case
                   (progn
                     (attempt config request fold token base)
                     (unless (equal conversation-id base-id)
                       (setf (gethash conversation-id *rotated-good*) t)
                       (remhash conversation-id *rotated-fresh*))
                     (let* ((message (round-message fold))
                            (usage (fold-usage fold))
                            (finish (if (nlk:json-value message :array "tool_calls") "tool_calls" "stop")))
                       (note-round-model message model-id)
                       (nle::emit-stream-part on-part :finish)
                       (return (values message
                                       (and (not (equalp usage (nle::make-provider-usage))) usage)
                                       finish
                                       (request-octets request)))))
                 (nle::provider-error (e)
                   (cond
                     ;; the normalized pair unknown before any output: the
                     ;; discovery id, once
                     ((and (eq mode :normalized) (request-fallback request)
                           (not (fold-progress fold)) (model-not-found-p e))
                      (setf mode :discovered))
                     (t
                      (when (and (not (fold-token-delta fold))
                                 (ppcre:scan "(?i)resource.?exhausted" (or (nle::provider-error-detail e) "")))
                        (rotate base-id conversation-id))
                      (nle::assembly-close-spans (fold-asm fold) :order '(:tools :reasoning :text))
                      (error e))))))))
      (when ephemeral
        (let ((rotated (gethash base-id *rotated*)))
          (dolist (id (list base-id rotated))
            (when id
              (forget-conversation id)
              (remhash id *rotated-good*)
              (remhash id *rotated-fresh*)))
          (remhash base-id *rotated*))))))

(defun register-lane ()
  "The cursor lane: Cursor's agent service, a run per round."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            :family :cursor
                            :reasoning-carry :text
                            :default-endpoint +base+
                            :path ""
                            :npm "nodecode-cursor")))

(defun unregister-lane ()
  "Take the cursor lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))
