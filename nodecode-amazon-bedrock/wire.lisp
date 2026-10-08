;;;; wire.lisp --- the Converse Stream wire: the request, the signed POST, the frame fold.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/ai/src/providers/
;;;; amazon-bedrock.ts (the request body, the tool plan, the thinking fields,
;;;; the cache points, the signed POST, the event fold, the stop reasons and
;;;; the in-stream exceptions), dialect/demotion.ts (an unsigned thought
;;;; replayed as text) and ai/src/stream.ts (the budget a thinking round's
;;;; cap must leave room for).
;;;;
;;;; The lane takes a compiled turn context and answers what every lane
;;;; answers (NLE::DEFINE-PROVIDER-LANE's contract): (values MESSAGE USAGE
;;;; FINISH-REASON REQUEST-JSON), MESSAGE the chat-shaped assistant message
;;;; the event log keeps, every part streamed to ON-PART on the way. It does
;;;; not go through NLE::WALK-PROVIDER-STREAM: that walk reads server-sent
;;;; event lines, and Converse Stream answers binary frames. It keeps the
;;;; walk's bracket otherwise: the core's cancellable POST and its failure
;;;; classes, an idle deadline on every read, the :stream-start part, the
;;;; socket closed on every exit.

(in-package #:nodecode-amazon-bedrock)

(defparameter +no-tools-sentinel+ "__no_tools__"
  "The placeholder tool a request whose history holds tool blocks but which
offers no tools carries: Bedrock wants a toolConfig beside any tool block.")

(defparameter +thinking-binding-beta+ "thinking-binding-controls-2026-08-01"
  "The beta a prefix-bound thinking model's block binding rides.")

(defparameter +budgets+
  '(("minimal" . 1024) ("low" . 2048) ("medium" . 8192) ("high" . 16384) ("xhigh" . 32768) ("max" . 32768))
  "The token budget each rung buys a budget-thinking model.")

(defparameter +exception-status+
  '(("accessdeniedexception" . 403) ("conflictexception" . 400) ("internalserverexception" . 500)
    ("modelerrorexception" . 424) ("modelnotreadyexception" . 429) ("modelstreamerrorexception" . 424)
    ("modeltimeoutexception" . 408) ("resourcenotfoundexception" . 404) ("servicequotaexceededexception" . 400)
    ("serviceunavailableexception" . 503) ("throttlingexception" . 429) ("validationexception" . 400))
  "The HTTP status each in-stream exception shape stands for, from the
bedrock-runtime service model: retry reads a status, and an unknown shape is
400 so it is never replayed by accident.")

(defun exception-status (code)
  (or (cdr (assoc (string-downcase (string-trim " " (or code ""))) +exception-status+ :test #'equal)) 400))

(defun row-value (row type key) (nlk:json-value row type key))

(defun row-flag (row key default)
  "ROW's boolean KEY, DEFAULT when the row leaves it out (or there is no row)."
  (multiple-value-bind (value present) (if (hash-table-p row) (gethash key row) (values nil nil))
    (if present (and value (not (eq value :false)) t) default)))

;;; --- the messages ----------------------------------------------------------------

(defun image-format (media-type)
  (cdr (assoc (string-downcase (or media-type ""))
              '(("image/jpeg" . "jpeg") ("image/jpg" . "jpeg") ("image/png" . "png") ("image/gif" . "gif")
                ("image/webp" . "webp"))
              :test #'equal)))

(defun image-block (url)
  "A Converse image block for the data: URI URL, or NIL: Converse takes bytes only."
  (multiple-value-bind (media-type data) (nle::parse-data-uri url)
    (let ((format (and media-type (image-format media-type))))
      (and format (nlk:json-object "image" (nlk:json-object "format" format "source" (nlk:json-object "bytes" data)))))))

(defun blank-p (text)
  (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))))

(defun content-blocks (content)
  "CONTENT, a chat message's, as Converse user blocks: text and images; blank text dropped."
  (let ((blocks '()))
    (dolist (part (nle::message-content-parts content))
      (let ((type (nle::content-part-type part)))
        (cond ((equal type "text")
               (let ((text (or (nlk:json-value part :text "text") "")))
                 (unless (blank-p text) (push (nlk:json-object "text" text) blocks))))
              ((equal type "image_url")
               (alexandria:when-let (block (image-block (nlk:json-value part :string "image_url" "url")))
                 (push block blocks))))))
    (nreverse blocks)))

(defun demoted-thinking (model-id text)
  "TEXT, a thought that cannot be replayed as reasoning, as the assistant
text omp demotes it to: bare for Claude (Anthropic refuses a wrapped one), a
<think> block for the families whose own form is a template token, a
<thinking> block otherwise (omp's renderDemotedThinking)."
  (cond ((zerop (length text)) "")
        ((or (search "anthropic." model-id) (search "claude" model-id)) text)
        ((some (lambda (family) (search family model-id)) '("deepseek" "gpt-oss" "gemma" "qwen" "kimi"))
         (format nil "<think>~%~a~%</think>" text))
        (t (format nil "<thinking>~%~a~%</thinking>" text))))

(defun assistant-blocks (message model-id)
  "MESSAGE, a chat-shaped assistant message, as Converse blocks: its signed
thought as reasoningContent, an unsigned one demoted to text, its text, its
tool calls."
  (let ((blocks '())
        (reasoning (nlk:json-value message :string "reasoning_content"))
        (signature (nlk:json-value message :text "reasoning_signature"))
        (text (nle::content-text (gethash "content" message))))
    (cond ((and reasoning signature)
           (push (nlk:json-object "reasoningContent"
                                  (nlk:json-object "reasoningText" (nlk:json-object "text" reasoning "signature" signature)))
                 blocks))
          ((not (blank-p reasoning))
           (push (nlk:json-object "text" (demoted-thinking model-id reasoning)) blocks)))
    (unless (blank-p text) (push (nlk:json-object "text" text) blocks))
    (loop for call across (nlk:json-array message "tool_calls")
          do (multiple-value-bind (name input) (nle::tool-call-function-input call)
               (push (nlk:json-object "toolUse" (nlk:json-object "toolUseId" (or (gethash "id" call) "")
                                                                 "name" name "input" input))
                     blocks)))
    (nreverse blocks)))

(defun tool-result-block (message hoist)
  "MESSAGE, a tool result, as a toolResult block => (values BLOCK IMAGES):
its images moved out beside it when HOIST, as the model's row asks."
  (let ((content '()) (images '()))
    (let ((value (gethash "content" message)))
      (if (stringp value)
          (push (nlk:json-object "text" value) content)
          (dolist (part (nle::message-content-parts value))
            (let ((type (nle::content-part-type part)))
              (cond ((equal type "text")
                     (push (nlk:json-object "text" (or (nlk:json-value part :string "text") "")) content))
                    ((equal type "image_url")
                     (alexandria:when-let (block (image-block (nlk:json-value part :string "image_url" "url")))
                       (if hoist
                           (progn (push (nlk:json-object "text" "(see attached image)") content)
                                  (push block images))
                           (push block content)))))))))
    (values (nlk:json-object "toolResult"
                             (nlk:json-object "toolUseId" (or (nlk:json-value message :string "tool_call_id") "")
                                              "content" (coerce (or (nreverse content) (list (nlk:json-object "text" ""))) 'vector)
                                              "status" "success"))
            (nreverse images))))

(defun join (messages role blocks)
  "BLOCKS under ROLE joined onto MESSAGES (newest first): into the newest
when it has ROLE, as a message of its own otherwise, so roles alternate."
  (if (and messages (equal role (gethash "role" (first messages))))
      (progn (setf (gethash "content" (first messages)) (concatenate 'vector (gethash "content" (first messages)) blocks))
             messages)
      (cons (nlk:json-object "role" role "content" (coerce blocks 'vector)) messages)))

(defun converse-messages (context row)
  "CONTEXT's history as Converse messages: the head joined into the first
user message, consecutive tool results in one user message, an eviction stub
as user text, empty messages left out (omp's convertMessages)."
  (let ((messages '())
        (model-id (nle::effective-provider-config-model (nle::compiled-turn-context-provider-config context)))
        (hoist (row-value row :boolean "hoist_images")))
    (loop for message across (nle::messages-with-head context)
          for role = (and (hash-table-p message) (gethash "role" message))
          do (cond ((equal role "assistant")
                    (alexandria:when-let (blocks (assistant-blocks message model-id))
                      (setf messages (join messages "assistant" blocks))))
                   ((equal role "tool")
                    (multiple-value-bind (block images) (tool-result-block message hoist)
                      (setf messages (join messages "user" (cons block images)))))
                   ((equal role "system")
                    (let ((text (nle::content-text (gethash "content" message))))
                      (unless (blank-p text)
                        (setf messages (join messages "user" (list (nlk:json-object "text" (format nil "[system] ~a" text))))))))
                   (role
                    (alexandria:when-let (blocks (content-blocks (gethash "content" message)))
                      (setf messages (join messages "user" blocks))))))
    (coerce (nreverse messages) 'vector)))

(defun tool-blocks-p (messages)
  "Whether MESSAGES carry a toolUse or a toolResult block."
  (some (lambda (message)
          (some (lambda (block) (or (gethash "toolUse" block) (gethash "toolResult" block)))
                (coerce (gethash "content" message) 'list)))
        (coerce messages 'list)))

(defun without-reasoning (messages)
  "MESSAGES with every reasoningContent block taken out, an assistant message
left empty dropped: the retry after Bedrock refused a thought bound to a
prefix that changed."
  (remove nil
          (map 'vector (lambda (message)
                         (if (equal "assistant" (gethash "role" message))
                             (let ((content (remove-if (lambda (block) (gethash "reasoningContent" block))
                                                       (gethash "content" message))))
                               (and (plusp (length content)) (nlk:copy-json-object message "content" content)))
                             message))
               messages)))

;;; --- the request ---------------------------------------------------------------------

(defun cache-point ()
  (nlk:json-object "cachePoint" (nlk:json-object "type" "default")))

(defun cache-checkpoints (row)
  "How many cache points a request of ROW's model places: up to two where its
row says caching is explicit (one at the final user message, one after the
system prompt), two for any model under AWS_BEDROCK_FORCE_CACHE."
  (cond ((equal (row-value row :string "cache_mode") "explicit")
         (min 2 (or (row-value row :integer "cache_checkpoints") 0)))
        ((member (env "AWS_BEDROCK_FORCE_CACHE") '("1" "true" "yes") :test #'string-equal) 2)
        (t 0)))

(defun supported-effort (row effort)
  "EFFORT as a rung ROW's model declares: itself, else the strongest below
it, else the weakest it has."
  (let ((efforts (coerce (or (row-value row :array "efforts") #()) 'list)))
    (cond ((null efforts) effort)
          ((member effort efforts :test #'equal) effort)
          (t (or (find-if (lambda (rung) (<= (or (nle::effort-rank rung) 0) (or (nle::effort-rank effort) 0)))
                          (sort (copy-list efforts) #'> :key (lambda (rung) (or (nle::effort-rank rung) 0))))
                 (first efforts))))))

(defun thinking-fields (row effort)
  "The additionalModelRequestFields a round at EFFORT on ROW's model carries
(omp's buildAdditionalModelRequestFields), or NIL for none."
  (when (and effort (not (string-equal effort "off")) (row-value row :boolean "reasoning"))
    (let* ((mode (row-value row :string "thinking_mode"))
           (level (supported-effort row effort)))
      (cond ((equal mode "anthropic-adaptive")
             (nlk:json-object "thinking" (nlk:json-object "type" "adaptive"
                                                          :when (row-value row :boolean "thinking_display")
                                                          "display" "summarized")
                              "output_config" (nlk:json-object "effort" (if (equal level "minimal") "low" level))))
            ((equal mode "effort")
             (nlk:json-object "reasoning" (nlk:json-object "effort" level)))
            (t
             (nlk:json-object "thinking" (nlk:json-object "type" "enabled"
                                                          "budget_tokens" (or (cdr (assoc level +budgets+ :test #'equal)) 1024)
                                                          "display" "summarized")))))))

(defun bind-thinking (fields)
  "FIELDS with the thinking block bound to its prefix (drop a block whose
prefix changed rather than refuse the request) and the beta that binding
rides (omp's applyBedrockThinkingBinding)."
  (let* ((fields (if fields (nlk:copy-json-object fields) (make-hash-table :test 'equal)))
         (thinking (if (hash-table-p (gethash "thinking" fields))
                       (nlk:copy-json-object (gethash "thinking" fields))
                       (nlk:json-object "type" "adaptive")))
         (betas (remove-if-not #'stringp (coerce (or (nlk:json-value fields :array "anthropic_beta") #()) 'list))))
    (setf (gethash "block_binding" thinking) (nlk:json-object "prefix_mismatch_behavior" "drop_block")
          (gethash "thinking" fields) thinking
          (gethash "anthropic_beta" fields) (coerce (if (member +thinking-binding-beta+ betas :test #'equal)
                                                        betas
                                                        (append betas (list +thinking-binding-beta+)))
                                                    'vector))
    fields))

(defun tool-spec (wrapper)
  "A chat function tool as a Converse toolSpec; an empty description is left
out (Converse refuses one)."
  (let ((function (gethash "function" wrapper)))
    (nlk:json-object "toolSpec"
                     (nlk:json-object "name" (gethash "name" function)
                                      :when (not (blank-p (nlk:json-value function :string "description")))
                                      "description" (nlk:json-value function :string "description")
                                      "inputSchema" (nlk:json-object "json" (or (gethash "parameters" function)
                                                                                (nlk:json-object "type" "object")))))))

(defun tool-plan (tools choice messages)
  "(values TOOL-CONFIG SENTINEL-P) for TOOLS under CHOICE (omp's planToolConfig)."
  (let ((specs (map 'vector #'tool-spec (or tools #()))))
    (cond ((and (zerop (length specs)) (tool-blocks-p messages))
           ;; Bedrock wants a toolConfig beside any tool block in the history:
           ;; omp sends this placeholder for a tool-less request under `none',
           ;; and every tool-less request here is one
           (values (nlk:json-object
                    "tools" (vector (nlk:json-object
                                     "toolSpec" (nlk:json-object
                                                 "name" +no-tools-sentinel+
                                                 "description" "Placeholder required by Bedrock validation. Do not call; answer with text."
                                                 "inputSchema" (nlk:json-object "json" (nlk:json-object "type" "object"
                                                                                                        "properties" (make-hash-table :test 'equal))))))
                    "toolChoice" (nlk:json-object "auto" (make-hash-table :test 'equal)))
                   t))
          ((zerop (length specs)) (values nil nil))
          (t (values (nlk:json-object
                      "tools" specs
                      :opt "toolChoice" (cond ((string-equal choice "auto") (nlk:json-object "auto" (make-hash-table :test 'equal)))
                                              ((member choice '("required" "any") :test #'string-equal)
                                               (nlk:json-object "any" (make-hash-table :test 'equal)))))
                     nil)))))

(defun guardrail-config ()
  "The guardrailConfig the section asks for, or NIL."
  (alexandria:when-let (identifier (guardrail-identifier))
    (nlk:json-object "guardrailIdentifier" identifier
                     "guardrailVersion" (or (present (setting :guardrail-version)) "DRAFT")
                     :opt "trace" (present (setting :guardrail-trace)))))

(defun converse-request (context)
  "(values BODY SENTINEL-P) for CONTEXT: the ConverseStream request omp's
streamBedrock builds, and whether its toolConfig is the placeholder."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (row (model-row model))
         (effort (nle::effective-provider-config-reasoning-effort config))
         (messages (converse-messages context row))
         (checkpoints (cache-checkpoints row))
         (fields (thinking-fields row effort))
         (bound (row-value row :boolean "prefix_binding"))
         (cap (nle::effective-max-output-tokens context))
         (sampling (row-flag row "sampling" t)))
    ;; the final user message's cache point first, then the system prompt's
    (when (and (plusp checkpoints) (plusp (length messages))
               (equal "user" (gethash "role" (aref messages (1- (length messages))))))
      (let ((last (aref messages (1- (length messages)))))
        (setf (gethash "content" last) (concatenate 'vector (gethash "content" last) (vector (cache-point))))
        (decf checkpoints)))
    ;; a budget leaves room for the answer: a cap at or under it grows to
    ;; the budget and the answer's headroom, within the model's own ceiling
    (let ((budget (nlk:json-value fields :integer "thinking" "budget_tokens")))
      (when (and cap budget (<= cap budget))
        (setf cap (min (or (row-value row :integer "output") most-positive-fixnum) (+ budget 1024)))))
    (multiple-value-bind (tool-config sentinel-p)
        (tool-plan (nle::compiled-turn-context-tools context) (nle::effective-provider-config-tool-choice config) messages)
      (let ((forced (and tool-config (or (nlk:json-value tool-config :object "toolChoice" "any")
                                         (nlk:json-value tool-config :object "toolChoice" "tool")))))
        (cond ((and forced (not (row-flag row "forced_tool_choice" t)))
               ;; some models refuse forced tool use: offer the tools under auto
               (setf (gethash "toolChoice" tool-config) (nlk:json-object "auto" (make-hash-table :test 'equal))))
              ((and forced fields)
               ;; Bedrock refuses thinking beside a forced choice; a bound
               ;; model's thinking cannot be turned off, so it goes to auto
               (if bound
                   (setf (gethash "toolChoice" tool-config) (nlk:json-object "auto" (make-hash-table :test 'equal)))
                   (setf fields nil)))))
      (when bound (setf fields (bind-thinking fields)))
      (values
       (nlk:json-object
        "messages" messages
        "system" (if (plusp checkpoints)
                     (vector (nlk:json-object "text" (nle::compiled-turn-context-system-prompt context)) (cache-point))
                     (vector (nlk:json-object "text" (nle::compiled-turn-context-system-prompt context))))
        "inferenceConfig" (nlk:json-object :opt "maxTokens" cap
                                           :opt "temperature" (and sampling (nle::effective-provider-config-temperature config))
                                           :opt "topP" (and sampling (nle::effective-provider-config-top-p config)))
        :opt "toolConfig" tool-config
        :opt "guardrailConfig" (guardrail-config)
        :opt "additionalModelRequestFields" fields
        :when bound "additionalModelResponseFieldPaths" (vector "/input_transformations"))
       sentinel-p))))

;;; --- the signed POST ------------------------------------------------------------------

(defun round-credential (config)
  "(values BEARER CREDS) for the round CONFIG freezes: a Bedrock API key to
send as a bearer, else the AWS credentials to sign with."
  (let ((key (nle::effective-provider-config-api-key config)))
    (if (member key '("aws-sigv4" "public") :test #'equal)
        (values nil (if (member (env "AWS_BEDROCK_SKIP_AUTH") '("1" "true" "yes") :test #'string-equal)
                        (make-creds "dummy-access-key" "dummy-secret-key")
                        :resolve))
        (values key nil))))

(defun request-headers (bearer creds host path query octets region)
  "The headers a Converse Stream POST carries: its content type and accept,
then a bearer or the SigV4 signature over them."
  (let ((base `(("user-agent" . ,(nle::user-agent))
                ("content-type" . "application/json")
                ("accept" . "application/vnd.amazon.eventstream"))))
    (if bearer
        (append base `(("authorization" . ,(format nil "Bearer ~a" bearer))))
        (append base (sign-request :host host :path path :query query :body octets :region region
                                   :service "bedrock" :headers (rest base)
                                   :access-key (getf creds :access-key) :secret-key (getf creds :secret-key)
                                   :session-token (getf creds :session-token))))))

(defun post-converse (url headers octets config request-json)
  "POST OCTETS to URL as the core's walk posts: cancellable, under the
config's idle deadline until the headers arrive, a refusal classified the
core's way => (values STREAM STATUS RESPONSE-HEADERS)."
  (let ((seconds (nle::effective-provider-config-request-timeout config))
        (proxy (or (nle::effective-provider-config-proxy config) dex:*default-proxy*)))
    (nle::with-idle-cut (seconds)
        (sb-sys:with-deadline (:seconds seconds)
          (nle::cancellable-post (request-json (or proxy url))
            (dex:post url :headers headers :content octets
                          :read-timeout (* 4 seconds) :connect-timeout seconds :proxy proxy
                          :want-stream t :force-binary t :use-connection-pool t)))
      (error 'nle::provider-error :scope :request
                                  :detail (format nil "no response for ~a s" (nle::idle-seconds-label seconds))
                                  :evidence-request-body (nle::bounded-evidence request-json)))))

(defun prefix-binding-refusal-p (condition)
  "Whether CONDITION is Bedrock refusing a thought whose bound prefix changed
(omp's isThinkingPrefixBindingError)."
  (let ((text (format nil "~a ~a" (nle::provider-error-detail condition)
                      (or (nle::provider-error-evidence-body condition) ""))))
    (and (eql 400 (nle::provider-error-status condition))
         (not (ppcre:scan "(?i)\\bcompaction_[a-z_]+\\b" text))
         (ppcre:scan "(?i)invalid\\s+`?signature`?\\s+in\\s+`?thinking`?" text)
         (ppcre:scan "(?i)bound to a different conversation|prefix_mismatch_behavior" text)
         t)))

;;; --- the lane ---------------------------------------------------------------------------

(defun finish-reason (stop-reason sentinel-p)
  "STOP-REASON, Converse's, in the chat vocabulary, or NIL for one that ends
the round in error (omp's mapStopReason)."
  (cond ((member stop-reason '("end_turn" "stop_sequence") :test #'equal) "stop")
        ((member stop-reason '("max_tokens" "model_context_window_exceeded") :test #'equal) "length")
        ((equal stop-reason "tool_use") (if sentinel-p "stop" "tool_calls"))))

(defun stop-refusal (stop-reason)
  "The words a round that ended on STOP-REASON fails with."
  (cond ((equal stop-reason "guardrail_intervened")
         (format nil "Response blocked by Amazon Bedrock guardrail (stop reason: ~a)." stop-reason))
        ((equal stop-reason "content_filtered")
         (format nil "Response filtered by Amazon Bedrock content filters (stop reason: ~a)." stop-reason))
        (t (format nil "Generation failed with stop reason: ~a" (or stop-reason "unknown")))))

(defun apply-usage (usage table)
  "Fold one metadata event's usage TABLE into USAGE: Converse's input count
already excludes the cache, as the Messages wire's does."
  (when (hash-table-p table)
    (flet ((count-of (key) (nlk:json-value table :integer key)))
      (alexandria:when-let (input (count-of "inputTokens")) (setf (nle::provider-usage-input-tokens usage) input))
      (alexandria:when-let (output (count-of "outputTokens")) (setf (nle::provider-usage-output-tokens usage) output))
      (alexandria:when-let (read (count-of "cacheReadInputTokens"))
        (setf (nle::provider-usage-cached-input-tokens usage) read))
      (alexandria:when-let (write (count-of "cacheWriteInputTokens"))
        (setf (nle::provider-usage-cache-write-tokens usage) write))
      (setf (nle::provider-usage-total-tokens usage)
            (or (count-of "totalTokens")
                (+ (or (nle::provider-usage-input-tokens usage) 0) (or (nle::provider-usage-output-tokens usage) 0)))))))

(defun fold-frames (stream asm seconds sentinel-p)
  "Read STREAM's frames into the assembly ASM, each read under SECONDS
=> (values STOP-REASON STOPPED-P)."
  (let ((kinds (make-hash-table)) (ignored '()) (stop-reason nil) (stopped nil))
    (loop
      (when nle::*current-durable-turn* (nlk:ensure-turn-not-cancelled nle::*current-durable-turn*))
      (let ((frame (handler-case (nlk:with-cancellable-wait (nle::*current-durable-turn*) (read-frame stream seconds))
                     (nlk:turn-cancelled-condition (condition) (error condition))
                     (nle::provider-error (condition) (error condition))
                     (sb-sys:deadline-timeout ()
                       (error 'nle::provider-stream-incomplete
                              :detail (format nil "no byte for ~a s" (nle::idle-seconds-label seconds))))
                     (error (condition)
                       (error 'nle::provider-stream-incomplete :detail (nle::stream-cut-detail condition))))))
        (unless frame (return (values stop-reason stopped)))
        (multiple-value-bind (headers payload) (decode-message frame)
          (flet ((header (name) (cdr (assoc name headers :test #'equal))))
            (let ((message-type (header ":message-type"))
                  (event (ignore-errors (if (zerop (length payload))
                                            (make-hash-table :test 'equal)
                                            (nlk:decode-json (sb-ext:octets-to-string payload :external-format :utf-8))))))
              (cond
                ((equal message-type "exception")
                 (let ((type (or (header ":exception-type") "Exception")))
                   (nle::emit-stream-part (nle::lane-assembly-on-part asm) :error :payload event)
                   (error 'nle::provider-error
                          :status (exception-status type)
                          :detail (format nil "~a: ~a" type
                                          (or (nlk:json-value event :string "message")
                                              (sb-ext:octets-to-string payload :external-format '(:utf-8 :replacement #\?)))))))
                ((equal message-type "error")
                 (let ((code (or (header ":error-code") "UnknownError")))
                   (error 'nle::provider-error
                          :status (exception-status code)
                          :detail (format nil "~a: ~a" code
                                          (or (header ":error-message")
                                              (sb-ext:octets-to-string payload :external-format '(:utf-8 :replacement #\?)))))))
                ((and (equal message-type "event") (hash-table-p event))
                 (let ((type (header ":event-type"))
                       (index (or (nlk:json-value event :integer "contentBlockIndex") 0)))
                   (cond
                     ((equal type "messageStart")
                      (unless (equal (nlk:json-value event :string "role") "assistant")
                        (error 'nle::provider-error :status 400
                                                    :detail "Unexpected assistant message start but got user message start instead")))
                     ((equal type "contentBlockStart")
                      (alexandria:when-let (tool (nlk:json-value event :object "start" "toolUse"))
                        (if (and sentinel-p (equal (nlk:json-value tool :string "name") +no-tools-sentinel+))
                            (push index ignored)
                            (progn (setf (gethash index kinds) :tool)
                                   (nle::open-tool-buffer asm index :id (or (nlk:json-value tool :string "toolUseId") "")
                                                                    :name (or (nlk:json-value tool :string "name") ""))))))
                     ((equal type "contentBlockDelta")
                      (let ((delta (nlk:json-value event :object "delta")))
                        (cond ((member index ignored))
                              ((nlk:json-value delta :string "text")
                               (setf (gethash index kinds) (or (gethash index kinds) :text))
                               (nle::assembly-text-delta asm (format nil "bedrock_text_~d" index)
                                                         (nlk:json-value delta :string "text")))
                              ((nlk:json-value delta :object "toolUse")
                               (nle::assembly-tool-fragment asm index (or (nlk:json-value delta :string "toolUse" "input") "")))
                              ((nlk:json-value delta :object "reasoningContent")
                               (setf (gethash index kinds) :reasoning)
                               (alexandria:when-let (text (nlk:json-value delta :string "reasoningContent" "text"))
                                 (nle::assembly-reasoning-delta asm (format nil "bedrock_reasoning_~d" index) text))
                               (alexandria:when-let (signature (nlk:json-value delta :string "reasoningContent" "signature"))
                                 (nle::assembly-signature-delta asm signature))))))
                     ((equal type "contentBlockStop")
                      (case (gethash index kinds)
                        (:text (nle::lifecycle-close-text asm))
                        (:reasoning (nle::lifecycle-close-reasoning asm))))
                     ((equal type "messageStop")
                      (setf stop-reason (nlk:json-value event :string "stopReason") stopped t))
                     ((equal type "metadata")
                      (apply-usage (nle::lane-assembly-usage asm) (nlk:json-value event :object "usage"))))))))))))))

(defun call-bedrock-streaming (context &key (on-part nle::*turn-part-fn*))
  "POST CONTEXT's round to Converse Stream, fold the event stream, and answer
(values MESSAGE USAGE FINISH-REASON REQUEST-JSON) in the chat wire shape."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (region (bedrock-region model))
         (seconds (nle::effective-provider-config-request-timeout config))
         (asm (nle::make-lane-assembly :on-part on-part))
         (retried nil))
    (multiple-value-bind (body sentinel-p) (converse-request context)
      (multiple-value-bind (url host path query) (request-address model region)
        (multiple-value-bind (bearer creds) (round-credential config)
          (loop
            (let* ((octets (nlk:encode-json-octets body))
                   (request-json octets)
                   (creds (if (eq creds :resolve) (aws-credentials region) creds))
                   (stream nil))
              (handler-case
                  (multiple-value-bind (body-stream status response-headers)
                      (post-converse url (request-headers bearer creds host path query octets region)
                                     octets config request-json)
                    (setf stream body-stream)
                    (unless (eql status 200)
                      (error 'nle::provider-error :status status :detail "non-200 response" :scope :request))
                    (let ((type (nlk:json-value response-headers :string "content-type")))
                      (when (and type (not (search "application/vnd.amazon.eventstream" type)))
                        (let ((text (nlk:body-text stream nle::+max-provider-evidence-bytes+)))
                          (error 'nle::provider-non-sse-response
                                 :status 200
                                 :detail (format nil "Bedrock answered the stream request with a ~a body: ~a"
                                                 type (subseq text 0 (min 400 (length text))))))))
                    (nle::emit-stream-part on-part :stream-start)
                    (multiple-value-bind (stop-reason stopped)
                        (unwind-protect
                             (handler-bind ((nle::provider-error
                                              (lambda (condition) (nle::attach-provider-evidence condition "" request-json))))
                               (fold-frames stream asm seconds sentinel-p))
                          (nle::close-provider-stream-body stream))
                      (unless stopped
                        (error 'nle::provider-stream-incomplete :detail "the stream ended before the answer finished"))
                      (return (finish-round asm on-part stop-reason sentinel-p request-json))))
                (nle::provider-error (condition)
                  (when (and stream (open-stream-p stream)) (nle::close-provider-stream-body stream))
                  (when (and (null bearer) (member (nle::provider-error-status condition) '(401 403)))
                    ;; stale credentials (rotated session keys): resolve afresh next time
                    (forget-credentials region))
                  (if (and (not retried) (prefix-binding-refusal-p condition))
                      (setf retried t
                            body (nlk:copy-json-object body "messages" (without-reasoning (gethash "messages" body))))
                      (error condition)))))))))))

(defun finish-round (asm on-part stop-reason sentinel-p request-json)
  "Close the assembly's spans, build the chat-shaped message, emit :finish,
and answer the lane's four values; a round that stopped in error fails here."
  (nle::assembly-close-spans asm :order '(:text :reasoning :tools))
  (nle::flush-thinking-tag asm)
  (let* ((content (get-output-stream-string (nle::lane-assembly-content asm)))
         (buffers (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car))
         (message (nlk:json-object
                   "role" "assistant"
                   "content" (if (string= content "") :null content)
                   :when (nle::lane-assembly-reasoning-seen-p asm) "reasoning_content"
                   (get-output-stream-string (nle::lane-assembly-reasoning asm))
                   :when (nle::lane-assembly-signature-seen-p asm) "reasoning_signature"
                   (get-output-stream-string (nle::lane-assembly-signature asm))
                   :when buffers "tool_calls"
                   (map 'vector (lambda (pair &aux (buffer (cdr pair)))
                                  (nle::chat-tool-call-object (or (getf buffer :id) "") (getf buffer :name)
                                                              (getf buffer :arguments)))
                        buffers)))
         (usage (and (not (equalp (nle::lane-assembly-usage asm) (nle::make-provider-usage)))
                     (nle::lane-assembly-usage asm)))
         (finish (finish-reason stop-reason sentinel-p)))
    (nle::emit-stream-part on-part :finish)
    (unless finish
      (error 'nle::provider-error :status 400 :detail (stop-refusal stop-reason)
                                  :evidence-request-body (nle::bounded-evidence request-json)))
    (values message usage finish request-json)))

(defun make-lane ()
  "The Bedrock lane: Converse Stream under this provider's name and a family
of its own, so no other family's credential hook mistakes it for its own."
  (nle::make-provider-lane :name +lane+
                           :stream-symbol 'call-bedrock-streaming
                           :family :amazon-bedrock
                           ;; a Claude thought must be signed to replay, as on the Messages wire
                           :reasoning-carry :text
                           :default-endpoint "https://bedrock-runtime.us-east-1.amazonaws.com"
                           :path ""
                           :npm "nodecode-amazon-bedrock"))
