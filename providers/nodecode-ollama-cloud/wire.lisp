;;;; wire.lisp --- Ollama's /api/chat: the request, the NDJSON stream, the lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi's packages/ai/src/providers/ollama.ts (createChatBody,
;;;; convertMessages, mapReasoning, mapToolChoice, streamOllamaOnce,
;;;; mapDoneReason) and the Ollama arm of utils/schema/normalize.ts
;;;; (sanitizeSchemaForOllama).
;;;;
;;;; The request is one JSON object:
;;;;
;;;;   {"model", "messages": [{role, content, images?, tool_calls?, tool_name?}],
;;;;    "tools"?, "think"?, "tool_choice"?, "options"?: {temperature, top_p},
;;;;    "stream": true}
;;;;
;;;; and the answer one JSON object per line (NDJSON, not SSE), each a chunk
;;;; {"message": {"content", "thinking", "tool_calls"}, "done", "done_reason",
;;;; "prompt_eval_count", "prompt_eval_cached_count", "eval_count"}. A tool
;;;; call arrives whole, its arguments an object; the last chunk says done.
;;;; The core's SSE walk refuses a non-SSE body, so the lane walks the lines
;;;; itself, under the same idle deadline and cancellation the core keeps.

(in-package #:nodecode-ollama-cloud)

;;; --- the request ----------------------------------------------------------------

(defparameter +image-omitted+ "[image omitted: model does not support vision]"
  "What stands where an image was for a model that takes none (vision-guard.ts).")

(defun plain-content (content images-p)
  "(values TEXT IMAGES) of wire CONTENT as Ollama carries it: the text parts
joined by newlines, each data: image's base64 when IMAGES-P (toPlainContent);
an image the model cannot take is the placeholder, after the text."
  (if (stringp content)
      (values content nil)
      (let ((texts '()) (images '()) (omitted nil))
        (dolist (part (nle::message-content-parts content))
          (let ((type (nle::content-part-type part)))
            (cond ((equal type "text")
                   (push (or (nlk:json-value part :string "text") "") texts))
                  ((equal type "image_url")
                   (let ((data (nth-value 1 (nle::parse-data-uri
                                             (nlk:json-value part :string "image_url" "url")))))
                     ;; Ollama takes images as base64 only: a fetchable url is not one
                     (if (and images-p data) (push data images) (setf omitted t)))))))
        (let ((text (format nil "~{~a~^~%~}" (nreverse texts))))
          (values (if omitted
                      (if (plusp (length text)) (format nil "~a~%~a" text +image-omitted+) +image-omitted+)
                      text)
                  (and images (coerce (nreverse images) 'vector)))))))

(defun call-arguments (call)
  "The arguments of chat-shaped tool CALL as the object Ollama wants."
  (nth-value 1 (nle::tool-call-function-input call)))

(defun wire-message (message images-p)
  "One history MESSAGE as Ollama's chat carries it (convertMessage), or NIL."
  (let ((role (nlk:json-value message :string "role"))
        (content (gethash "content" message)))
    (flet ((plain (role &rest more)
             (multiple-value-bind (text images) (plain-content content images-p)
               (apply #'nlk:make-json-object
                      "role" role
                      (append more (list "content" text)
                              (and images (list "images" images)))))))
      (cond ((equal role "user") (plain "user"))
            ;; a history system message is the harness's own (an eviction
            ;; stub, a reminder): an agent-attributed developer turn, which
            ;; keeps Ollama's system role
            ((equal role "system") (plain "system"))
            ((equal role "tool")
             (plain "tool" "tool_name" (or (nlk:json-value message :string "name") "")))
            ((equal role "assistant")
             ;; Ollama Cloud refuses history whose assistant turns carry
             ;; `thinking' (HTTP 400), so the reasoning never goes back
             (let ((calls (nlk:json-array message "tool_calls")))
               (nlk:json-object
                "role" "assistant"
                "content" (nle::content-text content)
                :when (plusp (length calls)) "tool_calls"
                (map 'vector (lambda (call)
                               (nlk:json-object
                                "type" "function"
                                "function" (nlk:json-object
                                            "name" (or (nlk:json-value call :string "function" "name") "")
                                            "arguments" (call-arguments call))))
                     calls))))))))

(defun wire-messages (context model)
  "CONTEXT's system prompt and history as Ollama messages (convertMessages)."
  (let* ((system (nle::compiled-turn-context-system-prompt context))
         (prefix (if (plusp (length system)) 1 0))
         (images-p (model-images-p model))
         (messages (concatenate
                    'vector
                    (and (plusp prefix) (vector (nlk:json-object "role" "system" "content" system)))
                    (remove nil (map 'vector (lambda (message) (wire-message message images-p))
                                     (nle::request-messages context))))))
    ;; Ollama answers done_reason `load' and generates nothing when a request
    ;; has no user turn: the last system turn past the prompt's own becomes one
    (unless (find "user" messages :key (lambda (message) (gethash "role" message)) :test #'equal)
      (loop for index from (1- (length messages)) downto prefix
            when (equal "system" (gethash "role" (aref messages index)))
              do (setf (gethash "role" (aref messages index)) "user")
                 (return)))
    messages))

;;; omp's OPEN_SUBSCHEMA_WIDENING and the key sets sanitizeSchemaForOllama walks.
(defparameter +open-subschema+
  '("string" "number" "boolean" "object" "array" "null")
  "The primitive types an open (`true') subschema widens to.")

(defparameter +subschema-map-keys+
  '("properties" "patternProperties" "dependencies" "dependentSchemas" "$defs" "definitions"))

(defparameter +subschema-array-keys+ '("anyOf" "oneOf" "allOf" "prefixItems"))

(defparameter +schema-value-keys+
  '("items" "additionalItems" "contains" "contentSchema" "propertyNames" "if" "then" "else"
    "not" "additionalProperties" "unevaluatedItems" "unevaluatedProperties"))

(defun open-subschema ()
  "Every JSON value: what a bare `true' subschema says."
  (nlk:json-object "anyOf" (map 'vector (lambda (type) (nlk:json-object "type" type)) +open-subschema+)))

(defun sanitize-schema (node)
  "NODE, a JSON Schema, in the forms Ollama's Go tool parser can unmarshal
(sanitizeSchemaForOllama): a boolean subschema widened to a union of every
primitive type, a boolean additional/unevaluatedProperties dropped, a type
list folded to one type or an anyOf under allOf."
  (cond ((eq node t) (open-subschema))
        ;; JSON false decodes to NIL: a `false' subschema accepts nothing
        ((null node) (nlk:json-object "not" (open-subschema)))
        ((and (vectorp node) (not (stringp node))) (map 'vector #'sanitize-schema node))
        ((not (hash-table-p node)) node)
        (t
         (let ((output (make-hash-table :test 'equal)) (alternatives nil))
           (maphash
            (lambda (key child)
              (cond ((and (member key '("additionalProperties" "unevaluatedProperties") :test #'equal)
                          (or (eq child t) (null child))))
                    ((and (equal key "type") (vectorp child) (not (stringp child)))
                     (let* ((variants (remove-duplicates (remove-if-not #'stringp (coerce child 'list))
                                                         :test #'equal :from-end t))
                            (non-null (remove "null" variants :test #'equal)))
                       (if (<= (length non-null) 1)
                           (setf (gethash "type" output) (or (first non-null) (first variants)
                                                            (and (plusp (length child)) (aref child 0))))
                           (setf alternatives (mapcar (lambda (type) (nlk:json-object "type" type)) variants)))))
                    ((and (member key +subschema-map-keys+ :test #'equal) (hash-table-p child))
                     (let ((map (make-hash-table :test 'equal)))
                       (maphash (lambda (name schema) (setf (gethash name map) (sanitize-schema schema))) child)
                       (setf (gethash key output) map)))
                    ((and (member key +subschema-array-keys+ :test #'equal) (vectorp child) (not (stringp child)))
                     (setf (gethash key output) (map 'vector #'sanitize-schema child)))
                    ((member key +schema-value-keys+ :test #'equal)
                     (setf (gethash key output) (sanitize-schema child)))
                    (t (setf (gethash key output) child))))
            node)
           (when alternatives
             (let ((union (nlk:json-object "anyOf" (coerce alternatives 'vector)))
                   (existing (nlk:json-value output :array "allOf")))
               (setf (gethash "allOf" output)
                     (if existing (concatenate 'vector (vector union) existing) (vector union)))))
           output))))

(defun wire-tools (tools)
  "The round's chat tool wrappers as Ollama's function tools, their
parameters sanitized (convertTools), or NIL for none."
  (when (plusp (length tools))
    (map 'vector
         (lambda (wrapper)
           (let ((fn (nlk:json-value wrapper :object "function")))
             (nlk:json-object
              "type" "function"
              "function" (nlk:json-object
                          "name" (or (nlk:json-value fn :string "name") "")
                          "description" (or (nlk:json-value fn :string "description") "")
                          "parameters" (sanitize-schema
                                        (or (nlk:json-value fn :object "parameters")
                                            (nlk:json-object "type" "object")))))))
         tools)))

(defun think-value (effort reasoning-p)
  "The `think' value EFFORT asks of a model (mapReasoning): :FALSE for a
thinking model asked to think not at all, low, medium, high or max, or NIL
to leave the key off and let the model's default stand."
  (cond ((null effort) nil)
        ((string-equal effort "off") (and reasoning-p :false))
        (t (cdr (assoc effort '(("minimal" . "low") ("low" . "low") ("medium" . "medium")
                                ("high" . "high") ("xhigh" . "high") ("max" . "max"))
                       :test #'string-equal)))))

(defun tool-choice-value (choice)
  "Ollama's tool_choice for CHOICE (mapToolChoice): none, required, or NIL
for auto and anything else."
  (cond ((null choice) nil)
        ((string-equal choice "none") "none")
        ((member choice '("required" "any") :test #'string-equal) "required")))

(defun chat-body (context)
  "Ollama's /api/chat request for the compiled CONTEXT (createChatBody)."
  ;; num_predict is never sent: omp marks every Ollama Cloud model
  ;; omitMaxOutputTokens (the cloud refuses a ceiling over its own, and its
  ;; models' advertised limits are not the served ones), so the output
  ;; ceiling is the endpoint's.
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (think (think-value (nle::effective-provider-config-reasoning-effort config)
                             (model-reasoning-p model)))
         (tools (wire-tools (nle::compiled-turn-context-tools context)))
         (choice (tool-choice-value (nle::effective-provider-config-tool-choice config)))
         (temperature (nle::effective-provider-config-temperature config))
         (top-p (nle::effective-provider-config-top-p config)))
    (nlk:json-object
     "model" model
     "messages" (wire-messages context model)
     :opt "tools" tools
     :when think "think" (if (eq think :false) nil think)
     :opt "tool_choice" choice
     :when (or temperature top-p) "options" (nlk:json-object :opt "temperature" temperature
                                                             :opt "top_p" top-p)
     "stream" t)))

;;; --- the stream -------------------------------------------------------------------

(defparameter +empty-length-detail+
  "Model returned no content: prompt filled the context window; raise Ollama num_ctx or shorten the prompt."
  "omp's EMPTY_OLLAMA_LENGTH_COMPLETION_MESSAGE: a length finish with nothing in it.")

(defparameter +load-detail+
  "Ollama loaded the model but generated nothing (done_reason: load): the request contained no user-role message."
  "omp's EMPTY_OLLAMA_LOAD_COMPLETION_MESSAGE.")

(defstruct (fold (:copier nil))
  "One round's NDJSON fold: the core's assembly, and what omp's stream tracks
beside it."
  asm
  ;; how many content blocks omp's message holds so far: the index a tool
  ;; call's id names (`ollama:<index>:<name>')
  (blocks 0)
  ;; the kind of block open, :thinking or :text, or NIL
  (open nil)
  (tools 0)
  ;; whether any text or thinking that is not blank arrived
  said-p
  done-reason
  done-p)

(defun blank-p (text)
  "Whether TEXT is whitespace alone."
  (not (find-if-not (lambda (char) (member char '(#\Space #\Tab #\Newline #\Return))) text)))

(defun fold-thinking (fold text)
  (unless (blank-p text) (setf (fold-said-p fold) t))
  (when (plusp (length text))
    (unless (eq (fold-open fold) :thinking)
      (incf (fold-blocks fold))
      (setf (fold-open fold) :thinking))
    (nle::assembly-reasoning-delta (fold-asm fold) "reasoning-0" text :close-text t)))

(defun fold-text (fold text)
  (unless (blank-p text) (setf (fold-said-p fold) t))
  (when (plusp (length text))
    (unless (eq (fold-open fold) :text)
      (incf (fold-blocks fold))
      (setf (fold-open fold) :text))
    (nle::assembly-text-delta (fold-asm fold) "txt-0" text :close-reasoning t)))

(defun fold-tool-calls (fold calls)
  "Each of CALLS, whole as Ollama sends them, as one buffered call."
  (let ((asm (fold-asm fold)))
    (nle::lifecycle-close-reasoning asm)
    (nle::lifecycle-close-text asm)
    (setf (fold-open fold) nil)
    (loop for call across calls
          for name = (or (nlk:json-value call :text "function" "name") "unknown_tool")
          for raw = (nlk:json-value call :any "function" "arguments")
          for arguments = (cond ((stringp raw) raw)
                                ((hash-table-p raw) (nlk:encode-json-object raw))
                                (t "{}"))
          do (nle::open-tool-buffer asm (fold-tools fold)
                                    :id (format nil "ollama:~d:~a" (fold-blocks fold) name)
                                    :name name :arguments arguments)
             (incf (fold-tools fold))
             (incf (fold-blocks fold)))))

(defun fold-usage (fold chunk)
  "The done CHUNK's counts on the assembly's usage: the cached part of the
prompt apart from the rest, as omp maps them."
  (let ((usage (nle::lane-assembly-usage (fold-asm fold)))
        (prompt (nlk:json-value chunk :integer "prompt_eval_count"))
        (cached (nlk:json-value chunk :integer "prompt_eval_cached_count"))
        (output (nlk:json-value chunk :integer "eval_count")))
    (when (or prompt cached output)
      (setf (nle::provider-usage-input-tokens usage) (- (or prompt 0) (or cached 0))
            (nle::provider-usage-output-tokens usage) (or output 0)
            (nle::provider-usage-total-tokens usage) (+ (or prompt 0) (or output 0)))
      (when cached
        (setf (nle::provider-usage-cached-input-tokens usage) cached)))))

(defun fold-chunk (fold chunk)
  "One NDJSON CHUNK into FOLD (the body of omp's readJsonl loop)."
  (alexandria:when-let (why (nlk:json-value chunk :any "error"))
    ;; Ollama ends a failed stream with {"error": ...} on a 200
    (error 'nle::provider-error
           :detail (format nil "in-stream provider error: ~a"
                           (if (stringp why) why (nlk:encode-json-object why)))))
  (nle::assembly-response-metadata (fold-asm fold) chunk)
  (fold-thinking fold (or (nlk:json-value chunk :string "message" "thinking") ""))
  (fold-text fold (or (nlk:json-value chunk :string "message" "content") ""))
  (let ((calls (nlk:json-array chunk "message" "tool_calls")))
    (when (plusp (length calls))
      (fold-tool-calls fold calls)))
  (when (nlk:json-value chunk :boolean "done")
    (setf (fold-done-p fold) t
          (fold-done-reason fold) (nlk:json-value chunk :string "done_reason"))
    (fold-usage fold chunk)))

(defun finish-reason (fold)
  "The round's finish reason (mapDoneReason), or :LOAD for Ollama's empty load."
  (let ((reason (fold-done-reason fold))
        (calls (plusp (fold-tools fold))))
    (cond ((equal reason "length") "length")
          ((equal reason "load") :load)
          ;; tool calls always mean `run and continue', whatever stop says
          (calls "tool_calls")
          ((equal reason "tool_calls") "tool_calls")
          (t "stop"))))

(defun visible-p (fold)
  "Whether the round said anything: text or thinking that is not blank, or a call."
  (or (fold-said-p fold) (plusp (fold-tools fold))))

(defun chat-url (config)
  "The /api/chat address a round on CONFIG posts to."
  (format nil "~a/api/chat" (normalized-base (nle::effective-provider-config-endpoint config))))

(defun post-chat (url key request-json config)
  "POST REQUEST-JSON (octets) to URL with bearer KEY: (values STREAM STATUS),
STREAM the response body as octets, under the config's idle deadline."
  (let ((seconds (nle::effective-provider-config-request-timeout config))
        (proxy (or (nle::effective-provider-config-proxy config) dex:*default-proxy*)))
    (nle::with-idle-cut (seconds)
        (sb-sys:with-deadline (:seconds seconds)
          (nle::cancellable-post (request-json (or proxy url))
            (dex:post url
                      :headers (nle::with-user-agent
                                `(("content-type" . "application/json")
                                  ("authorization" . ,(format nil "Bearer ~a" key))))
                      :content request-json
                      :read-timeout (* 4 seconds)
                      :connect-timeout seconds
                      :proxy proxy
                      :want-stream t
                      :force-binary t)))
      (error 'nle::provider-error :scope :request
                                  :detail (format nil "no response for ~a s" (nle::idle-seconds-label seconds))
                                  :evidence-request-body (nle::bounded-evidence request-json)))))

(defun walk-lines (stream config on-line)
  "Call ON-LINE with each non-blank line of STREAM, under the idle deadline
and the turn's cancellation, until EOF. => the evidence kept, bounded."
  (let* ((seconds (nle::effective-provider-config-request-timeout config))
         (buffer (and (nle::octet-stream-p stream)
                      (make-array 512 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
         (evidence (make-string-output-stream))
         (kept 0))
    (nle::with-idle-cut (seconds)
        (loop
          (when nle::*current-durable-turn*
            (nlk:ensure-turn-not-cancelled nle::*current-durable-turn*))
          (let ((line (handler-case (nlk:with-cancellable-wait (nle::*current-durable-turn*)
                                      (if buffer
                                          (nle::read-octet-line stream buffer seconds)
                                          (sb-sys:with-deadline (:seconds seconds)
                                            (read-line stream nil nil))))
                        (nlk:turn-cancelled-condition (c) (error c))
                        (error (e)
                          (error 'nle::provider-stream-incomplete :detail (nle::stream-cut-detail e))))))
            (unless line (return))
            (let ((room (- nle::+max-provider-evidence-bytes+ kept)))
              (when (plusp room)
                (write-line line evidence :end (min room (length line)))
                (incf kept (1+ (length line)))))
            (when (find-if-not (lambda (char) (member char '(#\Space #\Tab #\Return))) line)
              (funcall on-line line))))
      (error 'nle::provider-stream-incomplete
             :detail (format nil "no byte for ~a s" (nle::idle-seconds-label seconds))))
    (get-output-stream-string evidence)))

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The lane's stream: one POST /api/chat, its NDJSON folded into the
chat-shaped assistant message. => (values MESSAGE USAGE FINISH-REASON REQUEST-JSON)"
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (key (nle::effective-provider-config-api-key config))
         (body (chat-body context))
         (request-json (sb-ext:string-to-octets (nlk:encode-json-object body) :external-format :utf-8))
         (asm (nle::make-lane-assembly :on-part on-part))
         (fold (make-fold :asm asm))
         (evidence ""))
    (when (or (null key) (equal key "public") (zerop (length key)))
      (error 'nle::provider-config-error
             :status 401
             :detail (format nil "Ollama Cloud needs a key: make one at ~a, save it with /connect or set OLLAMA_CLOUD_API_KEY"
                             +key-page+)))
    (let ((stream (post-chat (chat-url config) key request-json config)))
      (nle::emit-stream-part on-part :stream-start)
      (unwind-protect
           (handler-bind ((nle::provider-error
                            (lambda (condition)
                              (nle::attach-provider-evidence condition evidence request-json))))
             (setf evidence
                   (walk-lines stream config
                               (lambda (line)
                                 (let ((chunk (ignore-errors (nlk:decode-json line))))
                                   (when (hash-table-p chunk)
                                     (fold-chunk fold chunk))))))
             (unless (fold-done-p fold)
               (error 'nle::provider-stream-incomplete
                      :detail "the stream ended before the answer finished")))
        (nle::close-provider-stream-body stream)))
    (nle::assembly-close-spans asm :order '(:tools :reasoning :text))
    (nle::flush-thinking-tag asm)
    (let ((finish (finish-reason fold)))
      (flet ((fail-round (detail)
               (error 'nle::provider-error :status 400 :detail detail :scope :contract
                                           :evidence-body (nle::bounded-evidence evidence)
                                           :evidence-request-body (nle::bounded-evidence request-json))))
        (cond ((eq finish :load) (fail-round +load-detail+))
              ((and (equal finish "length") (not (visible-p fold))) (fail-round +empty-length-detail+))))
      (let ((message
              (nlk:json-object
               "role" "assistant"
               "content" (let ((full (get-output-stream-string (nle::lane-assembly-content asm))))
                           (if (string= full "") :null full))
               :when (nle::lane-assembly-reasoning-seen-p asm) "reasoning_content"
               (get-output-stream-string (nle::lane-assembly-reasoning asm))
               :when (nle::lane-assembly-tool-buffers asm) "tool_calls"
               (map 'vector
                    (lambda (pair &aux (buf (cdr pair)))
                      (nle::chat-tool-call-object (or (getf buf :id) "") (getf buf :name) (getf buf :arguments)))
                    (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car))))
            (usage (and (not (equalp (nle::lane-assembly-usage asm) (nle::make-provider-usage)))
                        (nle::lane-assembly-usage asm))))
        (nle::emit-stream-part on-part :finish)
        (values message usage finish request-json)))))

;;; --- the listing --------------------------------------------------------------------

(defun window-of (model-info)
  "The context window /api/show's MODEL-INFO names: a key ending
.context_length, .num_ctx or .context_window (getContextWindow)."
  (when (hash-table-p model-info)
    (loop for key being the hash-keys of model-info using (hash-value value)
          when (and (integerp value)
                    (some (lambda (suffix) (uiop:string-suffix-p key suffix))
                          '(".context_length" ".num_ctx" ".context_window")))
            return value)))

(defun show-window (base headers id)
  "The window POST <BASE>/api/show answers for model ID, or NIL."
  (multiple-value-bind (body status)
      (nle::http-fetch (format nil "~a/api/show" base)
                       :content (nlk:encode-json-object (nlk:json-object "model" id))
                       :headers (cons '("content-type" . "application/json") headers)
                       :timeout 10)
    (and body (integerp status)
         (window-of (nlk:json-value (ignore-errors (nlk:decode-json body)) :object "model_info")))))

(defun list-models (base key)
  "(values ROWS ERROR): the models GET <BASE>/api/tags lists for KEY
(ollamaCloudModelManagerOptions): each one's id, its name when it differs,
and its window -- the bundled row's, else the one /api/show names, else
omp's 128000."
  (let ((base (normalized-base base))
        (headers `(("accept" . "application/json")
                   ("authorization" . ,(format nil "Bearer ~a" key)))))
    (multiple-value-bind (body status) (nle::http-fetch (format nil "~a/api/tags" base) :headers headers)
      (if (null body)
          (values nil status)
          (let ((parsed (ignore-errors (nlk:decode-json body)))
                (rows '()))
            (if (not (hash-table-p parsed))
                (values nil "unparseable listing")
                (progn
                  (loop for entry across (nlk:json-array parsed "models")
                        for id = (or (nlk:json-value entry :text "model") (nlk:json-value entry :text "name"))
                        for name = (nlk:json-value entry :text "name")
                        for row = (and id (model-row id))
                        when (and id (not (find id rows :key (lambda (row) (getf row :id)) :test #'equal)))
                          do (push (list :id id
                                         :display (if (and name (string/= name id))
                                                      name
                                                      (or (nlk:json-value row :text "name") id))
                                         :context-window (or (nlk:json-value row :integer "context")
                                                             (show-window base headers id)
                                                             128000))
                                   rows))
                  (values (sort rows #'string< :key (lambda (row) (getf row :id))) nil))))))))
