;;;; wire.lisp --- each Factory wire's body, as the Factory CLI sends it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/ai/src/providers/
;;;; factory-droid.ts (the four request encoders), factory-droid/gemini.ts
;;;; (the Gemini wire), the request rules of packages/catalog/src/compat/
;;;; rules/providers/factory-droid.kdl (each route's reasoning dialect,
;;;; thinking style and betas), utils/schema/allowlist.ts (the schema shape
;;;; Factory's Gemini takes), and anthropic.ts's shouldStripThinkingHistory.
;;;; omp checks these against the Factory CLI's own recorded traffic
;;;; (packages/ai/test/fixtures/factory-droid-native-requests.json).
;;;;
;;;; Each function takes the body the core's lane built for the round and
;;;; the round's facts (provider.lisp NOTE-ROUND) and sets it the way the CLI
;;;; sends it: the identity line ahead of the system prompt, the model's own
;;;; output ceiling, and each route's way of asking for reasoning.

(in-package #:nodecode-factory-droid)

(defun prefix-p (prefixes model)
  "Whether MODEL starts with one of PREFIXES."
  (some (lambda (prefix) (uiop:string-prefix-p prefix model)) prefixes))

;;; --- chat completions ----------------------------------------------------------

(defun completions-dialect (model upstream)
  "(values MODE HISTORY): how MODEL served by UPSTREAM asks for reasoning on
the chat wire (none, effort, forced-on, opt-in) and what reasoning_history it
names (NIL for none)."
  ;; The registry's completions-reasoning-mode and -history, per upstream:
  ;; Fireworks reasons on an effort and keeps the history, Baseten and
  ;; Databricks reason on an effort alone, and the open families send nothing
  ;; elsewhere. A model off these families takes the generic effort field.
  (if (not (or (prefix-p '("glm-" "kimi-" "deepseek-" "nemotron-") model)
               (member model '("inkling" "mistral-medium-3.5" "qwen3.8-max" "minimax-m3") :test #'equal)))
      (values "effort" nil)
      (let ((mode "none") (history nil))
        (cond ((equal upstream "fireworks") (setf mode "effort" history "preserved"))
              ((member upstream '("baseten" "databricks") :test #'equal) (setf mode "effort")))
        (when (and (equal model "minimax-m3") (equal upstream "fireworks"))
          (setf history nil))
        (when (and (equal model "nemotron-3-ultra") (equal upstream "baseten"))
          (setf mode "opt-in"))
        (when (uiop:string-prefix-p "deepseek-" model)
          (cond ((equal upstream "baseten") (setf mode "forced-on"))
                ((equal upstream "fireworks") (setf history "interleaved"))))
        (when (and (equal upstream "mistral")
                   (or (uiop:string-prefix-p "glm-" model) (equal model "mistral-medium-3.5")))
          (setf mode "effort" history nil))
        (values mode history))))

(defun chat-body (body facts config)
  "BODY, a chat completions request, as the CLI sends it."
  (let ((effort (getf facts :effort))
        (row (getf facts :row)))
    (setf (gethash "messages" body)
          (concatenate 'vector (vector (nlk:json-object "role" "system" "content" +identity+))
                       (gethash "messages" body)))
    ;; the dialect owns every reasoning field; the CLI sends no cache key here
    (remhash "reasoning_effort" body)
    (remhash "prompt_cache_key" body)
    (when row
      (setf (gethash "max_tokens" body) (output-limit row (getf facts :region))))
    (setf (gethash "temperature" body) (or (nle::effective-provider-config-temperature config) 1))
    (multiple-value-bind (mode history) (completions-dialect (getf facts :model) (getf facts :upstream))
      (cond ((equal mode "effort")
             (setf (gethash "reasoning_effort" body) (or effort "none")))
            ((equal mode "forced-on")
             ;; the route cannot turn thinking off: off is its lowest rung
             (setf (gethash "reasoning_effort" body) (or effort "low")))
            ((equal mode "opt-in")
             (setf (gethash "chat_template_args" body)
                   (nlk:json-object "enable_thinking" (if effort t :false)))))
      (when (and effort history)
        (setf (gethash "reasoning_history" body) history))))
  body)

;;; --- Responses ----------------------------------------------------------------

(defun responses-body (body facts)
  "BODY, a Responses request, as the CLI sends it: GPT models with the
OpenAI SDK's defaults, Grok models with xAI's dialect."
  (let* ((model (getf facts :model))
         (gpt (uiop:string-prefix-p "gpt-" model))
         (grok (uiop:string-prefix-p "grok-" model))
         (upstream (getf facts :upstream))
         (session (getf facts :session))
         (effort (getf facts :effort))
         (tools (nlk:json-value body :array "tools")))
    (setf (gethash "instructions" body)
          (format nil "~a~%~%~a" +identity+ (or (nlk:json-value body :string "instructions") "")))
    (remhash "temperature" body)
    (remhash "reasoning" body)
    (remhash "include" body)
    (cond (effort
           (setf (gethash "reasoning" body)
                 (nlk:json-object "effort" effort :when (not grok) "summary" "auto")
                 (gethash "include" body) (vector "reasoning.encrypted_content")))
          ;; the two upstreams whose off is the none rung rather than silence
          ((member upstream '("bedrock_openai" "snowflake") :test #'equal)
           (setf (gethash "reasoning" body) (nlk:json-object "effort" "none"))))
    (when (and tools (plusp (length tools)))
      ;; OpenAI reads an absent strict as strict; the CLI declares none
      (setf (gethash "tools" body)
            (map 'vector (lambda (tool) (nlk:copy-json-object tool "strict" :false)) tools)
            (gethash "parallel_tool_calls" body) t)
      (if gpt
          (unless (nlk:json-value body :any "tool_choice")
            (setf (gethash "tool_choice" body) "auto"))
          (remhash "tool_choice" body)))
    (setf (gethash "prompt_cache_key" body) session)
    (when gpt
      (when (and (equal upstream "openai") (not (equal model "gpt-5.2")))
        (setf (gethash "prompt_cache_retention" body) "24h"))
      (setf (gethash "text" body) (nlk:json-object "verbosity" "low"))
      (unless (equal model "gpt-5.2")
        (setf (gethash "safety_identifier" body) session))
      (when (uiop:string-suffix-p model "-fast")
        (setf (gethash "service_tier" body) "priority"))
      (remhash "max_output_tokens" body))
    (when (and grok (getf facts :row))
      (setf (gethash "max_output_tokens" body) (output-limit (getf facts :row) (getf facts :region)))))
  body)

;;; --- Messages -------------------------------------------------------------------

(defparameter +budgets+ '(("low" . 4096) ("medium" . 12288) ("high" . 24576))
  "The token budget each rung buys a budget-thinking model.")

(defparameter +always-thinking+ '("claude-fable-5.1" "claude-opus-5-5" "claude-opus-5-5-fast" "claude-sonnet-5-5")
  "The models whose thinking stays on, adaptive, when a turn asks for it off.")

(defparameter +fast+ '("claude-opus-5-5-fast" "claude-opus-5-fast" "claude-opus-4-8-fast")
  "The fast tiers: distinct SKUs served with speed fast.")

(defun thinking-style (model)
  "How MODEL thinks on Factory's Messages route, or NIL for a model the
registry does not name."
  (cond ((member model '("claude-opus-4-6" "claude-sonnet-4-6") :test #'equal) "adaptive")
        ((member model '("claude-sonnet-4-5-20250929" "claude-haiku-4-5-20251001") :test #'equal)
         "budget-interleaved")
        ((member model '("claude-opus-4-5-20251101" "minimax-m2.7") :test #'equal) "budget-effort")
        ((or (uiop:string-prefix-p "claude-" model)
             (member model '("atlas-07-21" "aster-07-15") :test #'equal))
         "adaptive-summarized")))

(defun block-type (block)
  "A content block's type, or NIL."
  (nlk:json-value block :string "type"))

(defun thinking-block-p (block)
  (member (block-type block) '("thinking" "redacted_thinking") :test #'equal))

(defun real-user-p (message)
  "Whether MESSAGE is a user's turn rather than tool results riding as one."
  (and (equal "user" (nlk:json-value message :string "role"))
       (let ((content (gethash "content" message)))
         (or (stringp content)
             (some (lambda (block) (not (equal "tool_result" (block-type block))))
                   (coerce (or content #()) 'list))))))

(defun opens-with-thinking-p (message)
  "Whether MESSAGE's first content block is thinking."
  (let ((content (nlk:json-value message :array "content")))
    (and content (plusp (length content)) (thinking-block-p (aref content 0)))))

(defun strip-thinking-p (messages)
  "Whether the conversation MESSAGES has stopped being thinking-led: an
assistant turn exists and none opens with thinking, or the turn right after
the last user turn is an assistant one that does not."
  (let* ((messages (coerce messages 'list))
         (assistants (remove-if-not (lambda (message)
                                      (and (equal "assistant" (nlk:json-value message :string "role"))
                                           (plusp (length (or (nlk:json-value message :array "content") #())))))
                                    messages))
         (last-user (position-if #'real-user-p messages :from-end t))
         (next (and last-user (nth (1+ last-user) messages))))
    (or (and assistants (notany #'opens-with-thinking-p assistants))
        (and next (equal "assistant" (nlk:json-value next :string "role"))
             (plusp (length (or (nlk:json-value next :array "content") #())))
             (not (opens-with-thinking-p next))))))

(defun without-thinking (messages)
  "MESSAGES with every thinking block taken out of the assistant turns; a
turn that held nothing else keeps its blocks rather than going empty."
  (map 'vector
       (lambda (message)
         (let ((content (nlk:json-value message :array "content")))
           (if (and content (equal "assistant" (nlk:json-value message :string "role"))
                    (some #'thinking-block-p content)
                    (notevery #'thinking-block-p content))
               (nlk:copy-json-object message "content" (remove-if #'thinking-block-p content))
               message)))
       messages))

(defun messages-body (body betas facts)
  "BODY, a Messages request, as the CLI sends it. => the BETAS the request
carries."
  (let* ((model (getf facts :model))
         (style (thinking-style model))
         (effort (getf facts :effort))
         (upstream (getf facts :upstream))
         (tools (nlk:json-value body :array "tools"))
         (betas (copy-list betas)))
    (setf (gethash "system" body)
          (concatenate 'vector (vector (nlk:json-object "type" "text" "text" +identity+))
                       (let ((system (gethash "system" body)))
                         (if (stringp system)
                             (vector (nlk:json-object "type" "text" "text" system))
                             (or system #())))))
    (when (getf facts :row)
      (setf (gethash "max_tokens" body) (output-limit (getf facts :row) (getf facts :region))))
    (when style
      (remhash "thinking" body)
      (remhash "output_config" body)
      (let ((adaptive (member style '("adaptive" "adaptive-summarized") :test #'equal))
            (budget (and effort (or (cdr (assoc effort +budgets+ :test #'equal)) 1024))))
        (cond ((and effort adaptive)
               (setf (gethash "thinking" body)
                     (nlk:json-object "type" "adaptive"
                                      :when (equal style "adaptive-summarized") "display" "summarized")
                     (gethash "output_config" body) (nlk:json-object "effort" effort)))
              (effort
               (setf (gethash "thinking" body)
                     (nlk:json-object "type" "enabled" "budget_tokens" budget))
               (when (equal style "budget-effort")
                 (setf (gethash "output_config" body)
                       (nlk:json-object "effort" (if (member effort '("xhigh" "max") :test #'equal)
                                                     "high"
                                                     effort)))))
              ((member model +always-thinking+ :test #'equal)
               (setf (gethash "thinking" body)
                     (nlk:json-object "type" "adaptive" "display" "summarized")))
              ((equal upstream "snowflake")
               (setf (gethash "thinking" body) (nlk:json-object "type" "disabled"))))
        ;; budget thinking on a history no longer thinking-led: no thinking
        ;; field, and the history replays without its thinking blocks
        (let ((stripped (and effort (not adaptive)
                             (strip-thinking-p (or (nlk:json-value body :array "messages") #())))))
          (when stripped
            (remhash "thinking" body)
            (setf (gethash "messages" body) (without-thinking (gethash "messages" body))))
          (when (and (equal style "budget-interleaved") effort (not stripped))
            (pushnew "interleaved-thinking-2025-05-14" betas :test #'equal))))
      (when (member (nlk:json-value body :string "thinking" "type") '("adaptive" "enabled") :test #'equal)
        (remhash "temperature" body)
        (remhash "top_p" body))
      (when (and (nlk:json-value body :string "output_config" "effort")
                 (or (member upstream '("bedrock_anthropic" "vertex_anthropic") :test #'equal)
                     (equal model "claude-opus-4-5-20251101")))
        (pushnew "effort-2025-11-24" betas :test #'equal)))
    (when (and tools (plusp (length tools)))
      (when (member upstream '("anthropic" "vertex_anthropic" "bedrock_anthropic") :test #'equal)
        (pushnew "fine-grained-tool-streaming-2025-05-14" betas :test #'equal))
      (when (member upstream '("anthropic" "vertex_anthropic") :test #'equal)
        (setf (gethash "tools" body)
              (map 'vector (lambda (tool) (nlk:copy-json-object tool "eager_input_streaming" t)) tools))))
    (when (member model +fast+ :test #'equal)
      (setf (gethash "speed" body) "fast")
      (pushnew "fast-mode-2026-02-01" betas :test #'equal))
    betas))

;;; --- Gemini ---------------------------------------------------------------------

(defparameter +skip-signature+ "skip_thought_signature_validator"
  "What an unsigned function call in the newest turn carries, so Gemini's
signature check lets it through.")

(defparameter +medium-level-models+ '("gemini-3.1-pro-preview" "gemini-3.7-flash" "gemini-3.8-flash")
  "The Gemini models that have a MEDIUM thinking level; the others hear medium as HIGH.")

(defun thinking-level (model effort)
  "EFFORT as MODEL's Gemini thinkingLevel."
  (cond ((member effort '("low" "minimal") :test #'equal) "LOW")
        ((equal effort "medium")
         (if (member model +medium-level-models+ :test #'equal) "MEDIUM" "HIGH"))
        (t "HIGH")))

(defun wire-tool-name (name)
  "NAME as the CLI sends a tool name: [a-zA-Z0-9_-] only, and past 64
characters cut there with an _ and eight hex digits of its SHA-256."
  (let ((clean (cl-ppcre:regex-replace-all "[^a-zA-Z0-9_-]" name "_")))
    (if (<= (length clean) 64)
        clean
        (format nil "~a_~a" (subseq clean 0 64) (subseq (nlk::sha256-text clean) 7 15)))))

(defparameter +schema-keys+
  '("type" "title" "description" "required" "format" "minimum" "maximum" "minLength" "maxLength"
    "pattern" "minItems" "maxItems" "default" "example")
  "The schema keywords Factory's Gemini takes as they are.")

(defun present-p (key node)
  (nth-value 1 (gethash key node)))

(defun stringified (value)
  "VALUE as an enum entry: a string as it is, anything else as its JSON."
  (if (stringp value) value (shasht:write-json value nil)))

(defun null-type-p (node)
  "Whether NODE's type is, or includes, null."
  (let ((type (gethash "type" node)))
    (or (equal type "null") (and (vectorp type) (not (stringp type)) (find "null" type :test #'equal)))))

(defun merge-schemas (left right required)
  "LEFT and RIGHT as one schema; REQUIRED :UNION for a conjunction (every
side's keys stay required), :INTERSECTION for alternatives (only the keys
every branch requires)."
  (let ((merged (nlk:copy-json-object left)))
    (maphash (lambda (key value)
               (cond ((equal key "properties")
                      (let ((properties (nlk:copy-json-object (nlk:json-value merged :object "properties"))))
                        (when (hash-table-p value)
                          (maphash (lambda (name schema)
                                     (let ((mine (gethash name properties)))
                                       (setf (gethash name properties)
                                             (if (and (hash-table-p mine) (hash-table-p schema))
                                                 (merge-schemas mine schema required)
                                                 schema))))
                                   value))
                        (setf (gethash "properties" merged) properties)))
                     ((equal key "required"))
                     ((and (equal key "enum") (vectorp (gethash "enum" left)) (vectorp value))
                      (setf (gethash "enum" merged)
                            (remove-duplicates (concatenate 'vector (gethash "enum" left) value)
                                               :test #'equal :from-end t)))
                     ((not (present-p key merged))
                      (setf (gethash key merged) value))))
             right)
    (let ((left-required (coerce (or (nlk:json-value left :array "required") #()) 'list))
          (right-required (coerce (or (nlk:json-value right :array "required") #()) 'list)))
      (if (eq required :union)
          (when (or (nlk:json-value left :array "required") (nlk:json-value right :array "required"))
            (setf (gethash "required" merged)
                  (coerce (remove-duplicates (append left-required right-required)
                                             :test #'equal :from-end t)
                          'vector)))
          (let ((shared (remove-duplicates (intersection left-required right-required :test #'equal)
                                           :test #'equal)))
            (if shared
                (setf (gethash "required" merged) (coerce shared 'vector))
                (remhash "required" merged)))))
    merged))

(defun gemini-schema (node)
  "NODE, a JSON Schema, projected onto the shape Factory's Gemini takes:
the allowed keywords, const and enum as strings, unions and allOf merged
into one object, a single type, and a type inferred where none was given."
  (when (hash-table-p node)
    (let ((out (make-hash-table :test 'equal)))
      (dolist (key +schema-keys+)
        (when (present-p key node)
          (setf (gethash key out) (gethash key node))))
      (when (present-p "const" node)
        (setf (gethash "enum" out) (vector (stringified (gethash "const" node)))))
      (when (vectorp (nlk:json-value node :array "enum"))
        (setf (gethash "enum" out) (map 'vector #'stringified (gethash "enum" node))))
      (alexandria:when-let (properties (nlk:json-value node :object "properties"))
        (let ((copied (make-hash-table :test 'equal)))
          (maphash (lambda (name schema)
                     (alexandria:when-let (schema (gemini-schema schema))
                       (setf (gethash name copied) schema)))
                   properties)
          (setf (gethash "properties" out) copied)))
      (alexandria:when-let (items (gemini-schema (nlk:json-value node :object "items")))
        (setf (gethash "items" out) items))
      (let ((union (or (nlk:json-value node :array "anyOf") (nlk:json-value node :array "oneOf"))))
        (when union
          (let* ((branches (remove nil (map 'list #'gemini-schema union)))
                 (solid (remove-if #'null-type-p branches))
                 (collapsed nil))
            (dolist (branch solid)
              (setf collapsed (if collapsed
                                  (merge-schemas collapsed branch :intersection)
                                  (nlk:copy-json-object branch))))
            (when collapsed
              (setf (gethash "nullable" collapsed) (if (< (length solid) (length branches)) t :false))
              (setf out (merge-schemas out collapsed :union))))))
      (loop for branch across (or (nlk:json-value node :array "allOf") #())
            for copied = (gemini-schema branch)
            when copied do (setf out (merge-schemas out copied :union)))
      (let ((type (gethash "type" out)))
        (when (and (vectorp type) (not (stringp type)))
          (let* ((types (remove-if-not #'stringp (coerce type 'list)))
                 (solid (remove "null" types :test #'equal)))
            (when (member "null" types :test #'equal)
              (setf (gethash "nullable" out) t))
            (setf (gethash "type" out) (or (first solid) (first types))))))
      (unless (present-p "type" out)
        (cond ((hash-table-p (gethash "properties" out)) (setf (gethash "type" out) "object"))
              ((present-p "items" out) (setf (gethash "type" out) "array"))
              ((present-p "enum" out) (setf (gethash "type" out) "string"))))
      out)))

(defun function-response-p (part)
  (nlk:json-value part :object "functionResponse"))

(defun tool-results-p (content)
  "Whether CONTENT, a GenAI content, is nothing but tool results."
  (let ((parts (nlk:json-value content :array "parts")))
    (and (equal "user" (nlk:json-value content :string "role"))
         parts (plusp (length parts)) (every #'function-response-p parts))))

(defun factory-part (part)
  "PART, a GenAI part of the core's, as the CLI sends it: tool names as the
wire names them, and a tool result's text under result."
  (alexandria:if-let (call (nlk:json-value part :object "functionCall"))
    (nlk:copy-json-object part "functionCall"
                          (nlk:copy-json-object call "name" (wire-tool-name (or (gethash "name" call) ""))))
    (alexandria:if-let (response (function-response-p part))
      (let ((text (or (nlk:json-value response :string "response" "content") "")))
        (nlk:json-object
         "functionResponse"
         (nlk:json-object "name" (wire-tool-name (or (nlk:json-value response :string "name") ""))
                          "response" (nlk:json-object
                                      "result" (if (plusp (length text)) text "Tool execution succeeded.")))))
      part)))

(defun factory-contents (contents)
  "CONTENTS, the core's GenAI history, as the CLI sends it: consecutive tool
results in one user turn, empty model turns dropped, and every unsigned call
from the newest user turn on carrying the signature-skip sentinel."
  (let ((out '()))
    (loop for content across contents
          for parts = (map 'vector #'factory-part (or (nlk:json-value content :array "parts") #()))
          do (cond ((and (tool-results-p content) out (tool-results-p (first out)))
                    (setf (first out)
                          (nlk:copy-json-object (first out) "parts"
                                                (concatenate 'vector (gethash "parts" (first out)) parts))))
                   ((and (equal "model" (nlk:json-value content :string "role")) (zerop (length parts))))
                   (t (push (nlk:copy-json-object content "parts" parts) out))))
    (let* ((contents (coerce (nreverse out) 'vector))
           (newest (or (position-if (lambda (content)
                                      (and (equal "user" (nlk:json-value content :string "role"))
                                           (notevery #'function-response-p
                                                     (or (nlk:json-value content :array "parts") #()))))
                                    contents :from-end t)
                       0)))
      (loop for index from newest below (length contents)
            for content = (aref contents index)
            when (equal "model" (nlk:json-value content :string "role"))
              do (loop for part across (gethash "parts" content)
                       when (and (nlk:json-value part :object "functionCall")
                                 (zerop (length (nlk:trimmed (or (nlk:json-value part :string "thoughtSignature") "")))))
                         do (setf (gethash "thoughtSignature" part) +skip-signature+)))
      contents)))

(defun gemini-body (body facts config)
  "BODY, a GenAI request, as the CLI sends it to Factory's /generate.
=> BODY, and the table from each tool's wire name back to its own."
  (let ((model (getf facts :model))
        (effort (getf facts :effort))
        (names (make-hash-table :test 'equal))
        (system (let ((parts (nlk:json-value body :array "systemInstruction" "parts")))
                  (and parts (plusp (length parts)) (nlk:json-value (aref parts 0) :string "text")))))
    (setf (gethash "model" body) model
          (gethash "systemInstruction" body)
          (nlk:json-object "parts" (vector (nlk:json-object
                                            "text" (format nil "~a~@[~%~a~]" +identity+ system))))
          (gethash "generationConfig" body)
          (nlk:json-object "temperature" (or (nle::effective-provider-config-temperature config) 1)
                           "topP" 0.95
                           "topK" 64
                           "thinkingConfig"
                           (if effort
                               (nlk:json-object "includeThoughts" t
                                                "thinkingLevel" (thinking-level model effort))
                               (nlk:json-object "includeThoughts" :false)))
          (gethash "contents" body) (factory-contents (or (nlk:json-value body :array "contents") #())))
    (remhash "toolConfig" body)
    (alexandria:when-let (tools (nlk:json-value body :array "tools"))
      (setf (gethash "tools" body)
            (map 'vector
                 (lambda (group)
                   (nlk:json-object
                    "functionDeclarations"
                    (map 'vector
                         (lambda (declaration &aux (name (or (nlk:json-value declaration :string "name") ""))
                                                   (wire (wire-tool-name name)))
                           (setf (gethash wire names) name)
                           (nlk:json-object
                            "name" wire
                            :opt "description" (nlk:json-value declaration :string "description")
                            "parameters" (or (gemini-schema (or (nlk:json-value declaration :object "parametersJsonSchema")
                                                                (nlk:json-value declaration :object "parameters")))
                                             (nlk:json-object "type" "object"))))
                         (or (nlk:json-value group :array "functionDeclarations") #()))))
                 tools)))
    (values body names)))

(defun named-back-fold (fold names)
  "FOLD, handed every GenAI frame with each function call's wire name turned
back into the tool's own name (NAMES maps them)."
  (lambda (frame finish record)
    (loop for candidate across (or (nlk:json-value frame :array "candidates") #())
          do (loop for part across (or (nlk:json-value candidate :array "content" "parts") #())
                   for call = (nlk:json-value part :object "functionCall")
                   for own = (and call (gethash (nlk:json-value call :string "name") names))
                   when own do (setf (gethash "name" call) own)))
    (funcall fold frame finish record)))
