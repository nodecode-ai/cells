;;;; wire.lisp --- one turn on the on-device model: the bridge's request, its events, the lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi's packages/ai/src/providers/apple-foundation-models.ts
;;;; (buildRequest, toEntries, toParts, mapToolChoice, REASONING_LEVELS,
;;;; streamAppleFoundationModels). Every request lowers the whole
;;;; conversation into a Foundation Models transcript and the bridge streams
;;;; exactly one model turn back; tool calls are returned, not run, so the
;;;; turn loop runs them and the next request carries their outputs.
;;;;
;;;;   {"instructions"?, "entries": [{"kind": "prompt"|"response", "parts"}
;;;;                                 | {"kind": "toolCalls", "calls": [{id, name, arguments}]}
;;;;                                 | {"kind": "toolOutput", "id", "name", "parts"}],
;;;;    "tools"?: [{name, description, parameters (a JSON text)}],
;;;;    "temperature"?, "maxTokens"?, "toolChoice"?, "greedy"?, "topP"?, "reasoningLevel"?}
;;;;
;;;; and the events, one JSON object per line on the helper's stdout:
;;;; text, reasoning, toolCall (an id, a name, an arguments fragment), usage,
;;;; then exactly one done or error.

(in-package #:nodecode-apple)

(defparameter +image-omitted+ "[image omitted: model does not support vision]"
  "What stands where an image was for a model that takes none (vision-guard.ts).")

(defparameter +reasoning-levels+
  '(("minimal" . "light") ("low" . "light") ("medium" . "moderate")
    ("high" . "deep") ("xhigh" . "deep") ("max" . "deep"))
  "An effort's ContextOptions.ReasoningLevel (REASONING_LEVELS).")

(defparameter +content-blocked+ '("guardrail_violation" "refusal")
  "The bridge's codes for a safety-filter outcome, which is final.")

;;; --- the request -------------------------------------------------------------------

(defun parts (content images-p counter)
  "Wire CONTENT as the bridge's parts (toParts): the text parts joined by
newlines (the placeholder after them for a model without vision), then each
data: image as {type: image, data, label}, labelled image-N across the whole
conversation through COUNTER (a box), so a label never changes between turns."
  (if (stringp content)
      (vector (nlk:json-object "type" "text" "text" content))
      (let ((texts '()) (images '()) (omitted nil))
        (dolist (part (nle::message-content-parts content))
          (let ((type (nle::content-part-type part)))
            (cond ((equal type "text") (push (or (nlk:json-value part :string "text") "") texts))
                  ((equal type "image_url")
                   (let ((data (nth-value 1 (nle::parse-data-uri
                                             (nlk:json-value part :string "image_url" "url")))))
                     (if (and images-p data) (push data images) (setf omitted t)))))))
        (let* ((text (format nil "~{~a~^~%~}" (nreverse texts)))
               (text (if omitted
                         (if (plusp (length text)) (format nil "~a~%~a" text +image-omitted+) +image-omitted+)
                         text)))
          (coerce (append (and (plusp (length text)) (list (nlk:json-object "type" "text" "text" text)))
                          (loop for data in (nreverse images)
                                collect (nlk:json-object "type" "image" "data" data
                                                         "label" (format nil "image-~d" (incf (car counter))))))
                  'vector)))))

(defun entries (messages images-p)
  "The history MESSAGES as transcript entries (toEntries)."
  (let ((counter (list 0)) (out '()))
    (loop for message across (coerce messages 'vector)
          for role = (nlk:json-value message :string "role")
          for content = (gethash "content" message)
          do (cond ((member role '("user" "system") :test #'equal)
                    (push (nlk:json-object "kind" "prompt" "parts" (parts content images-p counter)) out))
                   ((equal role "tool")
                    (push (nlk:json-object "kind" "toolOutput"
                                           "id" (or (nlk:json-value message :string "tool_call_id") "")
                                           "name" (or (nlk:json-value message :string "name") "")
                                           "parts" (parts content images-p counter))
                          out))
                   ((equal role "assistant")
                    (let ((text (nle::content-text content))
                          (calls (nlk:json-array message "tool_calls")))
                      (when (plusp (length text))
                        (push (nlk:json-object "kind" "response"
                                               "parts" (vector (nlk:json-object "type" "text" "text" text)))
                              out))
                      (when (plusp (length calls))
                        (push (nlk:json-object
                               "kind" "toolCalls"
                               "calls" (map 'vector
                                            (lambda (call)
                                              (multiple-value-bind (name input) (nle::tool-call-function-input call)
                                                (nlk:json-object "id" (or (nlk:json-value call :string "id") "")
                                                                 "name" name
                                                                 "arguments" (nlk:encode-json-object input))))
                                            calls))
                              out))))))
    (coerce (nreverse out) 'vector)))

(defun tool-choice-value (choice)
  "The bridge's toolChoice for CHOICE (mapToolChoice): none, required, or NIL for auto."
  (cond ((null choice) nil)
        ((string-equal choice "none") "none")
        ((member choice '("required" "any") :test #'string-equal) "required")))

(defun bridge-request (context)
  "(values REQUEST ENCODED): the bridge's request for the compiled CONTEXT
(buildRequest), and per tool the argument paths it JSON-encodes."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (effort (nle::effective-provider-config-reasoning-effort config))
         (temperature (nle::effective-provider-config-temperature config))
         (top-p (nle::effective-provider-config-top-p config))
         (system (nle::compiled-turn-context-system-prompt context))
         (encoded (make-hash-table :test 'equal))
         (tools (map 'vector
                     (lambda (wrapper)
                       (let* ((fn (nlk:json-value wrapper :object "function"))
                              (name (or (nlk:json-value fn :string "name") "")))
                         (multiple-value-bind (schema paths)
                             (foundation-schema (or (nlk:json-value fn :object "parameters")
                                                    (nlk:json-object "type" "object"))
                                                name)
                           (setf (gethash name encoded) paths)
                           (nlk:json-object "name" name
                                            "description" (or (nlk:json-value fn :string "description") "")
                                            "parameters" (nlk:encode-json-object schema)))))
                     (or (nle::compiled-turn-context-tools context) #())))
         (choice (and (plusp (length tools))
                      (tool-choice-value (nle::effective-provider-config-tool-choice config)))))
    (values
     (nlk:json-object
      :when (plusp (length system)) "instructions" system
      "entries" (entries (nle::request-messages context) (model-vision-p))
      :when (plusp (length tools)) "tools" tools
      :opt "temperature" temperature
      :opt "maxTokens" (nle::effective-max-output-tokens context)
      :opt "toolChoice" choice
      ;; temperature 0 with no bounds asks for deterministic output
      :when (and temperature (zerop temperature) (null top-p)) "greedy" t
      :opt "topP" top-p
      :opt "reasoningLevel" (and (model-reasoning-p) effort
                                 (cdr (assoc effort +reasoning-levels+ :test #'string-equal))))
     encoded)))

;;; --- the helper process ------------------------------------------------------------

(defun start-generation (helper request-json)
  "Run HELPER generate with REQUEST-JSON on its stdin, which is then closed.
=> the process, its stdout a character stream of event lines."
  (let ((process (sb-ext:run-program helper '("generate")
                                     :wait nil :input :stream :output :stream :error nil
                                     :external-format :utf-8)))
    (let ((in (sb-ext:process-input process)))
      (write-string request-json in)
      (finish-output in)
      (close in))
    process))

(defun end-generation (process)
  "End PROCESS however the round went: a generation still running is
cancelled by ending its helper."
  (when (sb-ext:process-alive-p process)
    (ignore-errors (sb-ext:process-kill process 15)))
  (ignore-errors (close (sb-ext:process-output process)))
  (ignore-errors (sb-ext:process-wait process))
  (ignore-errors (sb-ext:process-close process)))

(defun next-event (stream seconds)
  "The next event STREAM carries within SECONDS, or NIL at its end."
  (loop
    (when nle::*current-durable-turn*
      (nlk:ensure-turn-not-cancelled nle::*current-durable-turn*))
    (let ((line (handler-case (nlk:with-cancellable-wait (nle::*current-durable-turn*)
                                (sb-sys:with-deadline (:seconds seconds) (read-line stream nil nil)))
                  (sb-sys:deadline-timeout ()
                    (error 'nle::provider-stream-incomplete
                           :detail (format nil "the on-device model said nothing for ~a s" seconds))))))
      (unless line (return nil))
      (let ((event (ignore-errors (nlk:decode-json line))))
        (when (hash-table-p event) (return event))))))

(defun bridge-failure (event)
  "The provider error a bridge error EVENT stands for."
  (let* ((code (or (nlk:json-value event :string "code") "runtime"))
         (detail (format nil "~a (~a)" (or (nlk:json-value event :string "message") "") code)))
    (cond ((member code +content-blocked+ :test #'equal)
           (make-condition 'nle::provider-error :status 400 :scope :contract :detail detail))
          ;; `Prompt is too long: N tokens exceed the M token context window':
          ;; the overflow the core evicts on
          ((equal code "context_size_exceeded")
           (make-condition 'nle::provider-error :status 400 :detail detail))
          ((equal code "rate_limited")
           (make-condition 'nle::provider-error :status 429 :scope :request :detail detail))
          ((member code '("unavailable" "unsupported_os" "unsupported_platform" "not_built" "assets_unavailable")
                   :test #'equal)
           (make-condition 'nle::provider-config-error :status 503 :detail detail))
          ((member code '("invalid_request" "invalid_tool_schema" "unsupported_capability"
                          "unsupported_transcript_content" "unsupported_generation_guide" "unsupported_language")
                   :test #'equal)
           (make-condition 'nle::provider-error :status 400 :scope :request :detail detail))
          ((equal code "timeout") (make-condition 'nle::provider-error :detail detail))
          (t (make-condition 'nle::provider-error :status 500 :detail detail)))))

;;; --- the lane ------------------------------------------------------------------------

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The lane's stream: one bridge generation, its events folded into the
chat-shaped assistant message. => (values MESSAGE USAGE FINISH-REASON REQUEST-JSON)"
  (unless (mac-p)
    (error 'nle::provider-config-error
           :status 503
           :detail "Apple Foundation Models runs only on a Mac with Apple silicon (macOS 27 or later)"))
  (multiple-value-bind (request encoded) (bridge-request context)
    (let* ((config (nle::compiled-turn-context-provider-config context))
           (request-json (nlk:encode-json-object request))
           (asm (nle::make-lane-assembly :on-part on-part))
           (usage (nle::lane-assembly-usage asm))
           (calls '())                  ; (call-id . index), newest first
           (seconds (nle::effective-provider-config-request-timeout config))
           (done nil)
           (process (start-generation (helper) request-json)))
      (unwind-protect
           (progn
             (nle::emit-stream-part on-part :stream-start)
             (loop for event = (next-event (sb-ext:process-output process) seconds)
                   while event
                   do (let ((type (nlk:json-value event :string "type")))
                        (cond
                          ((equal type "text")
                           (nle::assembly-text-delta asm "txt-0" (or (nlk:json-value event :string "text") "")
                                                     :close-reasoning t))
                          ((equal type "reasoning")
                           (nle::assembly-reasoning-delta asm "reasoning-0" (or (nlk:json-value event :string "text") "")
                                                          :close-text t))
                          ((equal type "toolCall")
                           (let* ((id (or (nlk:json-value event :string "callId") ""))
                                  (known (assoc id calls :test #'equal)))
                             (nle::lifecycle-close-text asm)
                             (nle::lifecycle-close-reasoning asm)
                             (unless known
                               (setf known (cons id (length calls)))
                               (push known calls)
                               (nle::open-tool-buffer asm (cdr known) :id id
                                                                      :name (or (nlk:json-value event :string "name") "")
                                                                      :arguments ""))
                             (nle::assembly-tool-fragment asm (cdr known)
                                                          (or (nlk:json-value event :string "arguments") ""))))
                          ((equal type "usage")
                           (let ((input (or (nlk:json-value event :integer "input") 0))
                                 (cached (or (nlk:json-value event :integer "cachedInput") 0))
                                 (output (or (nlk:json-value event :integer "output") 0)))
                             (setf (nle::provider-usage-input-tokens usage) (- input cached)
                                   (nle::provider-usage-cached-input-tokens usage) cached
                                   (nle::provider-usage-output-tokens usage) output
                                   (nle::provider-usage-reasoning-tokens usage) (nlk:json-value event :integer "reasoning")
                                   (nle::provider-usage-total-tokens usage) (+ input output))))
                          ((equal type "error") (error (bridge-failure event)))
                          ((equal type "done") (setf done t) (return)))))
             (unless done
               (error 'nle::provider-stream-incomplete
                      :detail "the Foundation Models bridge ended before the turn finished")))
        (end-generation process))
      (nle::assembly-close-spans asm :order '(:text :reasoning :tools))
      (nle::flush-thinking-tag asm)
      (let* ((max-tokens (nlk:json-value request :integer "maxTokens"))
             (finish (cond (calls "tool_calls")
                           ((and max-tokens (nle::provider-usage-output-tokens usage)
                                 (>= (nle::provider-usage-output-tokens usage) max-tokens))
                            "length")
                           (t "stop")))
             (message
               (nlk:json-object
                "role" "assistant"
                "content" (let ((full (get-output-stream-string (nle::lane-assembly-content asm))))
                            (if (string= full "") :null full))
                :when (nle::lane-assembly-reasoning-seen-p asm) "reasoning_content"
                (get-output-stream-string (nle::lane-assembly-reasoning asm))
                :when calls "tool_calls"
                (map 'vector
                     (lambda (pair &aux (buf (cdr pair)))
                       (let* ((name (getf buf :name))
                              (raw (ignore-errors (nlk:decode-json (getf buf :arguments))))
                              (arguments (decode-arguments (if (hash-table-p raw) raw (make-hash-table :test 'equal))
                                                           (gethash name encoded))))
                         (nle::chat-tool-call-object (or (getf buf :id) "") name (nlk:encode-json-object arguments))))
                     (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car)))))
        (nle::emit-stream-part on-part :finish)
        (values message
                (and (not (equalp usage (nle::make-provider-usage))) usage)
                finish
                (sb-ext:string-to-octets request-json :external-format :utf-8))))))
