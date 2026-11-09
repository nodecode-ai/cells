;;;; wire.lisp --- Command Code's /alpha/generate: the request, the NDJSON stream, the lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from NanamiMio/hermes-commandcode-oauth's transport.py (see
;;;; NOTICE): FORMAT-MESSAGES, FORMAT-TOOLS, WORKSPACE-CONFIG, the request
;;;; body, the event reader and the fail-closed end of a stream.
;;;;
;;;; The request is one JSON object:
;;;;
;;;;   {"config": {workingDir, environment, date, ...},
;;;;    "params": {model, messages, tools?, system, max_tokens, stream,
;;;;               temperature?, reasoning_effort?},
;;;;    "permissionMode": "standard", "mode": "agent", "threadId",
;;;;    "memory": null, "taste": null, "skills": null}
;;;;
;;;; and the answer one event per line (NDJSON, not SSE):
;;;; {"type": "text-delta", "text"}, {"type": "reasoning-delta", "text"},
;;;; {"type": "tool-call", "toolCallId", "toolName", "input"},
;;;; {"type": "finish", "totalUsage": {...}}, and an in-stream
;;;; {"type": "error", "statusCode", "error"} that is a failure on a 200.
;;;; The core's SSE walk refuses a non-SSE body, so the lane walks the lines
;;;; itself, under the same idle deadline and cancellation the core keeps.

(in-package #:nodecode-commandcode)

(defparameter +alpha-path+ "/alpha/generate"
  "The CLI's generation route, appended to the section's base.")

(defparameter +cli-version+ "1.87.1"
  "The Command Code CLI version whose wire this speaks; the endpoint gates
capability on it.")

(defparameter +default-max-tokens+ 64000
  "The output budget the original sends when the caller names none.")

;;; --- one round's working state ---------------------------------------------------

(defstruct (fold (:constructor make-fold))
  "What one stream accumulates: the assembly the core reads, the count of
calls, and whether the endpoint said the round was over."
  (asm (error "assembly required"))
  (said-p nil)
  (tools 0)
  (done-p nil))

(defvar *session-counter* 0
  "Handed out so two rounds in one process never share a session id.")

(defun random-session-id ()
  "A fresh session id for a request the turn context does not name."
  (format nil "commandcode-~(~8,'0x~)-~(~5,'0x~)"
          (logand (get-universal-time) #xffffffff) (incf *session-counter*)))

;;; --- the request ----------------------------------------------------------------

(defun os-name ()
  "The platform as the CLI's `environment' field names it."
  (string-downcase (or (uiop:operating-system) "unknown")))

(defun machine-name ()
  "The architecture as the CLI's `environment' field names it."
  (or (ignore-errors (machine-type)) "unknown"))

(defun today ()
  "The local date, YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun client-headers (session)
  "The Command Code CLI's client identity, which every request carries
(transport.py _open_stream): the relay gates features on the wire version."
  `(("user-agent" . "cli")
    ("accept" . "application/json")
    ("x-command-code-version" . ,+cli-version+)
    ("x-cli-environment" . "production")
    ("x-taste-learning" . "false")
    ("x-co-flag" . "false")
    ("x-session-id" . ,session)))

(defun image-part (part)
  "PART as the wire carries an image: the url and the MIME type that makes
the endpoint actually read the pixels (transport.py _normalize_media_part)."
  (multiple-value-bind (mime data) (nle::parse-data-uri (nlk:json-value part :string "image_url" "url"))
    (declare (ignore data))
    (let ((url (or (nlk:json-value part :string "image_url" "url")
                   (nlk:json-value part :string "image"))))
      (when url
        (let ((type (or mime (nlk:json-value part :string "mimeType"))))
          (nlk:json-object "type" "image" "image" url
                           :when type "mimeType" type))))))

(defun user-parts (content images-p)
  "CONTENT as the wire's user parts: text as it is, an image when the model
takes one (else the placeholder), anything else as a short text standing in."
  (if (stringp content)
      (vector (nlk:json-object "type" "text" "text" content))
      (let ((parts '()))
        (dolist (part (nle::message-content-parts content))
          (let ((type (nle::content-part-type part)))
            (cond ((equal type "text")
                   (push (nlk:json-object "type" "text" "text" (or (nlk:json-value part :string "text") ""))
                         parts))
                  ((and images-p (image-part part)) (push (image-part part) parts))
                  (t (push (nlk:json-object "type" "text"
                                             "text" (format nil "[~a: attached]" (or type "attachment")))
                           parts)))))
        (coerce (nreverse parts) 'vector))))

(defun call-arguments (call)
  "The arguments of chat-shaped tool CALL as the object the wire wants."
  (nth-value 1 (nle::tool-call-function-input call)))

(defun tool-call-part (call)
  "CALL as the wire's assistant tool-call part."
  (nlk:json-object "type" "tool-call"
                   "toolCallId" (or (nlk:json-value call :string "id") "")
                   "toolName" (or (nlk:json-value call :string "function" "name") "tool")
                   "input" (call-arguments call)))

(defun wire-message (message images-p)
  "One history MESSAGE as /alpha/generate carries it, or NIL for a system
message (its text joins the request's `system' string)."
  (let ((role (nlk:json-value message :string "role"))
        (content (gethash "content" message)))
    (cond ((equal role "system") nil)
          ((equal role "developer") nil)
          ((equal role "user")
           (nlk:json-object "role" "user" "content" (user-parts content images-p)))
          ((equal role "tool")
           (nlk:json-object "role" "tool"
                            "content" (vector
                                       (nlk:json-object "type" "tool-result"
                                                        "toolCallId" (or (nlk:json-value message :string "tool_call_id") "")
                                                        "toolName" (or (nlk:json-value message :string "name") "tool")
                                                        "output" (nlk:json-object "type" "text"
                                                                                  "value" (nle::content-text content))))))
          ((equal role "assistant")
           (let ((parts '()))
             (when (plusp (length (nle::content-text content)))
               (push (nlk:json-object "type" "text" "text" (nle::content-text content)) parts))
             (loop for call across (nlk:json-array message "tool_calls")
                   do (push (tool-call-part call) parts))
             (nlk:json-object "role" "assistant"
                              "content" (or (coerce (nreverse parts) 'vector)
                                             (vector (nlk:json-object "type" "text" "text" ""))))))
          (t nil))))

(defun system-text (context)
  "CONTEXT's system prompt plus every history system/developer message: the
wire carries one `system' string, so all of them join it."
  (let ((parts (list (nle::compiled-turn-context-system-prompt context))))
    (loop for message across (nle::request-messages context)
          when (member (nlk:json-value message :string "role") '("system" "developer") :test #'equal)
            do (let ((text (nle::content-text (gethash "content" message))))
                 (when (plusp (length (nlk:trimmed text))) (push text parts))))
    (format nil "~{~a~^~%~%~}" (remove "" (nreverse parts) :test #'equal))))

(defun wire-messages (context model)
  "CONTEXT's history as /alpha/generate messages, system turns aside."
  (let ((images-p (model-images-p model)))
    (remove nil (map 'vector (lambda (message) (wire-message message images-p))
                     (nle::request-messages context)))))

(defun wire-tools (context)
  "CONTEXT's tools as the wire's {name, description, input_schema} declarations."
  (let ((tools (nle::compiled-turn-context-tools context)))
    (if (or (null tools) (zerop (length tools)))
        nil
        (nle::tool-declarations tools "input_schema"))))

(defun workspace-config ()
  "The `config' block the endpoint expects (transport.py WORKSPACE-CONFIG): a
light workspace snapshot, never a file listing."
  (let ((cwd (ignore-errors (uiop:getcwd))))
    (nlk:json-object "workingDir" (or (and cwd (uiop:native-namestring cwd)) "")
                     "isGitRepo" :false
                     "currentBranch" ""
                     "mainBranch" ""
                     "gitStatus" ""
                     "recentCommits" #()
                     "date" (today)
                     "environment" (format nil "~a-~a" (os-name) (machine-name))
                     "structure" #())))

(defun request-body (context)
  "CONTEXT as the /alpha/generate request body (transport.py _request_body)."
  (let* ((frozen (nle::compiled-turn-context-provider-config context))
         (model (canonical-model-id (nle::effective-provider-config-model frozen)))
         (params (nlk:json-object
                  "model" model
                  "messages" (wire-messages context model)
                  "system" (system-text context)
                  "max_tokens" (or (nle::effective-max-output-tokens context) +default-max-tokens+)
                  "stream" t))
         (tools (wire-tools context))
         (temperature (nle::effective-provider-config-temperature frozen))
         (effort (nle::effective-provider-config-reasoning-effort frozen)))
    (when tools
      (setf (gethash "tools" params) tools))
    (when temperature
      (setf (gethash "temperature" params) temperature))
    (when effort
      (setf (gethash "reasoning_effort" params) effort))
    (nlk:json-object "config" (workspace-config)
                     "memory" :null
                     "taste" :null
                     "skills" :null
                     "permissionMode" "standard"
                     "mode" "agent"
                     "params" params)))

;;; --- one round ----------------------------------------------------------------

(defun generate-url (frozen)
  "The /alpha/generate address a round on FROZEN posts to."
  (format nil "~a~a" (normalized-base (nle::effective-provider-config-endpoint frozen)) +alpha-path+))

(defun post-alpha (url key request-json headers frozen)
  "POST REQUEST-JSON (octets) to URL with bearer KEY: the response body as a
stream, under the config's idle deadline."
  (declare (ignore key))
  (let ((seconds (nle::effective-provider-config-request-timeout frozen))
        (proxy (or (nle::effective-provider-config-proxy frozen) dex:*default-proxy*)))
    (nle::with-idle-cut (seconds)
        (sb-sys:with-deadline (:seconds seconds)
          (nle::cancellable-post (request-json (or proxy url))
            (dex:post url
                      :headers (cons '("content-type" . "application/json")
                                     (cons (cons "authorization" (format nil "Bearer ~a" key)) headers))
                      :content request-json
                      :read-timeout (* 4 seconds)
                      :connect-timeout seconds
                      :proxy proxy
                      :want-stream t
                      :force-binary t)))
      (error 'nle::provider-error :scope :request
                                  :detail (format nil "no response for ~a s" (nle::idle-seconds-label seconds))
                                  :evidence-request-body (nle::bounded-evidence request-json)))))

(defun walk-lines (stream frozen on-line)
  "Call ON-LINE with each non-blank line of STREAM, under the idle deadline
and the turn's cancellation, until EOF. => the evidence kept, bounded."
  (let* ((seconds (nle::effective-provider-config-request-timeout frozen))
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

(defun fold-usage (asm event)
  "EVENT's totalUsage onto ASM's usage, the cached share apart from the rest
(transport.py _usage_from_event)."
  (let* ((raw (or (nlk:json-value event :object "totalUsage")
                  (nlk:json-value event :object "usage")))
         (usage (nle::lane-assembly-usage asm)))
    (when (hash-table-p raw)
      (let* ((prompt (or (nlk:json-value raw :integer "inputTokens") 0))
             (output (or (nlk:json-value raw :integer "outputTokens") 0))
             (details (nlk:json-value raw :object "inputTokenDetails"))
             (cached (or (nlk:json-value details :integer "cacheReadTokens") 0))
             (written (or (nlk:json-value details :integer "cacheWriteTokens") 0)))
        (declare (ignore written))
        (setf (nle::provider-usage-input-tokens usage) prompt
              (nle::provider-usage-output-tokens usage) output
              (nle::provider-usage-total-tokens usage) (+ prompt output))
        (when cached
          (setf (nle::provider-usage-cached-input-tokens usage) cached))))))

(defun fold-event (fold event)
  "One decoded EVENT into FOLD: text, reasoning, a tool call, the finish."
  (let ((asm (fold-asm fold))
        (type (nlk:json-value event :string "type")))
    (cond ((equal type "text-delta")
           (let ((text (or (nlk:json-value event :string "text") "")))
             (unless (zerop (length text))
               (setf (fold-said-p fold) t)
               (nle::assembly-text-delta asm "txt-0" text :close-reasoning t))))
          ((equal type "reasoning-delta")
           (let ((text (or (nlk:json-value event :string "text") "")))
             (unless (zerop (length text))
               (setf (fold-said-p fold) t)
               (nle::assembly-reasoning-delta asm "reasoning-0" text :close-text t))))
          ((equal type "tool-call")
           (let* ((name (or (nlk:json-value event :string "toolName") "tool"))
                  (raw (or (nlk:json-value event :object "input")
                           (nlk:json-value event :object "args")))
                  (arguments (if (hash-table-p raw) (nlk:encode-json-object raw) "{}")))
             (setf (fold-said-p fold) t)
             (nle::open-tool-buffer asm (fold-tools fold)
                                     :id (or (nlk:json-value event :string "toolCallId")
                                             (format nil "commandcode:~d:~a" (fold-tools fold) name))
                                     :name name :arguments arguments)
             (incf (fold-tools fold))))
          ((equal type "finish")
           (setf (fold-done-p fold) t)
           (fold-usage asm event))
          ((member type '("finish-step" "cache-write-tokens") :test #'equal)
           (fold-usage asm event))
          ((equal type "abort")
           (error 'nle::provider-error :status 502 :detail "Command Code aborted the generation"))
          ((equal type "error")
           (let* ((why (nlk:json-value event :any "error"))
                  (status (or (nlk:json-value event :integer "statusCode")
                              (and (hash-table-p why) (nlk:json-value why :integer "statusCode"))))
                  (detail (cond ((stringp why) why)
                                ((hash-table-p why) (or (nlk:json-value why :string "message") "generation error"))
                                (t "generation error"))))
             (error 'nle::provider-error :status (or status 502)
                                         :detail (format nil "in-stream provider error: ~a" detail)))))))

(defun finish-reason (fold)
  "The round's finish reason: a round with calls is a tool round."
  (if (plusp (fold-tools fold)) "tool_calls" "stop"))

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The lane's stream: one POST /alpha/generate, its NDJSON folded into the
chat-shaped assistant message. => (values MESSAGE USAGE FINISH-REASON REQUEST-JSON)"
  (let* ((frozen (nle::compiled-turn-context-provider-config context))
         (key (nle::effective-provider-config-api-key frozen))
         (session (or (nle::compiled-turn-context-session-id context) ""))
         (body (request-body context))
         (request-json (sb-ext:string-to-octets (nlk:encode-json-object body) :external-format :utf-8))
         (asm (nle::make-lane-assembly :on-part on-part))
         (fold (make-fold :asm asm))
         (evidence ""))
    (when (or (null key) (equal key "public") (zerop (length key)))
      (error 'nle::provider-config-error
             :status 401
             :detail (format nil "Command Code needs a key: sign in with the Command Code CLI (~a), save one with /connect, or set ~a"
                             +key-page+ (first +env+))))
    (let ((stream (post-alpha (generate-url frozen) key request-json
                              (client-headers (if (plusp (length session)) session (random-session-id)))
                              frozen)))
      (nle::emit-stream-part on-part :stream-start)
      (unwind-protect
           (handler-bind ((nle::provider-error
                            (lambda (condition)
                              (nle::attach-provider-evidence condition evidence request-json))))
             (setf evidence
                   (walk-lines stream frozen
                                    (lambda (line)
                                      (let ((event (ignore-errors (nlk:decode-json line))))
                                        (when (hash-table-p event)
                                          (fold-event fold event))))))
             (unless (fold-done-p fold)
               (error 'nle::provider-stream-incomplete
                      :detail "the stream ended before the answer finished")))
        (nle::close-provider-stream-body stream)))
    (nle::assembly-close-spans asm :order '(:tools :reasoning :text))
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
      (values message usage (finish-reason fold) request-json))))
