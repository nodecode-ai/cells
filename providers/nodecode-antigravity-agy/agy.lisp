;;;; agy.lisp --- the agy CLI as a model: one process, one prompt, one answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The shape follows Realtyxxx's hermes-plugin-antigravity-agy (see NOTICE):
;;;; one round is one `agy' print-mode run, in a private scratch workspace
;;;; that holds a custom agent stripping every built-in component, so agy acts
;;;; as the model only. The plugin hands tool calls back through an MCP server
;;;; that never answers; Nodecode has no MCP client to fake, so the tool
;;;; protocol here is text: agy is told to answer a tool use with one
;;;; <call name="...">{json}</call> line, and the relay turns that line into
;;;; the round's tool_calls. Nodecode executes every tool.

(in-package #:nodecode-antigravity-agy)

(defparameter +agent-name+ "nodecode-relay"
  "The custom agent's name: what --agent names and the workspace carries.")

(defparameter +fallback-marker+ "falling back to default"
  "What agy writes to its log when it did not load the agent and used its own
instead: that agent owns built-in tools, so the round is refused.")

(defparameter +fallback-models+
  '("gemini-3.8-flash-low" "gemini-3.8-flash-medium" "gemini-3.8-flash-high"
    "gemini-3.7-flash-low" "gemini-3.7-flash-medium" "gemini-3.7-flash-high"
    "gemini-3.6-flash-low" "gemini-3.6-flash-medium" "gemini-3.6-flash-high"
    "gemini-3.1-pro-low" "gemini-3.1-pro-high"
    "claude-sonnet-4-6" "claude-opus-4-6-thinking" "gpt-oss-120b-medium")
  "The slugs agy serves, as the plugin's own fallback catalog keeps them: the
section's list wins, and `agy models' lists the account's live picker.")

(defparameter +aliases+ '(("flash" . "gemini-3.8-flash-medium")
                          ("pro" . "gemini-3.1-pro-high")
                          ("sonnet" . "claude-sonnet-4-6")
                          ("opus" . "claude-opus-4-6-thinking"))
  "Short names the plugin accepts for a family's default variant.")

(defparameter +windows+ '(("gemini-" . 1000000) ("claude-" . 200000) ("gpt-oss-" . 128000))
  "Family prefix -> context window: agy's harness limits are not published,
and the whole conversation travels as one prompt.")

(defparameter +default-window+ 128000)

;;; --- models --------------------------------------------------------------------

(defun model-ids ()
  "The slugs /models offers: the section's models, else the bundled ones."
  (let ((configured (ignore-errors (coerce (setting :models) 'list))))
    (or (remove-duplicates (remove-if-not #'stringp configured) :test #'equal)
        (copy-list +fallback-models+))))

(defun native-model (id)
  "Hermes/Nodecode model ID as the slug agy is asked for: an alias resolved,
a bare family given the variant agy accepts."
  (let ((id (or id "")))
    (setf id (or (cdr (assoc id +aliases+ :test #'equal)) id))
    (if (member id +fallback-models+ :test #'equal)
        id
        (or (loop for effort in '("medium" "high" "low")
                  for candidate = (format nil "~a-~a" id effort)
                  when (member candidate +fallback-models+ :test #'equal) return candidate)
            id))))

(defun context-window (id)
  "The window the family of ID is given."
  (let ((slug (native-model id)))
    (or (loop for (prefix . window) in +windows+
              when (uiop:string-prefix-p prefix slug) return window)
        +default-window+)))

;;; --- the prompt ------------------------------------------------------------------

(defun json-string (text)
  "TEXT as a JSON string literal, quotes included."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across (or text "")
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (when (>= (char-code character) 32) (write-char character out)))))
    (write-char #\" out)))

(defun part-text (content)
  "A message's CONTENT as plain text: a string, or the text parts of a
content array; an image is a placeholder, since a round forwards text only."
  (cond ((stringp content) content)
        ((vectorp content)
         (with-output-to-string (out)
           (loop for part across content
                 for text = (nlk:json-value part :string "text")
                 do (write-string (or text "[image omitted: a round forwards text only]") out)
                    (terpri out))))
        (t "")))

(defun system-text (messages)
  "The system and developer messages of the round, joined."
  (with-output-to-string (out)
    (loop for message across messages
          when (member (nlk:json-value message :string "role") '("system" "developer")
                       :test #'equal)
            do (write-string (part-text (gethash "content" message)) out)
               (terpri out))))

(defun conversation-text (messages)
  "The round's history flattened into the one message agy reads."
  (with-output-to-string (out)
    (write-line "<conversation>" out)
    (loop for message across messages
          for role = (nlk:json-value message :string "role")
          unless (member role '("system" "developer") :test #'equal)
            do (cond
                 ((equal role "assistant")
                  (format out "<message role=\"assistant\">~a"
                          (part-text (gethash "content" message)))
                  (loop for call across (or (nlk:json-value message :array "tool_calls") #())
                        do (format out "<tool_call name=~a>~a</tool_call>"
                                   (json-string (or (nlk:json-value call :string "function" "name") ""))
                                   (or (nlk:json-value call :string "function" "arguments") "{}")))
                  (format out "</message>~%"))
                 ((equal role "tool")
                  (format out "<message role=\"tool_result\" call_id=~a>~a</message>~%"
                          (json-string (or (nlk:json-value message :string "tool_call_id") ""))
                          (part-text (gethash "content" message))))
                 (t (format out "<message role=~a>~a</message>~%"
                            (json-string (or role "user"))
                            (part-text (gethash "content" message))))))
    (write-line "</conversation>" out)
    (write-line "" out)
    (write-line "The conversation above is the complete history so far and you are the assistant in it. Continue from its end." out)))

(defun tool-catalog (tools)
  "The tools the round offers, rendered for the prompt: name, description,
and parameter schema."
  (when (and tools (or (vectorp tools) (listp tools)) (plusp (length tools)))
    (with-output-to-string (out)
      (loop for tool across (coerce tools 'vector)
            for function = (or (gethash "function" tool) tool)
            for name = (nlk:json-value function :string "name")
            when name
              do (format out "### ~a~%~a~%Parameters (JSON Schema):~%~a~%~%"
                         name
                         (or (nlk:json-value function :string "description") "")
                         (nlk:encode-json-object
                          (or (gethash "parameters" function)
                              (nlk:make-json-object))))))))

(defun system-prompt (messages tools)
  "What agy is told before the conversation: Nodecode's own system text, the
tool protocol, and the catalog."
  (let ((catalog (tool-catalog tools)))
    (with-output-to-string (out)
      (write-string (system-text messages) out)
      (terpri out)
      (write-line "# Tool protocol" out)
      (if catalog
          (progn
            (write-line "You run as the model inside Nodecode. Nodecode, not you, executes tools. The only way to use one is to answer with exactly one line and nothing else:" out)
            (write-line "<call name=\"TOOL\">{arguments as a JSON object}</call>" out)
            (write-line "Then stop: the result arrives as the next message. Never write a call as prose, never use a tool you were not offered. When no tool is needed, answer directly." out)
            (terpri out)
            (write-line "# Available tools" out)
            (write-string catalog out))
          (write-line "No tools are available in this conversation. Do not call anything; answer directly." out)))))

;;; --- the answer ------------------------------------------------------------------

(defparameter +call-scanner+
  (ppcre:create-scanner "<call\\s+name=\"([^\"]+)\"\\s*>([\\s\\S]*?)</call>")
  "One <call> line, name and arguments.")

(defun call-object (name arguments)
  "NAME and ARGUMENTS as the round's tool call: arguments always a JSON object."
  (format nil "{\"name\":~a,\"arguments\":~a}"
          (json-string name)
          (json-string
           (handler-case
               (let ((value (nlk:decode-json arguments)))
                 (if (hash-table-p value) (nlk:encode-json-object value) "{}"))
             (error () (format nil "{\"input\":~a}" (json-string arguments)))))))

(defun split-calls (text)
  "TEXT as (values CONTENT CALLS): the prose before the first call, and each
<call> in it as (NAME . ARGUMENTS-JSON)."
  (let ((first (ppcre:scan +call-scanner+ text)))
    (if (null first)
        (values (string-trim '(#\Space #\Tab #\Newline) text) nil)
        (values (string-trim '(#\Space #\Tab #\Newline) (subseq text 0 first))
                (loop for match in (ppcre:all-matches-as-strings +call-scanner+ text)
                      collect (multiple-value-bind (whole groups)
                                  (ppcre:scan-to-strings +call-scanner+ match)
                                (declare (ignore whole))
                                (cons (aref groups 0) (or (aref groups 1) "{}"))))))))

(defun usage-of (usage)
  "agy's usage counters as the chat wire's: cache reads are prompt tokens,
thinking tokens are reasoning tokens."
  (let ((input (or (nlk:json-value usage :integer "input_tokens") 0))
        (cached (or (nlk:json-value usage :integer "cache_read_tokens") 0))
        (output (or (nlk:json-value usage :integer "output_tokens") 0))
        (thinking (or (nlk:json-value usage :integer "thinking_tokens") 0)))
    (list :prompt (+ input cached) :completion output
          :cached cached :reasoning thinking)))

;;; --- the process -----------------------------------------------------------------

(defun command-argv ()
  "The agy command as argv: the section's command line, split on spaces."
  (let ((command (ignore-errors (setting :command))))
    (or (and command (stringp command) (uiop:split-string command))
        (list "agy"))))

(defun agy-available-p ()
  "Whether the agy CLI named by the section is on PATH (or a path that runs)."
  (let ((head (first (command-argv))))
    (and head (ignore-errors (nlk::executable-on-path head)) t)))

(defun agent-markdown (system)
  "The custom agent's file: no built-in tool, no default prompt section, no
user MCP server -- agy is the model and nothing else."
  (format nil "---~%name: ~a~%description: \"Nodecode relay: model only, Nodecode executes every tool\"~%excludeDefaultComponents: true~%inheritMcp: false~%subagent: false~%mainAgent: true~%---~%~a~%"
          +agent-name+ system))

(defun scratch-workspace ()
  "A private workspace for one round: (values ROOT WORKSPACE LOG-PATH)."
  (let* ((root (merge-pathnames (format nil "nodecode-agy-~36r/" (random (expt 2 32)))
                                 (uiop:temporary-directory)))
         (workspace (merge-pathnames "workspace/" root))
         (agent (merge-pathnames (format nil ".agents/agents/~a/" +agent-name+) workspace)))
    (ensure-directories-exist (merge-pathnames "agent.md" agent))
    (values root workspace (merge-pathnames "agy.log" root))))

(defun user-event (prompt)
  "The one stream-json line agy reads: the round's prompt as its user turn."
  (format nil "{\"event\":\"user\",\"message\":{\"role\":\"user\",\"content\":~a}}" (json-string prompt)))

(defun check-agent (seen agent log)
  "Refuse a round agy answered with its own agent: that one owns built-in tools."
  (when (and seen agent (not (string-equal agent +agent-name+)))
    (error 'agy-error :message (format nil "agy loaded agent ~a, not the relay agent; refusing the round so no built-in tool runs" agent)))
  (when (search +fallback-marker+ (or (ignore-errors (uiop:read-file-string log)) ""))
    (error 'agy-error :message "agy fell back to its default agent, which owns built-in tools; refusing the round")))

(defun read-events (stream log)
  "Read agy's NDJSON stream to its result: (values TEXT USAGE)."
  (let ((text "") (result nil) (agent nil) (seen nil) (line nil))
    (loop
      (setf line (read-line stream nil nil))
      (when (null line) (return))
      (let ((event (handler-case (nlk:decode-json line) (error () nil))))
        (when (hash-table-p event)
          (let ((kind (nlk:json-value event :string "event")))
            (cond ((equal kind "init")
                   (setf seen t
                         agent (nlk:json-value event :string "init" "agent")))
                  ((equal kind "step_update")
                   (let ((step (gethash "step_update" event)))
                     (when (equal (nlk:json-value step :string "step_type") "agent_response")
                       (let ((delta (nlk:json-value step :string "text_delta")))
                         (when delta
                           (setf text (concatenate 'string text delta)))))))
                  ((equal kind "result")
                   (setf result (gethash "result" event))))))))
    (check-agent seen agent log)
    (unless (hash-table-p result)
      (error 'agy-error :message "agy ended before it answered"))
    (let ((status (nlk:json-value result :string "status")))
      (unless (equal status "SUCCESS")
        (error 'agy-error :message (format nil "agy answered ~a: ~a"
                                           (or status "no status")
                                           (or (nlk:json-value result :string "error") "no detail")))))
    (let ((response (nlk:json-value result :string "response")))
      (values (or response text)
              (usage-of (or (gethash "usage" result) (nlk:make-json-object)))))))

(defun run-agy (model system prompt &key (timeout 300))
  "One round through the operator's agy: (values TEXT USAGE). Signals
AGY-ERROR when the CLI is missing, refuses, falls back to its own agent, or
runs past TIMEOUT."
  (unless (agy-available-p)
    (error 'agy-error :message (format nil "no ~a on PATH: install the Antigravity CLI, run it once to sign in, or set the command in the antigravity-agy section"
                                       (first (command-argv)))))
  (multiple-value-bind (root workspace log) (scratch-workspace)
    (unwind-protect
         (round-in root workspace log model system prompt timeout)
      (ignore-errors (nlk:remove-tree root)))))

(defun round-in (root workspace log model system prompt timeout)
  "ROOT's workspace as the one round's place: write the agent, run agy, read it."
  (declare (ignore root))
  (let ((agent-file (merge-pathnames (format nil ".agents/agents/~a/agent.md" +agent-name+) workspace)))
    (with-open-file (out agent-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (agent-markdown system) out)))
  (let* ((argv (append (command-argv)
                        (list "--input-format" "stream-json" "--output-format" "stream-json"
                              "--model" (native-model model) "--agent" +agent-name+
                              "--log-file" (uiop:native-namestring log)
                              "--disable-slash-commands")))
         (paths (multiple-value-list
                 (nlk:spawn-program argv :directory workspace :input :stream
                                         :output :stream :error-output log))))
    (destructuring-bind (pid out-stream err-stream process in-stream) paths
      (declare (ignore err-stream process))
      (unwind-protect
           (progn
             (write-line (user-event prompt) in-stream)
             (finish-output in-stream)
             (close in-stream)
             (handler-case
                 (sb-sys:with-deadline (:seconds timeout)
                   (read-events out-stream log))
               (sb-sys:deadline-timeout ()
                 (error 'agy-error :message (format nil "agy did not answer within ~ds" timeout)))))
        (ignore-errors (nlk:kill-tree pid))
        (ignore-errors (close out-stream))))))
