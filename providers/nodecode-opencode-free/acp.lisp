;;;; acp.lisp --- one OpenCode ACP exchange: prompt in, one answer out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A round starts a fresh `opencode acp' speaking ACP's JSON-RPC on stdin
;;;; and stdout. Client capabilities are none (no file reads, no terminal),
;;;; native tool events abort the round, and a permission ask is answered
;;;; "cancelled": the CLI's own tools never run, so every tool call the
;;;; model makes is Nodecode's, requested in text (CONTRACT below). The CLI
;;;; is stopped when the round is over. Only the curated free ids can be
;;;; selected, and both the agent mode and the model must be confirmed by
;;;; the CLI before a prompt is sent.

(in-package #:nodecode-opencode-free)

(defparameter +line-limit+ (* 4 1024 1024)
  "The longest frame the CLI may write on one line.")

(defparameter +contract+
  (concatenate
   'string
   "REMOTE TOOL PROTOCOL: This conversation belongs to Nodecode, not to OpenCode. "
   "All tools described below are REMOTE Nodecode tools. To request one, emit ONLY "
   "<hermes_tool_call>{\"id\":\"unique_call_id\",\"type\":\"function\",\"function\":"
   "{\"name\":\"tool_name\",\"arguments\":\"{...}\"}}</hermes_tool_call> as your final TEXT response. "
   "arguments must be a JSON object encoded as a JSON string. Stop after requesting the tool; "
   "Nodecode executes it AFTER your response and supplies the result in the next transcript. "
   "Do NOT use native OpenCode tools, shell, files, skills, or subagents: those are blocked. "
   "A blocked native tool does not mean the corresponding remote Nodecode tool is unavailable. "
   "Do not claim an action happened before a real tool result exists. If the transcript "
   "already contains the needed tool result, answer from it. If no tool is needed, answer normally.")
  "The prompt every round opens and closes with, carried over from the original bridge.")

(defparameter +open-tag+ "<hermes_tool_call>")
(defparameter +close-tag+ "</hermes_tool_call>")

;;; --- JSON, written by hand ------------------------------------------------

(defun json-escape (text)
  "TEXT with the characters a JSON string may not carry escaped."
  (with-output-to-string (out)
    (loop for char across text
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code char) 32)
                      (format out "\\u~4,'0x" (char-code char))
                      (write-char char out)))))))

(defun json-string (text)
  "TEXT as a JSON string, quotes included."
  (format nil "\"~a\"" (json-escape (princ-to-string text))))

(defun json-id (id)
  "A JSON-RPC id, string or number, as text."
  (if (stringp id) (json-string id) (princ-to-string id)))

;;; --- the process ----------------------------------------------------------

(defstruct (agent (:copier nil))
  process input output)

(defun launch-agent (command args)
  "COMMAND with ARGS, speaking ACP on its stdin and stdout."
  (handler-case
      (let ((process (uiop:launch-program (cons command args)
                                           :input :stream :output :stream :error nil
                                           :element-type 'character)))
        (make-agent :process process
                    :input (uiop:process-info-input process)
                    :output (uiop:process-info-output process)))
    (error (condition)
      (fail "could not start the OpenCode CLI ~a: ~a" command condition))))

(defun stop-agent (agent)
  "Close the CLI's stdin, which ends its loop, and take the process down."
  (ignore-errors (close (agent-input agent)))
  (ignore-errors (uiop:terminate-process (agent-process agent)))
  (ignore-errors (close (agent-output agent))))

(defun read-frame (agent)
  "The next JSON frame the CLI wrote, decoded."
  (let ((line (read-line (agent-output agent) nil nil)))
    (unless line (fail "the OpenCode CLI exited before it answered"))
    (when (> (length line) +line-limit+)
      (fail "the OpenCode CLI wrote a frame over ~d bytes" +line-limit+))
    (handler-case (nlk:decode-json line)
      (error (condition)
        (declare (ignore condition))
        (fail "the OpenCode CLI wrote something that is not one JSON frame")))))

(defun write-frame (agent text)
  (write-line text (agent-input agent))
  (finish-output (agent-input agent)))

(defun send-request (agent id method params)
  (write-frame agent
               (format nil "{\"jsonrpc\":\"2.0\",\"id\":~d,\"method\":~a,\"params\":~a}"
                       id (json-string method) params)))

(defun answer (agent id result-or-error)
  (write-frame agent (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,~a}" (json-id id) result-or-error)))

(defun options-of (option)
  "A config option's values: flat, or grouped one level."
  (let ((values '()))
    (loop for item across (or (nle::json-value option :array "options") #())
          do (cond ((nle::json-value item :string "value")
                    (push (nle::json-value item :string "value") values))
                   (t (loop for child across (or (nle::json-value item :array "options") #())
                            for value = (nle::json-value child :string "value")
                            when value do (push value values)))))
    (nreverse values)))

(defun config-option (session category required)
  "The session's config option for CATEGORY, one of whose values is REQUIRED.
Refuse rather than send a prompt on an option the CLI did not advertise."
  (let ((candidates '()))
    (loop for item across (or (nle::json-value session :array "configOptions") #())
          for id = (nle::json-value item :string "id")
          when (or (and id (string= id category))
                   (equal category (nle::json-value item :string "category")))
            do (push item candidates))
    (or (find-if (lambda (item) (member required (options-of item) :test #'string=)) candidates)
        (fail "OpenCode ACP advertised no ~a option ~s (offered: ~{~a~^, ~}); no prompt was sent"
              category required
              (remove-duplicates (loop for item in candidates append (options-of item)) :test #'string=)))))

(defun selected-value (result id)
  (loop for item across (or (nle::json-value result :array "configOptions") #())
        when (equal id (nle::json-value item :string "id"))
          do (return (nle::json-value item :string "currentValue"))))

(defun handle-server-frame (agent frame text reasoning)
  "Answer what the CLI asks of its client, and collect what it streams."
  (let ((method (nle::json-value frame :string "method"))
        (id (gethash "id" frame)))
    (cond
      ((equal method "session/update")
       (let* ((update (nle::json-value frame :object "params" "update"))
              (kind (and update (nle::json-value update :string "sessionUpdate")))
              (text-chunk (nle::json-value update :string "content" "text")))
         (cond
           ((member kind '("tool_call" "tool_call_update") :test #'equal)
            (fail "OpenCode attempted one of its native tools; this provider only permits Nodecode's own"))
           ((and text-chunk text (equal kind "agent_message_chunk")) (write-string text-chunk text))
           ((and text-chunk reasoning (equal kind "agent_thought_chunk")) (write-string text-chunk reasoning)))))
      ;; A request from the CLI to us: no files, no terminal, no permissions.
      ((not (null id))
       (if (equal method "session/request_permission")
           (answer agent id "\"result\":{\"outcome\":{\"outcome\":\"cancelled\"}}")
           (answer agent id
                   "\"error\":{\"code\":-32601,\"message\":\"native file and terminal operations are unavailable\"}"))))))

(defun request (agent id method params &key text reasoning)
  "Send one request, answering whatever the CLI asks meanwhile; its result."
  (send-request agent id method params)
  (loop
    (let ((frame (read-frame agent)))
      (cond
        ((nle::json-value frame :string "method")
         (handle-server-frame agent frame text reasoning))
        ((equal id (gethash "id" frame))
         (when (nth-value 1 (gethash "error" frame))
           (fail "OpenCode ACP ~a failed (code ~a); the CLI's own error text is not kept"
                 method (nle::json-value frame :any "error" "code")))
         (return (gethash "result" frame)))))))

;;; --- one exchange ---------------------------------------------------------

(defun exchange (model prompt &key (directory (uiop:native-namestring (uiop:getcwd))))
  "One ACP exchange: MODEL gets PROMPT in a session rooted at DIRECTORY.
=> (values TEXT REASONING), the CLI stopped and its pipes closed."
  (unless (approved-model-p model)
    (fail "~a is not one of OpenCode Free's curated models (~{~a~^, ~})" model
          (coerce +free-models+ 'list)))
  (let* ((command (setting :command))
         (args (map 'list (lambda (arg) (princ-to-string arg)) (setting :args)))
         (agent (launch-agent command args))
         (text (make-string-output-stream))
         (reasoning (make-string-output-stream))
         (sequence 0))
    (labels ((next-id () (incf sequence)))
      (unwind-protect
           (progn
             (request agent (next-id) "initialize"
                      (concatenate 'string
                                   "{\"protocolVersion\":1,\"clientCapabilities\":{\"fs\":{\"readTextFile\":false,"
                                   "\"writeTextFile\":false},\"terminal\":false},"
                                   "\"clientInfo\":{\"name\":\"nodecode-opencode-free\",\"version\":\"0.1.0\"}}"))
             (let* ((session (request agent (next-id) "session/new"
                                      (format nil "{\"cwd\":~a,\"mcpServers\":[]}" (json-string directory))))
                    (session-id (nle::json-value session :string "sessionId"))
                    (wanted (model-id model)))
               (unless (and session-id (plusp (length session-id)))
                 (fail "OpenCode ACP returned no session id"))
               (dolist (selection (list (cons (config-option session "mode" "build") "build")
                                        (cons (config-option session "model" wanted) wanted)))
                 (let* ((option (car selection))
                        (value (cdr selection))
                        (id (nle::json-value option :string "id"))
                        (result (request agent (next-id) "session/set_config_option"
                                         (format nil "{\"sessionId\":~a,\"configId\":~a,\"value\":~a}"
                                                 (json-string session-id) (json-string id) (json-string value)))))
                   (unless (equal value (selected-value result id))
                     (fail "OpenCode did not confirm ~a=~s; no prompt was sent" id value))))
               (request agent (next-id) "session/prompt"
                        (format nil "{\"sessionId\":~a,\"prompt\":[{\"type\":\"text\",\"text\":~a}]}"
                                (json-string session-id) (json-string prompt))
                        :text text :reasoning reasoning)
               (let ((said (get-output-stream-string text)))
                 (unless (plusp (length (string-trim '(#\Space #\Tab #\Newline) said)))
                   (fail "OpenCode returned no assistant text"))
                 (values said (get-output-stream-string reasoning)))))
        (stop-agent agent)))))
