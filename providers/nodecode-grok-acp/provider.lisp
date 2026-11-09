;;;; provider.lisp --- what grok-acp is, and how one round runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A round is one session/prompt (acp.lisp), and the CLI has no
;;;; client-supplied tool list on a session: so the round's tools ride the
;;;; prompt as OpenAI function schemas, the model answers with
;;;; <tool_call>{...}</tool_call> blocks in its text, and this file parses
;;;; them back into the round's tool calls -- Nodecode runs the tools, as in
;;;; every other lane. The CLI's own tools run in its own process, under the
;;;; account that owns the login, as the original's did.

(in-package #:nodecode-grok-acp)

;;; --- the prompt ---------------------------------------------------------------

(defparameter +preamble+
  (format nil "~{~a~^~%~}"
          '("You are operating as a model behind an external harness: your own"
            "tools are off, and every tool call and file operation is performed"
            "on the host side."
            "If you need a tool, emit ONLY <tool_call>{...}</tool_call> blocks in"
            "your text output, each a JSON object with 'id', 'type': 'function'"
            "and 'function': {'name': '...', 'arguments': '<JSON encoded string>'}."
            "After emitting tool calls, STOP generating; never simulate a tool result."))
  "What every prompt tells the CLI, so its calls come back as text blocks this
cell parses instead of steps the CLI takes itself. The same contract the
plugin this rewrites inherited from its harness.")

(defun part-text (part)
  "The text of one content part: a string, or an object carrying text."
  (typecase part
    (string part)
    (hash-table (or (nlk:json-value part :string "text")
                    (nlk:json-value part :string "content")
                    ""))
    (t "")))

(defun content-text (content)
  "CONTENT, a message's content, as text."
  (typecase content
    (null "")
    (string content)
    (hash-table (part-text content))
    (vector (format nil "~{~a~^~%~}" (map 'list #'part-text content)))
    (list (format nil "~{~a~^~%~}" (mapcar #'part-text content)))
    (t (princ-to-string content))))

(defun message-text (message)
  "One message as a prompt line: its role label and its text."
  (let ((role (or (nlk:json-value message :string "role") "user"))
        (text (content-text (gethash "content" message))))
    (format nil "~@(~a~): ~a" role text)))

(defun tool-text (tools)
  "The round's tool definitions, as the prompt states them."
  (when (and tools (plusp (length tools)))
    (format nil "Tools available (name, description, parameters):~%~a"
            (with-output-to-string (out)
              (loop for tool across tools
                    for function = (or (gethash "function" tool) tool)
                    do (format out "- ~a: ~a ~a~%"
                               (or (nlk:json-value function :string "name") "?")
                               (or (nlk:json-value function :string "description") "")
                               (or (let ((parameters (gethash "parameters" function)))
                                     (and parameters (nlk:encode-json-object parameters)))
                                   "")))))))

(defun round-prompt (context)
  "The whole prompt one round hands the CLI: the system prompt, the tools,
the preamble and the conversation, in that order."
  (let ((messages (coerce (nle::compiled-turn-context-messages context) 'list))
        (tools (nle::context-request-tools context)))
    (format nil "~a~%~%~@[~a~%~]~a~%~%~{~a~^~%~}~%"
            (nle::compiled-turn-context-system-prompt context)
            (tool-text tools)
            +preamble+
            (mapcar #'message-text messages))))

;;; --- the fold ------------------------------------------------------------------

(defstruct (scanner (:constructor make-scanner ()))
  "The text one round is given, and the tool blocks inside it: BUFFER holds
what may still be half a marker or half a block, CALLS the calls closed so far."
  (buffer "")
  (inside nil)
  (calls '()))

(defun parse-tool-block (body)
  "The (NAME . ARGUMENTS) one <tool_call> body names, or NIL when it names
no call."
  (let* ((object (ignore-errors (nlk:decode-json body)))
         (function (and (hash-table-p object) (gethash "function" object)))
         (name (and (hash-table-p function) (nlk:json-value function :string "name")))
         (arguments (and (hash-table-p function) (gethash "arguments" function))))
    (when (and (stringp name) (plusp (length name)))
      (cons name (cond ((stringp arguments) arguments)
                       ((hash-table-p arguments) (nlk:encode-json-object arguments))
                       (t "{}"))))))

(defun take-call (scanner body)
  "Parse BODY, a tool block just closed, into SCANNER's calls."
  (let ((call (parse-tool-block body)))
    (when call (push call (scanner-calls scanner)))))

(defun open-marker-hold (text)
  "How many characters at the end of TEXT are a proper prefix of the open
marker -- text no answer may show yet, because it may become the marker."
  (loop for length from (min (1- (length +tool-open+)) (length text)) downto 1
        when (string= text +tool-open+ :start1 (- (length text) length) :end1 (length text)
                              :start2 0 :end2 length)
          do (return length)
        finally (return 0)))

(defun scanner-feed (scanner chunk)
  "Add CHUNK to SCANNER and answer the text of it that is the answer's own:
everything outside a tool block, less a partial marker held back."
  (setf (scanner-buffer scanner) (concatenate 'string (scanner-buffer scanner) chunk))
  (let ((out (make-string-output-stream)))
    (loop
      (let ((buffer (scanner-buffer scanner)))
        (if (scanner-inside scanner)
            (let ((end (search +tool-close+ buffer)))
              (unless end (return))
              (take-call scanner (subseq buffer 0 end))
              (setf (scanner-buffer scanner)
                    (subseq buffer (+ end (length +tool-close+)))
                    (scanner-inside scanner) nil))
            (let ((begin (search +tool-open+ buffer)))
              (if begin
                  (progn (write-string (subseq buffer 0 begin) out)
                         (setf (scanner-buffer scanner) (subseq buffer (+ begin (length +tool-open+)))
                               (scanner-inside scanner) t))
                  (let ((hold (open-marker-hold buffer)))
                    (write-string (subseq buffer 0 (- (length buffer) hold)) out)
                    (setf (scanner-buffer scanner) (subseq buffer (- (length buffer) hold)))
                    (return)))))))
    (get-output-stream-string out)))

(defun scanner-flush (scanner)
  "The text SCANNER still holds once the round ends: a block left open is a
call anyway, and anything else is the answer's last words."
  (let ((buffer (scanner-buffer scanner)))
    (setf (scanner-buffer scanner) "")
    (if (scanner-inside scanner)
        (progn (take-call scanner buffer) "")
        buffer)))

(defun emit-text (asm text)
  "Say TEXT as the answer's own text, and keep it for the message."
  (when (plusp (length text))
    (write-string text (nle::lane-assembly-content asm))
    (nle::lifecycle-text-delta asm "grok-acp-text" text)))

;;; --- one round ----------------------------------------------------------------

(defun round-directory ()
  "Where the CLI runs for this round: the session's own directory, so its
tools work in the folder the conversation does; NIL outside a turn."
  (let* ((turn (nle:turn))
         (cwd (and turn (nlk:find-session-cwd (getf turn :session-id)))))
    (and cwd (ignore-errors (probe-file (uiop:ensure-directory-pathname cwd))))))

(defun call-grok-streaming (context &key (on-part nle::*turn-part-fn*))
  "One round on grok-acp: spawn the CLI, ask one session/prompt, and fold its
streamed text -- and the <tool_call> blocks in it -- into the round's
message, the answer's text and the calls Nodecode then runs."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (effort (or (nle::effective-provider-config-reasoning-effort config)
                     (setting :effort)))
         (prompt (round-prompt context))
         (asm (nle::make-lane-assembly :on-part on-part))
         (scanner (make-scanner)))
    (nle::emit-stream-part on-part :stream-start)
    (run-acp-round (setting :command) (setting :args) (round-directory) prompt
                    :model model
                    :effort (and (stringp effort) effort)
                    :on-text (lambda (text) (emit-text asm (scanner-feed scanner text))))
    (emit-text asm (scanner-flush scanner))
    (let ((calls (reverse (scanner-calls scanner))))
      (loop for (name . arguments) in calls
            for index from 0
            do (nle::open-tool-buffer asm index
                                       :id (format nil "call-~d" index)
                                       :name name :arguments arguments))
      (nle::assembly-close-spans asm :order '(:tools :text))
      (let* ((full (get-output-stream-string (nle::lane-assembly-content asm)))
             (message (nlk:json-object "role" "assistant"
                                        "content" (if (string= full "") :null full))))
        (when calls
          (setf (gethash "tool_calls" message)
                (map 'vector (lambda (pair &aux (buffer (cdr pair)))
                               (nle::chat-tool-call-object (or (getf buffer :id) "")
                                                            (getf buffer :name)
                                                            (getf buffer :arguments)))
                     (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car))))
        (nle::emit-stream-part on-part :finish)
        (values message nil (if calls "tool_calls" "stop") nil)))))
