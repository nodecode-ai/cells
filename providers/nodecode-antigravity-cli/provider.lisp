;;;; provider.lisp --- what the Antigravity CLI is, and how one round runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from the original plugin's prompt.py, stream.py and client.py:
;;;; a turn is one `agy' child, its prompt on standard input, its
;;;; `--output-format stream-json' events on standard output.
;;;;
;;;; agy's events are one JSON object a line:
;;;;   {"event": "init",        "init": {"conversation_id": "..."}}
;;;;   {"event": "step_update", "step_update": {"text_delta": "...", "usage": {...}}}
;;;;   {"event": "result",      "result": {"status": "success"}}
;;;; A step whose type is `tool' is the CLI taking a tool into its own hands;
;;;; like the original, such a round is closed rather than trusted.

(in-package #:nodecode-antigravity-cli)

;;; --- the CLI -------------------------------------------------------------------

(defun agy-command ()
  "The CLI to drive: the section's `command', a name on PATH or a path."
  (let ((command (setting :command)))
    (or (and (plusp (length command)) command) "agy")))

(defun agy-argv (model effort)
  "The argv for one round: stream-json out, no slash commands, the model and
its effort when the section names them."
  `("--output-format" "stream-json" "--disable-slash-commands"
    ,@(when (and model (plusp (length model))) (list "--model" model))
    ,@(when (and effort (plusp (length effort))) (list "--effort" effort))))

(defun cli-present-p (command)
  "Whether COMMAND names a file: a path as it stands, or a name on PATH."
  (if (find #\/ command)
      (and (probe-file command) t)
      (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                 :separator (list #\:))
            for found = (and (plusp (length directory))
                             (probe-file (merge-pathnames
                                          command
                                          (uiop:ensure-directory-pathname directory))))
            when found do (return found))))

(defun run-agy-process (command argv prompt)
  "Run the CLI once with PROMPT on its standard input; answer its stdout.
The one place a child process starts, and the seam every test stubs."
  (uiop:run-program (cons command argv)
                     :input (make-string-input-stream prompt)
                     :output :string
                     :error-output :string
                     :ignore-error-status t))

(defvar *agy-runner* #'run-agy-process
  "The round's runner, for a caller that wants its own: COMMAND, ARGV and
PROMPT in, the CLI's stdout out. A round calls RUN-AGY-PROCESS by symbol, so
a test stubs the fdefinition and no process starts.")

;;; --- the prompt ----------------------------------------------------------------

(defun part-text (part)
  "The text of one content part: a string, or an object carrying `text'."
  (typecase part
    (string part)
    (hash-table (or (nlk:json-value part :string "text")
                    (nlk:json-value part :string "content")
                    ""))
    (t "")))

(defun content-text (content)
  "CONTENT, a message's content, as text: a string, a part, or a list of parts."
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
  (let* ((messages (coerce (nle::compiled-turn-context-messages context) 'list))
         (tools (nle::context-request-tools context)))
    (format nil "~a~%~%~@[~a~%~]~a~%~%~{~a~^~%~}~%"
            (nle::compiled-turn-context-system-prompt context)
            (tool-text tools)
            +preamble+
            (mapcar #'message-text messages))))

;;; --- the fold ------------------------------------------------------------------

(defun parse-tool-block (block)
  "The (NAME . ARGUMENTS) calls in BLOCK, the text of one <tool_call> block."
  (let ((calls '()))
    (loop with start = 0
          for begin = (search "<tool_call>" block :start2 start)
          while begin
          for end = (search "</tool_call>" block :start2 (+ begin 11))
          do (let* ((body (subseq block (+ begin 11) (or end (length block))))
                    (object (ignore-errors (nlk:decode-json body)))
                    (function (and (hash-table-p object) (gethash "function" object)))
                    (name (and (hash-table-p function) (nlk:json-value function :string "name")))
                    (arguments (and (hash-table-p function) (gethash "arguments" function))))
               (when name
                 (push (cons name (if (stringp arguments)
                                      arguments
                                      (or (and arguments (nlk:encode-json-object arguments)) "{}")))
                       calls))
               (setf start (if end (+ end 12) (length block))))
          while end)
    (nreverse calls)))

(defun tool-call-prefix-length (text)
  "How many characters at the end of TEXT could still become `<tool_call>'."
  (loop for length from (min 10 (length text)) downto 1
        when (string= text (subseq text (- (length text) length))
                      :start2 0 :end2 length)
          do (return length)               ; not the fast path; refined below
        finally (return 0)))

(defun partial-open-p (text)
  "How many trailing characters of TEXT are a proper prefix of `<tool_call>'."
  (let ((open "<tool_call>"))
    (loop for length from (min (1- (length open)) (length text)) downto 1
          when (string= text open :start1 (- (length text) length) :end1 (length text)
                                  :start2 0 :end2 length)
            do (return length)
          finally (return 0))))

(defun emit-text (asm text)
  "Say TEXT as the answer's own text, and keep it for the next round."
  (when (plusp (length text))
    (write-string text (nle::lane-assembly-content asm))
    (nle::lifecycle-text-delta asm "agy-text" text)))

(defun fold-round (asm events)
  "Fold the CLI's EVENT lines into ASM: text out as it streams, the tool
blocks parsed at their close, and the calls the answer made."
  (let ((pending "") (inside nil) (calls '()))
    (flet ((take-block (text)
             (setf calls (nconc calls (parse-tool-block text)))))
      (dolist (event events)
        (when (equal "step_update" (nlk:json-value event :string "event"))
          (let* ((step (gethash "step_update" event))
                 (delta (and (hash-table-p step) (nlk:json-value step :string "text_delta"))))
            (when (and (hash-table-p step)
                       (equal "tool" (nlk:json-value step :string "step_type")))
              (return))                     ; a native tool step: close the round here
            (when (and delta (plusp (length delta)))
              (setf pending (concatenate 'string pending delta))
              (loop
                (if inside
                    (let ((end (search "</tool_call>" pending)))
                      (if end
                          (let ((block (subseq pending 0 (+ end 12))))
                            (setf pending (subseq pending (+ end 12)) inside nil)
                            (take-block block))
                          (return)))
                    (let ((begin (search "<tool_call>" pending)))
                      (if begin
                          (progn (emit-text asm (subseq pending 0 begin))
                                 (setf pending (subseq pending begin) inside t))
                          (let ((hold (partial-open-p pending)))
                            (emit-text asm (subseq pending 0 (- (length pending) hold)))
                            (setf pending (subseq pending (- (length pending) hold)))
                            (return))))))))))
      (when (plusp (length pending))
        (if inside (take-block pending) (emit-text asm pending))))
    calls))

(defun call-agy-streaming (context &key (on-part nle::*turn-part-fn*))
  "One round on the Antigravity CLI: spawn it with the round's prompt, fold
its stream-json events, and answer the message, the usage (the CLI reports
none here), the finish reason and the request the CLI was handed."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (effort (or (nle::effective-provider-config-reasoning-effort config) ""))
         (prompt (round-prompt context))
         (asm (nle::make-lane-assembly :on-part on-part)))
    (nle::emit-stream-part on-part :stream-start)
    (let* ((output (run-agy-process (agy-command) (agy-argv model effort) prompt))
           (events (loop for line in (uiop:split-string (or output "") :separator '(#\Newline))
                         for text = (string-trim '(#\Space #\Tab #\Return) line)
                         when (and (plusp (length text)) (char= (char text 0) #\{))
                           collect (or (ignore-errors (nlk:decode-json text)) (nlk:json-object))))
           (calls (fold-round asm events)))
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
                (map 'vector (lambda (pair &aux (buf (cdr pair)))
                               (nle::chat-tool-call-object (or (getf buf :id) "")
                                                            (getf buf :name)
                                                            (getf buf :arguments)))
                     (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car))))
        (nle::emit-stream-part on-part :finish)
        (values message nil (if calls "tool_calls" "stop") nil)))))
