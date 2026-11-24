;;;; cell.lisp --- the cell: one lane, the round path under it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The opencode-free provider rides a lane of its own name. A round is
;;;; one prompt to the operator's `opencode acp' process (acp.lisp), and the
;;;; answer comes back as the model's text plus the tool calls it asked for
;;;; in that text: Nodecode keeps the loop, its tools and its approvals, the
;;;; CLI only does inference. Five pieces:
;;;;
;;;;   CALL-OPENCODE-FREE        the lane's entry: it renders the round as
;;;;                              one ACP prompt and parses the answer back
;;;;   :CREDENTIAL                the CLI's own login, which this image
;;;;                              never holds
;;;;   CONFIGURED-PROVIDER-INVENTORY   /models lists opencode-free/<model>
;;;;
;;;; Config, a sibling top-level key next to `claude-code' and `acp':
;;;;   "opencode-free": {"command": "opencode", "args": ["acp"],
;;;;                     "models": ["glm-5.3-flash-free"],
;;;;                     "timeout_seconds": 600}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-opencode-free)

;;; --- JSON the round returns -----------------------------------------------

(defun json-obj (&rest pairs)
  "A JSON object of PAIRS, keys as strings, nil members left out."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          when value do (setf (gethash key object) value))
    object))

(defun join-text (parts)
  "PARTS joined with a newline between them."
  (with-output-to-string (out)
    (loop for (part . more) on parts
          do (write-string (or part "") out)
             (when more (write-char #\Newline out)))))

(defun entries (collection)
  "COLLECTION, a vector or a list, as a list."
  (cond ((null collection) '())
        ((listp collection) collection)
        (t (loop for item across collection collect item))))

(defun names-of (tools)
  "The tool names a round advertises, from its function schemas."
  (loop for wrapper in (entries tools)
        for function = (and (hash-table-p wrapper) (nle::json-value wrapper :object "function"))
        for name = (and function (nle::json-value function :string "name"))
        when name collect name))

(defun arguments-text (function)
  "The call's arguments as the chat wire wants them: a JSON object's text."
  (let ((arguments (nle::json-value function :any "arguments")))
    (typecase arguments
      (string (let ((decoded (handler-case (nlk:decode-json arguments) (error () nil))))
                (unless (hash-table-p decoded)
                  (fail "a remote tool call's arguments are not a JSON object; no tool was dispatched"))
                (nle::encode-json-object decoded)))
      (hash-table (nle::encode-json-object arguments))
      (t (fail "a remote tool call carried no arguments object; no tool was dispatched")))))

(defun parse-tool-calls (text tools choice)
  "TEXT's remote tool calls as chat-wire tool_call objects, and its prose.
A call naming a tool the round did not advertise, a malformed envelope or a
repeat id refuses the round: nothing is dispatched on a guess."
  (let ((allowed (names-of tools))
        (calls '()) (prose '()) (seen '())
        (cursor 0))
    (loop
      (let ((start (search +open-tag+ text :start2 cursor)))
        (if (null start)
            (progn (push (subseq text cursor) prose) (return))
            (let* ((body-start (+ start (length +open-tag+)))
                   (end (search +close-tag+ text :start2 body-start)))
              (push (subseq text cursor start) prose)
              (unless end (fail "an incomplete remote tool-call envelope; no tool was dispatched"))
              (let* ((raw (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (subseq text body-start end)))
                     (body (handler-case (nlk:decode-json raw) (error () nil)))
                     (function (and (hash-table-p body) (nle::json-value body :object "function")))
                     (name (and function (nle::json-value function :string "name")))
                     (id (and (hash-table-p body) (nle::json-value body :string "id"))))
                (unless (and (hash-table-p body)
                             (equal "function" (nle::json-value body :string "type"))
                             name (member name allowed :test #'string=))
                  (fail "an unadvertised or invalid remote tool call; no tool was dispatched"))
                (when (equalp choice "none")
                  (fail "a remote tool call violates tool_choice=none"))
                (when (and (hash-table-p choice) (nle::json-value choice :string "function" "name")
                           (not (equal (nle::json-value choice :string "function" "name") name)))
                  (fail "a remote tool call violates the selected function"))
                (unless (and id (plusp (length id)) (not (member id seen :test #'string=)))
                  (fail "remote tool-call ids must be nonempty and unique"))
                (push id seen)
                (push (json-obj "id" id "type" "function"
                                 "function" (json-obj "name" name "arguments" (arguments-text function)))
                      calls))
              (setf cursor (+ end (length +close-tag+)))))))
    (values (coerce (nreverse calls) 'vector)
            (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (join-text (nreverse prose))))))

;;; --- the round as one prompt ---------------------------------------------

(defun render-content (content)
  "CONTENT as text. A round is text-only: an image or audio part refuses."
  (typecase content
    (null "")
    (string content)
    (hash-table (render-content (vector content)))
    (t (let ((parts (make-string-output-stream)))
         (dolist (part (entries content))
           (cond ((stringp part) (write-string part parts))
                 ((hash-table-p part)
                  (let ((kind (nle::json-value part :string "type"))
                        (text (nle::json-value part :string "text")))
                    (unless (member kind '("text" "input_text" "output_text") :test #'equal :test #'equal)
                      (fail "OpenCode Free is text-only; image and audio parts are not forwarded"))
                    (write-string (or text "") parts)))
                 (t (write-string (princ-to-string part) parts))))
         (get-output-stream-string parts)))))

(defun render-prompt (system messages tools choice)
  "The one ACP prompt a round sends: the system voice, the contract, the
round's tool schemas, and the transcript with its tool results."
  (let ((parts (list +contract+)))
    (when (and system (plusp (length system)))
      (push (format nil "System:~%~a" system) parts))
    (when (plusp (length (entries tools)))
      (push (format nil "Remote Nodecode tools (OpenAI function schemas):~%~a"
                         (nle::encode-json-object (if (listp tools) (coerce tools 'vector) tools)))
            parts))
    (when choice (push (concatenate 'string "Remote tool choice: " (nle::encode-json-object choice)) parts))
    (push "Conversation transcript:" parts)
    (dolist (message (entries messages))
      (let* ((role (string-downcase (or (nle::json-value message :string "role") "context")))
             (label (if (equal role "tool")
                        (format nil "Tool result for ~a"
                                (or (nle::json-value message :string "tool_call_id") "unknown"))
                        (string-capitalize role)))
             (content (render-content (gethash "content" message))))
        (when (plusp (length content))
          (push (format nil "~a:~%~a" label content) parts))
        (let ((calls (nle::json-value message :any "tool_calls")))
          (when (plusp (length (entries calls)))
            (push (format nil "Assistant's prior remote tool requests:~%~a"
                               (nle::encode-json-object (if (listp calls) (coerce calls 'vector) calls)))
                  parts)))))
    (push "Continue from the latest user request, using actual tool results already in the transcript." parts)
    (push +contract+ parts)
    (join-text (nreverse parts))))

;;; --- the lane -------------------------------------------------------------

(defun call-opencode-free (context &key (on-part nle::*turn-part-fn*))
  "The opencode-free lane: one round, one ACP exchange with the OpenCode CLI.
=> (values MESSAGE USAGE FINISH-REASON REQUEST-JSON), the chat-shaped
message the rest of the turn loop runs on. The answer is buffered, not
streamed: the CLI collects it before it replies, as the original does."
  (declare (ignore on-part))
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (system (nle::compiled-turn-context-base-system-prompt context))
         (messages (nle::request-messages context))
         (tools (nle::compiled-turn-context-tools context))
         (choice (nle::effective-provider-config-tool-choice config))
         (prompt (render-prompt system messages tools choice)))
    (multiple-value-bind (text reasoning) (exchange model prompt)
      (multiple-value-bind (calls prose) (parse-tool-calls text tools choice)
        (let* ((message (json-obj "role" "assistant"
                                   "content" prose
                                   "tool_calls" (when (plusp (length calls)) calls)
                                   "reasoning_content" (when (plusp (length reasoning)) reasoning)))
               (request-json (nle::encode-json-octets
                               (json-obj "model" (model-id model)
                                         "messages" (vector (json-obj "role" "user" "content" prompt))))))
          (values message nil (if (plusp (length calls)) "tool_calls" "stop") request-json))))))

;;; --- what the cell installs ------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer: the CLI's own login is the credential, and this
image holds none of it."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential +provider+ :oauth)
      (funcall next op)))

(defun model-rows ()
  "The /models rows: the section's curated free ids."
  (loop for id in (entries (setting :models))
        collect (list :provider-id +provider+
                      :provider-display "OpenCode Free"
                      :model-id id
                      :context-window 131072)))

(defun inventory (next &rest arguments)
  "CONFIGURED-PROVIDER-INVENTORY advice: opencode-free among the providers
that need no key of ours, its curated models among the configured ones."
  (multiple-value-bind (ids models) (apply next arguments)
    (values (sort (adjoin +provider+ ids :test #'string-equal) #'string<)
            (append models (model-rows)))))

(defun executable-p (command)
  "Whether COMMAND names something runnable: a path as itself, else a name
on PATH."
  (if (or (find #\/ command) (find #\\ command))
      (and (probe-file command) t)
      (and (nlk::executable-on-path command) t)))

(defun register-lane ()
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                             :stream-symbol 'call-opencode-free
                             :family :openai-completions
                             :reasoning-carry :reasoning
                             :default-endpoint "http://opencode-free.invalid/v1/chat/completions"
                             :path "/chat/completions")))

(defun unregister-lane ()
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes*
                :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "Register the lane, and say so when there is no OpenCode CLI to drive."
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (unless (executable-p (setting :command))
    (nle:notice (format nil "opencode-free: no ~a on PATH, so opencode-free/ models cannot run; install the OpenCode CLI, run `opencode' once and sign in, then (restart-cells \"nodecode-opencode-free\")"
                        (setting :command))
                :level :warning :key "nodecode-opencode-free")
    (nle:on-stop (lambda () (nle:notice nil :key "nodecode-opencode-free")))))

(nle:define-cell opencode-free
  (:section ("opencode-free")
    (:guide "command is the OpenCode CLI (opencode acp), signed in with `opencode' once; models are what /models lists as opencode-free/<model>")
    ("command" :string :default "opencode"
     :doc "the OpenCode CLI: a name on PATH or a path")
    ("args" :list :default (vector "acp")
     :doc "the arguments that make the CLI speak ACP on stdio")
    ("models" :list :default +free-models+
     :doc "the curated free model ids /models offers under opencode-free/, as the CLI names them after their opencode/ prefix")
    ("timeout_seconds" :integer :default 600 :min 1
     :doc "how long one round may take: a fresh process, the history replayed, the answer collected"))
  (:start #'start)
  (:hook :credential #'credential)
  (:hook 'nle::configured-provider-inventory #'inventory))
