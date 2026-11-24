;;;; turn.lisp --- one Nodecode turn, run in the agent.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A turn on acp/<agent> is one session/prompt. The agent answers with
;;;; session/update notifications until the prompt's reply ends the turn, and
;;;; the turn thread folds each into what every surface already draws:
;;;;
;;;;   agent_message_chunk   text, streamed live (item_delta) and kept as the
;;;;                         assistant's message
;;;;   agent_thought_chunk   thinking, the same way; a plan is thinking too
;;;;   tool_call(_update)    a call named `acp': live while it runs, a durable
;;;;                         call and result once it ends (or once it asks to
;;;;                         run), the files it changed as the result's
;;;;                         workspace diff, its words the shell's own
;;;;                         (ACTIVITY, cell.lisp), and the agent's own
;;;;                         words for it as its `description' (CALL-WORDS)
;;;;   usage_update / reply  the turn's usage: tokens, the agent's own cost
;;;;
;;;; A session/request_permission is the question card the page and the
;;;; desktop app put up (NLE:ASK-OPERATOR): its options are the card's
;;;; choices, and no answer -- nobody there, or the wait out -- is the
;;;; agent's own `no'. Stop is session/cancel: the agent ends the prompt as
;;;; cancelled, and what it said and did up to then is kept.

(in-package #:nodecode-acp)

;;; --- the turn's queue: what the reader thread hands the turn thread ---------

(defstruct (queue (:constructor make-queue ()))
  (lock (bt2:make-lock :name "acp queue"))
  (items '())
  (semaphore (bt2:make-semaphore :name "acp queue")))

(defun enqueue (queue item)
  (bt2:with-lock-held ((queue-lock queue))
    (setf (queue-items queue) (nconc (queue-items queue) (list item))))
  (bt2:signal-semaphore (queue-semaphore queue)))

(defun dequeue (queue seconds)
  "The next item, waiting up to SECONDS; NIL when none came."
  (when (bt2:wait-on-semaphore (queue-semaphore queue) :timeout seconds)
    (bt2:with-lock-held ((queue-lock queue))
      (pop (queue-items queue)))))

;;; --- the fold ------------------------------------------------------------------

(defstruct (call (:constructor make-call (id)))
  "One tool call the agent made."
  (id "" :type string)
  (title nil) (kind nil) (status nil) (raw-input nil) (content nil)
  (locations nil) (raw-output nil)
  (index nil)                           ; its live row in the round in flight
  (recorded nil) (closed nil)
  (store nil) (tree nil))               ; the workspace bracket it opened

(defstruct (fold (:constructor make-fold (turn session agent conn agent-session directory on-part)))
  turn session agent conn agent-session directory on-part
  (text (make-string-output-stream)) (text-span nil)
  (thought (make-string-output-stream)) (thought-span nil)
  (spans 0) (indexes 0) (said nil) (fresh nil) (round-open nil)
  (calls (make-hash-table :test 'equal)) (order '())
  (permissions '()) (plan nil)
  (cost nil) (model nil) (last-error nil))

(defun emit (fold kind &rest initargs)
  (apply #'nle::emit-stream-part (fold-on-part fold) kind initargs))

(defun open-round (fold)
  "The round in flight, opened when it is not: what streams after a durable
record is a new round to every surface, as a provider's next round is."
  (unless (fold-round-open fold)
    (setf (fold-round-open fold) t)
    (emit fold :stream-start)))

(defun next-span (fold)
  (format nil "acp-~d" (incf (fold-spans fold))))

(defun close-text (fold)
  (when (fold-text-span fold)
    (emit fold :text-end :id (shiftf (fold-text-span fold) nil))))

(defun close-thought (fold)
  (when (fold-thought-span fold)
    (emit fold :reasoning-end :id (shiftf (fold-thought-span fold) nil))))

(defun said (fold text)
  "TEXT the agent said, streamed and kept."
  (when (plusp (length text))
    (open-round fold)
    (close-thought fold)
    (unless (fold-text-span fold)
      (setf (fold-text-span fold) (next-span fold))
      (emit fold :text-start :id (fold-text-span fold)))
    (emit fold :text-delta :id (fold-text-span fold) :text text)
    (setf (fold-said fold) t (fold-fresh fold) t)
    (write-string text (fold-text fold))))

(defun thought (fold text)
  "TEXT the agent thought, streamed and kept."
  (when (plusp (length text))
    (open-round fold)
    (close-text fold)
    (unless (fold-thought-span fold)
      (setf (fold-thought-span fold) (next-span fold))
      (emit fold :reasoning-start :id (fold-thought-span fold)))
    (emit fold :reasoning-delta :id (fold-thought-span fold) :text text)
    (setf (fold-fresh fold) t)
    (write-string text (fold-thought fold))))

(defun block-text (block)
  "The text of one ACP content block, or NIL."
  (when (hash-table-p block)
    (let ((type (gethash "type" block)))
      (cond ((equal type "text") (let ((text (gethash "text" block))) (and (stringp text) text)))
            ((equal type "resource_link")
             (let ((uri (gethash "uri" block))) (and (stringp uri) uri)))
            ((equal type "resource")
             (let* ((resource (gethash "resource" block))
                    (text (and (hash-table-p resource) (gethash "text" resource))))
               (and (stringp text) text)))))))

;;; --- calls -------------------------------------------------------------------

(defparameter +reading-kinds+ '("read" "search" "think" "fetch")
  "Kinds that change no file: no workspace bracket opens for them.")

(defun merge-call (fold update)
  "The call UPDATE (a ToolCall or ToolCallUpdate) names, with its fields laid on."
  (let* ((id (gethash "toolCallId" update))
         (call (or (gethash id (fold-calls fold))
                   (let ((new (make-call id)))
                     (setf (gethash id (fold-calls fold)) new)
                     (setf (fold-order fold) (append (fold-order fold) (list new)))
                     new))))
    (flet ((field (key setter)
             (multiple-value-bind (value present) (gethash key update)
               (when (and present (not (eq value :null)))
                 (funcall setter value)))))
      (field "title" (lambda (v) (when (stringp v) (setf (call-title call) v))))
      (field "kind" (lambda (v) (when (stringp v) (setf (call-kind call) v))))
      (field "status" (lambda (v) (when (stringp v) (setf (call-status call) v))))
      (field "rawInput" (lambda (v) (when (and (hash-table-p v) (plusp (hash-table-count v)))
                                      (setf (call-raw-input call) v))))
      (field "content" (lambda (v) (when (vectorp v) (setf (call-content call) v))))
      (field "locations" (lambda (v) (when (vectorp v) (setf (call-locations call) v))))
      (field "rawOutput" (lambda (v) (setf (call-raw-output call) v))))
    call))

(defun input-text (call &rest keys)
  "The first string among KEYS in CALL's raw input."
  (let ((input (call-raw-input call)))
    (and (hash-table-p input)
         (loop for key in keys
               for value = (gethash key input)
               thereis (and (stringp value) (plusp (length value)) value)))))

(defun call-path (call)
  "The file CALL works on: its first location, else a path in its input, else
the path of a diff it carries."
  (or (loop for location across (or (call-locations call) #())
            for path = (and (hash-table-p location) (gethash "path" location))
            thereis (and (stringp path) path))
      (input-text call "file_path" "path" "filePath" "notebook_path")
      (loop for item across (or (call-content call) #())
            thereis (and (hash-table-p item) (equal (gethash "type" item) "diff")
                         (stringp (gethash "path" item)) (gethash "path" item)))))

(defun call-new-file-p (call)
  "Whether CALL writes a file that was not there: a diff whose old text is null."
  (loop for item across (or (call-content call) #())
        thereis (and (hash-table-p item) (equal (gethash "type" item) "diff")
                     (eq (gethash "oldText" item :null) :null))))

(defparameter +words-limit+ 120
  "The longest an agent's words for one call stand on its row, in characters.")

(defun one-line (text)
  "TEXT's first non-blank line, trimmed and clipped to +WORDS-LIMIT+, or NIL."
  (let ((line (and (stringp text)
                   (find-if (lambda (line) (plusp (length line)))
                            (mapcar (lambda (line) (string-trim '(#\Space #\Tab #\Return) line))
                                    (uiop:split-string text :separator '(#\Newline)))))))
    (cond ((null line) nil)
          ((<= (length line) +words-limit+) line)
          (t (concatenate 'string (string-right-trim " " (subseq line 0 (1- +words-limit+))) "…")))))

(defun call-words (call path)
  "The agent's own words for CALL, one line, or NIL: the description its input
gives (Claude Code writes one for every command and every subagent), else its
title when the shell has none of its own -- no PATH, command, pattern or url,
as a plan, a skill or a tool of the agent's own (`Update TODOs: ...')."
  ;; Carried as the call's `description', which every surface reads as its
  ;; note in any process: a row says what the agent said it does, never the
  ;; bare tool name, even in a shell that lays no cell's words.
  (or (one-line (input-text call "description" "command_description"))
      (and (not (or path (input-text call "command" "cmd" "pattern" "query" "url")))
           (one-line (call-title call)))))

(defun call-arguments (fold call)
  "CALL as its `acp' call's arguments: what the shell's words are read from."
  (let* ((input (call-raw-input call))
         (encoded (and input (nlk:encode-json-object input)))
         (path (call-path call))
         (words (call-words call path)))
    (nlk:encode-json-object
     (nle::json-object
      "agent" (agent-title (fold-agent fold))
      "title" (or (call-title call) "")
      "kind" (or (call-kind call) "other")
      :when words "description" words
      :when (input-text call "command" "cmd") "command" (input-text call "command" "cmd")
      :when path "path" (relative-path fold path)
      :when (input-text call "pattern" "query") "pattern" (input-text call "pattern" "query")
      :when (input-text call "url") "url" (input-text call "url")
      :when (call-new-file-p call) "new" t
      ;; The input itself where it is small: a written file's whole text is
      ;; the diff's to show, not the call's.
      :when (and encoded (<= (length encoded) 2000)) "input" input))))

(defun relative-path (fold path)
  "PATH under the session's folder as the folder's own relative path."
  (let ((root (namestring (uiop:ensure-directory-pathname (fold-directory fold)))))
    (if (and (stringp path) (uiop:string-prefix-p root path))
        (subseq path (length root))
        path)))

(defun show-call (fold call)
  "CALL live in the round in flight, under its own row."
  (open-round fold)
  (close-text fold)
  (close-thought fold)
  (unless (call-index call)
    (setf (call-index call) (incf (fold-indexes fold)))
    (emit fold :tool-input-start :id (call-id call) :index (call-index call) :tool-name "acp"))
  (emit fold :tool-input-delta :id (call-id call) :index (call-index call) :tool-name "acp"
             :arguments (call-arguments fold call)))

(defun open-bracket (fold call)
  "Walk the session's folder before CALL changes it, so its result can carry
what it changed."
  (unless (or (call-store call) (member (call-kind call) +reading-kinds+ :test #'equal))
    (multiple-value-bind (store tree)
        (ignore-errors (nlk:open-workspace-bracket (fold-turn fold) :label (call-id call)))
      (setf (call-store call) store (call-tree call) tree))))

(defun close-bracket (call)
  "What CALL changed in the folder, as a workspace_diff document, or NIL."
  (when (and (call-store call) (call-tree call))
    (nth-value 1 (ignore-errors (nlk:close-workspace-bracket (call-store call) (call-tree call))))))

(defun take-text (fold)
  (get-output-stream-string (fold-text fold)))

(defun take-thought (fold)
  (get-output-stream-string (fold-thought fold)))

(defun close-round (fold calls)
  "Record what streamed since the last record as one assistant message, with
CALLS its tool calls, and each call's call fact. => the message."
  (close-text fold)
  (close-thought fold)
  (setf (fold-fresh fold) nil)
  (let* ((turn (fold-turn fold))
         (text (take-text fold))
         (thinking (take-thought fold))
         (message (nle::json-object
                   "role" "assistant"
                   "content" (if (and calls (string= text "")) :null text)
                   :when (plusp (length thinking)) "reasoning_content" thinking
                   :when calls "tool_calls"
                   (map 'vector (lambda (call)
                                  (nle::chat-tool-call-object (call-id call) "acp"
                                                              (call-arguments fold call)))
                        calls))))
    (when (fold-round-open fold)
      (emit fold :finish)
      (setf (fold-round-open fold) nil))
    (nle:append-message (fold-session fold) message)
    (when turn
      (nlk:record-assistant-message turn message)
      (dolist (call calls)
        (nlk:record-tool-call turn (call-id call) "acp" (call-arguments fold call))
        (setf (call-recorded call) t)))
    (dolist (call calls) (setf (call-recorded call) t))
    (nle::notify-turn-facts-committed)
    ;; The record closed the round in flight: the calls still running open
    ;; the next one, under rows of their own.
    (setf (fold-indexes fold) 0)
    (dolist (call (fold-order fold))
      (setf (call-index call) nil)
      (unless (or (call-recorded call) (call-closed call))
        (show-call fold call)))
    message))

(defun strip-fence (text)
  "TEXT without one fence around it all (```console ... ```)."
  (let ((trimmed (string-trim '(#\Newline #\Space) text)))
    (if (and (uiop:string-prefix-p "```" trimmed) (uiop:string-suffix-p trimmed "```")
             (find #\Newline trimmed))
        (let ((start (1+ (position #\Newline trimmed)))
              (end (- (length trimmed) 3)))
          (if (<= start end) (string-right-trim '(#\Newline) (subseq trimmed start end)) ""))
        text)))

(defun call-result-text (fold call)
  "What CALL answered, as its result's text."
  (let ((parts '()))
    (when (and (equal (call-status call) "failed") (stringp (call-raw-output call)))
      (return-from call-result-text (strip-fence (call-raw-output call))))
    (loop for item across (or (call-content call) #())
          when (hash-table-p item)
            do (let ((type (gethash "type" item)))
                 (cond ((equal type "content")
                        (let ((text (block-text (gethash "content" item))))
                          (when text (push (strip-fence text) parts))))
                       ((equal type "diff")
                        (push (format nil "~:[Edited~;Wrote~] ~a"
                                      (eq (gethash "oldText" item :null) :null)
                                      (relative-path fold (gethash "path" item)))
                              parts)))))
    (cond (parts (format nil "~{~a~^~%~}" (nreverse parts)))
          ((stringp (call-raw-output call)) (strip-fence (call-raw-output call)))
          ((hash-table-p (call-raw-output call)) (nlk:encode-json-object (call-raw-output call)))
          ((equal (call-status call) "failed") "failed")
          (t "done"))))

(defun finish-call (fold call &key stopped)
  "Record CALL's result: what it answered, failed when it failed, the files it
changed as the result's workspace diff; STOPPED, the stop's document, when
the turn's cancel cut it."
  (unless (call-closed call)
    (unless (call-recorded call) (close-round fold (list call)))
    (let* ((turn (fold-turn fold))
           (document (close-bracket call))
           (text (if stopped "interrupted by user" (call-result-text fold call)))
           (metadata (when (or document stopped)
                       (nle::json-object :when document "workspace_diff" document
                                         :when stopped "stopped" stopped))))
      (when turn
        (nlk:record-tool-result turn (call-id call) "acp" text
                                :failed (and (not stopped) (equal (call-status call) "failed"))
                                :metadata metadata))
      (nle:append-message (fold-session fold)
                          (nle::message "tool" text :name "acp" :tool-call-id (call-id call)))
      (setf (call-closed call) t)
      (nle::notify-turn-facts-committed))))

;;; --- updates -------------------------------------------------------------------

(defun plan-text (entries)
  "A plan's ENTRIES as a checklist."
  (with-output-to-string (out)
    (format out "Plan:~%")
    (loop for entry across entries
          when (hash-table-p entry)
            do (format out "- [~a] ~a~%"
                       (let ((status (gethash "status" entry)))
                         (cond ((equal status "completed") "x")
                               ((equal status "in_progress") "~")
                               (t " ")))
                       (gethash "content" entry "")))))

(defun meta-error (update)
  "An error an agent reports in an update's _meta (Codex's reconnect notes),
or NIL."
  (let ((meta (gethash "_meta" update)))
    (when (hash-table-p meta)
      (loop for value being the hash-values of meta
            for error = (and (hash-table-p value) (gethash "error" value))
            for message = (and (hash-table-p error) (gethash "message" error))
            thereis (and (stringp message)
                         (let ((more (gethash "additionalDetails" error)))
                           (if (stringp more) (format nil "~a (~a)" message more) message)))))))

(defun fold-update (fold update)
  "Fold one session/update into the turn."
  (let ((kind (gethash "sessionUpdate" update)))
    (cond
      ((equal kind "agent_message_chunk")
       (let ((text (block-text (gethash "content" update)))) (when text (said fold text))))
      ((equal kind "agent_thought_chunk")
       (let ((text (block-text (gethash "content" update)))) (when text (thought fold text))))
      ((member kind '("tool_call" "tool_call_update") :test #'equal)
       (when (stringp (gethash "toolCallId" update))
         (let ((call (merge-call fold update)))
           (unless (call-closed call)
             (open-bracket fold call)
             (if (member (call-status call) '("completed" "failed") :test #'equal)
                 (finish-call fold call)
                 (show-call fold call))))))
      ((equal kind "plan")
       (let* ((entries (gethash "entries" update))
              (text (and (vectorp entries) (plan-text entries))))
         (when (and text (not (equal text (fold-plan fold))))
           (setf (fold-plan fold) text)
           ;; Its own paragraph of the thinking it interrupts.
           (thought fold (if (fold-thought-span fold)
                             (format nil "~%~%~a" text)
                             text)))))
      ((equal kind "usage_update")
       (let ((cost (gethash "cost" update))
             (meta (gethash "_meta" update)))
         (when (and (hash-table-p cost) (realp (gethash "amount" cost))
                    (equal (gethash "currency" cost "USD") "USD"))
           (setf (fold-cost fold) (gethash "amount" cost)))
         (when (hash-table-p meta)
           (let ((model (gethash "_claude/model" meta)))
             (when (stringp model) (setf (fold-model fold) model)))))))
    (let ((error (meta-error update)))
      (when error (setf (fold-last-error fold) error)))))

;;; --- permission ----------------------------------------------------------------

(defstruct (permission (:constructor make-permission (respond options thread)))
  respond options thread (done nil))

(defun option-of (options kinds)
  "The first of OPTIONS whose kind is among KINDS."
  (find-if (lambda (option) (member (gethash "kind" option) kinds :test #'equal))
           options))

(defun answer-permission (permission option)
  "Answer PERMISSION with OPTION, or cancelled when OPTION is NIL, once."
  (unless (permission-done permission)
    (setf (permission-done permission) t)
    (funcall (permission-respond permission)
             (if option
                 (nle::json-object "outcome" (nle::json-object "outcome" "selected"
                                                               "optionId" (gethash "optionId" option)))
                 (nle::json-object "outcome" (nle::json-object "outcome" "cancelled"))))))

(defun permission-detail (fold call)
  "The line under a permission card: what the call would run or change."
  (or (let ((command (input-text call "command" "cmd"))) (and command (format nil "$ ~a" command)))
      (let ((path (call-path call)))
        (and path (format nil "~:[Edit~;Create~] ~a" (call-new-file-p call) (relative-path fold path))))
      (let ((input (call-raw-input call)))
        (and input (let ((text (nlk:encode-json-object input)))
                     (if (> (length text) 300) (concatenate 'string (subseq text 0 300) "…") text))))))

(defun ask-function ()
  "NLE:ASK-OPERATOR where this core has it (nodecode#185), else NIL."
  (let ((symbol (find-symbol "ASK-OPERATOR" :nle)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun ask-permission (fold params respond queue)
  "Put the agent's permission request PARAMS up as the question card, on a
thread of its own; its answer comes back through QUEUE."
  (let* ((options (remove-if-not #'hash-table-p (coerce (or (gethash "options" params) #()) 'list)))
         (update (gethash "toolCall" params))
         (call (and (hash-table-p update) (stringp (gethash "toolCallId" update))
                    (merge-call fold update)))
         (ask (ask-function))
         (session-id (nle::session-id (fold-session fold)))
         (labels (remove-duplicates (remove-if-not #'stringp (mapcar (lambda (o) (gethash "name" o)) options))
                                    :test #'string= :from-end t))
         (title (format nil "~a wants to ~a" (agent-title (fold-agent fold))
                        (let ((said (and call (call-title call))))
                          (if (and said (plusp (length said)))
                              (string-downcase said :end 1)
                              "go on"))))
         (detail (and call (permission-detail fold call)))
         (permission (make-permission respond options nil)))
    (when call
      ;; The card is about this call: the transcript shows it, and the
      ;; folder is walked before it may change anything.
      (open-bracket fold call)
      (unless (call-recorded call) (close-round fold (list call))))
    (push permission (fold-permissions fold))
    (if (or (null ask) (null labels))
        (enqueue queue (list :answer permission nil :nobody))
        (setf (permission-thread permission)
              (bt2:make-thread
               (lambda ()
                 (multiple-value-bind (label how)
                     (catch 'withdrawn
                       (funcall ask title :choices labels :detail detail
                                          :seconds (setting :ask-seconds) :session-id session-id))
                   (enqueue queue (list :answer permission label how))))
               :name "acp permission")))))

(defun settle-permission (fold permission label how)
  "Answer PERMISSION with the option LABEL names; with no label, the agent's
own `no' (reject once), and a note of why."
  (declare (ignore fold how))
  (let ((options (permission-options permission)))
    (answer-permission permission
                       (or (and label (find label options :key (lambda (o) (gethash "name" o))
                                                          :test #'equal))
                           (option-of options '("reject_once" "reject_always"))))))

(defun withdraw-permissions (fold)
  "Answer every open permission cancelled and take its card down."
  (dolist (permission (fold-permissions fold))
    (answer-permission permission nil)
    (let ((thread (permission-thread permission)))
      (when (and thread (bt2:thread-alive-p thread))
        (ignore-errors (bt2:interrupt-thread thread (lambda () (throw 'withdrawn (values nil :withdrawn)))))))))

;;; --- the turn ------------------------------------------------------------------

(defun last-user-text (session)
  "The words of SESSION's newest user message: the turn's input as the turn
loop holds it."
  (let ((message (find "user" (reverse (nle::session-messages session))
                       :key (lambda (m) (and (hash-table-p m) (gethash "role" m))) :test #'equal)))
    (when message
      (let ((content (gethash "content" message)))
        (cond ((stringp content) content)
              ((vectorp content)
               (format nil "~{~a~^~%~}"
                       (loop for part across content
                             when (and (hash-table-p part) (stringp (gethash "text" part)))
                               collect (gethash "text" part)))))))))

(defun prompt-blocks (fold)
  "The turn's prompt as ACP content: the user's words, and its pictures where
the agent takes images."
  ;; *INPUT* is RUN's own argument; a turn RUN began before this cell's hook
  ;; was in (the first turn after a boot can) reads it from the session.
  (let* ((text (cond ((stringp *input*) *input*)
                     ((last-user-text (fold-session fold)))
                     (t "")))
         (blocks (list (nle::json-object "type" "text" "text" text))))
    (when (agent-offers-p (fold-conn fold) "promptCapabilities" "image")
      (let ((turn (fold-turn fold)))
        (when turn
          (loop for (nil . images) in (ignore-errors (nle::turn-images (nlk:durable-turn-turn-id turn)))
                do (loop for (type . data) in images
                         do (setf blocks (append blocks (list (nle::json-object "type" "image"
                                                                                 "mimeType" type
                                                                                 "data" data)))))))))
    (coerce blocks 'vector)))

(defun record-usage (fold result)
  "The turn's usage fact: the counts the prompt's reply carries, and the
agent's own cost where it said one."
  (let* ((turn (fold-turn fold))
         (usage (and (hash-table-p result) (gethash "usage" result)))
         (usage (and (hash-table-p usage) usage))
         (stop (and (hash-table-p result) (gethash "stopReason" result))))
    (when turn
      (flet ((count-of (key) (let ((value (and usage (gethash key usage)))) (and (integerp value) value))))
        (apply #'nlk:record-turn-usage turn
               :provider +provider+ :model (agent-name (fold-agent fold))
               :response-model (fold-model fold)
               :finish-reason (and (stringp stop) stop)
               :input-tokens (count-of "inputTokens")
               :output-tokens (count-of "outputTokens")
               :cached-input-tokens (count-of "cachedReadTokens")
               :cache-write-tokens (count-of "cachedWriteTokens")
               :total-tokens (count-of "totalTokens")
               :reasoning-tokens (count-of "thoughtTokens")
               (when (fold-cost fold)
                 (list :cost-known t :cost-usd (coerce (fold-cost fold) 'single-float))))))))

(defun close-cut (fold)
  "Keep what the agent said and did before a cancel or an exit cut it: the
words as a message, each call that never ended stopped."
  (let* ((turn (fold-turn fold))
         (open (remove-if #'call-closed (fold-order fold)))
         (unrecorded (remove-if #'call-recorded open)))
    (when (or (fold-fresh fold) unrecorded)
      (close-round fold unrecorded))
    (dolist (call open)
      (finish-call fold call :stopped (or (and turn (ignore-errors (nle::call-stop-document turn (call-id call))))
                                          (nle::json-object "by" "operator"))))))

(defun run-turn (session)
  "Run the running turn of SESSION in its agent. => AGENT-LOOP's values:
the closing assistant message, and NIL NIL (no steer ended it, nothing was
handed off)."
  (let* ((turn nle::*current-durable-turn*)
         (session-id (nle::session-id session))
         (agent (or (find-agent nle::*model*)
                    (error 'nle::provider-config-error
                           :detail (format nil "no agent ~s: acp offers ~{~a~^, ~}" nle::*model*
                                           (mapcar #'agent-name (setting :agents))))))
         (directory (session-directory session-id))
         (link (handler-case (ensure-link session-id agent directory)
                 (acp-error (condition) (error 'nle::provider-config-error
                                               :detail (acp-error-detail condition)))))
         (conn (link-conn link))
         (agent-session (link-session link))
         (queue (make-queue))
         (fold (make-fold turn session agent conn agent-session directory nle::*turn-part-fn*))
         (cancel-deadline nil)
         (result nil))
    (bt2:with-lock-held ((conn-lock conn))
      (setf (gethash agent-session (conn-sinks conn)) (lambda (event) (enqueue queue event))))
    (unwind-protect
         (let ((box (request conn "session/prompt"
                             (nle::json-object "sessionId" agent-session "prompt" (prompt-blocks fold)))))
           (loop
             (let ((event (dequeue queue 0.2)))
               (cond
                 (event
                  (case (first event)
                    (:update (fold-update fold (second event)))
                    (:permission (if cancel-deadline
                                     (funcall (third event) (nle::json-object "outcome" (nle::json-object "outcome" "cancelled")))
                                     (ask-permission fold (second event) (third event) queue)))
                    (:answer (settle-permission fold (second event) (third event) (fourth event)))
                    (:closed (close-cut fold)
                     (error 'acp-error :detail (second event)))))
                 ((box-done-p box)
                  (setf result (reply-result conn (box-reply box)))
                  (return))))
             (when (and turn (null cancel-deadline) (nlk:turn-cancellation-requested-p turn))
               (setf cancel-deadline (+ (get-internal-real-time) (* 10 internal-time-units-per-second)))
               (withdraw-permissions fold)
               (ignore-errors (notify conn "session/cancel" (nle::json-object "sessionId" agent-session))))
             (when (and cancel-deadline (> (get-internal-real-time) cancel-deadline))
               ;; The agent did not end the prompt: it is stopped, and the next
               ;; turn starts it again on the same session.
               (close-connection conn (format nil "~a did not stop when asked" (agent-title agent)))
               (return)))
           (when cancel-deadline
             (close-cut fold)
             (record-usage fold result)
             (nlk:ensure-turn-not-cancelled turn))
           (dolist (call (remove-if #'call-closed (fold-order fold)))
             (finish-call fold call))
           (when (and (not (fold-said fold)) (null (fold-order fold)) (fold-last-error fold))
             (error 'acp-error :detail (format nil "~a: ~a" (agent-title agent) (fold-last-error fold))))
           (let ((message (close-round fold nil)))
             (record-usage fold result)
             (nle::notify-turn-facts-committed)
             (values message nil nil)))
      (withdraw-permissions fold)
      (bt2:with-lock-held ((conn-lock conn))
        (remhash agent-session (conn-sinks conn))))))

(defun session-directory (session-id)
  "The folder SESSION-ID works in, as the agent is handed it: absolute, no
trailing slash."
  (let* ((cwd (ignore-errors (nlk:find-session-cwd session-id)))
         (path (or (and cwd (ignore-errors (probe-file (uiop:ensure-directory-pathname cwd))))
                   (user-homedir-pathname))))
    (string-right-trim "/" (uiop:native-namestring path))))
