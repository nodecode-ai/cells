;;;; cell.lisp --- a session's work, streamed to one Telegram message.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a plugin for another harness that put one kanban task's
;;;; progress in one Telegram message and edited that message in place. This
;;;; machine has no board: a turn of a session is the unit of work, so a turn
;;;; is what owns one message -- the tool calls as they land, the elapsed
;;;; time, the tokens a second -- and the turn's end rewrites it once, with
;;;; the outcome, and pins it.
;;;;
;;;; THE SEAM. Every fact this reads is already a durable fact on the :FRAME
;;;; point, and NLK:FRAME-FACT hands each one over in the wire shape the
;;;; original's hooks were handed: turn.tool_call_started (a tool's name and
;;;; its arguments), turn.usage (the round's token counts), and
;;;; turn.completed / turn.failed / turn.cancelled (the end, with the
;;;; failure's own words). One hook therefore replaces the original's five.
;;;;
;;;; WHAT IS DIALED. Telegram's Bot API, and nothing else: sendMessage once
;;;; per turn, editMessageText on the same message after it, pinChatMessage
;;;; at the end. A failure is kept in *LAST-ERROR* and swallowed -- a
;;;; Telegram outage may never stall or fail a turn. The token is read from
;;;; the section's own bot_token, else from the environment variable its
;;;; bot_token_env names, so no secret is ever written into this folder.

(in-package #:nodecode-kanban-stream)

(nlk:define-peripheral kanban-stream :not-running t)

;;; --- the wire ---------------------------------------------------------------

(defparameter +api-base+ "https://api.telegram.org"
  "The one host this cell dials: Telegram's Bot API.")

(defparameter +text-limit+ 3800
  "Characters this cell puts in one message: Telegram refuses past 4096, and
the original kept the same headroom.")

(defparameter +detail-limit+ 200
  "Characters of a turn's own ending words a final message repeats, as the
original's summary line did.")

(defparameter +args-limit+ 80
  "Characters of a tool call's arguments a stream line repeats.")

(defvar *token* nil "The resolved bot token, or NIL while the cell is not running.")

(defvar *chat* nil "The chat a stream is posted to, or NIL while the cell is not running.")

(defvar *streams* nil "SESSION-ID -> its STREAM. NIL while the cell is not running.")

(defvar *finished* nil "SESSION-ID|TURN-ID -> T once a turn's final message went out.")

(defvar *streaming* t
  "Whether facts are streamed right now; /kanban-stream off mutes it without
stopping the cell, so a busy machine can be quiet for a while.")

(defvar *last-error* nil "What the last Bot API call failed with, if anything.")

;;; --- one turn's message -------------------------------------------------------

(defstruct (progress (:constructor make-progress (&key session turn label)))
  "One turn's message: the id Telegram gave it, the lines it carries, and
the counters a footer repeats."
  session
  turn
  label
  (message-id nil)
  (history '())
  (current "")
  (tokens 0)
  (started (get-internal-real-time))
  (last-edit nil)
  (edits 0))

(defun seconds-since (mark)
  "Seconds between MARK, an internal real time, and now."
  (if mark
      (/ (float (- (get-internal-real-time) mark)) internal-time-units-per-second)
      0))

(defun session-label (session-id)
  "What a person calls SESSION-ID: the name of the folder it works in, else
the id itself."
  (or (nlk:when-let (cwd (nlk:find-session-cwd session-id))
        (nlk:folder-name cwd))
      session-id))

(defun watched-p (session-id)
  "Whether SESSION-ID is one this cell streams: every session while the
section's sessions list is empty, else only the ones it names."
  (let ((chosen (ignore-errors (setting :sessions))))
    (or (null chosen)
        (member session-id chosen :test #'string=))))

;;; --- Telegram ----------------------------------------------------------------

(defun method-url (method)
  "The Bot API address of METHOD for the token this cell resolved."
  (format nil "~a/bot~a/~a" +api-base+ (or *token* "") method))

(defun api-call (method pairs)
  "POST PAIRS, a plist of the method's own fields, to the Bot API's METHOD.
=> (values ANSWER STATUS): ANSWER the decoded body, both NIL when the call
failed; every failure is kept in *LAST-ERROR*, none is signalled."
  (when (and *token* (plusp (length *token*)))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http :post (method-url method)
                      :headers '(("content-type" . "application/json"))
                      :content (nlk:encode-json-object (apply #'nlk:make-json-object pairs))
                      :timeout (or (setting :timeout-seconds) 5))
          (values (ignore-errors (nlk:decode-json (if (stringp body) body (princ-to-string body))))
                  status))
      (error (condition)
        (setf *last-error* (princ-to-string condition))
        (values nil nil)))))

(defun answer-ok-p (answer)
  "Whether ANSWER, a Bot API body, is the success it says."
  (and (hash-table-p answer) (nlk:json-value answer :boolean "ok")))

(defun answer-message-id (answer)
  "The message id in ANSWER, or NIL."
  (nlk:json-value answer :integer "result" "message_id"))

(defun answer-description (answer)
  "What ANSWER says went wrong, in lower case, or an empty string."
  (string-downcase (or (nlk:json-value answer :string "description") "")))

(defun message-gone-p (answer)
  "Whether ANSWER says the message is there no more -- the one failure worth
posting a fresh message for, as the original did."
  (let ((said (answer-description answer)))
    (and (plusp (length said))
         (or (search "message to edit not found" said)
             (search "message can't be edited" said)
             (search "message_id_invalid" said)))))

(defun send-text (text)
  "Send TEXT to the stream's chat. => the new message's id, or NIL."
  (let ((answer (api-call "sendMessage"
                          (list "chat_id" *chat* "text" text
                                "disable_notification" t))))
    (when (answer-ok-p answer)
      (answer-message-id answer))))

(defun edit-text (message-id text)
  "Rewrite MESSAGE-ID to TEXT. => (values OK GONE-P)."
  (let ((answer (api-call "editMessageText"
                          (list "chat_id" *chat* "message_id" message-id "text" text))))
    (values (answer-ok-p answer) (message-gone-p answer))))

(defun pin-message (message-id)
  "Pin MESSAGE-ID in the chat, without a notification."
  (api-call "pinChatMessage"
            (list "chat_id" *chat* "message_id" message-id "disable_notification" t)))

;;; --- what a message says ------------------------------------------------------

(defun elapsed-text (seconds)
  "SECONDS as a person reads a duration: 42s, 3m 07s."
  (let ((whole (round seconds)))
    (if (< whole 60)
        (format nil "~ds" whole)
        (format nil "~dm ~2,'0ds" (floor whole 60) (mod whole 60)))))

(defun rate-text (stream)
  "The turn's tokens a second, or NIL while a rate says nothing yet."
  (let ((elapsed (seconds-since (progress-started stream))))
    (when (and (>= elapsed 1) (plusp (progress-tokens stream)))
      (format nil "~d tok/s" (round (/ (progress-tokens stream) elapsed))))))

(defun footer-text (stream &key final blocked)
  "The one line under a stream's head: how long, how fast, and how it ended."
  (let ((rate (rate-text stream)))
    (format nil "~a ~a~@[ · ~a~]"
            (cond (final (if blocked "✗" "✅"))
                  (t "🔧"))
            (format nil "⏱ ~a ~a" (elapsed-text (seconds-since (progress-started stream)))
                    (if final "total" "elapsed"))
            rate)))

(defun progress-text (stream &key final blocked detail)
  "What the message says: the head, the footer, then the turn's own lines or
its outcome."
  (let* ((head (format nil "~a ~a — ~a"
                        (cond (final (if blocked "✗" "✅"))
                              (t "🔧"))
                        (progress-label stream)
                        (progress-session stream)))
         (lines (if final
                    (list (format nil "~a~@[ ~a~]"
                                  (if blocked "Blocked." "Done.")
                                  (and detail
                                       (plusp (length detail))
                                       (subseq detail 0 (min (length detail) +detail-limit+)))))
                    (append (last (progress-history stream)
                                  (min (length (progress-history stream))
                                       (or (setting :history) 5)))
                            (list (progress-current stream))))))
    (let ((text (format nil "~a~%~a~%---~%~{~a~^~%~}"
                         head
                         (footer-text stream :final final :blocked blocked)
                         lines)))
      (subseq text 0 (min (length text) +text-limit+)))))

;;; --- the stream's own state ---------------------------------------------------

(defun open-stream (session turn)
  "The stream of SESSION's TURN, opened (and its message not yet sent) when
the turn has none."
  (or (and *streams* (gethash session *streams*))
      (let ((stream (make-progress :session session :turn turn
                                 :label (session-label session))))
        (when *streams* (setf (gethash session *streams*) stream))
        stream)))

(defun line-for (tool-name arguments)
  "One stream line for a call: the tool and the head of what it was given."
  (let ((args (and (stringp arguments) (nlk:trimmed arguments))))
    (format nil "🔍 ~a~@[ ~a~]"
            tool-name
            (and args (plusp (length args))
                 (subseq args 0 (min (length args) +args-limit+))))))

(defun push-line (stream line)
  "Add LINE to STREAM's current line and its rolling history."
  (setf (progress-current stream) line)
  (when line
    (push line (progress-history stream))
    (let ((keep (max 1 (or (setting :history) 5))))
      (when (> (length (progress-history stream)) keep)
        (setf (progress-history stream) (subseq (progress-history stream) 0 keep))))))

(defun deliver (stream &key final blocked detail)
  "Put STREAM's message up, or rewrite the one already up. A message the
chat no longer has is posted afresh; every other failure waits for the next
edit, so a flood never duplicates a stream."
  (let ((text (progress-text stream :final final :blocked blocked :detail detail)))
    (if (progress-message-id stream)
        (multiple-value-bind (ok gone) (edit-text (progress-message-id stream) text)
          (incf (progress-edits stream))
          (when gone
            (setf (progress-message-id stream) (send-text text)))
          ok)
        (progn
          (setf (progress-message-id stream) (send-text text))
          (progress-message-id stream)))
    (setf (progress-last-edit stream) (get-internal-real-time))))

(defun update-stream (stream &key force final blocked detail)
  "Rewrite STREAM's message when the section's throttle allows it, or when
FORCE (or a final frame) says it must go now."
  (let ((waited (seconds-since (progress-last-edit stream))))
    (when (or force final (null (progress-last-edit stream))
              (>= waited (or (setting :throttle-seconds) 5)))
      (deliver stream :final final :blocked blocked :detail detail))))

;;; --- the frame point ----------------------------------------------------------

(defun usage-tokens (payload)
  "The output tokens PAYLOAD reports, whichever spelling carried them."
  (or (nlk:json-value payload :integer "output-tokens")
      (nlk:json-value payload :integer "output_tokens")
      (nlk:json-value payload :integer "outputTokens")
      0))

(defun turn-key (session turn)
  "The one key a finished turn is remembered under."
  (format nil "~a|~a" session (or turn "")))

(defun note-tool (session turn payload)
  "A tool call landed: put it on the session's message, or send the message."
  (let ((stream (open-stream session turn)))
    (push-line stream (line-for (or (nlk:json-value payload :string "tool-name") "tool")
                                (nlk:json-value payload :string "arguments")))
    (if (progress-message-id stream)
        (update-stream stream)
        (deliver stream))))

(defun note-usage (session turn payload)
  "A round closed: add its tokens to the session's rate."
  (let ((stream (and *streams* (gethash session *streams*))))
    (when (and stream (equal (progress-turn stream) turn))
      (incf (progress-tokens stream) (usage-tokens payload))
      (when (progress-message-id stream)
        (update-stream stream)))))

(defun finish-turn (session turn type payload)
  "A turn ended: rewrite its message once with the outcome, pin it, and
forget it."
  (let ((stream (and *streams* (gethash session *streams*))))
    (when stream
      (when *finished* (setf (gethash (turn-key session turn) *finished*) t))
      (deliver stream :final t
               :blocked (not (equal type "turn.completed"))
               :detail (or (nlk:json-value payload :string "detail") ""))
      (when (and (progress-message-id stream) (setting :pin-final))
        (pin-message (progress-message-id stream)))
      (when *streams* (remhash session *streams*)))))

(defun observe-frame (op)
  "The :FRAME point: one durable fact of a watched session becomes a line on
that session's message, and nothing else is read."
  (when (and *streams* *token* *streaming* (getf op :durable-p))
    (nlk:with-handlers ((error () nil))
      (multiple-value-bind (type payload _metadata turn) (nlk:frame-fact op)
        (declare (ignore _metadata))
        (let ((session (getf op :session-id)))
          (when (and (stringp type) (stringp session)
                     (watched-p session)
                     (not (and *finished* (gethash (turn-key session turn) *finished*))))
            (cond
              ((equal type "turn.tool_call_started") (note-tool session turn payload))
              ((equal type "turn.usage") (note-usage session turn payload))
              ((member type '("turn.completed" "turn.failed" "turn.cancelled")
                       :test #'equal)
               (finish-turn session turn type payload)))))))
    nil))

;;; --- /kanban-stream ------------------------------------------------------------

(defun where-to-text ()
  "Which chat a stream goes to, and from where its token came."
  (format nil "~@[~a~]chat ~a; token ~a"
          (if *streaming* "" "muted — ")
          (or *chat* "unset")
          (cond ((plusp (length (or (setting :bot-token) ""))) "in the section")
                (t (format nil "from $~a" (or (setting :bot-token-env) "?"))))))

(defun open-streams-text ()
  "One line per message this cell has open right now."
  (let ((rows '()))
    (when *streams*
      (maphash (lambda (session stream)
                 (push (format nil "  ~a — ~a, ~d edit~:p, message ~a"
                               (progress-label stream) session (progress-edits stream)
                               (or (progress-message-id stream) "not sent yet"))
                       rows))
               *streams*))
    (if rows
        (format nil "~{~a~^~%~}" (sort rows #'string<))
        "  nothing streaming right now")))

(defun status-text ()
  "The state this cell is in: where it posts, whether it is on, and what is
streaming."
  (format nil "kanban-stream: ~a~%~a~%last error: ~a"
          (where-to-text)
          (open-streams-text)
          (or *last-error* "none")))

(defun test-text ()
  "Post one test line to the chat, so a person learns now whether the token
and the chat work."
  (let ((id (send-text "📡 kanban-stream: the chat and the token work.")))
    (if id
        (format nil "Sent message ~a to chat ~a." id *chat*)
        (format nil "Telegram refused it: ~a" (or *last-error* "no answer")))))

(defun run-command (args session-id)
  "One /kanban-stream invocation: status, on, off, or test."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space)))
             (verb (string-downcase (or (first words) "status"))))
        (cond
          ((member verb '("status" "") :test #'string=) (values (status-text) nil))
          ((string= verb "on")
           (setf *streaming* t)
           (values "kanban-stream: streaming again." nil))
          ((string= verb "off")
           (setf *streaming* nil)
           (values "kanban-stream: muted — the message stays as it is until /kanban-stream on." nil))
          ((string= verb "test") (values (test-text) nil))
          (t (values (format nil "Unknown ~a. /kanban-stream [status|on|off|test]~%~a"
                             verb (status-text))
                     nil))))
    (error (condition)
      (values (format nil "ERROR: kanban-stream: ~a" condition) nil))))

;;; --- the tool ---------------------------------------------------------------

(nlk:define-contract kanban-progress-note
  (:doc "Read or write this machine's Telegram progress stream: post one line to the ops chat a running session streams to, or answer what the stream is doing. Call it when a person asks to push a note to their chat, or asks whether the stream is on.")
  ("text" :string :default ""
         :doc "the line to post; empty answers the stream's state instead"))

(defun tool-note (note)
  "The kanban_stream tool: send one line, or answer the state."
  (handler-case
      (let ((text (nlk:trimmed (or (kanban-progress-note-text note) ""))))
        (if (plusp (length text))
            (let ((id (send-text text)))
              (if id
                  (format nil "Sent to chat ~a as message ~a." *chat* id)
                  (format nil "Telegram refused it: ~a" (or *last-error* "no answer"))))
            (status-text)))
    (error (condition) (fail "~a" condition))))

;;; --- the cell -----------------------------------------------------------------

(defun resolve-token ()
  "The bot token: the section's own, else the value of the environment
variable its env field names. No secret is ever read from a file."
  (let ((given (nlk:trimmed (or (setting :bot-token) ""))))
    (if (plusp (length given))
        given
        (nlk:trimmed (or (uiop:getenv (or (setting :bot-token-env) "")) "")))))

(defun install ()
  "Resolve the chat and the token, refuse without them, and take the two
registrations back on stop."
  (let ((token (resolve-token))
        (chat (nlk:trimmed (or (setting :chat-id) ""))))
    (unless (plusp (length chat))
      (error 'nlk:config-refusal
             :detail "kanban-stream: chat_id is unset — the Telegram chat id a stream is posted to"))
    (unless (plusp (length token))
      (error 'nlk:config-refusal
             :detail (format nil "kanban-stream: no bot token — write bot_token in the section, or set ~a"
                             (or (setting :bot-token-env) "the variable bot_token_env names"))))
    (setf *token* token
          *chat* chat
          *streams* (make-hash-table :test #'equal)
          *finished* (make-hash-table :test #'equal)
          *streaming* t
          *last-error* nil)
    (nle:on-stop (lambda ()
                   (setf *token* nil *chat* nil *streams* nil *finished* nil
                         *streaming* nil)))))

(defparameter +primer+
  "Kanban-stream puts a running turn in a Telegram chat as one message: the tool calls
as they land, the elapsed time, the tokens a second, and at the end one rewrite with
the outcome, pinned. Nothing is posted for a turn that only talks, and nothing leaves
the machine but what that chat is sent. A session's folder names it in the head.
  /kanban-stream              where it posts, and what is streaming now
  /kanban-stream test         post one test line, to prove the token and the chat
  /kanban-stream off | on     mute it, or stream again
The kanban_stream tool posts one line, or reads the same state, from eval.
The section holds bot_token (or bot_token_env, naming the variable) and chat_id."
  "What (help :kanban-stream) answers while the cell runs.")

(nle:define-cell kanban-stream
  (:section ("kanban-stream")
    (:guide "chat_id is the Telegram chat a stream is posted to; bot_token is the bot's own, or bot_token_env names the variable that holds it; next step: /kanban-stream test posts one line")
    ("chat_id" :string :required t
     :doc "the Telegram chat id streams are posted to, as the Bot API spells it")
    ("bot_token" :secret
     :doc "the bot token, written here; empty reads bot_token_env instead")
    ("bot_token_env" :env :default "KANBAN_STREAM_BOT_TOKEN"
     :doc "the environment variable holding the bot token, when it is not written above")
    ("sessions" :list
     :doc "the session ids to stream; empty streams every session")
    ("history" :integer :default 5 :min 1
     :doc "tool lines a message repeats above the one now running")
    ("throttle_seconds" :integer :default 5 :min 1
     :doc "seconds between one message's rewrites")
    ("timeout_seconds" :integer :default 5 :min 1
     :doc "one Bot API call's ceiling")
    ("pin_final" :boolean :default t
     :doc "pin a turn's finished message"))
  (:help :kanban-stream
   "kanban-stream: a running turn in one Telegram message - /kanban-stream"
   +primer+)
  (:command "kanban-stream" #'run-command
            :description "A running turn streamed to Telegram; /kanban-stream test proves the chat"
            :argument-hint "[status|test|on|off]")
  (:hook :frame (nlk:observer #'observe-frame "nodecode-kanban-stream"))
  (:tool "kanban_stream" (nlk:contract 'kanban-progress-note) 'tool-note)
  (:start #'install))
