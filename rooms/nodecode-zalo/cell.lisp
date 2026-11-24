;;;; cell.lisp --- Zalo: a bot on the Zalo Bot Platform, and the agent behind it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of moken627-hub/hermes-plugin-zalo (see NOTICE). The wire, the
;;;; admission rule, the local slash commands and the plain-text shaping are
;;;; the original's; the harness seams are Nodecode's: the section is the
;;;; config, a turn is NLE:SUBMIT, and an answer is the session's own.
;;;;
;;;; One long-poll lap (getUpdates) takes the Bot API's updates, answers the
;;;; commands it can answer without a turn (/menu, /rules, /huong-dan, /kick,
;;;; /pin, /poll), and submits the rest into a session of the chat's own; a
;;;; second lap posts each finished answer back with sendMessage, chunked at
;;;; the platform's 2000 characters and stripped of markdown (Zalo renders
;;;; none of it).
(defpackage #:nodecode-zalo
  (:use #:cl)
  (:export #:zalo-error #:zalo-error-detail #:start-cell
           #:strip-markdown #:chunks #:command-of #:session-of))
(in-package #:nodecode-zalo)
(define-condition zalo-error (error)
  ((detail :initarg :detail :reader zalo-error-detail))
  (:report (lambda (condition stream) (write-string (zalo-error-detail condition) stream)))
  (:documentation "The Bot API refused, or could not be reached."))
(nlk:define-peripheral zalo :not-running t :lock "zalo")
(defparameter +default-api-base+ "https://bot-api.zaloplatforms.com"
  "The Bot API's root; the token is in the path of every call.")
(defparameter +menu+
  (list (cons "/menu" "Hiển thị menu lệnh")
        (cons "/rules" "Quy tắc nhóm")
        (cons "/huong-dan" "Hướng dẫn sử dụng bot")
        (cons "/kick @user" "Kick thành viên (admin)")
        (cons "/pin <message_id>" "Ghim tin nhắn (admin)")
        (cons "/poll <câu hỏi> | lựa chọn 1, lựa chọn 2" "Tạo poll (admin)"))
  "The commands the bot answers itself, without spending a turn: the
original's SLASH_COMMANDS, reduced to the six this cell carries.")
(defparameter +rules-text+
  (format nil "📜 Quy tắc nhóm:~2%1. Lịch sự, không spam hay xúc phạm~%~
2. Không chia sẻ link đáng ngờ~%3. Tuân thủ nội quy đã được thiết lập~%~
4. Gửi /menu để xem danh sách lệnh~%5. Vi phạm sẽ bị cảnh báo, 3 lần thì bị xử lý")
  "What /rules answers, as the original's format_rules has it.")
(defparameter +help-text+
  (format nil "🤖 Hướng dẫn sử dụng Zalo Bot:~2%Bot trả lời tin nhắn như một trợ lý.~
Các lệnh: /menu, /rules, /huong-dan.~%Admin: /kick @user, /pin <message_id>, ~
/poll <câu hỏi> | lựa chọn 1, lựa chọn 2.~%Nhắn trực tiếp cho bot để bắt đầu trò chuyện.")
  "What /huong-dan answers.")
(defvar *poll-worker* nil
  "The long-poll worker (NLK:WORKER-START) while the cell runs, or NIL.")
(defvar *asking* '()
  "Chats with a turn still owed an answer: (CHAT-ID . SESSION) entries,
newest first. Guarded by the cell's own lock.")
(defvar *sequence* 0
  "A counter for the command ids this cell mints, so two asks in one chat are
two admissions.")
;;; --- the section, read back --------------------------------------------------
(defun token-from-file (path)
  (when (and path (plusp (length path)) (probe-file path))
    (nlk:trimmed (uiop:read-file-string path))))
(defun bot-token ()
  "The bot token the section names: by environment variable, file, or inline."
  (or (let ((variable (setting :bot-token-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (token-from-file (setting :bot-token-file))
      (let ((inline (setting :bot-token))) (and inline (plusp (length inline)) inline))))
(defun api-base ()
  (let ((given (string-right-trim "/" (or (setting :api-base) ""))))
    (if (plusp (length given)) given +default-api-base+)))
(defun api-url (endpoint)
  (format nil "~a/bot~a/~a" (api-base) (or (bot-token) "") endpoint))
(defun allowed-p (user-id)
  "Whether USER-ID may talk to the bot: everyone with allow_all, else the list."
  (or (setting :allow-all)
      (member user-id (setting :allowed-users) :test #'equal)))
(defun check-config ()
  "Refuse a section the bot could never run on, naming the member."
  (unless (bot-token)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "zalo: no bot token: set \"bot_token\" in the channels.zalo section, "
                                 "\"bot_token_file\" naming a file that holds it, or \"bot_token_env\" "
                                 "naming the variable")))
  (unless (or (setting :allow-all) (setting :allowed-users))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "zalo: name who may talk to the bot: \"allowed_users\" with their Zalo "
                                 "user ids, or \"allow_all\": true to let anyone through"))))
;;; --- the wire ----------------------------------------------------------------
(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub this name."
  (apply #'nlk:http method url arguments))
(defun scrub (text)
  "TEXT with the bot token taken out: every URL the API quotes carries it."
  (let ((token (bot-token)))
    (if (and token (plusp (length token)) (stringp text))
        (ppcre:regex-replace-all (ppcre:quote-meta-chars token) text "***")
        (or text ""))))
(defun decode-body (answer)
  (cond ((stringp answer)
         (if (plusp (length (nlk:trimmed answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        (t (nlk:json-object))))
(defun body-says (body)
  "What a failed answer says, for the one line a caller reads."
  (or (nlk:json-value body :string "message")
      (nlk:json-value body :string "error")
      "no answer"))
(defun request (endpoint &optional body)
  "POST BODY to ENDPOINT on the Bot API => the decoded answer. A dial that
fails or a status outside 2xx signals ZALO-ERROR, the token scrubbed."
  (let ((answer nil) (status nil) (limit (setting :timeout-seconds)))
    (handler-case
        (multiple-value-setq (answer status)
          (http :post (api-url endpoint)
                :headers '(("content-type" . "application/json"))
                :content (nlk:encode-json-object (or body (nlk:json-object)))
                :timeout limit
                :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "zalo: ~a failed: ~a" endpoint (scrub (princ-to-string condition)))))
    (if (< status 300)
        (decode-body answer)
        (fail "zalo: ~a answered HTTP ~d: ~a" endpoint status (scrub (body-says (decode-body answer)))))))
(defun get-me ()
  "The bot's own identity, or NIL when the API did not answer one."
  (nlk:json-value (request "getMe") :object "result"))
(defun send-text (chat-id text)
  "Send TEXT to CHAT-ID, stripped of markdown and chunked at the platform's
length: => the number of messages it took."
  (let* ((plain (strip-markdown text))
         (parts (chunks plain (setting :max-length))))
    (dolist (part parts)
      (let ((answer (request "sendMessage" (nlk:json-object "chat_id" chat-id "text" part))))
        (unless (nlk:json-value answer :boolean "ok")
          (fail "zalo: sendMessage to ~a was refused: ~a" chat-id (scrub (body-says answer))))))
    (length parts)))
;;; --- plain text for a platform that renders none ------------------------------
(defun table-row (row)
  "ROW -- a markdown table line -- as its cells joined by an em dash."
  (let ((inner (string-trim " " (string-trim "|" row))))
    (format nil "~{~a~^ — ~}"
            (loop for cell in (ppcre:split "\\|" inner)
                  for cell-text = (nlk:trimmed cell)
                  when (plusp (length cell-text)) collect cell-text))))
(defun strip-markdown (text)
  "TEXT with the markdown Zalo does not render taken out, as the original's
_strip_markdown does: an image keeps its url, a link its words and url, code
and emphasis their words; headings, quotes and list markers go, and a table
row becomes its cells joined by an em dash."
  (let ((text (or text "")))
    (setf text (ppcre:regex-replace-all "!\\[([^\\]]*)\\]\\(([^)]+)\\)" text "\\2"))
    (setf text (ppcre:regex-replace-all "\\[([^\\]]+)\\]\\(([^)]+)\\)" text "\\1 (\\2)"))
    (setf text (ppcre:regex-replace-all "`([^`]*)`" text "\\1"))
    (setf text (ppcre:regex-replace-all "\\*\\*(.+?)\\*\\*" text "\\1"))
    (setf text (ppcre:regex-replace-all "__(.+?)__" text "\\1"))
    (setf text (ppcre:regex-replace-all "(?<![\\w*])\\*([^*\\n]+?)\\*(?![\\w*])" text "\\1"))
    (setf text (ppcre:regex-replace-all "~~(.+?)~~" text "\\1"))
    (setf text (ppcre:regex-replace-all "(?m)^#{1,6}[ \\t]*" text ""))
    (setf text (ppcre:regex-replace-all "(?m)^[ \\t]*>[ \\t]?" text ""))
    (setf text (ppcre:regex-replace-all "(?m)^[ \\t]*(?:[-*+][ \\t]+|[0-9]+[.)][ \\t]+)" text ""))
    (setf text (ppcre:regex-replace-all "(?m)^[ \\t]*\\|[\\s:|-]+\\|[ \\t]*$" text ""))
    (setf text (ppcre:regex-replace-all "(?m)^[ \\t]*\\|.*\\|[ \\t]*$" text #'table-row))
    (setf text (ppcre:regex-replace-all "[*~]" text ""))
    (setf text (ppcre:regex-replace-all "\\n{3,}" text (coerce (list #\Newline #\Newline) 'string)))
    (nlk:trimmed text)))
(defun chunks (text max)
  "TEXT cut into pieces of at most MAX characters, in order."
  (let ((text (or text "")) (max (max 1 (or max 2000))))
    (if (zerop (length text))
        (list "")
        (loop for start from 0 below (length text) by max
              collect (subseq text start (min (length text) (+ start max)))))))
;;; --- the commands the bot answers itself --------------------------------------
(defun menu-text ()
  (with-output-to-string (out)
    (format out "📋 Menu lệnh Zalo Bot:~%")
    (dolist (entry +menu+)
      (format out "  ~a — ~a~%" (car entry) (cdr entry)))))
(defun command-of (text)
  "The command word TEXT opens with, lowercased, or NIL when it opens with none."
  (let* ((text (nlk:trimmed (or text "")))
         (space (position #\Space text)))
    (when (and (plusp (length text)) (char= #\/ (char text 0)))
      (string-downcase (if space (subseq text 0 space) text)))))
(defun command-args (text)
  (let* ((text (nlk:trimmed (or text "")))
         (space (position #\Space text)))
    (nlk:trimmed (if space (subseq text (1+ space)) ""))))
(defun first-target (args)
  "The id ARGS opens with, a leading @ taken off."
  (let* ((args (nlk:trimmed (or args "")))
         (space (position #\Space args)))
    (string-left-trim "@" (if space (subseq args 0 space) args))))
(defun kick (chat-id args)
  "Remove the member ARGS names: the original's /kick."
  (let ((target (first-target args)))
    (if (zerop (length target))
        "Cú pháp: /kick @user"
        (let ((answer (request "kickGroupMember"
                                (nlk:json-object "group_id" chat-id "user_id" target))))
          (if (nlk:json-value answer :boolean "ok")
              (format nil "✅ Đã kick ~a" target)
              (format nil "⚠️ Không kick được ~a: ~a" target (body-says answer)))))))
(defun pin (chat-id args)
  "Pin one message in CHAT-ID: the original's /pin."
  (let ((message-id (first-target args)))
    (if (zerop (length message-id))
        "Cú pháp: /pin <message_id>"
        (let ((answer (request "pinGroupMessage"
                                (nlk:json-object "group_id" chat-id "message_id" message-id))))
          (if (nlk:json-value answer :boolean "ok")
              (format nil "📌 Đã ghim tin nhắn ~a" message-id)
              (format nil "⚠️ Không ghim được: ~a" (body-says answer)))))))
(defun poll-options (args)
  "The options a /poll line names after its bar, trimmed and in order."
  (let ((bar (position #\| (or args ""))))
    (when bar
      (loop for option in (ppcre:split "," (subseq args (1+ bar)))
            for option-text = (nlk:trimmed option)
            when (plusp (length option-text)) collect option-text))))
(defun poll (chat-id args)
  "Create a poll from \"question | option, option\": the original's /poll."
  (let* ((bar (position #\| (or args "")))
         (question (nlk:trimmed (if bar (subseq args 0 bar) (or args ""))))
         (options (poll-options args)))
    (cond ((or (null options) (< (length options) 2) (zerop (length question)))
           "Cú pháp: /poll <câu hỏi> | lựa chọn 1, lựa chọn 2")
          (t (let ((answer (request "createPoll"
                                     (nlk:json-object "group_id" chat-id "question" question
                                                      "options" (coerce options 'vector)
                                                      "expiry" 86400))))
               (if (nlk:json-value answer :boolean "ok")
                   (format nil "📊 Đã tạo poll: ~a" question)
                   (format nil "⚠️ Không tạo được poll: ~a" (body-says answer))))))))
(defun answer-locally (chat-id text)
  "Answer TEXT in CHAT-ID without a turn when it is one of the bot's own
commands => true; NIL when the message is for the agent."
  (let ((command (command-of text)) (args (command-args text)))
    (when command
      (cond ((member command '("/menu" "/commands") :test #'equal)
             (send-text chat-id (menu-text)))
            ((equal command "/rules") (send-text chat-id +rules-text+))
            ((member command '("/huong-dan" "/help") :test #'equal)
             (send-text chat-id +help-text+))
            ((equal command "/kick") (send-text chat-id (kick chat-id args)))
            ((equal command "/pin") (send-text chat-id (pin chat-id args)))
            ((equal command "/poll") (send-text chat-id (poll chat-id args)))
            (t (send-text chat-id (format nil "Không biết lệnh ~a. Gửi /menu để xem danh sách." command))))
      t)))
;;; --- the turn: one session per chat ------------------------------------------
(defun session-of (chat-id)
  "The session a chat's asks run in: one per chat, so a conversation there is
a conversation in the agent's history."
  (format nil "zalo-~a" (ppcre:regex-replace-all "[^A-Za-z0-9]+" (or chat-id "unknown") "-")))
(defun submit-prompt (session prompt command-id source-id)
  "PROMPT into SESSION through the one in-process ingress; tests stub this."
  (nle:submit session prompt :command-id command-id :source "zalo" :source-id source-id))
(defun ensure-session (session)
  (when (nlk:store-open-p)
    (unless (nlk:session-exists-p session)
      (nlk:standby-session :id session :cwd (uiop:getcwd)))))
(defun ask-agent (chat-id user-id name text)
  "Hand TEXT to the agent in the chat's session, and remember to answer CHAT-ID."
  (let ((session (session-of chat-id)))
    (handler-case
        (progn
          (ensure-session session)
          (submit-prompt session
                         (format nil "[zalo ~a, chat ~a, user ~a]~%~a" name chat-id user-id text)
                         (format nil "zalo:~a:~a" chat-id (incf *sequence*))
                         chat-id)
          (with-zalo-lock
            (unless (assoc chat-id *asking* :test #'equal)
              (push (cons chat-id session) *asking*))))
      (error (condition)
        (send-text chat-id (format nil "⚠️ Không gửi được tin nhắn: ~a" condition))))))
(defun deliver-pending ()
  "Post each finished answer back to the chat that asked for it."
  (with-zalo-lock
    (dolist (entry (copy-list *asking*))
      (let ((chat-id (car entry)) (session (cdr entry)))
        (unless (nlk:active-turn-p session)
          (setf *asking* (remove entry *asking* :test #'eq))
          (let ((answer (nlk:turn-answer session)))
            (when (and answer (plusp (length (nlk:trimmed answer))))
              (handler-case (send-text chat-id answer)
                (error (condition)
                  (warn "zalo: an answer did not reach ~a: ~a" chat-id condition))))))))))
;;; --- the lap -----------------------------------------------------------------
(defun updates-of (answer)
  "ANSWER's result as a list of update objects, whether the API wrapped one
or many, as the original's get_updates handles both."
  (let ((result (nlk:json-value answer :any "result")))
    (cond ((vectorp result) (coerce result 'list))
          ((hash-table-p result) (list result))
          (t '()))))
(defun handle-update (update)
  "One update: admit it, answer a local command, or submit it to the agent."
  (let* ((envelope (or (nlk:json-value update :object "message")
                        (nlk:json-value (nlk:json-value update :object "result") :object "message")))
         (from (nlk:json-value envelope :object "from"))
         (chat (nlk:json-value envelope :object "chat"))
         (text (nlk:json-value envelope :string "text"))
         (user-id (nlk:json-value from :string "id"))
         (chat-id (nlk:json-value chat :string "id"))
         (name (or (nlk:json-value from :string "display_name") user-id "")))
    (when (and text chat-id (plusp (length text)) (plusp (length (or user-id "")))
               (allowed-p user-id))
      (unless (answer-locally chat-id text)
        (ask-agent chat-id user-id name text)))))
(defun poll-once ()
  "One long poll of the Bot API, then each update it brought."
  (let ((answer (request "getUpdates" (nlk:json-object "timeout" (setting :poll-timeout)))))
    (when (nlk:json-value answer :boolean "ok")
      (dolist (update (updates-of answer)) (handle-update update)))))
(defun lap ()
  "The worker's step: deliver what is due, then take what came in. An error
is a warn, not a stop - the next lap tries again."
  (handler-case (deliver-pending) (error (condition) (warn "zalo: delivery: ~a" condition)))
  (handler-case (poll-once) (error (condition) (warn "zalo: poll: ~a" condition)))
  nil)
;;; --- the manual and /zalo ------------------------------------------------------
(defparameter +primer+
  "Zalo is the Vietnamese messaging platform; this cell puts the agent in it as a bot,
through the official Zalo Bot Platform (bot.zaloplatforms.com).
People the section allows message the bot in a private chat or a group; each chat is a session of
its own (zalo-<chat-id>), so a conversation there is one in the agent's history, and the answer
comes back as one or more plain-text messages.
The bot answers six commands itself, without spending a turn: /menu, /rules, /huong-dan, and, for a
group's admins, /kick @user, /pin <message_id> and /poll <question> | option, option.
Zalo renders no markdown: answers are stripped to plain text, and anything past the platform's limit
is split. The channels.zalo section names the bot token (bot_token, bot_token_file or
bot_token_env), who may talk (allowed_users, or allow_all), and the polling. /zalo shows what the
bot is; /zalo CHAT-ID TEXT sends a message."
  "What (help :zalo) answers while the cell runs.")
(defun status-line ()
  (let ((me (handler-case (get-me) (error () nil))))
    (format nil "Zalo bot~@[: ~a~] at ~a. Allowed: ~a. ~d ask~:p owed an answer.~
~%The token is bot_token, bot_token_file or bot_token_env in channels.zalo; allowed_users names ~
who may talk, or allow_all lets anyone through."
            (and me (nlk:json-value me :string "display_name"))
            (api-base)
            (if (setting :allow-all) "anyone"
                (format nil "~{~a~^, ~}" (setting :allowed-users)))
            (length *asking*))))
(defun run-command (args session-id)
  "One /zalo invocation: no argument answers the bot's state; \"CHAT-ID TEXT\"
sends TEXT to that chat."
  (declare (ignore session-id))
  (unless *poll-worker*
    (fail "zalo: the cell is not running: turn it on in /cells, or check its channels.zalo section"))
  (let* ((args (nlk:trimmed (or args "")))
         (space (position #\Space args)))
    (if (or (null space) (zerop (length args)))
        (status-line)
        (let ((chat-id (subseq args 0 space))
              (text (nlk:trimmed (subseq args (1+ space)))))
          (send-text chat-id text)
          (format nil "Sent to ~a." chat-id)))))
(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))
(defparameter *send-schema*
  (tool-schema
   "Send a plain-text message to a Zalo chat through the bot, without waiting for a turn's answer. Use it to report back to a chat the operator named; text past the platform's limit is split, and markdown is stripped."
   '(("chat_id" "string" "The Zalo chat id to post in." t)
     ("text" "string" "What to send." t)))
  "The zalo_send tool: the original's standalone sender, as a verb.")
(defun tool-send (arguments)
  "zalo_send: one message to one chat."
  (let ((chat-id (nlk:json-value arguments :string "chat_id"))
        (text (nlk:json-value arguments :string "text")))
    (unless (and chat-id (plusp (length chat-id)))
      (fail "zalo_send needs chat_id"))
    (unless (and text (plusp (length text)))
      (fail "zalo_send needs text"))
    (handler-case (format nil "Sent ~d message~:p to ~a." (send-text chat-id text) chat-id)
      (zalo-error (condition) (zalo-error-detail condition)))))
;;; --- the cell -------------------------------------------------------------------
(nle:define-cell zalo
  (:section ("channels" "zalo")
    (:guide "create a bot at bot.zaloplatforms.com and keep its token in a file or a variable; name who may talk, or set allow_all; next step: /zalo shows the bot")
    ("bot_token" :secret
     :doc "the bot token the Zalo Bot Platform issued")
    ("bot_token_file" :path
     :doc "a file holding the bot token, one line")
    ("bot_token_env" :env
     :doc "the environment variable holding the bot token, when it is not written here")
    (:one-of "bot_token" "bot_token_file" "bot_token_env")
    ("allowed_users" :list
     :doc "the Zalo user ids who may talk to the bot")
    ("allow_all" :boolean :default nil
     :doc "let every Zalo user talk to the bot (they run with your machine's authority)")
    ("api_base" :string :default "https://bot-api.zaloplatforms.com"
     :doc "the Bot API root")
    ("poll_seconds" :integer :default 2 :min 1
     :doc "how long one lap waits before the next")
    ("poll_timeout" :integer :default 25 :min 1
     :doc "how long the Bot API holds a getUpdates call open")
    ("max_length" :integer :default 2000 :min 100
     :doc "the platform's per-message character limit")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one request's ceiling"))
  (:help :zalo "zalo: the agent in a Zalo bot - /zalo shows the bot, /zalo CHAT-ID TEXT sends to a chat" +primer+)
  (:command "zalo" 'run-command
            :description "Show the Zalo bot, or send a message to a chat"
            :argument-hint "CHAT-ID TEXT")
  (:tool "zalo_send" *send-schema* 'tool-send)
  (:start (lambda ()
            (check-config)
            (setf *asking* '() *sequence* 0)
            (setf *poll-worker*
                  (nlk:worker-start "zalo-poll" #'lap :wake (lambda () (setting :poll-seconds))))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *poll-worker*)
                           (setf *poll-worker* nil *asking* '()))))))
