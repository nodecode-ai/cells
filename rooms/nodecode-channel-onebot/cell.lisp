;;;; cell.lisp --- OneBot 11: the segments, the poll, the platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; QQ has no bot API of its own: a self-hosted bridge speaks OneBot 11 at
;;;; it -- NapCat, Lagrange, LLOneBot, go-cqhttp -- and exposes the bridge's
;;;; HTTP API at one base URL, one access token in an Authorization header.
;;;; This lane talks to that URL and nothing else: (get_group_msg_history)
;;;; for a group, (get_friend_msg_history) for a person, (send_group_msg) and
;;;; (send_private_msg) to answer, (delete_msg) to take a card back, and
;;;; (get_login_info) to learn who the bot is.
;;;;
;;;; A message is a list of segments -- text, at, image, reply -- and on the
;;;; wire it may also be one string of CQ codes; both are read here. An
;;;; answer goes out with auto_escape, so a model's own "[CQ:at,qq=all]"
;;;; stays words and never pings a group.
;;;;
;;;; A rewrite of mario841859784's Hermes Agent plugin; see NOTICE.

(in-package #:nodecode-channel-onebot)

;;; The section is declared at the end of this file, after PROBE-CHANNEL: its
;;; :check names that function, which the declaration finds at load time.

;;; --- constants ------------------------------------------------------------------

(defparameter +token-page+
  "https://napneko.github.io/config/basic#token"
  "Where an operator sets the bridge's access token.")

(defparameter +history-count+ 20
  "How many messages one history read asks for: enough for a burst, small
enough that a five-second lap stays cheap.")

(defparameter +full-refresh-every+ 0
  "OneBot pushes nothing to an HTTP client, so every lap reads every chat.")

(defparameter +max-catchup-seconds+ (* 24 3600)
  "Asks older than this were said while the bot was away and are skipped, so
nobody gets an answer to last week's question.")

(defparameter +sent-keep+ 1000
  "How many of the bot's own postings are remembered.")

(defparameter +typing-refresh-ms+ 86400000
  "QQ has no typing indicator an API may raise; the lane says so by asking
for the bridge's status once a day.")

;;; --- what a message says ---------------------------------------------------------

(defun onebot-entity-decode (text)
  "TEXT with OneBot's CQ escapes read back: &amp; &#91; &#93; and &#44;."
  (let ((out (make-string-output-stream)))
    (loop with i = 0 with n = (length text)
          while (< i n)
          for ch = (char text i)
          do (cond
               ((and (char= ch #\&) (uiop:string-prefix-p "&amp;" (subseq text i)))
                (write-char #\& out) (incf i 5))
               ((and (char= ch #\&) (uiop:string-prefix-p "&#91;" (subseq text i)))
                (write-char #\[ out) (incf i 5))
               ((and (char= ch #\&) (uiop:string-prefix-p "&#93;" (subseq text i)))
                (write-char #\] out) (incf i 5))
               ((and (char= ch #\&) (uiop:string-prefix-p "&#44;" (subseq text i)))
                (write-char #\, out) (incf i 5))
               (t (write-char ch out) (incf i))))
    (get-output-stream-string out)))

(defun onebot-cq-params (body)
  "BODY, the inside of a CQ code, as an alist of (KEY . VALUE)."
  (let ((parts (uiop:split-string body :separator ","))
        (params '()))
    (dolist (part parts (nreverse params))
      (let ((eq (position #\= part)))
        (when eq
          (push (cons (subseq part 0 eq)
                      (onebot-entity-decode (subseq part (1+ eq))))
                params))))))

(defun onebot-parse-cq (text)
  "TEXT, a CQ code string, as the segments a message array carries:
(:text \"...\"), (:at \"qq\" \"name\"), (:image \"file\"), (:reply \"id\"),
(:other \"type\")."
  (let ((segments '()) (pos 0) (n (length text)))
    (loop
      (let ((open (search "[CQ:" text :start2 pos)))
        (when (null open)
          (when (< pos n)
            (push (list :text (onebot-entity-decode (subseq text pos))) segments))
          (return (nreverse segments)))
        (when (< pos open)
          (push (list :text (onebot-entity-decode (subseq text pos open))) segments))
        (let ((close (search "]" text :start2 open)))
          (if (null close)
              (progn
                (push (list :text (subseq text open)) segments)
                (return (nreverse segments)))
              (let* ((body (subseq text (+ open 4) close))
                     (eq (position #\, body))
                     (kind (if eq (subseq body 0 eq) body))
                     (params (if eq (onebot-cq-params (subseq body (1+ eq))) '())))
                (push (cond ((equal kind "at")
                             (list :at (or (cdr (assoc "qq" params :test #'equal)) "")
                                   (or (cdr (assoc "name" params :test #'equal)) "")))
                            ((equal kind "image")
                             (list :image (or (cdr (assoc "file" params :test #'equal)) "")))
                            ((equal kind "reply")
                             (list :reply (or (cdr (assoc "id" params :test #'equal)) "")))
                            (t (list :other kind)))
                      segments)
                (setf pos (1+ close)))))))))

(defun onebot-segment (entry)
  "One message-array ENTRY as the segment list ONEbot-PARSE-CQ makes of a
string, or NIL when it is not one."
  (when (hash-table-p entry)
    (let ((type (nlk:json-value entry :string "type"))
          (data (nlk:json-value entry :object "data")))
      (cond
        ((equal type "text") (list :text (or (nlk:json-value data :string "text") "")))
        ((equal type "at") (list :at (or (nlk:json-value data :string "qq")
                                          (nlk:json-value data :any "qq") "")
                                  (or (nlk:json-value data :string "name") "")))
        ((equal type "image") (list :image (or (nlk:json-value data :string "file") "")))
        ((equal type "reply") (list :reply (or (nlk:json-value data :string "id")
                                                (nlk:json-value data :any "id") "")))
        (t (list :other (or type "segment")))))))

(defun onebot-segments (message)
  "MESSAGE -- a segment array or a CQ string -- as segments."
  (cond
    ((stringp message) (onebot-parse-cq (string-trim '(#\Space #\Tab #\Newline #\Return) message)))
    ((vectorp message)
     (loop for entry across message
           for segment = (onebot-segment entry)
           when segment collect segment))
    ((hash-table-p message) (or (onebot-segment message) '()))
    (t '())))

(defun onebot-id-text (value)
  "VALUE -- an id a bridge spells as a JSON number or a string -- as text,
or NIL."
  (typecase value
    (integer (format nil "~d" value))
    (string (if (plusp (length value)) value nil))
    (t nil)))

(defun onebot-id-number (value)
  "VALUE as the number an id compares by; 0 when it is nothing."
  (let ((text (onebot-id-text value)))
    (or (and text (parse-integer text :junk-allowed t)) 0)))

(defun onebot-parse-message (message &key self-id)
  "MESSAGE as the plist a lane reads: :text (the words, mentions of other
people kept as @name), :to-bot (whether an at names SELF-ID), :reply (the id
of the message it answers), :images (how many pictures came with it).
A reply segment says which message was answered; whether that message was
the bot's own is the adapter's book, not the segment's."
  (let ((words '()) (to-bot nil) (reply nil) (images 0))
    (dolist (segment (onebot-segments message))
      (ecase (first segment)
        (:text (push (second segment) words))
        (:at
         (if (and self-id (equal self-id (second segment)))
             (setf to-bot t)
             (push (format nil "@~a" (if (plusp (length (third segment)))
                                         (third segment) (second segment)))
                   words)))
        (:image (incf images) (push "(image)" words))
        (:reply (unless reply (setf reply (second segment))))
        (:other (push (format nil "(~a)" (second segment)) words))))
    (list :text (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (format nil "~{~a~}" (nreverse words)))
          :to-bot to-bot
          :reply reply
          :images images)))

;;; --- what an answer is written as --------------------------------------------------

(defun onebot-defuse (text)
  "TEXT with a CQ code the model wrote made inert by full-width brackets, so
an answer is words and never a ping (the send says auto_escape as well)."
  (let ((out (make-string-output-stream)))
    (loop with i = 0 with n = (length (or text ""))
          while (< i n)
          for ch = (char text i)
          do (cond
               ((and (char= ch #\[) (uiop:string-prefix-p "[CQ:" (subseq text i (min n (+ i 4)))))
                (write-char #\［ out) (incf i))
               ((char= ch #\]) (write-char #\］ out) (incf i))
               (t (write-char ch out) (incf i))))
    (get-output-stream-string out)))

(defun onebot-render-outbound (markdown)
  "MARKDOWN as the words of a QQ message: the markdown QQ will not render is
left as it is, and only a CQ code is defused."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or markdown ""))))
    (when (zerop (length text)) (setf text "..."))
    (onebot-defuse text)))

;;; --- the read position ------------------------------------------------------------

(defstruct (onebot-state (:copier nil) (:constructor make-onebot-state))
  "Where this lane has read to, and what it has posted of its own."
  (self-id "" :type string)
  ;; chat key ("g123" / "p456") -> the newest message id already handled.
  (cursors (make-hash-table :test #'equal))
  (sent '() :type list)
  (sent-set (make-hash-table :test #'equal)))

(defun onebot-cursor (state chat)
  "The newest message id in CHAT already handled, 0 when none."
  (onebot-id-number (gethash chat (onebot-state-cursors state))))

(defun onebot-has-cursor-p (state chat)
  "Whether this lane has ever read CHAT: without a cursor the chat is only
marked, and nothing said before now is answered."
  (nth-value 1 (gethash chat (onebot-state-cursors state))))

(defun onebot-advance-cursor (state chat message-id)
  "Move CHAT's read position to MESSAGE-ID when it is newer."
  (let ((text (onebot-id-text message-id)))
    (when (and text (> (onebot-id-number text) (onebot-cursor state chat)))
      (setf (gethash chat (onebot-state-cursors state)) text))))

(defun onebot-remember-sent (state message-id)
  "Remember that the bot posted MESSAGE-ID itself."
  (let ((text (onebot-id-text (or message-id ""))))
    (when (and text (not (gethash text (onebot-state-sent-set state))))
      (setf (gethash text (onebot-state-sent-set state)) t)
      (push text (onebot-state-sent state))
      (when (> (length (onebot-state-sent state)) +sent-keep+)
        (setf (onebot-state-sent state)
              (loop for id in (onebot-state-sent state)
                    repeat +sent-keep+
                    collect id))))))

(defun onebot-sent-by-bot-p (state message-id)
  "Whether MESSAGE-ID is one this lane posted."
  (let ((text (onebot-id-text (or message-id ""))))
    (and text (gethash text (onebot-state-sent-set state)) t)))

;;; --- the lane ---------------------------------------------------------------------

(defstruct (onebot-adapter (:copier nil) (:constructor make-onebot-adapter))
  "One running OneBot lane: who the bot is, the chats it serves, and where
it has read to."
  (host nil)
  (token "" :type string)
  (self-id "" :type string)
  (nickname "" :type string)
  ;; group id -> name, from the bridge's own group list
  (groups (make-hash-table :test #'equal))
  (state (make-onebot-state) :type onebot-state)
  (allowed-groups '())
  (allowed-users '())
  (free-groups '())
  (require-mention t)
  (poll-seconds 5 :type integer)
  (catch-up-seconds +max-catchup-seconds+ :type integer))

(defvar *onebot-adapter* nil "The live lane, or NIL when it is stopped.")

(defun onebot-headers (adapter)
  "The header a bridge with an access token expects, or NIL."
  (let ((token (if (onebot-adapter-p adapter)
                      (onebot-adapter-token adapter)
                      (or adapter ""))))
    (if (and (stringp token) (plusp (length token)))
        (list (cons "authorization" (format nil "Bearer ~a" token)))
        '())))

(defun onebot-chat-key (kind id)
  "The key one chat is remembered under: g<group> or p<person>."
  (format nil "~a~a" (if (equal kind "group") "g" "p") id))

(defun onebot-group-id-p (adapter id)
  "Whether ID is a group this lane knows: listed in allowed_groups or named
by the bridge's group list."
  (or (member id (onebot-adapter-allowed-groups adapter) :test #'equal)
      (nth-value 1 (gethash id (onebot-adapter-groups adapter)))))

(defun onebot-learn-groups (adapter body)
  "Remember every group BODY's (get_group_list) answer names."
  (let ((entries (nlk:json-value body :array "data")))
    (if entries
        (loop for entry across entries
              for id = (onebot-id-text (nlk:json-value entry :any "group_id"))
              when id
                do (setf (gethash id (onebot-adapter-groups adapter))
                         (or (nlk:json-value entry :string "group_name") id)))
        nil))
  (hash-table-count (onebot-adapter-groups adapter)))

(defun onebot-watched-chats (adapter)
  "The chats this lane reads, as (:kind :id): every allowed group, and every
allowed person's private chat."
  (append (loop for group in (onebot-adapter-allowed-groups adapter)
                collect (list :kind "group" :id group))
          (loop for user in (onebot-adapter-allowed-users adapter)
                collect (list :kind "private" :id user))))

;;; --- what a message becomes ---------------------------------------------------------

(defun onebot-message-candidate (adapter message chat-kind chat-id
                                 &key (now (get-universal-time)))
  "MESSAGE, one entry of a history answer, as the kit's candidate, or NIL
when this lane answers nothing in it."
  ;; The rules are the original's: never the bot's own post; in a group a
  ;; message that ats the bot or replies to one of its messages, unless the
  ;; group is free-response or mention is off; anything in a private chat;
  ;; nothing older than the catch-up window; nothing blank once the segments
  ;; are read.
  (let* ((state (onebot-adapter-state adapter))
         (sender (onebot-id-text (nlk:json-value message :any "user_id")))
         (sender-name (or (nlk:json-value message :object "sender" "card")
                          (nlk:json-value message :object "sender" "nickname")
                          sender))
         (parsed (onebot-parse-message (nlk:json-value message :any "message")
                                        :self-id (onebot-adapter-self-id adapter)))
         (id (onebot-id-text (nlk:json-value message :any "message_id")))
         (text (getf parsed :text))
         (reply (getf parsed :reply))
         (group-p (equal chat-kind "group"))
         (free-p (member chat-id (onebot-adapter-free-groups adapter) :test #'equal))
         (addressed (and (or (getf parsed :to-bot)
                             (and reply (onebot-sent-by-bot-p state reply)))
                         t))
         (sent (nlk:json-value message :any "time"))
         (unix-now (- now 2208988800))
         (age (- unix-now (or (and (integerp sent) sent) unix-now))))
    (when (and id
               (not (equal sender (onebot-adapter-self-id adapter)))
               (not (onebot-sent-by-bot-p state id)))
      (when (and (plusp (length text))
                 (or (not group-p)
                     addressed
                     free-p
                     (not (onebot-adapter-require-mention adapter)))
                 (or (zerop (onebot-adapter-catch-up-seconds adapter))
                     (<= age (onebot-adapter-catch-up-seconds adapter))))
        (let ((source (nlk:json-object
                       "platform" "onebot"
                       "chat_kind" (if group-p "channel" "direct_message")
                       "channel_id" chat-id
                       "user_id" (or sender "")
                       "user_name" (or sender-name "")
                       "message_id" id
                       "addressed" (and addressed t)
                       "is_bot" nil)))
          (when reply (setf (gethash "reply_to_message_id" source) reply))
          (nlk:json-object "text" text "source" source))))))

;;; --- plans ------------------------------------------------------------------------

(defun onebot-ok-p (result)
  "Whether RESULT is a call the bridge made: a 2xx and retcode 0."
  (and (execution-ok-p result)
       (let ((code (and (hash-table-p (execution-body result))
                        (nlk:json-value (execution-body result) :any "retcode"))))
         (or (null code) (equal 0 code) (equal "0" code)))))

(defun onebot-login-plan (adapter)
  "Who the bridge speaks as: the bot's own QQ number and name."
  (rest-plan "POST" "get_login_info" "OneBot whoami" t 20 nil (onebot-headers adapter)))

(defun onebot-groups-plan (adapter)
  "Every group the bot's account is in, which is also the group names."
  (rest-plan "POST" "get_group_list" "OneBot groups" t 30 nil (onebot-headers adapter)))

(defun onebot-history-plan (adapter kind id)
  "The newest messages of one chat: (get_group_msg_history) for a group,
(get_friend_msg_history) for a person."
  (let ((params (nlk:json-object "count" +history-count+)))
    (setf (gethash (if (equal kind "group") "group_id" "user_id") params) id)
    (rest-plan "POST"
               (if (equal kind "group") "get_group_msg_history" "get_friend_msg_history")
               "OneBot history" t 30 params (onebot-headers adapter))))

(defun onebot-send-plan (adapter target chunk
                         &key reply-to ping timeout-seconds &allow-other-keys)
  "What one answer is: (send_group_msg) or (send_private_msg) of CHUNK, with
auto_escape on so a model's CQ code stays words."
  (declare (ignore reply-to ping))
  (let* ((id (or (getf target :channel-id) ""))
         (group-p (onebot-group-id-p adapter id))
         (params (nlk:json-object "message" (onebot-render-outbound chunk)
                                   "auto_escape" t)))
    (setf (gethash (if group-p "group_id" "user_id") params) id)
    (rest-plan "POST"
               (if group-p "send_group_msg" "send_private_msg")
               "OneBot message" t (or timeout-seconds 30)
               params (onebot-headers adapter))))

(defun onebot-delete-plan (adapter target message-id
                           &key timeout-seconds &allow-other-keys)
  "Take MESSAGE-ID back, the way a card follows its turn."
  (declare (ignore target))
  (rest-plan "POST" "delete_msg" "OneBot delete" nil (or timeout-seconds 30)
             (nlk:json-object "message_id" message-id)
             (onebot-headers adapter)))

(defun onebot-status-plan (adapter &key timeout-seconds &allow-other-keys)
  "QQ has no typing indicator. The lane still has to say something, so it
asks the bridge how it is -- the cheapest call there is, once a day."
  (rest-plan "POST" "get_status" "OneBot typing" nil (or timeout-seconds 10)
             nil (onebot-headers adapter)))

(defun onebot-edit-plan (adapter target message-id text
                         &key timeout-seconds &allow-other-keys)
  "OneBot has no edit: a card's next state is posted under the last, and the
last taken back when the bridge lets a message go."
  (declare (ignore message-id))
  (onebot-send-plan adapter target text :timeout-seconds timeout-seconds))

(defun onebot-message-id-of (body)
  "The id a send answer gives its message."
  (onebot-id-text (nlk:json-value body :any "data" "message_id")))

(defun onebot-noting-executor (adapter inner)
  "INNER, with every message this lane posts remembered as its own."
  (make-plan-executor
   :run (lambda (plan &aux (result (execute-plan inner plan)))
          (when (and (onebot-ok-p result)
                     (equal "POST" (request-plan-method plan))
                     (member (request-plan-path plan) '("send_group_msg" "send_private_msg")
                             :test #'equal))
            (let ((id (onebot-message-id-of (execution-body result))))
              (when id (onebot-remember-sent (onebot-adapter-state adapter) id))))
          result)))

;;; --- the platform -----------------------------------------------------------------

(defun onebot-platform (&key adapter)
  "What OneBot contributes to the kit's host: the plans a lane draws with,
the message ids the bridge gives, and where the chat is."
  (make-platform
   :id "onebot"
   :name "QQ"
   :noun "chat"
   :owner-label "QQ number"
   :session-prefix "onebot"
   :contract-section "onebot-chat"
   :text-limit 4000
   :typing-refresh-ms +typing-refresh-ms+
   :plan-message (lambda (target chunk &rest keys)
                   (apply #'onebot-send-plan adapter target chunk keys))
   :plan-edit (lambda (target message-id text &rest keys)
                (apply #'onebot-edit-plan adapter target message-id text keys))
   :plan-delete (lambda (target message-id &rest keys)
                  (apply #'onebot-delete-plan adapter target message-id keys))
   :plan-typing (lambda (target &rest keys)
                   (declare (ignore target))
                   (apply #'onebot-status-plan adapter keys))
   :message-id-of #'onebot-message-id-of
   :where-text (lambda (candidate)
                 (let ((target (channel-target candidate)))
                   (format nil "QQ ~a ~a as ~a."
                           (if (equal (source-field candidate "chat_kind") "channel")
                               "group" "private chat")
                           (or (getf target :channel-id) "unknown")
                           (or (onebot-adapter-nickname adapter) "the bot"))))))

(defun onebot-inbound-policy (adapter settings)
  "One inbound policy for the lane. The mention rule is applied when the
candidate is made, so the gate asks who may talk, and where."
  (make-inbound-policy
   :allowed-channels (getf settings :allowed-groups)
   :allowed-users (onebot-adapter-allowed-users adapter)
   :require-mention nil
   :self-id (onebot-adapter-self-id adapter)
   :dm-policy :enabled
   :group-policy :enabled
   :channel-match :exact
   :allow-bots :none))

;;; --- the lap ----------------------------------------------------------------------

(defun onebot-take-messages (adapter chat-kind chat-id body)
  "One history answer: every message past this chat's read position, in id
order, becomes a candidate the host is handed. The read position is saved
BEFORE the asks go out, so a crash can lose one answer but never answer the
same message twice."
  (let* ((state (onebot-adapter-state adapter))
         (chat (onebot-chat-key chat-kind chat-id))
         (messages (sort (coerce (or (nlk:json-value body :array "data" "messages") #()) 'list)
                         #'< :key (lambda (message)
                                    (onebot-id-number
                                     (nlk:json-value message :any "message_id"))))))
    (cond
      ((null messages) nil)
      ((not (onebot-has-cursor-p state chat))
       (onebot-advance-cursor state chat
                               (nlk:json-value (car (last messages)) :any "message_id"))
       nil)
      (t
       (let* ((fresh (remove-if-not
                       (lambda (message)
                         (> (onebot-id-number (nlk:json-value message :any "message_id"))
                            (onebot-cursor state chat)))
                       messages))
              (chosen (loop for message in fresh
                            for candidate = (onebot-message-candidate
                                             adapter message chat-kind chat-id)
                            when candidate collect candidate)))
         (when fresh
           (onebot-advance-cursor state chat
                                   (nlk:json-value (car (last fresh)) :any "message_id")))
         chosen)))))

(defun onebot-lap (adapter stop-p)
  "One poll: the history of every chat this lane serves. Answers the seconds
to wait before the next lap, or :stop."
  (when (funcall stop-p) (return-from onebot-lap :stop))
  (let* ((host (onebot-adapter-host adapter))
         (executor (host-executor host))
         (chats (onebot-watched-chats adapter))
         (trouble nil))
    (dolist (chat chats)
      (when (funcall stop-p) (return-from onebot-lap :stop))
      (let* ((kind (getf chat :kind))
             (id (getf chat :id))
             (answer (execute-plan executor (onebot-history-plan adapter kind id))))
        (if (onebot-ok-p answer)
            (dolist (candidate (onebot-take-messages
                                 adapter kind id (execution-body answer)))
              (handler-case (handle-candidate host candidate)
                (error (condition)
                  (warn "onebot: chat ~a ask signalled: ~a" id condition))))
            (setf trouble (probe-failure answer)))))
    (if trouble
        (set-channel-status "onebot" :state :degraded :connected nil :detail trouble)
        (set-channel-status "onebot" :state :connected :connected t
                                       :detail (format nil "~d chat~:p" (length chats))))
    (onebot-adapter-poll-seconds adapter)))

;;; --- start and stop ----------------------------------------------------------------

(defun onebot-settings (section)
  "The channels.onebot SECTION as the plist this lane runs on."
  (nlk:section-settings (nlk:find-section '("channels" "onebot")) section))

(defun onebot-api-base (section)
  "The bridge's HTTP API root."
  (nck:config-string section "base_url" "http://127.0.0.1:3000"))

(defun start-channel (section &key executor)
  "Start the OneBot lane from its channels.onebot SECTION; answers the stop
thunk. EXECUTOR overrides the live one -- the scripted test seam."
  (let* ((settings (onebot-settings section))
         (declared (nlk:find-section '("channels" "onebot")))
         (token (or (ignore-errors (resolve-channel-secret section "token")) ""))
         (groups (getf settings :allowed-groups))
         (users (getf settings :allowed-users))
         (owners (resolve-owners "onebot" (getf settings :owner) users "allowed_groups"))
         (allowed-users (and users (union (copy-list users) owners :test #'string=)))
         (bot (or executor
                  (make-dexador-executor :base-url (onebot-api-base section)
                                         :redact-prefixes (list token)))))
    (when (and declared (nlk:section-problems declared section))
      (config-error "~a" (nlk:section-summary declared section)))
    (require-non-empty-allowlist "onebot" "allowed_groups" groups "allowed_users" allowed-users)
    (let ((me (execute-plan bot (onebot-login-plan nil))))
      (unless (onebot-ok-p me)
        (config-error "channels.onebot: the bridge would not say who the bot is (~a); check base_url and the access token (~a)"
                      (probe-failure me) +token-page+))
      (let* ((self-id (or (onebot-id-text (nlk:json-value (execution-body me) :any "data" "user_id")) ""))
             (adapter (make-onebot-adapter
                       :token token
                       :self-id self-id
                       :nickname (or (nlk:json-value (execution-body me) :string "data" "nickname") "")
                       :state (make-onebot-state :self-id self-id)
                       :allowed-groups (copy-list groups)
                       :allowed-users allowed-users
                       :free-groups (getf settings :free-response-groups)
                       :require-mention (getf settings :require-mention)
                       :poll-seconds (or (getf settings :poll-seconds) 5)
                       :catch-up-seconds (* 60 (or (getf settings :catch-up-minutes) 1440))))
             (host (make-host-from-section section
                                           (onebot-platform :adapter adapter)
                                           :executor (onebot-noting-executor adapter bot)
                                           :policy (onebot-inbound-policy adapter settings)
                                           :owners owners
                                           :request-timeout 30)))
        (setf (onebot-adapter-host adapter) host)
        (let ((groups-answer (execute-plan bot (onebot-groups-plan adapter))))
          (when (onebot-ok-p groups-answer)
            (onebot-learn-groups adapter (execution-body groups-answer))))
        (run-host host "poll" (lambda (stop-p) (onebot-lap adapter stop-p))
                  :on-start (lambda () (setf *onebot-adapter* adapter))
                  :on-stop (lambda ()
                             (when (eq *onebot-adapter* adapter)
                               (setf *onebot-adapter* nil))))))))

(defun probe-channel (section &key executor)
  "What the bridge can see, as text: the bot it speaks as and the groups the
account is in. Never the token."
  (let* ((token (or (ignore-errors (resolve-channel-secret section "token")) ""))
         (bot (or executor
                  (make-dexador-executor :base-url (onebot-api-base section)
                                         :redact-prefixes (list token)))))
    (let ((me (execute-plan bot (onebot-login-plan nil))))
      (unless (onebot-ok-p me)
        (return-from probe-channel
          (format nil "onebot: the bridge did not answer (~a); set base_url and the access token (~a)"
                  (probe-failure me) +token-page+)))
      (let* ((adapter (make-onebot-adapter :token token))
             (groups (execute-plan bot (onebot-groups-plan adapter))))
        (unless (onebot-ok-p groups)
          (return-from probe-channel
            (format nil "onebot: the group list could not be read (~a)" (probe-failure groups))))
        (let* ((count (onebot-learn-groups adapter (execution-body groups)))
               (listing '()))
          (maphash (lambda (id name) (push (format nil "~a (~a)" id name) listing))
                   (onebot-adapter-groups adapter))
          (format nil "onebot: ~a (QQ ~a) is in ~d group~:p: ~{~a~^; ~}"
                  (or (nlk:json-value (execution-body me) :string "data" "nickname") "?")
                  (or (onebot-id-text (nlk:json-value (execution-body me) :any "data" "user_id")) "?")
                  count (sort listing #'string<)))))))

;;; --- what this lane's section is made of -------------------------------

(nlk:define-section ("channels" "onebot")
  (:guide "run a self-hosted OneBot 11 bridge -- NapCat, Lagrange, LLOneBot or go-cqhttp -- logged in as the QQ account the bot speaks as, with its HTTP API on (the plugin's forward endpoint), and set base_url to that API; if the bridge has an access token, keep it in a file or an environment variable, never in the config. Then set allowed_groups to the QQ group numbers the bot answers in and allowed_users to the QQ numbers allowed to drive it in private, and ask for (nck:probe \"onebot\") to name the groups the account is in.")
  (:check #'probe-channel)
  (:any-of "allowed_groups" "allowed_users")
  ("base_url" :string :required t
              :doc "the bridge's HTTP API root, e.g. http://127.0.0.1:3000")
  ("token_env" :env :doc "environment variable holding the bridge's access token")
  ("token_file" :path :doc "file holding that access token; leave both unset for a bridge without one")
  ("allowed_groups" :list
                    :doc "QQ group numbers the bot serves and answers in")
  ("allowed_users" :list
                   :doc "QQ numbers allowed to drive the bot in a private chat")
  ("owner" :list :doc "the operator's QQ number, whose word is standing policy in the chat")
  ("free_response_groups" :list
                          :doc "group numbers where every message is answered, an at or not")
  ("require_mention" :boolean :default t
                     :doc "in a group, answer only a message that ats the bot or replies to one of its own")
  ("poll_seconds" :integer :default 5 :min 2
                  :doc "seconds between history reads")
  ("catch_up_minutes" :integer :default 1440 :min 0
                      :doc "asks said while the bot was away are answered when they are at most this old; 0 answers none of them"))
