;;;; cell.lisp --- the Nextcloud Talk adapter: the platform, the plans, the
;;;; poll lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the answer, the
;;;; write-back -- is the kit's (host.lisp, room.lisp). This file is what
;;;; Nextcloud Talk contributes: how a message is spelled (Talk's own OCS
;;;; chat and reaction endpoints), where a conversation sits, what a person
;;;; in a room said, and the long-poll lap that turns new messages into
;;;; candidates. The account is a genuine Nextcloud user with an app
;;;; password, so the organism holds no listener and no public address.
;;;;
;;;; Thread topology (kit rule: no network I/O on a thread that is not ours):
;;;;   - channel-nextcloud-talk-poll (supervised lap): the room list now and
;;;;     then, one room's chat long poll per lap, every message through
;;;;     TALK-CANDIDATE, on the lap thread: return fast.
;;;;   - the kit's :FRAME fold and delivery pool: the kit's.

(in-package #:nodecode-channel-nextcloud-talk)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution)
            (host channel-host))

(defparameter +talk-text-limit+ 32000
  "Characters one Talk chat message carries: Talk's own cap, so the kit's
chunking splits only what the server would have refused.")

(defparameter +room-list-limit+ 100
  "Rooms read in one discovery pass; channels.nextcloud-talk.max_rooms caps
how many of them are polled.")

(defparameter +chat+ "/ocs/v2.php/apps/spreed/api/v1/chat"
  "Talk's chat endpoint: GET to read, POST to post, PUT to edit.")

(defparameter +reaction+ "/ocs/v2.php/apps/spreed/api/v1/reaction"
  "Talk's reaction endpoint: POST one emoji, DELETE it.")

(defparameter +rooms+ "/ocs/v2.php/apps/spreed/api/v4/room"
  "Talk's room list, as the account is a member of them.")

(defparameter +whoami+ "/ocs/v2.php/cloud/user"
  "The account an app password belongs to: its user id and display name.")

(defparameter +dm-types+ '(1 6)
  "Talk room types that are one-to-one: 1 one-to-one, 6 a former
one-to-one. Everything else is a group room.")

(defvar *talk-adapter* nil
  "The live Nextcloud Talk adapter, or NIL when the lane is stopped.")

(defparameter +talk-api-primer+
  "Nextcloud Talk is open to you as the account, over its own OCS API, from
eval: (channel-nextcloud-talk:request \"rooms\") lists the rooms;
(channel-nextcloud-talk:request \"chat\" \"<room token>\" :limit 20) reads one;
(channel-nextcloud-talk:request \"send\" \"<room token>\" :message \"...\") posts.
A room is named by its token, a message by its numeric id. Answers come back
as (:status N :body VALUE [:error TEXT]). Talk draws Markdown and allows
32000 characters. The adapter posts your final answer: do not post it again
yourself."
  "What the lane contract says about reaching Talk itself.")

;;; --- reading JSON the lenient way ---------------------------------------------------

(defun talk-field (object key)
  "KEY's value in OBJECT as a string, whatever JSON type it arrived as, or
NIL: Talk sends ids as numbers and words as strings in one object."
  (let ((value (and (hash-table-p object) (gethash key object))))
    (typecase value
      (string value)
      (integer (princ-to-string value))
      (null nil)
      (t (princ-to-string value)))))

(defun talk-object (object key)
  (let ((value (and (hash-table-p object) (gethash key object))))
    (and (hash-table-p value) value)))

(defun talk-true-p (object key)
  (and (hash-table-p object) (eq t (gethash key object))))

(defun talk-data (body)
  "The payload of an OCS answer, or NIL: Talk wraps every answer as
{\"ocs\": {\"meta\": {...}, \"data\": ...}}."
  (nlk:json-value body :any "ocs" "data"))

(defun talk-rows (body)
  "The OCS payload as an array, an empty vector when it is not one."
  (let ((data (talk-data body)))
    (if (vectorp data) data #())))

;;; --- the words a person said -------------------------------------------------------

(defun replace-all (string part replacement)
  "STRING with every occurrence of PART replaced by REPLACEMENT."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string :start2 old-pos)
          do (write-string string out :start old-pos :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun talk-resolve-mentions (text parameters)
  "TEXT with Talk's {mention-userN} placeholders written as @name.
Without this the model reads a placeholder and decides it was not named."
  (if (hash-table-p parameters)
      (loop for key being the hash-keys of parameters
            for param = (gethash key parameters)
            for name = (and (hash-table-p param) (talk-field param "name"))
            for placeholder = (format nil "{~a}" key)
            when (and name (search placeholder text))
              do (setf text (replace-all text placeholder (format nil "@~a" name)))
            finally (return text))
      (or text "")))

(defun talk-visible (text)
  "TEXT without its code: a mention quoted inside a span is an example, not
an address."
  (ppcre:regex-replace-all "```[\\s\\S]*?```|`[^`\\n]*`" (or text "") " "))

(defun talk-mentioned-p (text &key (user-id "") (display-name ""))
  "Whether TEXT names the account, by its user id or Talk's quoted
@\"Display Name\": case-insensitive, outside code."
  (let ((visible (talk-visible text)))
    (loop for name in (remove-if (lambda (one) (or (null one) (zerop (length one))))
                                 (list user-id display-name))
          thereis (ppcre:scan (format nil "(?i)(^|[^\\w.@-])@\"?~a\"?(?![\\w.-])"
                                       (ppcre:quote-meta-chars name))
                               visible))))

(defun talk-strip-mention (text &key (user-id "") (display-name ""))
  "TEXT without a leading @us, so a slash command reaches the core as one."
  (let ((text (or text "")))
    (loop for name in (sort (remove-if (lambda (one) (or (null one) (zerop (length one))))
                                       (list user-id display-name))
                             #'> :key #'length)
          do (multiple-value-bind (match regs)
                 (ppcre:scan-to-strings
                  (format nil "(?i)^\\s*@\"?~a\"?(?![\\w.-])[\\s,:]*"
                          (ppcre:quote-meta-chars name))
                  text)
               (declare (ignore regs))
               (when match (return (nlk:trimmed (subseq text (length match))))))
          finally (return (nlk:trimmed text)))))

;;; --- paths and plans ---------------------------------------------------------------

(defun talk-path (path &rest pairs)
  "PATH with format=json and PAIRS as its query, escaped; a NIL value is
left out."
  (with-output-to-string (out)
    (format out "~a?format=json" path)
    (loop for (key value) on pairs by #'cddr
          when value do (format out "&~a=~a" key (quri:url-encode (princ-to-string value))))))

(defun room-path (token suffix)
  "ROOM TOKEN's chat endpoint, SUFFIX appended when it is said."
  (format nil "~a/~a~@[/~a~]" +chat+ (quri:url-encode token) suffix))

(defun talk-chat (target)
  "The room token a kit TARGET names."
  (getf target :channel-id))

(defun text-of (chunk)
  "The words CHUNK carries, whichever of the kit's shapes it is."
  (if (stringp chunk) chunk (or (ignore-errors (text-chunk-text chunk)) "")))

(defun message-plan (target chunk &key reply-to card files (timeout-seconds 60)
                     &allow-other-keys)
  "One message of CHUNK's words in TARGET's room, replying under REPLY-TO.
CARD and FILES are drawn nowhere: Talk here carries words alone."
  (declare (ignore card files))
  (let ((body (nlk:json-object "message" (text-of chunk))))
    (when reply-to
      (setf (gethash "replyTo" body)
            (or (ignore-errors (parse-integer reply-to)) reply-to)))
    (rest-plan "POST" (talk-path (room-path (talk-chat target) nil))
               "nextcloud_talk_send" nil (or timeout-seconds 60) body)))

(defun edit-plan (target message-id text &key (timeout-seconds 60) &allow-other-keys)
  "A message's words, edited where it was posted: Talk edits its own."
  (rest-plan "PUT" (talk-path (room-path (talk-chat target) message-id))
             "nextcloud_talk_edit" nil (or timeout-seconds 60)
             (nlk:json-object "message" (or text ""))))

(defun delete-plan (target message-id &key (timeout-seconds 60) &allow-other-keys)
  (rest-plan "DELETE" (talk-path (room-path (talk-chat target) message-id))
             "nextcloud_talk_delete" nil (or timeout-seconds 60)))

(defun typing-plan (target &key (timeout-seconds 30) &allow-other-keys)
  "Talk's typing signal in TARGET's room; message id 0 says \"a new one\"."
  (rest-plan "POST" (talk-path (room-path (talk-chat target) "0/typing"))
             "nextcloud_talk_typing" nil (or timeout-seconds 30)))

(defun reaction-plans (target message-id emoji &key previous (timeout-seconds 60)
                       &allow-other-keys)
  "Leave EMOJI as the account's one reaction on MESSAGE-ID: the previous one
off first, then this one on. NIL clears every reaction."
  (let* ((path (format nil "~a/~a/~a" +reaction+ (quri:url-encode (talk-chat target))
                        (quri:url-encode (princ-to-string message-id))))
         (off (when (and previous (plusp (length previous)))
                (rest-plan "DELETE" (talk-path path) "nextcloud_talk_reaction_off"
                           nil (or timeout-seconds 60))))
         (on (when (and emoji (plusp (length emoji)))
               (rest-plan "POST" (talk-path path) "nextcloud_talk_reaction_on"
                          nil (or timeout-seconds 60)
                          (nlk:json-object "reaction" emoji)))))
    (remove nil (list off on))))

(defun message-id-of (body)
  "The id a send answered with, so the lane can edit what it posted."
  (talk-field (talk-data body) "id"))

(defun room-plan (&key (timeout-seconds 30))
  "The rooms the account is a member of."
  (rest-plan "GET" (talk-path +rooms+ "limit" +room-list-limit+)
             "nextcloud_talk_rooms" nil (or timeout-seconds 30)))

(defun whoami-plan (&key (timeout-seconds 30))
  (rest-plan "GET" (talk-path +whoami+) "nextcloud_talk_whoami" nil (or timeout-seconds 30)))

(defun poll-plan (token cursor timeout)
  "TOKEN's chat from CURSOR: Talk holds the answer open TIMEOUT seconds
while the room is quiet, so a lap costs one request per window."
  (rest-plan "GET" (talk-path (room-path token nil)
                               "lookIntoFuture" 1
                               "lastKnownMessageId" cursor
                               "includeLastKnown" 0
                               "timeout" timeout
                               "limit" +room-list-limit+)
             "nextcloud_talk_poll" nil (+ timeout 20)))

(defun baseline-plan (token &key (timeout-seconds 30))
  "The room's newest message, read without waiting: where a first poll
starts, so a start answers nothing said before it."
  (rest-plan "GET" (talk-path (room-path token nil) "lookIntoFuture" 0 "limit" 1)
             "nextcloud_talk_baseline" nil (or timeout-seconds 30)))

;;; --- what a person in a room said ---------------------------------------------------

(defun talk-room-type (room)
  "ROOM's Talk type as a number, NIL when it carries none."
  (let ((raw (and (hash-table-p room) (gethash "type" room))))
    (typecase raw
      (integer raw)
      (string (ignore-errors (parse-integer raw :junk-allowed t)))
      (t nil))))

(defun talk-dm-p (room-type)
  (and (integerp room-type) (member room-type +dm-types+)))

(defun talk-skippable-p (message)
  "A message that is not a person's words: a system event, a deletion, an
edit's shadow, an empty line."
  (or (talk-true-p message "systemMessage")
      (let ((kind (string-downcase (or (talk-field message "messageType") "comment"))))
        (not (member kind '("comment" "voice-message") :test #'string=)))))

(defun talk-candidate (message &key (user-id "") (display-name "") free-room-p
                                 (room-type nil))
  "One Talk chat message as the kit's candidate, or NIL for anything that is
not a person's words or is the account's own. A one-to-one room is
addressed to the account by being one; a group room's message by naming it,
or by being in a room the operator listed as free."
  (let* ((own-p (and (plusp (length user-id))
                      (string-equal (or (talk-field message "actorId") "") user-id)
                      (not (string= "federated_users"
                                    (string-downcase (or (talk-field message "actorType") ""))))))
         (raw (or (talk-field message "message") ""))
         (text (talk-strip-mention
                (talk-resolve-mentions raw (talk-object message "messageParameters"))
                :user-id user-id :display-name display-name))
         (sender (or (talk-field message "actorId") "")))
    (when (and (not own-p)
               (not (talk-skippable-p message))
               (plusp (length text))
               (plusp (length (or (talk-field message "id") ""))))
      (nlk:json-object
       "text" text
       "source"
       (nlk:json-object
        "platform" "nextcloud-talk"
        "chat_kind" (if (talk-dm-p room-type) "dm" "group")
        "channel_id" (or (talk-field message "token") (talk-field message "roomToken") "")
        "user_id" sender
        "user_name" (or (talk-field message "actorDisplayName") sender)
        "message_id" (talk-field message "id")
        "reply_to_message_id" (or (talk-field message "parentMessageId")
                                  (talk-field (talk-object message "parentMessage") "id"))
        "addressed" (and (or (talk-dm-p room-type)
                            free-room-p
                            (talk-mentioned-p raw :user-id user-id
                                               :display-name display-name))
                        t)
        "is_bot" (string= "bots" (string-downcase
                                  (or (talk-field message "actorType") ""))))))))

(defun talk-inbound-policy (&key allowed-users (user-id "") (display-name "")
                                 free-rooms)
  "The kit policy over Talk: people by Nextcloud user id, one-to-one rooms
and group rooms both, a group room's message only when it names the account
or the operator listed that room as free."
  (make-inbound-policy
   :allowed-users allowed-users
   :dm-policy :enabled
   :group-policy :enabled
   :require-mention t
   :free-response-channels free-rooms
   :mention-target (if (plusp (length display-name)) display-name user-id)
   :mention-test (lambda (text target)
                   (declare (ignore target))
                   (talk-mentioned-p text :user-id user-id :display-name display-name))
   :allow-bots :none
   :self-id user-id
   :channel-match :exact))

;;; --- the platform ------------------------------------------------------------------

(defun talk-platform (&key (user-id "") (display-name ""))
  "The Nextcloud Talk PLATFORM: Markdown, 32000 characters to a message, an
answer edited where it was posted, a typing signal, and reactions as
Talk's own emoji on a message."
  (make-platform
   :id "nextcloud-talk"
   :name "Nextcloud Talk"
   :noun "room"
   :owner-label "Nextcloud user id"
   :session-prefix "nextcloud-talk"
   :contract-section "nextcloud-talk-channel"
   :text-limit +talk-text-limit+
   :typing-refresh-ms 30000
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan target chunk keys))
   :plan-edit (lambda (target message-id text &rest keys)
                (apply #'edit-plan target message-id text keys))
   :plan-delete (lambda (target message-id &rest keys)
                  (apply #'delete-plan target message-id keys))
   :plan-typing (lambda (target &rest keys) (apply #'typing-plan target keys))
   :plan-reaction (lambda (target message-id emoji &rest keys)
                    (apply #'reaction-plans target message-id emoji keys))
   :message-id-of #'message-id-of
   :where-text (lambda (candidate)
                 (format nil "a Talk ~a. The account is ~@[~a (~a)~], and answers go in the room the ask came from."
                         (if (equal "dm" (nck:source-field candidate "chat_kind"))
                             "one-to-one room" "group room")
                         display-name user-id))
   :api-primer +talk-api-primer+
   :seams-primer "  channel-nextcloud-talk:talk-candidate (message) -- every chat message the lap reads, on the lap thread: return fast."))

;;; --- the lap -----------------------------------------------------------------------

(defstruct (talk-adapter (:copier nil))
  "One Talk lane's live facts: the host it speaks through, the account it is,
the rooms it polls and where each poll starts."
  (host nil)
  (user-id "" :type string)
  (display-name "" :type string)
  (rooms '() :type list)
  (cursors (make-hash-table :test #'equal))
  (seen (make-hash-table :test #'equal))
  (index 0 :type fixnum)
  (discovered-at 0 :type real)
  (poll-timeout 10 :type integer)
  (discovery-seconds 60 :type integer)
  (max-rooms 20 :type integer)
  (free-rooms '() :type list))

(defun execute (adapter plan)
  (execute-plan (host-executor (talk-adapter-host adapter)) plan))

(defun talk-room-of (row)
  "One row of the room list as the lap keeps it: its token, its name, its
kind."
  (let ((token (talk-field row "token")))
    (when (and token (plusp (length token)))
      (list :token token
            :name (or (talk-field row "displayName") token)
            :type (talk-room-type row)
            :dm (talk-dm-p (talk-room-type row))))))

(defun talk-discover (adapter)
  "The rooms the account is a member of, read now."
  (let ((result (execute adapter (room-plan))))
    (cond
      ((member (execution-status result) '(401 403))
       (set-channel-status "nextcloud-talk" :state :degraded :connected nil
                                          :detail "the server refused the app password")
       :fatal)
      ((not (execution-ok-p result))
       (set-channel-status "nextcloud-talk" :connected nil :detail (execution-error result))
       (warn "nextcloud talk: the room list failed: ~a" (execution-error result))
       (talk-adapter-rooms adapter))
      (t
       (setf (talk-adapter-rooms adapter)
             (loop for row across (talk-rows (execution-body result))
                   for room = (talk-room-of row)
                   when room collect room
                     into rooms
                   finally (return (if (> (length rooms) (talk-adapter-max-rooms adapter))
                                       (subseq rooms 0 (talk-adapter-max-rooms adapter))
                                       rooms))))
       (setf (talk-adapter-discovered-at adapter) (get-universal-time))
       (talk-adapter-rooms adapter)))))

(defun talk-note-message (adapter room message)
  "One chat message through the gate: a candidate when it is a person's
words, the room's cursor moved past it either way."
  (let* ((id (talk-field message "id"))
         (key (format nil "~a/~a" (getf room :token) id)))
    (when id
      (setf (gethash (getf room :token) (talk-adapter-cursors adapter)) id))
    (unless (gethash key (talk-adapter-seen adapter))
      (setf (gethash key (talk-adapter-seen adapter)) t)
      (let ((candidate (talk-candidate message
                                        :user-id (talk-adapter-user-id adapter)
                                        :display-name (talk-adapter-display-name adapter)
                                        :room-type (getf room :type)
                                        :free-room-p (member (getf room :token)
                                                             (talk-adapter-free-rooms adapter)
                                                             :test #'string=))))
        (when candidate
          (when (zerop (length (or (nck:source-field candidate "channel_id") "")))
            (setf (gethash "channel_id" (gethash "source" candidate)) (getf room :token)))
          (handle-candidate (talk-adapter-host adapter) candidate))))))

(defun talk-poll-room (adapter room)
  "One room's read: the baseline on first sight, then the long poll from
the kept cursor. => :quiet, :read, :gone or :failed."
  (let* ((token (getf room :token))
         (cursor (gethash token (talk-adapter-cursors adapter))))
    (if (null cursor)
        (let ((result (execute adapter (baseline-plan token))))
          (if (execution-ok-p result)
              (progn
                (loop for message across (talk-rows (execution-body result))
                      for id = (talk-field message "id")
                      when id do (setf (gethash token (talk-adapter-cursors adapter)) id))
                ;; Nothing said before the start is answered later.
                (setf (gethash token (talk-adapter-cursors adapter))
                      (or (gethash token (talk-adapter-cursors adapter)) "0"))
                :quiet)
              :failed))
        (let ((result (execute adapter (poll-plan token cursor
                                                  (talk-adapter-poll-timeout adapter)))))
          (cond
            ((eql (execution-status result) 304) :quiet)
            ((member (execution-status result) '(403 404)) :gone)
            ((eql (execution-status result) 401)
             (set-channel-status "nextcloud-talk" :state :degraded :connected nil
                                                :detail "the server refused the app password")
             :failed)
            ((not (execution-ok-p result))
             (warn "nextcloud talk: the read of ~a failed: ~a" token (execution-error result))
             :failed)
            (t
             (loop for message across (talk-rows (execution-body result))
                   do (handler-case (talk-note-message adapter room message)
                        (error (condition)
                          (warn "nextcloud talk: a message signalled: ~a" condition))))
             ;; A quiet answer moves the cursor where the room's own last
             ;; message is, so the next read does not ask for history.
             (setf (gethash token (talk-adapter-cursors adapter))
                   (or (gethash token (talk-adapter-cursors adapter)) cursor))
             :read))))))

(defun talk-poll-lap (adapter stop-p)
  "One lap: the room list when it is stale, else the next room's chat from
its kept cursor. => seconds to wait before the next lap, or :FATAL for a
credential the server refuses."
  (when (funcall stop-p) (return-from talk-poll-lap :stop))
  (let ((rooms (if (or (null (talk-adapter-rooms adapter))
                       (>= (- (get-universal-time) (talk-adapter-discovered-at adapter))
                           (talk-adapter-discovery-seconds adapter)))
                   (let ((found (talk-discover adapter)))
                     (when (eq found :fatal)
                       (return-from talk-poll-lap :fatal))
                     found)
                   (talk-adapter-rooms adapter))))
    (if (null rooms)
        (progn
          (set-channel-status "nextcloud-talk" :state :running :connected t
                                             :detail "the account is in no room yet")
          (talk-adapter-discovery-seconds adapter))
        (let* ((index (mod (talk-adapter-index adapter) (length rooms)))
               (room (nth index rooms)))
          (setf (talk-adapter-index adapter) (mod (1+ index) (length rooms)))
          (set-channel-status "nextcloud-talk" :state :running :connected t)
          (case (talk-poll-room adapter room)
            (:gone
             (setf (talk-adapter-rooms adapter)
                   (remove (getf room :token) rooms :key (lambda (one) (getf one :token))
                                                   :test #'string=))
             0.5)
            (:failed 5.0)
            (t 0.05))))))

;;; --- the door ----------------------------------------------------------------------

(defun request (op &rest arguments)
  "One Talk call from eval: OP names it (\"rooms\", \"chat\", \"send\"),
the rest are its arguments. => (:status N :body VALUE [:error TEXT])."
  (unless (and *talk-adapter* (talk-adapter-host *talk-adapter*))
    (error "the nextcloud talk channel is not running"))
  (let* ((token (getf arguments :token))
         (id (getf arguments :message_id))
         (plan
           (cond
             ((string= op "rooms") (room-plan))
             ((string= op "chat")
              (rest-plan "GET" (talk-path (room-path token nil)
                                           "limit" (or (getf arguments :limit) 50))
                         "nextcloud_talk_chat" nil 60))
             ((string= op "send")
              (message-plan (list :channel-id token) (or (getf arguments :message) "")
                            :reply-to id))
             ((string= op "delete") (delete-plan (list :channel-id token) id))
             ((string= op "react")
              (first (reaction-plans (list :channel-id token) id
                                     (getf arguments :reaction))))
             (t (rest-plan "GET" (talk-path (format nil "/~a" op))
                           "nextcloud_talk_raw" nil 60))))
         (result (execute *talk-adapter* plan)))
    (list* :status (execution-status result) :body (execution-body result)
           (and (execution-error result) (list :error (execution-error result))))))

;;; --- the door the kit opens ---------------------------------------------------------

(defun talk-headers (user password)
  "Every OCS request's headers: the app password as Basic auth, Talk's JSON
marker, and no cookie jar said."
  (list (cons "authorization"
              (format nil "Basic ~a"
                      (cl-base64:string-to-base64-string
                       (format nil "~a:~a" user password) :external-format :utf-8)))
        (cons "ocs-apirequest" "true")
        (cons "accept" "application/json")))

(defun start-channel (section &key executor)
  "Start the Nextcloud Talk lane from its channels.nextcloud-talk config
SECTION. => a stop thunk."
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "nextcloud-talk"))
                                         section))
         (server (let ((named (config-string section "server_url")))
                   (if (plusp (length named)) named
                       (config-error "channels.nextcloud-talk: server_url is not set -- the instance's address, e.g. https://cloud.example.com"))))
         (password (resolve-channel-secret section "app_password"))
         (username (let ((named (config-string section "username")))
                     (if (plusp (length named))
                         named
                         (config-error "channels.nextcloud-talk: username is not set -- the account's Nextcloud user id"))))
         (raw (or executor (make-dexador-executor :base-url server
                                                  :headers (talk-headers username password)
                                                  :redact-prefixes (list password))))
         (me (execute-plan raw (whoami-plan)))
         (user-id (and (execution-ok-p me) (talk-field (talk-data (execution-body me)) "id"))))
    (unless user-id
      (config-error "channels.nextcloud-talk: the server did not take the app password (~a)"
                    (or (execution-error me) "no user in the answer")))
    (let* ((display-name (or (talk-field (talk-data (execution-body me)) "displayname")
                             (config-string section "display_name")))
           (named (getf settings :allowed-users))
           (owners (resolve-owners "nextcloud-talk" (getf settings :owner) named "allowed_users"))
           (allowed (and named (union named owners :test #'string=)))
           (free-rooms (getf settings :free-rooms))
           (host (progn
                   (require-non-empty-allowlist "nextcloud-talk" "allowed_users" allowed)
                   (make-host-from-section
                    section (talk-platform :user-id user-id :display-name (or display-name ""))
                    :executor raw
                    :owners owners
                    :policy (talk-inbound-policy :allowed-users allowed
                                                 :user-id user-id
                                                 :display-name (or display-name "")
                                                 :free-rooms free-rooms)
                    :soul-path (soul-path section)
                    :request-timeout 60)))
           (adapter (make-talk-adapter
                     :host host
                     :user-id user-id
                     :display-name (or display-name "")
                     :poll-timeout (max 1 (or (getf settings :poll-timeout-seconds) 10))
                     :discovery-seconds (max 10 (or (getf settings :discovery-seconds) 60))
                     :max-rooms (max 1 (or (getf settings :max-rooms) 20))
                     :free-rooms free-rooms)))
      (run-host host "poll" (lambda (stop-p) (talk-poll-lap adapter stop-p))
                :on-start (lambda () (setf *talk-adapter* adapter))
                :on-stop (lambda () (when (eq *talk-adapter* adapter)
                                      (setf *talk-adapter* nil)))))))

;;; --- what the app password can see --------------------------------------------------

(defun probe-channel (section &key executor)
  "What the app password in SECTION can see, as lines: the account it is, and
the rooms it is in. Never carries the password."
  (let* ((lines '())
         (server (let ((named (config-string section "server_url")))
                   (and (plusp (length named)) named)))
         (username (let ((named (config-string section "username")))
                     (and (plusp (length named)) named)))
         (password (resolve-channel-secret section "app_password")))
    (flet ((say (control &rest args) (push (apply #'format nil control args) lines)))
      (when (null server)
        (config-error "channels.nextcloud-talk: server_url is not set -- the instance's address"))
      (when (null username)
        (config-error "channels.nextcloud-talk: username is not set -- the account's Nextcloud user id"))
      (let* ((exec (or executor (make-dexador-executor :base-url server
                                                        :headers (talk-headers username password)
                                                        :redact-prefixes (list password))))
             (me (execute-plan exec (whoami-plan))))
        (if (execution-ok-p me)
            (let ((data (talk-data (execution-body me))))
              (say "account: ~a (~a)" (or (talk-field data "displayname") "unnamed")
                   (or (talk-field data "id") username))
              (let ((rooms (execute-plan exec (room-plan))))
                (if (execution-ok-p rooms)
                    (let ((rows (talk-rows (execution-body rooms))))
                      (say "rooms: ~d" (length rows))
                      (loop for row across rows
                            for room = (talk-room-of row)
                            when room
                              do (say "  ~a (~:[group~;one-to-one~])"
                                      (getf room :name) (if (getf room :dm) t nil))))
                    (say "rooms: ~a" (execution-error rooms)))))
            (say "app password: the server refused it (~a)" (or (execution-error me) "no answer")))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(nlk:define-section ("channels" "nextcloud-talk")
  (:guide "make a dedicated Nextcloud account for the agent and an app password for it (Settings, Security, Devices and sessions); keep the password in an environment variable or a file only you can read, and name that here; give the account's user id and list the Nextcloud user ids it may answer; the account joins rooms like a person and reads them by long polling, so nothing is exposed.")
  (:check #'probe-channel)
  (:one-of "app_password_env" "app_password_file")
  ("server_url" :string :required t :doc "the instance's address, e.g. https://cloud.example.com")
  ("username" :string :required t :doc "the account's Nextcloud user id")
  ("app_password_env" :env :doc "environment variable holding the account's app password")
  ("app_password_file" :path :doc "file holding the app password, one line")
  ("display_name" :string :doc "the account's display name, when the server's answer is not the one to match @mentions on")
  ("allowed_users" :list :required t :doc "Nextcloud user ids whose messages the agent answers")
  ("owner" :list :doc "the operator's Nextcloud user id, whose word is standing policy")
  ("free_rooms" :list :doc "room tokens that answer every message, not only a mention")
  ("poll_timeout_seconds" :integer :default 10 :min 1 :doc "seconds one room's read is held open while it is quiet")
  ("discovery_seconds" :integer :default 60 :min 10 :doc "seconds between two reads of the room list")
  ("max_rooms" :integer :default 20 :min 1 :doc "rooms polled at most, in turn")
  ("reactions" :boolean :default nil :doc "mark the message being worked on with a reaction")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona")
  ("turn_budget_minutes" :integer :default 0 :doc "minutes an ask's turn may run; 0 caps nothing"))
