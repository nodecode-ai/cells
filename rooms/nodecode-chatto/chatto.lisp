;;;; chatto.lisp --- the Chatto lane: login, rooms, the poll, the platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Chatto is a self-hosted team chat server. Its API is ConnectRPC: JSON
;;;; over HTTP POST at /api/connect/<service>/<method>, a bearer token from
;;;; /auth/login. The plugin this is rewritten from listens to a websocket
;;;; realtime feed; this lane polls instead, which needs no protobuf codec and
;;;; no long-lived socket, and means a mention is answered within poll_seconds.

(in-package #:nodecode-chatto)

(nlk:define-section ("channels" "chatto")
  (:guide "url is the Chatto server, e.g. https://chat.example.com; then the bot account's login and a password file; then allowed_users and owner; next step: (nck:probe \"chatto\") names the rooms it can see")
  ("url"             :string  :required t :doc "base URL of the Chatto server")
  ("login"           :string  :required t :doc "the bot account's Chatto login")
  ("password_file"   :path               :doc "file holding the bot account's password")
  ("password_env"    :env                :doc "environment variable holding the password")
  ("allowed_users"   :list               :doc "Chatto logins allowed to talk to the bot")
  ("allowed_rooms"   :list               :doc "room ids the bot serves; empty means every room it is in")
  ("owner"           :list               :doc "logins whose instructions are standing policy")
  ("free_response_rooms" :list           :doc "room ids answered without a mention")
  ("require_mention" :boolean :default t :doc "answer only @mentions in rooms; direct messages always answer"))

;;; --- constants ------------------------------------------------------------------

(defparameter +login-path+ "/auth/login" "Where a token comes from.")
(defparameter +rpc+ "/api/connect/" "Every ConnectRPC service path starts here.")
(defparameter +rooms-path+ (concatenate 'string +rpc+ "chatto.api.v1.RoomDirectoryService/ListRooms"))
(defparameter +events-path+ (concatenate 'string +rpc+ "chatto.api.v1.RoomService/GetRoomEvents"))
(defparameter +create-path+ (concatenate 'string +rpc+ "chatto.api.v1.MessageService/CreateMessage"))
(defparameter +delete-path+ (concatenate 'string +rpc+ "chatto.api.v1.MessageService/DeleteMessage"))
(defparameter +typing-path+ (concatenate 'string +rpc+ "chatto.api.v1.RoomService/UpdateTypingIndicator"))


(defun chatto-headers (token)
  "The one header a Chatto call carries: the bearer TOKEN."
  (list (cons "authorization" (format nil "Bearer ~a" token))))

;;; --- the adapter ----------------------------------------------------------------

(defstruct (chatto-adapter (:copier nil) (:constructor make-chatto-adapter))
  (host nil)
  (base-url "" :type string)
  (login "" :type string)
  (token "" :type string)
  (rooms (make-hash-table :test #'equal))
  (seen (make-hash-table :test #'equal))
  (allowed-users '())
  (allowed-rooms '())
  (poll-seconds 5 :type integer)
  (first-lap t)
  (require-mention t))

(defvar *chatto-adapter* nil "The live lane, or NIL when it is stopped.")

;;; --- text -----------------------------------------------------------------------

(defun chatto-mention-p (text login)
  "Whether TEXT mentions LOGIN as Chatto spells a mention: @login."
  (and (stringp text) (stringp login) (plusp (length login))
       (let ((needle (format nil "@~a" login)))
         (and (search needle text) t))))

(defun chatto-strip-mention (text login)
  "TEXT with our own @login taken out, and the punctuation after it."
  (when (and (stringp login) (plusp (length login)) (stringp text))
    (setf text (ppcre:regex-replace-all
                (format nil "@~a[:,]?" (ppcre:quote-meta-chars login)) text "")))
  (nlk:trimmed (or text "")))

;;; --- what a room event becomes --------------------------------------------------

(defun chatto-message-candidate (event room-id &key login dm-p)
  "One Chatto room EVENT as the kit's candidate, or NIL when it says nothing."
  (let* ((msg (nlk:json-value event :object "messagePosted" "message"))
         (id (and msg (nlk:json-value msg :string "id")))
         (thread (and msg (nlk:json-value msg :string "thread" "threadRootEventId")))
         (body (or (and msg (nlk:json-value msg :string "body")) ""))
         (actor (and msg (or (nlk:json-value msg :string "actorId") "")))
         (name (and msg (or (nlk:json-value msg :string "actorLogin")
                            (nlk:json-value msg :string "actorDisplayName")
                            actor))))
    (when (and id (or (plusp (length body)) thread))
      (nlk:json-object
       "text" body
       "source"
       (nlk:json-object
        "platform" "chatto"
        "chat_kind" (cond ((and thread (not dm-p)) "thread")
                           (dm-p "direct_message")
                           (t "channel"))
        "channel_id" room-id
        "user_id" actor
        "user_name" name
        "message_id" id
        "reply_to_message_id" thread
        "addressed" (and (or (chatto-mention-p body login)
                             dm-p
                             (uiop:string-prefix-p "/" (nlk:trimmed body)))
                         t)
        "is_bot" nil)))))

(defun chatto-message-id (body)
  "The id a CreateMessage answer gives its message, in whichever of the
spellings Chatto versions use."
  (or (nlk:json-value body :string "eventId")
      (nlk:json-value body :string "messageId")
      (nlk:json-value body :string "id")
      (nlk:json-value body :string "event" "id")
      (nlk:json-value body :string "message" "id")))

;;; --- plans ----------------------------------------------------------------------

(defun chatto-login-plan (login password)
  (rest-plan "POST" +login-path+ "Chatto login" t 20
             (nlk:json-object "login" login "password" password)))

(defun chatto-rooms-plan (adapter)
  (rest-plan "POST" +rooms-path+ "Chatto rooms" t 30 (nlk:json-object)
             (chatto-headers (chatto-adapter-token adapter))))

(defun chatto-events-plan (adapter room-id)
  (rest-plan "POST" +events-path+ "Chatto events" t 30
             (nlk:json-object "roomId" room-id)
             (chatto-headers (chatto-adapter-token adapter))))

(defun chatto-message-plan (adapter target chunk
                            &key reply-to timeout-seconds &allow-other-keys)
  "What one answer is: CreateMessage in the room, in the ask's thread."
  (let ((body (nlk:json-object "roomId" (getf target :channel-id) "body" chunk)))
    (when reply-to (setf (gethash "threadRootEventId" body) reply-to))
    (rest-plan "POST" +create-path+ "Chatto message" t (or timeout-seconds 30) body
               (chatto-headers (chatto-adapter-token adapter)))))

(defun chatto-delete-plan (adapter target message-id &key timeout-seconds &allow-other-keys)
  (rest-plan "POST" +delete-path+ "Chatto delete" t (or timeout-seconds 30)
             (nlk:json-object "roomId" (getf target :channel-id) "eventId" message-id)
             (chatto-headers (chatto-adapter-token adapter))))

(defun chatto-typing-plan (adapter target &key timeout-seconds &allow-other-keys)
  (rest-plan "POST" +typing-path+ "Chatto typing" nil (or timeout-seconds 10)
             (nlk:json-object "roomId" (getf target :channel-id))
             (chatto-headers (chatto-adapter-token adapter))))

;;; --- the platform ---------------------------------------------------------------


(defun chatto-platform (&key adapter)
  "What Chatto contributes to the kit's host: the three plans a lane draws
with, room-scoped message ids, the @login mention, and where the room is."
  (make-platform
   :id "chatto"
   :name "Chatto"
   :noun "room"
   :owner-label "Chatto login"
   :session-prefix "chatto"
   :contract-section "chatto-channel"
   :text-limit 10000
   :typing-refresh-ms 30000
   :plan-message (lambda (target chunk &rest keys)
                    (apply #'chatto-message-plan adapter target chunk keys))
   ;; Chatto's edit call is not modelled here: a card's edit is a fresh line in
   ;; the same thread, which every Chatto client shows in place.
   :plan-edit (lambda (target message-id text &rest keys)
                 (declare (ignore message-id))
                 (apply #'chatto-message-plan adapter target text keys))
   :plan-delete (lambda (target message-id &rest keys)
                   (apply #'chatto-delete-plan adapter target message-id keys))
   :plan-typing (lambda (target &rest keys)
                   (apply #'chatto-typing-plan adapter target keys))
   :message-id-of #'chatto-message-id
   :strip-mention (lambda (text) (chatto-strip-mention text (chatto-adapter-login adapter)))
   :where-text (lambda (candidate)
                 (let ((target (channel-target candidate)))
                   (format nil "Chatto room ~a as ~a."
                           (or (getf target :channel-id) "unknown")
                           (chatto-adapter-login adapter))))))

(defun chatto-policy (adapter settings)
  (make-inbound-policy
   :allowed-channels (getf settings :allowed-rooms)
   :allowed-users (chatto-adapter-allowed-users adapter)
   :require-mention (and (chatto-adapter-require-mention adapter) t)
   :free-response-channels (getf settings :free-response-rooms)
   :mention-target (chatto-adapter-login adapter)
   :mention-test #'chatto-mention-p
   :self-id (chatto-adapter-login adapter)
   :channel-match :exact
   :allow-bots :none))

;;; --- the rooms the lane serves ---------------------------------------------------

(defun chatto-learn-rooms (adapter body)
  "Remember every room BODY's ListRooms answer names: its name, its kind and
whether the bot is in it."
  (loop for entry across (or (nlk:json-array body "rooms") #())
        for room = (nlk:json-value entry :object "room")
        for id = (and room (nlk:json-value room :string "id"))
        when (and id (plusp (length id)))
          do (setf (gethash id (chatto-adapter-rooms adapter))
                   (list :name (or (nlk:json-value room :string "name") id)
                         :dm-p (equal "ROOM_KIND_DM" (nlk:json-value room :string "kind"))
                         :member-p (and (nlk:json-value entry :boolean "viewerState" "isMember") t)))))

(defun chatto-watched-rooms (adapter)
  "The rooms this lane polls: the named ones, else every room the bot is in."
  (or (chatto-adapter-allowed-rooms adapter)
      (loop for id being the hash-keys of (chatto-adapter-rooms adapter)
            for room = (gethash id (chatto-adapter-rooms adapter))
            when (getf room :member-p) collect id)))

(defun chatto-dm-p (adapter room-id)
  (getf (gethash room-id (chatto-adapter-rooms adapter)) :dm-p))

(defun chatto-seen-p (adapter room-id event-id)
  (gethash event-id (or (gethash room-id (chatto-adapter-seen adapter))
                         (setf (gethash room-id (chatto-adapter-seen adapter))
                               (make-hash-table :test #'equal)))))

(defun chatto-take-events (adapter room-id body)
  "One GetRoomEvents answer: every event not seen yet that says something
goes to the host as an ask -- unless this is the first lap, which only
marks where the rooms stand and answers nothing said before it."
  (let ((first-p (chatto-adapter-first-lap adapter)))
    (loop for event across (or (nlk:json-array body "page" "events") #())
          for id = (nlk:json-value event :string "id")
          when (and id (plusp (length id)) (not (chatto-seen-p adapter room-id id)))
            do (setf (gethash id (or (gethash room-id (chatto-adapter-seen adapter))
                                      (setf (gethash room-id (chatto-adapter-seen adapter))
                                            (make-hash-table :test #'equal))))
                     t)
               (unless first-p
                 (nlk:when-let (candidate (chatto-message-candidate
                                           event room-id
                                           :login (chatto-adapter-login adapter)
                                           :dm-p (chatto-dm-p adapter room-id)))
                   (handler-case (handle-candidate (chatto-adapter-host adapter) candidate)
                     (error (condition)
                       (warn "chatto event handling signalled: ~a" condition))))))))

;;; --- the lap --------------------------------------------------------------------

(defun chatto-lap (adapter stop-p)
  "One poll: the room list, then each served room's recent events. Answers
the seconds to wait before the next, or :stop."
  (when (funcall stop-p) (return-from chatto-lap :stop))
  (let ((host (chatto-adapter-host adapter)))
    (let ((rooms (execute-plan (host-executor host) (chatto-rooms-plan adapter))))
      (cond
        ((not (execution-ok-p rooms))
         (set-channel-status "chatto" :state :degraded :connected nil
                                      :detail (probe-failure rooms))
         (chatto-adapter-poll-seconds adapter))
        (t
         (chatto-learn-rooms adapter (execution-body rooms))
         (dolist (room (chatto-watched-rooms adapter))
           (when (funcall stop-p) (return-from chatto-lap :stop))
           (let ((events (execute-plan (host-executor host) (chatto-events-plan adapter room))))
             (if (execution-ok-p events)
                 (handler-case (chatto-take-events adapter room (execution-body events))
                   (error (condition) (warn "chatto room ~a signalled: ~a" room condition)))
                 (set-channel-status "chatto" :state :degraded :connected nil
                                              :detail (probe-failure events)))))
         (setf (chatto-adapter-first-lap adapter) nil)
         (set-channel-status "chatto" :state :connected :connected t
                                      :detail (format nil "~d room~p"
                                                      (length (chatto-watched-rooms adapter))
                                                      (length (chatto-watched-rooms adapter))))
         (chatto-adapter-poll-seconds adapter))))))

;;; --- start ----------------------------------------------------------------------


(defun start-channel (section &key executor)
  "Start the Chatto lane from its channels.chatto SECTION; answers the stop
thunk. EXECUTOR overrides the live one -- the scripted test seam."
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "chatto")) section))
         (url (or (getf settings :url) ""))
         (login (or (getf settings :login) ""))
         (password (resolve-channel-secret section "password"))
         (configured (getf settings :allowed-users))
         (owners (resolve-owners "chatto" (getf settings :owner) configured "allowed_rooms"))
         (allowed-users (and configured (union configured owners :test #'string=)))
         (allowed-rooms (getf settings :allowed-rooms))
         (base (string-right-trim "/" url))
         (bot (or executor
                  (make-dexador-executor :base-url base :redact-prefixes (list password)))))
    (when (zerop (length base))
      (config-error "channels.chatto: url is not set -- the server's address, e.g. https://chat.example.com"))
    (require-non-empty-allowlist "chatto" "allowed_rooms" allowed-rooms "allowed_users" allowed-users)
    (let ((answer (execute-plan bot (chatto-login-plan login password))))
      (let ((token (and (execution-ok-p answer)
                        (nlk:json-value (execution-body answer) :string "token"))))
        (unless token
          (config-error "channels.chatto: the server would not log ~a in (~a)" login (probe-failure answer)))
        (let* ((adapter (make-chatto-adapter
                         :base-url base :login login :token token
                         :allowed-users allowed-users :allowed-rooms allowed-rooms
                         :require-mention (getf settings :require-mention)
                         :poll-seconds (max 2 (or (getf settings :poll-seconds) 5))))
               (host (make-host-from-section section
                                             (chatto-platform :adapter adapter)
                                             :executor bot
                                             :policy (chatto-policy adapter settings)
                                             :owners owners
                                             :request-timeout 30)))
          (setf (chatto-adapter-host adapter) host)
          (run-host host "poll" (lambda (stop-p) (chatto-lap adapter stop-p))
                    :on-start (lambda () (setf *chatto-adapter* adapter))
                    :on-stop (lambda ()
                               (when (eq *chatto-adapter* adapter)
                                 (setf *chatto-adapter* nil)))))))))

(defun probe-channel (section &key executor)
  "What the bot account can see, as text: the rooms it is in, by id and
name. Never the token."
  (let* ((url (or (config-string section "url") ""))
         (login (or (config-string section "login") ""))
         (base (string-right-trim "/" url))
         (bot (or executor (make-dexador-executor :base-url base)))
         (answer (execute-plan bot (chatto-login-plan login (resolve-channel-secret section "password"))))
         (token (and (execution-ok-p answer) (nlk:json-value (execution-body answer) :string "token"))))
    (unless token
      (return-from probe-channel
        (format nil "chatto: the server would not log ~a in (~a)" login (probe-failure answer))))
    (let* ((adapter (make-chatto-adapter :base-url base :login login :token token))
           (rooms (execute-plan bot (chatto-rooms-plan adapter))))
      (unless (execution-ok-p rooms)
        (return-from probe-channel (format nil "chatto: rooms could not be read (~a)"
                                          (probe-failure rooms))))
      (chatto-learn-rooms adapter (execution-body rooms))
      (format nil "chatto: ~a on ~a sees ~d room~p: ~{~a~^; ~}"
              login base
              (hash-table-count (chatto-adapter-rooms adapter))
              (hash-table-count (chatto-adapter-rooms adapter))
              (loop for id being the hash-keys of (chatto-adapter-rooms adapter)
                    for room = (gethash id (chatto-adapter-rooms adapter))
                    when (getf room :member-p)
                      collect (format nil "~a (~a~:[~; / direct~])" id (getf room :name) (getf room :dm-p)))))))
