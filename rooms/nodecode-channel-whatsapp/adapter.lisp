;;;; adapter.lisp --- the WhatsApp adapter: the platform, the lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the card, the answer,
;;;; the write-back -- is the kit's (host.lisp, room.lisp). This file is what
;;;; WhatsApp contributes: how a message, an edit, a deletion, a typing beat
;;;; and a reaction are spelled (plans.lisp, bound into a PLATFORM), how a
;;;; mention is written, where the chat is, and the lap that turns what the
;;;; driver reports into candidates.
;;;;
;;;; Thread topology (kit rule: no platform I/O on a thread that is not ours):
;;;;   - channel-whatsapp-driver (supervised lap): starts the driver when none
;;;;     runs (building it the first time), then takes its events one by one
;;;;     through WHATSAPP-HANDLE-EVENT until it exits, when the next lap
;;;;     starts another.
;;;;   - channel-whatsapp-driver-read: the driver's stdout, line by line --
;;;;     replies to the calls waiting on them, events onto the lap's queue.
;;;;   - the host's :FRAME fold and channel-whatsapp-deliver pool: the kit's.
;;;;
;;;; The linked device keeps its keys in the home (channels/whatsapp/), so a
;;;; restart picks the session up without linking again, and WhatsApp hands
;;;; over what was said while it was away: CATCH-UP decides what of it to
;;;; answer.

(in-package #:nodecode-channel-whatsapp)

(nlk:access (host channel-host) (result execution) (book whatsapp-book))

(defstruct (whatsapp-adapter (:constructor %make-whatsapp-adapter) (:copier nil))
  (host nil)
  (book (make-whatsapp-book))
  ;; The driver running now, and how a new one is opened: the live process
  ;; (OPEN-DRIVER-PROCESS), or a test's fake.
  (driver nil)
  (open nil :type (or null function))
  ;; Who the linked account is, as the driver last said: phone, lid, jid,
  ;; push_name, linked. Read by the platform's closures at call time.
  (identity (nlk:json-object) :type hash-table)
  ;; The newest QR code the driver showed and when, for LINK.
  (qr nil) (qr-at 0)
  ;; channels.whatsapp settings the lap reads.
  (self-chat-p nil) (read-receipts-p t) (reject-calls-p t) (roles-p nil)
  (catch-up-minutes 60 :type integer)
  (allowed-users '())
  ;; caller -> when they were last told the bot takes no calls
  (call-notes (make-hash-table :test #'equal :synchronized t))
  ;; the process's paths, for OPEN-DRIVER-PROCESS and the status line
  (driver-path nil) (db nil) (media nil) (log nil) (lock nil)
  (online t) (device-name "Nodecode"))

(nlk:access (adapter whatsapp-adapter))

(defvar *whatsapp-adapter* nil
  "The live WhatsApp adapter, or NIL when the lane is stopped.")

(defun unix-ms ()
  (* 1000 (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0))))

(defun own (adapter key)
  "What the driver last said of the linked account under KEY (phone, lid,
jid, push_name), or NIL."
  (let ((value (and adapter (gethash key adapter.identity))))
    (and (stringp value) (plusp (length value)) value)))

;;; --- what WhatsApp contributes to the host ---------------------------------------

;;; Data, so an operator's layer can reword it.
(defparameter +whatsapp-api-primer+
  "WhatsApp is open to you as the linked account, from eval, through its driver:
  (ncw:request OP &key body timeout) => (:status N :body VALUE)
OP is one of the driver's operations and BODY its arguments as a keyword plist. A chat is a JID: a group's ends in @g.us, a person's direct chat is <their number>@s.whatsapp.net (digits only, with the country code); a person is named by their number's digits. send (:chat :text :quote MESSAGE-ID :mentions (NUMBERS)), edit (:chat :id :text; only within 20 minutes of sending), revoke (:chat :id), react (:chat :id :emoji; \"\" takes it off), send_file (:chat :path :kind image|video|audio|voice|document|sticker :mime :caption :quote), poll (:chat :name :options (...)), typing (:chat), read (:chat :ids (...)), history (:chat :limit N :before_ms T) -- the messages this device has seen there, oldest first, including what the phone synced when it was linked -- group_info (:chat), groups, resolve (:phones (...)) -- which numbers are on WhatsApp -- contact (:user). Objects come back as keyword plists, arrays as vectors. WhatsApp cannot be read back: a reaction, an edit or a deletion is what you sent, never what is there. To ping someone in a group, write @<their number> in the words; WhatsApp notifies them. Bold is *one star*, italics _underscores_, code `backticks`: the adapter respells Markdown for you. The adapter posts your final answer quoting the ask: do not send it again yourself."
  "What the lane contract says about reaching WhatsApp itself.")

(defun whatsapp-where-text (candidate adapter &aux (target (channel-target candidate)))
  "Where the chat is and who the bot is, for the lane contract: the chat, its
kind and name, the account's own number -- whichever are known."
  (let ((group-p (equal "group" (source-field candidate "chat_kind"))))
    (format nil "WhatsApp ~:[direct chat~;group~] ~a~@[ (~a)~].~@[ The bot is the linked account +~a~]~@[ (~a)~]."
            group-p (or (getf target :channel-id) "unknown")
            (source-field candidate "chat_title")
            (own adapter "phone") (own adapter "push_name"))))

(defun press-response-plan (book candidate text &key private controls panel (timeout-seconds 30))
  "A command's PRIVATE answer, typed in a group, goes to the asker's own
chat with the bot, held open by a typing beat there (TEXT NIL), then sent
there opening on where it was asked. NIL for every other answer: WhatsApp has
no way to show one person a line in a group, and a vote on a poll is
answered in the chat it was cast in."
  (let ((user (source-field candidate "user_id")))
    (when (and private user (not (candidate-pressed-p candidate))
               (equal "group" (source-field candidate "chat_kind")))
      (let ((target (list :channel-id (chat-jid user))))
        (if (null text)
            (typing-plan target :timeout-seconds timeout-seconds)
            (send-message-plan book target
                               (one-chunk (format nil "> From ~a~%~%~a"
                                                              (or (source-field candidate "chat_title") "the group")
                                                              text))
                               :controls controls :panel panel :timeout-seconds timeout-seconds))))))

(defun control-ack-plans (book payload &key text (timeout-seconds 30))
  "What a press on a control answers: nothing to stop -- a vote shows no
spinner -- unless the kit has TEXT for the presser alone (a card's
Details), which goes to their own chat with the bot."
  (when (and text (getf payload :user-id))
    (list (send-message-plan book (list :channel-id (chat-jid (getf payload :user-id)))
                             (one-chunk (nlk:clip text (1- +whatsapp-text-limit+) :ellipsis "…"))
                             :timeout-seconds timeout-seconds))))

(defun whatsapp-platform (&key (book (make-whatsapp-book)) adapter)
  "The WhatsApp PLATFORM: driver plans over BOOK, 6000-character messages in
WhatsApp's marks, chat-scoped message ids (so an address carries the chat),
a quote as the reply, a typing beat, one reaction a message, choices as a
poll, files as media messages and a voice message as WhatsApp's own; the
linked account's number, read off ADAPTER when a call needs it, is the
mention."
  ;; WhatsApp has no threads, no command menu and no buttons for an account
  ;; like this one: those slots stay empty, and a message's choices ride
  ;; under it as a poll whose vote the lap reads as the press.
  (make-platform
   :id "whatsapp"
   :name "WhatsApp"
   :noun "chat"
   :owner-label "WhatsApp number"
   :session-prefix "whatsapp"
   :contract-section "whatsapp-chat"
   :text-limit +whatsapp-text-limit+
   :typing-refresh-ms 8000
   :plan-message (lambda (target chunk &rest keys) (apply #'send-message-plan book target chunk keys))
   :plan-edit (lambda (target id text &rest keys) (apply #'edit-message-plan book target id text keys))
   :plan-delete #'revoke-plan
   :plan-typing #'typing-plan
   :plan-reaction #'reaction-plans
   :plan-file #'file-plan
   :message-id-of #'whatsapp-message-id
   :address-of #'whatsapp-address
   :mention (lambda (user-id) (format nil "@~a" (jid-user (user-jid user-id))))
   :strip-mention (lambda (text)
                    (whatsapp-strip-mention text (remove nil (list (own adapter "phone")
                                                                   (own adapter "lid")))))
   :where-text (lambda (candidate) (whatsapp-where-text candidate adapter))
   :answer-body (lambda (text footer) (format nil "~a~%~%_~a_" text (nlk:trimmed footer)))
   :api-primer +whatsapp-api-primer+
   :seams-primer "  ncw:whatsapp-handle-event (adapter event) -- every event the WhatsApp driver reports (message, edit, revoke, reaction, poll_vote, call, ready, qr, logged_out, ...), on the driver lap: return fast. event is the hash table the driver wrote (\"event\" names it)."
   :choices t
   :plan-respond (lambda (candidate text &rest keys) (apply #'press-response-plan book candidate text keys))
   :plan-control-ack (lambda (payload &rest keys) (apply #'control-ack-plans book payload keys))))

;;; --- what the lap learns --------------------------------------------------------

(defun lap-call (adapter op &optional args (timeout 10))
  "OP through the driver now, from the lap: (values STATUS RESULT ERROR)."
  (driver-call adapter.driver op args :timeout timeout))

(defun note-identity (adapter event)
  (dolist (key '("phone" "lid" "jid" "push_name" "linked" "connected"))
    (multiple-value-bind (value present) (gethash key event)
      (when present (setf (gethash key adapter.identity) value)))))

(defparameter +group-facts-ms+ (* 5 60 1000)
  "How long what a group's info said -- its name, its admins -- is believed.")

(defun group-facts (adapter chat &aux (book adapter.book) (now (unix-ms)))
  "What the group CHAT is, as its info said at most five minutes ago: a
JSON object (name, participants), or NIL when it could not be read. A read
that failed is tried again in a minute."
  (let ((entry (gethash chat book.groups)))
    (unless (and entry (> (car entry) now))
      (multiple-value-bind (status info) (lap-call adapter "group_info" (nlk:json-object "chat" chat) 10)
        (let ((ok (and (= status 200) (hash-table-p info))))
          (when ok
            (loop for person across (nlk:json-array info "participants")
                  for id = (nlk:json-value person :string "id")
                  for name = (nlk:json-value person :text "name")
                  when (and id (plusp (length name)))
                    do (book-note book.names id name)))
          (setf entry (cons (+ now (if ok +group-facts-ms+ 60000)) (and ok info)))
          (book-note book.groups chat entry))))
    (cdr entry)))

(defun author-roles (info user)
  "USER's standing in the group INFO describes: (\"admin\") for an admin,
(\"admin\" \"superadmin\") for its creator, NIL for anyone else."
  (loop for person across (nlk:json-array info "participants")
        when (or (equal user (nlk:json-value person :string "id"))
                 (equal user (nlk:json-value person :string "phone")))
          return (cond ((gethash "superadmin" person) (list "admin" "superadmin"))
                       ((gethash "admin" person) (list "admin")))))

(defun note-chat (adapter candidate)
  "Keep what CANDIDATE shows: its speaker's name; in a group, its name and,
where the section gates on standing, the speaker's roles."
  (let* ((source (candidate-source candidate))
         (user (gethash "user_id" source))
         (name (gethash "user_name" source)))
    (when (and user name (not (equal user name)))
      (book-note (whatsapp-book-names adapter.book) user name))
    (when (equal "group" (gethash "chat_kind" source))
      (nlk:when-let (info (group-facts adapter (gethash "channel_id" source)))
        (let ((title (nlk:json-value info :text "name")))
          (when (plusp (length title)) (setf (gethash "chat_title" source) title)))
        (when adapter.roles-p
          (setf (gethash "role_ids" source) (author-roles info user)))))
    candidate))

(defparameter +late-ms+ 60000
  "How old a message may be when it arrives and still count as said just
now: anything older was said while the device was away.")

(defun catch-up (adapter candidate &aux (sent (gethash "sent_ms" (candidate-source candidate))))
  "CANDIDATE, or NIL when it is a message WhatsApp kept while the device was
away that is not to be answered: older than channels.whatsapp.catch_up_minutes
(0: every one), or one that already opened a lane before the gap. One that is
answered says how late it is (source.late_minutes)."
  (let ((age (and (integerp sent) (- (unix-ms) sent))))
    (cond ((or (null age) (< age +late-ms+)) candidate)
          ((or (zerop adapter.catch-up-minutes)
               (> age (* 60000 adapter.catch-up-minutes)))
           (format *error-output* "~&whatsapp: message ~a in ~a was sent ~d minutes ago, past catch_up_minutes; not answered~%"
                   (source-field candidate "message_id") (source-field candidate "channel_id")
                   (round age 60000))
           nil)
          ((candidate-taken-p adapter.host candidate) nil)
          (t (setf (gethash "late_minutes" (candidate-source candidate))
                   (max 1 (round age 60000)))
             candidate))))

(defun media-fetcher (adapter media)
  "A thunk answering the bytes of the file MEDIA refers to, downloaded and
decrypted by the driver, or signalling with the whole truth in its message."
  ;; Built when the message is admitted, on the lap; called when the ask's
  ;; turn is prepared, on the kit's delivery pool, so no download parks the
  ;; lap. The kit reads the size WhatsApp declared before this runs.
  (lambda ()
    (multiple-value-bind (status result error)
        (driver-call adapter.driver "download" (nlk:json-object "media" media) :timeout 120)
      (let ((path (and (= status 200) (nlk:json-value result :string "path"))))
        (unless path
          (error "the download from WhatsApp failed: ~a" (or error (format nil "status ~a" status))))
        (unwind-protect (alexandria:read-file-into-byte-vector path)
          (ignore-errors (delete-file path)))))))

(defun attach-fetchers (adapter candidate)
  "CANDIDATE's attachment entries -- on the message and on the one it
quotes -- each gain the thunk that fetches the file."
  (dolist (holder (list candidate (nlk:json-value candidate :object "reply")))
    (when holder
      (loop for attachment across (nlk:json-array holder "attachments")
            for media = (gethash "media" attachment)
            when (and (hash-table-p media) (null (gethash "fetch" attachment)))
              do (setf (gethash "fetch" attachment) (media-fetcher adapter media)))))
  candidate)

(defun self-chat-p (adapter chat)
  "Whether CHAT is the account's chat with itself -- \"Message yourself\"."
  (and chat (or (equal chat (own adapter "jid"))
                (equal chat (own adapter "lid")))))

(defun mark-read (adapter candidate)
  (when adapter.read-receipts-p
    (let ((chat (source-field candidate "channel_id"))
          (id (source-field candidate "message_id")))
      (when (and chat id)
        (execute-plan (host-executor adapter.host) (read-plan chat (list id) :timeout-seconds 5))))))

(defun handle-message (adapter event)
  "A message EVENT: the candidate it is, through the host -- unless the
account sent it itself from another of its devices, which is never an ask,
but in the chat with itself when channels.whatsapp.self_chat says so."
  (let* ((chat (nlk:json-value event :string "chat"))
         (from-me (gethash "from_me" event))
         (self-p (and from-me adapter.self-chat-p (self-chat-p adapter chat))))
    (when (or (not from-me) self-p)
      (nlk:when-let (candidate (whatsapp-message-candidate
                                event :names (lambda (id) (gethash id (whatsapp-book-names adapter.book)))))
        (note-chat adapter candidate)
        (nlk:when-let (candidate (catch-up adapter candidate))
          (attach-fetchers adapter candidate)
          (let ((action (handle-candidate adapter.host candidate)))
            (when (and (eq action :answer) (not self-p))
              (mark-read adapter candidate))
            candidate))))))

(defun handle-edit (adapter event)
  "An edited message: the ask it became when its new words address the bot
and it opened no lane before; NIL for every other edit."
  ;; As Telegram's and Discord's: an edit wakes the bot exactly when the same
  ;; words typed fresh would have been said to it, and an edit to an ask
  ;; already taken asks nothing again.
  (unless (gethash "from_me" event)
    (nlk:when-let (candidate (whatsapp-message-candidate event))
      (when (and (plusp (length (nlk:trimmed (or (gethash "text" candidate) ""))))
                 (or (source-field candidate "addressed")
                     (mention-involves-p (host-policy adapter.host) (gethash "text" candidate)))
                 (not (candidate-taken-p adapter.host candidate)))
        (note-chat adapter candidate)
        (handle-candidate adapter.host candidate)
        candidate))))

(defun handle-reaction (adapter event &aux (book adapter.book))
  "A reaction EVENT: one on the bot's messages is a line of context for that
conversation's next ask (NCK:REACTION-NOTICED)."
  (let* ((user (nlk:json-value (gethash "from" event) :string "id"))
         (key (book-key (nlk:json-value event :string "chat") (nlk:json-value event :string "target_id") user))
         (emoji (or (nlk:json-value event :string "emoji") ""))
         (candidate (whatsapp-reaction-candidate event (gethash key book.marks))))
    (if (plusp (length emoji)) (book-note book.marks key emoji) (remhash key book.marks))
    (when candidate
      (note-chat adapter candidate)
      (reaction-noticed adapter.host candidate))))

(defun handle-vote (adapter event)
  "A vote on one of the bot's polls: the press of the choice it voted for."
  (when (nlk:json-value event :string "error")
    (warn "whatsapp: a poll vote could not be read: ~a" (nlk:json-value event :string "error"))
    (return-from handle-vote nil))
  (multiple-value-bind (kind thing)
      (whatsapp-vote-press event (gethash (nlk:json-value event :string "poll_id")
                                          (whatsapp-book-polls adapter.book)))
    (case kind
      (:line (note-chat adapter thing)
       (handle-candidate adapter.host thing)
       thing)
      (:control (control-pressed adapter.host thing)
       thing))))

(defparameter +no-calls-note+
  "I can't take calls here. Send me a voice message and I'll answer it."
  "What a caller the bot would answer is told, at most every ten minutes.")

(defun handle-call (adapter event &aux (now (unix-ms)))
  "A call offered to the account: declined when channels.whatsapp.reject_calls
says so, and a caller the bot answers told, once in ten minutes, to send a
voice message instead."
  (let* ((from (gethash "from" event))
         (user (and (hash-table-p from) (normalize-phone (nlk:json-value from :string "id"))))
         (jid (and (hash-table-p from) (nlk:json-value from :string "jid"))))
    (when (and adapter.reject-calls-p jid)
      (lap-call adapter "reject_call" (nlk:json-object "from" jid "call_id" (nlk:json-value event :string "call_id")))
      (when (and user (member user adapter.allowed-users :test #'equal)
                 (> (- now (gethash user adapter.call-notes 0)) 600000))
        (setf (gethash user adapter.call-notes) now)
        (execute-plan (host-executor adapter.host)
                      (send-message-plan adapter.book (list :channel-id (chat-jid user))
                                         (one-chunk +no-calls-note+)))))))

(defparameter +link-hint+
  "link this machine as one of the phone's devices: (ncw:link :phone \"+<number>\") answers a code to type in WhatsApp, (ncw:link) a QR code to scan")

(defun whatsapp-handle-event (adapter event)
  "One EVENT the driver reported, on the lap: a message, an edit, a reaction
and a vote go to the host as the kit reads them, a call is declined, and
the device's own state -- linked, connected, refused -- is the channel's
status."
  ;; Exported as the seam a layer advises -- (hook 'ncw:whatsapp-handle-event
  ;; key fn) sees every event, receipts and history syncs too -- with the
  ;; lap's own rule: return fast. Returns what a message became, or NIL.
  (let ((kind (nlk:json-value event :string "event")))
    (flet ((status (&rest fields) (apply #'set-channel-status "whatsapp" fields)))
      (cond
        ((equal kind "message") (handle-message adapter event))
        ((equal kind "edit") (handle-edit adapter event))
        ((equal kind "reaction") (handle-reaction adapter event))
        ((equal kind "poll_vote") (handle-vote adapter event))
        ((equal kind "call") (handle-call adapter event))
        ((member kind '("started" "ready") :test #'equal)
         (note-identity adapter event)
         (cond ((not (gethash "linked" event))
                (status :state :running :connected nil :detail (format nil "not linked yet: ~a" +link-hint+)))
               ((equal kind "ready")
                (status :state :running :connected t
                        :detail (format nil "linked as +~a~@[ (~a)~]" (own adapter "phone") (own adapter "push_name"))))))
        ((equal kind "qr")
         (setf adapter.qr (nlk:json-value event :string "code") adapter.qr-at (unix-ms))
         nil)
        ((equal kind "paired")
         (note-identity adapter (let ((copy (alexandria:copy-hash-table event)))
                                  (setf (gethash "linked" copy) t)
                                  copy))
         (setf adapter.qr nil)
         (status :detail (format nil "linked as +~a; connecting" (nlk:json-value event :string "phone"))))
        ((member kind '("pair_error" "pair_state") :test #'equal)
         (when (or (equal kind "pair_error") (equal "error" (nlk:json-value event :string "state")))
           (status :detail (format nil "linking failed: ~a; ~a" (nlk:json-value event :string "error") +link-hint+)))
         (when (equal "timeout" (nlk:json-value event :string "state"))
           (setf adapter.qr nil)))
        ((equal kind "logged_out")
         (setf (gethash "linked" adapter.identity) nil)
         (warn "whatsapp: the phone unlinked this device (~a)" (nlk:json-value event :string "reason"))
         (status :state :degraded :connected nil
                 :detail (format nil "the phone unlinked this device; ~a" +link-hint+)))
        ((member kind '("disconnected" "keepalive_timeout") :test #'equal)
         (status :connected nil :detail "reconnecting to WhatsApp"))
        ((member kind '("keepalive_restored") :test #'equal)
         (status :connected t :detail nil))
        ((equal kind "stream_replaced")
         (warn "whatsapp: another program connected as this device, and WhatsApp closed this one")
         (status :state :degraded :connected nil
                 :detail "another program connected with this device's keys; stop it, then restart the channel"))
        ((equal kind "temporary_ban")
         (warn "whatsapp: ~a" (nlk:json-value event :string "reason"))
         (status :state :degraded :connected nil :detail (nlk:json-value event :string "reason")))
        ((equal kind "client_outdated")
         (warn "whatsapp: WhatsApp says this driver is outdated; update whatsmeow in driver/go.mod")
         (status :state :degraded :connected nil
                 :detail "WhatsApp refused this driver as outdated: update whatsmeow in driver/go.mod"))
        ((member kind '("connect_failure" "fatal") :test #'equal)
         (status :connected nil :detail (or (nlk:json-value event :string "message")
                                            (nlk:json-value event :string "error")
                                            (nlk:json-value event :string "reason"))))))))

;;; --- the lap --------------------------------------------------------------------

(defun open-driver-process (adapter)
  "The live driver: built when this version of driver/ never was, started
over the account's keys."
  (let ((binary (ensure-driver-binary :path adapter.driver-path)))
    (start-driver-process binary :db adapter.db :media adapter.media :log adapter.log
                                 :online adapter.online :device-name adapter.device-name)))

(defun whatsapp-lap (adapter stop-p)
  "One life of the driver: started when none runs, then each of its events
through WHATSAPP-HANDLE-EVENT until it exits or the lane stops."
  ;; Returns the supervisor verdict: :STOP, or seconds to wait before a new
  ;; driver. A driver that cannot be built or started signals, and the
  ;; supervisor backs off and tries again.
  (when (funcall stop-p) (return-from whatsapp-lap :stop))
  (unless (driver-alive-p adapter.driver)
    (setf adapter.driver (funcall adapter.open adapter)))
  (let ((driver adapter.driver))
    (loop
      (when (funcall stop-p) (return :stop))
      (let ((event (driver-next-event driver 0.5)))
        (cond (event
               ;; One boundary per event, outside the seam, so advice that
               ;; signals costs that event and not the lap.
               (handler-case (whatsapp-handle-event adapter event)
                 (error (condition)
                   (warn "whatsapp event handling signalled: ~a" condition))))
              ((not (driver-alive-p driver))
               (stop-driver driver :grace 0)
               (setf adapter.driver nil)
               (set-channel-status "whatsapp" :connected nil
                                   :detail (format nil "the WhatsApp driver exited; starting it again~@[ (its log: ~a)~]"
                                                   (and adapter.log (uiop:native-namestring adapter.log))))
               (return 2.0)))))))

(defun note-sent-message (adapter op args result)
  "What one message the bot sent leaves in the book: when it was sent (the
edit window) and, for a poll, what each of its options presses."
  (let ((id (nlk:json-value (execution-body result) :string "id"))
        (chat (and (hash-table-p args) (gethash "chat" args))))
    (when (and id (member op '("send" "send_file" "poll") :test #'equal))
      (note-sent adapter.book chat id)
      (when (and (equal op "poll") (hash-table-p args) (gethash "__choices" args))
        (book-note (whatsapp-book-polls adapter.book) id
                   (map 'list #'cons (gethash "options" args) (gethash "__choices" args)))))))

;;; --- the door -------------------------------------------------------------------

(defun request (op &rest arguments &key body timeout retry
                &aux (adapter *whatsapp-adapter*))
  "One driver OP as the linked account: (:status N :body VALUE [:error TEXT])
-- the shape the eval snippet prints legibly, see NCK:CALL."
  ;; OP is \"send\", \"history\", ... (+WHATSAPP-API-PRIMER+). BODY is a keyword
  ;; plist or a JSON object. Anything the account may do; nothing here narrows
  ;; it. Nothing WhatsApp does can be read back, so no :STATE rides along.
  ;; Blocks on the caller's thread.
  (declare (ignore body timeout retry))
  (unless (and adapter adapter.host (host-executor adapter.host))
    (error "the whatsapp channel is not running"))
  (apply #'call (host-executor adapter.host) "POST" (format nil "/~a" (string-left-trim "/" op)) arguments))

(defun link (&key phone (wait 15) &aux (adapter *whatsapp-adapter*))
  "Link this machine to a WhatsApp account as one of its devices. With
PHONE, the account's number: the code to type on that phone (WhatsApp >
Linked devices > Link a device > Link with phone number instead). Without:
a QR code to scan from the same screen, as a PNG file and its text."
  (unless (and adapter adapter.driver)
    (error "the whatsapp channel is not running: add a channels.whatsapp section and restart the channels"))
  (when (gethash "linked" adapter.identity)
    (return-from link
      (list :linked (own adapter "phone") :name (own adapter "push_name")
            :note "already linked; (ncw:unlink) first to link another account")))
  (if phone
      (multiple-value-bind (status result error)
          (driver-call adapter.driver "pair_phone" (nlk:json-object "phone" phone) :timeout 30)
        (if (= status 200)
            (list :code (nlk:json-value result :string "code")
                  :note (format nil "On the phone with WhatsApp for ~a: Settings > Linked devices > Link a device > Link with phone number instead, and type the code. It lasts about two minutes." phone))
            (error "WhatsApp would not give a linking code: ~a" error)))
      (progn
        ;; A code older than 20 s has rotated; one older than the pairing
        ;; window means the driver gave up, and a connect starts it over.
        (when (or (null adapter.qr) (> (- (unix-ms) adapter.qr-at) 20000))
          (setf adapter.qr nil)
          (driver-call adapter.driver "connect" nil :timeout 15)
          (loop repeat (* 10 wait) until adapter.qr do (sleep 0.1)))
        (unless adapter.qr
          (error "WhatsApp showed no QR code to link with; try (ncw:link :phone \"+<number>\")"))
        (let ((png (ignore-errors (qr-png adapter.qr (nlk:home "channels/whatsapp/link-qr.png")))))
          (list :qr adapter.qr :png png
                :note "Scan it in WhatsApp: Settings > Linked devices > Link a device. It changes every 20 seconds; call (ncw:link) again for a fresh one.")))))

(defun unlink (&aux (adapter *whatsapp-adapter*))
  "Unlink this machine from the WhatsApp account: its keys are forgotten on
both sides, and linking again starts over."
  (unless (and adapter adapter.driver)
    (error "the whatsapp channel is not running"))
  (multiple-value-bind (status result error) (driver-call adapter.driver "logout" nil :timeout 30)
    (declare (ignore result))
    (if (= status 200)
        (progn (setf (gethash "linked" adapter.identity) nil) :unlinked)
        (error "unlinking failed: ~a" error))))

;;; --- the channel entry ----------------------------------------------------------

(defun section-with-defaults (section)
  "SECTION as the host reads it, with WhatsApp's own defaults where it is
silent: a card edited at most every four seconds -- every edit is a message
to each of the chat's devices -- and no answer streamed in edits. A chat is
always flat: WhatsApp has no threads, so a quote is the only route an ask
takes to a lane, and the host must never set it aside for a thread of the
ask's own."
  (let ((copy (alexandria:copy-hash-table section)))
    (setf (gethash "thread_behavior" copy) "reply_in_place")
    (unless (nth-value 1 (gethash "status_update_ms" copy))
      (setf (gethash "status_update_ms" copy) 4000))
    (unless (nth-value 1 (gethash "stream" copy))
      (setf (gethash "stream" copy) nil))
    copy))

(defun chats (list) (remove nil (mapcar #'chat-jid list)))
(defun people (list) (remove nil (mapcar #'normalize-phone list)))

(defun take-session-lock (db)
  "The lock that keeps one process per linked device: two would each be
closed by WhatsApp in turn, forever."
  (let* ((path (make-pathname :type "lock" :defaults db))
         (lock (progn (ensure-directories-exist path) (nlk:lock-file path))))
    (unless lock
      (config-error "channels.whatsapp: the linked device ~a is in use by another organism -- one per device; stop that one, or give this one a session of its own"
                    (uiop:native-namestring db)))
    lock))

(defvar *driver-opener* 'open-driver-process
  "The function of the adapter that answers the DRIVER a lap runs: the live
process (OPEN-DRIVER-PROCESS), or, bound by a test, a fake over the same
wire.")

(defun start-channel (section &key executor (open *driver-opener*))
  "Start the WhatsApp lane from its channels.whatsapp config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides the driver executor (the
  ;; recording test seam); OPEN, a function of the adapter answering a
  ;; DRIVER, replaces the process (the fake driver's seam, *DRIVER-OPENER*).
  ;;
  ;; Fail-closed: refuses unless at least one of allowed_chats / allowed_users
  ;; is populated. A number may be written any way (+1 555 123-4567); a chat
  ;; is a group JID or a number for that person's direct chat.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "whatsapp")) section))
         (configured-users (people (getf settings :allowed-users)))
         (owners (resolve-owners "whatsapp" (people (getf settings :owner)) configured-users "allowed_chats"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (allowed-chats (chats (getf settings :allowed-chats)))
         (session (or (config-string section "session") "default"))
         (db (nlk:home (format nil "channels/whatsapp/~a.db" session)))
         (book (make-whatsapp-book)))
    (require-non-empty-allowlist "whatsapp"
                                 "allowed_chats" allowed-chats
                                 "allowed_users" allowed-users
                                 "allowed_roles" (getf settings :allowed-roles))
    (set-other-addressees "whatsapp" (getf settings :other-addressees))
    (let* ((adapter (%make-whatsapp-adapter
                     :book book
                     :open (coerce open 'function)
                     :self-chat-p (config-boolean section "self_chat" nil)
                     :read-receipts-p (config-boolean section "read_receipts" t)
                     :reject-calls-p (config-boolean section "reject_calls" t)
                     :roles-p (and (getf settings :allowed-roles) t)
                     :catch-up-minutes (config-integer section "catch_up_minutes" 60 :min 0)
                     :allowed-users allowed-users
                     :driver-path (config-string section "driver_path")
                     :db db
                     :media (nlk:home "channels/whatsapp/media/")
                     :log (nlk:home "channels/whatsapp/driver.log")
                     :online (config-boolean section "online" t)
                     :device-name (or (config-string section "device_name") "Nodecode")))
           (executor (or executor
                         (make-driver-executor (lambda () (whatsapp-adapter-driver adapter))
                                               :on-sent (lambda (op args result)
                                                          (note-sent-message adapter op args result)))))
           (host (make-host-from-section
                  (section-with-defaults section)
                  (whatsapp-platform :book book :adapter adapter)
                  :executor executor
                  :owners owners
                  :soul-path (soul-path section)
                  :request-timeout 30
                  ;; The declared members name the policy's own keys; the
                  ;; rest are read here.
                  :policy (apply #'whatsapp-inbound-policy
                                 :allowed-users allowed-users
                                 :allowed-chats allowed-chats
                                 :free-response-chats (chats (getf settings :free-response-chats))
                                 :require-mention-chats (chats (getf settings :require-mention-chats))
                                 :ignored-chats (chats (getf settings :ignored-chats))
                                 ;; The account's number and LID, as the
                                 ;; driver last said them.
                                 :bot-ids (lambda ()
                                            (remove nil (list (or (config-string section "phone")
                                                                  (own adapter "phone"))
                                                              (own adapter "lid"))))
                                 :allow-private-chats (config-boolean section "allow_private_chats" t)
                                 :allow-group-chats (config-boolean section "allow_group_chats" t)
                                 settings))))
      (setf adapter.host host)
      ;; One process per linked device: the live driver holds the keys' lock.
      (when (eq open 'open-driver-process)
        (setf adapter.lock (take-session-lock db)))
      (run-host host "driver" (lambda (stop-p) (whatsapp-lap adapter stop-p))
                :on-start (lambda () (setf *whatsapp-adapter* adapter))
                :on-stop (lambda ()
                           (when adapter.driver
                             (ignore-errors (stop-driver adapter.driver))
                             (setf adapter.driver nil))
                           (when adapter.lock
                             (ignore-errors (nlk:unlock-file adapter.lock))
                             (setf adapter.lock nil))
                           ;; A stale stop thunk racing a respawned lane must
                           ;; not clear the fresh adapter's binding.
                           (when (eq *whatsapp-adapter* adapter)
                             (setf *whatsapp-adapter* nil)))))))
