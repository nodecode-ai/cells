;;;; adapter.lisp --- the iMessage adapter: the platform, the poll lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the status line, the
;;;; answer, the write-back -- is the kit's (host.lisp, room.lisp). This file
;;;; is what iMessage contributes: how a message, a tapback and a typing beat
;;;; are spelled (rest.lisp, bound into a PLATFORM), where the chat is, and the
;;;; poll lap that turns new messages into candidates.
;;;;
;;;; Thread topology (kit rule: no network I/O on a thread that is not ours):
;;;;   - channel-imessage-poll (supervised lap): every message the Mac wrote
;;;;     since the last read, oldest first, through IMESSAGE-HANDLE-MESSAGE; a
;;;;     person's contact name asked once each; the row the next read goes on
;;;;     from kept on disk after each batch.
;;;;   - the host's :FRAME fold and channel-imessage-deliver pool: the kit's.
;;;;
;;;; The first start reads where the Messages database stands and answers
;;;; nothing said before it; a later start goes on from the kept row, so a
;;;; message sent while the organism was down is answered when it comes back.

(in-package #:nodecode-channel-imessage)

(nlk:access (host channel-host) (result execution) (book imessage-book))

(defstruct (imessage-adapter (:copier nil) (:constructor %make-imessage-adapter))
  (host nil)
  (url "" :type string)
  (password "" :type string)
  (book (make-imessage-book))
  ;; handle -> contact name, NIL for one the Mac's contacts do not name; the lap's.
  (names (make-hash-table :test #'equal))
  ;; The last message row read, and the file it is kept in.
  (cursor nil :type (or null integer))
  (cursor-path nil)
  (poll-seconds 1.5 :type real))

(nlk:access (adapter imessage-adapter))

(defmethod print-object ((adapter imessage-adapter) stream)
  ;; never the slots: they hold the server password
  (print-unreadable-object (adapter stream :type t :identity t)
    (princ (imessage-adapter-url adapter) stream)))

(defvar *imessage-adapter* nil
  "The live iMessage adapter, or NIL when the lane is stopped.")

(defvar *cursor-state-directory* nil
  "Where the message row is kept, one file per server; NIL is the home's
channels/imessage/. Tests bind a scratch folder.")

;;; --- what iMessage contributes to the host ------------------------------------------

;;; Data, so an operator's layer can reword it.
(defparameter +imessage-api-primer+
  "iMessage is open to you as the bot's Apple ID, through its BlueBubbles server's REST API, from eval:
  (nci:request METHOD PATH &key body timeout) => (:status N :body VALUE)
PATH is under the server: (nci:request \"GET\" \"/api/v1/chat/<chat>/message?limit=20&with=handle\") reads a chat's latest messages; (nci:request \"POST\" \"/api/v1/message/text\" :body (:|chatGuid| \"<chat>\" :|tempGuid| \"temp-1\" :|message| \"hi\" :|method| \"apple-script\")) sends one -- an outward effect: send only what was asked. The server password is added for you and never shown. Percent-encode a guid in a path. A chat guid reads iMessage;-;<handle> for a direct message and iMessage;+;chat<id> for a group; a message is named by its guid -- the m... handle on each line -- and a person by their handle, a phone number or an email address. Messages draws no Markdown: answers are sent as plain text. Choices you offer are drawn as numbered lines, and a reply of the number picks one. The adapter sends your final answer: do not send it again yourself."
  "What the lane contract says about reaching iMessage itself.")

(defun imessage-where-text (candidate &key private-api)
  "Where the chat is, for the lane contract: a direct message with whom, or
which group, and how answers land there."
  (let* ((target (channel-target candidate))
         (direct-p (equal "direct_message" (source-field candidate "chat_kind")))
         (place (if direct-p
                    (format nil "direct message with ~a"
                            (or (source-field candidate "user_name") (source-field candidate "user_id") "someone"))
                    (format nil "group chat ~a" (or (getf target :channel-id) "unknown")))))
    (format nil "iMessage ~a. The bot is an Apple ID served by a BlueBubbles server. ~
~:[Answers are sent into the chat~;Answers are inline replies under the ask~], as plain text."
            place (and private-api (not direct-p)))))

(defun imessage-platform (&key (book (make-imessage-book)) mention-names)
  "The iMessage PLATFORM over BOOK: BlueBubbles REST plans, 4,000-character
plain-text messages, no card (the typing bubble and the ask's tapback stand
for it), inline replies in groups, numbered choices, tapbacks and the typing
bubble where the server has its Private API, files and voice messages.
MENTION-NAMES are the names that call the bot in a group."
  ;; Messages keeps no command menu, opens no thread a bot asks for and has
  ;; no return path but the chat: those slots stay empty.
  (make-platform
   :id "imessage"
   :name "iMessage"
   :noun "chat"
   :owner-label "iMessage handle"
   :session-prefix "imessage"
   :contract-section "imessage-channel"
   :text-limit +imessage-text-limit+
   :typing-refresh-ms 30000
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan book target chunk keys))
   :plan-edit (lambda (target id text &rest keys) (apply #'edit-plan book target id text keys))
   :plan-delete (lambda (target id &rest keys) (apply #'delete-plan book target id keys))
   :plan-typing (lambda (target &rest keys) (apply #'typing-plan book target keys))
   :plan-reaction (and (imessage-book-private-api book)
                       (lambda (target id emoji &rest keys) (apply #'reaction-plans book target id emoji keys)))
   :plan-file (lambda (target path &rest keys) (apply #'file-plan book target path keys))
   :message-id-of #'imessage-message-id
   :strip-mention (lambda (text) (imessage-strip-mention text mention-names))
   :where-text (lambda (candidate)
                 (imessage-where-text candidate :private-api (imessage-book-private-api book)))
   :choices t
   :api-primer +imessage-api-primer+
   :seams-primer "  nci:imessage-handle-message (adapter message) -- every message the poll reads (asks, tapbacks, the bot's own sends, group events), on the poll thread: return fast. message is the hash table the server sent (\"guid\", \"text\", \"handle\", \"chats\")."))

;;; --- what the lap learns -------------------------------------------------------------

(defun lap-call (adapter plan)
  (execute-plan (host-executor adapter.host) plan))

(defun contact-name (adapter handle)
  "HANDLE's name in the Mac's contacts, asked of the server once per person,
or NIL when it has none."
  (when handle
    (multiple-value-bind (name known) (gethash handle adapter.names)
      (if known
          name
          (setf (gethash handle adapter.names)
                (gethash handle (contact-names (host-executor adapter.host) (list handle))))))))

(defun attachment-fetcher (adapter guid)
  "A thunk that returns the bytes of attachment GUID, downloaded from the
server -- a photo as JPEG, a voice note as MP3 -- or signals with the whole
truth in its message, never the password."
  ;; Built when the message is admitted, on the lap thread; called when the
  ;; ask's turn is prepared, on the kit's delivery pool, so no download ever
  ;; parks the poll.
  (let ((url (format nil "~a~a/attachment/~a/download?password=~a" adapter.url +api+
                     (encode guid) (encode adapter.password))))
    (lambda ()
      (multiple-value-bind (body status)
          (nlk:with-handlers ((error () (error "the download from the iMessage server failed")))
            (nlk:http :get url :binary t :timeout 60))
        (unless (eql 200 status)
          (error "the download from the iMessage server failed with status ~a" status))
        body))))

(defun attach-fetchers (adapter candidate)
  "CANDIDATE's attachment entries each gain the thunk that fetches the file."
  (loop for attachment across (nlk:json-array candidate "attachments")
        for guid = (nlk:json-value attachment :string "download")
        when guid
          do (setf (gethash "fetch" attachment) (attachment-fetcher adapter guid)))
  candidate)

(defun take-press (adapter message name)
  "MESSAGE, when it picks one of the choices its chat has open: a choice's
line is said as the presser's words (the kit's press candidate), any other
control pressed. => the candidate or payload, or NIL for words that pick
nothing."
  (let* ((chat (message-chat-guid message))
         (open (and chat (gethash chat (imessage-book-choices adapter.book))))
         (picked (and open (zerop (length (nlk:json-array message "attachments")))
                      (choice-picked (imessage-text message) (getf open :items)))))
    (when picked
      (destructuring-bind (label data) picked
        (declare (ignore label))
        (let ((line (said-line data))
              (guid (nlk:json-value message :string "guid"))
              (sender (message-sender message)))
          (if line
              (let ((candidate (nlk:json-object
                                "text" line
                                "source" (nlk:json-object
                                          "platform" "imessage"
                                          "chat_kind" (if (direct-chat-p chat) "direct_message" "group")
                                          "channel_id" chat
                                          "user_id" sender
                                          "user_name" name
                                          "message_id" guid
                                          "pressed" t
                                          "press_data" data
                                          "reply_to_message_id" (getf open :message)
                                          "addressed" t))))
                (handle-candidate adapter.host candidate)
                candidate)
              (let ((payload (list :id guid :data data :user-id sender :user-name name
                                   :message-id (getf open :message) :channel-id chat)))
                (control-pressed adapter.host payload)
                payload)))))))

(defun imessage-handle-message (adapter message)
  "One MESSAGE the poll read, on the poll lap: the bot's own noted as the
bot's, a tapback relayed as the kit's reaction, a reply picking a choice
pressed, anything said mapped to the shared candidate and handed to the
host. => the candidate or payload handed on, or NIL."
  ;; Exported as the seam a layer advises -- (hook 'nci:imessage-handle-message
  ;; key fn) sees every message the poll reads -- with the lap's own rule:
  ;; return fast.
  (let* ((book adapter.book)
         (guid (nlk:json-value message :string "guid"))
         (root (thread-root message)))
    (when (and guid root (string/= guid root))
      (book-note book.roots guid root))
    (cond
      ((null guid) nil)
      ((nlk:json-value message :boolean "isFromMe")
       (book-note book.mine guid t)
       nil)
      ((imessage-tapback message)
       (nlk:when-let (candidate (imessage-reaction-candidate
                                 message :user-name (contact-name adapter (message-sender message))))
         (reaction-noticed adapter.host candidate)
         candidate))
      (t
       (let ((name (contact-name adapter (message-sender message))))
         (or (take-press adapter message name)
             (nlk:when-let (candidate (imessage-message-candidate
                                       message :user-name name
                                       :bot-message-p (lambda (id) (bot-message-p book id))))
               (handle-candidate adapter.host (attach-fetchers adapter candidate))
               candidate)))))))

;;; --- the poll lap ----------------------------------------------------------------------

(defun cursor-path (url)
  "The file the message row read from the server at URL is kept in."
  (let ((name (format nil "~a.cursor" (substitute-if #\_ (lambda (ch) (not (or (alphanumericp ch) (find ch ".-"))))
                                                      (ppcre:regex-replace "^[a-z]+://" url "")))))
    (if *cursor-state-directory*
        (merge-pathnames name *cursor-state-directory*)
        (nlk:home (format nil "channels/imessage/~a" name)))))

(defun keep-cursor (adapter)
  (when (and adapter.cursor-path adapter.cursor)
    (handler-case (nlk:write-file-atomically adapter.cursor-path (princ-to-string adapter.cursor))
      (error (condition) (warn "imessage: the message row was not kept: ~a" condition)))))

(defun message-row (message)
  (nlk:json-value message :integer "originalROWID"))

(defparameter +batch+ 100
  "Messages one read takes; a full batch is read on from at once.")

(defun imessage-poll-lap (adapter stop-p)
  "One read of the messages written since the last."
  ;; Returns the supervisor verdict: seconds to wait before the next lap, or
  ;; :FATAL for a password the server refuses, since no wait mends that.
  (when (funcall stop-p) (return-from imessage-poll-lap :stop))
  (let* ((first-p (null adapter.cursor))
         (result (lap-call adapter (if first-p (newest-plan) (query-plan adapter.cursor :limit +batch+)))))
    (when (funcall stop-p) (return-from imessage-poll-lap :stop))
    (cond
      ((eql 401 result.status)
       (set-channel-status "imessage" :state :degraded :connected nil
                                      :detail "the BlueBubbles server refused the password")
       :fatal)
      ((not result.ok-p)
       (set-channel-status "imessage" :connected nil :detail result.error)
       (warn "imessage read failed: ~a" result.error)
       3.0)
      (t
       (set-channel-status "imessage" :state :running :connected t)
       (let ((messages (nlk:json-array result.body "data")))
         (if first-p
             (setf adapter.cursor (or (and (plusp (length messages)) (message-row (aref messages 0))) 0))
             (loop for message across messages
                   for row = (message-row message)
                   ;; One boundary per message, outside the seam, so advice
                   ;; that signals costs that message and not the read.
                   do (handler-case (imessage-handle-message adapter message)
                        (error (condition)
                          (warn "imessage message handling signalled: ~a" condition)))
                      (when (and row (> row adapter.cursor))
                        (setf adapter.cursor row))))
         (keep-cursor adapter)
         (if (>= (length messages) +batch+) 0.05 adapter.poll-seconds))))))

;;; --- the door ----------------------------------------------------------------------

(defun request (method path &rest arguments &key body headers timeout retry
                &aux (host (and *imessage-adapter* (imessage-adapter-host *imessage-adapter*))))
  "One REST call to the BlueBubbles server as the bot's Apple ID: METHOD
PATH, answered as (:status N :body VALUE [:error TEXT]) -- the shape the eval
cell prints legibly, see NCK:CALL."
  ;; PATH is under the server (/api/v1/...). BODY is a keyword plist (a JSON
  ;; object) or an NLK:JSON-OBJECT. Anything the server may do; nothing here
  ;; narrows it. The password rides the executor and never the answer.
  (declare (ignore body headers timeout retry))
  (unless (and host host.executor)
    (error "the imessage channel is not running"))
  (apply #'call host.executor method path arguments))

(defun section-with-defaults (section)
  "SECTION as the host reads it, with iMessage's own terms: no answer
streamed in edits, which Messages caps at five, and the card -- which is
never posted -- ticked seldom."
  (let ((copy (alexandria:copy-hash-table section)))
    (setf (gethash "stream" copy) nil)
    (unless (nth-value 1 (gethash "status_update_ms" copy))
      (setf (gethash "status_update_ms" copy) 60000))
    copy))

(defun start-channel (section &key executor)
  "Start the iMessage lane from its channels.imessage config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides the HTTP under the live
  ;; executor -- the scripted test seam; it is wrapped in BlueBubbles' terms
  ;; like the live one.
  ;;
  ;; Fail-closed: refuses unless at least one of allowed_chats / allowed_users
  ;; is populated. allowed_users says who may talk; owner says whose
  ;; instructions are standing policy (the kit's room, AUTHORITY). An owner
  ;; may always talk: the handles join allowed_users when that list is the gate.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "imessage")) section))
         (url (section-url section))
         (password (resolve-channel-secret section "password"))
         (configured-users (mapcar #'normalize-handle (getf settings :allowed-users)))
         (owners (resolve-owners "imessage" (mapcar #'normalize-handle (getf settings :owner))
                                 configured-users "allowed_chats"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (allowed-chats (getf settings :allowed-chats))
         (mention-names (remove "" (mapcar (lambda (name) (string-left-trim "@" (nlk:trimmed name)))
                                           (getf settings :mention-names))
                                :test #'string=))
         (book (make-imessage-book))
         (bot (section-executor section executor :book book)))
    (when (zerop (length url))
      (config-error "channels.imessage: url is not set -- the BlueBubbles server's address, e.g. http://192.168.1.20:1234"))
    (require-non-empty-allowlist "imessage"
                                 "allowed_chats" allowed-chats
                                 "allowed_users" allowed-users)
    ;; What the server can do decides how every answer is sent. A server that
    ;; will not say is loud: no chat would be served.
    (multiple-value-bind (info result) (server-info bot)
      (unless info
        (config-error "channels.imessage: the BlueBubbles server would not answer (~a)" (execution-error result)))
      (setf (imessage-book-private-api book) (private-api-p info)))
    (let* ((host (make-host-from-section
                  (section-with-defaults section)
                  (imessage-platform :book book :mention-names mention-names)
                  :executor bot
                  :owners owners
                  :soul-path (soul-path section)
                  :request-timeout 60
                  :policy (apply #'imessage-inbound-policy
                                 :allowed-users allowed-users
                                 :mention-names mention-names
                                 settings)))
           (path (cursor-path url))
           (adapter (%make-imessage-adapter
                     :host host
                     :url url
                     :password password
                     :book book
                     :cursor (let ((kept (and (probe-file path)
                                              (nlk:trimmed (uiop:read-file-string path)))))
                               (and (plusp (length kept)) (every #'digit-char-p kept) (parse-integer kept)))
                     :cursor-path path
                     :poll-seconds (/ (or (getf settings :poll-ms) 1500) 1000.0))))
      (run-host host "poll" (lambda (stop-p) (imessage-poll-lap adapter stop-p))
                :on-start (lambda () (setf *imessage-adapter* adapter))
                :on-stop (lambda ()
                           ;; A stale stop thunk racing a respawned lane must
                           ;; not clear the fresh adapter's binding.
                           (when (eq *imessage-adapter* adapter)
                             (setf *imessage-adapter* nil)))))))
