;;;; adapter.lisp --- the Matrix adapter: the platform, the /sync lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the status line, the
;;;; answer, the write-back -- is the kit's (host.lisp, room.lisp). This file
;;;; is what Matrix contributes: how a message, an edit, a redaction and a
;;;; typing beat are spelled (rest.lisp, bound into a PLATFORM), how a mention
;;;; is written, where the room is, and the /sync lap that turns room events
;;;; into candidates.
;;;;
;;;; Thread topology (kit rule: no network I/O on a thread that is not ours):
;;;;   - channel-matrix-sync (supervised lap): the long poll, the invites it
;;;;     joins, every timeline event through MATRIX-HANDLE-EVENT, a person's
;;;;     name and a room's kind asked once each, the reactions that draw a
;;;;     message's controls, and the sync token kept on disk after each batch.
;;;;   - the host's :FRAME fold and channel-matrix-deliver pool: the kit's.
;;;;
;;;; The first start reads where the rooms stand and answers nothing said
;;;; before it; a later start goes on from the kept token, so a message sent
;;;; while the organism was down is answered when it comes back.

(in-package #:nodecode-channel-matrix)

(nlk:access (host channel-host) (result execution) (book matrix-book))

(defstruct (matrix-adapter (:copier nil) (:constructor %make-matrix-adapter))
  (host nil)
  (homeserver "" :type string)
  (token "" :type string)
  (bot-user-id nil :type (or null string))
  (book (make-matrix-book))
  ;; user id -> display name, NIL for one the homeserver would not name; the lap's.
  (names (make-hash-table :test #'equal))
  ;; The /sync token the next poll goes on from, and the file it is kept in.
  (since nil :type (or null string))
  (since-path nil)
  (timeout-ms 5000 :type integer)
  ;; Who may invite the bot in, and to what: the gate's own lists.
  (allowed-users '())
  (allowed-rooms '()))

(nlk:access (adapter matrix-adapter))

(defvar *matrix-adapter* nil
  "The live Matrix adapter, or NIL when the lane is stopped.")

(defvar *sync-state-directory* nil
  "Where the sync token is kept, one file per account; NIL is the home's
channels/matrix/. Tests bind a scratch folder.")

;;; --- what Matrix contributes to the host ----------------------------------------------

;;; Data, so an operator's layer can reword it; the ids above it are the
;;; adapter's.
(defparameter +matrix-api-primer+
  "Matrix's Client-Server API is open to you as the bot's account, from eval:
  (ncm:request METHOD PATH &key body headers timeout) => (:status N :body VALUE)
PATH is under the homeserver: a read is a GET, (ncm:request \"GET\" \"/_matrix/client/v3/rooms/<room>/messages?dir=b&limit=20\"); a send is a PUT naming a fresh transaction id, (ncm:request \"PUT\" \"/_matrix/client/v3/rooms/<room>/send/m.reaction/<txn>\" :body (:|m.relates_to| (:rel_type \"m.annotation\" :event_id \"<event>\" :key \"👍\"))). Room ids start with !, event ids with $, user ids with @; percent-encode them in a path. Objects come back as keyword plists, arrays as vectors; a refusal carries errcode and error. A message is named by its event id -- the m... handle on each line -- and a person is mentioned by their user id. Encrypted rooms cannot be read or written from here. The adapter posts your final answer under the ask: do not post it again yourself."
  "What the lane contract says about reaching Matrix itself.")

(defun matrix-where-text (candidate bot-user-id &aux (target (channel-target candidate)))
  "Where the room is and who the bot is, for the lane contract: the room,
its kind, the bot's own id -- whichever are known."
  (let ((direct-p (equal "direct_message" (source-field candidate "chat_kind"))))
    (format nil "Matrix ~:[room~;direct message~] ~a.~@[ The bot is ~a.~]~
~:[ Answers go in the thread under the ask.~;~]"
            direct-p (or (getf target :channel-id) "unknown") bot-user-id direct-p)))

(defun matrix-platform (&key bot-user-id (book (make-matrix-book)))
  "The Matrix PLATFORM: Client-Server API plans, 10,000-character Markdown
messages drawn as HTML, the user id as the mention, room-scoped event ids (so
an address carries the room), replies threaded under their ask, a typing
beat, reactions as events, and files uploaded to the media repository. BOOK
is the matrix book the plans and the lap share."
  ;; Matrix draws no buttons and keeps no command menu a bot may write, and a
  ;; thread is opened by its first reply: those slots stay empty, and the
  ;; kit's controls are drawn as reactions (the lap's NOTE-ECHO).
  (make-platform
   :id "matrix"
   :name "Matrix"
   :noun "room"
   :owner-label "Matrix user id"
   :session-prefix "matrix"
   :contract-section "matrix-channel"
   :text-limit +matrix-text-limit+
   :typing-refresh-ms 25000
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan book target chunk keys))
   :plan-edit (lambda (target id text &rest keys) (apply #'edit-plan book target id text keys))
   :plan-delete #'delete-plan
   :plan-typing (lambda (target &rest keys) (apply #'typing-plan bot-user-id target keys))
   :plan-reaction (lambda (target id emoji &rest keys) (apply #'reaction-plans book target id emoji keys))
   :plan-file (lambda (target path &rest keys) (apply #'file-plan book target path keys))
   :message-id-of #'matrix-event-id
   :address-of #'matrix-address
   :mention #'identity
   :strip-mention (lambda (text) (matrix-strip-mention text bot-user-id))
   :where-text (lambda (candidate) (matrix-where-text candidate bot-user-id))
   :api-primer +matrix-api-primer+
   :seams-primer "  ncm:matrix-handle-event (adapter room-id event) -- every timeline event /sync delivers (messages, reactions, encrypted events, membership), on the sync thread: return fast. event is the hash table the homeserver sent (\"type\" names it)."))

;;; --- what the lap learns -------------------------------------------------------------

(defun lap-call (adapter plan)
  (execute-plan (host-executor adapter.host) plan))

(defun matrix-user-name (adapter user-id)
  "USER-ID's display name: the one a membership event gave, else asked of
the homeserver once per person, or NIL when it would not say."
  (when user-id
    (multiple-value-bind (name known) (gethash user-id adapter.names)
      (if known
          name
          (let ((result (lap-call adapter (read-plan (format nil "/profile/~a/displayname" (encode user-id))
                                                     "displayname" :timeout-seconds 10))))
            (setf (gethash user-id adapter.names)
                  (and result.ok-p (nlk:json-value result.body :text "displayname"))))))))

(defun room-kind (adapter room)
  "ROOM's kind, :DIRECT or :GROUP: what /sync's summary said, else its
joined members counted once -- two make a direct message."
  (or (gethash room (matrix-book-kinds adapter.book))
      (let* ((result (lap-call adapter (rest-plan "GET" (room-path room "joined_members")
                                                  "joined_members" nil 10)))
             (members (and result.ok-p (nlk:json-value result.body :object "joined"))))
        (if members
            (book-note (matrix-book-kinds adapter.book) room (if (<= (hash-table-count members) 2) :direct :group))
            :group))))

(defun matrix-file-fetcher (adapter mxc)
  "A thunk that returns the bytes of MXC, fetched from the homeserver's
authenticated media (spec v1.11) with the token, or signals with the whole
truth in its message -- never the token."
  ;; Built when the message is admitted, on the lap thread; called when the
  ;; ask's turn is prepared, on the kit's delivery pool, so no download ever
  ;; parks the poll. The ceiling a file may take is the kit's, read off the
  ;; size the entry declares before this runs.
  (let ((url (format nil "~a/_matrix/client/v1/media/download/~a" adapter.homeserver
                     (subseq mxc (length "mxc://"))))
        (token adapter.token))
    (lambda ()
      (multiple-value-bind (body status)
          (nlk:with-handlers ((error () (error "the download from Matrix failed")))
            (nlk:http :get url :headers (list (cons "authorization" (format nil "Bearer ~a" token)))
                               :binary t :timeout 30))
        (unless (eql 200 status)
          (error "the download from Matrix failed with status ~a" status))
        body))))

(defun attach-matrix-file-fetchers (adapter candidate)
  "CANDIDATE's attachment entries each gain the thunk that fetches the file."
  (loop for attachment across (nlk:json-array candidate "attachments")
        for url = (nlk:json-value attachment :string "download")
        when url
          do (setf (gethash "fetch" attachment) (matrix-file-fetcher adapter url)))
  candidate)

(defun draw-controls (adapter room event controls)
  "Put one reaction per control on the bot's EVENT, so a press is one click."
  (setf (gethash (book-key room event) (matrix-book-controls adapter.book)) controls)
  (loop for control in controls
        for index from 0
        do (lap-call adapter (first (reaction-plans adapter.book (list :channel-id room) event
                                                    (control-glyph control index))))))

(defun take-controls (adapter room event)
  "Take EVENT's controls off: their reactions redacted, the press gone."
  (let ((controls (gethash (book-key room event) (matrix-book-controls adapter.book))))
    (remhash (book-key room event) (matrix-book-controls adapter.book))
    (loop for control in controls
          for index from 0
          do (dolist (plan (reaction-plans adapter.book (list :channel-id room) event nil
                                           :previous (control-glyph control index)))
               (lap-call adapter plan)))))

(defun note-echo (adapter room event)
  "One of the bot's own events, come back through /sync: noted as the
bot's, and what its send meant beyond it carried out -- a reaction's id kept
so it can be taken off, a message's controls drawn, an edit's controls
redrawn or taken off."
  (let* ((book adapter.book)
         (id (nlk:json-value event :string "event_id"))
         (type (nlk:json-value event :string "type"))
         (txn (nlk:json-value event :string "unsigned" "transaction_id"))
         (intent (and txn (gethash txn book.pending))))
    (when txn (remhash txn book.pending))
    (book-note book.mine (book-key room id) t)
    (cond
      ((equal type "m.reaction")
       (let ((relation (nlk:json-value event :object "content" "m.relates_to")))
         (when relation
           (book-note book.marks (book-key room (nlk:json-value relation :string "event_id")
                                           (nlk:json-value relation :string "key"))
                      id))))
      ((getf intent :edit)
       (let ((target (getf intent :edit))
             (controls (getf intent :controls)))
         (cond ((and controls (not (gethash (book-key room target) book.controls)))
                (draw-controls adapter room target controls))
               (controls (setf (gethash (book-key room target) book.controls) controls))
               ((gethash (book-key room target) book.controls)
                (take-controls adapter room target)))))
      ((getf intent :controls)
       (draw-controls adapter room id (getf intent :controls))))))

(defparameter +encrypted-notice+
  "This room is end-to-end encrypted, and I can't read encrypted rooms yet. Ask me in a room with encryption turned off."
  "What the bot says, once, in an encrypted room an allowed person speaks in.")

(defun allowed-sender-p (adapter user room)
  "Whether USER may bring the bot into ROOM: an allowed user, or anyone in
an allowed room when rooms are the gate."
  (if adapter.allowed-users
      (and (member user adapter.allowed-users :test #'equal)
           (or (null adapter.allowed-rooms) (member room adapter.allowed-rooms :test #'equal))
           t)
      (and (member room adapter.allowed-rooms :test #'equal) t)))

(defun tell-encrypted (adapter room)
  "Say once in ROOM why the bot does not answer there."
  (unless (gethash room (matrix-book-told adapter.book))
    (book-note (matrix-book-told adapter.book) room t)
    (lap-call adapter (post-plan adapter.book (list :channel-id room) +encrypted-notice+))
    (set-channel-status "matrix" :detail (format nil "room ~a is encrypted: not served" room))))

(defun matrix-handle-event (adapter room-id event)
  "One timeline EVENT of ROOM-ID, as /sync delivered it, on the sync lap: a
message maps to the shared candidate and goes through the host, a reaction
on a message carrying controls presses one; everything else is learned
from and falls through."
  ;; Exported as the seam a layer advises -- (hook 'ncm:matrix-handle-event
  ;; key fn) sees reactions, membership, whatever the filter delivers -- with
  ;; the lap's own rule: return fast. Returns the candidate admitted, the
  ;; press made, or NIL.
  (let ((book adapter.book)
        (type (nlk:json-value event :string "type"))
        (sender (nlk:json-value event :string "sender")))
    (note-room-event book room-id event)
    (cond
      ((equal type "m.room.member")
       (nlk:when-let (user (nlk:json-value event :string "state_key"))
         (nlk:when-let (name (nlk:json-value event :text "content" "displayname"))
           (setf (gethash user adapter.names) name)))
       nil)
      ((equal type "m.room.encryption")
       (book-note book.encrypted room-id t)
       nil)
      ((and sender (equal sender adapter.bot-user-id))
       (note-echo adapter room-id event)
       nil)
      ((equal type "m.room.encrypted")
       (book-note book.encrypted room-id t)
       (when (allowed-sender-p adapter sender room-id)
         (tell-encrypted adapter room-id))
       nil)
      ((equal type "m.reaction")
       (nlk:when-let (press (matrix-control-press
                             event room-id
                             (lambda (room target) (gethash (book-key room target) book.controls))))
         (control-pressed adapter.host press)
         press))
      (t
       (nlk:when-let (candidate (matrix-message-candidate
                                 event room-id
                                 :bot-user-id adapter.bot-user-id
                                 :direct-p (eq :direct (room-kind adapter room-id))
                                 :bot-event-p (lambda (room target) (bot-event-p book room target))))
         (setf (gethash "user_name" (gethash "source" candidate))
               (matrix-user-name adapter sender))
         (handle-candidate adapter.host (attach-matrix-file-fetchers adapter candidate))
         candidate)))))

;;; --- the sync lap ----------------------------------------------------------------------

(defun learn-room (adapter room-id room)
  "What one joined ROOM section says of the room's shape: its member count,
its members' names, whether it is encrypted."
  (let ((count (nlk:json-value room :integer "summary" "m.joined_member_count")))
    (when count
      (book-note (matrix-book-kinds adapter.book) room-id (if (<= count 2) :direct :group))))
  (loop for event across (nlk:json-array room "state" "events")
        do (matrix-handle-event adapter room-id event)))

(defun take-invite (adapter room-id invite)
  "Join ROOM-ID when an allowed person invited the bot there; any other
invite is left as it is."
  (multiple-value-bind (sender direct-p) (invite-sender invite adapter.bot-user-id)
    (when (and sender (allowed-sender-p adapter sender room-id))
      (let ((result (lap-call adapter (join-plan room-id))))
        (if result.ok-p
            (when direct-p (book-note (matrix-book-kinds adapter.book) room-id :direct))
            (warn "matrix: joining ~a failed: ~a" room-id result.error))))))

(defun take-sync (adapter body &key first-p)
  "One /sync answer: every joined room's shape learned and -- unless this is
the first, which only says where the rooms stand -- its timeline handled,
and every invite an allowed person sent joined."
  (flet ((each (table fn)
           (when (hash-table-p table)
             (maphash fn table))))
    (each (nlk:json-value body :object "rooms" "join")
          (lambda (room-id room)
            (learn-room adapter room-id room)
            (loop for event across (nlk:json-array room "timeline" "events")
                  for type = (nlk:json-value event :string "type")
                  when (or (not first-p)
                           (member type '("m.room.member" "m.room.encryption") :test #'equal))
                    ;; One boundary per event, outside the seam, so advice
                    ;; that signals costs that event and not the poll.
                    do (handler-case (matrix-handle-event adapter room-id event)
                         (error (condition)
                           (warn "matrix event handling signalled: ~a" condition))))))
    (each (nlk:json-value body :object "rooms" "invite")
          (lambda (room-id invite)
            (handler-case (take-invite adapter room-id invite)
              (error (condition) (warn "matrix invite handling signalled: ~a" condition)))))))

(defun since-path (bot-user-id)
  "The file the sync token of BOT-USER-ID is kept in."
  (let ((name (format nil "~a.since" (substitute-if #\_ (lambda (ch) (not (or (alphanumericp ch) (find ch "._-"))))
                                                     bot-user-id))))
    (if *sync-state-directory*
        (merge-pathnames name *sync-state-directory*)
        (nlk:home (format nil "channels/matrix/~a" name)))))

(defun keep-since (adapter)
  (when adapter.since-path
    (handler-case (nlk:write-file-atomically adapter.since-path adapter.since)
      (error (condition) (warn "matrix: the sync token was not kept: ~a" condition)))))

(defun matrix-sync-lap (adapter stop-p)
  "One /sync long poll."
  ;; Returns the supervisor verdict: seconds to wait before the next lap, or
  ;; :FATAL for a token the homeserver refuses, since no wait mends that.
  (when (funcall stop-p) (return-from matrix-sync-lap :stop))
  (let ((result (lap-call adapter (sync-plan adapter.since :timeout-ms adapter.timeout-ms))))
    (when (funcall stop-p) (return-from matrix-sync-lap :stop))
    (cond
      ((eql 401 result.status)
       (set-channel-status "matrix" :state :degraded :connected nil
                                    :detail "the homeserver refused the access token")
       :fatal)
      ((not result.ok-p)
       (set-channel-status "matrix" :connected nil :detail result.error)
       (warn "matrix sync failed: ~a" result.error)
       2.0)
      (t
       (set-channel-status "matrix" :state :running :connected t)
       (let ((first-p (null adapter.since))
             (next (nlk:json-value result.body :string "next_batch")))
         (take-sync adapter result.body :first-p first-p)
         (when next
           (setf adapter.since next)
           (keep-since adapter)))
       0.05))))

;;; --- the door ----------------------------------------------------------------------

(defun request (method path &rest arguments &key body headers timeout retry
                &aux (host (and *matrix-adapter* (matrix-adapter-host *matrix-adapter*))))
  "One Client-Server API call as the bot's account: METHOD PATH, answered as
(:status N :body VALUE [:error TEXT]) -- the shape the eval cell prints
legibly, see NCK:CALL."
  ;; PATH is under the homeserver (/_matrix/client/v3/...). BODY is a keyword
  ;; plist (a JSON object) or an NLK:JSON-OBJECT. Anything the account may do;
  ;; nothing here narrows it. The token stays on the executor and never
  ;; appears in the answer. Blocks on the caller's thread.
  (declare (ignore body headers timeout retry))
  (unless (and host host.executor)
    (error "the matrix channel is not running"))
  (apply #'call host.executor method path arguments))

(defun section-with-defaults (section)
  "SECTION as the host reads it, with Matrix's own defaults where it is
silent: a card edited at most every six seconds -- a homeserver's default
rate allows a burst, then one event every five -- and no answer streamed
in edits, each of which would be an event in the room's history."
  (let ((copy (alexandria:copy-hash-table section)))
    (unless (nth-value 1 (gethash "status_update_ms" copy))
      (setf (gethash "status_update_ms" copy) 6000))
    (unless (nth-value 1 (gethash "stream" copy))
      (setf (gethash "stream" copy) nil))
    copy))

(defun start-channel (section &key executor)
  "Start the Matrix lane from its channels.matrix config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides the live executor -- the
  ;; scripted test seam; it is wrapped in Matrix's terms like the live one.
  ;;
  ;; Fail-closed: refuses unless at least one of allowed_rooms / allowed_users
  ;; is populated. allowed_users says who may talk; owner says whose
  ;; instructions are standing policy (the kit's room, AUTHORITY). An owner
  ;; may always talk: the ids join allowed_users when that list is the gate.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "matrix")) section))
         (homeserver (section-homeserver section))
         (token (resolve-channel-secret section "token"))
         (configured-users (getf settings :allowed-users))
         (owners (resolve-owners "matrix" (getf settings :owner) configured-users "allowed_rooms"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (allowed-rooms (getf settings :allowed-rooms))
         (bot (if executor
                  (wrap-matrix-executor executor)
                  (make-matrix-executor :homeserver homeserver :token token)))
         (book (make-matrix-book))
         (bot-user-id nil))
    (when (zerop (length homeserver))
      (config-error "channels.matrix: homeserver is not set -- the server's address, e.g. https://matrix.org"))
    (require-non-empty-allowlist "matrix"
                                 "allowed_rooms" allowed-rooms
                                 "allowed_users" allowed-users)
    ;; The account's own id is the mention, the self filter, the typing beat
    ;; and the file the sync token is kept in. A refused whoami is loud: no
    ;; room would be served.
    (let ((me (execute-plan bot (read-plan "/account/whoami" "whoami"))))
      (if (execution-ok-p me)
          (setf bot-user-id (nlk:json-value (execution-body me) :string "user_id"))
          (config-error "channels.matrix: the homeserver would not say who the token is (~a)"
                        (execution-error me))))
    (let* ((host (make-host-from-section
                  (section-with-defaults section)
                  (matrix-platform :bot-user-id bot-user-id :book book)
                  :executor bot
                  :owners owners
                  :soul-path (soul-path section)
                  :request-timeout 30
                  :policy (apply #'matrix-inbound-policy
                                 :allowed-users allowed-users
                                 :bot-user-id bot-user-id
                                 settings)))
           (path (since-path bot-user-id))
           (adapter (%make-matrix-adapter
                     :host host
                     :homeserver homeserver
                     :token token
                     :bot-user-id bot-user-id
                     :book book
                     :since (let ((kept (and (probe-file path)
                                             (nlk:trimmed (uiop:read-file-string path)))))
                              (and (plusp (length kept)) kept))
                     :since-path path
                     :allowed-users allowed-users
                     :allowed-rooms allowed-rooms)))
      (run-host host "sync" (lambda (stop-p) (matrix-sync-lap adapter stop-p))
                :on-start (lambda () (setf *matrix-adapter* adapter))
                :on-stop (lambda ()
                           ;; A stale stop thunk racing a respawned lane must
                           ;; not clear the fresh adapter's binding.
                           (when (eq *matrix-adapter* adapter)
                             (setf *matrix-adapter* nil)))))))
