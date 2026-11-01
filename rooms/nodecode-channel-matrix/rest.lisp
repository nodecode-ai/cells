;;;; rest.lisp --- Client-Server API plans, the room book, the executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is under the homeserver's /_matrix/client/v3 (media under
;;;; /_matrix/media/v3 and /_matrix/client/v1/media) with the account's access
;;;; token as a bearer. A send is a PUT naming a transaction id the client
;;;; picks, so a retried send is one event. A refusal is a 4xx carrying
;;;; {errcode, error}; a rate limit is a 429 whose body says retry_after_ms,
;;;; which the executor hands the kit's retry loop.
;;;;
;;;; A message goes as m.room.message: `body' the Markdown as written,
;;;; `formatted_body' the same as HTML (MATRIX-HTML). The answer is m.text,
;;;; the one message that should notify; a card, a status line, a notice is
;;;; m.notice, which the default push rules keep silent. An edit is a new
;;;; event whose m.new_content replaces the message it relates to (m.replace).

(in-package #:nodecode-channel-matrix)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution))

(defparameter +client-path+ "/_matrix/client/v3")

;;; An event may not exceed 64 KiB of JSON, and a message carries its words
;;; twice (body and formatted_body): a chunk this long stays under it.
(defparameter +matrix-text-limit+ 10000)

(defparameter +command-event-type+ "org.matrix.msc4391.command_description"
  "The state event one command of a bot's menu is (commands.lisp).")

(defparameter +menu-event-types+
  (list +command-event-type+ "m.room.power_levels" "m.room.create")
  "The state a room's command menu is planned from: the commands it holds,
and who may set them.")

(defparameter +sync-filter+
  (nlk:encode-json-object
   (nlk:json-object
    "presence" (nlk:json-object "not_types" (vector "*"))
    "account_data" (nlk:json-object "not_types" (vector "*"))
    "room" (nlk:json-object
            "account_data" (nlk:json-object "not_types" (vector "*"))
            "ephemeral" (nlk:json-object "not_types" (vector "*"))
            "state" (nlk:json-object "types" (coerce (list* "m.room.encryption" "m.room.member" "m.room.name"
                                                             +menu-event-types+)
                                                      'vector)
                                     "lazy_load_members" t)
            "timeline" (nlk:json-object "limit" 50
                                        "types" (coerce (list* "m.room.message" "m.room.encrypted"
                                                               "m.room.encryption" "m.room.member"
                                                               "m.reaction" +menu-event-types+)
                                                        'vector)))))
  "What the lap asks /sync for: the room events an ask, a press or a room's
shape is made of, and nothing else.")

(defun encode (segment)
  "SEGMENT, a room, event or user id, as one path segment."
  (quri:url-encode segment))

(defun room-path (room &rest segments)
  (format nil "~a/rooms/~a~{/~a~}" +client-path+ (encode room) (mapcar #'encode segments)))

(sb-ext:defglobal **txn-counter** (list 0)
  "How many transaction ids this process has handed out.")

(defun new-txn ()
  "A transaction id no other send of this process uses."
  (format nil "nc~36r~36r" (now-ms) (sb-ext:atomic-incf (car **txn-counter**))))

;;; --- the matrix book ---------------------------------------------------------------
;;; What the lap learns from /sync and the plans need: which thread an event
;;; lives in, which events and threads are the bot's, which rooms are direct
;;; messages or encrypted, which of the bot's messages carry controls and the
;;; reactions that draw them. Learned from the events (the lap thread) and the
;;; plans (the delivery pool), so the tables are synchronized; live-only, and
;;; bounded.

(defparameter +matrix-book-cap+ 20000
  "Entries any one table holds before it starts over.")

(defstruct (matrix-book (:copier nil))
  ;; "<room>|<event>" -> the root of the thread the event lives in.
  (roots (make-hash-table :test #'equal :synchronized t))
  ;; "<room>|<root>" -> T once the bot has posted in that thread.
  (spoken (make-hash-table :test #'equal :synchronized t))
  ;; "<room>|<event>" -> T for an event the bot sent.
  (mine (make-hash-table :test #'equal :synchronized t))
  ;; room -> :direct or :group, once known.
  (kinds (make-hash-table :test #'equal :synchronized t))
  ;; room -> T once it is end-to-end encrypted; room -> T once told so.
  (encrypted (make-hash-table :test #'equal :synchronized t))
  (told (make-hash-table :test #'equal :synchronized t))
  ;; txn -> what a send meant beyond its event: (:controls LIST), or
  ;; (:edit TARGET :controls LIST), read when /sync echoes the event.
  (pending (make-hash-table :test #'equal :synchronized t))
  ;; "<room>|<event>" -> the controls the message carries.
  (controls (make-hash-table :test #'equal :synchronized t))
  ;; "<room>|<event>|<key>" -> the bot's reaction event drawing KEY on it.
  (marks (make-hash-table :test #'equal :synchronized t))
  ;; room -> (state key -> what the room holds there: the JSON-SHAPE of the
  ;; bot's command, or :FOREIGN), once the room's state has been read.
  (menus (make-hash-table :test #'equal :synchronized t))
  ;; room -> the JSON-SHAPE of its m.room.power_levels; room -> the creators
  ;; who outrank every level (version 12 rooms).
  (levels (make-hash-table :test #'equal :synchronized t))
  (creators (make-hash-table :test #'equal :synchronized t))
  ;; room -> the bot that spoke there last: this one, or another whose
  ;; m.notice the room carried. A bare command is that bot's (BARE-COMMAND-P).
  (speakers (make-hash-table :test #'equal :synchronized t))
  ;; Bumped by the lap whenever the menu tables change; the generation the
  ;; last menu plan read; whether every joined room's state has been read.
  (menu-generation 0 :type fixnum)
  (menu-planned -1 :type fixnum)
  (menus-listed-p nil))

(nlk:access (book matrix-book))

(defun book-key (&rest parts)
  (format nil "~{~a~^|~}" parts))

(defun book-note (table key value)
  "Set KEY to VALUE in TABLE, starting it over once it holds the cap."
  (when (>= (hash-table-count table) +matrix-book-cap+)
    (clrhash table))
  (setf (gethash key table) value))

(defun note-room-event (book room event)
  "Remember which thread EVENT lives in, so a reply to it lands there."
  (let ((root (nlk:json-value event :string "content" "m.relates_to" "event_id"))
        (id (nlk:json-value event :string "event_id")))
    (when (and id root (equal "m.thread" (nlk:json-value event :string "content" "m.relates_to" "rel_type")))
      (book-note book.roots (book-key room id) root))))

(defun bot-thread-p (book room root)
  "Whether the bot has posted in the thread ROOT starts in ROOM."
  (and (gethash (book-key room root) book.spoken) t))

(defun bot-event-p (book room event)
  "Whether EVENT in ROOM is the bot's own, or the root of a thread it speaks in."
  (or (gethash (book-key room event) book.mine)
      (bot-thread-p book room event)))

(defun direct-room-p (book room)
  (eq :direct (gethash room book.kinds)))

(defun reply-thread (book target reply-to &aux (room (getf target :channel-id)))
  "The thread a reply to REPLY-TO goes in: the thread REPLY-TO already lives
in, else -- outside a direct message -- the thread REPLY-TO starts; NIL posts
in the room itself."
  ;; A direct message reads as a chat, so an ask typed there is answered
  ;; there, quoting it; in a room the answer hangs off the ask, in its thread.
  (and reply-to
       (or (gethash (book-key room reply-to) book.roots)
           (and (not (direct-room-p book room)) reply-to))))

;;; --- content ---------------------------------------------------------------------

(defun pill (user-id)
  (format nil "<a href=\"https://matrix.to/#/~a\">~a</a>" (html-escape user-id) (html-escape user-id)))

(defun controls-hint (controls)
  "The line under a message carrying CONTROLS that says how to press them."
  (format nil "~{~a~^ · ~}"
          (loop for control in controls
                for index from 0
                collect (format nil "~a ~a" (control-glyph control index)
                                (string-downcase (first control))))))

(defun relation (thread reply-to)
  "The m.relates_to of a post: under THREAD's root, falling back to a reply
to REPLY-TO for a client that draws no threads; or a plain reply."
  (cond (thread (nlk:json-object "rel_type" "m.thread" "event_id" thread
                                 "is_falling_back" t
                                 "m.in_reply_to" (nlk:json-object "event_id" (or reply-to thread))))
        (reply-to (nlk:json-object "m.in_reply_to" (nlk:json-object "event_id" reply-to)))))

(defun message-content (text &key notice-p mentions controls)
  "An m.room.message's own members: TEXT as body and as HTML, MENTIONS
drawn as pills and named under m.mentions, CONTROLS' hint under the words."
  (let* ((words (if controls (format nil "~a~%~%~a" text (controls-hint controls)) text))
         (html (matrix-html words)))
    (dolist (id mentions)
      (setf html (uiop:frob-substrings html (list (html-escape id))
                                       (lambda (match emit) (declare (ignore match))
                                         (funcall emit (pill id))))))
    (nlk:json-object "msgtype" (if notice-p "m.notice" "m.text")
                     "body" words
                     "format" "org.matrix.custom.html"
                     "formatted_body" html
                     ;; Present and empty, it says the post mentions nobody:
                     ;; a name in the words pings no one by accident.
                     "m.mentions" (nlk:json-object "user_ids" (coerce mentions 'vector)))))

;;; --- plans -----------------------------------------------------------------------

(defun send-plan (room type content label &key retry (timeout-seconds 30) (txn (new-txn)))
  "The PUT of one event of TYPE carrying CONTENT into ROOM."
  (rest-plan "PUT" (room-path room "send" type txn) label retry timeout-seconds content))

(defun send-txn (plan)
  "The transaction id a send PLAN names: its path's last segment."
  (let ((path plan.path)) (subseq path (1+ (position #\/ path :from-end t)))))

(defun message-plan (book target chunk &rest keys &key &allow-other-keys)
  "One m.room.message of CHUNK (a kit text-chunk): POST-PLAN of its words,
its place among the chunks of one post saying which quotes the ask."
  (apply #'post-plan book target chunk.text :index chunk.index :total chunk.total keys))

(defun post-plan (book target text &key (index 1) (total 1) reply-to ping controls files mentions
                                     (timeout-seconds 30) &allow-other-keys)
  "One m.room.message of TEXT, chunk INDEX of TOTAL, in the thread REPLY-TO
lives in or starts (REPLY-THREAD), the first chunk quoting REPLY-TO; FILES
ride ahead of the first chunk, sent by the executor."
  ;; PING says the post may notify: the answer is m.text, everything else
  ;; m.notice. CONTROLS ride the chunk the kit hands them with; the hint says
  ;; how to press them and the echo of the send puts the reactions on
  ;; (the adapter's NOTE-ECHO). Retried: a post that fails is an answer lost.
  (let* ((room (getf target :channel-id))
         (thread (reply-thread book target reply-to))
         (first-p (= 1 index))
         (controls (unless (eq controls :clear) controls))
         (content (message-content text :notice-p (not ping) :mentions mentions
                                        :controls controls)))
    (when thread
      (book-note book.spoken (book-key room thread) t))
    (nlk:when-let (relates (relation thread (and first-p reply-to)))
      (setf (gethash "m.relates_to" content) relates))
    (when (and files first-p)
      (setf (gethash "__files" content) (coerce (mapcar #'namestring files) 'vector)))
    (let ((plan (send-plan room "m.room.message" content
                           (if (> total 1)
                               (format nil "send_message_chunk_~a_of_~a" index total)
                               "send_message")
                           :retry t :timeout-seconds timeout-seconds)))
      (when controls
        (book-note book.pending (send-txn plan) (list :controls controls)))
      plan)))

(defun edit-plan (book target message-id text &key retry controls (timeout-seconds 30)
                  &allow-other-keys)
  "An m.replace of MESSAGE-ID: the status line settled in place, its controls
with it."
  ;; RETRY opts into 5xx/429 retries: a running update leaves it off (the next
  ;; tick supersedes a missed edit), the terminal settle turns it on. CONTROLS
  ;; NIL leaves the message with none: the echo takes their reactions off.
  (let* ((room (getf target :channel-id))
         (controls (unless (eq controls :clear) controls))
         (new (message-content text :notice-p t :controls controls))
         (content (nlk:json-object "msgtype" "m.notice"
                                   "body" (format nil "* ~a" (gethash "body" new))
                                   "m.new_content" new
                                   "m.mentions" (nlk:json-object)
                                   "m.relates_to" (nlk:json-object "rel_type" "m.replace"
                                                                   "event_id" message-id)))
         (plan (send-plan room "m.room.message" content "edit_message"
                          :retry (and retry t) :timeout-seconds timeout-seconds)))
    (book-note book.pending (send-txn plan) (list :edit message-id :controls controls))
    plan))

(defun delete-plan (target message-id &key (timeout-seconds 30) &allow-other-keys)
  "A redaction of MESSAGE-ID -- retiring a status line once the answer it
stood in for has landed. Retried: a status line outliving its answer is a
duplicate."
  (rest-plan "PUT" (room-path (getf target :channel-id) "redact" message-id (new-txn))
             "delete_message" t timeout-seconds (nlk:json-object)))

(defun typing-plan (bot-user-id target &key (timeout-seconds 30) &allow-other-keys)
  "The bot typing in the room for 30 s, which the kit re-asserts on the
platform's cadence. Not retried: a missed beat corrects itself."
  (when bot-user-id
    (rest-plan "PUT" (room-path (getf target :channel-id) "typing" bot-user-id)
               "typing_indicator" nil timeout-seconds
               (nlk:json-object "typing" t "timeout" 30000))))

(defun reaction-plans (book target message-id emoji &key previous (timeout-seconds 30)
                       &allow-other-keys)
  "The bot's one reaction on MESSAGE-ID: PREVIOUS redacted first -- Matrix
stacks reactions -- then EMOJI added, or nothing added for NIL."
  ;; A reaction is an event: taking one off redacts the event the echo noted
  ;; for it, and one whose echo has not come yet stays. Retried: the mark
  ;; holds for the whole turn, so its clear must land even through a 429.
  (let ((room (getf target :channel-id)))
    (unless (equal previous emoji)
      (append
       (nlk:when-let (mark (and previous (gethash (book-key room message-id previous) book.marks)))
         (remhash (book-key room message-id previous) book.marks)
         (list (rest-plan "PUT" (room-path room "redact" mark (new-txn))
                          "remove_reaction" t timeout-seconds (nlk:json-object))))
       (and emoji
            (list (send-plan room "m.reaction"
                             (nlk:json-object "m.relates_to"
                                              (nlk:json-object "rel_type" "m.annotation"
                                                               "event_id" message-id
                                                               "key" emoji))
                             "add_reaction" :retry t :timeout-seconds timeout-seconds)))))))

(defun file-plan (book target pathname &key content reply-to (timeout-seconds 60)
                  &allow-other-keys)
  "PATHNAME posted on TARGET as its own message, CONTENT the words with it."
  (post-plan book target (or content "") :reply-to reply-to :files (list pathname)
                                         :timeout-seconds timeout-seconds))

(defun sync-plan (since &key (timeout-ms 5000) (filter +sync-filter+))
  "The /sync long poll from SINCE (NIL: the first, which waits for nothing)."
  ;; The window is short for the reason Telegram's is: the poll thread's
  ;; blocking read is the lane's stop latency, and an event ends the window
  ;; at once. The HTTP timeout carries a margin over the window so the
  ;; server's own wins the race; the lap is its own retry.
  (rest-plan "GET" (format nil "~a/sync?timeout=~d&filter=~a~@[&since=~a~]"
                           +client-path+ (if since timeout-ms 0) (encode filter)
                           (and since (encode since)))
             "sync" nil (+ 10 (ceiling (if since timeout-ms 0) 1000))))

(defun join-plan (room &key (timeout-seconds 30))
  (rest-plan "POST" (format nil "~a/join/~a" +client-path+ (encode room))
             "join_room" t timeout-seconds (nlk:json-object)))

(defun read-plan (path label &key (timeout-seconds 15))
  (rest-plan "GET" (format nil "~a~a" +client-path+ path) label nil timeout-seconds))

(defun matrix-event-id (body)
  "The id of the event a send created."
  (nlk:json-value body :string "event_id"))

(defun matrix-address (target message-id)
  "The address-book key for one Matrix event: the room rides in the key."
  (book-key (getf target :channel-id) message-id))

;;; --- the executor ----------------------------------------------------------------

(defparameter +settled-errors+ '("M_DUPLICATE_ANNOTATION")
  "Matrix refusals that mean the state asked for is the state there: a
reaction already on.")

(defun matrix-failure-message (plan status detail)
  "One failed call in words: the homeserver's errcode and error when it says."
  (format nil "matrix ~a failed with status ~a: ~a" plan.audit-label status detail))

(defun json-body (body)
  (if (stringp body) (ignore-errors (nlk:decode-json body)) body))

(defun matrix-retry-after-ms (status body headers)
  "How long a 429 asks the client to wait, from its body's retry_after_ms."
  (declare (ignore headers))
  (let ((body (json-body body)))
    (and (eql status 429) (hash-table-p body)
         (nlk:json-value body :integer "retry_after_ms"))))

(defparameter +image-types+
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp"))
  "A file's type by its extension, for the pictures a client draws inline.")

(defun file-media-type (path)
  (or (cdr (assoc (string-downcase (or (pathname-type path) "")) +image-types+ :test #'string=))
      "application/octet-stream"))

(defun file-content (path uri relates)
  "The message that shows the file at PATH, uploaded as URI."
  (let* ((type (file-media-type path))
         (name (file-namestring path))
         (content (nlk:json-object "msgtype" (if (uiop:string-prefix-p "image/" type) "m.image" "m.file")
                                   "body" name "filename" name "url" uri
                                   "info" (nlk:json-object "mimetype" type
                                                           "size" (or (ignore-errors
                                                                       (with-open-file (in path :element-type '(unsigned-byte 8))
                                                                         (file-length in)))
                                                                      0))
                                   "m.mentions" (nlk:json-object))))
    (when relates (setf (gethash "m.relates_to" content) relates))
    content))

(defun run-with-files (inner plan uploader)
  "PLAN, a send whose content names files under __files: each file uploaded
through UPLOADER and sent as its own message under the same relation, then
the words -- or, when there are none, the last file's send as the answer."
  (let* ((content (alexandria:copy-hash-table plan.body))
         (paths (coerce (gethash "__files" content) 'list))
         (room (let ((path plan.path)) (quri:url-decode (subseq path (+ 7 (search "/rooms/" path))
                                                               (search "/send/" path)))))
         (relates (gethash "m.relates_to" content))
         (last nil))
    (remhash "__files" content)
    (dolist (path paths)
      (multiple-value-bind (uri error) (funcall uploader path)
        (unless uri
          (return-from run-with-files
            (make-execution :ok-p nil :status 0 :error (format nil "matrix upload of ~a failed: ~a"
                                                               (file-namestring path) error))))
        (setf last (execute-plan inner (send-plan room "m.room.message" (file-content path uri relates)
                                                  "send_file" :retry t
                                                  :timeout-seconds plan.timeout-seconds)))
        (unless (execution-ok-p last) (return-from run-with-files last))))
    (if (and last (zerop (length (nlk:trimmed (or (gethash "body" content) "")))))
        last
        (execute-plan inner (send-plan room "m.room.message" content plan.audit-label
                                       :retry t :timeout-seconds plan.timeout-seconds
                                       :txn (send-txn plan))))))

(defun wrap-matrix-executor (inner &key uploader)
  "An executor speaking Matrix in the kit's terms: a refusal that means
`already so' succeeds, a plan that is NIL -- a beat Matrix cannot show -- is
done, and a send naming files uploads them through UPLOADER first."
  (make-plan-executor
   :run (lambda (plan)
          (cond
            ((null plan) (make-execution :ok-p t :status 200 :attempts 0))
            ((and (hash-table-p plan.body) (gethash "__files" plan.body))
             (if uploader
                 (run-with-files inner plan uploader)
                 (make-execution :ok-p nil :status 0 :error "matrix: no uploader for files")))
            (t
             (let ((result (execute-plan inner plan)))
               (when (and (not result.ok-p) result.error
                          (some (lambda (code) (search code result.error)) +settled-errors+))
                 (setf result.ok-p t result.body nil))
               result))))))

(defun media-uploader (homeserver token)
  "A function of a file's path answering (values MXC-URI ERROR): the file
posted to the homeserver's media repository with TOKEN."
  (lambda (path)
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http :post (format nil "~a/_matrix/media/v3/upload?filename=~a"
                                    homeserver (encode (file-namestring path)))
                      :headers (list (cons "authorization" (format nil "Bearer ~a" token))
                                     (cons "content-type" (file-media-type path)))
                      :content (alexandria:read-file-into-byte-vector path)
                      :timeout 60)
          (let ((uri (and (eql 200 status)
                          (nlk:json-value (json-body (if (stringp body) body
                                                         (ignore-errors (sb-ext:octets-to-string body :external-format :utf-8))))
                                          :string "content_uri"))))
            (if uri (values uri nil) (values nil (format nil "status ~a" status)))))
      (error (condition)
        (values nil (nlk:one-line (princ-to-string condition) :cap 200))))))

(defun make-matrix-executor (&key homeserver token)
  "The live executor: the Client-Server API under HOMESERVER with TOKEN as
the bearer, which never reaches a failure text."
  (wrap-matrix-executor
   (make-dexador-executor
    :base-url homeserver
    :headers (list (cons "authorization" (format nil "Bearer ~a" token)))
    :redact-prefixes (list token)
    :failure-message-fn #'matrix-failure-message
    :retry-after-fn #'matrix-retry-after-ms)
   :uploader (media-uploader homeserver token)))
