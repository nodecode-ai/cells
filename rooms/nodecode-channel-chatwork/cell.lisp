;;;; cell.lisp --- Chatwork: the notation, the poll, the platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Chatwork is Japanese business chat: rooms, no threads, no typing
;;;; indicator, and a message body that is its own markup ([To:...], [rp
;;;; aid=.. to=room-message], [code], [qt], [info]). The API is REST v2 under
;;;; api.chatwork.com, one token in an x-chatworktoken header, 300 calls per
;;;; five minutes per token and 10 posts per ten seconds per room. Nothing is
;;;; pushed, so this lane polls: the room list every lap, and a room's newest
;;;; messages only when that room's own metadata moved (or every twelfth lap),
;;;; which is what keeps a five-second lap inside the budget.
;;;;
;;;; An answer is Chatwork's own reply -- an [rp aid=.. to=..] line at its
;;;; head, which notifies the asker as the Reply button does. In group rooms
;;;; the bot answers a [To:itself] or a reply to one of its own messages; in
;;;; My Chat and direct rooms it answers what is typed there.
;;;;
;;;; A rewrite of TakeshiTGAL's Hermes Agent plugin; see NOTICE.

(in-package #:nodecode-channel-chatwork)

;;; The section is declared at the end of this file, after PROBE-CHANNEL: its
;;; :check names that function, which the declaration finds at load time.

;;; --- constants ------------------------------------------------------------------

(defparameter +api-base+ "https://api.chatwork.com/v2"
  "Where every Chatwork API v2 call goes.")

(defparameter +token-page+
  "https://www.chatwork.com/service/packages/chatwork/subpackages/api/token.php"
  "Where an operator issues the token, logged in as the bot's account.")

(defparameter +text-limit+ 10000
  "The most characters one message carries: the API allows 65535, and this
keeps an answer readable and copyable.")

(defparameter +full-refresh-every+ 12
  "Every this-many laps a served room is read even when its metadata did not
change: clock drift and edits are what that window is for.")

(defparameter +max-catchup-seconds+ (* 24 3600)
  "Asks older than this were said while the bot was away and are skipped, so
nobody gets an answer to last week's question.")

(defparameter +sent-keep+ 1000
  "How many of the bot's own postings are remembered.")

(defparameter +inbound-keep+ 500
  "How many inbound messages are remembered for the reply header.")

(defparameter +typing-refresh-ms+ 86400000
  "Chatwork has no typing indicator; the lane says so by asking once a day.")

;;; --- the notation, inbound ------------------------------------------------------

(defparameter +address-pattern+
  "\\[To:(\\d+)\\]|\\[(?:rp|返信) aid=(\\d+) to=(\\d+)-(\\d+)\\]"
  "A Chatwork address: a [To:id] mention, or the reply tag in either
spelling. Groups: (1) the account a [To:] names; (2) the author a reply
names, (3) its room and (4) its message.")

(defparameter +honorific-pattern+ "[ \t　]*(?:さん|様|さま|殿|san)?"
  "What a To or Reply button puts after the addressee's name.")

(defun chatwork-name-end (text start names)
  "Where the words after a tag ending at START begin: past the account's own
display name with its honorific and the line break a To button inserts, or
past a bare line break, or START itself when neither is there."
  (dolist (name (sort (remove "" (copy-list names) :test #'equal) #'> :key #'length))
    (multiple-value-bind (from to)
        (ppcre:scan (format nil "^[ \t　]*~a~a[ \t　]*(?:\\n|$)"
                             (ppcre:quote-meta-chars name) +honorific-pattern+)
                    text :start start)
      (when (and from (= from start)) (return-from chatwork-name-end to))))
  (multiple-value-bind (from to) (ppcre:scan "^[ \t　]*\\n" text :start start)
    (if (and from (= from start)) to start)))

(defun chatwork-quoted-line (quoted)
  "QUOTED as the blockquote a model reads: every line opened with \"> \"."
  (with-output-to-string (out)
    (loop for line in (ppcre:split "\\n" (string-trim '(#\Newline #\Return) quoted))
          for first = t then nil
          do (unless first (terpri out))
             (format out "> ~a" line))))

(defun chatwork-drop-tags (text)
  "TEXT with the tags that say nothing to a reader: [info], [toall], [hr],
previews, icons, names and [deleted]."
  (ppcre:regex-replace-all "\\[(?:toall|hr|preview[^\\]]*|picon:\\d+|piconname:\\d+|pname:\\d+|deleted)\\]"
                            text ""))

(defun chatwork-readable-text (text)
  "TEXT with Chatwork's block notation as plain reading: [code] as a fence,
[qt] as a quote, [download:N] as a note, the rest dropped."
  (let ((text (ppcre:regex-replace-all
               "(?s)\\[code\\](.*?)\\[/code\\]" text
               (lambda (match code)
                 (declare (ignore match))
                 (format nil "~%```~%~a~%```~%"
                         (string-trim '(#\Newline #\Return) code)))
               :simple-calls t)))
    (setf text (ppcre:regex-replace-all
                "(?s)\\[qt\\](?:\\[qtmeta[^\\]]*\\])?(.*?)\\[/qt\\]" text
                (lambda (match quoted)
                  (declare (ignore match))
                  (chatwork-quoted-line quoted))
                :simple-calls t))
    (setf text (ppcre:regex-replace-all
                "(?s)\\[title\\](.*?)\\[/title\\]" text
                (lambda (match title)
                  (declare (ignore match))
                  (format nil "~a~%" (string-trim '(#\Space #\Tab #\Newline) title)))
                :simple-calls t))
    (setf text (ppcre:regex-replace-all "\\[/?info\\]" text ""))
    (setf text (ppcre:regex-replace-all
                "(?s)\\[download:\\d+\\](.*?)\\[/download\\]" text
                (lambda (match name)
                  (declare (ignore match))
                  (format nil "(attached file: ~a)" (string-trim '(#\Space #\Tab #\Newline) name)))
                :simple-calls t))
    (setf text (ppcre:regex-replace-all
                "(?s)\\[url\\](.*?)\\[/url\\]" text
                (lambda (match url)
                  (declare (ignore match))
                  (string-trim '(#\Space #\Tab #\Newline) url))
                :simple-calls t))
    (setf text (chatwork-drop-tags text))
    (setf text (ppcre:regex-replace-all "\\n{3,}" text (format nil "~%~%")))
    (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun chatwork-parse-inbound (body &key (account-id "") (account-names '()))
  "BODY as the plist a lane reads: :text (the readable words), :to-bot
(whether it opens with [To:ACCOUNT-ID]), :reply-account and :reply-message
(what the first reply tag names), and :system-p (a Chatwork notice, never
answered). A [To:someone-else] stays as \"@\" so the model sees who else was
addressed."
  (let ((body (or body "")))
    (when (search "[dtext:" body)
      (return-from chatwork-parse-inbound (list :text "" :to-bot nil :system-p t)))
    (let ((out '()) (pos 0) (to-bot nil) (reply-account nil) (reply-message nil)
          (names (remove-if-not (lambda (name) (and (stringp name) (plusp (length name))))
                                account-names)))
      (loop
        (multiple-value-bind (from to starts ends)
            (ppcre:scan +address-pattern+ body :start pos)
          (unless from (return))
          (push (subseq body pos from) out)
          (let ((direct (and (svref starts 0) (subseq body (svref starts 0) (svref ends 0))))
                (author (and (svref starts 1) (subseq body (svref starts 1) (svref ends 1))))
                (message (and (svref starts 3) (subseq body (svref starts 3) (svref ends 3)))))
            (cond
              (direct
               (if (equal direct account-id)
                   (progn (setf to-bot t)
                          (setf pos (chatwork-name-end body to names)))
                   (progn (push "@" out)
                          (setf pos to))))
              (t
               (unless reply-account (setf reply-account author reply-message message))
               (if (equal (or author "") account-id)
                   (setf pos (chatwork-name-end body to names))
                   (progn (push "@" out)
                          (setf pos to))))))))
      (push (subseq body (min pos (length body))) out)
      (list :text (chatwork-readable-text (format nil "~{~a~}" (nreverse out)))
            :to-bot to-bot
            :reply-account reply-account
            :reply-message reply-message))))

(defun chatwork-reply-header (account-id room-id message-id name)
  "The line Chatwork's own Reply button writes into a body: it notifies the
author of ACCOUNT-ID and quotes MESSAGE-ID."
  (let ((who (string-trim '(#\Space #\Tab) (or name ""))))
    (format nil "[rp aid=~a to=~a-~a]~a~%"
            (or account-id "") room-id message-id
            (if (plusp (length who)) (format nil "~aさん" who) ""))))

;;; --- the notation, outbound -----------------------------------------------------

(defparameter +notify-tag-pattern+
  "(?i)\\[(toall|To:\\d+|rp aid=[^\\]]*|返信 aid=[^\\]]*|qtmeta[^\\]]*|piconname:\\d+|picon:\\d+|pname:\\d+|dtext:[^\\]]*)\\]"
  "Tags that notify people or speak in someone else's name; a model must not
send them, and a prompt-injected [toall] would ping a whole company room.")

(defparameter +fence-pattern+ "(?ms)^```[^\\n`]*\\n(.*?)^```[ \\t]*$"
  "A fenced block the model wrote, which becomes a Chatwork code box.")

(defparameter +open-fence-pattern+ "(?m)^```[^\\n`]*(?:\\n|$)"
  "A fence still open at a line start: a long answer cut inside a code block
is still a code block.")

(defparameter +heading-pattern+ "(?m)^[ \\t]{0,3}#{1,6}[ \\t]+(.+?)[ \\t#]*$"
  "A Markdown heading, which Chatwork shows as 【...】.")

(defparameter +bold-pattern+ "(\\*\\*)(?=\\S)(.+?)(?<=\\S)\\1"
  "**bold**, which Chatwork does not render; underscores are left alone
because __init__ is code more often than emphasis.")

(defparameter +image-pattern+ "!\\[([^\\]]*)\\]\\((\\S+?)\\)"
  "![alt](url), which becomes the bare url.")

(defparameter +link-pattern+ "\\[([^\\]\\n]+)\\]\\((https?://[^\\s)]+)\\)"
  "[text](url), which becomes \"text (url)\" unless they are the same.")

(defparameter +rule-pattern+ "(?m)^[ \\t]*(?:-{3,}|\\*{3,}|_{3,})[ \\t]*$"
  "A horizontal rule, which Chatwork spells [hr].")

(defun chatwork-placeholder (index)
  "Where the INDEX-th code block waits while the rest of the body is rewritten."
  (format nil "~cCWCODE~d~c" (code-char 0) index (code-char 0)))

(defun chatwork-defuse-tags (text)
  "TEXT with the tags that notify people or speak in someone else's name made
inert by full-width brackets."
  (ppcre:regex-replace-all
   +notify-tag-pattern+ (or text "")
   (lambda (match tag)
     (declare (ignore match))
     (format nil "［~a］" tag))
   :simple-calls t))

(defun chatwork-render-outbound (markdown)
  "MARKDOWN as the body of a Chatwork message: fenced code as a [code] box,
headings as 【...】, no **bold**, and none of the tags that ping a room. The
reply header is the adapter's, not this function's."
  (let ((text (chatwork-defuse-tags (or markdown "")))
        (blocks '()))
    (flet ((stash (code)
             (setf blocks (nconc blocks (list (string-right-trim '(#\Newline) code))))
             (chatwork-placeholder (1- (length blocks)))))
      (setf text (ppcre:regex-replace-all +fence-pattern+ text
                                           (lambda (match code)
                                             (declare (ignore match))
                                             (stash code))
                                           :simple-calls t))
      (multiple-value-bind (from to) (ppcre:scan +open-fence-pattern+ text)
        (when from
          (let ((tail (subseq text to)))
            (setf text (concatenate 'string (subseq text 0 from) (stash tail)))))))
    (setf text (ppcre:regex-replace-all
                +heading-pattern+ text
                (lambda (match heading)
                  (declare (ignore match))
                  (format nil "【~a】" (string-trim '(#\Space #\Tab) heading)))
                :simple-calls t))
    (setf text (ppcre:regex-replace-all
                +bold-pattern+ text
                (lambda (match marker bold)
                  (declare (ignore match marker))
                  bold)
                :simple-calls t))
    (setf text (ppcre:regex-replace-all
                +image-pattern+ text
                (lambda (match alt url) (declare (ignore match alt)) url)
                :simple-calls t))
    (setf text (ppcre:regex-replace-all
                +link-pattern+ text
                (lambda (match label url)
                  (declare (ignore match))
                  (if (equal (string-trim '(#\Space) label) url)
                      url
                      (format nil "~a (~a)" label url)))
                :simple-calls t))
    (setf text (ppcre:regex-replace-all +rule-pattern+ text "[hr]"))
    (loop for block in blocks
          for index from 0
          do (setf text (ppcre:regex-replace-all
                         (ppcre:quote-meta-chars (chatwork-placeholder index))
                         text (format nil "[code]~a[/code]" block))))
    (string-trim '(#\Newline) text)))

;;; --- ids -------------------------------------------------------------------------

(defun chatwork-id-text (value)
  "VALUE -- an id Chatwork spells as a JSON number or a string -- as text,
or NIL."
  (typecase value
    (integer (format nil "~d" value))
    ((string) (if (plusp (length value)) value nil))
    (t nil)))

(defun chatwork-id-number (value)
  "VALUE as the number an id compares by; 0 when it is nothing."
  (let ((text (chatwork-id-text value)))
    (or (and text (parse-integer text :junk-allowed t)) 0)))

;;; --- the read position ------------------------------------------------------------

(defstruct (chatwork-state (:copier nil) (:constructor make-chatwork-state))
  "Where this lane has read to, and what it has posted of its own."
  (account-id "" :type string)
  ;; room id -> the newest message id already handled, as text.
  (cursors (make-hash-table :test #'equal))
  ;; The bot's own postings, newest first, bounded to +SENT-KEEP+: Chatwork
  ;; has no silent post, so in My Chat an answer and the person's line carry
  ;; the same account and only this list tells them apart.
  (sent '() :type list)
  (sent-set (make-hash-table :test #'equal)))

(defun chatwork-state-file ()
  "Where this lane keeps its read position."
  (nlk:home "channel-chatwork/state.json"))

(defun chatwork-state-load (path)
  "The read position stored at PATH, or an empty one when it is absent or
unreadable. A broken file starts the lane fresh instead of stopping it."
  (let ((state (make-chatwork-state :sent-set (make-hash-table :test #'equal))))
    (handler-case
        (when (probe-file path)
          (let ((raw (nlk:decode-json (uiop:read-file-string path))))
            (when (hash-table-p raw)
              (setf (chatwork-state-account-id state)
                    (or (nlk:json-value raw :string "account_id") ""))
              (let ((rooms (nlk:json-value raw :object "rooms")))
                (when rooms
                  (maphash (lambda (room info)
                             (let ((cursor (chatwork-id-text
                                            (nlk:json-value info :string "cursor"))))
                               (when cursor
                                 (setf (gethash room (chatwork-state-cursors state)) cursor))))
                           rooms)))
              (let ((sent (nlk:json-value raw :array "sent")))
                (when sent
                  (loop for id across sent
                        for text = (and (stringp id) id)
                        when text
                          do (push text (chatwork-state-sent state))
                             (setf (gethash text (chatwork-state-sent-set state)) t)))))))
      (error () nil))
    state))

(defun chatwork-state-save (state path)
  "Write STATE to PATH atomically. A write that fails says so and the lane
keeps running."
  (handler-case
      (let ((rooms (make-hash-table :test #'equal)))
        (maphash (lambda (room cursor)
                   (setf (gethash room rooms) (nlk:json-object "cursor" cursor)))
                 (chatwork-state-cursors state))
        (let ((body (nlk:json-object
                     "version" 1
                     "account_id" (chatwork-state-account-id state)
                     "rooms" rooms
                     "sent" (coerce (reverse (copy-list (chatwork-state-sent state))) 'vector)))
              (temp (merge-pathnames (format nil ".state-~d.json" (random 1000000)) path)))
          (ensure-directories-exist path)
          (with-open-file (out temp :direction :output :if-exists :supersede
                                     :external-format :utf-8)
            (write-string (nlk:encode-json-object body) out))
          (uiop:rename-file-overwriting-target temp path)
          t))
    (error (condition)
      (warn "chatwork: could not save the read position to ~a: ~a" path condition)
      nil)))

(defun chatwork-cursor (state room-id)
  "The newest message id in ROOM-ID already handled, 0 when none."
  (chatwork-id-number (gethash room-id (chatwork-state-cursors state))))

(defun chatwork-has-cursor-p (state room-id)
  "Whether this lane has ever read ROOM-ID: without a cursor, the room is
only marked, and nothing said before now is answered."
  (nth-value 1 (gethash room-id (chatwork-state-cursors state))))

(defun chatwork-advance-cursor (state room-id message-id)
  "Move ROOM-ID's read position to MESSAGE-ID when it is newer."
  (let ((text (chatwork-id-text message-id)))
    (when (and text (> (chatwork-id-number text) (chatwork-cursor state room-id)))
      (setf (gethash room-id (chatwork-state-cursors state)) text))))

(defun chatwork-remember-sent (state message-id)
  "Remember that the bot posted MESSAGE-ID itself."
  (let ((text (chatwork-id-text (or message-id ""))))
    (when (and text (not (gethash text (chatwork-state-sent-set state))))
      (setf (gethash text (chatwork-state-sent-set state)) t)
      (push text (chatwork-state-sent state))
      (when (> (length (chatwork-state-sent state)) +sent-keep+)
        (setf (chatwork-state-sent state)
              (loop for id in (chatwork-state-sent state)
                    repeat +sent-keep+
                    collect id))))))

(defun chatwork-sent-by-bot-p (state message-id)
  "Whether MESSAGE-ID is one this lane posted."
  (let ((text (chatwork-id-text (or message-id ""))))
    (and text (gethash text (chatwork-state-sent-set state)) t)))

;;; --- the lane ---------------------------------------------------------------------

(defstruct (chatwork-adapter (:copier nil) (:constructor make-chatwork-adapter))
  "One running Chatwork lane: who the bot is, the rooms it serves, and where
it has read to."
  (host nil)
  (token "" :type string)
  (account-id "" :type string)
  (account-name "" :type string)
  ;; room id -> (:name "..." :type "group" :update-time N :message-num N)
  (rooms (make-hash-table :test #'equal))
  ;; room id -> the metadata marker last read, so an unchanged room costs no call
  (seen (make-hash-table :test #'equal))
  (state (make-chatwork-state) :type chatwork-state)
  (state-file nil)
  ;; message id -> (room account name): who wrote the message an answer replies to
  (inbound (make-hash-table :test #'equal))
  ;; message ids an answer already carried the reply header for
  (addressed (make-hash-table :test #'equal))
  (allowed-rooms '())
  (allowed-users '())
  (free-rooms '())
  (require-mention t)
  (poll-seconds 5 :type integer)
  (catch-up-seconds +max-catchup-seconds+ :type integer)
  (laps 0 :type integer))

(defvar *chatwork-adapter* nil "The live lane, or NIL when it is stopped.")

(defun chatwork-headers (adapter)
  "The one header every Chatwork call carries. The token never rides a body
or a URL, so no message, log line or failure detail can leak it."
  (list (cons "x-chatworktoken" (chatwork-adapter-token-text adapter))))

(defun chatwork-adapter-token-text (adapter)
  "The token this lane speaks with."
  (or (chatwork-adapter-token adapter) ""))

(defun chatwork-room-meta (adapter room-id)
  (gethash room-id (chatwork-adapter-rooms adapter)))

(defun chatwork-room-type (adapter room-id)
  "The kind of room ROOM-ID is: \"group\", \"direct\" or \"my\"."
  (or (getf (chatwork-room-meta adapter room-id) :type) "group"))

(defun chatwork-dm-room-p (adapter room-id)
  "Whether ROOM-ID is a one-to-one room: My Chat or a direct chat."
  (member (chatwork-room-type adapter room-id) '("my" "direct") :test #'equal))

(defun chatwork-learn-rooms (adapter body)
  "Remember every room BODY's GET /rooms answer names."
  (let ((entries (or (nlk:json-value body :array) body)))
    (when (vectorp entries)
      (loop for entry across entries
            for id = (chatwork-id-text (nlk:json-value entry :any "room_id"))
            when id
              do (setf (gethash id (chatwork-adapter-rooms adapter))
                       (list :name (or (nlk:json-value entry :string "name") id)
                             :type (or (nlk:json-value entry :string "type") "group")
                             :update-time (nlk:json-value entry :any "last_update_time")
                             :message-num (nlk:json-value entry :any "message_num")))))))

(defun chatwork-watched-rooms (adapter)
  "The rooms this lane reads: the ones allowed_rooms names and the account is
in, else every room the account is in."
  (let ((listed (chatwork-adapter-allowed-rooms adapter)))
    (if (consp listed)
        (remove-if-not (lambda (room) (chatwork-room-meta adapter room)) listed)
        (loop for room being the hash-keys of (chatwork-adapter-rooms adapter)
              collect room))))

;;; --- what a message becomes ---------------------------------------------------------

(defun chatwork-message-candidate (adapter message room-id &key (now (get-universal-time)))
  "MESSAGE in ROOM-ID as the kit's candidate, or NIL when this lane answers
nothing in it."
  ;; The rules are the original's: never the bot's own post; never a Chatwork
  ;; system notice; a [To:the bot] or a reply to one of its messages in a
  ;; group room, anything in My Chat and in a direct room; nothing older than
  ;; the catch-up window; nothing blank once the notation is read.
  (let* ((state (chatwork-adapter-state adapter))
         (id (chatwork-id-text (nlk:json-value message :any "message_id")))
         (account (nlk:json-value message :object "account"))
         (sender (chatwork-id-text (nlk:json-value account :any "account_id")))
         (name (or (nlk:json-value account :string "name") sender))
         (room-type (chatwork-room-type adapter room-id))
         (my-chat-p (equal room-type "my"))
         (dm-p (member room-type '("my" "direct") :test #'equal))
         (free-p (member room-id (chatwork-adapter-free-rooms adapter) :test #'equal)))
    (when (and id
               (not (chatwork-sent-by-bot-p state id))
               (not (and (equal sender (chatwork-adapter-account-id adapter))
                         (not my-chat-p))))
      (let* ((parsed (chatwork-parse-inbound
                       (nlk:json-value message :string "body")
                       :account-id (chatwork-adapter-account-id adapter)
                       :account-names (list (chatwork-adapter-account-name adapter))))
             (mine (chatwork-adapter-account-id adapter))
             (reply-message (getf parsed :reply-message))
             (replied-to-bot (and (equal (getf parsed :reply-account) mine)
                                   (or (not my-chat-p)
                                       (chatwork-sent-by-bot-p state reply-message))))
             (addressed (or (getf parsed :to-bot) replied-to-bot))
             (text (getf parsed :text))
             (sent (nlk:json-value message :any "send_time"))
             (age (- now (or (and (integerp sent) sent) now))))
        (when (and (not (getf parsed :system-p))
                   (plusp (length text))
                   (or addressed
                       free-p
                       dm-p
                       (not (chatwork-adapter-require-mention adapter)))
                   (or (zerop (chatwork-adapter-catch-up-seconds adapter))
                       (<= age (chatwork-adapter-catch-up-seconds adapter))))
          (let ((source (nlk:json-object
                         "platform" "chatwork"
                         "chat_kind" (if dm-p "direct_message" "channel")
                         "channel_id" room-id
                         "user_id" (or sender "")
                         "user_name" (or name "")
                         "message_id" id
                         "addressed" (and addressed t)
                         "is_bot" nil)))
            (when reply-message
              (setf (gethash "reply_to_message_id" source) reply-message))
            (when (> (hash-table-count (chatwork-adapter-inbound adapter)) +inbound-keep+)
              (clrhash (chatwork-adapter-inbound adapter)))
            (setf (gethash id (chatwork-adapter-inbound adapter))
                  (list room-id (or sender "") (or name "")))
            (nlk:json-object "text" text "source" source)))))))

;;; --- plans ------------------------------------------------------------------------

(defun chatwork-me-plan ()
  "Who the token belongs to: the account the bot speaks as."
  (rest-plan "GET" "me" "Chatwork whoami" t 20))

(defun chatwork-rooms-plan (adapter)
  "Every room the account is in, which is also the room names and types."
  (rest-plan "GET" "rooms" "Chatwork rooms" t 30 nil (chatwork-headers adapter)))

(defun chatwork-messages-plan (adapter room-id)
  "The newest messages of ROOM-ID. force=1, so Chatwork's own unread
pointer -- shared by everything using this token -- is left alone."
  (rest-plan "GET" (format nil "rooms/~a/messages?force=1" room-id)
             "Chatwork messages" t 30 nil (chatwork-headers adapter)))

(defun chatwork-message-id (body)
  "The id a POST /rooms/<id>/messages answer gives its message."
  (chatwork-id-text (nlk:json-value body :any "message_id")))

(defun chatwork-reply-prefix (adapter room-id reply-to)
  "The reply header an answer to REPLY-TO in ROOM-ID opens with, or \"\".
A message is addressed once: a second post to the same ask does not repeat
the tag."
  (when (and reply-to
             (not (gethash reply-to (chatwork-adapter-addressed adapter))))
    (let ((entry (gethash reply-to (chatwork-adapter-inbound adapter))))
      (when (and entry (equal room-id (first entry)))
        (setf (gethash reply-to (chatwork-adapter-addressed adapter)) t)
        (chatwork-reply-header (second entry) room-id reply-to (third entry))))))

(defun chatwork-body (text)
  "TEXT as the form field Chatwork reads a message body from."
  (list (cons "body" (or text ""))))

(defun chatwork-message-plan (adapter target chunk
                              &key reply-to ping timeout-seconds &allow-other-keys)
  "What one answer is: POST the chunk to the room, under Chatwork's own
reply line when this post is the one that answers the ask (PING)."
  (let* ((room (getf target :channel-id))
         (text (format nil "~a~a"
                       (if ping (chatwork-reply-prefix adapter room reply-to) "")
                       (chatwork-render-outbound chunk))))
    (rest-plan "POST" (format nil "rooms/~a/messages" room)
               "Chatwork message" t (or timeout-seconds 30)
               (chatwork-body text) (chatwork-headers adapter))))

(defun chatwork-edit-plan (adapter target message-id text &key timeout-seconds &allow-other-keys)
  "Rewrite MESSAGE-ID: Chatwork lets a posted message be corrected in place,
which is how a card follows its turn."
  (let ((room (getf target :channel-id)))
    (rest-plan "PUT" (format nil "rooms/~a/messages/~a" room message-id)
               "Chatwork edit" t (or timeout-seconds 30)
               (chatwork-body (chatwork-render-outbound text))
               (chatwork-headers adapter))))

(defun chatwork-typing-plan (adapter &key timeout-seconds &allow-other-keys)
  "Chatwork has no typing indicator. The lane still has to say something, so
it says who it is -- the cheapest call the API has, once a day at most."
  (declare (ignore adapter))
  (rest-plan "GET" "me" "Chatwork typing" nil (or timeout-seconds 10)))

(defun chatwork-noting-executor (adapter inner)
  "INNER, with every message this lane posts remembered as its own."
  (make-plan-executor
   :run (lambda (plan &aux (result (execute-plan inner plan)))
          (when (and (execution-ok-p result)
                     (equal "POST" (request-plan-method plan))
                     (search "/messages" (request-plan-path plan)))
            (let ((id (chatwork-message-id (execution-body result))))
              (when id
                (chatwork-remember-sent (chatwork-adapter-state adapter) id)
                (when (chatwork-adapter-state-file adapter)
                  (chatwork-state-save (chatwork-adapter-state adapter)
                                       (chatwork-adapter-state-file adapter))))))
          result)))

;;; --- the platform -----------------------------------------------------------------

(defun chatwork-platform (&key adapter)
  "What Chatwork contributes to the kit's host: the plans a lane draws with,
room-scoped message ids, and where the room is."
  (make-platform
   :id "chatwork"
   :name "Chatwork"
   :noun "room"
   :owner-label "Chatwork account id"
   :session-prefix "chatwork"
   :contract-section "chatwork-room"
   :text-limit +text-limit+
   ;; No typing indicator: the beat is one cheap call a day.
   :typing-refresh-ms +typing-refresh-ms+
   :plan-message (lambda (target chunk &rest keys)
                   (apply #'chatwork-message-plan adapter target chunk keys))
   :plan-edit (lambda (target message-id text &rest keys)
                (apply #'chatwork-edit-plan adapter target message-id text keys))
   ;; Chatwork cannot delete a message, so a card stays where it was written
   ;; and the answer posts under it.
   :plan-delete (constantly nil)
   :plan-typing (lambda (target &rest keys)
                   (declare (ignore target))
                   (apply #'chatwork-typing-plan adapter keys))
   :message-id-of #'chatwork-message-id
   ;; Message ids are per room, so a reply finds its lane only with the room.
   :address-of (lambda (target message-id)
                 (format nil "~a-~a" (or (getf target :channel-id) "") message-id))
   :where-text (lambda (candidate)
                 (let ((target (channel-target candidate)))
                   (format nil "Chatwork room ~a as ~a."
                           (or (getf target :channel-id) "unknown")
                           (or (chatwork-adapter-account-name adapter) "the bot"))))))

(defun chatwork-policy (adapter settings)
  "One inbound policy for the lane. Chatwork's own address rule is applied
when the candidate is made, so the gate has no mention to ask about; it asks
who may talk, and in which room."
  (make-inbound-policy
   :allowed-channels (getf settings :allowed-rooms)
   :allowed-users (chatwork-adapter-allowed-users adapter)
   :require-mention nil
   :self-id (chatwork-adapter-account-id adapter)
   :dm-policy :enabled
   :group-policy :enabled
   :channel-match :exact
   :allow-bots :none))

;;; --- the lap ----------------------------------------------------------------------

(defun chatwork-room-due-p (adapter room-id)
  "Whether ROOM-ID is read this lap: its own metadata moved since it was
last read, or the full-refresh window came round, or it has never been read."
  (let ((marker (chatwork-room-meta adapter room-id))
        (laps (chatwork-adapter-laps adapter)))
    (or (not (chatwork-has-cursor-p (chatwork-adapter-state adapter) room-id))
        (zerop (mod laps +full-refresh-every+))
        (not (equal (list (getf marker :update-time) (getf marker :message-num))
                    (gethash room-id (chatwork-adapter-seen adapter)))))))

(defun chatwork-mark-room (adapter room-id)
  "Remember the metadata ROOM-ID stood at when it was read."
  (let ((marker (chatwork-room-meta adapter room-id)))
    (setf (gethash room-id (chatwork-adapter-seen adapter))
          (list (getf marker :update-time) (getf marker :message-num)))))

(defun chatwork-take-messages (adapter room-id body)
  "One GET messages answer: every message past this room's read position, in
id order, becomes a candidate the host is handed. The read position is saved
BEFORE the asks go out, so a crash can lose one answer but never answer the
same message twice."
  (let* ((state (chatwork-adapter-state adapter))
         (messages (sort (coerce (or (nlk:json-value body :array) #()) 'list)
                         #'< :key (lambda (message)
                                    (chatwork-id-number
                                     (nlk:json-value message :any "message_id"))))))
    (when messages
      (cond
        ((not (chatwork-has-cursor-p state room-id))
         (chatwork-advance-cursor state room-id
                                   (nlk:json-value (car (last messages)) :any "message_id"))
         nil)
        (t
         (let* ((fresh (remove-if-not
                         (lambda (message)
                           (> (chatwork-id-number (nlk:json-value message :any "message_id"))
                              (chatwork-cursor state room-id)))
                         messages))
                (chosen (loop for message in fresh
                               for candidate = (chatwork-message-candidate
                                                adapter message room-id)
                               when candidate collect candidate)))
           (when fresh
             (chatwork-advance-cursor state room-id
                                       (nlk:json-value (car (last fresh)) :any "message_id")))
           chosen))))))

(defun chatwork-lap (adapter stop-p)
  "One poll: the room list, then the new messages of every room due. Answers
the seconds to wait before the next lap, or :stop."
  (when (funcall stop-p) (return-from chatwork-lap :stop))
  (let* ((host (chatwork-adapter-host adapter))
         (executor (host-executor host))
         (state (chatwork-adapter-state adapter))
         (rooms (execute-plan executor (chatwork-rooms-plan adapter))))
    (cond
      ((not (execution-ok-p rooms))
       (set-channel-status "chatwork" :state :degraded :connected nil
                                       :detail (probe-failure rooms))
       (chatwork-adapter-poll-seconds adapter))
      (t
       (chatwork-learn-rooms adapter (execution-body rooms))
       (dolist (room (chatwork-watched-rooms adapter))
         (when (funcall stop-p) (return-from chatwork-lap :stop))
         (when (chatwork-room-due-p adapter room)
           (let ((messages (execute-plan executor (chatwork-messages-plan adapter room))))
             (if (execution-ok-p messages)
                 (dolist (candidate (chatwork-take-messages
                                      adapter room (execution-body messages)))
                   (handler-case (handle-candidate host candidate)
                     (error (condition)
                       (warn "chatwork: room ~a ask signalled: ~a" room condition))))
                 (set-channel-status "chatwork" :state :degraded :connected nil
                                               :detail (probe-failure messages))))
           (chatwork-mark-room adapter room)))
       (setf (chatwork-adapter-laps adapter) (1+ (chatwork-adapter-laps adapter)))
       (when (chatwork-adapter-state-file adapter)
         (chatwork-state-save state (chatwork-adapter-state-file adapter)))
       (let ((count (length (chatwork-watched-rooms adapter))))
         (set-channel-status "chatwork" :state :connected :connected t
                                        :detail (format nil "~d room~:p" count)))
       (chatwork-adapter-poll-seconds adapter)))))

;;; --- start and stop ----------------------------------------------------------------

(defun chatwork-settings (section)
  "The channels.chatwork SECTION as the plist this lane runs on."
  (nlk:section-settings (nlk:find-section '("channels" "chatwork")) section))

(defun start-channel (section &key executor)
  "Start the Chatwork lane from its channels.chatwork SECTION; answers the
stop thunk. EXECUTOR overrides the live one -- the scripted test seam."
  (let* ((settings (chatwork-settings section))
         (declared (nlk:find-section '("channels" "chatwork")))
         (token (resolve-channel-secret section "token"))
         (rooms (getf settings :allowed-rooms))
         (users (getf settings :allowed-users))
         (owners (resolve-owners "chatwork" (getf settings :owner) users "allowed_rooms"))
         (allowed-users (and users (union (copy-list users) owners :test #'string=)))
         (bot (or executor
                  (make-dexador-executor :base-url +api-base+
                                         :redact-prefixes (list token)))))
    (when (and declared (nlk:section-problems declared section))
      (config-error "~a" (nlk:section-summary declared section)))
    (require-non-empty-allowlist "chatwork" "allowed_rooms" rooms "allowed_users" allowed-users)
    (let ((me (execute-plan bot (chatwork-me-plan))))
      (unless (execution-ok-p me)
        (config-error "channels.chatwork: Chatwork would not say who this token belongs to (~a); issue a token at ~a"
                      (probe-failure me) +token-page+))
      (let* ((account-id (or (nlk:json-value (execution-body me) :any "account_id") ""))
             (file (chatwork-state-file))
             (state (chatwork-state-load file)))
        ;; A token of another account starts from now in every room: the
        ;; rooms it may not see are forgotten with the position it had in them.
        (unless (equal (chatwork-state-account-id state) (chatwork-id-text account-id))
          (setf state (make-chatwork-state :sent-set (make-hash-table :test #'equal))))
        (setf (chatwork-state-account-id state) (or (chatwork-id-text account-id) ""))
        (let* ((adapter (make-chatwork-adapter
                         :token token
                         :account-id (or (chatwork-id-text account-id) "")
                         :account-name (or (nlk:json-value (execution-body me) :string "name") "")
                         :state state
                         :state-file file
                         :allowed-rooms (copy-list rooms)
                         :allowed-users allowed-users
                         :free-rooms (getf settings :free-response-rooms)
                         :require-mention (getf settings :require-mention)
                         :poll-seconds (or (getf settings :poll-seconds) 5)
                         :catch-up-seconds (* 60 (or (getf settings :catch-up-minutes) 1440))))
               (host (make-host-from-section section
                                             (chatwork-platform :adapter adapter)
                                             :executor (chatwork-noting-executor adapter bot)
                                             :policy (chatwork-policy adapter settings)
                                             :owners owners
                                             :request-timeout 30)))
          (setf (chatwork-adapter-host adapter) host)
          (run-host host "poll" (lambda (stop-p) (chatwork-lap adapter stop-p))
                    :on-start (lambda ()
                                (setf *chatwork-adapter* adapter)
                                (chatwork-state-save state file))
                    :on-stop (lambda ()
                               (chatwork-state-save state file)
                               (when (eq *chatwork-adapter* adapter)
                                 (setf *chatwork-adapter* nil)))))))))

(defun probe-channel (section &key executor)
  "What the token can see, as text: the account it belongs to, and the rooms
it is in by id and name. Never the token."
  (let* ((token (ignore-errors (resolve-channel-secret section "token")))
         (bot (or executor
                  (make-dexador-executor :base-url +api-base+
                                         :redact-prefixes (and token (list token))))))
    (unless token
      (return-from probe-channel
        (format nil "chatwork: set token_env or token_file first (a token from ~a)" +token-page+)))
    (let ((me (execute-plan bot (chatwork-me-plan))))
      (unless (execution-ok-p me)
        (return-from probe-channel
          (format nil "chatwork: the token was not accepted (~a)" (probe-failure me))))
      (let ((rooms (execute-plan bot (chatwork-rooms-plan
                                      (make-chatwork-adapter :token token)))))
        (unless (execution-ok-p rooms)
          (return-from probe-channel
            (format nil "chatwork: the rooms could not be read (~a)" (probe-failure rooms))))
        (let* ((adapter (make-chatwork-adapter :token token))
               (listing '()))
          (chatwork-learn-rooms adapter (execution-body rooms))
          (maphash (lambda (id meta)
                     (push (format nil "~a (~a, ~a)" id (getf meta :name) (getf meta :type))
                           listing))
                   (chatwork-adapter-rooms adapter))
          (format nil "chatwork: ~a (account ~a) is in ~d room~:p: ~{~a~^; ~}"
                  (or (nlk:json-value (execution-body me) :string "name") "?")
                  (or (chatwork-id-text (nlk:json-value (execution-body me) :any "account_id")) "?")
                  (hash-table-count (chatwork-adapter-rooms adapter))
                  (sort listing #'string<)))))))

;;; --- what this lane's section is made of -------------------------------

(nlk:define-section ("channels" "chatwork")
  (:guide "log in to Chatwork as the account the bot speaks as, open the API token page (.../subpackages/api/token.php) and issue a token, and keep it in a file or an environment variable -- never in the config; that account must be a member of every room it answers in. Then set allowed_rooms to the room ids it serves (the number after 'rid' in the room's URL) and allowed_users to the account ids allowed to drive the bot, and ask for (nck:probe \"chatwork\") to name the rooms the token can see.")
  (:check #'probe-channel)
  (:one-of "token_env" "token_file")
  (:any-of "allowed_rooms" "allowed_users")
  ("token_env" :env :doc "environment variable holding the API token of the account the bot speaks as")
  ("token_file" :path :doc "file holding that API token")
  ("allowed_rooms" :list
                    :doc "room ids the bot serves and answers in; empty serves every room the account is in")
  ("allowed_users" :list
                   :doc "Chatwork account ids allowed to drive the bot; empty lets everyone in a served room ask")
  ("owner" :list :doc "the operator's account id, whose word is standing policy in the room")
  ("free_response_rooms" :list
                         :doc "room ids where every message is answered, [To:the bot] or not")
  ("require_mention" :boolean :default t
                     :doc "in a group room, answer only a [To:the bot] or a reply to one of the bot's own messages")
  ("poll_seconds" :integer :default 5 :min 2
                  :doc "seconds between checks; Chatwork allows 300 calls per five minutes, so a shorter lap is not a faster bot")
  ("catch_up_minutes" :integer :default 1440 :min 0
                      :doc "asks said while the bot was away are answered when they are at most this old; 0 answers none of them"))

