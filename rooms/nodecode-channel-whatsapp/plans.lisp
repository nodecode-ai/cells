;;;; plans.lisp --- the plans a post, an edit, a reaction, a file, a poll is.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every plan is one driver op: the request-plan's path is the op (/send,
;;;; /edit, /revoke, /react, /typing, /send_file, /poll, /read ...) and its body
;;;; the op's args (driver/main.go, DISPATCH). The executor (driver.lisp) runs
;;;; them; a recording executor in a test records them.
;;;;
;;;; What WhatsApp allows, and so what the plans do:
;;;;   - A post may quote one message -- the reply gesture -- and @mention
;;;;     people by number; its words are WhatsApp's own marks (markdown.lisp).
;;;;   - An edit is allowed for 20 minutes after sending. A card a longer turn
;;;;     keeps editing past that stays as it last read; its answer posts fresh.
;;;;   - A deletion is a revoke: the message is replaced, for everyone, by a
;;;;     note that it was deleted.
;;;;   - One reaction a person a message: a new one replaces the old.
;;;;   - No buttons: a message's choices ride under it as a poll (POLL-OF),
;;;;     whose vote the adapter reads as the press. Stop is a typed /stop, and
;;;;     a running card says so.

(in-package #:nodecode-channel-whatsapp)

(nlk:access (chunk text-chunk))

;;; WhatsApp takes 65,536 characters a message; a phone reads a long one
;;; badly well before that, so an answer comes in parts of this many.
(defparameter +whatsapp-text-limit+ 6000)

(defparameter +edit-window-ms+ (* 19 60 1000)
  "How long after sending the bot still edits a message: WhatsApp's own
window is 20 minutes, and an edit sent near its end can land past it.")

(defparameter +poll-option-limit+ 12
  "The most options a WhatsApp poll holds.")

;;; --- the book -------------------------------------------------------------------
;;; What the plans and the lap share: when each of the bot's messages was
;;; sent (the edit window), what each poll's options press, the reaction each
;;; person last left on one of the bot's messages (WhatsApp says only that it
;;; was taken back), and the names and groups the chats showed. Live-only and
;;; bounded.

(defparameter +book-cap+ 20000
  "Entries one table holds before it starts over.")

(defstruct (whatsapp-book (:constructor make-whatsapp-book) (:copier nil))
  (sent (make-hash-table :test #'equal :synchronized t))
  (polls (make-hash-table :test #'equal :synchronized t))
  (marks (make-hash-table :test #'equal :synchronized t))
  (names (make-hash-table :test #'equal :synchronized t))
  (groups (make-hash-table :test #'equal :synchronized t)))

(nlk:access (book whatsapp-book))

(defun book-key (&rest parts)
  (format nil "~{~a~^|~}" parts))

(defun book-note (table key value)
  "Set KEY to VALUE in TABLE, starting it over once it holds the cap."
  (when (>= (hash-table-count table) +book-cap+)
    (clrhash table))
  (setf (gethash key table) value))

(defun note-sent (book chat id &optional (at (now-ms)))
  (when (and chat id)
    (book-note book.sent (book-key chat id) at)))

(defun editable-p (book chat id &optional (now (now-ms)))
  "Whether the bot's message ID in CHAT is still inside the edit window --
T for one the book does not know, which the driver then judges."
  (let ((sent (gethash (book-key chat id) book.sent)))
    (or (null sent) (< (- now sent) +edit-window-ms+))))

;;; --- plans ----------------------------------------------------------------------

(defun op-plan (op args label &key retry (timeout-seconds 30))
  "The driver OP with ARGS, named LABEL in audits: retried through a
reconnect when RETRY."
  (make-request-plan :method "POST" :path (format nil "/~a" op) :body args
                     :timeout-seconds timeout-seconds :retry-server-errors (and retry t)
                     :audit-label label))

(defun stop-control-p (control)
  (and (consp control) (stringp (second control))
       (or (eql 0 (search "nck:stop:" (second control)))
           (eql 0 (search "nck:details:" (second control))))))

(defun control-options (controls)
  "CONTROLS -- the kit's rows of buttons and menus -- as (LABEL . DATA)
options in order, every enabled one but a card's Stop and Details: a menu's
options, the current one checked; a row's choice; each button of a row."
  (loop for control in (unless (eq controls :clear) controls)
        append (cond ((eq (first control) :menu)
                      (loop for (label data description current) in (third control)
                            collect (cons (format nil "~:[~;✓ ~]~a~@[ · ~a~]" current label description)
                                          data)))
                     ((eq (first control) :row)
                      (destructuring-bind (mark label detail choice) (rest control)
                        (and choice (not (fourth choice))
                             (list (cons (format nil "~@[~a ~]~a~@[ · ~a~]" mark label detail)
                                         (second choice))))))
                     (t (loop for button in (if (consp (first control)) control (list control))
                              for (label data nil disabled) = button
                              when (and (stringp label) (stringp data) (not disabled)
                                        (not (stop-control-p button)))
                                collect (cons label data))))))

(defun distinct-options (options)
  "OPTIONS with every label cut to a poll's 100 characters and made unique:
a poll tells its options apart by their words alone."
  (let ((seen (make-hash-table :test #'equal)))
    (loop for (label . data) in options
          for base = (nlk:clip (nlk:one-line label) 95 :ellipsis "…")
          for n = (incf (gethash base seen 0))
          collect (cons (if (= n 1) base (format nil "~a (~d)" base n)) data))))

(defun poll-name (text panel)
  "What a poll under TEXT asks: a dialog PANEL's title, else the words' last
line -- the question an answer that offers choices ends on."
  (let ((title (and (consp panel) (evenp (length panel)) (getf panel :title)))
        (last-line (car (last (remove-if (lambda (line) (zerop (length (nlk:trimmed line))))
                                         (uiop:split-string (or text "") :separator '(#\Newline)))))))
    (nlk:clip (nlk:one-line (or (and (stringp title) (plusp (length title)) title)
                                (and last-line (ppcre:regex-replace-all "[*_~`]" last-line ""))
                                "Choose one"))
              250 :ellipsis "…")))

(defun poll-of (controls text panel)
  "The poll that carries CONTROLS' choices under TEXT, as the composite
post's __poll member, or NIL when there are fewer than two -- a poll asks
between at least two. More than a poll holds keeps the first twelve."
  (let ((options (distinct-options (control-options controls))))
    (when (>= (length options) 2)
      (let ((options (subseq options 0 (min (length options) +poll-option-limit+))))
        (nlk:json-object "name" (poll-name text panel)
                         "options" (coerce (mapcar #'car options) 'vector)
                         "selectable" 1
                         "__choices" (coerce (mapcar #'cdr options) 'vector))))))

(defun controls-hint (controls)
  "The line a card carrying a Stop says, since WhatsApp draws no button for
it; NIL for any other."
  (when (some (lambda (control)
                (some (lambda (button)
                        (and (consp button) (stringp (second button))
                             (eql 0 (search "nck:stop:" (second button)))))
                      (if (consp (first control)) control (list control))))
              (unless (eq controls :clear) controls))
    "_/stop ends it_"))

(defun post-text (markdown controls)
  (let ((text (whatsapp-text markdown))
        (hint (controls-hint controls)))
    (if hint (format nil "~a~%~%~a" text hint) text)))

(defun send-message-plan (book target chunk &key reply-to ping controls files mentions card panel media
                                            (timeout-seconds 30))
  "One message of CHUNK (a kit text-chunk) in TARGET's chat: the first chunk
quoting REPLY-TO, MENTIONS named so WhatsApp notifies them, FILES sent ahead
of the words and CONTROLS' choices as a poll under them (POLL-OF)."
  ;; PING is the kit's "this post should notify": every WhatsApp message does,
  ;; to the chat's own settings. CARD and MEDIA are a card's pictures, which
  ;; WhatsApp's text carries none of; the card's words are CHUNK. Retried: a
  ;; post that fails is an answer lost.
  (declare (ignore ping card media book))
  (let* ((chat (getf target :channel-id))
         (first-p (= 1 chunk.index))
         (text (post-text chunk.text controls))
         (poll (poll-of controls chunk.text panel))
         (body (nlk:json-object "chat" chat "text" text
                                :when (and reply-to first-p) "quote" reply-to
                                :when mentions "mentions" (coerce (remove nil (mapcar #'user-jid mentions))
                                                                  'vector))))
    (when (and files first-p)
      (setf (gethash "__files" body) (coerce (mapcar #'file-args files) 'vector)))
    (when poll
      (setf (gethash "__poll" body) poll))
    (op-plan "send" body (if (> chunk.total 1)
                             (format nil "send_message_chunk_~a_of_~a" chunk.index chunk.total)
                             "send_message")
             :retry t :timeout-seconds (if (or files poll) (max 60 timeout-seconds) timeout-seconds))))

(defun edit-message-plan (book target message-id text &key retry controls card media (timeout-seconds 30))
  "The bot's MESSAGE-ID edited to say TEXT, a card's Stop said beneath it;
NIL once WhatsApp's edit window has passed for it -- nothing to send, and
the message keeps what it said."
  ;; RETRY opts into retries through a reconnect: a running update leaves it
  ;; off (the next tick supersedes a missed edit), the terminal settle turns
  ;; it on.
  (declare (ignore card media))
  (let ((chat (getf target :channel-id)))
    (when (editable-p book chat message-id)
      (op-plan "edit" (nlk:json-object "chat" chat "id" message-id "text" (post-text text controls))
               "edit_message" :retry retry :timeout-seconds timeout-seconds))))

(defun revoke-plan (target message-id &key (timeout-seconds 30))
  "MESSAGE-ID deleted for everyone -- retiring a status line once the
answer it stood in for has landed. Retried: one that outlives its answer is
a duplicate."
  (op-plan "revoke" (nlk:json-object "chat" (getf target :channel-id) "id" message-id)
           "delete_message" :retry t :timeout-seconds timeout-seconds))

(defun typing-plan (target &key (timeout-seconds 30))
  "The bot typing in TARGET's chat, which the kit re-asserts on the
platform's cadence. Not retried: a missed beat corrects itself."
  (op-plan "typing" (nlk:json-object "chat" (getf target :channel-id) "state" "composing")
           "typing_indicator" :timeout-seconds timeout-seconds))

(defun reaction-plans (target message-id emoji &key previous (timeout-seconds 30))
  "The bot's one reaction on MESSAGE-ID: EMOJI, or none for NIL -- WhatsApp
replaces a person's reaction, so one call says it."
  (unless (equal previous emoji)
    (list (op-plan "react" (nlk:json-object "chat" (getf target :channel-id) "id" message-id
                                            "emoji" (or emoji ""))
                   (if emoji "add_reaction" "remove_reaction")
                   :retry t :timeout-seconds timeout-seconds))))

(defun read-plan (chat ids &key (timeout-seconds 15))
  "IDS in CHAT marked read: the blue ticks an ask the bot took shows."
  (op-plan "read" (nlk:json-object "chat" chat "ids" (coerce ids 'vector))
           "mark_read" :timeout-seconds timeout-seconds))

;;; --- files ----------------------------------------------------------------------

(defun file-head (pathname &optional (count 16))
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let* ((octets (make-array count :element-type '(unsigned-byte 8) :initial-element 0))
           (read (read-sequence octets in)))
      (subseq octets 0 read))))

(defun file-kind (pathname &key voice)
  "(values KIND MIME) of the file at PATHNAME, by what its first bytes say:
a picture WhatsApp shows inline is an image, a recording audio -- a voice
message when VOICE -- an MP4 video, anything else a document."
  (let* ((head (ignore-errors (file-head pathname)))
         (n (length head)))
    (flet ((at (offset &rest octets)
             (and (>= n (+ offset (length octets)))
                  (loop for octet in octets for i from offset always (= octet (aref head i)))))
           (ascii (offset text)
             (and (>= n (+ offset (length text)))
                  (loop for char across text for i from offset always (= (char-code char) (aref head i))))))
      (cond ((at 0 #x89 #x50 #x4E #x47) (values "image" "image/png"))
            ((at 0 #xFF #xD8 #xFF) (values "image" "image/jpeg"))
            ((and (ascii 0 "RIFF") (ascii 8 "WEBP")) (values "image" "image/webp"))
            ((ascii 0 "OggS") (values (if voice "voice" "audio") "audio/ogg; codecs=opus"))
            ((or (ascii 0 "ID3") (and (at 0 #xFF) (>= n 2) (= #xE0 (logand #xE0 (aref head 1)))))
             (values "audio" "audio/mpeg"))
            ((and (ascii 4 "ftyp") (or (ascii 8 "M4A") (ascii 8 "M4B"))) (values "audio" "audio/mp4"))
            ((ascii 4 "ftyp") (values "video" "video/mp4"))
            ((ascii 0 "%PDF") (values "document" "application/pdf"))
            ((ascii 0 "GIF8") (values "document" "image/gif"))
            (t (values "document" "application/octet-stream"))))))

(defun voice-waveform (octets &optional (points 64))
  "OCTETS, the kit's waveform of a recording (each loudness 0-255), as the
POINTS loudnesses 0-100 a WhatsApp voice message draws."
  (let ((n (length octets)))
    (when (plusp n)
      (coerce (loop for i below points
                    for start = (floor (* i n) points)
                    for end = (max (1+ start) (floor (* (1+ i) n) points))
                    collect (round (* 100 (loop for j from start below (min end n)
                                                maximize (aref octets j)))
                                   255))
              'vector))))

(defun file-args (pathname &key content voice)
  "The send_file args for the file at PATHNAME: its kind and type from its
bytes, CONTENT its caption, VOICE (:seconds S :waveform W) a voice message."
  (multiple-value-bind (kind mime) (file-kind pathname :voice voice)
    (nlk:json-object "path" (uiop:native-namestring pathname)
                     "kind" kind "mime" mime
                     "filename" (file-namestring pathname)
                     :when (and content (plusp (length content))) "caption" (whatsapp-text content)
                     :when voice "seconds" (ceiling (or (getf voice :seconds) 0))
                     :when (and voice (getf voice :waveform)) "waveform" (voice-waveform (getf voice :waveform)))))

(defun file-plan (target pathname &key content reply-to voice (timeout-seconds 60))
  "The file at PATHNAME posted in TARGET's chat as its own message -- a
picture inline, a recording to play, anything else a document -- CONTENT
its caption, quoting REPLY-TO."
  ;; A minute is the floor: a file is heavier than a message.
  (let ((args (file-args pathname :content content :voice voice)))
    (setf (gethash "chat" args) (getf target :channel-id))
    (when reply-to (setf (gethash "quote" args) reply-to))
    (op-plan "send_file" args (if voice "send_voice" "send_file")
             :retry t :timeout-seconds (max 60 timeout-seconds))))

(defun one-chunk (text)
  "TEXT as the one chunk a single message is, cut to a message's length."
  (first (split-text-chunks text +whatsapp-text-limit+)))

;;; --- ids ------------------------------------------------------------------------

(defun whatsapp-message-id (body)
  "The id of the message a send created."
  (nlk:json-value body :string "id"))

(defun whatsapp-address (target message-id)
  "The address-book key for one WhatsApp message: ids are a sender's own,
so the chat rides in the key."
  (book-key (getf target :channel-id) message-id))
