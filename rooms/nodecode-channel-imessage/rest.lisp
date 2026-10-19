;;;; rest.lisp --- BlueBubbles REST plans, the chat book, the executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every call is under the server's /api/v1, the server password a query
;;;; parameter (the server reads no header), added by the executor so no plan
;;;; and no failure text carries it. An answer is an envelope {status,
;;;; message, data}; a refusal says why under error.message.
;;;;
;;;; Two ways to send. AppleScript drives Messages.app itself and works on any
;;;; server: it sends words and files, nothing else. The Private API (the
;;;; server's helper bundle inside Messages, which needs SIP partly off on the
;;;; Mac) adds what a person can do by hand: an inline reply, a tapback, the
;;;; typing bubble, an unsend. The book says which the server has, and every
;;;; plan reads it, so a server without the helper is served plainly and
;;;; never asked for what it cannot do.
;;;;
;;;; A turn's card is never posted. Messages can edit a sent message five
;;;; times in fifteen minutes and marks it Edited, so a card ticking in place
;;;; is not something it can carry: while a turn works, the typing bubble and
;;;; the ask's tapback say so, and a turn that fails says why in a message of
;;;; its own.

(in-package #:nodecode-channel-imessage)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution))

(defparameter +api+ "/api/v1")

;;; Messages takes far longer bubbles, but one this long is still read on a
;;; phone without a fold.
(defparameter +imessage-text-limit+ 4000)

(defun encode (segment)
  "SEGMENT, a chat or message guid, as one path segment."
  (quri:url-encode segment))

(defun api-path (&rest segments)
  (format nil "~a~{/~a~}" +api+ segments))

(sb-ext:defglobal **temp-counter** (list 0)
  "How many temp guids this process has handed out.")

(defun new-temp-guid ()
  "A temp guid no other send of this process uses: the server matches a sent
message back to its send by it."
  (format nil "temp-nc-~36r-~36r" (now-ms) (sb-ext:atomic-incf (car **temp-counter**))))

;;; --- the chat book -------------------------------------------------------------------
;;; What the lap learns and the plans need: whether the server has the Private
;;; API, which messages are the bot's, which thread a message lives in, which
;;; threads the bot speaks in, and the choices each chat has open. Learned on
;;; the lap thread and the delivery pool, so the tables are synchronized;
;;; live-only, and bounded.

(defparameter +imessage-book-cap+ 20000
  "Entries any one table holds before it starts over.")

(defstruct (imessage-book (:copier nil))
  ;; Whether the server sends through its Private API helper.
  (private-api nil)
  ;; guid -> T for a message the bot sent.
  (mine (make-hash-table :test #'equal :synchronized t))
  ;; guid -> the root of the thread the message lives in.
  (roots (make-hash-table :test #'equal :synchronized t))
  ;; root guid -> T once the bot has replied in that thread.
  (spoken (make-hash-table :test #'equal :synchronized t))
  ;; chat guid -> (:message GUID :items ((LABEL DATA) ...)), the choices under
  ;; the bot's latest message carrying some.
  (choices (make-hash-table :test #'equal :synchronized t)))

(nlk:access (book imessage-book))

(defun book-note (table key value)
  "Set KEY to VALUE in TABLE, starting it over once it holds the cap."
  (when (>= (hash-table-count table) +imessage-book-cap+)
    (clrhash table))
  (setf (gethash key table) value))

(defun bot-message-p (book guid)
  "Whether GUID is the bot's own message, or the root of a thread it speaks in."
  (and guid (or (gethash guid book.mine) (gethash guid book.spoken)) t))

(defun reply-root (book guid)
  "The root of the thread a reply to GUID lives in."
  (or (gethash guid book.roots) guid))

;;; --- plans -----------------------------------------------------------------------

(defun send-method (book)
  (if book.private-api "private-api" "apple-script"))

(defun noop-plan (label timeout-seconds &optional body)
  "A plan that reaches nothing: what a card's post, edit and delete are."
  (rest-plan "NOOP" "none" label nil timeout-seconds
             (list* :id (format nil "card-~36r" (sb-ext:atomic-incf (car **temp-counter**))) body)))

(defun card-id-p (id)
  "Whether ID names a card, which was never posted."
  (and (stringp id) (uiop:string-prefix-p "card-" id)))

(defun fail-plan (label timeout-seconds why)
  (rest-plan "FAIL" "none" label nil timeout-seconds (list :error why)))

(defun text-plan (book chat text &key reply-to (label "send_message") (timeout-seconds 60)
                                   files voice items)
  "The POST of one message of TEXT into CHAT, an inline reply to REPLY-TO
where the server can send one; FILES ride ahead of the words, VOICE says the
one file is a voice message, ITEMS are the choices the words end with."
  ;; Not retried: a send the server timed out on may still have gone, and a
  ;; message said twice is worse than a failure the card reports. The
  ;; private members (__files, __voice, __choices) are the executor's, taken
  ;; off before the request.
  (let ((reply (and reply-to book.private-api reply-to)))
    (when reply
      (book-note book.spoken (reply-root book reply) t))
    (rest-plan "POST" (api-path "message" "text") label nil timeout-seconds
               (nlk:json-object "chatGuid" chat
                                "tempGuid" (new-temp-guid)
                                "message" text
                                "method" (send-method book)
                                :when reply "selectedMessageGuid" reply
                                :when reply "partIndex" 0
                                :when files "__files" (mapcar #'namestring files)
                                :when voice "__voice" t
                                :when items "__choices" items))))

(defun message-plan (book target chunk &key reply-to controls files card panel (timeout-seconds 60)
                     &allow-other-keys)
  "One message of CHUNK (a kit text-chunk) -- or nothing, for a turn's CARD.
The first chunk replies to REPLY-TO in a group; CONTROLS are drawn as
numbered choices under the words."
  ;; A direct message reads as a chat, so its answer is a plain message; in a
  ;; group the answer hangs off its ask, in the ask's thread. A command's
  ;; PANEL is its words and its choices.
  (declare (ignore panel))
  (if card
      (noop-plan "imessage_card" timeout-seconds)
      (let* ((chat (getf target :channel-id))
             (items (choice-items controls))
             (words (markdown-plain (text-chunk-text chunk)))
             (first-p (= 1 (text-chunk-index chunk))))
        (text-plan book chat (if items (format nil "~a~%~%~a" words (choice-lines items)) words)
                   :reply-to (and first-p (not (direct-chat-p chat)) reply-to)
                   :label (if (> (text-chunk-total chunk) 1)
                              (format nil "send_message_chunk_~a_of_~a" (text-chunk-index chunk)
                                      (text-chunk-total chunk))
                              "send_message")
                   :files (and first-p files)
                   :items items
                   :timeout-seconds timeout-seconds))))

(defun stop-typing-plan (book target timeout-seconds)
  (if book.private-api
      (rest-plan "DELETE" (api-path "chat" (encode (getf target :channel-id)) "typing")
                 "typing_stop" nil timeout-seconds)
      (noop-plan "imessage_typing" timeout-seconds)))

(defun edit-plan (book target message-id text &key card controls (timeout-seconds 60)
                  &allow-other-keys)
  "The card's settle, where it says something: a turn that ended stops the
typing bubble, one that failed says why in a message of its own. Any other
edit leaves the message as it was sent."
  ;; A sent message stays as sent: an edit would mark it Edited and costs one
  ;; of Messages' five, so a settled message's choices are only closed in the
  ;; book (a reply picking one then reads as words).
  (let ((state (getf card :state)))
    (cond ((not (card-id-p message-id))
           (when (or (null controls) (eq controls :clear))
             (let ((chat (getf target :channel-id)))
               (when (equal message-id (getf (gethash chat book.choices) :message))
                 (remhash chat book.choices))))
           (noop-plan "imessage_edit" timeout-seconds (list :id message-id)))
          ((eq state :failed)
           (text-plan book (getf target :channel-id) (markdown-plain text) :label "card_failed"
                                                                            :timeout-seconds timeout-seconds))
          ((member state '(:done :stopped :paused))
           (stop-typing-plan book target timeout-seconds))
          (t (noop-plan "imessage_card" timeout-seconds (list :id message-id))))))

(defun delete-plan (book target message-id &key (timeout-seconds 60) &allow-other-keys)
  "A card's retirement -- its answer has landed -- which stops the typing
bubble; a sent message unsent, where the server can (Messages allows two
minutes), else refused in words."
  ;; The server's helper keeps the bubble up until told otherwise, the
  ;; answer's arrival notwithstanding.
  (cond ((card-id-p message-id) (stop-typing-plan book target timeout-seconds))
        (book.private-api
         (rest-plan "POST" (api-path "message" (encode message-id) "unsend") "unsend_message" nil
                    timeout-seconds (nlk:json-object "partIndex" 0)))
        (t (fail-plan "unsend_message" timeout-seconds
                      "iMessage without the server's Private API cannot take a message back"))))

(defun typing-plan (book target &key (timeout-seconds 30) &allow-other-keys)
  "The typing bubble in the chat, which the kit re-asserts on the platform's
cadence; nothing on a server without the Private API."
  (if book.private-api
      (rest-plan "POST" (api-path "chat" (encode (getf target :channel-id)) "typing")
                 "typing_indicator" nil timeout-seconds)
      (noop-plan "imessage_typing" timeout-seconds)))

(defun reaction-plans (book target message-id emoji &key previous (timeout-seconds 30)
                       &allow-other-keys)
  "The bot's one tapback on MESSAGE-ID: EMOJI's (EMOJI-TAPBACK), which takes
the place of the one there; NIL takes PREVIOUS back."
  ;; Retried: the mark holds for the whole turn, so its clear must land.
  (let ((now (and emoji (emoji-tapback emoji)))
        (before (and previous (emoji-tapback previous))))
    (when (and book.private-api (or now before) (not (equal now before)))
      (list (rest-plan "POST" (api-path "message" "react") (if now "add_reaction" "remove_reaction")
                       t timeout-seconds
                       (nlk:json-object "chatGuid" (getf target :channel-id)
                                        "selectedMessageGuid" message-id
                                        "reaction" (or now (format nil "-~a" before))
                                        "partIndex" 0))))))

(defun file-plan (book target pathname &key content reply-to voice (timeout-seconds 120)
                  &allow-other-keys)
  "PATHNAME sent into the chat as its own message, CONTENT the words after
it; VOICE sends an Ogg Opus PATHNAME as a voice message."
  (let ((chat (getf target :channel-id)))
    (text-plan book chat (markdown-plain (or content ""))
               :reply-to (and (not (direct-chat-p chat)) reply-to)
               :label (if voice "send_voice" "send_file")
               :files (list pathname) :voice (and voice t)
               :timeout-seconds timeout-seconds)))

(defparameter +message-with+ '("chat" "chat.participants" "attachment" "handle")
  "What each message the lap reads comes with.")

(defun query-plan (after-rowid &key (limit 100))
  "The POST that reads the messages written after AFTER-ROWID, oldest first."
  (rest-plan "POST" (api-path "message" "query") "read_messages" nil 30
             (nlk:json-object "with" (coerce +message-with+ 'vector)
                              "where" (vector (nlk:json-object "statement" "message.ROWID > :rowid"
                                                               "args" (nlk:json-object "rowid" after-rowid)))
                              "sort" "ASC"
                              "limit" limit)))

(defun newest-plan ()
  "The POST that reads the newest message, whose row is where a first start
begins."
  (rest-plan "POST" (api-path "message" "query") "newest_message" nil 30
             (nlk:json-object "limit" 1 "sort" "DESC")))

(defun read-plan (path label &key (timeout-seconds 15))
  (rest-plan "GET" (format nil "~a~a" +api+ path) label nil timeout-seconds))

(defun contacts-plan (addresses)
  (rest-plan "POST" (api-path "contact" "query") "contacts" nil 15
             (nlk:json-object "addresses" (coerce addresses 'vector))))

(defun imessage-message-id (body)
  "The guid of the message a send created, or the id a card stands under."
  (if (consp body)
      (getf body :id)
      (nlk:json-value body :string "data" "guid")))

;;; --- the executor ----------------------------------------------------------------

(defun imessage-failure-message (plan status detail)
  "One failed call in words: the server's own reason when it says."
  (format nil "imessage ~a failed with status ~a: ~a" plan.audit-label status detail))

(defun with-password (plan password)
  "PLAN with the server PASSWORD on its query."
  (if (or (null password) (search "://" plan.path))
      plan
      (rest-plan plan.method (format nil "~a~:[?~;&~]password=~a" plan.path (find #\? plan.path)
                                     (encode password))
                 plan.audit-label plan.retry-server-errors plan.timeout-seconds plan.body plan.headers)))

(defun audio-message-file (path)
  "PATH, Ogg Opus, as the MP3 the server sends as a voice message (it turns
MP3 into Messages' own CAF), made with ffmpeg beside a scratch name."
  (let ((out (uiop:tmpize-pathname (make-pathname :name "nodecode-voice" :type "mp3"
                                                  :defaults (uiop:temporary-directory)))))
    (multiple-value-bind (output error status)
        (uiop:run-program (list "ffmpeg" "-y" "-loglevel" "error" "-i" (namestring path)
                                "-ac" "1" "-b:a" "64k" (namestring out))
                          :ignore-error-status t :output :string :error-output :string)
      (declare (ignore output))
      (unless (and (zerop status) (probe-file out))
        (error "ffmpeg could not make the voice message: ~a" (nlk:one-line error :cap 200)))
      out)))

(defun attachment-plan (chat path &key method reply-to voice (timeout-seconds 120))
  "The multipart POST of one file into CHAT."
  (rest-plan "POST" (api-path "message" "attachment") (if voice "send_voice" "send_file") nil
             timeout-seconds
             (append (list (cons "chatGuid" chat)
                           (cons "tempGuid" (new-temp-guid))
                           (cons "name" (file-namestring path))
                           (cons "method" method))
                     (and voice (list (cons "isAudioMessage" "true")))
                     (and reply-to (list (cons "selectedMessageGuid" reply-to) (cons "partIndex" "0")))
                     (list (cons "attachment" (pathname path))))))

(defun run-with-files (inner plan)
  "PLAN, a send naming files under __files: each sent as its own message,
then the words -- or, when there are none, the last file's send as the post."
  (let* ((body plan.body)
         (chat (gethash "chatGuid" body))
         (method (gethash "method" body))
         (reply-to (gethash "selectedMessageGuid" body))
         (voice (gethash "__voice" body))
         (last nil))
    (dolist (path (gethash "__files" body))
      (let ((sent (if voice
                      (handler-case (audio-message-file path)
                        (error (condition)
                          (return-from run-with-files
                            (make-execution :ok-p nil :status 0 :error (princ-to-string condition)))))
                      path)))
        (setf last (unwind-protect
                        (execute-plan inner (attachment-plan chat sent :method method :reply-to reply-to
                                                                       :voice voice
                                                                       :timeout-seconds plan.timeout-seconds))
                     (unless (eq sent path) (ignore-errors (delete-file sent)))))
        (setf reply-to nil)
        (unless (execution-ok-p last) (return-from run-with-files last))))
    (if (and last (zerop (length (nlk:trimmed (or (gethash "message" body) "")))))
        last
        (let ((words (alexandria:copy-hash-table body)))
          (remhash "__files" words)
          (remhash "__voice" words)
          (unless reply-to
            (remhash "selectedMessageGuid" words)
            (remhash "partIndex" words))
          (execute-plan inner (rest-plan plan.method plan.path plan.audit-label nil plan.timeout-seconds
                                         words plan.headers))))))

(defun note-sent (book plan result)
  "What one successful send leaves in BOOK: its message the bot's, and the
choices it ends with open in its chat."
  (let ((guid (and result.ok-p (imessage-message-id result.body))))
    (when (and guid (hash-table-p plan.body))
      (book-note book.mine guid t)
      (nlk:when-let (items (gethash "__choices" plan.body))
        (book-note book.choices (gethash "chatGuid" plan.body) (list :message guid :items items))))
    result))

(defun wrap-imessage-executor (inner &key book password)
  "An executor speaking BlueBubbles in the kit's terms: PASSWORD on every
query, a NOOP plan done and a FAIL plan refused without a call, a send
naming files sending them first, every send noted in BOOK."
  (make-plan-executor
   :run (lambda (plan)
          (cond
            ((null plan) (make-execution :ok-p t :status 200 :attempts 0))
            ((string= "NOOP" plan.method) (make-execution :ok-p t :status 200 :attempts 0 :body plan.body))
            ((string= "FAIL" plan.method)
             (make-execution :ok-p nil :status 0 :attempts 0 :error (getf plan.body :error)))
            (t
             (let* ((files (and (hash-table-p plan.body) (gethash "__files" plan.body)))
                    (bare (if (and (hash-table-p plan.body) (gethash "__choices" plan.body))
                              (let ((body (alexandria:copy-hash-table plan.body)))
                                (remhash "__choices" body)
                                (rest-plan plan.method plan.path plan.audit-label plan.retry-server-errors
                                           plan.timeout-seconds body plan.headers))
                              plan))
                    (keyed (make-plan-executor
                            :run (lambda (each) (execute-plan inner (with-password each password)))))
                    (result (if files (run-with-files keyed bare) (execute-plan keyed bare))))
               ;; A message sent ends the typing bubble, as it does on
               ;; Discord; the kit re-asserts it while the turn still works.
               ;; The server's helper would keep it up under the answer.
               (when (and book book.private-api (execution-ok-p result)
                          (equal plan.path (api-path "message" "text")))
                 (execute-plan keyed (stop-typing-plan book (list :channel-id (gethash "chatGuid" plan.body)) 30)))
               (if book (note-sent book plan result) result)))))))

(defun bluebubbles-detail (body)
  "The reason a refusal BODY gives, for a failure text."
  (let ((body (if (stringp body) (ignore-errors (nlk:decode-json body)) body)))
    (and (hash-table-p body)
         (or (nlk:json-value body :text "error" "message")
             (nlk:json-value body :text "message")))))

(defun make-imessage-executor (&key url password book)
  "The live executor: the REST API under URL with PASSWORD on every query,
which never reaches a failure text."
  (wrap-imessage-executor
   (make-dexador-executor
    :base-url url
    :redact-prefixes (list password (encode password))
    :failure-message-fn (lambda (plan status detail)
                          (imessage-failure-message plan status (or (bluebubbles-detail detail) detail))))
   :book book :password password))
