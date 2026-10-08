;;;; rest.lisp --- Slack Web API request plans, the thread book, the executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Web API method is a POST (or, for a read, a GET) under
;;;; https://slack.com/api/ carrying the bot token as a bearer. Slack answers
;;;; nearly every refusal as HTTP 200 with {"ok": false, "error": "..."}, so
;;;; the executor reads the envelope: a 200 that says ok false is a failure
;;;; naming Slack's error. A 429 carries Retry-After, which the kit's retry
;;;; loop already reads.
;;;;
;;;; A message is posted as blocks: one markdown block, which renders the
;;;; standard Markdown a model writes (headings, lists, code, links), and --
;;;; on a running status line -- an actions block holding its buttons. `text'
;;;; rides beside the blocks as the notification's words. An edit sends the
;;;; blocks whole, so a button taken away is gone.

(in-package #:nodecode-channel-slack)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution))

(defparameter +slack-api-base+ "https://slack.com/api")

;;; The markdown block's own ceiling: 12,000 characters across a message's
;;; markdown blocks.
(defparameter +slack-text-limit+ 12000)

;;; --- the thread book -------------------------------------------------------------
;;; Which thread a message lives in, and which threads the bot speaks in. A
;;; reply goes in the thread its ask lives in -- Slack refuses nothing but
;;; files a reply to a reply as a thread of its own -- and a thread the bot
;;; answers in is one a person may go on in without mentioning it. Learned
;;; from the events (the lap thread) and from the plans (the delivery pool),
;;; so the tables are synchronized; live-only, and bounded.

(defparameter +thread-book-cap+ 20000
  "Entries either table holds before it starts over.")

(defstruct (thread-book (:copier nil))
  ;; "<channel>:<ts>" -> the ts of the thread's first message.
  (roots (make-hash-table :test #'equal :synchronized t))
  ;; "<channel>:<root ts>" -> T once the bot has posted in that thread.
  (spoken (make-hash-table :test #'equal :synchronized t)))

(nlk:access (book thread-book))

(defun book-key (channel ts)
  (format nil "~a:~a" channel ts))

(defun book-note (table key value)
  "Set KEY to VALUE in TABLE, starting it over once it holds the cap."
  (when (>= (hash-table-count table) +thread-book-cap+)
    (clrhash table))
  (setf (gethash key table) value))

(defun note-thread-message (book channel ts root)
  "Remember that message TS in CHANNEL lives in the thread ROOT starts."
  (when (and channel ts root (not (equal ts root)))
    (book-note book.roots (book-key channel ts) root)))

(defun bot-thread-p (book channel root)
  "Whether the bot has posted in the thread ROOT starts in CHANNEL."
  (and (gethash (book-key channel root) book.spoken) t))

(defun dm-channel-p (channel)
  "Whether CHANNEL is a direct message: Slack's DM ids begin with D."
  (uiop:string-prefix-p "D" (or channel "")))

(defun reply-thread (book target reply-to &aux (channel (getf target :channel-id)))
  "The thread a reply to REPLY-TO goes in: the thread REPLY-TO already lives
in, else -- outside a DM -- the thread REPLY-TO starts; NIL posts in the
channel itself."
  ;; A DM reads as a chat, so an ask typed there is answered there; in a
  ;; channel the answer hangs off the ask, the way Slack's own apps answer.
  (and reply-to
       (or (gethash (book-key channel reply-to) book.roots)
           (and (not (dm-channel-p channel)) reply-to))))

;;; --- plans -----------------------------------------------------------------------

(defun api-plan (method body label &key retry (timeout-seconds 30))
  "The POST of BODY to the Web API METHOD: 5xx and 429 retried when RETRY,
LABEL naming it in audits and failure copy."
  (rest-plan "POST" (format nil "/~a" method) label retry timeout-seconds body))

(defun control-button (control index &aux (style (third control)))
  "One of the kit's (LABEL DATA STYLE) controls as a Block Kit button."
  (nlk:json-object "type" "button"
                   "action_id" (format nil "nodecode-~d" index)
                   "text" (nlk:json-object "type" "plain_text" "text" (first control))
                   "value" (second control)
                   :when (member style '(:danger :primary))
                   "style" (string-downcase (symbol-name style))))

(defun message-blocks (text controls)
  "TEXT as a markdown block, followed by CONTROLS -- the kit's buttons, or
:CLEAR for none -- as an actions block."
  (let ((buttons (loop for control in (unless (eq controls :clear) controls)
                       for index from 0
                       collect (control-button control index))))
    (coerce (list* (nlk:json-object "type" "markdown" "text" text)
                   (and buttons
                        (list (nlk:json-object "type" "actions"
                                               "elements" (coerce buttons 'vector)))))
            'vector)))

(defun message-body (target text &key thread controls)
  "The members every post and edit carry: the channel, the blocks, and the
notification's words."
  (nlk:json-object "channel" (getf target :channel-id)
                   :opt "thread_ts" thread
                   "text" (nlk:clip (nlk:one-line text) 1000)
                   "blocks" (message-blocks text controls)))

(defun mentions-text (chunk mentions &aux (text chunk.text))
  "CHUNK's text, the first chunk opening with a mention of each of MENTIONS:
the note that needs the operator reaches them."
  (if (and mentions (= 1 chunk.index))
      (format nil "~{<@~a> ~}~a" mentions text)
      text))

(defun message-plan (book target chunk &key reply-to ping controls files mentions
                                            (timeout-seconds 30))
  "One chat.postMessage CHUNK (a kit text-chunk), in the thread REPLY-TO
lives in or starts (REPLY-THREAD); every chunk of one post goes in the same
thread."
  ;; CONTROLS ride the first chunk. Slack has no silent message, so PING
  ;; changes nothing: a thread notifies whoever follows it, the way it does
  ;; for any reply. FILES are refused before a message is planned: the
  ;; platform carries none (see SLACK-PLATFORM). Posting in a thread notes
  ;; that the bot speaks there. Retried: a post that fails is an answer lost.
  (declare (ignore ping files))
  (let ((thread (reply-thread book target reply-to))
        (channel (getf target :channel-id)))
    (when thread
      (book-note book.spoken (book-key channel thread) t))
    (api-plan "chat.postMessage"
              (message-body target (mentions-text chunk mentions)
                            :thread thread :controls (and (= 1 chunk.index) controls))
              (if (> chunk.total 1)
                  (format nil "send_message_chunk_~a_of_~a" chunk.index chunk.total)
                  "send_message")
              :retry t :timeout-seconds timeout-seconds)))

(defun edit-plan (target message-id text &key retry controls (timeout-seconds 30))
  "chat.update: the status line settled in place, its buttons with it."
  ;; RETRY opts into 5xx/429 retries: a running update leaves it off (the next
  ;; tick supersedes a missed edit), the terminal settle turns it on. CONTROLS
  ;; NIL keeps the message buttonless: an edit sends the blocks whole.
  (let ((body (message-body target text :controls controls)))
    (setf (gethash "ts" body) message-id)
    (api-plan "chat.update" body "edit_message" :retry (and retry t)
                                                :timeout-seconds timeout-seconds)))

(defun delete-plan (target message-id &key (timeout-seconds 30))
  "chat.delete -- retiring a status line once the answer it stood in for has
landed. Retried: a status line outliving its answer is a duplicate."
  (api-plan "chat.delete" (nlk:json-object "channel" (getf target :channel-id) "ts" message-id)
            "delete_message" :retry t :timeout-seconds timeout-seconds))

(defun typing-plan (book target &key (timeout-seconds 30))
  "assistant.threads.setStatus: `is working...' under the thread the answer
will land in, or NIL where the answer lands in the channel itself -- a DM --
since Slack shows the status only in a thread."
  ;; Slack clears it when the bot posts in the thread, and after two minutes;
  ;; the kit re-asserts it on the platform's cadence. Not retried: a missed beat
  ;; corrects itself on the next tick.
  (nlk:when-let (thread (reply-thread book target (getf target :message-id)))
    (api-plan "assistant.threads.setStatus"
              (nlk:json-object "channel_id" (getf target :channel-id)
                               "thread_ts" thread
                               "status" "is working...")
              "typing_indicator" :timeout-seconds timeout-seconds)))

(defparameter +reaction-names+ '(("👀" . "eyes"))
  "Slack names a reaction by its emoji name: the kit's glyphs, spelled that way.")

(defun reaction-name (emoji)
  "EMOJI as Slack names it: the kit's glyph through the table, a name as is."
  (or (cdr (assoc emoji +reaction-names+ :test #'equal))
      (string-trim ":" emoji)))

(defun reaction-plans (target message-id emoji &key previous (timeout-seconds 30))
  "The bot's one reaction on MESSAGE-ID: PREVIOUS removed first -- Slack
stacks reactions -- then EMOJI added, or nothing added for NIL."
  ;; Retried: the mark holds for the whole turn, so its clear must land even
  ;; through a 429. already_reacted and no_reaction read as success (the
  ;; executor), the state asked for being the state there.
  (flet ((plan (method glyph label)
           (api-plan method (nlk:json-object "channel" (getf target :channel-id)
                                             "timestamp" message-id
                                             "name" (reaction-name glyph))
                     label :retry t :timeout-seconds timeout-seconds)))
    (unless (equal previous emoji)
      (append (and previous (list (plan "reactions.remove" previous "remove_reaction")))
              (and emoji (list (plan "reactions.add" emoji "add_reaction")))))))

(defun respond-plan (candidate text &key (timeout-seconds 30)
                                    &aux (url (source-field candidate "response_url")))
  "The answer to a slash command, through the response_url it arrived with,
in the channel for everyone to read -- or NIL for anything else, which is
answered as a reply."
  (when url
    (rest-plan "POST" url "respond" t timeout-seconds
               (nlk:json-object "response_type" "in_channel"
                                "text" (nlk:clip (nlk:one-line text) 1000)
                                "blocks" (message-blocks text nil)))))

(defun slack-message-id (body)
  "The ts of the message a chat.postMessage execution created."
  (nlk:json-value body :string "ts"))

(defun slack-address (target message-id)
  "The address-book key for one Slack message: its ts is unique within its
channel, so the channel rides in the key."
  (book-key (getf target :channel-id) message-id))

;;; --- the executor ----------------------------------------------------------------

(defparameter +settled-errors+ '("already_reacted" "no_reaction" "message_not_found")
  "Slack errors that mean the state asked for is the state there: a reaction
already on or already off, a status line already gone.")

(defun slack-failure-message (plan status detail)
  "One failed call in words: Slack's error code when the envelope names one."
  (format nil "slack ~a failed with status ~a: ~a" plan.audit-label status detail))

(defun wrap-slack-executor (inner)
  "An executor speaking Slack's envelope in the kit's terms: a 200 that says
ok false fails with Slack's error, the errors that mean `already so' succeed,
and a plan that is NIL -- a typing beat Slack cannot show -- is done."
  (make-plan-executor
   :run (lambda (plan)
          (if (null plan)
              (make-execution :ok-p t :status 200 :attempts 0)
              (let* ((result (execute-plan inner plan))
                     (body result.body)
                     (code (and (hash-table-p body)
                                (eq nil (gethash "ok" body t))
                                (or (nlk:json-value body :string "error") "error"))))
                (when (and result.ok-p code)
                  (if (member code +settled-errors+ :test #'string=)
                      (setf result.body nil)
                      (setf result.ok-p nil
                            result.error (slack-failure-message plan result.status code))))
                result)))))

(defun make-slack-executor (&key (api-base +slack-api-base+) token)
  "The live executor: the Web API under API-BASE with TOKEN as the bearer,
which never reaches a failure text."
  (wrap-slack-executor
   (make-dexador-executor
    :base-url api-base
    :headers (list (cons "authorization" (format nil "Bearer ~a" token)))
    :redact-prefixes (list token)
    :failure-message-fn #'slack-failure-message)))
