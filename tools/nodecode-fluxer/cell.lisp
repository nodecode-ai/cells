;;;; cell.lisp --- the section, the five tools, /fluxer, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Fluxer is a chat platform with a bot API: guilds, channels, messages,
;;;; reactions and pins. This cell is the bot's client half, against the
;;;; same REST API the original Hermes plugin talks to -- the outbound
;;;; half of that plugin (post, edit-by-reply, react, pin, read history,
;;;; enumerate the channel directory) rather than its gateway websocket.
;;;;
;;;; Five tools, what the plugin's send/reaction/pin/directory surface
;;;; gives a model:
;;;;   fluxer_send_message   post a message in a channel, as a reply when asked
;;;;   fluxer_read_messages  the last N messages of a channel, oldest first
;;;;   fluxer_list_channels  the channels of one guild, or of every guild
;;;;   fluxer_react          put an emoji reaction on one message
;;;;   fluxer_pin            pin a message, or unpin it
;;;;
;;;; Every verb answers a string; a refusal is ERROR: FLUXER-ERROR, so the
;;;; model reads what went wrong and the transcript draws the call failed.

(in-package #:nodecode-fluxer)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Fluxer is a chat platform: a bot posts into its guilds' channels, and
this cell is the handle on that bot.

(fluxer:send-message \"CHANNEL\" \"TEXT\" &key reply-to)  post a message.
(fluxer:read-messages \"CHANNEL\" &key limit)  recent messages, oldest first,
    each line its message id, who wrote it and what it says.
(fluxer:list-channels &key guild)  channel ids and names.
(fluxer:react \"CHANNEL\" \"MESSAGE\" \"EMOJI\")  react to one message.
(fluxer:pin-message \"CHANNEL\" \"MESSAGE\" &key remove)  pin it, or unpin it.
The model has the same five as tools: fluxer_send_message,
fluxer_read_messages, fluxer_list_channels, fluxer_react and fluxer_pin.

A channel id is what a line of /fluxer channels prints first; a message id is
what a line of /fluxer read prints first. /fluxer alone shows the endpoint,
the home channel and the last failure; /fluxer channels, /fluxer read
CHANNEL and /fluxer send TEXT (or /fluxer send #CHANNEL TEXT) ask by hand, and
\"/fluxer send TEXT\" posts into the section's home_channel. Every message this
cell posts has @everyone, role and user mentions broken with a zero-width
space unless the section's allow_mentions is on. A refusal is
ERROR: FLUXER-ERROR and says what to set."
  "What (help :fluxer) answers while the cell runs.")

;;; --- the five tools -------------------------------------------------------------

(nlk:define-contract send-call
  (:doc "Post a message in a Fluxer channel, as the bot. Answers the message id the send minted, for a later reaction, pin or reply.")
  ("channel" :text :required t
             :doc "the channel id to post in; /fluxer channels names them")
  ("content" :text :required t
             :doc "the message, as Markdown Fluxer renders")
  ("reply_to" :text
              :doc "a message id in the same channel to reply under; leave it out for a plain message"))

(nlk:define-contract read-call
  (:doc "The most recent messages of a Fluxer channel, oldest first, one line each: message id, author, text, attachments. Newest first is how the platform answers; this reverses it, so a conversation reads downward.")
  ("channel" :text :required t :doc "the channel id to read")
  ("limit" :integer :default 20
           :doc "how many recent messages to bring back"))

(nlk:define-contract channels-call
  (:doc "The channels a Fluxer bot can post in: one guild's by id, or every guild the bot is in, each channel's id and name.")
  ("guild" :text :doc "a guild id to list one guild's channels; leave it out to list every guild's"))

(nlk:define-contract react-call
  (:doc "Put an emoji reaction on one Fluxer message, as the bot.")
  ("channel" :text :required t :doc "the channel the message is in")
  ("message" :text :required t :doc "the message id to react to")
  ("emoji" :text :required t :doc "the emoji to react with, as one character or a :name:"))

(nlk:define-contract pin-call
  (:doc "Pin a Fluxer message in its channel, or take the pin off one.")
  ("channel" :text :required t :doc "the channel the message is in")
  ("message" :text :required t :doc "the message id to pin")
  ("remove" :boolean :default nil :slot unpin
            :doc "true to unpin a message, false to pin it"))

(defun guarded (body)
  "Run BODY, answering its string; a refusal or a dial that failed becomes
the tool answer ERROR: ..., so the model reads what went wrong."
  (handler-case (funcall body)
    (fluxer-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: FLUXER-ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: FLUXER-ERROR: ~a" condition)))))

;;; --- the verbs ----------------------------------------------------------------

(defun send-message (channel content &key reply-to)
  "Post CONTENT in CHANNEL, as a reply to REPLY-TO when given => a sentence."
  (guarded
   (lambda ()
     (let* ((message (post-message channel content :reply-to reply-to))
            (id (or (text-field message "id") "(no id)")))
       (setf *last-error* nil)
       (format nil "Sent message ~a to channel ~a~@[ as a reply to ~a~]."
               id channel (and reply-to (plusp (length reply-to)) reply-to))))))

(defun read-messages (channel &key limit)
  "The last LIMIT messages of CHANNEL, oldest first => their lines."
  (guarded
   (lambda ()
     (let* ((count (or limit (setting :read-limit) 20))
            (lines (mapcar #'message-line (fetch-messages channel count))))
       (setf *last-error* nil)
       (if lines
           (format nil "~{~a~^~%~}" lines)
           (format nil "No messages in channel ~a." channel))))))

(defun channel-line (channel &optional guild)
  "One channel as a line: the id a call takes, then its name."
  (format nil "~a  #~a~@[  (~a)~]"
          (or (text-field channel "id") "?")
          (or (text-field channel "name") "?")
          guild))

(defun guild-name (guild)
  (or (text-field guild "name") (text-field guild "id") "?"))

(defun list-channels (&key guild)
  "GUILD's channels, or every guild's => their lines."
  (guarded
   (lambda ()
     (let ((lines
             (if (and guild (plusp (length guild)))
                 (mapcar (lambda (channel) (channel-line channel)) (guild-channels guild))
                 (loop for one in (guild-list)
                       append (mapcar (lambda (channel)
                                        (channel-line channel (guild-name one)))
                                      (guild-channels (text-field one "id")))))))
       (setf *last-error* nil)
       (if lines
           (format nil "~{~a~^~%~}" lines)
           "This bot is in no guild with a channel it can name.")))))

(defun react (channel message emoji)
  "React to MESSAGE in CHANNEL with EMOJI => a sentence."
  (guarded
   (lambda ()
     (put-reaction channel message emoji)
     (setf *last-error* nil)
     (format nil "Reacted with ~a to message ~a in channel ~a." emoji message channel))))

(defun pin-message (channel message &key remove)
  "Pin MESSAGE in CHANNEL, or unpin it with REMOVE => a sentence."
  (guarded
   (lambda ()
     (set-pin channel message :remove remove)
     (setf *last-error* nil)
     (format nil "~:[Pinned~;Unpinned~] message ~a in channel ~a."
             (and remove t) message channel))))

;;; --- the model's side of them ---------------------------------------------------

(defun send-tool (call)
  (send-message (send-call-channel call) (send-call-content call)
                :reply-to (send-call-reply-to call)))

(defun read-tool (call)
  (read-messages (read-call-channel call) :limit (read-call-limit call)))

(defun channels-tool (call)
  (list-channels :guild (channels-call-guild call)))

(defun react-tool (call)
  (react (react-call-channel call) (react-call-message call) (react-call-emoji call)))

(defun pin-tool (call)
  (pin-message (pin-call-channel call) (pin-call-message call)
               :remove (pin-call-unpin call)))

;;; --- /fluxer -------------------------------------------------------------------

(defun status-line ()
  "One line: the endpoint, the home channel, the switches, the last failure."
  (format nil "fluxer: ~a; bot token ~:[missing~;set~]; home channel ~:[none~;~:*~a~]; mentions ~:[neutralized~;allowed~]; last error ~:[none~;~:*~a~]"
          (base)
          (plusp (length (bot-token)))
          (let ((home (setting :home-channel))) (and home (plusp (length home)) home))
          (allow-mentions-p)
          (and *last-error* (subseq *last-error* 0 (min 200 (length *last-error*))))))

(defun split-verb (args)
  "ARGS as (values HEAD REST): the first word, and the trimmed remainder."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun split-word (text)
  "TEXT as (values FIRST-WORD REST)."
  (let ((space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun home-channel ()
  (let ((home (setting :home-channel)))
    (and home (plusp (length (nlk:trimmed home))) (nlk:trimmed home))))

(defun slash-send (rest)
  "REST as a send: `#CHANNEL TEXT' posts there, a bare TEXT into home_channel."
  (let ((home (home-channel)))
    (cond ((zerop (length rest)) "fluxer: send what? /fluxer send TEXT")
          ((char= (char rest 0) #\#)
           (multiple-value-bind (place text) (split-word (subseq rest 1))
             (if (zerop (length text))
                 "fluxer: send what? /fluxer send #CHANNEL TEXT"
                 (send-message place text))))
          (home (send-message home rest))
          (t "fluxer: no home_channel in the section: /fluxer send #CHANNEL TEXT"))))

(defun run-slash (args session-id)
  "One /fluxer invocation => the answer's text, and typed alone the status."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (split-verb args)
    (cond ((zerop (length head)) (values (status-line) nil))
          ((string-equal head "status") (values (status-line) nil))
          ((string-equal head "channels")
           (values (list-channels :guild (and (plusp (length rest)) rest)) nil))
          ((string-equal head "read")
           (multiple-value-bind (channel limit) (split-word rest)
             (if (zerop (length channel))
                 (values "fluxer: read which channel? /fluxer read CHANNEL [LIMIT]" nil)
                 (values (read-messages channel
                                         :limit (and (plusp (length limit))
                                                     (ignore-errors (parse-integer limit))))
                         nil))))
          ((string-equal head "send") (values (slash-send rest) nil))
          (t (values (format nil "fluxer: ~a is not one of its choices (send, read, channels, status)"
                              head)
                     nil)))))

;;; --- the one declaration -------------------------------------------------------

(defun start ()
  "Refuse a section that could never reach Fluxer, then clear the last
failure; a stop takes the state with it."
  (check-config)
  (setf *last-error* nil)
  (nle:on-stop (lambda () (setf *last-error* nil))))

(nle:define-cell fluxer
  (:section ("fluxer")
    (:guide "make a Fluxer bot in the Fluxer developer settings and paste its token in bot_token; base_url is the hosted API unless you run your own instance; home_channel is where /fluxer sends and where cron reports go; allow_mentions decides whether the bot's words may ping @everyone")
    ("bot_token" :secret :required t
                 :doc "the Fluxer bot token, <application id>.<secret>")
    ("base_url" :string :default "https://api.fluxer.app/v1"
                :doc "the API root: the hosted one, or a self-hosted deployment's")
    ("home_channel" :string
                    :doc "the channel id /fluxer send and reports go to when none is named")
    ("allow_mentions" :boolean
                      :doc "let a posted message ping @everyone, roles and users; off, every mention is broken with a zero-width space")
    ("read_limit" :integer :default 20 :min 1
                  :doc "how many recent messages one read brings back when it names no limit")
    ("timeout" :integer :default 20 :min 1
               :doc "seconds one call to the Fluxer API may take"))
  (:tool "fluxer_send_message" (nlk:contract 'send-call) 'send-tool)
  (:tool "fluxer_read_messages" (nlk:contract 'read-call) 'read-tool)
  (:tool "fluxer_list_channels" (nlk:contract 'channels-call) 'channels-tool)
  (:tool "fluxer_react" (nlk:contract 'react-call) 'react-tool)
  (:tool "fluxer_pin" (nlk:contract 'pin-call) 'pin-tool)
  (:command "fluxer" 'run-slash
            :description "Fluxer: post, read, react and pin as the bot"
            :argument-hint "[send|read|channels|status] ...")
  (:help :fluxer "fluxer: Fluxer chat from Nodecode - fluxer:send-message, fluxer:read-messages, fluxer:list-channels" +primer+)
  (:start #'start))
