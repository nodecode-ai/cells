;;;; probe.lisp --- what the token can see, and how the operator gets one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) resolves the secret the way
;;;; START-CHANNEL does, asks Discord who the bot is and where it may
;;;; speak, and answers a text — never the token — so channel and user ids
;;;; are picked by name from the conversation instead of typed out of
;;;; Developer Mode. openclaw's probe.ts / resolve-users.ts are the shape;
;;;; chrome's do-doctor is the in-tree precedent. The section declaration
;;;; at the end is the adapter's one statement of what channels.discord is
;;;; made of (NLK:DEFINE-SECTION): the kit's refusal, the setup wizard's
;;;; panel, the model's setup primer and the cells report all read it.

(in-package #:nodecode-channel-discord)

(defparameter +probed-channel-types+ '(0 5)
  "GUILD_TEXT and GUILD_ANNOUNCEMENT: where a text message can land.")

(defparameter +probed-guilds-cap+ 10)

(defun probe-array (body)
  "A top-level JSON array from an execution body — the executor decodes it
to a vector (NCK PARSE-JSON-BODY) — or NIL for any other shape."
  (and (vectorp body) (not (stringp body)) body))

(defun probe-text-channels (execution)
  "The text channels a guild's channel read EXECUTION answered, as a list:
none when it failed."
  (remove-if-not
   (lambda (channel)
     (member (nlk:json-value channel :integer "type") +probed-channel-types+))
   (coerce (or (and (execution-ok-p execution) (probe-array (execution-body execution))) #())
           'list)))

(defparameter +message-content-flags+ (logior (ash 1 18) (ash 1 19))
  "GATEWAY_MESSAGE_CONTENT, or its _LIMITED twin (granted under 100 servers).")

(defparameter +message-content-limited+ (ash 1 19)
  "GATEWAY_MESSAGE_CONTENT_LIMITED: the Message Content Intent of an app in
fewer than 100 servers, the switch on its Bot page.")

(defparameter +limited-intent-flags+ (logior (ash 1 13) (ash 1 15) (ash 1 19))
  "GATEWAY_PRESENCE_LIMITED, GATEWAY_GUILD_MEMBERS_LIMITED and
GATEWAY_MESSAGE_CONTENT_LIMITED: the only flags Discord lets an app write
on itself (Edit Current Application, PATCH /applications/@me).")

(defparameter +administrator+ (ash 1 3))

(defparameter +manage-threads+ (ash 1 34)
  "What closing a done thread takes, the board's included.")

(defparameter +invite-permissions+
  (logior (ash 1 10) (ash 1 11) (ash 1 16) (ash 1 6) (ash 1 15) ; view, send, history, react, attach
          (ash 1 14)                                            ; embed links: each turn's card
          +manage-threads+ (ash 1 35) (ash 1 38)                ; manage, create public, send in threads
          (ash 1 20) (ash 1 21))                                ; voice: connect, speak
  "What the invite link asks for: everything the adapter does in a server,
voice included -- docs/setup.md's table, 326420778048.")

(defparameter +people-channels-cap+ 30
  "The most text channels whose recent messages name the people offered.")

(defparameter +people-channels-per-guild+ 8
  "The most of one server's text channels read for its people, so every
server's are reached.")

(defun invite-url (application-id)
  "The link that adds APPLICATION-ID's bot to a server the operator picks,
with +INVITE-PERMISSIONS+ and its slash commands."
  (format nil "https://discord.com/oauth2/authorize?client_id=~a&scope=bot%20applications.commands&permissions=~d"
          application-id +invite-permissions+))

(defun probe-guilds (section executor timeout)
  "(values PROBLEM GUILDS ME FETCH LONELY) with SECTION's token: the line
saying why nothing further was read, else NIL; the first
+PROBED-GUILDS-CAP+ guilds as (ID NAME CHANNELS-EXECUTION); the /users/@me
execution; the probe's request, (FETCH PATH &optional METHOD BODY); LONELY
true when the token is good and the bot is in no server yet."
  (let* ((token (resolve-channel-secret section "bot_token"))
         (executor (or executor
                       (make-discord-executor
                        :api-base (config-string section "api_base"
                                                 +discord-rest-api-base+)
                        :token token))))
    (flet ((fetch (path &optional (method "GET") body)
             (execute-plan executor (rest-plan method path "probe" nil timeout body))))
      (let* ((me (fetch "/users/@me"))
             (guilds (and (execution-ok-p me) (fetch "/users/@me/guilds")))
             (vector (and guilds (execution-ok-p guilds)
                          (probe-array (execution-body guilds))))
             (lonely (and vector (zerop (length vector)))))
        (values (cond ((not (execution-ok-p me))
                       (format nil "token: ~a"
                               (if (eql 401 (execution-status me))
                                   "unauthorized (401): the token is wrong or revoked; reset it on the Bot page of the developer portal and save the new one"
                                   (probe-failure me))))
                      ((not (execution-ok-p guilds))
                       (format nil "guilds: ~a" (probe-failure guilds)))
                      ((or (null vector) lonely)
                       "guilds: none yet - open the invite link and pick a server"))
                (loop for guild across (or vector #())
                      for count from 0 below +probed-guilds-cap+
                      collect (let ((id (nlk:json-value guild :string "id")))
                                (list id (nlk:json-value guild :string "name")
                                      (fetch (format nil "/guilds/~a/channels" id)))))
                me
                #'fetch
                lonely)))))

(defun probe-application (fetch)
  "The bot's app as GET /applications/@me answers it through FETCH, or NIL."
  (let ((application (funcall fetch "/applications/@me")))
    (and (execution-ok-p application) (execution-body application))))

(defun application-makers (application)
  "Who made APPLICATION (an /applications/@me body): its team's members, else
its owner, as ((ID . NAME) ...)."
  (nlk:if-let (team (nlk:json-value application :object "team"))
    (loop for member across (or (nlk:json-value team :array "members") #())
          for user = (nlk:json-value member :object "user")
          when user
            collect (cons (nlk:json-value user :string "id") (nlk:json-value user :string "username")))
    (nlk:when-let (owner (nlk:json-value application :object "owner"))
      (list (cons (nlk:json-value owner :string "id") (nlk:json-value owner :string "username"))))))

(defun message-content-line (application fetch)
  "Whether APPLICATION's bot reads message content, as a line: when its
intent is off, FETCH switches it on first (PATCH /applications/@me, the
limited flag only), and the line says which way that went."
  ;; The only write the probe makes, and the one switch an operator would
  ;; otherwise hunt for on the Bot page. Discord lets an app set its limited
  ;; intent flags itself; one in 100 servers or more needs review instead,
  ;; and Discord's refusal says so.
  (let ((flags (or (nlk:json-value application :integer "flags") 0)))
    (if (logtest flags +message-content-flags+)
        "intents: message content granted"
        (let* ((patch (funcall fetch "/applications/@me" "PATCH"
                               (nlk:json-object "flags" (logior (logand flags +limited-intent-flags+)
                                                                +message-content-limited+))))
               (now (and (execution-ok-p patch)
                         (nlk:json-value (execution-body patch) :integer "flags"))))
          (if (and now (logtest now +message-content-flags+))
              "intents: message content switched on (it was off)"
              (format nil "intents: message content not granted, and Discord would not switch it on (~a) - enable Message Content Intent on the Bot page, or keep require_mention true"
                      (if (execution-ok-p patch) "it answered without it" (probe-failure patch))))))))

(defun guild-threads-line (guild-id guild-name bot-id fetch)
  "Whether the bot may close threads in GUILD-ID, as a line, or NIL when
its roles could not be read: its server-wide permissions (@everyone's and
its roles') holding Manage Threads or Administrator."
  (let ((member (funcall fetch (format nil "/guilds/~a/members/~a" guild-id bot-id)))
        (roles (funcall fetch (format nil "/guilds/~a/roles" guild-id))))
    (when (and (execution-ok-p member) (execution-ok-p roles))
      (let* ((held (cons guild-id (coerce (or (nlk:json-value (execution-body member) :array "roles") #()) 'list)))
             (bits (reduce #'logior
                           (loop for role across (or (probe-array (execution-body roles)) #())
                                 when (member (nlk:json-value role :string "id") held :test #'equal)
                                   collect (or (parse-integer (or (nlk:json-value role :string "permissions") "0")
                                                              :junk-allowed t)
                                               0))
                           :initial-value 0)))
        (if (logtest bits (logior +administrator+ +manage-threads+))
            (format nil "threads: can close them in ~s" guild-name)
            (format nil "threads: cannot close them in ~s - give its role Manage Threads, or invite it again with the link above"
                    guild-name))))))

(defun probe-channel (section &key executor (timeout 15))
  "What the token in SECTION can see, as lines: the bot's identity and who
made it, whether it reads message content (switched on when it was off),
its invite link, each guild with its text channels and whether it may
close threads there."
  ;; EXECUTOR overrides the live one (the scripted test seam). The answer
  ;; never carries the token (the executor redacts every failure text). A
  ;; wrong token is one line naming the 401.
  (nlk:bind (((problem guilds me fetch) (probe-guilds section executor timeout)) (lines '()))
    (flet ((say (control &rest args)
             (push (apply #'format nil control args) lines)))
      (let ((application (and (execution-ok-p me) (probe-application fetch)))
            (bot-id (and (execution-ok-p me) (nlk:json-value (execution-body me) :string "id"))))
        (when (execution-ok-p me)
          (let* ((body (execution-body me))
                 (discriminator (nlk:json-value body :string "discriminator")))
            ;; A bot still on the old discriminators reads nodecode#4821.
            (say "token: ok — bot ~a~@[#~a~] (id ~a)~@[, made by ~{~a~^, ~}~]"
                 (nlk:json-value body :string "username")
                 (and (not (equal discriminator "0")) discriminator)
                 bot-id
                 (and application (mapcar #'cdr (application-makers application))))))
        (when application
          (say "~a" (message-content-line application fetch))
          (say "invite: ~a" (invite-url (or (nlk:json-value application :string "id") bot-id))))
        (when problem (say "~a" problem))
        ;; A guild's text channels read as `#name id' entries, capped at 30.
        (loop for (id name channels) in guilds
              for text = (probe-text-channels channels)
              for shown = (subseq text 0 (min (length text) 30))
              do (say "guild ~s (id ~a): ~a" name id
                      (cond ((not (execution-ok-p channels))
                             (format nil "channels: ~a" (probe-failure channels)))
                            ((null text) "no text channels")
                            (t (format nil "~{~a~^, ~}~@[, +~d more~]"
                                       (mapcar (lambda (channel)
                                                 (format nil "#~a ~a"
                                                         (nlk:json-value channel :string "name")
                                                         (nlk:json-value channel :string "id")))
                                               shown)
                                       (and (> (length text) 30) (- (length text) 30))))))
                 (nlk:when-let (line (and bot-id (guild-threads-line id name bot-id fetch)))
                   (say "~a" line)))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun probe-lonely-wait (me fetch)
  "What the operator does while the bot of ME (its /users/@me execution) is
in no server: open its invite link."
  (let* ((bot (execution-body me))
         (application (probe-application fetch)))
    (format nil "~a is in no server yet. Add it to one: open~%  ~a"
            (nlk:json-value bot :string "username")
            (invite-url (or (and application (nlk:json-value application :string "id"))
                            (nlk:json-value bot :string "id"))))))

(defun probe-channel-choices (section &key executor (timeout 15))
  "The text channels the token in SECTION can post to, as ((id . label)
...) with the server named in the label -- what a setup panel offers for
allowed_channels, so ids are picked by name. A bot in no server yet offers
none, and says to wait for it there (:WAIT, the invite link)."
  ;; Refuses (CONFIG-REFUSAL) in the probe's own words when the token is wrong.
  (multiple-value-bind (problem guilds me fetch lonely) (probe-guilds section executor timeout)
    (cond (lonely (values '() (list :wait (probe-lonely-wait me fetch))))
          (problem (config-error "~a" problem))
          (t (loop for (nil name channels) in guilds
                   append (mapcar (lambda (channel)
                                    (cons (nlk:json-value channel :string "id")
                                          (format nil "#~a  ~a"
                                                  (nlk:json-value channel :string "name")
                                                  name)))
                                  (probe-text-channels channels)))))))

(defun probe-people (guilds fetch application)
  "The people to offer by name, as ((id . label) ...): whoever made
APPLICATION first, then the people (never bots) who wrote lately in the
text channels of GUILDS: the first +PEOPLE-CHANNELS-PER-GUILD+ of each, at
most +PEOPLE-CHANNELS-CAP+ in all."
  ;; Who is in a server needs the privileged members intent; who wrote in it
  ;; needs only Read Message History, which the invite asks for.
  (let ((people (mapcar (lambda (maker) (cons (car maker) (format nil "~a  made the app" (cdr maker))))
                        (and application (application-makers application))))
        (read 0))
    (loop for (nil name channels) in guilds
          do (dolist (channel (let ((text (probe-text-channels channels)))
                                (subseq text 0 (min (length text) +people-channels-per-guild+))))
               (when (>= read +people-channels-cap+) (return))
               (incf read)
               (let ((messages (funcall fetch (format nil "/channels/~a/messages?limit=50"
                                                      (nlk:json-value channel :string "id")))))
                 (when (execution-ok-p messages)
                   (loop for message across (or (probe-array (execution-body messages)) #())
                         for author = (nlk:json-value message :object "author")
                         for id = (and author (nlk:json-value author :string "id"))
                         when (and id (not (nlk:json-value author :boolean "bot"))
                                   (not (assoc id people :test #'equal)))
                           do (setf people (append people
                                                   (list (cons id (format nil "~a  ~a"
                                                                          (nlk:json-value author :string "username")
                                                                          name))))))))))
    people))

(defun probe-people-choices (section default executor timeout)
  "PROBE-PEOPLE for the token in SECTION, with whoever made the app as the
:DEFAULT when DEFAULT is true."
  (multiple-value-bind (problem guilds me fetch lonely) (probe-guilds section executor timeout)
    (when (and problem (not lonely)) (config-error "~a" problem))
    (let ((application (probe-application fetch)))
      (values (probe-people guilds fetch application)
              (and default application
                   (list :default (mapcar #'car (application-makers application))))))))

(defun probe-owner-choices (section &key executor (timeout 15))
  "Who may own the bot, by name (PROBE-PEOPLE), whoever made the app the
default an unset owner takes."
  (probe-people-choices section t executor timeout))

(defun probe-user-choices (section &key executor (timeout 15))
  "Who may drive the bot, by name (PROBE-PEOPLE)."
  (probe-people-choices section nil executor timeout))

(defun probe-role-choices (section &key executor (timeout 15))
  "The roles of the bot's servers a member may drive it by, as ((id .
label) ...): every role but @everyone and the ones an integration manages."
  (multiple-value-bind (problem guilds me fetch lonely) (probe-guilds section executor timeout)
    (declare (ignore me))
    (when (and problem (not lonely)) (config-error "~a" problem))
    (loop for (id name) in guilds
          for roles = (funcall fetch (format nil "/guilds/~a/roles" id))
          when (execution-ok-p roles)
            append (loop for role across (or (probe-array (execution-body roles)) #())
                         for role-id = (nlk:json-value role :string "id")
                         unless (or (equal role-id id) (nlk:json-value role :boolean "managed"))
                           collect (cons role-id (format nil "@~a  ~a" (nlk:json-value role :string "name") name))))))

;;; The declaration. Types and constraints are what START-CHANNEL reads and
;;; RESOLVE-CHANNEL-SECRET / REQUIRE-NON-EMPTY-ALLOWLIST enforce; the kit
;;; refuses on SECTION-PROBLEMS before either runs, so the refusal, the
;;; panel and the primer say the same thing in the same words.
(nlk:define-section ("channels" "discord")
  ;; Two steps on Discord's side, all a token cannot do for itself; the
  ;; check does the rest: Message Content switched on, and an invite link
  ;; asking for +INVITE-PERMISSIONS+ and the slash commands.
  (:guide "discord.com/developers/applications: New Application, and give it a name; on its Bot page: Reset Token, then Copy - nothing else on Discord's side: the check below switches on Message Content and makes the invite link")
  (:check #'probe-channel)
  (:one-of "bot_token_env" "bot_token_file")
  (:any-of "allowed_channels" "allowed_users" "allowed_roles")
  ("bot_token_env" :env :doc "environment variable holding the bot token")
  ("bot_token_file" :path :doc "file holding the bot token")
  ("allowed_channels" :list :doc "channel ids the bot answers in"
                      :choices #'probe-channel-choices)
  ("owner" :list :doc "the operator's user id, whose word is standing policy in the room"
           :choices #'probe-owner-choices)
  ("allowed_users" :list :doc "user ids allowed to drive the bot"
                   :choices #'probe-user-choices)
  ("allowed_roles" :list :doc "role ids: a server member holding any one may drive the bot as a listed user does (in the server; a direct message carries no roles)"
                   :choices #'probe-role-choices)
  ("allow_bots" :choice :options '("none" "mentions" "all") :default "none"
                :doc "other bots' messages: none ignores them, mentions answers one that @mentions this bot, all treats them as people's; the bot's own are always ignored")
  ("pairing" :boolean :default t
             :doc "a direct message from someone not allowed is answered with a pairing code, and the operator is told; /channels pair CODE lets them in, /channels unpair ID takes it back")
  ("allowed_guilds" :list :doc "server ids; empty admits every server the bot is in")
  ("require_mention" :boolean :default t
                     :doc "default for channels not listed below: answer when @mentioned, replied to, or in a DM; false answers every unlisted channel and needs the Message Content intent")
  ("free_response_channels" :list
                            :choices #'probe-channel-choices
                            :doc "channel ids where every allowed human message may open a turn")
  ("require_mention_channels" :list
                               :choices #'probe-channel-choices
                               :doc "channel ids where a message must mention or reply to the bot")
  ("ignored_channels" :list
                       :choices #'probe-channel-choices
                       :doc "channel ids the bot reads neither as prompts nor room context")
  ("mention_patterns" :list
                       :doc "case-insensitive wake words that address the bot in mention-gated channels")
  ("other_addressees" :list
                       :doc "names other than the bot's that a message may open by addressing (\"ada, ...\"); such a message is observed, never answered")
  ("dm_policy" :choice :options '("allow" "disabled") :default "allow"
               :doc "direct messages")
  ("group_policy" :choice :options '("allow" "disabled") :default "allow"
                  :doc "group direct messages")
  ("reactions" :boolean :default :false
               :doc "mark each ask on the message itself: 👀 from admission through the working turn, then ✅ when its answer lands or ❌ when it fails; needs Add Reactions")
  ("stream" :boolean :default t
            :doc "the model's words reach the room as it writes them, a message edited as it grows; the answer still posts fresh, pinging its asker, and its draft goes; false, each round's words arrive whole")
  ("voice_replies" :choice :options '("off" "on" "tts") :default "off"
                   :doc "an answer comes as a voice message too, below its words: on, to an ask said in a voice message; tts, to every ask; off, never; /voice on, tts or off sets it for one room")
  ("turn_budget_minutes" :integer :default 0
                        :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("answer_words" :integer :default 0
                 :doc "the most words one answer posts, code blocks aside: the lane is told so, and a longer answer is cut down by its own model before it posts, the whole one attached as a file; 0, the default, caps nothing")
  ("room_tokens" :integer :default 40000
                :doc "the most history the room keeps, in estimated tokens (the provider counts about a quarter more); past it the older half is evicted, so every ask opens on at most this much; 0 keeps everything")
  ("catch_up_minutes" :integer :default 60
                     :doc "after the bot was away — a restart, a machine that was off — read what the channels said in the last this-many minutes of it and answer the asks it missed, each marked as late; 0 never")
  ("soul_file" :path :doc "a SOUL.md whose text is the room's standing persona")
  ("board_channel" :string
                   :choices #'probe-channel-choices
                   :doc "a channel the bot keeps one message in, edited in place: every open thread it started, by who moves next (needs you, no answer, running, done); a done thread closes after 48 hours quiet, any other after 7 days")
  ;; Voice needs none of these: /voice join sits beside whoever typed it.
  ;; Discord requires its end-to-end encryption (DAVE) for voice now, so the
  ;; first join fetches libdave into the cache — see discord/dave.lisp.
  ("voice_channel_id" :string
                      :doc "a voice channel of the bot's own: voice_autojoin sits in it, and /voice join typed by someone in no voice channel; absent, the bot sits only beside whoever asks or whom it follows")
  ("voice_text_channel_id" :string
                           :choices #'probe-channel-choices
                           :doc "the channel the voice lane talks through — its transcript, status and answers; defaults to the voice channel's own chat")
  ("voice_speakers" :list
                    :doc "user ids whose speech opens a turn; empty falls back to allowed_users, then owner, then whoever typed /voice join")
  ("voice_autojoin" :boolean :default :false
                    :doc "sit in the voice channel whenever somebody is in it: join when the first person comes in, leave when the last one goes; false waits for /voice join")
  ("voice_follow" :list
                  :doc "user ids the bot follows into voice: it joins the channel the first of them sits in, moves when they move, and leaves when they leave")
  ("voice_idle_minutes" :integer :default 5
                        :doc "a seat /voice join took is given up after this many minutes with nobody speaking to the bot; 0 stays"))
