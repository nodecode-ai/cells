;;;; operator.lisp --- what the operator sets for the rooms: per channel, who
;;;; is let in, and where the bot reports.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four controls, each Hermes had and a room lacked (2026-09-28):
;;;;
;;;;   agents, routes    channels.<id>.agents names the agents a room's asks
;;;;                     may run as — each its folder, persona, standing
;;;;                     prompt, model and skills — and channels.<id>.routes
;;;;                     sends asks to one by server, channel, thread, person
;;;;                     or role; /agent typed in a room hands the room to one
;;;;                     (2026-10-03, after Hermes' profile routes and
;;;;                     OpenClaw's bindings).
;;;;   the room's model  /models typed in a room sets that room's model and
;;;;                     no other: every lane forked there runs on it, a
;;;;                     thread's on its channel's unless it has its own. The
;;;;                     organism's default, which the shells' /models moves,
;;;;                     stays where it was — before, a pick in a room moved it
;;;;                     for the whole organism, the operator's terminal
;;;;                     included.
;;;;   pairing           a direct message from someone no allowlist names is
;;;;                     answered with a pairing code; the person gives it to
;;;;                     the operator, and /channels pair CODE lets them in.
;;;;                     The code is never in a notice or a log: it is what
;;;;                     proves the person asking the operator is the one who
;;;;                     wrote to the bot (Hermes' rule). The web page lists
;;;;                     the asks without it and approves one by its id, as
;;;;                     Hermes' admin page does (PAIRING-JSON).
;;;;   the home channel  /sethome typed in a room makes it where the bot
;;;;                     reports to the operator: that it is back after a
;;;;                     restart, the release it runs once it is new, who
;;;;                     asked to be let in, who was refused.
;;;;
;;;; The room's agent and model, the paired people and the home channel are
;;;; kept in the host's own state session (HANDOFF-SESSION), so a restart
;;;; keeps them; the agents and routes are config, read at each start.

(in-package #:nodecode-channel-kit)

;;; --- agents and routes ----------------------------------------------------------------------
;;; An agent is what a room's asks run as, short of a whole other organism:
;;; the folder its lanes work in — their tools' directory, the AGENTS.md they
;;; read, the project memory they keep — its persona, its standing prompt, its
;;; model and the skills it reads first. This process runs every one, as
;;; Hermes' multiplexed profiles and OpenClaw's agents run in theirs; a whole
;;; other home is a profile, an organism of its own with a bot of its own.
;;;
;;; A route sends asks to an agent by where they were said and who said them:
;;; a server (guild), a channel and its threads, one thread, a person (user),
;;; a role. Every key a route names must match, and the most specific route
;;; that does wins: person, then thread, channel, role, server, each
;;; outweighing every key below it together — Hermes' additive rule
;;; (gateway/profile_routing.py), with roles where OpenClaw ranks them, above
;;; the server and below the channel — and a tie goes to the route written
;;; first. An ask no route takes runs as the channel itself: its soul_file,
;;; no standing prompt, the organism's model, the room's folder.

(defparameter +agent-keys+ '("folder" "soul_file" "prompt" "provider" "model" "skills")
  "What an agents entry may name.")

(defparameter +route-keys+ '(("user" :user 16) ("thread" :thread 8) ("channel" :channel 4)
                             ("role" :role 2) ("guild" :guild 1))
  "What a route may match on, as (KEY KEYWORD WEIGHT): a route's specificity
is the sum of the weights of the keys it names.")

(defparameter +own-agent+ "default"
  "The agent name that names none: the channel itself.")

(defun read-agents (channel-id section)
  "channels.<CHANNEL-ID>.agents in SECTION as ((NAME . PLIST) ...), each PLIST
(:folder :soul-path :prompt :provider :model :skills); a malformed entry is a
CONFIG-REFUSAL naming it."
  (let ((table (and (hash-table-p section) (gethash "agents" section))))
    (cond ((null table) '())
          ((not (hash-table-p table))
           (config-error "channels.~a.agents is an object keyed by agent name" channel-id))
          (t (loop for name being the hash-keys of table using (hash-value entry)
                   collect (cons name (agent-plist channel-id name entry)))))))

(defun agent-plist (channel-id name entry)
  "One agents ENTRY, under NAME, as a plist."
  (flet ((refuse (control &rest arguments)
           (config-error "channels.~a.agents.~a: ~?" channel-id name control arguments)))
    ;; One word, so /agent NAME can say it.
    (when (or (string-equal name +own-agent+) (not (ppcre:scan "^[\\w.-]+\\z" name)))
      (refuse "an agent's name is one word, and not ~a" +own-agent+))
    (unless (hash-table-p entry)
      (refuse "an object of ~{~a~^, ~}" +agent-keys+))
    (loop for key being the hash-keys of entry
          unless (member key +agent-keys+ :test #'string=)
            do (refuse "no setting ~s; one of ~{~a~^, ~}" key +agent-keys+))
    (let ((provider (config-string entry "provider"))
          (model (config-string entry "model"))
          (folder (config-string entry "folder")))
      ;; A provider alone would run the organism's model under another lane.
      (when (and provider (not model))
        (refuse "a provider needs its model"))
      (list :folder (and folder
                         ;; A lane in a folder that is not there fails its
                         ;; every call, so the start refuses it instead.
                         (handler-case (nlk:check-session-cwd folder)
                           (error (condition) (refuse "folder: ~a" condition))))
            :soul-path (config-string entry "soul_file")
            :prompt (config-string entry "prompt")
            :provider provider
            :model model
            :skills (config-string-list entry "skills")))))

(defun read-routes (channel-id section agents)
  "channels.<CHANNEL-ID>.routes in SECTION as plists (:agent :weight and the
keys each matches on), the most specific first and ties in the order
written. :AGENT is a name of AGENTS, or NIL for the channel itself; a route
that names no agent of AGENTS, or nothing to match, is a CONFIG-REFUSAL
naming it."
  (let ((routes (and (hash-table-p section) (gethash "routes" section))))
    (cond ((null routes) '())
          ((not (vectorp routes))
           (config-error "channels.~a.routes is a list of routes" channel-id))
          (t (stable-sort (loop for route across routes
                                for index from 0
                                collect (route-plist channel-id index route agents))
                          #'> :key (lambda (route) (getf route :weight)))))))

(defun route-plist (channel-id index route agents &aux (keys (mapcar #'first +route-keys+)))
  "One ROUTES entry, the INDEXth, as a plist."
  (flet ((refuse (control &rest arguments)
           (config-error "channels.~a.routes[~d]: ~?" channel-id index control arguments)))
    (unless (hash-table-p route)
      (refuse "an object of an agent and one or more of ~{~a~^, ~}" keys))
    (loop for key being the hash-keys of route
          unless (or (string= key "agent") (member key keys :test #'string=))
            do (refuse "no key ~s; agent, and one or more of ~{~a~^, ~}" key keys))
    (let ((agent (config-string route "agent"))
          (matches '())
          (weight 0))
      (unless (or (equal agent +own-agent+) (assoc agent agents :test #'string=))
        (refuse "~:[names no agent~;~:*no agent ~s~]; channels.~a.agents defines ~:[none~;~:*~{~a~^, ~}~], ~
                 and ~s is the channel itself"
                agent channel-id (mapcar #'car agents) +own-agent+))
      (loop for (key keyword key-weight) in +route-keys+
            for value = (gethash key route)
            when value
              do (unless (stringp value)
                   ;; A Discord id is past what a JSON number holds exactly.
                   (refuse "~a is an id, written as a string" key))
                 (setf matches (list* keyword (config-string route key) matches))
                 (incf weight key-weight))
      (when (zerop weight)
        (refuse "matches nothing: name one or more of ~{~a~^, ~}" keys))
      (list* :agent (if (equal agent +own-agent+) nil agent) :weight weight matches))))

(defun route-matches-p (route source target)
  "Whether every key ROUTE names matches an ask said at TARGET by the person
SOURCE — a candidate's source, or NIL where nobody is known — describes."
  ;; A thread's TARGET names its channel as the parent's, so a channel's
  ;; route takes its threads.
  (flet ((said (key) (and (hash-table-p source) (gethash key source))))
    (loop for (key value) on route by #'cddr
          always (ecase key
                   ((:agent :weight) t)
                   (:user (equal value (said "user_id")))
                   (:thread (equal value (getf target :thread-id)))
                   (:channel (equal value (getf target :channel-id)))
                   (:role (let ((roles (said "role_ids")))
                            (and (typep roles 'sequence) (find value roles :test #'equal))))
                   (:guild (equal value (said "workspace_id")))))))

(defun route-text (route)
  "ROUTE in words: the keys it matches on."
  (format nil "~{~a~^ and ~}"
          (loop for (key keyword) in +route-keys+
                for value = (getf route keyword)
                when value collect (format nil "~a ~a" key value))))

(defun agent-of (host name)
  "HOST's agent NAME as its plist, or NIL: the channel itself."
  (and name (cdr (assoc name host.agents :test #'string=))))

(defun agent-standing (agent noun)
  "What AGENT — an agent's plist, or NIL for the channel itself — sets for
the lanes it runs as the lane contract's last paragraph — its prompt, then
the skills to view — or NIL when it sets neither."
  ;; Standing, not live: it is a fact of the room every lane there shares, so
  ;; it rides with the contract ahead of the history (LANE-CONTRACT).
  (let ((prompt (getf agent :prompt))
        (skills (getf agent :skills)))
    (when (or prompt skills)
      (with-output-to-string (out)
        (when prompt
          (format out "The operator's standing instructions for this ~a, which bind every ask in it:~%~a"
                  noun prompt))
        (when (and prompt skills) (format out "~%~%"))
        (when skills
          (format out "The operator bound these skills to this ~a: ~{~a~^, ~}. Read each — ~
                       (help 'NAME) — before you answer the first ask here, and follow it."
                  noun skills))))))

;;; --- what a room set for itself: its agent, its model, its voice ---------------------------

(defparameter +room-agents-key+ "room-agents"
  "The state key the agents /agent handed rooms to are kept under.")

(defparameter +room-models-key+ "room-models"
  "The state key the models /models set in rooms are kept under.")

(defparameter +room-efforts-key+ "room-efforts"
  "The state key the reasoning effort /think sets in rooms are kept under.")

(defparameter +room-voices-key+ "room-voices"
  "The state key the voice replies /voice set in rooms are kept under.")

(defvar *room-settings-lock* (bt2:make-lock :name "channel-room-settings")
  "Serializes a read-change-write of a host's room settings.")

(defun room-settings (host key)
  "HOST's rooms' settings kept under the state KEY, as a hash: room session
id -> value."
  (or (and (nlk:store-open-p)
           (nlk:when-let (json (nlk:session-state-get (handoff-session host) key))
             (let ((table (nlk:decode-json json))) (and (hash-table-p table) table))))
      (make-hash-table :test #'equal)))

(defun set-room-setting (host key room value)
  "Keep VALUE as ROOM's setting under KEY; VALUE NIL clears ROOM's."
  (bt2:with-lock-held (*room-settings-lock*)
    (let ((table (room-settings host key)))
      (if value (setf (gethash room table) value) (remhash room table))
      (nlk:session-state-put (handoff-session host) key (nlk:encode-json-object table)))))

(defun agent-for (host room target source)
  "(values NAME WHY ROUTE): the agent asks in ROOM at TARGET, said by the
person SOURCE describes, run as — the one /agent handed ROOM to (WHY :room),
else the one it handed the channel room a thread hangs in (WHY :parent),
else the most specific route that matches (WHY :route, ROUTE it) — or NIL,
the channel itself."
  ;; A room handed to an agent the config no longer defines goes back to the
  ;; routes.
  (loop with set = (room-settings host +room-agents-key+)
        for (room why) in (list (list room :room) (list (room-parent-session-id host target) :parent))
        for name = (and room (gethash room set))
        when (agent-of host name)
          do (return-from agent-for (values name why)))
  (nlk:when-let (route (find-if (lambda (route) (route-matches-p route source target)) host.routes))
    (values (getf route :agent) :route route)))

(defun agent-command (args room &aux (words (nlk:split-words (or args ""))))
  "/agent ARGS typed in ROOM: which agent answers there and why; NAME hands
ROOM to that agent, default gives it back to the routes."
  ;; Handing a room over retires its lanes and its threads', as a head move
  ;; does: a lane works in its agent's folder, and a direct message's next
  ;; line would otherwise go on with the agent it left.
  (multiple-value-bind (host target) (room-host room)
    (unless host (error "~a is no room of a running channel" room))
    (agent-report host room target words)))

(defun agent-choices (text room)
  "The /agent argument completions for the tail TEXT typed in ROOM: its
channel's agents in the order the config names them, each beside its folder,
then default — none outside a channel's room."
  (nlk:when-let (host (room-host room))
    (loop with needle = (nlk:trimmed text)
          for (name . agent) in (append host.agents (list (list +own-agent+)))
          when (search needle name :test #'char-equal)
            collect (list :name (cond ((equal name +own-agent+) "default — back to the routes")
                                      ((getf agent :folder)
                                       (format nil "~a — ~a" name (getf agent :folder)))
                                      (t name))
                          :value name))))

(defun agent-report (host room target words)
  "AGENT-COMMAND's answer to WORDS in HOST's ROOM at TARGET."
  (let ((here (if (getf target :thread-id) "thread" "channel"))
        (names (mapcar #'car host.agents)))
    (flet ((who (name) (if name (format nil "agent ~a" name) "the channel itself"))
           (hand (name)
             (set-room-setting host +room-agents-key+ room name)
             (retire-lanes host (lambda (parent)
                                  (or (equal room parent)
                                      (equal room (room-parent-session-id
                                                   host (session-target (platform-session-prefix host.platform)
                                                                        parent)))))))
           (now () (agent-for host room target *command-source*)))
      (cond
        ((null words)
         (multiple-value-bind (name why route) (now)
           (format nil "this ~a is answered by ~a~a~%~:[no agents: channels.~a.agents defines them~;~
                        ~:*agents: ~{~a~^, ~}; /agent NAME hands this ~a to one~]"
                   here (who name)
                   (case why
                     (:room ", handed to it with /agent; /agent default gives it back")
                     (:parent ", as its channel was handed with /agent")
                     (:route (format nil ", by the route on ~a" (route-text route)))
                     (t ""))
                   names (if names here (host-id host)))))
        ((rest words) "usage: /agent [NAME | default]")
        ((string-equal (first words) +own-agent+)
         (hand nil)
         (format nil "this ~a is answered by ~a from its next ask" here (who (now))))
        ((null (agent-of host (first words)))
         (error "no agent ~a; ~:[channels.~a.agents defines none~;~:*the agents are ~{~a~^, ~}~]"
                (first words) names (host-id host)))
        (t (hand (first words))
           (format nil "this ~a is answered by ~a from its next ask" here (who (first words))))))))

(defun room-model (host room)
  "(values PROVIDER MODEL) /models set in ROOM, or NIL."
  (nlk:when-let (entry (and room (gethash room (room-settings host +room-models-key+))))
    (values (nlk:json-value entry :string "provider") (nlk:json-value entry :string "model"))))

(defun set-room-model (host room provider model)
  "Make ROOM run on PROVIDER's MODEL; MODEL NIL clears ROOM's."
  (set-room-setting host +room-models-key+ room
                    (and model (nlk:json-object "provider" provider "model" model))))

(defun room-effort (host room)
  "ROOM's reasoning-effort pick, or NIL when it follows the provider default."
  (and room (gethash room (room-settings host +room-efforts-key+))))

(defun set-room-effort (host room effort)
  "Make ROOM use EFFORT from its next ask; NIL follows the provider default."
  (set-room-setting host +room-efforts-key+ room effort))

(defun lane-effort (host room target)
  "ROOM's effort pick, inherited by a thread from its channel, or NIL."
  (loop for candidate in (list room (room-parent-session-id host target))
        for effort = (room-effort host candidate)
        when effort do (return effort)))

(defun effort-target (host room target agent)
  "(values PROVIDER MODEL LADDER): the target ROOM's next lane binds."
  (multiple-value-bind (provider model) (lane-model host room target agent)
    (unless model
      (multiple-value-setq (provider model) (nle:gateway-target nil)))
    (values provider model (nle::effort-ladder provider model))))

(defun apply-lane-effort (host ask)
  "Apply ASK's room effort after its room model has been applied."
  (let ((effort (lane-effort host ask.room ask.target)))
    (multiple-value-bind (provider model) (nlk:session-model-selection ask.lane)
      (unless (equal effort (nlk:session-model-effort ask.lane))
        (nlk:record-session-model-selection ask.lane
                                            :provider provider
                                            :model model
                                            :effort effort)))))

(defun lane-model (host room target agent)
  "(values PROVIDER MODEL WHY) a lane of ROOM at TARGET, run as AGENT (a
plist, or NIL), runs on: what /models set in ROOM, else in the channel room a
thread hangs in (WHY :room), else AGENT's model (WHY :agent), else NIL — the
organism's default."
  (dolist (room (list room (room-parent-session-id host target)))
    (multiple-value-bind (provider model) (room-model host room)
      (when model (return-from lane-model (values provider model :room)))))
  (nlk:when-let (model (getf agent :model))
    (values (getf agent :provider) model :agent)))

(defun apply-lane-model (host ask)
  "Pin ASK's lane to the model its room runs on (LANE-MODEL), or clear its
pin to follow the organism's default, when its pin says otherwise."
  ;; A lane is a session of its own, forked from the room, and a pin does not
  ;; ride a fork: the lane carries it. A lane still standing by holds the pin
  ;; until the ask's admission makes it durable, which is before its turn
  ;; binds a model.
  (multiple-value-bind (provider model) (lane-model host ask.room ask.target (agent-of host ask.agent))
    (multiple-value-bind (pinned-provider pinned-model) (nlk:session-model-selection ask.lane)
      (unless (and (equal provider pinned-provider) (equal model pinned-model))
        (nlk:record-session-model-selection ask.lane :provider provider :model model)))))

(defun lane-voice-replies (host room target)
  "Whether a lane of ROOM at TARGET answers in a voice message too, as a key
of +VOICE-REPLIES+: what /voice set in ROOM, else in the channel room a
thread hangs in, else the section's voice_replies."
  (or (loop with set = (room-settings host +room-voices-key+)
            for room in (list room (room-parent-session-id host target))
            thereis (and room (gethash room set)))
      host.voice-replies))

(defun room-voice-replies (room)
  "The key of +VOICE-REPLIES+ ROOM — a room session of a running channel's —
answers with (LANE-VOICE-REPLIES), or NIL for a session no channel holds."
  (multiple-value-bind (host target) (room-host room)
    (and host (lane-voice-replies host room target))))

(defun set-voice-replies (room mode &aux (host (room-host room)))
  "Make ROOM — a room session of a running channel's — answer in voice
messages as MODE, a key of +VOICE-REPLIES+, says. => the line that says so."
  ;; Kept even when it is what the section says: the room chose it, and a
  ;; section changed later does not change the room's mind.
  (let ((meaning (cdr (assoc mode +voice-replies+ :test #'equal))))
    (cond ((null meaning)
           (error "voice replies are ~{~a~^, ~}, not ~s" (mapcar #'car +voice-replies+) mode))
          ((null host) (error "~a is no room of a running channel" room))
          (t (set-room-setting host +room-voices-key+ room mode)
             (format nil "voice: ~a." meaning)))))

(defun model-provider (model &aux (found (loop for (provider . nil) in (nle:models)
                                                   when (member model (ignore-errors (nle:models provider))
                                                                :test #'string=)
                                                     collect provider)))
  "The configured provider that lists MODEL, or a refusal naming why none
does: no provider lists it, or more than one does."
  (cond ((null found)
         (error "no configured provider lists ~a; /models list shows the catalog" model))
        ((rest found)
         (error "~a is served by ~{~a~^ and ~}; name one: /models ~a ~a"
                model found (first found) model))
        (t (first found))))

(defun checked-pick (provider model)
  "PROVIDER, confirmed to be configured and to list MODEL — its listing
unreachable, the model is taken on its word — or a refusal saying why."
  (multiple-value-bind (ids listing-error) (nle:models provider)
    (unless (or listing-error (member model ids :test #'string=))
      (error "~a lists no model ~a; /models list ~a shows its ~d" provider model provider (length ids)))
    provider))

(defun room-models-command (host candidate args room &aux (words (nlk:split-words (or args "")))
                                                         (target (channel-target candidate))
                                                         (agent (agent-of host (agent-for host room target
                                                                                          (candidate-source candidate))))
                                                         (here (if (getf target :thread-id) "thread" "channel"))
                                                         (cards (platform-choices host.platform)))
  "Worker thread: the room's /models ARGS — the model ROOM runs on, or a
pick of one for ROOM alone; where the platform's messages carry choices, a
picker card, (values TEXT CONTROLS)."
  ;; The catalog is the organism's: `list' and `aux' answer as the shells'
  ;; /models does (NLE:SLASH). A pick names a model, or a provider and a
  ;; model; `default' goes back to what the room ran on before one. With
  ;; cards, bare /models is the picker's first view and a lone provider its
  ;; second (MODELS-CARD), `page N' after either the page a menu shows.
  (flet ((page (word) (or (and word (parse-integer word :junk-allowed t)) 1))
         (paged-p (tail) (or (null tail)
                             (and (string-equal (first tail) "page") (rest tail) (null (cddr tail))))))
    (cond
      ((and words (member (first words) '("list" "aux" "auxiliary") :test #'string-equal))
       (nle:slash (format nil "/models ~a" args) :session-id room))
      ((and cards (paged-p words))
       (providers-card host room target agent (page (second words))))
      ((null words)
       (multiple-value-bind (provider model why) (lane-model host room target agent)
         (if model
             (format nil "this ~a runs ~@[~a/~]~a, ~:[set by its agent~;set with /models~]; ~
                          /models <provider> <model> changes it, /models default goes back"
                     here provider model (eq why :room))
             ;; The organism's report ends saying how a pick is made, and here
             ;; a pick is this room's.
             (format nil "this ~a runs the organism's default:~%~a" here (nle:slash "/models" :session-id room)))))
      ((and (null (rest words)) (member (first words) '("default" "reset") :test #'string-equal))
       (set-room-model host room nil nil)
       (multiple-value-bind (provider model) (lane-model host room target agent)
         (format nil "this ~a runs ~:[the organism's default~;~:*~@[~a/~]~a~] from its next ask"
                 here provider model)))
      ((and (paged-p (rest words)) (assoc (first words) (nle:models) :test #'string-equal))
       (if cards
           (models-card host room target agent (first words) (page (third words)))
           (nle:slash (format nil "/models list ~a" (first words)) :session-id room)))
      ((rest (rest words)) "usage: /models [provider] [model] | default | list [provider]")
      (t
       (let* ((model (first (last words)))
              (provider (if (rest words) (checked-pick (first words) model) (model-provider model))))
         (set-room-model host room provider model)
         (format nil "this ~a runs ~a/~a from its next ask; the organism's default is unchanged"
                 here provider model))))))

;;; --- the effort picker (a /think card) ---------------------------------------------------

(defun room-effort-command (host candidate args room
                            &aux (words (nlk:split-words (or args "")))
                                  (target (channel-target candidate))
                                  (agent (agent-of host (agent-for host room target
                                                                    (candidate-source candidate))))
                                  (here (if (getf target :thread-id) "thread" "channel"))
                                  (cards (platform-choices host.platform)))
  "Worker thread: the room's /think ARGS — a reasoning effort for ROOM alone,
or a picker card where the platform carries choices."
  (multiple-value-bind (provider model ladder) (effort-target host room target agent)
    (let ((current (lane-effort host room target)))
      (cond
        ((and cards (null words))
         (effort-card host room provider model ladder current))
        ((null words)
         (format nil "this ~a uses ~a/~a at ~a~%/think <rung> changes it; /think default follows the provider"
                 here provider model (or current "provider default")))
        ((and (null (rest words))
              (member (first words) '("default" "reset") :test #'string-equal))
         (set-room-effort host room nil)
         (format nil "this ~a follows ~a/~a's provider default from its next ask"
                 here provider model))
        ((rest words)
         "usage: /think [off | minimal | low | medium | high | xhigh | max | default]")
        ((null ladder)
         (format nil "~a/~a offers no reasoning control" provider model))
        ((not (member (first words) ladder :test #'string-equal))
         (error "~s is not a reasoning effort ~a/~a offers; the ladder: ~{~a~^ ~}"
                (first words) provider model ladder))
        (t
         (set-room-effort host room (first words))
         (format nil "this ~a uses ~a/~a at ~a from its next ask; the organism's default is unchanged"
                 here provider model (first words)))))))

(defun effort-card (host room provider model ladder current)
  "(values TEXT CONTROLS): the room's reasoning-effort picker."
  (declare (ignore host room))
  (values (format nil "## Reasoning Effort~%Target: ~a/~a~%Current: ~a~%Select the effort for this room's next asks."
                  provider model (or current "provider default"))
          (list (list :menu "Select effort"
                      (loop for rung in ladder
                            collect (menu-choice rung (format nil "/think ~a" rung)
                                                 :description (if (equal rung "off")
                                                                  "Answer without reasoning."
                                                                  "")
                                                 :current (equal rung current))))
                (choice "Provider default" "/think default"
                        :disabled (null current)))))

;;; --- the model picker (a /models card) ---------------------------------------------------
;;; Where a platform's messages carry choices, bare /models answers with a
;;; card after OpenClaw's Discord picker (the operator, 2026-10-03: "they have
;;; a better UI"): a menu of the providers, then of one provider's models,
;;; with a way back and a way to the default. Every step is a CHOICE saying a
;;; /models line — /models PROVIDER, /models PROVIDER MODEL, /models default —
;;; so a press is that command typed, admitted as typed, and its answer takes
;;; the card's place. OpenClaw's Submit step is not copied: picking a model
;;; picks it. A menu shows 25; a longer list pages.

(defparameter +picker-page-size+ 25
  "The most options one picker menu shows, a Discord select's own cap.")

(defun picker-page (items page)
  "(values SHOWN PAGE PAGES): the PAGEth run of +PICKER-PAGE-SIZE+ ITEMS, PAGE
clamped to the PAGES there are."
  (let* ((pages (max 1 (ceiling (length items) +picker-page-size+)))
         (page (min (max 1 page) pages))
         (start (* (1- page) +picker-page-size+)))
    (values (subseq items start (min (length items) (+ start +picker-page-size+))) page pages)))

(defun picker-pager (line page pages)
  "The row that pages a picker's menu — back, where it is, on — each a choice
saying LINE with the page it goes to; NIL for a menu one page holds."
  (when (> pages 1)
    (list (list (choice "Prev" (format nil "~a page ~d" line (1- page)) :disabled (= page 1))
                (list (format nil "Page ~d/~d" page pages) "nck:page" :secondary t)
                (choice "Next" (format nil "~a page ~d" line (1+ page)) :disabled (= page pages))))))

(defun picker-current (host room target agent)
  "(values PROVIDER MODEL): the model ROOM's next ask, run as AGENT, runs on —
LANE-MODEL's pick, else the organism's default."
  (multiple-value-bind (provider model) (lane-model host room target agent)
    (if model
        (values provider model)
        (multiple-value-bind (provider model) (nle:gateway-target nil)
          (values provider model)))))

(defun providers-card (host room target agent page &aux (providers (nle:models)))
  "(values TEXT CONTROLS): the picker's first view — the model ROOM runs on,
and a menu of the providers, each saying /models PROVIDER."
  (multiple-value-bind (current-provider current-model) (picker-current host room target agent)
    (multiple-value-bind (shown page pages) (picker-page providers page)
      (values (format nil "## Model Picker~%Current model: ~@[~a/~]~a~%Select a provider (~d available)."
                      current-provider current-model (length providers))
              (list* (list :menu "Select provider"
                           (loop for (provider . count) in shown
                                 collect (menu-choice provider (format nil "/models ~a" provider)
                                                      :description (format nil "~d model~:p" count)
                                                      :current (equal provider current-provider))))
                     (picker-pager "/models" page pages))))))

(defun models-card (host room target agent provider page)
  "(values TEXT CONTROLS): the picker's second view — PROVIDER's models, each
saying /models PROVIDER MODEL, then the way back to the providers and the way
to the default, which only a room that picked its own can take."
  (multiple-value-bind (ids listing-error) (nle:models provider)
    (multiple-value-bind (current-provider current-model) (picker-current host room target agent)
      (multiple-value-bind (default-provider default-model) (nle:gateway-target nil)
        (multiple-value-bind (shown page pages) (picker-page ids page)
          (values (format nil "## Model Picker~%Current model: ~@[~a/~]~a~%Default: ~a/~a~%~
                               Select a ~a model~@[ (~a)~].~@[~%listing: ~a~]"
                          current-provider current-model default-provider default-model provider
                          (and (> pages 1) (format nil "page ~d/~d, ~d models" page pages (length ids)))
                          listing-error)
                  (append (list (list :menu (format nil "Select ~a model" provider)
                                      (loop for id in shown
                                            collect (menu-choice id (format nil "/models ~a ~a" provider id)
                                                                 :current (and (equal id current-model)
                                                                               (equal provider current-provider))))))
                          (picker-pager (format nil "/models ~a" provider) page pages)
                          (list (list (choice "Providers" "/models")
                                      ;; ROOM's own pick: a thread running its
                                      ;; channel's has nothing to reset.
                                      (choice "Reset to default" "/models default"
                                              :disabled (null (nth-value 1 (room-model host room)))))))))))))

;;; --- pairing ---------------------------------------------------------------------------------

(defparameter +pairing-alphabet+ "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"
  "What a pairing code is spelled from: no 0/O, no 1/I.")

(defparameter +pairing-code-ms+ 3600000
  "How long a pairing code stands.")

(defparameter +pairing-ask-ms+ 600000
  "How often one person may be given a code.")

(defparameter +pairing-pending-cap+ 3
  "The most codes one host has standing at once.")

(defparameter +paired-key+ "paired"
  "The state key the people the operator paired are kept under.")

(defvar *pairing-codes* (make-hash-table :test #'equal :synchronized t)
  "Code -> (:host :user :name :target :at :id): the codes standing, :ID the
handle the operator's page approves an ask by. Live-only: a restart drops
them, and the person asks again.")

(defvar *pairing-asked* (make-hash-table :test #'equal :synchronized t)
  "(host id . user id) -> when that person was last given a code.")

(defun pairing-code ()
  "Eight characters of +PAIRING-ALPHABET+ from the system's entropy."
  ;; 32 letters divide 256: each byte picks one uniformly.
  (map 'string (lambda (byte) (char +pairing-alphabet+ (mod byte 32))) (nlk:random-bytes 8)))

(defun standing-codes (host-id &aux (now (now-ms)))
  "HOST-ID's codes still standing, as (CODE . ENTRY); the expired ones go."
  (let ((standing '()))
    (sb-ext:with-locked-hash-table (*pairing-codes*)
      (loop for code being the hash-keys of *pairing-codes* using (hash-value entry)
            do (cond ((> (- now (getf entry :at)) +pairing-code-ms+) (remhash code *pairing-codes*))
                     ((equal host-id (getf entry :host)) (push (cons code entry) standing)))))
    standing))

(defun pairing-candidate-p (host candidate reason)
  "Whether CANDIDATE, rejected for REASON, is a person writing to HOST
directly whom a pairing code could let in."
  (and host.pairing-p
       (equal reason (inbound-reason host.policy :user-not-allowed))
       (equal "direct_message" (source-field candidate "chat_kind"))
       (not (candidate-bot-p candidate))
       (source-field candidate "user_id")
       t))

(defun offer-pairing (host candidate &aux (user (source-field candidate "user_id"))
                                          (name (or (source-field candidate "user_name") "someone"))
                                          (key (cons (host-id host) user))
                                          (now (now-ms)))
  "Worker thread: answer CANDIDATE, a direct message from someone not let in,
with a pairing code, and tell the operator who asked — at most once in ten
minutes a person."
  (let ((asked (gethash key *pairing-asked*)))
    (unless (and asked (< (- now asked) +pairing-ask-ms+))
      (setf (gethash key *pairing-asked*) now)
      (let ((target (channel-target candidate)))
        (if (>= (length (standing-codes (host-id host))) +pairing-pending-cap+)
            (post-message host target "The bot's operator has too many requests waiting; try again later."
                          :what "pairing refusal")
            (let ((code (pairing-code)))
              (setf (gethash code *pairing-codes*)
                    (list :host (host-id host) :user user :name name :target target :at now
                          :id (nlk:make-durable-id "ask")))
              (post-message host target
                            (format nil "I don't know you yet. Your pairing code is ~a: give it to the ~
                                         bot's operator, who can let you in with it. It lasts an hour."
                                    code)
                            :what "pairing code")
              (tell-operator host (format nil "~a: ~a (~a) wrote to the bot and asks to be let in; ~
                                               /channels pair <their code> does it"
                                          (host-id host) name user))))))))

(defun paired (host)
  "The people the operator paired on HOST, as JSON objects {id, name, at}."
  (or (and (nlk:store-open-p)
           (nlk:when-let (json (nlk:session-state-get (handoff-session host) +paired-key+))
             (coerce (nlk:decode-json json) 'list)))
      '()))

(defun save-paired (host people)
  "Keep PEOPLE as HOST's paired, and let the gate read them."
  (nlk:session-state-put (handoff-session host) +paired-key+
                         (nlk:encode-json-object (coerce people 'vector)))
  (note-paired host people))

(defun note-paired (host people)
  "Let the admission gate read PEOPLE as HOST's paired users."
  (setf (gethash (host-id host) *paired-users*)
        (mapcar (lambda (person) (nlk:json-value person :string "id")) people)))

(defun pair (code &aux (entry (gethash (string-upcase (remove #\Space (or code ""))) *pairing-codes*)))
  "Let in the person CODE was given to. => what happened, as a line."
  (let ((host (and entry (find (getf entry :host) *hosts* :key #'host-id :test #'equal))))
    (cond
      ((or (null entry) (> (- (now-ms) (getf entry :at)) +pairing-code-ms+))
       "no such pairing code standing; the person can write to the bot again for a new one")
      ((null host) (format nil "the ~a channel is not running" (getf entry :host)))
      (t
       (remhash (string-upcase (remove #\Space code)) *pairing-codes*)
       (let ((user (getf entry :user)))
         (save-paired host (append (remove user (paired host)
                                           :key (lambda (person) (nlk:json-value person :string "id"))
                                           :test #'equal)
                                   (list (nlk:json-object "id" user "name" (getf entry :name)
                                                          "at" (nlk:iso-now)))))
         (on-worker host
           (post-message host (getf entry :target) "You're in: the operator let you in. Ask away."
                         :what "pairing welcome"))
         (format nil "~a: ~a (~a) is let in" (host-id host) (getf entry :name) user))))))

(defun unpair (user)
  "Take back the pairing of USER, a user id, on every host. => a line."
  (let ((hosts (loop for host in *hosts*
                     when (find user (paired host) :key (lambda (person) (nlk:json-value person :string "id"))
                                                   :test #'equal)
                       collect host)))
    (dolist (host hosts)
      (save-paired host (remove user (paired host)
                                :key (lambda (person) (nlk:json-value person :string "id"))
                                :test #'equal)))
    (if hosts
        (format nil "~{~a~^, ~}: ~a is no longer let in" (mapcar #'host-id hosts) user)
        (format nil "nobody paired has the id ~a" user))))

(defun pairing-report ()
  "Who is paired on every host, and how many codes stand."
  (format nil "~{~a~^~%~}"
          (or (loop for host in *hosts*
                    for people = (paired host)
                    for standing = (length (standing-codes (host-id host)))
                    when (or people (plusp standing))
                      collect (format nil "~a: paired ~:[nobody~;~:*~{~a~^, ~}~]~[~:;; ~:*~d code~:p waiting~]"
                                      (host-id host)
                                      (mapcar (lambda (person)
                                                (format nil "~a (~a)" (nlk:json-value person :string "name")
                                                        (nlk:json-value person :string "id")))
                                              people)
                                      standing))
              (list "nobody is paired and no code is waiting"))))

(defun pairing-json (&aux (now (now-ms)))
  "PAIRING-REPORT as the operator's page reads it, per started host: whether a
stranger's direct message is offered a code at all, the asks standing — who
wrote, how long ago, how long the code has left, and the ask's id to approve
it by — and the people let in."
  ;; Never the code: it is the person's to bring (Hermes' admin surface lists
  ;; its asks the same way, approved by id).
  (map 'vector
       (lambda (host)
         (nlk:json-object
          "id" (host-id host)
          "on" host.pairing-p
          "asks" (map 'vector
                      (lambda (standing &aux (entry (cdr standing)) (age (- now (getf entry :at))))
                        (nlk:json-object "id" (getf entry :id) "user" (getf entry :user)
                                         "name" (getf entry :name)
                                         "age_seconds" (floor age 1000)
                                         "seconds_left" (floor (- +pairing-code-ms+ age) 1000)))
                      (sort (standing-codes (host-id host)) #'< :key (lambda (standing) (getf (cdr standing) :at))))
          "paired" (coerce (paired host) 'vector)))
       (reverse *hosts*)))

(defun ask-code (id)
  "The code of the standing ask whose id is ID, or NIL: what PAIR takes when
the operator's page approves the ask it lists."
  (sb-ext:with-locked-hash-table (*pairing-codes*)
    (loop for code being the hash-keys of *pairing-codes* using (hash-value entry)
          when (equal id (getf entry :id)) return code)))

;;; --- the home channel ---------------------------------------------------------------------

(defparameter +home-key+ "home"
  "The state key a host's home channel is kept under.")

(defun home-target (host)
  "HOST's home channel as a target plist, or NIL when none is set."
  (and (nlk:store-open-p)
       (nlk:when-let (json (nlk:session-state-get (handoff-session host) +home-key+))
         (nlk:json-plist (nlk:decode-json json)))))

(defun set-home (host target)
  "Make TARGET HOST's home channel; NIL clears it."
  (if target
      (nlk:session-state-put (handoff-session host) +home-key+
                             (nlk:encode-json-object
                              (nlk:plist-json (list :channel-id (getf target :channel-id)
                                                    :thread-id (getf target :thread-id)))))
      (nlk:session-state-delete (handoff-session host) +home-key+)))

(defun say-home (host text)
  "TEXT posted in HOST's home channel when one is set, on its worker."
  (nlk:when-let (home (home-target host))
    (on-worker host (post-message host home text :what "home channel notice"))))

(defvar *home-notes* (make-hash-table :test #'equal :synchronized t)
  "Host id -> the note of the organism's its home channel last heard: a note
comes as a frame, and again at a host's start while it is fresh.")

(defun say-note-home (host text)
  "TEXT, a note the organism said (NLE:ORGANISM-NOTE), in HOST's home channel
once."
  (when (sb-ext:with-locked-hash-table (*home-notes*)
          (unless (equal text (gethash (host-id host) *home-notes*))
            (setf (gethash (host-id host) *home-notes*) text)))
    (say-home host text)))

(defun tell-operator (host text &key (level :info))
  "TEXT to the operator: a notice on every attached shell, and a post in
HOST's home channel when one is set."
  ;; The notice carries no key, so it never reaches the board a model reads.
  (nle:notice text :level level)
  (say-home host text))

(defun room-host (session-id)
  "The started host whose room SESSION-ID is, and the room's target, or NIL."
  (loop for host in *hosts*
        for target = (session-target (platform-session-prefix host.platform) session-id)
        when target return (values host target)))

(defun sethome-command (args session-id)
  "/sethome: make the room SESSION-ID is the home channel — `off' clears it —
or, from a shell, say where each home is."
  (multiple-value-bind (host target) (room-host session-id)
    (cond
      ((null host)
       (format nil "~{~a~^~%~}"
               (or (loop for host in *hosts*
                         collect (format nil "~a: ~:[no home channel; type /sethome in the one you want~;~:*home is ~a~]"
                                         (host-id host)
                                         (nlk:when-let (home (home-target host))
                                           (format nil "channel ~a~@[ thread ~a~]"
                                                   (getf home :channel-id) (getf home :thread-id)))))
                   (list "no channel adapters running"))))
      ((string-equal (nlk:trimmed (or args "")) "off")
       (set-home host nil)
       "no home channel now: the bot reports to the operator's shells alone")
      (t
       (set-home host target)
       "this is the home channel now: restart notices, new releases, pairing requests and refused commands come here"))))
