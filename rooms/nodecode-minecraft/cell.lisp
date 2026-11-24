;;;; cell.lisp --- the bot in the game, and the agent behind its chat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of kyssta-exe/hermes-minecraft-gateway (see NOTICE). What the
;;;; original gives a player is kept: a bot joins the server, each player who
;;;; talks to it gets a session of their own, answers come back in the game's
;;;; chat (public, or whispered when private_replies is on), and a long answer
;;;; is split with [1/N] markers because the game caps chat at 256 characters.
;;;; The harness seams are Nodecode's: the section is the config, a turn is
;;;; NLE:SUBMIT, and the answer is the session's own.
;;;;
;;;; What replaces the original's Node bridge is this file's own protocol
;;;; client (wire.lisp, protocol.lisp): no Node, no npm, no subprocess. A
;;;; reader worker holds the socket and turns packets into prompts; a delivery
;;;; worker posts finished answers and keeps the bot turning in place.

(in-package #:nodecode-minecraft)

(nlk:define-peripheral minecraft :not-running t)

;;; --- what the cell runs on ------------------------------------------------------

(defvar *bot* nil
  "The live connection (a BOT), or NIL when there is none.")

(defvar *bot-lock* (bt:make-lock "minecraft")
  "One writer at a time: the reader confirms teleports, the deliverer speaks.")

(defvar *read-worker* nil)
(defvar *deliver-worker* nil)

(defvar *asking* '()
  "Players owed an answer: (PLAYER . SESSION) entries, newest first.")

(defvar *sequence* 0
  "A counter for the command ids this cell mints, so two asks are two admissions.")

(defstruct (bot (:copier nil))
  (host "" :type string)
  (port 25565 :type integer)
  (username "" :type string)
  (link nil)
  (threshold nil)
  (players (make-hash-table :test #'equal) :type hash-table)
  (x 0d0) (y 64d0) (z 0d0)
  (yaw 0.0) (pitch 0.0)
  (connected-p nil)
  (last-idle 0))

(defun say (text &key (level :warning))
  "Tell the operator something without taking the cell down."
  (handler-case (nle:notice (format nil "minecraft: ~a" text) :level level :key "channels.minecraft")
    (error () nil)))

;;; --- the transport, and the one seam a test stubs -------------------------------

(defstruct (link (:copier nil))
  (input nil) (output nil) (socket nil))

(defun open-link (host port)
  "Dial the server. The one network seam: a test binds *OPEN-LINK*."
  (let* ((socket (usocket:socket-connect host port
                                         :element-type '(unsigned-byte 8)
                                         :timeout (setting :timeout-seconds)))
         (stream (usocket:socket-stream socket)))
    (ignore-errors
     (setf (sb-bsd-sockets:sockopt-tcp-nodelay (usocket:socket socket)) t))
    (make-link :input stream :output stream :socket socket)))

(defun close-link (link)
  (when link
    (ignore-errors (close (link-input link)))
    (when (link-socket link) (ignore-errors (usocket:socket-close (link-socket link))))
    nil))

(defun send-packet (bot id octets)
  "Send the packet a builder answered out: its id and its bytes."
  (send bot id octets))

(defun send (bot id octets)
  "One packet out, the whole of it under the lock."
  (when (and bot (bot-link bot))
    (bt:with-lock-held (*bot-lock*)
      (write-frame (link-output (bot-link bot)) (bot-threshold bot)
                   id (or octets (make-array 0 :element-type '(unsigned-byte 8))))))
  bot)

;;; --- joining --------------------------------------------------------------------

(defun read-login (bot)
  "Read login packets until the server says the bot is in: the compression
threshold it sets, then a Login Success. Refuses on a Disconnect."
  (loop
    (multiple-value-bind (id body) (read-frame (link-input (bot-link bot)) nil)
      (cond
        ((= id (packet-id :compression :login)) (setf (bot-threshold bot) (cur-varint (make-cur body))))
        ((= id (packet-id :success :login))
         (let ((cur (make-cur body)))
           (cur-uuid cur)
           (setf (bot-username bot) (cur-string cur))
           (setf (bot-connected-p bot) t))
         (return t))
        ((= id (packet-id :disconnect :login)) (fail "minecraft: ~a" (disconnect-reason body)))))))

(defun connect-bot ()
  "Dial, hand shake, log in => a connected BOT, or a condition."
  (let* ((host (setting :server))
         (port (setting :port))
         (link (open-link host port)))
    (let ((bot (make-bot :host host :port port :username (setting :username) :link link)))
      (handler-case
          (progn
            (multiple-value-call #'send-packet bot (handshake-packet host port))
            (multiple-value-call #'send-packet bot (login-start-packet (setting :username)))
            (read-login bot)
            (say (format nil "joined ~a:~d as ~a" host port (bot-username bot)) :level :info)
            bot)
        (error (condition)
          (close-link link)
          (fail "minecraft: ~a" (princ-to-string condition)))))))

(defun drop-bot (&optional (reason nil))
  (when *bot*
    (close-link (bot-link *bot*)))
  (setf *bot* nil)
  (when reason (say reason)))

;;; --- what the server says -------------------------------------------------------

(defun player-name (bot uuid)
  "The name of UUID in the player list, or its first bytes when the list has
not named it yet."
  (or (gethash uuid (bot-players bot))
      (format nil "player-~a" (subseq uuid 0 (min 8 (length uuid))))))

(defun own-chat-p (bot uuid)
  (let ((name (gethash uuid (bot-players bot))))
    (and name (string-equal name (bot-username bot)))))

(defun dispatch (bot id body)
  "One play packet: keep alive answered, teleports confirmed, names kept,
chat turned into a prompt."
  (cond
    ((= id (packet-id :keep-alive)) (send bot (packet-id :keep-alive :play :out) body))
    ((= id (packet-id :sync-position))
     (multiple-value-bind (x y z teleport-id) (sync-position-of body)
       (setf (bot-x bot) x (bot-y bot) y (bot-z bot) z)
       (multiple-value-call #'send-packet bot (confirm-teleport-packet teleport-id))))
    ((= id (packet-id :player-info))
     (dolist (entry (player-info-names body))
       (setf (gethash (car entry) (bot-players bot)) (cdr entry))))
    ((= id (packet-id :player-chat))
     (multiple-value-bind (uuid text) (player-chat-said body)
       (unless (own-chat-p bot uuid)
         (handle-chat (player-name bot uuid) text))))
    ((= id (packet-id :system-chat)) (system-chat-said body))
    ((= id (packet-id :disconnect))
     (fail "minecraft: the server said: ~a" (disconnect-reason body)))
    (t nil))
  t)

(defun pump-one ()
  "One packet off the socket, dispatched; a closed socket drops the bot."
  (let ((bot *bot*))
    (when bot
      (handler-case (multiple-value-bind (id body) (read-frame (link-input (bot-link bot))
                                                               (bot-threshold bot))
                      (dispatch bot id body))
        (error (condition) (drop-bot (format nil "~a" (princ-to-string condition))))))))

(defun read-step ()
  "The reader worker's step: join when there is no bot, then take one packet."
  (handler-case
      (progn
        (unless *bot* (setf *bot* (connect-bot)))
        (pump-one))
    (error (condition) (drop-bot (format nil "~a" condition))))
  nil)

;;; --- what a player says, and what the agent answers ------------------------------

(defun session-of (player)
  "The session a player's asks run in: one per player, so a conversation in
the game is a conversation in the agent's history."
  (format nil "minecraft-~a"
          (string-downcase
           (ppcre:regex-replace-all "[^A-Za-z0-9_]+" (or player "unknown") "-"))))

(defun strip-prefix (text)
  "TEXT without the trigger the section names, when it opens with it."
  (let ((prefix (setting :prefix)))
    (if (and prefix (plusp (length prefix))
             (uiop:string-prefix-p prefix text))
        (nlk:trimmed (subseq text (length prefix)))
        (nlk:trimmed text))))

(defun allowed-p (player)
  "Whether PLAYER may talk to the bot: everyone with allow_all, else the list."
  (or (setting :allow-all)
      (member player (setting :allowed-users) :test #'string-equal)))

(defun submit-prompt (session prompt command-id source-id)
  "PROMPT into SESSION through the one in-process ingress; tests stub this."
  (nle:submit session prompt :command-id command-id :source "minecraft" :source-id source-id))

(defun ensure-session (session)
  (when (nlk:store-open-p)
    (unless (nlk:session-exists-p session)
      (nlk:standby-session :id session :cwd (uiop:getcwd)))))

(defun handle-chat (player text)
  "One line of in-game chat, from a player: a prompt, when the player may talk
and the line is meant for the bot."
  (let* ((text (nlk:trimmed (or text "")))
         (prefix (or (setting :prefix) ""))
         (opens (and (plusp (length prefix)) (uiop:string-prefix-p prefix text))))
    (when (and (plusp (length text)) (allowed-p player)
               (or opens (not (setting :require-prefix))))
      (let ((clean (strip-prefix text)))
        (when (plusp (length clean)) (ask-agent player clean)))))
  t)

(defun ask-agent (player text)
  "Hand TEXT to the agent in the player's session, and remember to answer them."
  (let ((session (session-of player)))
    (handler-case
        (progn
          (ensure-session session)
          (submit-prompt session
                         (format nil "[minecraft, ~a] ~a" player text)
                         (format nil "minecraft:~a:~a" (session-of player) (incf *sequence*))
                         player)
          (unless (assoc player *asking* :test #'equal)
            (push (cons player session) *asking*)))
      (error (condition)
        (send-chat (format nil "~a: I could not think that through: ~a" player condition) player)))))

;;; --- what the bot says ----------------------------------------------------------

(defun mc-format (text)
  "TEXT as the game's chat takes it: section-sign codes off (servers reject
them from a client), markdown flattened to its words, and only the
characters a chat line holds."
  (let ((text (or text "")))
    (setf text (ppcre:regex-replace-all "§[0-9a-fklmnor]" text ""))
    (setf text (ppcre:regex-replace-all "```[a-zA-Z]*\\n?" text ""))
    (setf text (ppcre:regex-replace-all "\\*\\*(.+?)\\*\\*" text "\\1"))
    (setf text (ppcre:regex-replace-all "__(.+?)__" text "\\1"))
    (setf text (ppcre:regex-replace-all "~~(.+?)~~" text "\\1"))
    (setf text (ppcre:regex-replace-all "`([^`]+)`" text "\\1"))
    (setf text (ppcre:regex-replace-all "(?<![\\w*])\\*([^*\\n]+?)\\*(?![\\w*])" text "\\1"))
    (setf text (ppcre:regex-replace-all "(?m)^#{1,6}[ \\t]*" text ""))
    (setf text (coerce (loop for character across text
                              when (or (<= 32 (char-code character) 126) (member character '(#\Tab #\Newline)))
                                collect character)
                       'string))
    (nlk:trimmed text)))

(defun chunk-text (text &key (limit 256))
  "TEXT cut into chat lines of at most LIMIT characters, in order, the
markers of a split answer taken off the room they need."
  (let* ((text (or text ""))
         (limit (max 40 (or limit 256)))
         (parts (if (zerop (length text))
                    (list "")
                    (loop for start from 0 below (length text) by limit
                          collect (subseq text start (min (length text) (+ start limit)))))))
    (if (null (cdr parts))
        parts
        (let ((room (max 1 (- limit 10))))
          (loop for start from 0 below (length text) by room
                collect (subseq text start (min (length text) (+ start room))))))))

(defun send-chat (text &optional (player nil))
  "Post TEXT into the game: public chat, or a whisper to PLAYER when
private_replies is on. => the number of lines it took."
  (let* ((plain (mc-format text))
         (parts (chunk-text plain :limit (setting :reply-length)))
         (whisper (and (setting :private-replies) player)))
    (loop for part in parts
          for i from 1
          do (let ((line (if (and whisper (null (cdr parts)))
                             part
                             (if (and (> (length parts) 1) (not whisper))
                                 (format nil "[~d/~d] ~a" i (length parts) part)
                                 part))))
               (when *bot*
                 (multiple-value-call #'send-packet *bot*
                   (chat-message-packet (if whisper
                                            (format nil "/tell ~a ~a" player line)
                                            line)
                                        :timestamp (now-ms)))
                 (sleep 0.3))))
    (length parts)))

(defun now-ms ()
  (- (* 1000 (get-universal-time)) (* 1000 2208988800)))

(defun deliver-pending ()
  "Post each finished answer back to the player who asked."
  (dolist (entry (copy-list *asking*))
    (let ((player (car entry)) (session (cdr entry)))
      (unless (nlk:active-turn-p session)
        (setf *asking* (remove entry *asking* :test #'eq))
        (let ((answer (nlk:turn-answer session)))
          (when (and answer (plusp (length (nlk:trimmed answer))))
            (handler-case (send-chat answer player)
              (error (condition) (warn "minecraft: an answer did not reach ~a: ~a" player condition)))))))))

(defun idle-step ()
  "Keep the bot looking alive: turn on the spot now and then, and step a
little, which is the original's wander without its movement physics."
  (when (and *bot* (bot-connected-p *bot*) (plusp (setting :idle-seconds)))
    (when (> (- (now-ms) (bot-last-idle *bot*)) (* 1000 (setting :idle-seconds)))
      (setf (bot-last-idle *bot*) (now-ms))
      (setf (bot-yaw *bot*) (mod (+ (bot-yaw *bot*) (+ 15 (random 60))) 360.0))
      (multiple-value-call #'send-packet *bot*
        (set-rotation-packet (bot-yaw *bot*) (bot-pitch *bot*))))))

(defun deliver-step ()
  "The delivery worker's step: answers first, then the bot's idle turn."
  (handler-case (deliver-pending) (error (condition) (warn "minecraft: delivery: ~a" condition)))
  (handler-case (idle-step) (error (condition) (warn "minecraft: idle: ~a" condition)))
  nil)

;;; --- the manual, the /command and the cell --------------------------------------

(defparameter +primer+
  "Minecraft is a game; this cell puts the agent in a server's chat as a bot (protocol 763,
Minecraft 1.20.1, offline mode).
Every player who talks in public chat gets a session of their own (minecraft-<player>), and the
answer is posted back in the game's chat, split with [1/N] markers because chat lines hold 256
characters and stripped of markdown and colour codes. private_replies whispers the answer to the
player instead; a command like /command or /help is passed through to the server only when written
with a slash.
The channels.minecraft section names the server, the bot's username and who may talk (allowed_users,
or allow_all). /minecraft shows the bot and the players it has seen; /minecraft say TEXT posts TEXT
in chat."
  "What (help :minecraft) answers while the cell runs.")

(defun status-line ()
  (let ((bot *bot*))
    (format nil "Minecraft bot ~a at ~a:~d (~a). ~d player~:p seen, ~d ask~:p owed an answer.~
~%The server, the username and who may talk are the channels.minecraft section; /minecraft say TEXT ~
posts a line in chat."
            (if bot (bot-username bot) (setting :username))
            (setting :server) (setting :port)
            (if (and bot (bot-connected-p bot)) "in the world" "not connected")
            (if bot (hash-table-count (bot-players bot)) 0)
            (length *asking*))))

(defun run-command (args session-id)
  "One /minecraft invocation: no argument answers the bot's state; \"say TEXT\"
posts TEXT in the game's chat."
  (declare (ignore session-id))
  (let* ((args (nlk:trimmed (or args "")))
         (space (position #\Space args)))
    (cond
      ((zerop (length args)) (status-line))
      ((and space (string-equal "say" (subseq args 0 space)))
       (maybe-say (nlk:trimmed (subseq args (1+ space)))))
      (t (maybe-say args)))))

(defun maybe-say (text)
  (cond ((zerop (length (or text ""))) "Nothing to say.")
        ((null *bot*) (fail "minecraft: the cell is not running: turn it on in /cells, and check its channels.minecraft section"))
        (t (format nil "Said it in ~d line~:p." (send-chat text)))))

(nle:define-cell minecraft
  (:section ("channels" "minecraft")
    (:guide "name the server and who may talk; the bot joins in offline mode as the username; /minecraft shows it, /minecraft say TEXT posts in chat")
    ("server" :string :required t
     :doc "the Minecraft server's hostname or address")
    ("port" :integer :default 25565 :min 1
     :doc "the server's port")
    ("username" :string :default "NodecodeBot"
     :doc "the name the bot joins under (offline mode: any unused name)")
    ("allowed_users" :list
     :doc "the Minecraft player names who may talk to the bot")
    ("allow_all" :boolean :default nil
     :doc "let every player on the server talk to the bot (they run with your machine's authority)")
    (:any-of "allowed_users" "allow_all")
    ("prefix" :string :default ""
     :doc "a trigger word a line may open with, stripped before the agent sees it")
    ("require_prefix" :boolean :default nil
     :doc "only answer a line that opens with prefix")
    ("private_replies" :boolean :default nil
     :doc "whisper answers with /tell instead of speaking in public chat")
    ("reply_length" :integer :default 256 :min 40
     :doc "the game's chat line limit; a longer answer is split")
    ("idle_seconds" :integer :default 8 :min 0
     :doc "how often the bot turns in place; 0 stands still")
    ("reconnect_seconds" :integer :default 10 :min 1
     :doc "how long to wait before joining again after a disconnect")
    ("timeout_seconds" :integer :default 10 :min 1
     :doc "how long dialling the server may take"))
  (:help :minecraft
         "minecraft: the agent in a Minecraft server's chat - /minecraft shows the bot, /minecraft say TEXT posts a line"
         +primer+)
  (:command "minecraft" 'run-command
            :description "Show the Minecraft bot, or say something in the game's chat"
            :argument-hint "[say TEXT]")
  (:start (lambda ()
            (check-config)
            (setf *asking* '() *sequence* 0 *bot* nil)
            (setf *read-worker* (nlk:worker-start "minecraft-read" #'read-step :wake 1))
            (setf *deliver-worker* (nlk:worker-start "minecraft-deliver" #'deliver-step :wake 1))
            (nle:on-stop
             (lambda ()
               (when *deliver-worker* (nlk:worker-stop *deliver-worker*))
               (when *bot* (close-link (bot-link *bot*)))
               (when *read-worker* (nlk:worker-stop *read-worker*))
               (setf *read-worker* nil *deliver-worker* nil *bot* nil *asking* '())
               (handler-case (nle:notice nil :key "channels.minecraft") (error () nil)))))))

(defun check-config ()
  "Refuse a section the bot could never run on, naming the member."
  (unless (and (setting :server) (plusp (length (setting :server))))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "minecraft: no server: set \"server\" in the channels.minecraft section")))
  (unless (or (setting :allow-all) (setting :allowed-users))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "minecraft: name who may talk: \"allowed_users\" with their Minecraft "
                                 "names, or \"allow_all\": true to let every player through"))))
