;;;; cell-test.lisp --- lifecycle, the wire, a chat line, and /minecraft.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here without a server: a packet round-trips through the
;;;; framing, with and without compression; a login is read off in-memory
;;;; bytes; a player's line becomes one turn in that player's session and the
;;;; answer is written back as chat packets; a stranger and a silent line are
;;;; ignored; a long answer is stripped and split; /minecraft answers; and the
;;;; cell installs, stops, refuses and starts twice cleanly.

(in-package #:nodecode.test)

(define-test-slice "minecraft" "MINECRAFT-CELL-")

(defun minecraft-config (&rest pairs)
  "A whole shared config with a channels.minecraft section built from PAIRS."
  (nlk:json-object "channels"
                    (nlk:json-object "minecraft" (apply #'nlk:make-json-object pairs))))

(defun minecraft-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-minecraft:start-cell (apply #'minecraft-config pairs)))

;;; The cell's own names, reached the way a test in this package reaches a
;;; cell: forwarders, so the assertions below read as the code they prove.

(defun make-buf () (nodecode-minecraft:make-buf))
(defun buf-octets (buf) (nodecode-minecraft:buf-octets buf))
(defun buf-varint (buf n) (nodecode-minecraft:buf-varint buf n))
(defun buf-byte (buf n) (nodecode-minecraft:buf-byte buf n))
(defun buf-bytes (buf octets) (nodecode-minecraft:buf-bytes buf octets))
(defun buf-us (buf n) (nodecode-minecraft:buf-us buf n))
(defun buf-long (buf n) (nodecode-minecraft:buf-long buf n))
(defun buf-string (buf string) (nodecode-minecraft:buf-string buf string))
(defun write-frame (stream threshold &rest body)
  (apply #'nodecode-minecraft:write-frame stream threshold body))
(defun read-frame (stream threshold) (nodecode-minecraft:read-frame stream threshold))
(defun packet-id (name &optional (state :play) (direction :in))
  (nodecode-minecraft:packet-id name state direction))
(defun make-cur (data &optional (pos 0)) (nodecode-minecraft:make-cur data pos))
(defun cur-string (cur) (nodecode-minecraft:cur-string cur))
(defun player-chat-said (body) (nodecode-minecraft:player-chat-said body))
(defun mc-format (text) (nodecode-minecraft:mc-format text))
(defun chunk-text (text &key (limit 256)) (nodecode-minecraft:chunk-text text :limit limit))
(defun session-of (player) (nodecode-minecraft:session-of player))

;;; --- a server in memory ---------------------------------------------------------

(defun octets-of (stream) (flexi-streams:get-output-stream-sequence stream))

(defun varint-octets (n)
  (let ((buf (make-buf)))
    (buf-varint buf n)
    (buf-octets buf)))

(defun login-success-body (&optional (name "NodecodeBot"))
  (let ((buf (make-buf)))
    (buf-bytes buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 7))
    (buf-string buf name)
    (buf-octets buf)))

(defun player-chat-body (text)
  "A Player Chat Message as 1.20.1 spells it, as far as the bot reads it."
  (let ((buf (make-buf)))
    (buf-bytes buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 3))
    (buf-varint buf 0)                              ; message index
    (buf-byte buf 0)                                ; no signature
    (buf-string buf text)
    (buf-long buf 0)                                ; timestamp
    (buf-long buf 0)                                ; salt
    (buf-octets buf)))

(defun server-stream (&key threshold (name "NodecodeBot") (extra '()))
  "An input stream carrying a Login Success (and, with THRESHOLD, the
compression packet before it) and then EXTRA (ID . BODY) packets."
  (let ((out (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
    (when threshold
      (write-frame out nil (packet-id :compression :login) (varint-octets threshold)))
    (write-frame out threshold (packet-id :success :login) (login-success-body name))
    (dolist (packet extra)
      (write-frame out (if threshold 0 nil) (car packet) (cdr packet)))
    (flexi-streams:make-in-memory-input-stream (octets-of out))))

(defun output-sink ()
  (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))

;;; --- the seams a test stands in front of -----------------------------------------

(defmacro with-server (&body body)
  "BODY with the one network seam stubbed: every dial answers a fresh
in-memory server that has just let the bot in."
  `(with-stubbed-fdefinition (nodecode-minecraft::open-link (host port)
                              (nodecode-minecraft::make-link :input (server-stream)
                                                             :output (output-sink)))
     ,@body))

(defmacro with-minecraft-section ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, every dial answered in memory."
  `(with-server
     (with-cell-stop ((minecraft-start ,@pairs))
       ,@body)))

(defmacro with-minecraft (&body body)
  "BODY with the cell started and every player allowed in."
  `(with-minecraft-section ("server" "mc.test" "allow_all" t) ,@body))

(defvar *written* nil
  "The output stream the fake bot writes into while a test runs.")

(defmacro with-fake-bot (&body body)
  "BODY with a connected bot over a recorded output stream, no socket."
  `(let* ((*written* (output-sink))
          (nodecode-minecraft::*bot*
            (nodecode-minecraft::make-bot :host "mc.test" :port 25565 :username "NodecodeBot"
                      :link (nodecode-minecraft::make-link :input (server-stream)
                                                          :output *written*)
                      :connected-p t)))
     ,@body))

(defvar *submits* '()
  "Every prompt the cell submitted, (SESSION PROMPT SOURCE-ID COMMAND-ID), newest first.")

(defmacro with-submits (&body body)
  `(let ((*submits* '()))
     (with-stubbed-fdefinition (nodecode-minecraft::submit-prompt (session prompt command-id source-id)
                                (push (list session prompt source-id command-id) *submits*)
                                :started)
       ,@body)))

(defun prompts () (mapcar #'second (reverse *submits*)))

(defun written-lines ()
  "The chat lines the bot wrote out, oldest first."
  (let ((in (flexi-streams:make-in-memory-input-stream (octets-of *written*)))
        (lines '()))
    (handler-case
        (loop (multiple-value-bind (id body) (read-frame in nil)
                (when (= id (packet-id :chat :play :out))
                  (push (cur-string (make-cur body)) lines))))
      (error () nil))
    (nreverse lines)))

;;; --- the framing ----------------------------------------------------------------

(deftest minecraft-cell-a-packet-round-trips-through-the-framing ()
  (dolist (threshold (list nil 1 4096))
    (let ((out (output-sink))
          (id (packet-id :chat :play :out)))
      (write-frame out threshold id "hello there, world")
      (let ((in (flexi-streams:make-in-memory-input-stream (octets-of out))))
        (multiple-value-bind (read-id body) (read-frame in threshold)
          (is (= id read-id) "the id survives")
          (is (equal "hello there, world" (cur-string (make-cur body))) "and so does the text")))))
  (let ((big (make-string 600 :initial-element #\x))
        (out (output-sink)))
    (write-frame out 256 (packet-id :chat :play :out) big)
    (let ((in (flexi-streams:make-in-memory-input-stream (octets-of out))))
      (multiple-value-bind (id body) (read-frame in 256)
        (is (= (packet-id :chat :play :out) id) "a deflated body still leads with its id")
        (is (equal big (cur-string (make-cur body))) "and inflates to what was sent")))))

(deftest minecraft-cell-a-login-is-read-off-the-wire ()
  (with-minecraft
    (let ((bot (nodecode-minecraft::connect-bot)))
      (is (nodecode-minecraft::bot-connected-p bot) "the login success means the bot is in")
      (is (equal "NodecodeBot" (nodecode-minecraft::bot-username bot))
          "under the name it was given"))))

(deftest minecraft-cell-a-chat-message-parses-to-its-sender-and-text ()
  (multiple-value-bind (uuid text) (player-chat-said (player-chat-body "hi there"))
    (is (equal "03030303030303030303030303030303" uuid) "the sender uuid is read")
    (is (equal "hi there" text) "and the words after it")))

;;; --- a line becomes a turn, the answer comes back --------------------------------

(deftest minecraft-cell-a-players-line-becomes-one-turn-and-its-answer-comes-back ()
  (with-minecraft
    (with-fake-bot
      (with-submits
        (nodecode-minecraft::handle-chat "Steve" "what is this place?")
        (is (= 1 (length *submits*)) "one line, one turn")
        (is (equal "minecraft-steve" (first (first *submits*))) "in the player's own session")
        (is-carrying (prompt (first (prompts))) ("Steve" "what is this place?"))
        (is (null (written-lines)) "nothing is said before the turn answers")
        (with-stubbed-fdefinitions ((nlk:active-turn-p (session) nil)
                                    (nlk:turn-answer (session) "**A quiet valley**, with a river."))
          (nodecode-minecraft::deliver-pending))
        (is (equal '("A quiet valley, with a river.") (written-lines))
            "the answer goes back into chat, markdown flattened")
        (is (null nodecode-minecraft::*asking*) "the player is not owed twice")))))

(deftest minecraft-cell-a-long-answer-is-split-with-markers ()
  (is (equal '("abcde") (chunk-text "abcde" :limit 40)) "a short answer is one line")
  (is (= 2 (length (chunk-text (make-string 400 :initial-element #\a) :limit 256))))
  (with-minecraft
    (with-fake-bot
      (nodecode-minecraft::send-chat (make-string 400 :initial-element #\a))
      (let ((lines (written-lines)))
        (is (= 2 (length lines)) "400 characters at 256 is two lines")
        (is (uiop:string-prefix-p "[1/2] " (first lines)) "the first says it is the first")
        (is (uiop:string-prefix-p "[2/2] " (second lines)) "and the second the second")))))

(deftest minecraft-cell-the-answer-is-plain-text ()
  (is (equal "Title here" (mc-format "# Title here")))
  (is (equal "bold and code" (mc-format "**bold** and `code`")))
  (is (equal "plain" (mc-format "§cplain")))
  (is (equal "caf" (mc-format "café")) "a character chat cannot carry is dropped")
  (is (equal "minecraft-steve" (session-of "Steve"))))

;;; --- who may talk ---------------------------------------------------------------

(deftest minecraft-cell-a-stranger-and-a-silent-line-are-ignored ()
  (with-minecraft-section ("server" "mc.test" "allowed_users" (vector "Steve"))
    (with-fake-bot
      (with-submits
        (nodecode-minecraft::handle-chat "Alex" "let me in")
        (nodecode-minecraft::handle-chat "Steve" "")
        (is (null *submits*) "neither is a turn")
        (nodecode-minecraft::handle-chat "Steve" "one for the bot")
        (is (= 1 (length *submits*)) "but a named player who says something is heard")))))

;;; --- /minecraft -----------------------------------------------------------------

(deftest minecraft-cell-the-command-answers-and-says ()
  (with-minecraft
    (with-fake-bot
      (is-carrying (text (nodecode-minecraft::run-command "" nil)) ("mc.test" "NodecodeBot"))
      (is (equal "Said it in 1 line." (nodecode-minecraft::run-command "say hi all" nil)))
      (is (equal '("hi all") (written-lines))))))

;;; --- the cell ------------------------------------------------------------------

(define-cell-lifecycle-tests "minecraft"
  (:fixture with-minecraft)
  (:config "server" "mc.test" "allow_all" t)
  (:help :minecraft)
  (:command "minecraft")
  (:running (is (equal "mc.test" (nodecode-minecraft::setting :server)) "the section is read")
            (is (= 256 (nodecode-minecraft::setting :reply-length)) "and its defaults")
            (is nodecode-minecraft::*read-worker* "the reader worker runs")
            (is nodecode-minecraft::*deliver-worker* "and so does the delivery worker"))
  (:stopped (is (null nodecode-minecraft::*read-worker*) "the reader worker is gone")
            (is (null nodecode-minecraft::*deliver-worker*) "and the deliverer"))
  (:refused ("port" 0) ("allow_all" nil "allowed_users" #()))
  (:idle nodecode-minecraft:minecraft-error (nodecode-minecraft::run-command "say hi" nil)))

(deftest minecraft-cell-nobody-allowed-refuses-by-name ()
  (with-server
    (let ((text (handler-case (progn (minecraft-start "server" "mc.test" "allowed_users" #()) "")
                  (error (condition) (princ-to-string condition)))))
      (is-carrying (text text) ("allowed_users" "allow_all")))
    (is (null nodecode-minecraft::*read-worker*) "a refused start installed nothing")))
