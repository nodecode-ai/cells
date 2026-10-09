;;;; voice-test.lisp --- the pure half of Discord voice, and the policy above it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No socket, no libdave, no engine: the framing, the transport cipher,
;;;; the container, the speech boundary, who may speak, what a candidate
;;;; carries, what the commands answer, and what a failure says. The media
;;;; layer these sit on was proved live (see voice.lisp's header); these are
;;;; the parts that must keep behaving without Discord in the room.

(in-package #:nodecode.test)

(nlk:access (utterance ncd::utterance))

(defun voice-key (&aux (key (make-array 32 :element-type '(unsigned-byte 8))))
  "A 32-octet transport key, fixed so a failure is reproducible."
  (dotimes (index 32 key) (setf (aref key index) (mod (* 7 (1+ index)) 256))))

(defun voice-frame (&rest octets)
  (coerce octets '(vector (unsigned-byte 8))))

(defun voice-speech-frame (size &optional (seed 3))
  "A frame that is not the silence frame: something a speaker actually said."
  (let ((frame (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (index size frame)
      (setf (aref frame index) (mod (+ seed (* 13 index)) 256)))))

;;; --- payloads ---------------------------------------------------------------

(deftest channel-discord-voice-identify-never-offers-version-zero ()
  ;; Discord closes a voice connection that declares no DAVE support with
  ;; 4017. There is no downgrade, so the payload may never carry a 0.
  (let ((d (nlk:json-value (ncd:voice-identify-payload "g1" "u1" "s1" "tok") :object "d")))
    (is-shape d ((:string "server_id") "g1") ((:string "user_id") "u1")
      ((:string "session_id") "s1") ((:string "token") "tok")
      ((:integer "max_dave_protocol_version") eql 1))
    (is (plusp ncd:+voice-dave-version+) "the constant is never zero")))

(deftest channel-discord-voice-select-protocol-names-the-aead-mode ()
  (let* ((payload (ncd:voice-select-protocol-payload "203.0.113.7" 51234))
         (d (nlk:json-value payload :object "d"))
         (data (nlk:json-value d :object "data")))
    (is (equal "udp" (nlk:json-value d :string "protocol")))
    (is-shape data ((:string "address") "203.0.113.7") ((:integer "port") eql 51234)
      ((:string "mode") "aead_aes256_gcm_rtpsize") ((:string "mode") ncd:+voice-transport-mode+))))

(deftest channel-discord-voice-leaving-sends-a-null-channel ()
  ;; The one way out: op 4 with a null channel clears the voice state
  ;; Discord keeps. Anything else leaves the bot sitting there.
  (let* ((join (nlk:json-value (ncd:voice-state-payload "g1" "c1") :object "d"))
         (leave (nlk:json-value (ncd:voice-state-payload "g1" nil) :object "d")))
    (is (equal "c1" (nlk:json-value join :string "channel_id")))
    (is (null (nlk:json-value leave :string "channel_id")))))

(deftest channel-discord-voice-endpoint-keeps-its-port ()
  ;; Measured 2026-09-18: dialing the host on 443 when the endpoint named
  ;; another port is answered with close 4006, which reads like a stale
  ;; session and is not one.
  (is-values (host port) (ncd:voice-endpoint-parts "c-sea01-9b.discord.media:8443")
    (host "c-sea01-9b.discord.media") (port eql 8443 "the endpoint's own port is dialed"))
  (is-values (host port) (ncd:voice-endpoint-parts "plain.discord.media")
    (host "plain.discord.media") (port eql 443 "no port named falls back to 443")))

;;; --- binary framing ----------------------------------------------------------

(deftest channel-discord-voice-binary-frames-are-asymmetric ()
  ;; In: [seq uint16][opcode][body]. Out: [opcode][body].
  (is-values (sequence opcode body) (ncd:voice-binary-parts (voice-frame 0 7 25 #xAA #xBB))
    (sequence eql 7) (opcode eql 25) (body equalp (voice-frame #xAA #xBB)))
  (is (equalp (voice-frame 26 1 2) (ncd:voice-binary-frame 26 (voice-frame 1 2))))
  (is-values (sequence opcode body) (ncd:voice-binary-parts (voice-frame 0 1))
    (sequence null "a runt frame decodes to nothing") (opcode null) (body null)))

(deftest channel-discord-voice-transition-id-leads-a-welcome ()
  (is-values (transition rest) (ncd:transition-and-rest (voice-frame 0 3 9 9 9))
    (transition eql 3) (rest equalp (voice-frame 9 9 9))))

;;; --- IP discovery -------------------------------------------------------------

(deftest channel-discord-voice-ip-discovery-is-the-74-octet-type-1 ()
  (let ((packet (ncd:ip-discovery-packet #x01020304)))
    (is (eql 74 (length packet)) "the current packet is 74 octets")
    (is (eql 0 (aref packet 0)))
    (is (eql 1 (aref packet 1)) "type 0x0001, a request")
    (is (eql 70 (aref packet 3)) "length 70, the part after type and length")
    (is (equalp (voice-frame 1 2 3 4) (subseq packet 4 8)) "the ssrc rides it")))

(deftest channel-discord-voice-ip-discovery-answer-is-read-back ()
  (let ((answer (make-array 74 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref answer 1) 2)
    (replace answer (sb-ext:string-to-octets "198.51.100.9") :start1 8)
    (setf (aref answer 72) #xC0 (aref answer 73) #x01)
    (is-values (address port) (ncd:ip-discovery-answer answer 74)
      (address "198.51.100.9") (port eql 49153))
    ;; A request echoed back, or anything short, is not an answer.
    (setf (aref answer 1) 1)
    (is (null (ncd:ip-discovery-answer answer 74)) "a type 1 is not an answer")))

;;; --- the RTP packet and the transport cipher -------------------------------------

(deftest channel-discord-voice-head-length-counts-csrcs-and-the-preamble ()
  ;; The AAD under an -rtpsize mode. Getting this wrong fails every tag.
  (is-let* ((plain (voice-frame #x80 #x78 0 0 0 0 0 0 0 0 0 0))
            (two-csrc (voice-frame #x82 #x78 0 0 0 0 0 0 0 0 0 0))
            (extended (voice-frame #x90 #x78 0 0 0 0 0 0 0 0 0 0 #xBE #xDE 0 2)))
    ((ncd:rtp-head-length plain) eql 12)
    ((ncd:rtp-head-length two-csrc) eql 20 "four octets per CSRC")
    ((ncd:rtp-head-length extended) eql 16 "the extension preamble is head")
    ((ncd:rtp-extension-octets plain) eql 0) ((ncd:rtp-extension-octets extended) eql 8)))

(deftest channel-discord-voice-packet-round-trips-through-the-cipher ()
  (let* ((key (voice-key))
         (frame (voice-speech-frame 57))
         (packet (ncd:voice-packet key 9 1 960 42 frame)))
    (is (ncd:rtp-audio-p packet (length packet)) "it is an Opus RTP packet")
    (is (eql 42 (ncd:rtp-ssrc packet)))
    (is (equalp frame (ncd:voice-packet-frame key packet (length packet))))))

(deftest channel-discord-voice-packet-refuses-a-tampered-tag ()
  (let* ((key (voice-key))
         (packet (ncd:voice-packet key 3 1 960 42 (voice-speech-frame 40))))
    ;; One octet of ciphertext moved: the tag must not verify.
    (setf (aref packet 20) (logxor 1 (aref packet 20)))
    (is (null (ncd:voice-packet-frame key packet (length packet))))))

(deftest channel-discord-voice-packet-ignores-what-is-not-audio ()
  (let ((key (voice-key))
        (rtcp (voice-frame #x80 #xC9 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    (is (not (ncd:rtp-audio-p rtcp (length rtcp))) "payload type 201 is RTCP")
    (is (null (ncd:voice-packet-frame key rtcp (length rtcp))))
    ;; A runt is not a packet either.
    (is (null (ncd:voice-packet-frame key (voice-frame #x80 #x78 0) 3)))))

(deftest channel-discord-voice-extension-body-is-stepped-over-after-decryption ()
  ;; The extension body is INSIDE the ciphertext under an -rtpsize mode, so
  ;; it is skipped after the tag verifies, never before.
  (let* ((key (voice-key))
         (opus (voice-speech-frame 30 5))
         (extension (voice-frame 1 2 3 4 5 6 7 8))
         (header (voice-frame #x90 #x78 0 1 0 0 3 #xC0 0 0 0 42 #xBE #xDE 0 2))
         (sealed (ncd:transport-seal key 11 header
                                     (concatenate '(vector (unsigned-byte 8))
                                                  extension opus)))
         (packet (concatenate '(vector (unsigned-byte 8))
                              header sealed (voice-frame 0 0 0 11))))
    (is (equalp opus (ncd:voice-packet-frame key packet (length packet))))))

;;; --- the container ---------------------------------------------------------------

(deftest channel-discord-voice-ogg-round-trips-the-frames ()
  ;; A container is framing, not a codec: the frames Discord sent come back
  ;; out of the file byte for byte, which is what lets an utterance reach
  ;; the transcriber with no Opus implementation anywhere in this tree.
  (let* ((frames (loop for index from 1 to 47 collect (voice-speech-frame (+ 20 index) index)))
         (file (ncd:ogg-opus-file frames)))
    (is (equal "OggS" (sb-ext:octets-to-string (subseq file 0 4)
                                               :external-format :latin-1)))
    (let ((packets (ncd:ogg-packets file)))
      (is (eql (+ 2 (length frames)) (length packets)) "two headers and the frames")
      (is (equal "OpusHead" (sb-ext:octets-to-string (subseq (first packets) 0 8)
                                                     :external-format :latin-1)))
      (is (equal "OpusTags" (sb-ext:octets-to-string (subseq (second packets) 0 8)
                                                     :external-format :latin-1))))
    (is (equalp frames (ncd:opus-audio-frames file)))))

(deftest channel-discord-voice-ogg-handles-a-frame-past-255-octets ()
  ;; A segment table entry is one octet: a long frame is split across
  ;; several and ends on one that is not 255.
  (let* ((long (voice-speech-frame 600 9))
         (file (ncd:ogg-opus-file (list long))))
    (is (equalp (list long) (ncd:opus-audio-frames file)))))

;;; --- the speech boundary -----------------------------------------------------------

(defun voice-utterance (&key (user "u1") (now 1000))
  (ncd:make-utterance :user-id user :last-ms now))

(deftest channel-discord-voice-silence-frames-carry-no-words ()
  ;; 0xF8FFFE is what a client sends to say it stopped. DAVE lets it past
  ;; unencrypted by design, so it arrives as itself and must not be counted
  ;; as speech — an utterance made only of them is nothing at all.
  (is (ncd:opus-silence-p ncd:+opus-silence-frame+))
  (is (not (ncd:opus-silence-p (voice-speech-frame 3))))
  (let ((utterance (voice-utterance)))
    (dotimes (index 10)
      (ncd:utterance-note-frame utterance ncd:+opus-silence-frame+ (+ 1000 (* 20 index))))
    (is (eql 0 utterance.count) "silence adds nothing")
    (is (not (ncd:utterance-complete-p utterance 5000)))))

(deftest channel-discord-voice-an-utterance-ends-in-silence (let ((utterance (voice-utterance))))
  (loop for index from 0 below 40
        do (ncd:utterance-note-frame utterance (voice-speech-frame 40)
                                     (+ 1000 (* 20 index))))
  (is-shape utterance (.count eql 40) (ncd:utterance-milliseconds eql 800))
  (is (not (ncd:utterance-complete-p utterance 2100)))
  (is (ncd:utterance-complete-p utterance 3000))
  (is (ncd:utterance-worth-hearing-p utterance)))

(deftest channel-discord-voice-a-cough-opens-no-turn (let ((utterance (voice-utterance))))
  (loop for index from 0 below 5
        do (ncd:utterance-note-frame utterance (voice-speech-frame 30)
                                     (+ 1000 (* 20 index))))
  (is (eql 100 (ncd:utterance-milliseconds utterance)))
  (is (ncd:utterance-complete-p utterance 4000) "it did end")
  (is (not (ncd:utterance-worth-hearing-p utterance))))

(deftest channel-discord-voice-a-monologue-is-cut-rather-than-grown ()
  (let ((utterance (voice-utterance)))
    (loop for index from 0 below 3100
          do (ncd:utterance-note-frame utterance (voice-speech-frame 30)
                                       (+ 1000 (* 20 index))))
    (is (>= (ncd:utterance-milliseconds utterance) 60000))
    (is (ncd:utterance-complete-p utterance (+ 1000 (* 20 3100))))))

(deftest channel-discord-voice-an-utterance-becomes-an-ogg-recording ()
  (let ((utterance (voice-utterance))
        (frames (loop for index from 1 to 12 collect (voice-speech-frame (+ 25 index) index))))
    (dolist (frame frames)
      (ncd:utterance-note-frame utterance frame 1000))
    (is (equalp frames (ncd:opus-audio-frames (ncd:utterance-recording utterance))))))

;;; --- who may speak -------------------------------------------------------------------

(defun voice-lane-for-test (&key (speakers '("u-allowed")) (room "text-1"))
  "A lane whose threads are already up, so nothing here opens a socket."
  ;; The stop thunk standing in for them is what VOICE-ATTACH-SERVER reads to
  ;; decide it has nothing to start.
  (let ((lane (ncd::%make-voice-lane :guild-id "g1" :self-user-id "bot-1"
                                     :channel-id "vc-1" :room-channel-id room
                                     :speakers speakers)))
    (setf (ncd::voice-lane-work-queue lane) (nck:make-work-queue "voice-test" :cap 32)
          (ncd::voice-lane-stop-lap lane) (lambda () t))
    lane))

(deftest channel-discord-voice-only-an-allowlisted-human-is-heard ()
  (let ((lane (voice-lane-for-test)))
    (is (ncd:voice-speaker-allowed-p lane "u-allowed"))
    (is (not (ncd:voice-speaker-allowed-p lane "u-stranger")))
    (is (not (ncd:voice-speaker-allowed-p lane nil)))))

(deftest channel-discord-voice-a-star-admits-the-room ()
  (is (ncd:voice-speaker-allowed-p (voice-lane-for-test :speakers '("*")) "u-anyone")))

(deftest channel-discord-voice-a-stranger-makes-no-utterance (let ((lane (voice-lane-for-test))))
  (setf (gethash 11 (ncd::voice-lane-speaker-ids lane)) "u-stranger"
        (gethash 12 (ncd::voice-lane-speaker-ids lane)) "u-allowed")
  (ncd:voice-note-frame lane 11 (voice-speech-frame 40))
  (is (eql 0 (hash-table-count (ncd::voice-lane-utterances lane))))
  (ncd:voice-note-frame lane 12 (voice-speech-frame 40))
  (is (eql 1 (hash-table-count (ncd::voice-lane-utterances lane)))))

(deftest channel-discord-voice-the-bot-never-hears-itself ()
  ;; Our own audio does not come back from Discord, and the speaking map is
  ;; the belt: the lane refuses to learn its own id as a speaker, so even a
  ;; loop would find no utterance to join.
  (let ((lane (voice-lane-for-test :speakers '("*"))))
    (ncd::voice-handle-text
     lane (cell-json "{\"op\": 5, \"d\": {\"user_id\": \"bot-1\", \"ssrc\": 99, \"speaking\": 1}}"))
    (is (eql 0 (hash-table-count (ncd::voice-lane-speaker-ids lane))))
    (ncd::voice-handle-text
     lane (cell-json "{\"op\": 5, \"d\": {\"user_id\": \"u-other\", \"ssrc\": 98, \"speaking\": 1}}"))
    (is (equal "u-other" (gethash 98 (ncd::voice-lane-speaker-ids lane))))))

(deftests channel-discord-voice (frames queued)
    (let ((lane (voice-lane-for-test))
          (utterance (voice-utterance :user "u-allowed" :now 0)))
      (setf (gethash 12 (ncd::voice-lane-speaker-ids lane)) "u-allowed")
      (loop for index from 0 below frames
            do (ncd:utterance-note-frame utterance (voice-speech-frame 40) (* 20 index)))
      ;; Backdated, so the silence gap has already passed: FRAMES decide
      ;; whether the utterance was speech worth QUEUED asks or a cough.
      (setf (gethash 12 (ncd::voice-lane-utterances lane)) utterance)
      (ncd:voice-close-finished-utterances lane)
      (is (eql 0 (hash-table-count (ncd::voice-lane-utterances lane))))
      (is (eql queued (nck:queue-depth (ncd::voice-lane-work-queue lane)))))
  (a-finished-utterance-reaches-the-work-queue 40 1)
  (a-cough-is-dropped-rather-than-queued 1 0))

;;; --- what the ask carries -----------------------------------------------------------

(deftest channel-discord-voice-candidate-is-an-ordinary-ask-with-its-origin ()
  (let* ((lane (voice-lane-for-test))
         (utterance (voice-utterance :user "u-allowed"))
         (candidate (ncd:voice-candidate lane utterance "what is the plan" "msg-77"))
         (source (nlk:json-value candidate :object "source")))
    (is (equal "what is the plan" (nlk:json-value candidate :string "text")))
    (is-shape source ((:string "platform") "discord") ((:string "channel_id") "text-1")
      ((:string "message_id") "msg-77") ((:string "user_id") "u-allowed")
      ((:boolean "addressed") eq t) ((:string "voice_origin") "discord_voice")
      ((:string "voice_channel_id") "vc-1"))
    ;; The lane a voice ask opens must be its own, whatever the kit keys it
    ;; by: the transcript message is what makes it unique.
    (is (equal "discord-text-1-mmsg-77"
               (nck:lane-id (nck:room-session-id "discord" candidate) "msg-77")))))

(deftest channel-discord-voice-an-answer-is-matched-by-its-room-not-a-guessed-lane ()
  ;; Measured 2026-09-18: the kit re-keys a lane when it opens a thread for
  ;; an ask, so the live session was discord-<channel>-t<thread>-m<message>
  ;; while the lane name this predicted was discord-<channel>-m<message>.
  ;; Nothing ever matched and no answer was ever spoken. The room prefix is
  ;; what holds.
  (let ((lane (voice-lane-for-test :room "text-1")))
    (is (ncd::voice-room-session-p lane "discord-text-1") "the room itself")
    (is (ncd::voice-room-session-p lane "discord-text-1-mmsg-77") "a lane under it")
    (is (ncd::voice-room-session-p lane "discord-text-1-t900-m900"))
    (is (not (ncd::voice-room-session-p lane "discord-other-room")))
    (is (not (ncd::voice-room-session-p lane nil)))))

;;; --- cancellation -------------------------------------------------------------------

(deftest channel-discord-voice-a-new-utterance-stops-the-answer-being-spoken ()
  (let ((lane (voice-lane-for-test)))
    (setf (gethash 12 (ncd::voice-lane-speaker-ids lane)) "u-allowed")
    (let ((generation (ncd::voice-play lane (list (voice-speech-frame 40)))))
      (is (eql 1 (length (ncd::voice-lane-play-queue lane))) "something is queued to say")
      (ncd:voice-note-frame lane 12 (voice-speech-frame 40))
      (is (null (ncd::voice-lane-play-queue lane)) "the queue is dropped")
      (is (> (ncd::voice-lane-play-generation lane) generation)))))

(deftest channel-discord-voice-silence-does-not-interrupt (let ((lane (voice-lane-for-test))))
  (setf (gethash 12 (ncd::voice-lane-speaker-ids lane)) "u-allowed")
  (ncd::voice-play lane (list (voice-speech-frame 40)))
  (ncd:voice-note-frame lane 12 ncd:+opus-silence-frame+)
  (is (eql 1 (length (ncd::voice-lane-play-queue lane)))))

;;; --- configuration and its refusals ------------------------------------------------------

(defun voice-section (&rest pairs)
  (apply #'nlk:make-json-object pairs))

(defun voice-adapter (&key (bot "bot-1"))
  "A Discord adapter as the lane has it once READY landed, and no socket."
  (ncd::%make-discord-adapter :token "t" :bot-user-id bot :application-id "app-1"))

(defmacro with-voice-seats ((&rest guilds) &body body)
  "BODY with no lane, an empty seat table, and GUILDS — (CHANNEL GUILD) — the
voice channels the bot knows."
  `(let ((ncd:*voice-lane* nil)
         (ncd::*voice-states* (make-hash-table :test #'equal))
         (ncd::*voice-channel-guilds* (make-hash-table :test #'equal)))
     (loop for (channel guild) in ',guilds
           do (setf (gethash channel ncd::*voice-channel-guilds*) guild))
     ,@body))

(deftest channel-discord-voice-a-join-needs-no-configuration ()
  ;; /voice join needs nothing configured: it sits in the voice channel of
  ;; whoever typed it, in the server that channel is in, talks through the
  ;; channel it was typed in and hears its asker when no list names anyone.
  ;; Each thing it cannot have is refused in words saying what to do.
  (with-voice-seats (("vc-1" "g1"))
    (flet ((refusal (section &rest keys)
             (nth-value 1 (apply #'ncd:join-voice section keys))))
      (let ((ncd::*discord-adapter* nil))
        (is (search "still connecting" (refusal (voice-section) :channel "vc-1"))))
      (let ((ncd::*discord-adapter* (voice-adapter)))
        (is-table (needle section keys) (search needle (apply #'refusal section keys))
          ("sit in a voice channel first" (voice-section) ())
          ("<#vc-9> is no voice channel of a server" (voice-section) '(:channel "vc-9"))
          ("type /voice join in a server channel" (voice-section) '(:channel "vc-1"))
          ("nobody is allowed to speak" (voice-section) '(:channel "vc-1" :room "t-1"))))
      (let ((ncd::*discord-adapter* (voice-adapter :bot nil)))
        (is (search "own user id" (refusal (voice-section) :channel "vc-1" :room "t-1"
                                                            :speakers '("u1"))))))
    (let* ((adapter (voice-adapter))
           (ncd::*discord-adapter* adapter)
           (ncd::*dave-loaded* t))
      (nlk:bind (((lane refusal) (ncd:join-voice (voice-section) :channel "vc-1" :room "t-1"
                                                                 :speakers '("u1")))
                 (sent (nck:queue-pop (ncd::discord-adapter-outbound adapter) 0)))
        (is (null refusal))
        (is-shape lane (ncd::voice-lane-guild-id "g1") (ncd::voice-lane-channel-id "vc-1")
          (ncd::voice-lane-room-channel-id "t-1") (ncd::voice-lane-speakers '("u1")))
        (is-shape sent ((:integer "op") = 4) ((:string "d" "guild_id") "g1")
          ((:string "d" "channel_id") "vc-1"))
        (is (search "already sitting in <#vc-1>"
                    (nth-value 1 (ncd:join-voice (voice-section) :channel "vc-2"))))))))

(deftest channel-discord-voice-a-join-discord-never-grants-says-why ()
  ;; Discord answers a join it will not grant with nothing: the join waits
  ;; for the bot's own voice state a few seconds, then lets the lane go and
  ;; says what fixes it — Connect and Speak, and the invite that grants them.
  ;; A seat Discord gave is kept.
  (with-voice-seats (("vc-1" "g1"))
    (let* ((adapter (voice-adapter))
           (ncd::*discord-adapter* adapter)
           (ncd::*dave-loaded* t)
           (ncd::*voice-seat-seconds* 0.2)
           (lane (ncd:join-voice (voice-section) :channel "vc-1" :room "t-1" :speakers '("u1"))))
      (nck:queue-pop (ncd::discord-adapter-outbound adapter) 0)
      (is (not (ncd:voice-await-seat lane)))
      (is (null ncd:*voice-lane*))
      (is-shape (nck:queue-pop (ncd::discord-adapter-outbound adapter) 0)
        ((:string "d" "guild_id") "g1") ((:any "d" "channel_id") null))
      (is-carrying (said (ncd::voice-unseated-text lane))
        "Discord did not seat me in <#vc-1>" "Connect and Speak"
        "client_id=app-1&scope=bot+applications.commands&permissions=309240908864")
      (let ((seated (ncd:join-voice (voice-section) :channel "vc-1" :room "t-1" :speakers '("u1"))))
        (ncd:route-voice-dispatch
         adapter (cell-json "{\"t\": \"VOICE_STATE_UPDATE\", \"d\": {\"guild_id\": \"g1\",
                   \"user_id\": \"bot-1\", \"channel_id\": \"vc-1\", \"session_id\": \"s1\"}}"))
        (is (ncd:voice-await-seat seated))
        (is (eq seated ncd:*voice-lane*))
        (setf ncd:*voice-lane* nil)))))

(deftest channel-discord-voice-talks-and-listens-where-it-was-asked ()
  ;; The text room: named, else where /voice join was typed — a thread's
  ;; channel, never a direct message — else the first allowed channel. Who
  ;; is heard: named, else the allowed users, else the owner, else the asker.
  (let ((ncd::*discord-adapter* nil))
    (is-table (expected section typed) (equal expected (ncd:voice-room-channel section typed))
      ("t-1" (voice-section "voice_text_channel_id" "t-1") "t-5")
      ("t-5" (voice-section "allowed_channels" (vector "t-9")) "t-5")
      ("t-9" (voice-section "allowed_channels" (vector "t-9" "t-8")) nil)
      (nil (voice-section) nil)))
  (is-table (expected kind) (equal expected (ncd:voice-typed-room
                                             (nlk:json-object "chat_kind" kind "channel_id" "c1"
                                                              "parent_channel_id" "p1")))
    ("c1" "channel") ("p1" "thread") (nil "direct_message"))
  (is-table (expected section) (equal expected (ncd:voice-speakers section "asker"))
    ('("u-1") (voice-section "voice_speakers" (vector "u-1") "allowed_users" (vector "u-2")))
    ('("u-2") (voice-section "allowed_users" (vector "u-2") "owner" (vector "u-3")))
    ('("u-3") (voice-section "owner" (vector "u-3")))
    ('("asker") (voice-section))))

;;; --- the command ---------------------------------------------------------------------

(deftest channel-discord-voice-status-says-how-to-start (let ((ncd:*voice-lane* nil)))
  ;; Seated nowhere, /voice says how to start, and what seats the bot on its
  ;; own.
  (is-table (needle verb) (search needle (ncd:voice-command verb (voice-section)))
    ("Sit in one and type `/voice join`" "status") ("not in a voice channel" "leave")
    ("no such verb" "dance") ("Sit in one and type `/voice join`" ""))
  (is-carrying (said (ncd:voice-command "status"
                                        (voice-section "voice_channel_id" "vc-1"
                                                       "voice_autojoin" t
                                                       "voice_follow" (vector "ana"))))
    "voice_autojoin sits in <#vc-1>" "I follow <@ana> into voice")
  ;; One sentence, as written: no format directive left in it.
  (is (equal "voice: not in a voice channel. Sit in one and type `/voice join`, and I sit beside you."
             (ncd:voice-command "status" (voice-section)))))

(deftest channel-discord-voice-bare-in-a-room-is-a-card ()
  ;; /voice bare in a room answers with a card, an embed: grey and how to
  ;; start, or green with where the bot sits, the channel it talks through and
  ;; whom it hears; Join, or Leave and the notes; Refresh; and a menu of the
  ;; room's voice messages with its pick shown. A press on the card answers
  ;; with what it did, quoted above the card again. The words stay the answer
  ;; where no card is drawn; from a shell, bare /voice is the status in words.
  (flet ((says (control) (nck:said-line (second control)))
         (card () (getf nck::*command-card* :controls))
         (panel () (getf nck::*command-card* :panel)))
    (let ((ncd:*voice-lane* nil)
          (nck:*command-source* (nlk:json-object "chat_kind" "channel" "channel_id" "t-1"
                                                 "user_id" "u1"))
          (nck::*command-card* nil))
      (is-carrying (said (ncd:voice-command "" (voice-section) "tui-1"))
        "## Voice" "Sit in one, then press Join" "Voice messages here: words only.")
      (destructuring-bind (buttons (menu placeholder options)) (card)
        (is (equal '("/voice join" "/voice") (mapcar #'says buttons)))
        (is-shape nil (menu eq :menu) (placeholder "Voice messages here"))
        (is (equal '("/voice off" "/voice on" "/voice tts") (mapcar #'says options)))
        (is (equal '(t nil nil) (mapcar #'fourth options))))
      (is-shape (panel) (:title "Voice") (:tone :stopped)
        (:fields '(("Voice messages here" . "Words only"))))
      (is (search "Sit in one, then press Join, and I sit beside you." (getf (panel) :text)))
      (setf (gethash "pressed" nck:*command-source*) t
            nck::*command-card* nil)
      (is-carrying (said (ncd:voice-command "leave" (voice-section) "tui-1"))
        "voice: not in a voice channel" "## Voice")
      (is (uiop:string-prefix-p "> not in a voice channel" (getf (panel) :text)))
      (let ((ncd:*voice-lane* (voice-lane-for-test)))
        (setf (ncd::voice-lane-notes-by ncd:*voice-lane*) "u1")
        (is-carrying (said (ncd::voice-card (voice-section) "tui-1" "voice: done."))
          "voice: done." "In <#vc-1>, talking through <#text-1>, hearing <@u-allowed>."
          "Taking notes: 0 lines so far.")
        (is (equal '("/voice leave" "/voice notes stop" "/voice") (mapcar #'says (first (card)))))
        (is-shape (panel) (:tone :done)
          (:fields '(("Sitting in" . "<#vc-1>") ("Talking through" . "<#text-1>")
                     ("Hearing" . "<@u-allowed>") ("Notes" . "0 lines so far")
                     ("Voice messages here" . "Words only")))))))
  (let ((nck:*command-source* nil) (ncd:*voice-lane* nil))
    (is (search "voice: not in a voice channel" (ncd:voice-command "" (voice-section))))))

(deftest channel-discord-voice-completes-the-verbs-that-would-do-something ()
  ;; /voice's argument completes to the verbs that would act now, each with
  ;; what it does: join while the bot sits nowhere, leave, notes and say
  ;; while it sits, notes stop while notes are taken, and always status and
  ;; the voice-message modes; the tail typed narrows them.
  (let ((lane (voice-lane-for-test)))
    (flet ((verbs (text &optional seat)
             (let ((ncd:*voice-lane* seat))
               (mapcar (lambda (choice) (getf choice :value)) (ncd::voice-choices text)))))
      (is-table (expected text seat) (equal expected (verbs text seat))
        ('("join" "status" "off" "on" "tts") "" nil)
        ('("leave" "notes" "say" "status" "off" "on" "tts") "" lane)
        ('("tts") "TS" lane))
      (setf (ncd::voice-lane-notes-by lane) "u1")
      (is (equal '("notes stop") (verbs "notes" lane))))
    (let ((ncd:*voice-lane* nil))
      (is (equal "join — sit in the voice channel you are in"
                 (getf (first (ncd::voice-choices "jo")) :name))))))

;;; --- routing the two dispatches ----------------------------------------------------------

(deftest channel-discord-voice-server-update-is-routed-and-nothing-else-is ()
  (let* ((lane (voice-lane-for-test))
         (ncd:*voice-lane* lane)
         (adapter (ncd::%make-discord-adapter :token "t" :bot-user-id "bot-1")))
    (is (null (ncd:route-voice-dispatch
               adapter (cell-json "{\"t\": \"MESSAGE_CREATE\", \"d\": {\"id\": \"1\"}}"))))
    (is (ncd:route-voice-dispatch
         adapter (cell-json "{\"t\": \"VOICE_SERVER_UPDATE\", \"d\": {\"token\": \"vt\", \"endpoint\": \"host:8443\"}}")))
    (is-shape lane (ncd::voice-lane-token "vt") (ncd::voice-lane-endpoint "host:8443"))
    (is (ncd:route-voice-dispatch
         adapter (cell-json "{\"t\": \"VOICE_STATE_UPDATE\", \"d\": {\"user_id\": \"bot-1\", \"channel_id\": \"vc-9\", \"session_id\": \"sess-9\"}}")))
    (is-shape lane (ncd::voice-lane-session-id "sess-9") (ncd::voice-lane-channel-id "vc-9"))))

(deftest channel-discord-voice-somebody-else-s-voice-state-is-not-ours ()
  (let* ((lane (voice-lane-for-test))
         (ncd:*voice-lane* lane)
         (adapter (ncd::%make-discord-adapter :token "t" :bot-user-id "bot-1")))
    (ncd:route-voice-dispatch
     adapter (cell-json "{\"t\": \"VOICE_STATE_UPDATE\", \"d\": {\"user_id\": \"u-other\", \"channel_id\": \"vc-9\", \"session_id\": \"sess-other\"}}"))
    (is (null (ncd::voice-lane-session-id lane)))))

;;; --- what a voice answer sounds like, before any engine runs -----------------------------

(deftest channel-discord-voice-an-answer-is-read-aloud-without-its-markdown ()
  (is-carrying (said (nck:speakable-text
                     "# Heading~%Use `frobnicate` for **that**, see [docs](http://x)."))
    (:absent "`" "backticks are not read out") (:absent "**") (:absent "#")
    ("frobnicate" "the words survive"))
  (is (search "(code)" (nck:speakable-text (format nil "before~%```~%(a b c)~%```~%after")))))

(deftest channel-discord-voice-a-long-answer-is-cut-at-a-sentence ()
  (let ((long (with-output-to-string (out)
                (dotimes (index 60)
                  (format out "This is sentence number ~a and it says a thing. " index)))))
    (is-values (said cut-p) (nck:spoken-text long 300)
      (cut-p is "it was cut") ((<= (length said) 300) is)
      ((char= #\. (char said (1- (length said)))) is))
    (is-values (said cut-p) (nck:spoken-text "Short answer." 300)
      (cut-p not) (said "Short answer."))))

(deftest channel-discord-voice-speech-refuses-with-a-reason-rather-than-lying ()
  ;; Text is the authority: every refusal here is a line in the room, and
  ;; the caller keeps the written answer either way.
  (let ((nck:*speech* (nlk:make-json-object "enabled" nil)))
    (is (search "turned off"
                (refusal-text error (nck:synthesize-speech "anything")))))
  (let ((nck:*speech* nil))
    (is (search "nothing to say"
                (refusal-text error (nck:synthesize-speech "   "))))))

;;; --- where the bot sits by its own rules ------------------------------------------------

(defun voice-states (&rest seats)
  "A voice-state table from SEATS, each (USER CHANNEL [BOT-P])."
  (let ((states (make-hash-table :test #'equal)))
    (loop for (user channel bot-p) in seats do (setf (gethash user states) (cons channel bot-p)))
    states))

(deftest channel-discord-voice-rules-seat-the-bot ()
  ;; voice_follow seats it beside the first person it names who is in voice,
  ;; and takes it away when they leave the channel it sits in; voice_autojoin
  ;; seats it in its own channel while a person is there; it leaves a channel
  ;; the last person left. Bots never count as company.
  (let ((follow (voice-section "voice_channel_id" "home" "voice_follow" (vector "ana" "kim")))
        (auto (voice-section "voice_channel_id" "home" "voice_autojoin" t)))
    (flet ((react (section here user was &rest seats)
             (multiple-value-list
              (ncd::voice-reaction section here user was (apply #'voice-states seats)))))
      (is-each (react)
        (follow nil "ana" nil '("ana" "a") '(:join "a" "following <@ana>") "joins ana")
        (follow "a" "ana" "a" '("ana" "b") '(:move "b" "following <@ana>") "moves with her")
        (follow "a" "ana" "a" '("bo" "a") '(:leave nil "<@ana> left") "leaves when she does")
        (follow "a" "ana" "a" '("kim" "c") '(:move "c" "following <@kim>")
                "goes to the next person it follows")
        (follow "z" "ana" "a" '("bo" "z") '(nil) "stays where she never was")
        (auto nil "bo" nil '("bo" "home") '(:join "home" "somebody is in it") "joins its own")
        (auto nil "bo" nil '("bo" "elsewhere") '(nil) "not for somebody elsewhere")
        (auto "home" "bo" "home" '("helper" "home" t) '(:leave nil "everyone left")
              "leaves the last person; a bot is no company")
        (auto "home" "bo" "home" '("cy" "home") '(nil) "stays while somebody is there")
        (auto nil nil nil '("cy" "home") '(:join "home" "somebody is in it")
              "the boot seats it where its rules do")))))

(deftest channel-discord-voice-every-server-s-voice-states-are-kept ()
  ;; GUILD_CREATE seeds who sits where in that server and which voice
  ;; channels are its — falling through to the ordinary routing, which seeds
  ;; threads from it — and every person's VOICE_STATE_UPDATE after it moves
  ;; them, in any server the bot is in; a server's GUILD_CREATE again
  ;; replaces its own seats and keeps the others'.
  (with-voice-seats ()
    (let ((ncd::*voice-section* (voice-section))
          (adapter (voice-adapter))
          (g1 (cell-json "{\"t\": \"GUILD_CREATE\", \"d\": {\"id\": \"g1\",
                 \"channels\": [{\"id\": \"t-1\", \"type\": 0}, {\"id\": \"vc-3\", \"type\": 2}],
                 \"voice_states\": [{\"user_id\": \"ana\", \"channel_id\": \"vc-1\"},
                                    {\"user_id\": \"helper\", \"channel_id\": \"vc-1\"}],
                 \"members\": [{\"user\": {\"id\": \"helper\", \"bot\": true}}]}}")))
      (flet ((seats ()
               (sort (loop for user being the hash-keys of ncd::*voice-states*
                             using (hash-value seat)
                           collect (cons user seat))
                     #'string< :key #'car)))
        (is (null (ncd:route-voice-dispatch adapter g1)))
        (is (equal '("ana") (ncd::voice-humans-in "vc-1")))
        (is (ncd:route-voice-dispatch
             adapter (cell-json "{\"t\": \"VOICE_STATE_UPDATE\", \"d\": {\"guild_id\": \"g1\",
                       \"user_id\": \"ana\", \"channel_id\": \"vc-2\"}}")))
        (is (ncd:route-voice-dispatch
             adapter (cell-json "{\"t\": \"VOICE_STATE_UPDATE\", \"d\": {\"guild_id\": \"g9\",
                       \"user_id\": \"kim\", \"channel_id\": \"vc-9\"}}")))
        (is (equal '(("ana" "vc-2" . nil) ("helper" "vc-1" . t) ("kim" "vc-9" . nil)) (seats)))
        (is-each (gethash)
          ("vc-1" ncd::*voice-channel-guilds* "g1" "where somebody sits")
          ("vc-3" ncd::*voice-channel-guilds* "g1" "a voice channel nobody sits in")
          ("vc-9" ncd::*voice-channel-guilds* "g9" "another server's")
          ("t-1" ncd::*voice-channel-guilds* nil "a text channel is none"))
        (ncd:route-voice-dispatch adapter g1)
        (is (equal '(("ana" "vc-1" . nil) ("helper" "vc-1" . t) ("kim" "vc-9" . nil)) (seats)))))))

(deftest channel-discord-voice-its-own-leave-ends-no-lane ()
  ;; The bot asked to leave: the voice state that says it is out is its own
  ;; doing, and a lane sitting down meanwhile keeps its seat. Disconnected by
  ;; somebody else, the lane is over.
  (let* ((lane (voice-lane-for-test))
         (ncd:*voice-lane* lane)
         (ncd::*voice-expecting-leave* t)
         (out (cell-json "{\"t\": \"VOICE_STATE_UPDATE\", \"d\": {\"user_id\": \"bot-1\",
                            \"channel_id\": null}}"))
         (adapter (ncd::%make-discord-adapter :token "t" :bot-user-id "bot-1")))
    (is (ncd:route-voice-dispatch adapter out))
    (is (eq lane ncd:*voice-lane*))
    (is (null ncd::*voice-expecting-leave*))
    (ncd:route-voice-dispatch adapter out)
    (is (null ncd:*voice-lane*))))

(deftest channel-discord-voice-an-idle-seat-is-given-up ()
  ;; A seat /voice join took is given up after voice_idle_minutes with nobody
  ;; speaking to the bot; one its rules hold, notes being taken, or minutes of
  ;; 0 keep it.
  (let ((lane (voice-lane-for-test))
        (ncd::*voice-states* (voice-states '("ana" "vc-1")))
        (hour (* 60 60000)))
    (setf (ncd::voice-lane-active-ms lane) 0)
    (flet ((idle (section &optional (now hour)) (ncd::voice-idle-p lane section now)))
      (is (idle (voice-section)))
      (is (not (idle (voice-section) (* 4 60000))) "five minutes are not up")
      (is (not (idle (voice-section "voice_idle_minutes" 0))))
      (is (not (idle (voice-section "voice_follow" (vector "ana")))) "its rules hold the seat")
      (setf (ncd::voice-lane-notes-by lane) "ana")
      (is (not (idle (voice-section))) "a meeting is not idle"))))

;;; --- meeting notes ----------------------------------------------------------------------

(deftest channel-discord-voice-notes-are-everyone-s-and-answer-nothing ()
  ;; /voice notes listens to everyone in the channel and writes down what
  ;; they say, in the name of the person who asked; it is asked for in
  ;; Discord, where there is somebody to ask it. The transcript reads who
  ;; said what, minutes and seconds from the start.
  (let ((lane (voice-lane-for-test)))
    (is (search "somebody to answer" (ncd::voice-notes-command lane nil nil)))
    (is (search "no notes" (ncd::voice-notes-command lane "stop" "ana")))
    (is (search "taking notes" (ncd::voice-notes-command lane nil "ana")))
    (is (search "already" (ncd::voice-notes-command lane nil "ana")))
    (is (ncd:voice-speaker-allowed-p lane "u-stranger") "a meeting is everyone's")
    (is (search "nobody said anything" (ncd::voice-notes-command lane "stop" "ana")))
    (is (not (ncd:voice-speaker-allowed-p lane "u-stranger")) "and then it is over"))
  (is (equal (format nil "[00:05] <@ana>: hello~%[01:02] <@kim>: bye")
             (ncd::voice-notes-transcript '((6000 "ana" "hello") (63000 "kim" "bye")) 1000)))
  (is-carrying (ask (ncd::voice-notes-ask "vc-1" 3 1 "[00:05] <@ana>: hello"))
    "<#vc-1>" "decided" "3 minutes, 1 person" "[00:05] <@ana>: hello"))

(deftest channel-discord-voice-replies-are-set-by-their-verbs (let ((ncd:*voice-lane* nil)))
  ;; on, tts and off are the room's voice replies, set where /voice is typed;
  ;; typed anywhere no running channel holds, the refusal says so.
  (is (search "no room of a running channel"
              (ncd:voice-command "tts" (voice-section) "tui-session")))
  (is (search "join, leave, status, on, tts, off, notes"
              (ncd:voice-command "dance" (voice-section)))))
