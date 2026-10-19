;;;; adapter-test.lisp --- the Matrix lane end to end, over a fake homeserver.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane runs as it runs for an operator: its live executor and its
;;;; /sync lap against fake-matrix.lisp on 127.0.0.1, the real nodecode
;;;; gateway behind it (WITH-ADAPTER-LANE, the provider a stub). Covers: the
;;;; first sync answering nothing said before it, a direct message answered
;;;; quoting its ask, a room ask answered in its thread and continued there
;;;; unmentioned, invites joined only from allowed people, an encrypted room
;;;; told once and never answered, controls drawn as reactions and taken off,
;;;; the sync token kept, and the probe. The digest flow itself is the kit's
;;;; (host-test.lisp).

(in-package #:nodecode.test)

(defun matrix-section (fake token-path soul-path &rest members)
  "channels.matrix against FAKE, the token in TOKEN-PATH, MEMBERS over it."
  (let ((section (nlk:json-object "homeserver" (fake-matrix-url fake) "token_file" token-path
                                  "request_timeout_seconds" 5
                                  "soul_file" soul-path)))
    (loop for (key value) on members by #'cddr
          do (setf (gethash key section) value))
    section))

(defun matrix-answers (fake needle)
  "Every m.room.message send so far whose body says NEEDLE, oldest first."
  (remove-if-not (lambda (body) (search needle (or (ignore-errors (gethash "body" body)) "")))
                 (matrix-calls fake "send" :type "m.room.message")))

(defmacro with-matrix-lane ((fake &rest members) &body body)
  "BODY against a started Matrix lane over FAKE -- MEMBERS the section's own
-- once its first sync has been answered; the provider answers `matrix reply'.
The sync token is kept in a scratch folder."
  `(with-matrix-state-dir (state-dir)
     (let ((ncm::*sync-state-directory* state-dir))
       (with-adapter-lane (:reply "matrix reply" :token "syt_fake" :start ncm:start-channel
                           :bindings ((executor nil))
                           :section (matrix-section ,fake token-path soul-path ,@members)
                           :plans nil
                           :after ((is (adapter-threads-stopped-p "channel-matrix"))))
         (is (await (:timeout 10) (plusp (fake-matrix-polls ,fake))) "the lane is polling past its first sync")
         ,@body))))

(deftest channel-matrix-dm-answered-backlog-not ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!dm:fake" :members '("@kim:fake"))
    (setf (fake-matrix-backlog fake) (list (cons "!dm:fake" (matrix-text "@kim:fake" "said before" :id "$old"))))
    (with-matrix-lane (fake "allowed_users" (vector "@kim:fake") "owner" (vector "@kim:fake"))
      (fake-matrix-push fake "!dm:fake" (matrix-text "@kim:fake" "hi bot" :id "$ask1"))
      (is-present (answer (await (:timeout 15) (first (matrix-answers fake "matrix reply"))))
          "the answer was posted"
        (is (equal "m.text" (gethash "msgtype" answer)) "the answer notifies")
        (is (equal "$ask1" (nlk:json-value answer :string "m.relates_to" "m.in_reply_to" "event_id"))
            "a direct message answers quoting the ask")
        (is (null (nlk:json-value answer :string "m.relates_to" "rel_type")) "and opens no thread"))
      (is-lane-carrying "matrix" "matrix-!dm:fake" "matrix-!dm:fake-m$ask1" "matrix-channel"
                        "(Matrix user id @kim:fake)" "(ncm:request")
      (is (wire-row "kim (operator) [m$ask1 u@kim:fake]: hi bot" (first messages-seen) "content")
          "the line names its speaker by the name the homeserver gives")
      (is (await (:timeout 15) (plusp (count-session-events "matrix-!dm:fake-m$ask1" "turn.completed"))))
      (is (= 1 (length (matrix-answers fake "matrix reply"))) "what was said before the first sync opened nothing")
      (is (not (nlk:session-exists-p "matrix-!dm:fake-m$old")))
      (is (matrix-calls fake "typing") "the bot typed while it worked")
      (is (await (:timeout 5) (let ((kept (probe-file (merge-pathnames "_bot_fake.since" ncm::*sync-state-directory*))))
                                (and kept (uiop:string-prefix-p "b" (uiop:read-file-string kept)))))
          "the sync token is kept for the next start"))))

(deftest channel-matrix-room-ask-threads-and-goes-on ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!room:fake" :members '("@kim:fake" "@lee:fake"))
    (with-matrix-lane (fake "allowed_rooms" (vector "!room:fake") "allowed_users" (vector "@kim:fake")
                            "require_mention" t)
      (let ((lane "matrix-!room:fake-m$root"))
        (fake-matrix-push fake "!room:fake"
                          (matrix-text "@kim:fake" "bot: what is up" :id "$root" :mentions '("@bot:fake")
                                       :html "<a href=\"https://matrix.to/#/@bot:fake\">bot</a>: what is up"))
        (is-present (answer (await (:timeout 15) (first (matrix-answers fake "matrix reply"))))
            "the answer was posted"
          (is (equal "m.thread" (nlk:json-value answer :string "m.relates_to" "rel_type")))
          (is (equal "$root" (nlk:json-value answer :string "m.relates_to" "event_id")) "in the ask's thread"))
        (is (await (:timeout 15) (plusp (count-session-events lane "turn.completed"))))
        (is (some (lambda (row) (search "[m$root u@kim:fake]: what is up"
                                        (or (ignore-errors (nlk:json-value row :string "content")) "")))
                  (coerce (first messages-seen) 'list))
            "the model reads the ask, not the mention that routed it")
        ;; A room line that mentions nobody is context, not an ask.
        (fake-matrix-push fake "!room:fake" (matrix-text "@kim:fake" "just chatting" :id "$chat"))
        ;; In the thread the bot answers in, no mention is needed, and the
        ;; lane the ask opened takes the next turn.
        (fake-matrix-push fake "!room:fake" (matrix-text "@kim:fake" "and more" :id "$more" :thread "$root"))
        (is (await (:timeout 15) (= 2 (length (matrix-answers fake "matrix reply"))))
            "the follow-up was answered")
        (is (every (lambda (body) (equal "$root" (nlk:json-value body :string "m.relates_to" "event_id")))
                   (matrix-answers fake "matrix reply"))
            "both answers are in the one thread")
        (is (await (:timeout 15) (= 2 (count-session-events lane "turn.completed"))))
        (is (= 2 (count-session-events lane "turn.started")) "one lane, two turns")
        (is (not (nlk:session-exists-p "matrix-!room:fake-m$more")) "no second lane")
        (is (not (nlk:session-exists-p "matrix-!room:fake-m$chat")) "chatter opened nothing")))))

(deftest channel-matrix-invites-and-encrypted-rooms ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!secret:fake" :members '("@kim:fake" "@lee:fake") :encrypted t)
    (with-matrix-lane (fake "allowed_users" (vector "@kim:fake"))
      (fake-matrix-invite fake "!new:fake" "@kim:fake")
      (fake-matrix-invite fake "!spam:fake" "@stranger:fake")
      (is (await (:timeout 10) (matrix-calls fake "join" :room "!new:fake")) "an allowed person's invite is joined")
      (is (null (matrix-calls fake "join" :room "!spam:fake")) "a stranger's is not")
      (dotimes (i 2)
        (fake-matrix-push fake "!secret:fake"
                          (matrix-event "@kim:fake" "m.room.encrypted"
                                        (nlk:json-object "algorithm" "m.megolm.v1.aes-sha2" "ciphertext" "AAAA")
                                        :id (format nil "$enc~d" i))))
      (is (await (:timeout 10) (matrix-answers fake "end-to-end encrypted")) "the room is told why")
      (sleep 1)
      (is (= 1 (length (matrix-answers fake "end-to-end encrypted"))) "once")
      (is (equal "m.notice" (gethash "msgtype" (first (matrix-answers fake "end-to-end encrypted")))))
      (is (null (matrix-answers fake "matrix reply")) "and nothing in it is answered"))))

(deftest channel-matrix-controls-are-reactions ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!room:fake" :members '("@kim:fake" "@lee:fake"))
    (with-matrix-lane (fake "allowed_users" (vector "@kim:fake"))
      (let* ((adapter ncm::*matrix-adapter*)
             (host (ncm::matrix-adapter-host adapter))
             (target (list :channel-id "!room:fake"))
             (posted (nck:execute-plan (nck:host-executor host)
                                       (ncm::post-plan (ncm::matrix-adapter-book adapter) target "working"
                                                       :controls '(("Stop" "stop:x" :danger)
                                                                   ("Details" "details:x")))))
             (card (ncm:matrix-event-id (nck:execution-body posted))))
        (is (search "stop" (gethash "body" (first (matrix-answers fake "working")))) "the hint says how to press")
        (is (await (:timeout 10) (= 2 (length (matrix-calls fake "send" :type "m.reaction"))))
            "one reaction per control, on the card")
        (is (equal (list (ncm:control-glyph '("Stop") 0) (ncm:control-glyph '("Details") 1))
                   (mapcar (lambda (body) (nlk:json-value body :string "m.relates_to" "key"))
                           (matrix-calls fake "send" :type "m.reaction"))))
        (is (every (lambda (body) (equal card (nlk:json-value body :string "m.relates_to" "event_id")))
                   (matrix-calls fake "send" :type "m.reaction")))
        ;; The reactions' own echoes are noted, so the settle can take them off.
        (is (await (:timeout 10) (= 2 (hash-table-count (ncm::matrix-book-marks (ncm::matrix-adapter-book adapter))))))
        (nck:execute-plan (nck:host-executor host)
                          (ncm:edit-plan (ncm::matrix-adapter-book adapter) target card "done"))
        (is (await (:timeout 10) (= 2 (length (matrix-calls fake "redact"))))
            "a settled card's controls are taken off")))))

(deftest channel-matrix-probe-names-what-the-token-sees ()
  (with-fake-matrix (fake)
    (fake-matrix-room fake "!dm:fake" :members '("@kim:fake"))
    (fake-matrix-room fake "!room:fake" :members '("@kim:fake" "@lee:fake") :name "General")
    (fake-matrix-room fake "!secret:fake" :members '("@kim:fake" "@lee:fake") :name "Secret" :encrypted t)
    (with-temp-file (token-path :contents "syt_fake" :type "txt")
      (let* ((section (matrix-section fake token-path nil "allowed_users" (vector "@kim:fake")))
             (text (ncm:probe-channel section)))
        (is (search "token: ok -- @bot:fake (device DEV1)" text) text)
        (is (search "DM with kim !dm:fake, General !room:fake, Secret !secret:fake (encrypted: not served)" text) text)
        (is (search "people: kim @kim:fake, lee @lee:fake" text) text)
        (is (not (search "syt_fake" text)) "the token is never said")
        (is (equal '(("!dm:fake" . "DM with kim") ("!room:fake" . "General") ("!secret:fake" . "Secret"))
                   (ncm:probe-channel-choices section)))
        (is (equal '(("@kim:fake" . "kim") ("@lee:fake" . "lee")) (ncm:probe-user-choices section)))))))
