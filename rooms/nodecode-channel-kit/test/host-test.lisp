;;;; host-test.lisp --- the digest flow through the host, over a test platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The delivery half of the host without threads-of-uncertainty: folds run
;;;; on the test thread (as the :FRAME hook would), the status flush is
;;;; driven directly, and only the terminal finish rides the real worker.
;;;; Platform-neutral by construction — the plans are the test platform's.

(in-package #:nodecode.test)

(defmacro with-digest-host ((host lane &key responses (workers 1) (name "digest-flow")
                             (session "chat-123-m1") (trigger "m1") host-keys)
                            &body body)
  "A host with one interned lane for SESSION — LANE NIL interns none — and a
live delivery pool, torn down after BODY."
  ;; EXECUTOR is bound around it as the
  ;; recording executor, HOST-KEYS ride into TEST-HOST, and BODY gets the
  ;; moves every digest test makes: (FACT KIND PAYLOAD [TURN-ID]), one fact
  ;; folded into SESSION's turn, (SAID TEXT [TURN-ID]), the round that says TEXT,
  ;; (CALLED CALL-ID [ARGUMENTS]), the turn's eval call CALL-ID starting,
  ;; (EARN-LINE [TURN-ID]), a started turn's eval call and its status line flushed,
  ;; (PLANS), the plans the executor has recorded so far, (LABELLED LABEL), those
  ;; under the audit LABEL, (POSTED N), whether it has recorded N of them within ten
  ;; seconds, (SAY TEXT USER MESSAGE), USER typing TEXT as MESSAGE in room 123,
  ;; (PRESS ID VERB USER), USER pressing SESSION's VERB control as interaction ID,
  ;; and (RECAP SAID [:ANNOUNCE] [:TURN-ID]), an experience recap folded back as a
  ;; recorded exchange, ANNOUNCE or not, whose round says SAID.
  `(let* ((executor (nck:make-recording-executor :responses ,responses))
          (,host (test-host :executor executor :name ,name :workers ,workers ,@host-keys)))
     (nlk:with-cleanup ((nck:stop-delivery-worker (nck:host-worker ,host)))
       (let (,@(when lane
                 `((,lane (nck:intern-lane (nck:host-lanes ,host) ,session
                                           :target '(:channel-id "123")
                                           :trigger-message-id ,trigger)))))
         ,@(when lane `((declare (ignorable ,lane))))
         (labels ((fact (kind payload &optional (turn-id "t1"))
                    (fold-fact ,host ,session kind payload turn-id))
                  (said (text &optional (turn-id "t1"))
                    (fact "turn.assistant_message_completed"
                          (nlk:json-object "message" (nlk:json-object "content" text)) turn-id))
                  (called (call-id &optional arguments)
                    (fact "turn.tool_call_started"
                          (nlk:json-object "tool-name" "eval" "call-id" call-id
                                           :opt "arguments" arguments)))
                  (earn-line (&optional (turn-id "t1"))
                    (fact "turn.started" (nlk:json-object) turn-id)
                    (fact "turn.tool_call_started" (nlk:json-object "tool-name" "eval") turn-id)
                    (nck:flush-now ,host ,lane))
                  (plans () (nck:recording-executor-plans executor))
                  (labelled (label) (plans-matching (plans) :label label))
                  (posted (n)
                    (await (:timeout 10) (>= (length (plans)) n)))
                  (say (text user message)
                    (nck:handle-candidate ,host (test-candidate :channel "123" :message message
                                                                :text text :user user
                                                                :user-name user)))
                  (press (id verb user)
                    (nck:control-pressed ,host (list :id id :user-id user :message-id "s1"
                                                     :data (format nil "nck:~a:~a" verb ,session)
                                                     :channel-id "123")))
                  (recap (text &key announce (turn-id "t1"))
                    (let ((input (format nil "[experience] recap of the turn above, ~
                                              by a reflection of this session")))
                      (fact "turn.input_committed"
                            (nlk:json-object "role" "user" "message" input "disposition" "recorded"
                                             :when announce "announce" t)
                            turn-id)
                      (fact "turn.started" (nlk:json-object "input" input) turn-id)
                      (said text turn-id))))
           (declare (ignorable #'fact #'said #'called #'earn-line #'plans #'labelled #'posted
                               #'say #'press #'recap))
           ,@body)))))

(deftest channel-host-digest-flow (with-digest-host (host lane :responses (replies "s1" "s1" "a1" "s1")))
  (fact "turn.started" (nlk:json-object))
  (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
  (said "probing the registry")
  (is (null (plans)) "hook-thread folds perform no I/O")
  (nck:flush-now host lane)
  (is-present (post (first (plans))) "the first flush posts the card"
    (is-plan post :label "send_message" :path "/rooms/123/messages" "reply_to" "m1" "ping" nil)
    (is-shape (gethash "card" (nck:request-plan-body post))
      (:state :working) (:headline "Running just lint"))
    (is-carrying (content (plan-content post))
      "1 step" (:absent "round" "the card counts steps, never rounds")
      (:absent "probing the registry" "a round's words leave as their own message")))
  ;; The thought that chose the work lands on the card, as its headline.
  (fold-delta host "chat-123-m1" "Weighing the lint output. Then")
  (nck:flush-now host lane)
  (is-present (edit (second (plans))) "the second flush edits the card in place"
    (is-plan edit :label "edit_message" :method "PATCH" :path "/rooms/123/messages/s1")
    (is (search "thinking · Weighing the lint output." (plan-content edit))))
  (said "the final answer")
  (fact "turn.completed" (nlk:json-object))
  (is (posted 4) "the finish job posts the answer and settles the card")
  (let ((all (plans)))
    (is-present (final (third all)) "the answer is a fresh message, not an edit of the card"
      (is-plan final :label "send_message" "reply_to" "m1" "ping" t)
      (is (equal "the final answer" (plan-content final)) "its words alone: the card says the work"))
    (is-present (settle (fourth all))
      "and the card settles above it as the turn's record — never deleted"
      (is-plan settle :label "edit_message" :method "PATCH" :path "/rooms/123/messages/s1")
      (is-shape (gethash "card" (nck:request-plan-body settle)) (:state :done))
      (is (uiop:string-prefix-p "Done in " (plan-content settle))))
    (is (= 4 (length all)))
    (is (eq lane (nck:lane-for-address (nck:host-lanes host) "a1")))))

(deftest channel-host-a-waiting-turn-keeps-the-ask-open ()
  ;; The turn ended with its own background evaluation still in flight: the
  ;; model's word was status, not the ask's answer. The card the ask watches
  ;; stays up saying what the wait is, the word on it, nothing is delivered
  ;; as the answer — and the exit wake's turn takes the same card over and
  ;; answers for real.
  (with-digest-host (host lane :responses (replies "s1" "a1" "a2"))
    (fact "turn.started" (nlk:json-object "input" "link me the report"))
    (called "c1")
    (nck:flush-now host lane)
    (is-present (line (first (plans))) "the ask's line is up while the turn works"
      (is (equal "send_message" (nck:request-plan-audit-label line))))
    (said "On it — digging through it.")
    (fact "turn.completed" (nlk:json-object "background-pending" 1))
    ;; Its word rides the card it watches, never a message of its own.
    (is (await (:timeout 10) (equal "On it — digging through it."
                                    (getf (nck:digest-card (nck:lane-digest lane) 9999999999) :said))))
    (is (= 1 (length (labelled "send_message"))))
    (is (null (plan-matching (plans) :label "delete_message")))
    (is (eql 1 (nck:turn-digest-background-pending (nck:lane-digest lane))))
    (is (search "Waiting on a background evaluation"
                (nck:card-text (nck:digest-card (nck:lane-digest lane) 9999999999))))
    ;; The exit wake's turn: same lane, the same ask's line.
    (fact "turn.started"
          (nlk:json-object "input" "[background eval 7 exited 0] done")
          "t2")
    (is (equal "t2" (nck:turn-digest-turn-id (nck:lane-digest lane))))
    (is (null (nck:turn-digest-background-pending (nck:lane-digest lane))))
    (is (equal "link me the report" (nck:lane-prompt lane)))
    (said "Here it is — the report." "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is-present (answer (await-plan executor :ping t)) "the answer posts, alone"
      (is-plan answer "reply_to" "m1" "content" "Here it is — the report."))
    ;; The card the ask watched settles as the turn's record.
    (is (await-plan executor :label "edit_message" :content "Done in"))))

(deftest channel-host-an-exchange-records-what-was-said-while-it-waited ()
  ;; The room's record of an ask names what the model said while the turn
  ;; waited on its own background work, then the answer — the turn that ends
  ;; the work would otherwise record an exchange missing everything spoken
  ;; before the result arrived.
  (let ((digest (nck::make-turn-digest :turn-id "t1" :started-at-ms 0)))
    (nck::digest-note-waiting-words digest "On it — digging.")
    (is (equal (format nil "On it — digging.~%~%Here it is.")
               (nck:recorded-answer-text digest "Here it is.")))
    (is (null (nck::turn-digest-waiting-words digest)))))

(deftest channel-host-what-a-turn-says-stays-on-its-card ()
  ;; A round that spoke and kept working: its words show on the card, and
  ;; never as a message of their own; the answer that ends the turn is the
  ;; one message, with nothing above it to be set apart from.
  (with-digest-host (host lane :responses (replies "s1" "a1"))
    (fact "turn.started" (nlk:json-object))
    (fact "turn.assistant_message_completed"
          (nlk:json-object
           "message" (nlk:json-object
                      "content" "the registry answers first"
                      "tool_calls" (vector (nlk:json-object "id" "call-1")))))
    (fact "turn.tool_call_started" (nlk:json-object "tool-name" "eval"))
    (nck:flush-now host lane)
    (is-present (card (await-plan executor :label "send_message")) "the card posts, the words on it"
      (is (equal "the registry answers first" (getf (plan-field card "card") :said))))
    (said "the registry is fine")
    (fact "turn.completed" (nlk:json-object))
    (is-present (answer (await-plan executor :ping t)) "the answer posts"
      (is (equal "the registry is fine" (plan-content answer))))
    (is (= 2 (length (labelled "send_message"))) "the card and the answer, nothing else")))

(defmacro with-streaming-host ((host lane &key responses) &body body)
  "WITH-DIGEST-HOST whose section streams (channels.<id>.stream), its turn
started nine seconds ago, past the eight a writing turn waits to earn its
card; BODY also gets (WRITES TEXT), one text delta."
  `(with-digest-host (,host ,lane :responses ,responses :name "streaming")
     (setf (nck::host-stream-p ,host) t)
     (fact "turn.started" (nlk:json-object))
     (setf (nck::turn-digest-started-at-ms (nck:lane-digest ,lane)) (- (nck:now-ms) 9000))
     (flet ((writes (text) (fold-delta ,host "chat-123-m1" text :type "text")))
       ,@body)))

(deftest channel-host-a-streamed-answer-posts-fresh ()
  ;; channels.<id>.stream, on unless a section says otherwise: the words a
  ;; round writes show on its card as they come, the cursor at their end; the
  ;; answer then posts fresh, pinging its asker, and leaves the card. No
  ;; message carries words on their way. A turn that fails says so on its card.
  (is (nck::host-stream-p (nck:make-host-from-section (nlk:make-json-object) (test-platform))))
  (is (not (nck::host-stream-p (nck:make-host-from-section (nlk:make-json-object "stream" nil)
                                                           (test-platform)))))
  (with-streaming-host (host lane :responses (replies "s1" "a1"))
    (writes "The registry answers on port 80, ")
    (nck::schedule-flush host lane)
    (is-present (card (await-plan executor :label "send_message")) "a writing turn earns its card"
      (is-plan card "reply_to" "m1" "ping" nil)
      (is (equal (writing-cursor "The registry answers on port 80,") (getf (plan-field card "card") :said))))
    (said "The registry answers on port 80, and on 443.")
    (fact "turn.completed" (nlk:json-object))
    (is-present (answer (await-plan executor :ping t)) "the answer posts fresh"
      (is-plan answer "reply_to" "m1" "content" "The registry answers on port 80, and on 443."))
    (is-present (settle (await-plan executor :label "edit_message" :content "Done in")) "the card settles"
      (is (null (getf (plan-field settle "card") :said)) "the answer's words left it"))
    (is (null (labelled "delete_message")) "no message carried words on their way"))
  (with-streaming-host (host lane :responses (replies "s1"))
    (writes "The registry answers on port 80, and then")
    (nck::schedule-flush host lane)
    (is (await-plan executor :label "send_message"))
    (fact "turn.failed" (nlk:json-object "detail" "provider 502"))
    (is (await-plan executor :content "> provider 502"))
    (is (null (labelled "delete_message")))))

(deftest channel-host-an-answer-is-said-in-a-voice-message-as-the-room-asks ()
  ;; channels.<id>.voice_replies: off, the default, an answer is words alone;
  ;; on, an ask said in a voice message is answered in one too; tts, every ask
  ;; is. An ask said in a voice channel is answered out loud there, not twice.
  ;; The voice message follows the answer, and a voice that cannot speak costs
  ;; one line saying why, never the answer.
  (is-table (mode voice due) (eq due (nck::voice-reply-due-p mode voice))
    ("off" :note nil) ("on" nil nil) ("on" :note t) ("tts" nil t) ("tts" :channel nil))
  (is (equal "off" (nck::host-voice-replies
                    (nck:make-host-from-section (nlk:make-json-object) (test-platform)))))
  (let ((speech nck:*speech*))
    (nlk:with-cleanup ((setf nck:*speech* speech))
      ;; SETF, not LET: the voice message is made on the delivery pool's thread.
      (setf nck:*speech* (nlk:make-json-object "enabled" nil))
      (with-digest-host (host lane :name "voice-replies"
                                   :host-keys (:platform (test-platform :files t)))
        (setf (nck::host-voice-replies host) "tts")
        (fact "turn.started" (nlk:json-object))
        (said "The registry answers on port 80.")
        (fact "turn.completed" (nlk:json-object))
        (let ((notice (await-plan executor :content "no voice message (speech is turned off")))
          (is-present notice "the room hears why there is no voice message"
            (is-plan notice "ping" nil "reply_to" nil)))
        (is (< (position-if (lambda (plan) (plan-matching-p plan :content "port 80")) (plans))
               (position-if (lambda (plan) (plan-matching-p plan :content "no voice message"))
                            (plans))))))))

(deftest channel-host-streamed-words-a-round-said-stay-on-its-card ()
  ;; A round that wrote its words and called a tool: they stay on the card as
  ;; what it said, the cursor gone, and the answer posts alone.
  (with-streaming-host (host lane :responses (replies "s1" "a1"))
    (writes "Looking at the registry before I answer.")
    (nck::schedule-flush host lane)
    (is (await-plan executor :label "send_message"))
    (fact "turn.assistant_message_completed"
          (nlk:json-object
           "message" (nlk:json-object "content" "Looking at the registry before I answer."
                                      "tool_calls" (vector (nlk:json-object "id" "call-1")))))
    (is (await (:timeout 10)
          (progn (nck::schedule-flush host lane)
                 (find "Looking at the registry before I answer." (labelled "edit_message")
                       :key (lambda (plan) (getf (plan-field plan "card") :said)) :test #'equal))))
    (said "the registry is fine")
    (fact "turn.completed" (nlk:json-object))
    (is-present (answer (await-plan executor :ping t)) "the answer posts"
      (is (equal "the registry is fine" (plan-content answer))))
    (is (= 2 (length (labelled "send_message"))) "the card and the answer")))

(deftest channel-host-a-call-is-a-step-not-a-message ()
  ;; A call posts no message of its own: it is a step on the card, and the
  ;; room carries the turn's own words. The room-wide trail this replaced
  ;; posted every call for everyone.
  (with-digest-host (host lane :responses (replies "s1" "s1" "s1"))
    (fact "turn.started" (nlk:json-object))
    (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
    (nck::schedule-flush host lane)
    (is (posted 1) "the card goes out")
    (sleep 0.3)
    (is (= 1 (length (plans))) "and no call message follows")
    (is-carrying (content (plan-content (first (plans))))
      (:= (format nil "Running just lint · 0s~%› Running just lint · 0s~%1 step")))))

(deftest channel-host-a-recorded-exchange-is-silent-unless-announced ()
  ;; A recap folded back by the experience cell is a RECORDED exchange: a
  ;; settled turn nobody asked. The lane answers asks, so it plans no chrome
  ;; and posts nothing — while an ANNOUNCED one ends in a single silent note.
  ;; The room write-back needs a store and is the experience suite's; here the
  ;; fold's whole visible contract is the silence and that one note.
  (with-digest-host (host lane :responses (replies "n1"))
    (recap "asked: a thing")
    (nck:flush-now host lane)
    (is (null (plans)) "a recorded exchange plans no chrome")
    (is (null (nck:lane-digest lane)) "and never opens a digest")
    (is (null (nck:lane-active-turn-id lane)) "nor counts as a running turn")
    (fact "turn.completed" (nlk:json-object))
    (nck:flush-now host lane)
    (is (null (plans)) "and settles with nothing posted")
    (is (null (gethash lane nck::*recorded-turns*)) "the fold's entry is gone")
    ;; Announced: the same flow ends in one note that @s the operator.
    (recap "it broke" :announce t :turn-id "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is (posted 1) "the announced recap posts its one note")
    (is-present (note (first (plans))) "one message"
      (is-plan note :label "send_message" "ping" nil "reply_to" nil)
      (is (equalp #("mike") (gethash "mentions" (nck:request-plan-body note))))
      (is (equal (format nil "~a~%~%~a" nck::+note-head+ "it broke")
                 (plan-content note))))
    (is (= 1 (length (plans))) "nothing beside the note")
    (is (eq lane (nck:lane-for-address (nck:host-lanes host) "n1")))))

(deftest channel-host-an-undeclared-room-notes-without-an-address ()
  ;; A room that declares no operator: the note posts, and mentions nobody.
  (with-digest-host (host lane :host-keys (:owners nil) :responses (replies "n1"))
    (recap "it broke" :announce t :turn-id "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is (posted 1) "the announced recap posts its one note")
    (is (null (gethash "mentions" (nck:request-plan-body (first (plans))))))))

(deftest channel-host-parked-input-shows-on-the-lane-line ()
  ;; A steer parked on the lane's session while its turn runs is a ⌎ row
  ;; under the running status line — the same queue snapshot the TUI folds
  ;; into its pending band — and its promotion clears the row. A snapshot
  ;; for a turn this lane is not running is dropped.
  (with-digest-host (host lane :responses (replies "s1" "s1"))
    (flet ((parked (turn-id &rest prompts)
             (fact "session_input_queue_snapshot_updated"
                   (nlk:json-object "snapshot" (nlk:json-object "pending_prompts"
                                                                (coerce prompts 'vector)))
                   turn-id)))
      (fact "turn.started" (nlk:json-object))
      (nck:flush-now host lane)
      (is (null (plans)) "a quiet turn has no line yet")
      (parked "t1" (nlk:json-object "prompt_id" "p1"
                                    "content" "mila [m2 u8]: also run the tests"
                                    "disposition" "steer"))
      (is (null (plans)) "the fold performs no I/O")
      (nck:flush-now host lane)
      (is-present (post (first (plans))) "the parked steer opens the line at once"
        (is (equal "send_message" (nck:request-plan-audit-label post)))
        (let ((content (plan-content post)))
          (is (search (format nil "~%⌎ also run the tests — after this round")
                      content))
          (is (uiop:string-prefix-p "Working · " content))))
      (parked "t-other" (nlk:json-object "prompt_id" "p9"
                                         "content" "someone else's"
                                         "disposition" "steer"))
      (is (equal 1 (length (nck:turn-digest-pending (nck:lane-digest lane)))))
      (parked "t1")
      (nck:flush-now host lane)
      (is-present (edit (second (plans)))
        "the promotion's empty snapshot takes the row off the line"
        (is (equal "edit_message" (nck:request-plan-audit-label edit)))
        (is-carrying (content (plan-content edit)) (:absent "⌎" "no row is left")
          (is (uiop:string-prefix-p "Working · " content)))))))

(defun room-1-ask (message)
  "The ask MESSAGE from u8 opens in room 1, as its lane chat-1-MESSAGE."
  (nck:make-ask :room "chat-1" :lane (format nil "chat-1-~a" message) :owner-id "u8"
                :message-id message :target '(:channel-id "1") :prompt "hi"))

(deftest channel-host-reactions-follow-the-ask ()
  ;; Opt-in: the ask carries the eye on the message itself from admission
  ;; through the working turn, and the check once its answer has landed.
  ;; Off, nothing is ever planned.
  (with-digest-host (host nil :responses (replies "s1" "ok" "ok" "a1" "ok" "ok")
                     :name "reactions" :session "chat-1-m7"
                     :host-keys (:max-concurrent 0 :reactions t))
    ;; No slot at all: the ask queues, and is marked seen at once.
    (nck::gate-ask host (room-1-ask "m7"))
    (is (posted 2) "the queued line and the reaction both go out")
    (is-present (seen (first (labelled "reaction"))) "the ask is marked seen"
      (is-plan seen :method "PUT" :path "/rooms/1/messages/m7/reaction"
                    "emoji" nck:+reaction-seen+))
    (is-present (queued (first (labelled "send_message")))
      "the queued line is a silent reply to the ask"
      (is-plan queued "reply_to" "m7" "ping" nil "content" "Queued"))
    ;; The gate's book still lists the ask as waiting; this test
    ;; drives the turn's facts itself (the kernel is not here), so
    ;; the entry is taken off by hand — else the finish job's
    ;; promotion would try to admit it for real.
    (nck:release-slot host "u8")
    (let ((lane (nck:find-lane (nck:host-lanes host) "chat-1-m7")))
      (fact "turn.started" (nlk:json-object))
      (is (not (await (:timeout 0.3) (>= (length (labelled "reaction")) 2))))
      (is (equal "m7" (nck:turn-digest-ask-id (nck:lane-digest lane))))
      (said "done")
      (fact "turn.completed" (nlk:json-object))
      (is (await (:timeout 10) (>= (length (labelled "reaction")) 3)))
      (is-plan (second (labelled "reaction")) :method "DELETE" "emoji" nck:+reaction-seen+)
      (is-plan (third (labelled "reaction")) :method "PUT" "emoji" nck:+reaction-done+)
      (is (null (nck:lane-reactions lane)) "the outcome is no turn's to clear")))
  ;; Off by default: the same admission plans no reaction.
  (with-digest-host (host nil :responses (replies "s1") :name "no-reactions"
                     :host-keys (:max-concurrent 0))
    (nck::gate-ask host (room-1-ask "m8"))
    (is (posted 1))
    (sleep 0.2)
    (is (every (lambda (plan)
                 (equal "send_message" (nck:request-plan-audit-label plan)))
               (plans)))))

(defun test-reaction (&key (channel "123") (on "a1") (user "u9") (user-name "alice")
                           (emoji "❤️") (action "add"))
  "One reaction candidate as an adapter's route normalizes it: the emoji as
its text, the message it sits on as its reply gesture, no message id of its
own — a reaction is not a message — and addressed, having been left on a
message of ours."
  (test-candidate :channel channel :message nil :reply-to on :user user :user-name user-name
                  :text emoji :reaction action :addressed t))

(deftest channel-host-a-reaction-on-our-message-notices-never-answers ()
  ;; A reaction is a line the room said without typing one — and a notice,
  ;; never an actionable trigger: it opens no turn and steers none, whoever
  ;; left it and whether it was added or taken back. Turns start from
  ;; words, never from a gesture. Every accepted one becomes one line of
  ;; chatter the room's next ask carries. The emoji itself is never read
  ;; here: what a room means by one is the model's to read, not a table's
  ;; to decide. Only ever on a message the kit posted and still holds an
  ;; address for.
  (with-digest-host (host lane :name "reaction-inbound" :session "chat-123-m1"
                     :host-keys (:max-concurrent 0 :owners '("mike")))
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("123"))
          (nck:lane-owner-id lane) "u8")
    ;; a1 is a message this lane posted: the address book is what says "mine".
    (nck:bind-lane-address (nck:host-lanes host) "a1" "chat-123-m1")
    (is-each (nck:reaction-noticed host)
      ;; An operator's reaction is chatter like anyone's: no turn, no queue.
      ((test-reaction :user "mike" :user-name "mike" :emoji "✅") :observe "the operator's")
      ;; The lane's own asker's reaction is chatter too.
      ((test-reaction :user "u8" :user-name "owner" :emoji "🎉") :observe "the asker's")
      ;; Anybody else's is chatter: no turn, no post, no ping.
      ((test-reaction) :observe "anybody else's")
      ;; So is one the operator takes back — a turn already run cannot be unrun.
      ((test-reaction :user "mike" :user-name "mike" :emoji "✅" :action "remove") :observe
       "the operator's, taken back"))
    (is (null (nck:release-slot host "mike")) "no ask was queued")
    (is (null (nck:release-slot host "u8")) "no ask was queued for the asker either")
    (let ((ambient (nck:drain-ambient host "chat-123")))
      (is-shape ambient (length = 4 "one line each, and nothing was posted")
        (first "mike (operator) [umike ra1]: reacted ✅") (second "owner [uu8 ra1]: reacted 🎉")
        (third "alice [uu9 ra1]: reacted ❤️") (fourth "mike (operator) [umike ra1]: took back ✅")))
    (is (null (plans)) "no gesture ever posted a message")
    ;; A reaction on a message no lane of ours holds is not ours to read.
    (is (null (nck:reaction-noticed host (test-reaction :on "someone-elses"))))
    ;; And the room's own read gates still answer first.
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("999")))
    (is (eq :reject (nck:reaction-noticed host (test-reaction))))
    (is (equal "channel_not_allowed"
               (getf (nck:channel-status "test") :last-rejection)))))

(deftest channel-host-terminal-clears-the-completed-turn-reactions ()
  ;; A steer changes the lane's current ask before the old turn's terminal
  ;; delivery runs. Clear the old turn's marker while preserving the queued
  ;; follow-up's fresh marker for its own turn.
  (with-digest-host (host lane :name "reaction-generations" :host-keys (:reactions t))
    (fact "turn.started" (nlk:json-object))
    (is (await (:timeout 10) (= 1 (length (plans)))))
    (setf (nck:lane-trigger-message-id lane) "m2")
    (nck::schedule-reaction host lane "m2" nck:+reaction-seen+)
    (is (await (:timeout 10) (= 2 (length (plans)))))
    (fact "turn.completed" (nlk:json-object))
    (is (await (:timeout 10) (= 3 (length (plans)))))
    (is-plan (third (plans)) :method "DELETE" :path "/rooms/123/messages/m1/reaction")
    (is (equal '(("m2" . "👀")) (nck:lane-reactions lane)))))

(deftest channel-host-reactions-say-the-outcome ()
  ;; A failed turn leaves the cross; a stopped one leaves the message bare.
  (loop for (kind payload outcome) in `(("turn.failed" ,(nlk:json-object "detail" "provider 502")
                                                       ,nck:+reaction-failed+)
                                        ("turn.cancelled" ,(nlk:json-object "reason" "stop") nil))
        do (with-digest-host (host lane :name "reaction-outcome" :host-keys (:reactions t))
             (fact "turn.started" (nlk:json-object))
             (is (await (:timeout 10) (labelled "reaction")))
             (fact kind payload)
             (is (await (:timeout 10) (>= (length (labelled "reaction")) (if outcome 3 2))))
             (sleep 0.1)
             (is (equal (if outcome
                            (list "PUT" "DELETE" "PUT")
                            (list "PUT" "DELETE"))
                        (mapcar #'nck:request-plan-method (labelled "reaction")))
                 kind)
             (when outcome
               (is-plan (third (labelled "reaction")) :path "/rooms/123/messages/m1/reaction"
                                                      "emoji" outcome)))))

(deftest channel-host-an-answer-the-room-refused-says-so ()
  ;; 2026-10-03: Discord refused a thread lane's answer, and the turn's
  ;; working line stayed up with its stop under a check on the ask: four
  ;; presses found no turn to stop, and the asker never learnt the answer
  ;; was lost. The card settles failed into the refusal and loses its stop —
  ;; no delete, it is the one word the room has, and its steps stay with
  ;; Details on it — and the ask takes the cross.
  (with-digest-host (host lane :name "answer-refused"
                     :responses (list (reply 200 "id" "s1") (reply 400 "message" "Invalid Form Body"))
                     :host-keys (:platform (test-platform :choices t)))
    (earn-line)
    (said "the final answer")
    (fact "turn.completed" (nlk:json-object))
    (is-present (settle (await-plan executor :label "edit_message"
                                             :content "The answer could not be posted"))
      "the card says the answer did not post"
      (is-plan settle :path "/rooms/123/messages/s1")
      (is (eq :failed (getf (plan-field settle "card") :state)) "a failed card")
      (is (search "status 400" (getf (plan-field settle "card") :note)) "and why")
      (is (equal '("Details") (mapcar #'first (plan-field settle "controls"))) "its stop goes, Details stays"))
    (is (null (labelled "delete_message")))
    (is (null (nck::turn-digest-status-id (nck:lane-digest lane))) "the lane holds no line"))
  ;; A turn that earned no line says it in one, a silent reply to the ask.
  (with-digest-host (host lane :name "answer-refused-bare" :host-keys (:reactions t)
                     :responses (list (reply 200) (reply 400 "message" "Invalid Form Body")))
    (fact "turn.started" (nlk:json-object))
    (is (await (:timeout 10) (labelled "reaction")))
    (said "the final answer")
    (fact "turn.completed" (nlk:json-object))
    (is (await (:timeout 10) (= 3 (length (labelled "reaction")))))
    (is-present (notice (plan-matching (labelled "send_message")
                                       :content "the answer could not be posted"))
      "one line says the answer did not post"
      (is-plan notice "reply_to" "m1" "ping" nil))
    (is-plan (third (labelled "reaction")) :method "PUT" "emoji" nck:+reaction-failed+)))

(deftest channel-host-a-steered-turn-hands-its-marks-on ()
  ;; A steer cuts the turn short and the turn it opens answers both lines:
  ;; the first ask's eye holds through the handover, and the answer's check
  ;; lands on both messages.
  (with-digest-host (host lane :name "reaction-steer" :host-keys (:reactions t))
    (fact "turn.started" (nlk:json-object))
    (is (await (:timeout 10) (= 1 (length (labelled "reaction")))))
    (setf (nck:lane-trigger-message-id lane) "m2")
    (nck::schedule-reaction host lane "m2" nck:+reaction-seen+)
    (is (await (:timeout 10) (= 2 (length (labelled "reaction")))))
    (fact "turn.completed" (nlk:json-object "steered" t))
    ;; The cut turn touches neither mark.
    (is (not (await (:timeout 0.5) (> (length (labelled "reaction")) 2))))
    (fact "turn.started" (nlk:json-object) "t2")
    (said "both, answered" "t2")
    (fact "turn.completed" (nlk:json-object) "t2")
    (is (await (:timeout 10) (= 6 (length (labelled "reaction")))))
    (is (equal '("m1" "m2")
               (sort (loop for plan in (nthcdr 2 (labelled "reaction"))
                           when (equal nck:+reaction-done+ (plan-field plan "emoji"))
                             collect (ppcre:scan-to-strings "m[0-9]+" (nck:request-plan-path plan)))
                     #'string<)))
    (is (null (nck:lane-reactions lane)))))

(deftest channel-host-failure-settles-into-the-chrome ()
  ;; A failed turn is the one case where the line IS the message: nothing is
  ;; posted beside it — and its ask stays in the room's record with the notice.
  (with-digest-host (host lane :responses (replies "s1"))
    (fact "turn.started" (nlk:json-object))
    (fact "turn.failed" (nlk:json-object "detail" "provider 502"))
    (is (await (:timeout 10) (plans)))
    (is (= 1 (length (plans))))
    (is-present (notice (first (plans))) "the notice posts"
      (is (equal "send_message" (nck:request-plan-audit-label notice)))
      (is-carrying (content (plan-content notice)) "Failed at" "> provider 502"))))

(deftest channel-host-a-cancelled-turn-keeps-its-ask-in-the-room (with-temp-store ())
  ;; A failed or cancelled turn is its status line — and the ask it was given
  ;; stays in the room's record with the notice as the assistant half: an ask
  ;; that was stopped is still something the room said, so the next lane
  ;; forked above sees it. (2026-09-16: the operator's "Yo", stopped, was
  ;; invisible to the lane that asked what it saw above.)
  (with-digest-host (host lane :responses (replies "s1") :name "cancel-record")
    (setf (nck:lane-parent-session-id lane) "chat-123")
    (nlk:create-session :id "chat-123")
    (nlk:record-exchange-turn "chat-123" "bob [m3 u2]: is the deploy up" "yes")
    (fact "turn.started" (nlk:json-object "input" "alice [m1 u2]: yo"))
    (fact "turn.cancelled" (nlk:json-object "reason" "stopped from the room"))
    (is (await (:timeout 10) (>= (count-session-events "chat-123" "turn.completed") 2)))
    (is (= 2 (count-session-events "chat-123" "turn.started")))))

(deftest channel-host-reconciles-a-missed-terminal-frame (with-temp-store ())
  ;; A durable terminal fact can land after the live frame is missed. The tick
  ;; must close the running digest and let the normal terminal delivery settle
  ;; its captured chrome instead of leaving a permanent working line.
  (with-digest-host (host lane :responses (replies "s1"))
    (let* ((session-id "chat-123-m1")
           (turn (progn
                   (ensure-durable-session session-id)
                   (nlk:admit-turn session-id "command-terminal-recovery"
                                  "the question")))
           (turn-id turn.turn-id))
      ;; Fold only the opening and the status-worthy tool fact into the
      ;; channel. Deliberately omit the terminal frame below.
      (earn-line turn-id)
      (nlk:complete-turn turn nil)
      (is (eq :running (nck:turn-digest-phase (nck:lane-digest lane))))
      (is (nck::reconcile-terminal-digest host lane))
      (is (await () (>= (length (plans)) 2)))
      (is (equal turn-id (nck:turn-digest-turn-id (nck:lane-digest lane))))
      (is (eq :completed (nck:turn-digest-phase (nck:lane-digest lane))))
      (is (equal "delete_message" (nck:request-plan-audit-label (second (plans))))))))

(deftests channel-host (spellings)
    (dolist (spelling spellings)
      (with-digest-host (host lane :responses (replies "s1"))
            (earn-line)
        (when spelling (said (format nil " ~%~a~% " spelling)))
        (fact "turn.completed" (nlk:json-object))
        (is (posted 2) "a completion with no answer to post deletes its existing line")
        (is (= 1 (length (labelled "send_message"))) (format nil "~s posts no answer" spelling))
        (is-plan (second (plans))
                 :label "delete_message" :method "DELETE" :path "/rooms/123/messages/s1")
        (is (= 2 (length (plans))))
        (is-shape (nck:lane-digest lane) (nck:turn-digest-status-id null)
          (nck:turn-digest-answer null))))
  ;; A completed turn whose live digest missed every assistant fact must
  ;; remove its temporary working line without leaving a failure tombstone.
  (completed-without-answer-settles-chrome '(nil))
  ;; SILENT is a control value, not a Discord reply: a turn that already
  ;; earned chrome deletes that line and emits no answer message. The no-post
  ;; sentinel is not one spelling: SILENT, [SILENT], NO_REPLY and NO REPLY all
  ;; settle the turn quietly — trimmed and case-insensitive — with no answer
  ;; post.
  (silence-spellings-all-settle-quietly '("SILENT" "[SILENT]" "[silent]" "No_Reply" "no reply")))

(deftest channel-host-completed-without-answer-is-silent-without-chrome ()
  ;; A quick no-response turn never earns chrome and must remain completely
  ;; absent from the platform surface.
  (with-digest-host (host lane :responses nil)
    (fact "turn.started" (nlk:json-object))
    (fact "turn.completed" (nlk:json-object))
    (is (await (:timeout 10) (eq :completed
                                  (nck:turn-digest-phase (nck:lane-digest lane)))))
    (is (null (plans)))))

(deftest channel-host-says-when-the-model-moved ()
  ;; A durable turn.provider_fallback folds onto the card as one line naming
  ;; the model the turn moved to and why — the room's half of the engine's
  ;; failover fact, never a second post.
  (with-digest-host (host lane :responses (replies "s1"))
    (fact "turn.started" (nlk:json-object))
    (fact "turn.provider_fallback"
          (nlk:json-object "from-provider" "deepseek"
                           "from-model" "deepseek-flash"
                           "to-provider" "a6api"
                           "to-model" "deepseek-v4-flash-free"
                           "reason" "overloaded"
                           "status" 503))
    (nck:flush-now host lane)
    (is-present (post (first (plans))) "the failover opens the line"
      (is (search "fell back to deepseek-v4-flash-free (overloaded)"
                  (plan-content post))))))

(deftest channel-host-a-sentence-carrying-the-silence-word-is-spoken ()
  ;; Only the sentinel itself is silent: the same word inside a sentence is
  ;; the answer, said out loud.
  (with-digest-host (host lane :responses (replies "s1"))
    (said "I will stay NO_REPLY until Friday")
    (fact "turn.completed" (nlk:json-object))
    (let ((send (await-plan executor :ping t)))
      (is-present send "the answer posts"
        (is (search "NO_REPLY until Friday" (plan-content send)))))))

(deftest channel-host-a-stop-press-is-acted-on-where-it-arrives (with-digest-host (host lane))
  ;; The room's one escape from a running turn does not queue behind the
  ;; delivery pool, and does not wait out the acknowledgement's round trip
  ;; to the platform: cancelling is not a platform call. The press is acted
  ;; on inside CONTROL-PRESSED, on the thread it arrived on, so a handler
  ;; around the press sees what the stop said — a worker's warning would be
  ;; raised on another thread and never reach here.
  ;; (2026-09-16: a stop pressed at 75s reached the turn at 135s.)
  (let ((said (warnings-of (press "i1" "stop" "mike"))))
    (is (search "no active turn" (format nil "~{~a~}" said))))
  ;; Details is a worker's: its answer is platform work and belongs off the
  ;; transport thread.
  (let ((said (warnings-of (press "i2" "details" "stranger"))))
    (is (null said) "nothing a Details press says is raised on the pressing thread")))

(deftest channel-host-a-status-post-landing-after-settle-retires-itself ()
  ;; The settle can race the status post: the finish job retires the chrome
  ;; while the line is still in flight — it reads no id — and the line lands
  ;; after, a working line no turn watches. The landing retires it.
  ;; (2026-09-16: a NO_REPLY settle left "working · 8s" in the room.)
  (with-digest-host (host lane :responses (replies "s1"))
    (fact "turn.started" (nlk:json-object))
    (said "NO_REPLY")
    (fact "turn.completed" (nlk:json-object))
    ;; The silent finish retires nothing: no line was ever recorded.
    (await (:timeout 10) (null (nck:turn-digest-answer (nck:lane-digest lane))))
    (is (null (labelled "send_message")))
    ;; The in-flight live plan lands now, with the flags it captured then.
    (nck::perform-flush host lane :post '(:state :working :elapsed "8s" :headline "Working")
                        nil nil :reply-to "m1")
    (is (= 1 (length (labelled "send_message"))) "the late line posts")
    (is (= 1 (length (labelled "delete_message"))) "and retires itself on landing")
    (is-plan (first (labelled "delete_message")) :path "/rooms/123/messages/s1")
    (is (null (nck::turn-digest-status-id (nck:lane-digest lane))))))

(deftests channel-host-a-completion-the-fold-missed (messages said answer card)
  (with-temp-store ()
    (with-digest-host (host lane
                       :responses (replies "s1" "a1")
                       :name "store-fallback" :session "chat-123-m-store"
                       :trigger "m-store")
      (let ((turn (progn (ensure-durable-session "chat-123-m-store")
                         (nlk:admit-turn "chat-123-m-store" "command-store-answer"
                                         "the question"))))
        (dolist (message messages)
          (nlk:record-assistant-message turn message))
        (nlk:complete-turn turn nil)
        (earn-line turn.turn-id)
        ;; Deliberately omit turn.assistant_message_completed from the
        ;; live fold; only turn.completed reaches the host.
        (fact "turn.completed" (nlk:json-object) turn.turn-id)
        (is (posted 3) "the store's word posts beside the card")
        (let ((plans (plans)))
          (is-plan (second plans) :label "send_message")
          (is (search said (plan-content (second plans))))
          (is-plan (third plans) :label card)
          (is (equal answer (nck:turn-digest-answer (nck:lane-digest lane))))))))
  ;; The live frame may be missed, but the durable assistant fact remains the
  ;; answer authority. The worker recovers it and settles the captured card.
  (falls-back-to-store
   (list (nlk:json-object "role" "assistant" "content" "partial answer")
         (nlk:json-object "role" "assistant" "content" "durable answer"))
   "durable answer" "durable answer" "edit_message")
  ;; 2026-09-21: a DeepSeek round leaked its tool call into text — null
  ;; content, 10,462 chars of reasoning — and the turn completed in silence:
  ;; the engine accepted a reasoning-only final as an answer, and the room
  ;; batched "no text at all" with the SILENT sentinel, so a failure read as
  ;; intent. The engine now retries that final; this is the room's backstop —
  ;; a completion whose only words the room cannot show says so in one line,
  ;; where a turn that never spoke at all keeps the pinned absence.
  (says-a-wordless-one-has-no-answer
   (list (nlk:json-object "role" "assistant" "reasoning_content" "leaked tool call text"))
   "without an answer" nil "delete_message"))

(deftest channel-host-thinking-status (with-digest-host (host lane :responses (replies "s1")))
  ;; A round that speaks only in reasoning: once it has thought for the
  ;; earning window the card opens, saying the turn is thinking and naming
  ;; the thought by its newest whole sentence — never the tail it is still
  ;; writing (2026-09-28: a raw tail read as the answer) — and a content-null
  ;; round's committed reasoning settles into it.
  (fact "turn.started" (nlk:json-object))
  (fold-delta host "chat-123-m1" "Looking up the registry. the registry lookup")
  (fold-delta host "chat-123-m1" " NOT THIS TURN" :turn-id "other-turn")
  (is (null (plans)) "delta folds perform no I/O")
  (setf (nck::turn-digest-started-at-ms (nck:lane-digest lane)) (- (nck:now-ms) 9000))
  (nck:flush-now host lane)
  (is-present (post (first (plans))) "a thinking round opens the card"
    (is-carrying (content (plan-content post))
      "Thinking · " "thinking · Looking up the registry."
      (:absent "the registry lookup" "the sentence still being written stays off the card")))
  (fact "turn.assistant_message_completed"
        (nlk:json-object
         "message" (nlk:json-object "content" :null
                                    "reasoning_content"
                                    "Settled on eval.")))
  (nck:flush-now host lane)
  (is-present (edit (second (plans))) "a content-null round settles into the same card"
    (is (equal "PATCH" (nck:request-plan-method edit)))
    (is-carrying (content (plan-content edit))
      "thinking · Settled on eval."
      (:absent "NOT THIS TURN" "another turn's deltas never fold into this digest")))
  (is (= 2 (length (plans)))))

(deftest channel-host-a-quick-turn-says-its-answer-once ()
  ;; A round that says the answer and calls a tool anyway is followed within a
  ;; second by the final round saying it again (2026-09-28: "144." then "12
  ;; squared is **144**." in one DM). The room reads it once, in the answer:
  ;; words on the way never post, whatever the answer says.
  (flet ((run (said answer)
           (with-digest-host (host lane :responses (replies "c1" "a1" "a2") :name "quick-answer")
                      (fold-fact host "chat-123-m1" "turn.started" (nlk:json-object))
             (fold-fact host "chat-123-m1" "turn.assistant_message_completed"
                        (nlk:json-object "message" (nlk:json-object
                                                    "content" said
                                                    "tool_calls" (vector (nlk:json-object "id" "c")))))
             (nck::schedule-flush host lane)
             (fold-fact host "chat-123-m1" "turn.assistant_message_completed"
                        (nlk:json-object "message" (nlk:json-object "content" answer)))
             (fold-fact host "chat-123-m1" "turn.completed" (nlk:json-object))
             (await (:timeout 10) (plans-matching (nck:recording-executor-plans executor)
                                                  :content answer))
             (sleep 0.3)
             (mapcar #'plan-content (plans-matching (nck:recording-executor-plans executor)
                                                    :label "send_message")))))
    (is (equal '("12 squared is **144**.") (run "144." "12 squared is **144**.")))
    (is (equal '("They cover Q3 hiring.") (run "Reading the notes first." "They cover Q3 hiring.")))))

(deftest channel-host-one-flush-in-flight-per-lane ()
  ;; With a pool of drainers, two flushes planned from one digest would race
  ;; to create two status messages for one ask.
  (with-digest-host (host lane :responses (replies "s1") :workers 4)
    (fact "turn.tool_call_started" (nlk:json-object "tool-name" "eval"))
    (dotimes (index 8) (nck:schedule-flush host lane))
    (is (await (:timeout 10) (plans)))
    (sleep 0.3)
    (is (= 1 (length (plans))))))

(deftest channel-host-laneless-failure-notice (with-digest-host (host nil :name "laneless"))
  (fold-fact host "chat-999-t555-m4" "turn.failed"
             (nlk:json-object "detail" "turn was not resumed after 3 boots") "t9")
  (is (await (:timeout 10) (plans)))
  (is-present (plan (first (plans))) "notice plan recorded"
    (is-plan plan :path "/rooms/555/messages")
    (is (search "turn was not resumed" (plan-content plan))))
  (fold-fact host "web-abc" "turn.failed" (nlk:json-object "detail" "x") "t10")
  (sleep 0.2)
  (is (= 1 (length (plans)))))

(defun typing-beats (host executor)
  "Run one typing tick on HOST; the beats EXECUTOR has recorded so far."
  (nck::typing-tick host)
  (length (nck:recording-executor-plans executor)))

(defun queued-lane-digest (host id)
  "Open a queued digest on HOST's lane ID in room 1, the live turn a typing beat follows."
  (let ((lane (nck:intern-lane (nck:host-lanes host) id :target '(:channel-id "1"))))
    (bt2:with-lock-held ((nck:lane-lock lane))
      (nck:digest-note-queued (nck:lane-open-digest lane (nck:now-ms)) 0))))

(deftest channel-host-typing-follows-the-platform-cadence ()
  ;; One indicator per surface, re-asserted on the platform's own refresh —
  ;; Telegram's beat dies in five seconds where Discord's lives ten.
  (let* ((executor (nck:make-recording-executor))
         (host (test-host :executor executor :name "typing")))
    (dolist (id '("chat-1-m1" "chat-1-m2"))
      (queued-lane-digest host id))
    (is (= 1 (typing-beats host executor)) "two live lanes in one surface drive one beat")
    (is-plan (first (nck:recording-executor-plans executor)) :path "/rooms/1/typing")
    (is (= 1 (typing-beats host executor)) "inside the refresh window nothing is re-sent")
    (sleep 0.15)
    (is (= 2 (typing-beats host executor)))))

(deftest channel-host-typing-cap-measures-the-turn-silence ()
  ;; The cap is the turn's silence, not the indicator's age: a surface whose
  ;; turn keeps showing something keeps its beat past the cap, and one that
  ;; has shown nothing for the cap stops — the wedged-turn guard, kept.
  (nlk:bind ((executor (nck:make-recording-executor))
             (host (test-host :executor executor :name "typing-silence")) (nck::+typing-cap-ms+ 300)
             (digest (queued-lane-digest host "chat-1-m1")))
    (is (= 1 (typing-beats host executor)) "a live surface gets its first beat")
    (sleep 0.25)
    ;; The turn shows something — a part, a call, a round — and the
    ;; indicator is already older than the cap when the next tick looks.
    (nck:digest-note-visible digest (nck:now-ms))
    (sleep 0.12)
    (is (= 2 (typing-beats host executor)) "a turn still producing keeps its beat past the cap")
    (sleep 0.35)
    (is (= 2 (typing-beats host executor)) "silence past the cap ends the beat")))

;;; --- commands ---------------------------------------------------------------

(deftest channel-host-slash-line-is-a-command-answered-in-the-room (with-temp-store ())
  ;; A slash line never becomes a lane: it runs through NLE:SLASH against
  ;; the room on a delivery thread and answers as one reply that pings its
  ;; asker. With operators declared, /help is everyone's and the rest are
  ;; theirs; a failure is an answer, never a warning nobody reads.
  (with-digest-host (host nil :name "command-test" :host-keys (:owners '("mike")))
    (setf (nck:host-policy host) (channel-policy :allowed-channels '("123")))
    (flet ((answered (text user message n)
             (say text user message)
             (is (posted n))
             (plan-content (nth (1- n) (plans)))))
      (is (eq :answer (say "/help" "mike" "m1")))
      (is (posted 1) "the answer lands")
      (is-present (post (first (plans))) "the operator's /help answers as a reply to the ask"
        (is-plan post :label "send_message" :path "/rooms/123/messages" "reply_to" "m1" "ping" t)
        (is (search "/models" (plan-content post)) "with the catalog"))
      (is (nlk:session-exists-p "chat-123"))
      (is (zerop (nck:lane-count (nck:host-lanes host))))
      (is (equal "/models is the operator's; /help lists what you can run"
                 (answered "/models" "alice" "m2" 2)))
      (is (search "/models" (answered "/help" "alice" "m3" 3)))
      (is (equal "nothing to undo" (answered "<@999> /undo" "mike" "m4" 4)))
      (is (equal "unknown command /nope; /help lists them" (answered "/nope" "mike" "m5" 5)))
      (is (uiop:string-prefix-p "/think: " (answered "/think bogus" "mike" "m6" 6)))
      (is (= 6 (length (plans))) "one message per command, nothing else"))))

(deftest channel-host-model-aux-in-a-room-answers-the-auxiliary-target (with-temp-store ())
  ;; /model-aux is a room command like any other: it runs the headless arm
  ;; and answers the image's auxiliary target with how to change it — never
  ;; the session summary the bare /models line prints. The token's old
  ;; home, /models aux, answers where it went instead of landing a pick.
  (with-saved-globals (nle::*auxiliary-model*)
    (setf nle::*auxiliary-model* '(:provider "zeta" :model "z-model" :effort "low"))
    (with-digest-host (host nil :name "aux-command" :host-keys (:owners '("mike")))
      (setf (nck:host-policy host) (channel-policy :allowed-channels '("123")))
      (say "/model-aux" "mike" "m1")
      (is (posted 1) "the command answers once")
      (is-carrying (content (plan-content (first (plans))))
        (is (uiop:string-prefix-p "auxiliary model: zeta/z-model (low)" content))
        "/model-aux <provider> <model> changes it" (:absent "provider: "))
      ;; The old token answers the notice, and moves nothing.
      (say "/models aux" "mike" "m2")
      (is (posted 2) "the redirect answers too")
      (is (equal nle::+auxiliary-command-notice+
                 (plan-content (second (plans)))))
      (is (equal '(:provider "zeta" :model "z-model" :effort "low")
                 nle::*auxiliary-model*)))))

(deftest channel-host-completion-follows-the-commands-own-rule ()
  ;; Completion is the commands' own rule: with operators declared, only an
  ;; operator sees a command's argument choices — /help is everyone's, as
  ;; every command but /help is the operator's.
  (let ((host (test-host :name "completions")))
    (is (nck::completion-allowed-p host "models" "mike"))
    (is (not (nck::completion-allowed-p host "models" "sam")))
    (is (nck::completion-allowed-p host "help" "sam")))
  (is (nck::completion-allowed-p
       (test-host :name "completions-open" :owners '())
       "models" "sam")))

(deftest channel-host-publishes-the-menu-once-per-catalog-shape ()
  ;; The tick publishes the catalog to the platform when its shape differs
  ;; from the one last published: once at start, again when a cell
  ;; registers a command later, never while a publish is in flight, and
  ;; not at all for a platform without a menu or one not ready to take it.
  (with-saved-globals (nle::*registered-commands*)
    (let ((platform (test-platform))
          (ready (list t))
          (seen '()))
      (setf (nck:platform-plan-commands platform)
            (lambda (entries &key timeout-seconds)
              (push entries seen)
              (and (car ready)
                   (list (nck:make-request-plan
                          :method "PUT" :path "/commands"
                          :body (nlk:json-object "count" (length entries))
                          :timeout-seconds timeout-seconds
                          :audit-label "set_commands")))))
      (with-digest-host (host nil :name "menu-test" :host-keys (:platform platform))
        (setf (car ready) nil)
        (nck:sync-commands host)
        (is (not (await (:timeout 0.3) (plans))))
        (is (null (nck:host-commands-published host)))
        (setf (car ready) t)
        (nck:sync-commands host)
        (is (await (:timeout 5) (= 1 (length (plans)))))
        (is-present (plan (first (plans))) "as the platform's plans"
          (is-plan plan :label "set_commands" :timeout 5))
        (is (equal "help" (getf (first (first seen)) :name)))
        (is-present (new (find "new" (first seen) :key (lambda (entry) (getf entry :name))
                                                   :test #'string=))
          "/clear's alias is a command of its own in the menu"
          (is (equal "Start a fresh session" (getf new :description))))
        (is (stringp (nck:host-commands-published host)))
        (nck:sync-commands host)
        (nck:sync-commands host)
        (is (not (await (:timeout 0.3) (> (length (plans)) 1))))
        (nle:register-command "late-pack" "late" (constantly nil) :description "Registered later")
        (nck:sync-commands host)
        (is (await (:timeout 5) (= 2 (length (plans)))))
        (is (find "late" (first seen)
                  :key (lambda (entry) (getf entry :name)) :test #'string=))
        (is (null (nck:sync-commands (test-host))))))))

(deftest channel-host-the-menu-names-every-alias ()
  ;; A platform menu has no aliases, so each name a command answers to is a
  ;; command of its own there, after every command — 2026-10-03: /new, /clear's
  ;; alias, was in no Discord menu. A name already in the menu stays the one
  ;; command it names.
  (is (equal '(("a" "first") ("b" "second") ("c" "first"))
             (mapcar (lambda (entry)
                       (list (getf entry :name) (getf entry :description)))
                     (nck::menu-entries '((:name "a" :description "first" :aliases ("b" "c"))
                                          (:name "b" :description "second" :aliases ("c")))))))
  (let ((names (mapcar (lambda (entry) (getf entry :name)) (nck::menu-entries (nle:slash-catalog)))))
    (is (member "new" names :test #'string=))
    (is (equal names (remove-duplicates names :test #'string-equal :from-end t)) "no name twice")))

(deftest channel-ask-prompt-reads-attachments-in-at-their-anchors (let ((nck:*transcription* nil)
                                                                        (read '())))
  ;; What the lane reads when people attached things: at the end of each line
  ;; that brought a file, the marker that pairs with the image part the turn
  ;; carries, a recording's transcript, or the one note saying why neither.
  ;; The fetch and the engine are stubbed: the network and the model are the
  ;; live proof's business, the composition is this test's.
  (with-stubbed-fdefinitions
      ((nck::read-attachment (attachment)
        (let ((name (gethash "filename" attachment)))
          (push name read)
          (cond ((equal name "good.png")
                 (values :image (cons "image/png" "QUJD") nil))
                ((member name '("note.ogg" "broken.ogg") :test #'equal)
                 (values :audio (cons (octets-of 1) "ogg") nil))
                (t (values nil nil "its bytes open no image (PNG, JPEG, GIF, WebP) and no recording")))))
       (nck:transcribe-audio (octets container &key seconds)
        (if (eql seconds 3)
            (error "the local transcriber failed: out of memory")
            (values "ship it" 5.52))))
    (flet ((file (name type &optional seconds)
             (nlk:json-object "url" (format nil "https://cdn/~a" name)
                              "filename" name "media_type" type "seconds" seconds)))
      (nlk:bind (((prompt anchors) (nck::compose-prompt
                                    (list "bob [m1 u2]: morning"
                                          (cons "v1se [m2 u3]:" (list (file "note.ogg" "audio/ogg" 5.52))))
                                    "kim [m3 u4]: what does this say?"
                                    :anchors (list (cons (length "kim [m3 u4]: what does this say?")
                                                         (list (file "good.png" "image/png")
                                                               (file "bad.png" "image/png")
                                                               (file "broken.ogg" "audio/ogg" 3)
                                                               (file "long.ogg" "audio/ogg" 900))))))
                 (ask (nck:make-ask :room "discord-1" :lane "discord-1-m3"
                                    :prompt prompt :attachments anchors))
                 ((prompt images) (nck::ask-prompt-and-attachments ask)))
        (is (equal '(("image/png" . "QUJD")) images))
        (is (equal "bob [m1 u2]: morning
v1se [m2 u3]: [Audio #1, 5.5s, transcribed] \"ship it\"
kim [m3 u4]: what does this say? [Image #1] [the attachment \"bad.png\" could not be read: its bytes open no image (PNG, JPEG, GIF, WebP) and no recording] [the recording \"broken.ogg\" could not be transcribed: the local transcriber failed: out of memory] [the recording \"long.ogg\" could not be transcribed: 15:00 long, over the 5:00 cap (transcription.max_seconds)]"
                   prompt))
        (is (not (member "long.ogg" read :test #'equal)))))))

(deftest channel-read-attachment-sniffs-what-it-fetched ()
  ;; The bytes decide, never the declared type: a recording declared as an
  ;; image is still a recording, a file that opens as nothing is saved for the
  ;; lane's tools and named, and a size declared past the ceiling a file may
  ;; take is refused before the fetch runs.
  (flet ((entry (octets &rest keys)
           (apply #'nlk:make-json-object "fetch" (lambda () octets) keys)))
    (multiple-value-bind (kind data) (nck::read-attachment
                                      (entry (fixture-recording) "media_type" "image/png"))
      (is (eq :audio kind))
      (is (equal "ogg" (cdr data))))
    (is-values (kind data) (nck::read-attachment (entry (test-png-octets) "media_type" "audio/ogg"))
      (kind eq :image)
      ((car data) "image/png"))
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil))
          (clip (octets-of 1 2 3 4 5 6 7 8 9 10 11 12)))
      (unwind-protect
           (is-values (kind data) (nck::read-attachment (entry clip "id" "a7" "filename" "clip 1.mp4"
                                                               "media_type" "video/mp4"))
             (kind eq :file)
             ((cdr data) = 12)
             ((file-namestring (car data)) "a7-clip_1.mp4")
             ((alexandria:read-file-into-byte-vector (car data)) equalp clip))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore))
      ;; The prompt names it, its type and size, and where it is.
      (let ((ask (nck::make-ask :prompt "mike [m1 u1]: what happens in this"
                                :attachments (list (cons 34 (list (entry clip "id" "a8" "filename" "c.mp4"
                                                                         "media_type" "video/mp4")))))))
        (unwind-protect
             (is (search (format nil "what happens in this [File \"c.mp4\" (video/mp4, 12 bytes) saved at ~a"
                                 (uiop:native-namestring (merge-pathnames "a8-c.mp4" nck::*attachment-directory*)))
                         (nck::ask-prompt-and-attachments ask)))
          (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore))))
    ;; A video's container holds sound, but a video file is opened, not
    ;; heard; a round video note that declares its length is a recording.
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil))
          (mp4 (octets-of 0 0 0 24 #x66 #x74 #x79 #x70 #x69 #x73 #x6F #x6D)))
      (unwind-protect
           (progn
             (is (eq :file (nck::read-attachment (entry mp4 "media_type" "video/mp4"
                                                        "filename" "clip.mp4"))))
             (is (eq :audio (nck::read-attachment (entry mp4 "media_type" "video/mp4"
                                                         "seconds" 12)))))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore)))
    (multiple-value-bind (kind data note)
        (nck::read-attachment (nlk:json-object "fetch" (lambda () (error "fetched"))
                                               "media_type" "video/mp4"
                                               "size" (1+ nck::+file-max-bytes+)))
      (is (null kind))
      (is (null data))
      (is (search "ceiling" note))
      (is (not (search "fetched" note)) "nothing was fetched"))))

(deftest channel-read-attachment-reads-a-text-file-whole ()
  ;; A file declared as text is read into the prompt whole, fenced under its
  ;; name — the fence longer than any run of backticks inside, so the file's
  ;; own fences stay its own — up to the ceiling Hermes reads; bytes that are
  ;; not UTF-8 text, and a text file past that ceiling, are files on disk the
  ;; lane opens with its tools.
  (flet ((entry (octets &rest keys)
           (apply #'nlk:make-json-object "fetch" (lambda () octets) keys)))
    (let ((notes (format nil "# Q3~%- ship /link (owner: mike)~%```lisp~%(+ 1 2)~%```~%")))
      (is-values (kind data) (nck::read-attachment
                              (entry (sb-ext:string-to-octets notes :external-format :utf-8)
                                     "media_type" "text/markdown" "filename" "notes.md"))
        (kind eq :text)
        (data notes))
      (let ((text (nck::document-prompt-text (nlk:json-object "filename" "notes.md") notes)))
        (is (uiop:string-prefix-p (format nil "[File \"notes.md\"]~%````~%# Q3") text))
        (is (uiop:string-suffix-p text (format nil "```~%````")))))
    ;; Bytes that are not UTF-8 text are a file the lane opens with its tools.
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil)))
      (unwind-protect
           (is (eq :file (nck::read-attachment (entry (octets-of 104 105 255 254) "filename" "odd.txt"))))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore)))
    ;; A text file past the ceiling one is read whole at is a file on disk.
    (let ((nck::*attachment-directory* (temp-path "channel-files" nil)))
      (unwind-protect
           (is (eq :file (nck::read-attachment
                          (entry (make-array (1+ nck::+document-max-bytes+)
                                             :element-type '(unsigned-byte 8) :initial-element 97)
                                 "filename" "huge.log"))))
        (uiop:delete-directory-tree nck::*attachment-directory* :validate t :if-does-not-exist :ignore)))
    ;; The prompt carries it where the asker's line ends.
    (let ((ask (nck::make-ask :prompt "mike [m1 u1]: summarize this"
                              :attachments (list (cons 28 (list (entry (sb-ext:string-to-octets
                                                                        "- a" :external-format :utf-8)
                                                                       "filename" "notes.md")))))))
      (is (search (format nil "summarize this [File \"notes.md\"]~%```~%- a~%```")
                  (nck::ask-prompt-and-attachments ask))))))

(defmacro with-recordings-heard ((transcript &key (seconds 'seconds) on-read) &body body)
  "BODY with every attachment read as a recording, ON-READ run at each read, that
transcribes to TRANSCRIPT and lasts SECONDS — by default the length it declares."
  `(with-stubbed-fdefinitions
       ((nck::read-attachment (attachment) ,on-read (values :audio (cons (octets-of 1) "ogg") nil))
        (nck:transcribe-audio (octets container &key seconds) (values ,transcript ,seconds)))
     ,@body))

(deftest channel-voice-note-rides-the-chatter-to-the-next-ask (let ((nck:*transcription* nil)
                                                                    (host (test-host))))
  ;; v1se's voice note in a room that wants a mention: nobody addressed the
  ;; bot, so it is chatter — and chatter a voice note is: its line rides with
  ;; the recording, and the ask that asks about it reads the transcript in
  ;; right after the person who spoke it, not after the asker.
  (nck:observe-candidate
   host (test-candidate :text "" :user-name "v1se" :message "7"
                        :attachments (vector (nlk:json-object "url" "https://cdn/voice-message.ogg"
                                                              "media_type" "audio/ogg"
                                                              "filename" "voice-message.ogg"
                                                              "size" 22399 "seconds" 5.52))))
  (nck:observe-candidate host (test-candidate :text "did anyone hear that?"
                                              :user-name "bob" :message "8"))
  ;; An image nobody addressed stays out of the chatter, as it always has.
  (nck:observe-candidate
   host (test-candidate :text "" :user-name "carol" :message "8b"
                        :attachments (vector (nlk:json-object "url" "https://cdn/x.png"
                                                              "media_type" "image/png"))))
  (let ((ask (nck:build-ask host (test-candidate :text "<@999> do you see the voice msg above"
                                                 :message "9" :user-name "mike")
                            "chat-100-m9" nil)))
    (is (equal "v1se [m7 uu1]:
bob [m8 uu1]: did anyone hear that?
mike [m9 uu1]: do you see the voice msg above"
               (nck:ask-prompt ask)))
    (with-recordings-heard ("can you check why the build is red")
      (multiple-value-bind (prompt images) (nck::ask-prompt-and-attachments ask)
        (is (null images))
        (is (equal "v1se [m7 uu1]: [Audio #1, 5.5s, transcribed] \"can you check why the build is red\"
bob [m8 uu1]: did anyone hear that?
mike [m9 uu1]: do you see the voice msg above"
                   prompt)))))
  ;; A voice note that is itself the ask, replying to a message: the
  ;; transcript follows the speaker's line, above the reply it answers.
  (let ((candidate (test-candidate :text "" :user-name "v1se" :message "10"
                                   :reply "the deploy is done" :reply-to "5"
                                   :reply-name "bob"
                                   :attachments (vector (nlk:json-object
                                                         "url" "https://cdn/voice-message.ogg"
                                                         "media_type" "audio/ogg" "seconds" 2)))))
    (let ((ask (nck:build-ask host candidate "chat-100-m10" nil)))
      (with-recordings-heard ("" :seconds 2.0)
        (is (equal "v1se [m10 uu1 r5]: [Audio #1, 2.0s, transcribed: no speech heard]
↩ bob [m5]: the deploy is done"
                   (nck::ask-prompt-and-attachments ask)))))))

(deftest channel-reply-to-a-voice-note-reads-what-it-says ()
  ;; "do u hear this", typed as a reply to the operator's own voice note: the
  ;; ask carries the note it answers, so its transcript is read in under the
  ;; reply's line whether or not the note is still in the chatter — and when
  ;; it is, it is read once, not twice. An edit that adds the mention to the
  ;; voice note makes the note its own ask the same way.
  (let ((nck:*transcription* nil)
        (host (test-host))
        (read 0)
        (note (nlk:json-object "id" "att-1" "url" "https://cdn/qb8rzbq.ogg"
                               "media_type" "audio/ogg" "filename" "qb8rzbq.ogg"
                               "seconds" 5.52)))
    (nck:observe-candidate host (test-candidate :text "" :user-name "peas" :message "7"
                                                :attachments (vector note)))
    (let ((ask (test-candidate :text "<@999> do u hear this" :message "9"
                               :user-name "peas" :reply "" :reply-to "7"
                               :reply-name "peas")))
      ;; The same file, as the platform ships it again inside the reply.
      (setf (gethash "attachments" (gethash "reply" ask))
            (vector (nlk:json-object "id" "att-1" "url" "https://cdn/qb8rzbq.ogg?ex=2"
                                     "media_type" "audio/ogg" "seconds" 5.52)))
      (with-recordings-heard ("is the bot hearing this" :on-read (incf read))
        (is (equal "peas [m7 uu1]:
peas [m9 uu1 r7]: do u hear this
↩ peas [m7] [Audio #1, 5.5s, transcribed] \"is the bot hearing this\""
                   (nck::ask-prompt-and-attachments
                    (nck:build-ask host ask "chat-100-m9" nil))))
        (is (= 1 read) "the note is fetched and transcribed once")))
    ;; The voice note edited to mention the bot: its own ask, its own file.
    (setf read 0)
    (nck:observe-candidate host (test-candidate :text "" :user-name "peas" :message "7"
                                                :attachments (vector note)))
    (let ((edited (test-candidate :text "<@999>" :user-name "peas" :message "7"
                                  :attachments (vector note))))
      (with-recordings-heard ("is the bot hearing this" :on-read (incf read))
        (is (equal "peas [m7 uu1]:
peas [m7 uu1]: [Audio #1, 5.5s, transcribed] \"is the bot hearing this\""
                   (nck::ask-prompt-and-attachments
                    (nck:build-ask host edited "chat-100-m7" nil))))
        (is (= 1 read) "the chatter's copy is not read again")))))

;;; --- a file into the room --------------------------------------------------

(defun test-png-octets (&optional (size 64))
  "PNG magic bytes and filler: an image the sniffer opens, without a real
picture's weight."
  (replace (make-array size :element-type '(unsigned-byte 8) :initial-element 0)
           #(137 80 78 71 13 10 26 10)))

(defun write-pea-fixture ()
  "Write /tmp/pea.png — the picture the file tests hand around — so a
 delivery that checks the file exists has one to join."
  ;; The fixture is the test's own: an ambient /tmp/pea.png was the silent
  ;; dependency (2026-09-17, /tmp was cleaned and
  ;; CHANNEL-ANSWER-CARRIES-ITS-FILES went red).
  (alexandria:write-byte-vector-into-file (test-png-octets) #p"/tmp/pea.png" :if-exists :supersede)
  #p"/tmp/pea.png")

(defmacro with-file-host ((host name &key responses (platform '(test-platform))) &body body)
  "BODY with HOST, WITH-DIGEST-HOST's host named NAME over PLATFORM and its lane
LANE, as the one running channel."
  ;; Writes the file fixture first: a file test that depends on ambient /tmp
  ;; is a test that fails the day /tmp is cleaned.
  `(with-digest-host (,host lane :name ,name :responses ,responses :host-keys (:platform ,platform))
     (let ((nck::*hosts* (list ,host)))
       (write-pea-fixture)
       ,@body)))

(deftest channel-post-file-carries-a-file ()
  ;; One file posts as its own message: the kit plans the platform's own
  ;; upload, executes it on the caller's thread, and answers what the
  ;; platform created — the id a read-back would name.
  (with-file-host (host "post-file" :responses (replies "f1") :platform (test-platform :files t))
    (is (equal '(:status 200 :body (:id "f1"))
               (nck:post-file #p"/tmp/pea.png" :platform "test"
                              :channel "123" :thread "t9"
                              :content "the picture" :reply-to "m1")))
    (let ((plan (first (nck:recording-executor-plans executor))))
      (is-plan plan :path "/rooms/t9/files" "caption" "the picture")
      (is (equal "m1" (plan-field plan "reply_to")) "and the message it answers rides with it"))))

(deftest channel-post-file-says-when-the-platform-carries-none (with-file-host (host "no-files"))
  ;; A platform that cannot carry a file answers why and sends nothing: the
  ;; caller reads the refusal instead of a caption naming a picture nobody
  ;; can see.
  (let ((answer (nck:post-file #p"/tmp/pea.png" :platform "test"
                               :channel "123")))
    (is (zerop (getf answer :status)))
    (is (search "carries no files" (getf answer :error)))
    (is (null (nck:recording-executor-plans executor)))))

(deftest channel-post-file-refuses-a-platform-that-is-not-running (let ((nck::*hosts* '())))
  (is (search "no nowhere channel is running"
              (refusal-text error
                (nck:post-file #p"/tmp/pea.png" :platform "nowhere" :channel "1")))))

(deftest channel-answer-file-notes-the-running-turn (with-file-host (host "answer-file"))
  ;; A file handed to the answer is recorded on the running turn — read
  ;; back as the turn's own file, never posted now.
  (setf (nck:lane-active-turn-id lane) "t9")
  (is (equal (list :session-id "chat-123-m1" :turn-id "t9"
                   :files (list #p"/tmp/pea.png"))
             (nck:answer-file #p"/tmp/pea.png" :platform "test"
                              :channel "123")))
  (is (null (nck:recording-executor-plans executor)))
  (is (equal (list #p"/tmp/pea.png")
             (nck::take-answer-files lane "t9")))
  (is (null (nck::take-answer-files lane "t9"))))

(deftest channel-answer-file-needs-a-running-turn (with-file-host (host "no-turn"))
  ;; No turn, no answer, no place for the file: the call says which of the
  ;; two is missing rather than posting beside an answer that never comes.
  (is-table (needle channel)
    (search needle (refusal-text error
                     (nck:answer-file #p"/tmp/pea.png" :platform "test" :channel channel)))
    ("no turn is running" "123")
    ("no lane answers" "999")))

(deftest channel-answer-carries-its-files ()
  ;; The answer's own message carries the picture: the plan the terminal
  ;; delivery posts names the file the turn handed over, on its first chunk.
  (with-file-host (host "answer-carries" :responses (replies "a1"))
    (let ((digest (nck:make-turn-digest :turn-id "t9" :phase :completed :answer "the answer")))
      (setf (nck:lane-active-turn-id lane) "t9")
      (nck:answer-file #p"/tmp/pea.png" :platform "test" :channel "123")
      (nck::deliver-answer host lane digest)
      (let ((plan (first (nck:recording-executor-plans executor))))
        (is (search "the answer" (plan-content plan)))
        (is (equalp #("pea.png") (plan-field plan "files")))))))

;;; --- choices: a clarifying question's answers on it ---------------------------------

(deftest channel-answer-choices-ride-the-answer ()
  ;; Choices handed to the answer ride its message as one row of buttons,
  ;; each saying its own label, under the words they answer: on the last
  ;; chunk of a long answer. A second set replaces the first.
  (with-file-host (host "answer-choices" :responses (replies "a1" "a2")
                                         :platform (test-platform :choices t))
    (setf (nck:lane-active-turn-id lane) "t9")
    (nck:answer-choices '("Postgres" "SQLite") :platform "test" :channel "123")
    (is (equal '(:session-id "chat-123-m1" :turn-id "t9"
                 :choices ("Zero setup" "Relational queries" "Realtime sync"))
               (nck:answer-choices '("Zero setup" " Relational queries " "Realtime sync")
                                   :platform "test" :channel "123")))
    (is (null (nck:recording-executor-plans executor)) "nothing posts before the answer")
    (nck::deliver-answer host lane (nck:make-turn-digest
                                    :turn-id "t9" :phase :completed
                                    :answer (format nil "~a~%~%What matters most?"
                                                    (make-string 2100 :initial-element #\x))))
    (let* ((plans (nck:recording-executor-plans executor))
           (last-plan (first (last plans))))
      (is (< 1 (length plans)) "a long answer is several messages")
      ;; None but the last carries controls.
      (is (every (lambda (plan) (null (plan-field plan "controls"))) (butlast plans)))
      (is (search "What matters most?" (plan-content last-plan)))
      ;; The last carries one row of buttons, each saying its label.
      (is (equal '((("Zero setup" "nck:say:Zero setup" :primary nil)
                    ("Relational queries" "nck:say:Relational queries" :primary nil)
                    ("Realtime sync" "nck:say:Realtime sync" :primary nil)))
                 (plan-field last-plan "controls"))))
    (is (null (nck::take-answer-choices lane "t9")) "taken once")))

(deftest channel-answer-choices-refuses-what-it-cannot-show (with-file-host (host "no-choices"))
  ;; Labels a row of buttons cannot show refuse, and so does a platform whose
  ;; messages carry no choices: the model hears why instead of a question
  ;; posted without its answers.
  (setf (nck:lane-active-turn-id lane) "t9")
  (is-table (needle labels)
    (search needle (refusal-text error (nck:answer-choices labels :platform "test" :channel "123")))
    ("2 to 5 distinct labels" '("only one"))
    ("2 to 5 distinct labels" '("same" "same"))
    ("2 to 5 distinct labels" '("a" "b" "c" "d" "e" "f"))
    ("2 to 5 distinct labels" (list "short" (make-string 81 :initial-element #\x)))
    ("Test messages carry no choices" '("Postgres" "SQLite"))))

(defun test-press (text card &key (card-text "What matters most?") (user "u1"))
  "USER pressing the choice that says TEXT on the card message CARD in room
123, as interaction i1 — no message of its own."
  (let ((press (test-candidate :channel "123" :message nil :text text :user user
                               :reply-to card :addressed t)))
    (setf (gethash "pressed" (gethash "source" press)) t
          (gethash "interaction_id" (gethash "source" press)) "i1"
          (gethash "card_text" (gethash "source" press)) card-text)
    press))

(deftest channel-a-press-answers-the-card-it-sits-on ()
  ;; A press on a choice is the presser's reply to the card: the lane that
  ;; posted it takes the label as its next ask, and the card is answered in
  ;; place — its words kept, its buttons gone, a line naming the answer.
  (with-digest-host (host lane :name "press" :host-keys (:platform (test-platform :choices t)))
    (setf (nck:host-policy host) (channel-policy))
    (nck:bind-lane-address (nck:host-lanes host) "c9" (nck:lane-session-id lane))
    (let ((continued '()))
      (with-stubbed-fdefinition (nck:continue-lane (host lane ask)
                                  (push (list lane ask) continued))
        (is (eq :answer (nck:handle-candidate host (test-press "Zero setup" "c9"))))
        (is (= 1 (length continued)))
        (is (eq lane (first (first continued))) "the card's own lane")
        (is (search "Zero setup" (nck:ask-prompt (second (first continued)))))
        (is (posted 2))
        (is-plan (first (labelled "interaction_defer")) :path "/interactions/i1"
                 "private" nil "pressed" t)
        (let ((answer (first (labelled "interaction_response"))))
          (is (search "What matters most?" (plan-content answer)) "the question stays")
          (is (search "Answered: Zero setup" (plan-content answer)))
          (is (eq :clear (plan-field answer "controls")) "its buttons go"))))))

(deftest channel-a-press-on-a-card-no-lane-answers ()
  ;; A card whose lane is gone has no question open: the presser is told so,
  ;; privately, and nothing runs.
  (with-digest-host (host lane :name "stale-press" :host-keys (:platform (test-platform :choices t)))
    (setf (nck:host-policy host) (channel-policy))
    (with-stubbed-fdefinition (nck:continue-lane (host lane ask)
                                (error "a stale press continued a lane"))
      (nck:handle-candidate host (test-press "Zero setup" "c404"))
      (is (posted 2))
      (is-plan (first (labelled "interaction_defer")) "private" t)
      (is-plan (first (labelled "interaction_response"))
               "content" "this question is no longer open"))))

(deftest channel-a-command-answers-its-words-not-its-panel ()
  ;; A cell's command answers a shell its panel beside its words
  ;; (NLE:SLASH's second value: /index's picker, /link's code). A room
  ;; carries no panel, so the words are the whole answer, and nothing a
  ;; platform would draw as controls rides them.
  (with-temp-store ()
    (with-stubbed-fdefinition (nle:slash (line &key session-id)
                                (values (format nil "ran ~a" line) '(:kind "picker" :rows ())))
      (let ((host (test-host :name "command-panel" :platform (test-platform :choices t))))
        (is (equal '("ran /skills" nil nil)
                   (multiple-value-list
                    (nck::run-command host (test-candidate :channel "100") "/skills"))))))))

(deftest channel-a-command-in-a-room-may-answer-with-a-card ()
  ;; A command run in a room may offer a card (NCK:OFFER-CARD): controls,
  ;; whose presses say their lines, and a panel the platform draws in place of
  ;; its words. Run anywhere else it offers nothing, and nothing is left for
  ;; the next one. Answered through a held interaction, the panel rides the
  ;; answer, which is one message.
  (with-temp-store ()
    (let ((card (list (list (nck:choice "Join" "/voice join"))))
          (panel '(:title "Voice" :text "Not in a voice channel." :tone :stopped)))
      (with-stubbed-fdefinition (nle:slash (line &key session-id)
                                  (nck:offer-card :controls card :panel panel)
                                  (values (format nil "ran ~a" line) nil))
        (let* ((executor (nck:make-recording-executor))
               (host (test-host :name "command-card" :platform (test-platform :choices t)
                                :executor executor)))
          (is (equal (list "ran /voice" card panel)
                     (multiple-value-list
                      (nck::run-command host (test-candidate :channel "100") "/voice"))))
          (is (equal '("ran /help" nil nil)
                     (multiple-value-list
                      (with-stubbed-fdefinition (nle:slash (line &key session-id)
                                                  (values (format nil "ran ~a" line) nil))
                        (nck::run-command host (test-candidate :channel "100") "/help")))))
          (nck::respond host (test-press "Join" "c1")
                        (lambda () (values (format nil "words~%~a" (make-string 3000 :initial-element #\x))
                                          card panel)))
          (flet ((labelled (label)
                   (find label (nck:recording-executor-plans executor)
                         :key #'nck:request-plan-audit-label :test #'equal)))
            (is-present (answer (labelled "interaction_response")) "the press is answered in place"
              (is-plan answer "panel" panel))
            ;; A panel's words past the first chunk are not posted after it.
            (is (null (labelled "send_message")))))))
    (is (equal '(x) (nck:offer-card :controls '(x))))
    (is (null nck::*command-card*))))

(deftest channel-fetch-image-brings-a-picture-down ()
  ;; The borrow half: an image URL becomes a file a lane can post — its
  ;; bytes sniffed, named from the URL, and written where files to post
  ;; live. The network is stubbed; the composition is this test's, a real
  ;; fetch is the live proof's.
  (let ((nck::*image-cache-directory* (temp-path "fetched-images" nil))
        (octets (lambda () (test-png-octets))))
    (with-stubbed-fdefinition (nck::url-octets (url &key timeout) (funcall octets))
      (is-values (path media-type)
          (nck:fetch-image "https://example.test/pea.png")
        (media-type "image/png")
        ((pathname-type path) "png")
        ((search "pea-" (file-namestring path)) is)
        ((probe-file path) is "the file is there to post")
        ((plusp (nlk:file-bytes path)) is)
        ((file-namestring (nck:fetch-image "https://example.test/pea.png"))
         (file-namestring path)))
      (is-values (path media-type)
          (nck:fetch-image "https://example.test/anything"
                           :filename "poster.png")
        ((file-namestring path) "poster.png")
        (media-type "image/png"))
      (setf octets (lambda ()
                     (sb-ext:string-to-octets "<html>not a picture</html>")))
      (is (search "did not answer with an image"
                  (refusal-text error (nck:fetch-image "https://example.test/page"))))
      (setf octets (lambda () (test-png-octets (1+ nle:*image-max-bytes*))))
      (is (search "over the" (refusal-text error (nck:fetch-image "https://example.test/huge.png")))))))

(deftest channel-host-the-card-carries-stop-and-details (with-digest-host (host lane))
  ;; A running card carries Stop beside Details on one row; a settled one
  ;; carries Details alone, the card staying as the turn's record; a queued
  ;; ask's carries none.
  (fact "turn.started" (nlk:json-object))
  (let ((digest (nck:lane-digest lane)))
    (is (equal (list (list (list "Stop" "nck:stop:chat-123-m1" :danger)
                           (list "Details" "nck:details:chat-123-m1" :secondary)))
               (nck::line-controls lane digest)))
    (nck:digest-note-terminal digest :completed nil (nck:now-ms))
    (is (equal (list (list "Details" "nck:details:chat-123-m1" :secondary))
               (nck::line-controls lane digest)))
    (nck:digest-note-queued digest 1)
    (is (null (nck::line-controls lane digest)))))

(deftest channel-host-a-usage-fact-meters-the-card (with-digest-host (host lane))
  ;; A round's usage fact, in the keys the engine writes it under, reaches the
  ;; card's footer as the TUI reads it: what the round ran on, the meter with
  ;; its cache, write and reasoning marks, the prompt in the window, the price.
  (fact "turn.started" (nlk:json-object))
  (fact "turn.usage" (nlk:json-object "input-tokens" 1200 "output-tokens" 40 "cached-input-tokens" 8800
                                      "cache-write-tokens" 300 "reasoning-tokens" 12
                                      "cost_known" t "cost_usd" 0.5 "provider" "qa"
                                      "model" "qa-model" "reasoning-effort" "high"))
  (is (equal (format nil "qa/qa-model (high)~%↑1.2k c88.0% w300 ↓40 r12 · ctx 10.3k · $0.500")
             (getf (nck:digest-card (nck:lane-digest lane) (nck:now-ms)) :meta))))

(deftest channel-host-details-answer-the-presser-alone ()
  ;; A Details press answers the person who pressed, and no one else, with
  ;; every step and the newest thought: the running turn's from its digest,
  ;; a settled card's from what it kept when it settled. Stop's rule says who
  ;; may: the asker and the room's operators.
  (with-digest-host (host lane :responses (replies "s1" "s1" "a1" "s1")
                               :host-keys (:platform (test-platform :choices t)))
    (fact "turn.started" (nlk:json-object))
    (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
    (fact "turn.tool_result" (nlk:json-object "call-id" "c1" "result" "lint: clean"))
    (nck:flush-now host lane)
    (flet ((details (id user &optional (card "s1"))
             (nck:control-pressed host (list :id id :user-id user :message-id card :channel-id "123"
                                             :data "nck:details:chat-123-m1"))
             (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text")))
      (is-carrying (text (details "i1" "mike"))
        "**Steps** · 1" "1. ✓ Ran just lint" "lint: clean")
      (is (equal "Details are for the person who asked and the room's operators."
                 (details "i2" "alice")))
      (said "all clean")
      (fact "turn.completed" (nlk:json-object))
      (is (await-plan executor :label "edit_message" :content "Done in"))
      (is-carrying (text (details "i3" "mike")) "1. ✓ Ran just lint"
        (is (search "lint: clean" text) "a settled card's details are what it kept"))
      (is (equal "This card's steps are no longer held." (details "i4" "mike" "gone"))))))

(deftest channel-host-a-steps-output-answers-the-presser-alone ()
  ;; A step's Output press answers the person who pressed, and no one else,
  ;; with the end of what that step answered: the running turn's from its
  ;; digest, a settled card's from what it kept. Details' rule says who may.
  (with-digest-host (host lane :responses (replies "s1" "s1" "a1" "s1")
                               :host-keys (:platform (test-platform :choices t)))
    (fact "turn.started" (nlk:json-object))
    (called "c1" "{\"form\": \"(sh \\\"just lint\\\")\"}")
    (fact "turn.tool_result" (nlk:json-object "call-id" "c1" "result" (format nil "checking~%lint: clean")))
    (nck:flush-now host lane)
    (is-present (card (await-plan executor :label "send_message"))
      "the card posts its step with its press"
      (is (equal "nck:step:chat-123-m1:1" (fourth (first (getf (plan-field card "card") :steps))))))
    (flet ((output (id user &optional (press "nck:step:chat-123-m1:1"))
             (nck:control-pressed host (list :id id :user-id user :message-id "s1" :channel-id "123"
                                             :data press))
             (plan-field (await-plan executor :path (format nil "/presses/~a" id)) "text")))
      (is-carrying (text (output "i1" "mike"))
        "**1. ✓ Ran just lint" (format nil "```~%checking~%lint: clean~%```"))
      (is (equal "Details are for the person who asked and the room's operators." (output "i2" "alice")))
      (said "all clean")
      (fact "turn.completed" (nlk:json-object))
      (is (await-plan executor :label "edit_message" :content "Done in"))
      ;; A settled card's step is what it kept.
      (is (await (:timeout 10) (gethash "s1" nck::*card-details*)))
      (is-carrying (text (output "i3" "mike")) (format nil "checking~%lint: clean"))
      (is (equal "This step is no longer held." (output "i4" "mike" "nck:step:chat-123-m1:7"))))))

;;; --- a stop in the middle of a turn ----------------------------------------------

(deftest channel-host-a-stop-hands-its-running-lanes-on (with-temp-store ())
  ;; A host stopping under a running turn says so on the turn's line and
  ;; writes the lane down; the next start takes it back on the same line, and
  ;; the turn — resumed by the boot, or still running — answers there. A
  ;; queued ask's line says it never ran.
  (let* ((first-executor (nck:make-recording-executor :responses (replies "s1" "s2")))
         (host (test-host :executor first-executor :name "handoff" :max-concurrent 1))
         (lane (nck:intern-lane (nck:host-lanes host) "chat-123-m1"
                                :target '(:channel-id "123") :trigger-message-id "m1"
                                :owner-id "u8"))
         (queued (nck:intern-lane (nck:host-lanes host) "chat-123-m2"
                                  :target '(:channel-id "123") :trigger-message-id "m2")))
    (nck:start-host host)
    (fold-fact host "chat-123-m1" "turn.started" (nlk:json-object))
    (fold-fact host "chat-123-m1" "turn.tool_call_started" (nlk:json-object "tool-name" "eval"))
    (nck:flush-now host lane)
    (bt2:with-lock-held ((nck:lane-lock queued))
      (nck:digest-note-queued (nck::lane-open-digest queued (nck::now-ms)) 0))
    (nck:flush-now host queued)
    (nck:stop-host host)
    (let ((plans (recorded-plans first-executor)))
      (is-present (paused (plan-matching plans :method "PATCH" :content "Paused at"))
        "the running line says the turn is paused, and picks up"
        (is (search nck::+paused-detail+ (plan-field paused "content")))
        (is (search "/s1" (nck:request-plan-path paused))))
      ;; The queued line says it never ran.
      (is (plan-matching plans :method "PATCH" :content nck::+dropped-detail+))))
  ;; The next start: the same line, the same turn, and its answer lands.
  (let* ((executor (nck:make-recording-executor))
         (host (test-host :executor executor :name "handoff-2")))
    (nck:start-host host)
    (unwind-protect
         (is-present (lane (nck:find-lane (nck:host-lanes host) "chat-123-m1"))
           "the running lane is taken back"
           (is (null (nck:find-lane (nck:host-lanes host) "chat-123-m2")) "the queued one is not")
           (is (equal "t1" (nck:lane-active-turn-id lane)))
           (let ((digest (nck:lane-digest lane)))
             (is (equal "s1" (nck:turn-digest-status-id digest)))
             (is (eq :running (nck:turn-digest-phase digest)))
             (is (= 1 (nck:turn-digest-tool-calls digest))))
           (is (equal '(:channel-id "123") (nck:lane-target lane)))
           (fold-fact host "chat-123-m1" "turn.assistant_message_completed"
                      (nlk:json-object "message" (nlk:json-object "content" "done at last")))
           (fold-fact host "chat-123-m1" "turn.completed" (nlk:json-object))
           (is (await-plan executor :content "done at last") "the answer lands")
           ;; What was written down is taken.
           (is (null (nlk:session-state-get "channel-test" nck::+handoff-key+))))
      (nck:stop-host host))))

(deftest channel-reply-to-a-picture-reads-it-in ()
  ;; "what's wrong here", typed as a reply to someone's screenshot: the ask
  ;; carries the picture it answers, read in under the reply's line, as it
  ;; carries a voice note's words.
  (let* ((host (test-host))
         (ask (test-candidate :text "<@999> what's wrong here" :message "9" :user-name "kim"
                              :reply "" :reply-to "7" :reply-name "bob")))
    (setf (gethash "attachments" (gethash "reply" ask))
          (vector (nlk:json-object "id" "shot" "fetch" (lambda () (test-png-octets))
                                   "media_type" "image/png" "filename" "ci.png")))
    (is-values (prompt images) (nck::ask-prompt-and-attachments
                                (nck:build-ask host ask "chat-100-m9" nil))
      ((search "↩ bob [m7] [Image #1]" prompt) is)
      ((length images) = 1))))

(deftest channel-host-a-line-someone-replied-to-is-settled-not-deleted ()
  ;; A status line goes when the answer lands, unless someone replied to it:
  ;; then it is settled in place, and their reply still points at something.
  (with-digest-host (host lane :responses (replies "ok" "ok"))
    (bt2:with-lock-held ((nck::lane-lock lane))
      (setf (nck::turn-digest-status-id (nck::lane-open-digest lane 0)) "s1"))
    (nck::note-replied-line lane "s1")
    (nck::note-replied-line lane "s9")
    (nck::retire-chrome host lane "s1")
    (is-plan (first (plans)) :method "PATCH" :path "/rooms/123/messages/s1" "content" "answered below")
    ;; A card's message stays a card.
    (is (equal "answered below" (getf (plan-field (first (plans)) "card") :headline)))
    (nck::retire-chrome host lane "s2")
    (is-plan (second (plans)) :method "DELETE" :path "/rooms/123/messages/s2")
    (is (null (gethash "s9" nck::*replied-lines*)) "a reply to anything but the line marks nothing")))

(deftest channel-host-the-card-wears-its-asks-title ()
  ;; The card's title is the ask's task: its first words at once, then the
  ;; title a model writes for it -- the one call its thread is named by too,
  ;; so a thread's card costs no call of its own. An ask of five words or
  ;; fewer is its own title, and costs none.
  (let ((asked '())
        (said "lint is red on main, find out why and fix it if it's small"))
    (with-saved-globals ((nck::*ask-title-generation* :inline)
                         (nck::*ask-titles* (make-hash-table :test #'equal :synchronized t)))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  (declare (ignore system session-id max-tokens fallback-p))
                                  (push user asked)
                                  "Fix the red lint on main")
        (with-digest-host (host lane :responses (replies "s1" "s1"))
          (setf (nck::lane-said lane) said
                (nck::host-status-update-ms host) 0)
          (earn-line)
          (is-present (post (first (plans))) "the card opens under the ask's first words"
            (is (equal "lint is red on main" (getf (gethash "card" (nck:request-plan-body post)) :task)))
            (is (uiop:string-prefix-p (format nil "lint is red on main~%") (plan-content post))))
          (nck::name-thread host "t9" "123" said "chat-123")
          (is (equal (list said) asked) "one call names the card and the thread")
          (nck:flush-now host lane)
          (is-present (edit (second (plans))) "the card takes the written title"
            (is-plan edit :label "edit_message")
            (is (equal "Fix the red lint on main" (getf (gethash "card" (nck:request-plan-body edit)) :task)))))
        (setf asked '())
        (with-digest-host (host lane :responses (replies "s1"))
          (setf (nck::lane-said lane) "fix the auth bug")
          (earn-line)
          (is (equal "fix the auth bug" (getf (gethash "card" (nck:request-plan-body (first (plans)))) :task)))
          (is (null asked) "a short ask is its own title"))))))

(deftest channel-host-a-late-title-retitles-the-settled-card ()
  ;; Naming never holds the turn: the call runs on a thread of its own, the
  ;; turn answers meanwhile and its card settles under the ask's first words;
  ;; the title, landing after, retitles the settled card in place — the whole
  ;; card, its Details and the pictures its message holds restated.
  (let* ((said "lint is red on main, find out why and fix it if it's small")
         (gate (bt2:make-semaphore :name "title-gate")))
    (with-saved-globals ((nck::*ask-title-generation* :thread)
                         (nck::*ask-titles* (make-hash-table :test #'equal :synchronized t)))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  (declare (ignore system user session-id max-tokens fallback-p))
                                  (bt2:wait-on-semaphore gate :timeout 10)
                                  "Fix the red lint on main")
        (with-digest-host (host lane :responses (make-list 8 :initial-element
                                                           (reply 200 "id" "s1" "media" '(("step-1.png" . "900"))))
                                     :host-keys (:platform (test-platform :choices t)))
          (setf (nck::lane-said lane) said)
          (earn-line)
          (said "all clean")
          (fact "turn.completed" (nlk:json-object))
          (is (await-plan executor :content "all clean") "the answer did not wait on the title")
          (is-present (settle (await-plan executor :label "edit_message" :content "Done in"))
            "the card settled under the ask's first words"
            (is (equal "lint is red on main" (getf (gethash "card" (nck:request-plan-body settle)) :task)))
            (is (equal '(("step-1.png" . "900")) (plan-field settle "media")) "keeping what its message holds"))
          (bt2:signal-semaphore gate)
          (is-present (retitle (await-plan executor :label "edit_message" :content "Fix the red lint on main"))
            "the late title retitled the settled card"
            (is-plan retitle :path "/rooms/123/messages/s1")
            (is-shape (gethash "card" (nck:request-plan-body retitle))
              (:task "Fix the red lint on main") (:state :done))
            (is (equal '("Details") (mapcar #'first (plan-field retitle "controls"))))
            (is (equal '(("step-1.png" . "900")) (plan-field retitle "media")))))))))
