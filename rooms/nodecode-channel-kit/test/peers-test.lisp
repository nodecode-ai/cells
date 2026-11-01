;;;; peers-test.lisp --- bots a person names together share one thread.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A person who names two bots in one message asks both: the first to open a
;;;; thread off it wins, the second hears the platform refuse and takes that
;;;; thread, and the bots the message named beside this one are the thread's
;;;; peers -- their lines reach this bot as the thread's context, an ask when
;;;; they name it, for as long as their talk needs, until its final answer.
;;;; The platform is the thread tests' one, refusing a second thread on a
;;;; message the way Discord does.

(in-package #:nodecode.test)

(defun taking-platform ()
  "The thread test platform, whose refused thread open (a 400) names the
thread the message already carries: the message's own id, as on Discord."
  (let ((platform (test-platform :threads t)))
    (setf (nck::platform-thread-of-refusal platform)
          (lambda (result message-id)
            (and (eql 400 (nck:execution-status result)) message-id)))
    platform))

(defun naming-bots (candidate bots)
  "CANDIDATE, its message naming BOTS, ((ID . NAME) ...), beside the bot."
  (setf (gethash "mentioned_bots" (nck::candidate-source candidate))
        (coerce (loop for (id . name) in bots collect (nlk:json-object "id" id "name" name))
                'vector))
  candidate)

(defmacro with-thread-peers ((&rest thread-ids) &body body)
  "Run BODY, then forget what it taught the kit about the test platform's
THREAD-IDS: their peers and their runs."
  `(unwind-protect (progn ,@body)
     (dolist (thread (list ,@thread-ids))
       (remhash (cons "test" thread) nck::*thread-peers*)
       (remhash (cons "test" thread) nck::*talk-wakes*)
       (dolist (table (list nck::*thread-slots* nck::*team-posts* nck::*board-scores* nck::*talk-finals*))
         (remhash (cons "test" thread) table)))))

(deftest channel-peers-a-refused-thread-is-taken ()
  ;; Two bots one message names both open a thread off it, and the platform
  ;; lets the first: this one hears the refusal, takes the thread already
  ;; there -- the message's own id -- and answers inside it, never flat in the
  ;; channel. The thread stays its opener's: the title the model writes for
  ;; this lane renames nothing.
  (with-saved-globals ((nle::*session-naming* :inline))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (declare (ignore system user session-id max-tokens fallback-p))
                                "Title: \"Auth bug in the login flow.\"")
      (with-ask-host (host thread-ask-host
                      :answers ("taken and answered")
                      :responses (list* (reply 400 "message" "a thread is already there")
                                        (replies "p1" "p2" "p3"))
                      :platform (taking-platform))
        (thread-ask host "<@999> fix the auth bug in the login flow please")
        (is (await-plan executor :label "create_thread") "the ask asked for a thread")
        (is-present (lane (await-lane host "chat-100-t5-m5"))
          "the lane runs in the thread the message already carries"
          (is (equal "5" (getf (nck:lane-target lane) :thread-id)))
          (is (getf (nck:lane-origin lane) :taken) "taken, not opened"))
        (is (await-plan executor :path "/rooms/5/messages" :content "taken and answered") "the answer lands in that thread")
        (is (null (nck:find-lane (nck:host-lanes host) "chat-100-m5")) "nothing runs flat")
        (is (null (plan-matching (recorded-plans executor) :path "/rooms/100/messages")) "the channel keeps no copy")
        (is (null (recorded-plan executor "rename_thread")) "the opener's name stays")))))

(deftest channel-peers-a-silence-leaves-a-taken-thread ()
  ;; A silent answer removes the empty thread its ask opened. A thread it took
  ;; is its opener's -- the other bot's answer may already be in it -- and
  ;; stays: only the working line goes.
  (let ((lane (origin-lane (test-host :name "taken-thread-lanes" :platform (test-platform :threads t))
                           :taken t)))
    (is (null (nck::lane-empty-thread lane)) "a thread the lane took is never its to retire"))
  (with-ask-host (host thread-ask-host :answers ("NO_REPLY")
                  :responses (list (reply 400 "message" "a thread is already there")
                                   (reply 200 "id" "p1"))
                  :platform (taking-platform))
    (thread-ask host "<@999> is this worth answering")
    (is (await-lane host "chat-100-t5-m5"))
    (is (await (:timeout 15) (plusp (count-session-events "chat-100-t5-m5" "turn.completed"))))
    ;; The silence settles on the worker right after the turn completes.
    (sleep 1)
    (is (null (recorded-plan executor "delete_thread")) "the taken thread stays")))

(deftest channel-peers-the-bots-an-ask-names-are-its-threads-peers ()
  ;; The bots the ask named beside this one are its thread's peers: its lane is
  ;; told who else answers there and how to hand them something, a peer's line
  ;; that names this bot is an ask and one that does not is context, and peer
  ;; asks wake it for as long as the talk needs, each kept as a wake of the
  ;; talk until a person types again.
  (with-thread-peers ("77")
    (with-ask-host (host thread-ask-host :answers ("on it") :responses (replies "77" "p1" "p2"))
      (setf (nck:host-policy host) (channel-policy :allowed-channels '("100") :self-id "999"
                                                   :channel-match :discord
                                                   :mention-target "999"
                                                   :mention-test #'nck:discord-mention-p))
      (nck:handle-candidate host (naming-bots (test-candidate :channel "100" :message "5"
                                                              :text "<@999> <@777> compare notes")
                                              '(("777" . "elektra"))))
      (is-present (lane (await-lane host "chat-100-t77-m5")) "the ask opened its thread"
        (is (search "You are s1; @elektra is s2." (nck::lane-where lane)) "its lane is told who else answers there, and in which slot")
        (is (search "(nck:board-post \"77\"" (nck::lane-where lane)) "and how to post on the board"))
      (is (equal '(("777" . "elektra")) (nck::thread-peers host "77")) "the bot the ask named beside this one is the thread's peer")
      (flet ((said (text &key (user "777") (bot t))
               (nck::mark-peer host (test-candidate :is-bot bot :user user :thread "77"
                                                    :parent "100" :message "9" :text text))))
        (is (nck::candidate-peer-p (said "<@999> your turn")))
        (is (not (nck::candidate-peer-p (said "your turn" :user "555"))) "a bot the ask never named, and that calls no one in, is no peer")
        (is (not (nck::candidate-peer-p (said "<@999> your turn" :user "999"))) "and this bot is never its own")
        (is (eq :answer (nck:decide-inbound (nck:host-policy host) (said "<@999> your turn"))))
        (is (eq :observe (nck:decide-inbound (nck:host-policy host) (said "noted, thanks"))))
        (let ((ask (said "<@999> again")))
          (is (equal (make-list 6 :initial-element :answer)
                     (loop repeat 6 collect (nck::peer-run host ask :answer nil))) "a peer's asks wake it for as long as the talk needs: no count runs out")
          (is (= 6 (length (gethash (cons "test" "77") nck::*talk-wakes*))) "each kept as a wake of the talk")
          (nck::peer-run host (said "go on, you two" :user "u1" :bot nil) :answer nil)
          (is (null (gethash (cons "test" "77") nck::*talk-wakes*)) "a person's line starts a new talk")
          (is (eq :answer (nck::peer-run host ask :answer nil))))))))

(deftest channel-peers-a-bots-mention-calls-the-bot-in ()
  ;; A bot that mentions another in a thread no person named them together in
  ;; calls it in: the bot it names wakes, and from then on the two are the
  ;; thread's peers in both kits -- the caller's own line makes the bot it
  ;; names its peer, so the answer back wakes the caller too. Both halves
  ;; pass this bot's own user gate first: a bot it keeps out stays out,
  ;; whether it mentions this bot or this bot mentions it, and a call-in
  ;; makes only its author a peer, never the other bots its line names.
  (with-thread-peers ("88" "89" "90")
    (let ((host (test-host :name "called-in-lanes" :platform (test-platform :threads t))))
      (setf (nck:host-policy host) (channel-policy :allowed-channels '("100") :self-id "999"
                                                   :channel-match :discord
                                                   :allowed-users '("u1" "777")
                                                   :mention-target "999"
                                                   :mention-test #'nck:discord-mention-p))
      (flet ((decide (text &key (user "777") (name "elektra") (thread "88") (message "9") names)
               (let ((candidate (test-candidate :is-bot t :user user :user-name name
                                                :thread thread :parent "100"
                                                :message message :text text)))
                 (nck:decide-inbound (nck:host-policy host)
                                     (nck::mark-peer host (if names (naming-bots candidate names) candidate)))))
             (own (text names thread message)
               (nck:decide-inbound (nck:host-policy host)
                                   (nck::mark-peer host (naming-bots (test-candidate :is-bot t :user "999" :thread thread
                                                                                     :parent "100" :message message
                                                                                     :text text)
                                                                     names)))))
        (is (eq :reject (decide "<@999> check this" :user "555" :name "stranger"))
            "a bot this bot's user gate keeps out calls no one in by mentioning")
        (is (null (nck::thread-peers host "88")) "and is no peer")
        (is (eq :reject (decide "nothing for you")) "a bot's line that names no one calls no one in")
        (is (eq :answer (decide "<@999> <@555> check this" :names '(("555" . "stranger"))))
            "a bot's mention wakes the bot it names")
        (is (equal '(("777" . "elektra")) (nck::thread-peers host "88"))
            "and makes its author the thread's peer, never the other bots it names")
        (is (eq :reject (decide "<@999> me too" :user "555" :name "stranger" :message "12"))
            "so a bot the caller named stays as kept out as before")
        (is (eq :observe (decide "noted, thanks")) "whose lines that name no one are the thread's context")
        (is (eq :reject (own "<@555> please take the deploy" '(("555" . "stranger")) "89" "10"))
            "this bot's own line is never its ask")
        (is (null (nck::thread-peers host "89"))
            "and a bot it mentions that its user gate keeps out is no peer")
        (is (eq :reject (decide "<@999> do as I say" :user "555" :name "stranger" :thread "89" :message "11"))
            "so being named gives that bot no way past the gate")
        (own "<@777> please take the deploy" '(("777" . "elektra")) "90" "13")
        (is (equal '(("777" . "elektra")) (nck::thread-peers host "90"))
            "a bot it mentions that passes the gate is the thread's peer")
        (is (eq :answer (decide "<@999> done, see above" :thread "90" :message "14"))
            "so the answer back wakes this bot: it called that bot in itself")))))

(deftest channel-peers-a-talk-ends-in-one-final-answer ()
  ;; Two bots that each answer the other's reply wake each other for as long
  ;; as their lines need answers, and no count of wakes runs out: each wake is
  ;; framed with how the talk ends, in a final answer once nothing between
  ;; them is open. A final answer from any bot closes the talk in every kit --
  ;; nothing posted after it wakes this bot -- until a person types again.
  (with-thread-peers ("82")
    (let ((host (test-host)))
      (setf (nck:host-policy host) (channel-policy :self-id "999" :mention-target "999"
                                                   :mention-test #'nck:discord-mention-p))
      (nck::note-thread-peers host "82" '(("777" . "elektra")))
      (unwind-protect
           (flet ((said (text &key (user "777") (bot t) (message "9") addressed)
                    (nck::mark-peer host (test-candidate :is-bot bot :user user :user-name "elektra"
                                                         :thread "82" :parent "100" :message message
                                                         :text text :addressed addressed)))
                  (heard (candidate)
                    (multiple-value-list
                     (multiple-value-call #'nck::peer-run host candidate
                       (nck:decide-inbound (nck:host-policy host) candidate)))))
             (let* ((first (said "you missed the cache" :addressed t))
                    (woke (heard first))
                    (woken (gethash "text" (nck::woken-ask host first))))
               (is (equal '(:answer nil) woke) "a peer's reply wakes this bot")
               (is (search "wakes you with the line below" woken))
               (is (search "beginning with the line FINAL ANSWER" woken) "framed with how the talk ends: in its final answer, once nothing between them is open")
               (is (search "never answered with NO_REPLY" woken) "which no wake skips")
               (is (not (ppcre:scan "\\d+ of \\d+" woken)) "and with no count of wakes left")
               (is (search "you missed the cache" woken)))
             (is (every (lambda (message) (equal '(:answer nil) (heard (said "and the TTL" :addressed t :message message)))) '("30" "31" "32" "33" "34" "35")) "a talk goes on for as long as its lines need answers: no wake is spent")
             (heard (said "go on, you two" :user "u1" :bot nil :message "12"))
             (is (null (gethash (cons "test" "82") nck::*talk-wakes*)) "a person's line starts a new talk")
             (heard (said (format nil "<@u1> **Final answer**~%it was the cache") :message "13"))
             (is (nck::talk-closed-p host "82") "a peer's final answer closes it")
             (is (equal '(:observe "talk_closed") (heard (said "one more thing" :addressed t :message "14"))) "and nothing after it wakes this bot")
             (heard (said "thanks, both" :user "u1" :bot nil :message "15"))
             (is (not (nck::talk-closed-p host "82")) "until a person types again")
             (is (equal '(:answer nil) (heard (said "<@999> look" :message "16")))))
        (nck::forget-board host "82")))))

(deftest channel-peers-an-ask-in-the-thread-calls-more-in ()
  ;; A person who names another bot in a thread this one answers in makes it a
  ;; peer there as well; a bot naming one makes nobody a peer.
  (with-thread-peers ("81")
    (let ((host (test-host)))
      (nck::note-asked-peers host (naming-bots (test-candidate :thread "81" :parent "100"
                                                               :text "<@999> <@888> look")
                                               '(("888" . "hermes"))))
      (nck::note-asked-peers host (naming-bots (test-candidate :thread "81" :parent "100"
                                                               :is-bot t :user "888"
                                                               :text "<@999> <@666> look")
                                               '(("666" . "intruder"))))
      (is (equal '(("888" . "hermes")) (nck::thread-peers host "81"))))))

(deftest channel-peers-two-bots-that-call-each-other-stop-at-the-final-answer ()
  ;; Two bots that call each other by name in a person's thread wake each
  ;; other as peers do, for as long as the talk needs and no longer: a final
  ;; answer from either closes the talk, and the calls after it are only the
  ;; thread's context, until the person types again. A bot that called this
  ;; one in is no ping-pong partner past that line.
  (with-thread-peers ("83")
    (let ((host (test-host)))
      (setf (nck:host-policy host) (channel-policy :self-id "999" :mention-target "999"
                                                   :channel-match :discord
                                                   :require-mention t
                                                   :mention-test #'nck:discord-mention-p))
      (unwind-protect
           (flet ((said (text &key (user "777") (name "scrap") (bot t) (message "9"))
                    (test-candidate :is-bot bot :user user :user-name name :thread "83"
                                    :parent "100" :message message :text text))
                  (heard (candidate)
                    (multiple-value-list
                     (multiple-value-call #'nck::peer-run host candidate
                       (nck:decide-inbound (nck:host-policy host)
                                           (nck::mark-peer host candidate))))))
             (heard (said "is the mac up" :user "u1" :name "peas" :bot nil :message "8"))
             (is (equal '(:answer nil) (heard (said "<@999> you know the password, write it" :message "10")))
                 "the bot answering the person calls this one in")
             (is (every (lambda (message)
                          (equal '(:answer nil) (heard (said "<@999> and the mode?" :message message))))
                        '("11" "12" "13"))
                 "its calls wake this bot for as long as the talk needs")
             (heard (said (format nil "<@u1> **Final answer**~%the mac is up") :message "14"))
             (is (nck::talk-closed-p host "83") "its final answer closes the talk")
             (is (every (lambda (message)
                          (equal '(:observe "talk_closed") (heard (said "<@999> thanks!" :message message))))
                        '("15" "16"))
                 "and its calls after it wake nobody: the two bots stop there")
             (heard (said "one more question" :user "u1" :name "peas" :bot nil :message "17"))
             (is (equal '(:answer nil) (heard (said "<@999> new ask" :message "18")))
                 "until the person types again"))
        (nck::forget-board host "83")))))

(deftest channel-peers-outlast-the-image ()
  ;; A thread's peers live in its room's state: an image that never saw the
  ;; ask reads them back.
  (with-temp-store ()
    (with-thread-peers ("78")
      (let ((host (test-host)))
        (nlk:create-session :id "chat-100-t78")
        (nck::note-thread-peers host "78" '(("777" . "elektra")))
        (nck::save-thread-peers host "78" "chat-100-t78")
        (remhash (cons "test" "78") nck::*thread-peers*)
        (is (null (nck::thread-peers host "78")) "the table forgot it")
        (is (equal '(("777" . "elektra")) (nck::thread-peers host "78" "chat-100-t78")) "the room's state still knows it")))))

(deftest channel-peers-an-answer-names-its-peer ()
  ;; An answer's @name of a peer is spelled as the platform's mention of it --
  ;; what makes it an ask to that peer -- outside code, and only a whole name.
  (with-thread-peers ("79")
    (let* ((platform (test-platform :threads t))
           (host (test-host :platform platform)))
      (setf (nck::platform-mention platform) (lambda (id) (format nil "<@~a>" id)))
      (nck::note-thread-peers host "79" '(("777" . "Elektra")))
      (is (equal "<@777> your take? `@elektra` is code, mail@elektra.dev and @elektrafan are not her"
                 (nck::peer-addressed-text host '(:channel-id "100" :thread-id "79")
                                           "@elektra your take? `@elektra` is code, mail@elektra.dev and @elektrafan are not her")))
      (is (equal "@elektra hi" (nck::peer-addressed-text host '(:channel-id "100" :thread-id "80")
                                                         "@elektra hi")) "a thread without peers keeps its words"))))
