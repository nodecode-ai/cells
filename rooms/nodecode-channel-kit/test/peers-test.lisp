;;;; peers-test.lisp --- bots a person names together share one thread.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A person who names two bots in one message asks both: the first to open a
;;;; thread off it wins, the second hears the platform refuse and takes that
;;;; thread, and the bots the message named beside this one are the thread's
;;;; peers -- their lines reach this bot as the thread's context, an ask when
;;;; they name it, at most +PEER-TURNS+ in a run between two lines a person
;;;; typed. The platform is the thread tests' one, refusing a second thread on
;;;; a message the way Discord does.

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
       (remhash (cons "test" thread) nck::*peer-runs*)
       (dolist (table (list nck::*thread-slots* nck::*team-posts* nck::*board-scores* nck::*thread-steers*))
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
  ;; that names this bot is an ask and one that does not is context, and a run
  ;; of peer asks stops at +PEER-TURNS+ until a person types again.
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
        (is (not (nck::candidate-peer-p (said "<@999> your turn" :user "555"))) "a bot the ask never named is no peer")
        (is (not (nck::candidate-peer-p (said "<@999> your turn" :user "999"))) "and this bot is never its own")
        (is (eq :answer (nck:decide-inbound (nck:host-policy host) (said "<@999> your turn"))))
        (is (eq :observe (nck:decide-inbound (nck:host-policy host) (said "noted, thanks"))))
        (let ((ask (said "<@999> again")))
          (is (equal '(:answer :answer :answer :observe)
                     (loop repeat 4 collect (nck::peer-run host ask :answer nil))) "a run of peer asks stops at +PEER-TURNS+")
          (nck::peer-run host (said "go on, you two" :user "u1" :bot nil) :answer nil)
          (is (eq :answer (nck::peer-run host ask :answer nil)) "a person's line starts a new run"))))))

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
