;;;; owner-test.lisp --- an ask that reaches several bots has one owner; answers stay short.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defparameter *owner-bots* '(("999" . "scrap") ("777" . "elektra") ("555" . "hindsight"))
  "Three bots that share role r1, as the owner tests' kits know them.")

(defun owner-policy (self)
  "The policy of the bot SELF: channel 100, mentioned by its id or role r1."
  (channel-policy :allowed-channels '("100") :self-id self :channel-match :discord
                  :mention-target self
                  :mention-test (lambda (text target) (nck:discord-mention-p text target '("r1")))))

(defun owner-host (self)
  "An unstarted host for the bot SELF."
  (let ((host (test-host :name (format nil "owner-~a" self) :platform (test-platform :threads t))))
    (setf (nck:host-policy host) (owner-policy self))
    host))

(defun others-of (self)
  "The owner tests' bots other than SELF."
  (remove self *owner-bots* :key #'car :test #'equal))

(defun person-line (self text &rest keys &key bots replied &allow-other-keys)
  "TEXT typed by a person, as the kit of the bot SELF reads it: BOTS, the
other bots it reaches (all of them by default), and REPLIED, the author of
the message it answers."
  (let ((candidate (apply #'test-candidate :text text
                          (uiop:remove-plist-keys '(:bots :replied) keys))))
    (when replied
      (setf (gethash "user_id" (gethash "reply" candidate)) replied))
    (naming-bots candidate (if (eq bots :none) nil (or bots (others-of self))))))

(deftest channel-owner-one-bot-owns-an-ask-every-bot-reaches ()
  ;; A line naming no one in a thread all three bots take part in reaches
  ;; all three: each kit ranks them the same way, so one owns it, and every
  ;; later line of the thread too. A line that names a bot, or answers its
  ;; message, puts that bot first; one that names this bot is this bot's,
  ;; and one that names several, or pings their role, is a team's.
  (with-thread-peers ("5" "8")
    (let* ((hosts (mapcar (lambda (bot) (owner-host (car bot))) *owner-bots*))
           (ranks (progn
                    (loop for host in hosts
                          for (self) in *owner-bots*
                          do (nck::note-thread-peers host "5" (others-of self)))
                    (loop for host in hosts
                          for (self) in *owner-bots*
                          collect (nck::ask-rank host (person-line self "next priority" :bots :none :thread "5"
                                                                                   :parent "100" :message "5")))))
           (owner (first (first ranks))))
      (is (every (lambda (rank) (equal rank (first ranks))) ranks) "every kit ranks the ask the same way")
      (is (equal '("555" "777" "999") (sort (copy-list (first ranks)) #'string<)) "and ranks every bot it reached")
      (is (= 1 (count owner (mapcar #'nck::host-self-id hosts) :test #'equal)) "so exactly one bot owns it")
      (is (every (lambda (host self)
                   (equal (first ranks)
                          (nck::ask-rank host (person-line self "and the Mac?" :bots :none :thread "5"
                                                                                :parent "100" :message "6"))))
                 hosts (mapcar #'car *owner-bots*)) "a later line of the thread, naming no one, has the same owner")
      (let ((host (first hosts)))
        (flet ((rank (text &rest keys)
                 (nck::ask-rank host (apply #'person-line "999" text
                                            (append keys (list :thread "5" :parent "100" :message "7"))))))
          (is (null (rank "<@999> you take it")) "a line that names this bot is its own")
          (is (equal "555" (first (rank "what about this" :bots :none :reply "the plan" :reply-to "40"
                                                          :replied "555"))) "a line answering a bot's message is that bot's to own")
          (is (equal "777" (first (rank "<@777> take this" :bots '(("777" . "elektra"))))) "and so is one that names a bot")
          (is (null (rank "<@777> <@555> compare notes")) "one that names several bots is a team's")
          (is (null (rank "<@&r1> all of you")) "and so is one that pings their role")
          (is (null (nck::ask-rank host (test-candidate :is-bot t :user "777" :thread "5" :parent "100"
                                                        :message "9" :text "<@&r1> hi"))) "a bot's line has no owner to rank")
          (nck::note-thread-peers host "8" (others-of "999"))
          (setf (gethash (cons "test" "8") nck::*thread-slots*) '("999" "777" "555"))
          (is (null (nck::ask-rank host (person-line "999" "go on" :bots :none :thread "8" :parent "100"
                                                     :message "10"))) "and a team's board keeps its own rules"))))))

(deftest channel-owner-a-role-ping-seats-the-team ()
  ;; A role every bot holds calls them all in, as a line naming them all by
  ;; name does: the ask is the team's. No bot owns it, each answers it, and
  ;; the thread it opens is the team's board, every bot it reached on it.
  (loop for (self) in *owner-bots*
        do (is (null (nck::ask-rank (owner-host self)
                                    (person-line self "<@&r1> next priority" :channel "100" :message "5")))
               "no bot owns a role's ping"))
  (with-thread-peers ("5")
    (with-ask-host (host thread-ask-host :answers ("on it") :responses (replies "5" "p1" "p2"))
      (setf (nck:host-policy host) (owner-policy "999"))
      (is (eq :answer (nck:handle-candidate host (person-line "999" "<@&r1> next priority"
                                                              :channel "100" :message "5")))
          "each bot it reaches answers it")
      (is-present (lane (await-lane host "chat-100-t5-m5")) "in the thread the ask opens"
        (is (await-plan executor :content "on it"))
        (is (equal '("555" "777" "999") (nck::thread-slots host "5")) "which is the team's board")
        (is (null (search "it is yours" (nck::lane-where lane))) "and no bot is told the ask is its own")))))

(defun owner-line-where (self owner-p)
  "A person's line naming no one in a thread the three bots share, as the
bot SELF reads it, in the first thread where SELF owns it (OWNER-P) or does
not. That thread's peers stay noted: the caller forgets them."
  (loop with host = (owner-host self)
        for n from 5 below 60
        for thread = (princ-to-string n)
        for line = (person-line self "next priority" :bots :none :thread thread :parent "100"
                                                     :message (princ-to-string (+ n 100)))
        do (setf (gethash (nck::peer-key host thread) nck::*thread-peers*) (others-of self))
        when (eq owner-p (equal self (first (nck::ask-rank host line))))
          return line
        do (remhash (nck::peer-key host thread) nck::*thread-peers*)))

(deftest channel-owner-the-owner-answers-and-is-told-so ()
  ;; The bot that owns an ask answers it at once, and its lane is told who
  ;; else read it and that the whole of it is this lane's.
  (let* ((line (owner-line-where "999" t))
         (thread (nck::source-field line "thread_id"))
         (message (nck::source-field line "message_id")))
   (setf (gethash "joined" (gethash "source" line)) t)
   (with-thread-peers (thread)
    (with-ask-host (host thread-ask-host :answers ("owned") :responses (replies thread "p1" "p2"))
      (setf (nck:host-policy host) (owner-policy "999"))
      (setf (gethash (nck::peer-key host thread) nck::*thread-peers*) (others-of "999"))
      (progn
        (is (eq :answer (nck:handle-candidate host line)))
        (is-present (lane (await-lane host (format nil "chat-100-t~a-m~a" thread message)))
            "the owner opened the ask's lane"
          (is (search "too, and it is yours: you own it. They read it and stay quiet" (nck::lane-where lane)) "and its lane is told the ask is its own")
          (is (search "@elektra" (nck::lane-where lane)) "and who else read it")
          (is (null (nck::thread-slots host thread)) "an ask one bot owns opens no team's board"))
        (is (await-plan executor :content "owned") "the owner's answer posts"))))))

(deftest channel-owner-a-bot-takes-over-an-ask-its-owner-left ()
  ;; A bot ranked after the owner keeps the ask as context and posts nothing;
  ;; when its grace ends with no bot ranked before it seen in the thread, it
  ;; takes the ask over and answers -- and an owner seen at work keeps it.
  (let ((nck::+owner-grace-seconds+ 0.3))
    (with-thread-peers ("5")
      (with-ask-host (host thread-ask-host :answers ("taken over") :responses (replies "5" "p1" "p2"))
        (setf (nck:host-policy host) (owner-policy "999"))
        ;; A line answering another bot's message, in a channel every bot
        ;; reads; the reply pings that bot.
        (let ((line (person-line "999" "what about this" :channel "100" :message "5" :bots '(("777" . "elektra"))
                                 :reply "the plan" :reply-to "40" :replied "777")))
          (is (equal "777" (first (nck::ask-rank host line))) "the bot whose message it answers owns it")
          (is (eq :observe (nck:handle-candidate host line)) "so this bot only reads it")
          (sleep 0.1)
          (is (null (plan-matching (recorded-plans executor) :content "taken over")) "and posts nothing while its owner may answer")
          (is (await-plan executor :content "taken over") "until its grace ends with the owner unseen, and it takes the ask over"))))
    (with-thread-peers ("6")
      (with-ask-host (host thread-ask-host :answers ("taken over") :responses (replies "6" "p1" "p2"))
        (setf (nck:host-policy host) (owner-policy "999"))
        (let ((line (person-line "999" "what about this" :channel "100" :message "6" :bots '(("777" . "elektra"))
                                 :reply "the plan" :reply-to "40" :replied "777")))
          (is (eq :observe (nck:handle-candidate host line)))
          (is (equal '("777") (mapcar #'car (nck::thread-peers host "6"))) "the bots it reached are the thread's peers here too")
          (is (nck::ask-rank host (person-line "999" "and the Mac?" :bots :none :thread "6" :parent "100" :message "7")) "so a later line of the thread has an owner as well")
          ;; The owner's eye on the ask, left in the channel it was typed in.
          (nck::note-bot-activity host (test-candidate :channel "100" :message "41" :user "777" :is-bot t
                                                       :reaction "add" :reply-to "6" :text "👀"))
          (sleep 1.5)
          (is (null (plan-matching (recorded-plans executor) :content "taken over")) "an owner seen at work keeps its ask")
          (is (null (nck:find-lane (nck:host-lanes host) "chat-100-t6-m6")) "and no lane opened here"))))))

(deftest channel-owner-kits-that-know-different-peers-pick-one-owner ()
  ;; Each kit keeps its own record of a thread's peers, and those differ -- a
  ;; kit that restarted, a peer called in while another kit was away. Who
  ;; posted in the thread is what every kit reads alike, so the bot that
  ;; answered there last owns its next line that names no one, in every kit,
  ;; and a post after the line never moves its owner.
  (with-thread-peers ("50")
    (let ((hosts (mapcar (lambda (bot) (owner-host (car bot))) *owner-bots*)))
      (flet ((owners (message)
               (loop for host in hosts
                     for (self) in *owner-bots*
                     collect (first (nck::ask-rank host (person-line self "and the Mac?" :bots :none :thread "50"
                                                                              :parent "100" :message message))))))
        (loop for host in hosts
              for peers in (list (others-of "999") '(("999" . "scrap")) '(("777" . "elektra")))
              do (setf (gethash (nck::peer-key host "50") nck::*thread-peers*) peers))
        ;; The posts all three kits saw -- the tests' kits share one table.
        (nck:note-bot-presence (first hosts) '(:post "777" "elektra" "50" "98"))
        (nck:note-bot-presence (first hosts) '(:post "555" "hindsight" "50" "102"))
        (is (equal '("555" "555" "555") (owners "103")) "the bot that answered last owns the next line, in every kit")
        (nck:note-bot-presence (first hosts) '(:post "999" "scrap" "50" "104"))
        (is (equal '("555" "555" "555") (owners "103")) "a post after the line never moves its owner")
        (is (equal '("999" "999" "999") (owners "105")) "and the line after it goes to the bot that answered last")
        (nck:note-bot-presence (first hosts) '(:post "999" "scrap" "50" "99"))
        (is (equal "999" (first (first (list (owners "105"))))) "an older post never replaces a bot's latest")))))

(defun steered-follow-up (thread message)
  "A person's line in THREAD, naming no one, as the bot 999 reads it in a
thread it takes part in."
  (let ((line (person-line "999" "and the Mac?" :bots :none :thread thread :parent "100" :message message)))
    (setf (gethash "joined" (gethash "source" line)) t)
    line))

(deftest channel-owner-an-owner-busy-with-a-follow-up-keeps-it ()
  ;; A person's follow-up while the owner's turn runs is steered into that
  ;; turn, and the eye the owner leaves on it is all the room sees: a bot's
  ;; reaction routes no line. The adapter reads it all the same
  ;; (NOTE-BOT-PRESENCE), so the bot ranked next never answers the line a
  ;; second time -- and without that eye, it does take it over.
  (let ((nck::+owner-grace-seconds+ 0.3)
        (nck::+owner-activity-slack-ms+ 50))
    (loop for (thread eye) in '(("50" t) ("51" nil))
          do (with-thread-peers (thread)
               (with-ask-host (host thread-ask-host :answers ("taken over") :responses (replies thread "p1" "p2"))
                 (setf (nck:host-policy host) (owner-policy "999"))
                 (setf (gethash (nck::peer-key host thread) nck::*thread-peers*) (others-of "999"))
                 ;; The owner's card, posted when its turn began.
                 (nck:note-bot-presence host (list :post "777" "elektra" thread "60"))
                 (sleep 0.1)
                 (let ((line (steered-follow-up thread "61")))
                   (is (equal "777" (first (nck::ask-rank host line))) "the bot that answered last owns the follow-up")
                   (is (eq :observe (nck:handle-candidate host line)) "so this bot only reads it")
                   (when eye
                     (sleep 0.05)
                     (nck:note-bot-presence host (list :seen "777" thread)))
                   (if eye
                       (progn (sleep 1.0)
                              (is (null (plan-matching (recorded-plans executor) :content "taken over"))
                                  "the owner's eye on the line keeps it the owner's"))
                       (is (await-plan executor :content "taken over")
                           "and with no eye on it, the bot ranked next takes it over"))))))))

(deftest channel-owner-answer-words-count-what-a-person-reads ()
  ;; Prose words count; code blocks, markup alone and the FINAL ANSWER line do not.
  (is (= 4 (nck::answer-word-count "Fixed: #39 is merged.")))
  (is (= 3 (nck::answer-word-count (format nil "FINAL ANSWER~%- **It works** now~%```~%make test all~%```"))))
  (is (= 0 (nck::answer-word-count "")))
  (is (= 2 (nck::answer-word-count (format nil "**Final answer**~%one - two")))))

(deftest channel-owner-a-long-answer-posts-cut-down-with-the-whole-attached ()
  ;; Past answer_words the answer posts as its own model cut it down, and the
  ;; whole answer rides along as a file; within the cap it posts as written.
  (let ((long "The registry answers on port 80, and the cache warms in ten minutes after a deploy.")
        (asked '()))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (declare (ignore max-tokens fallback-p))
                                (push (list system user session-id) asked)
                                "Registry: port 80.")
      (with-digest-host (host lane :name "short-answers" :host-keys (:platform (test-platform :files t)))
        (setf (nck::host-answer-words host) 8)
        (is (search "post at most 8 words" (nck::answer-cap-paragraph host)) "the lane is told the cap")
        (fact "turn.started" (nlk:json-object))
        (said long)
        (fact "turn.completed" (nlk:json-object))
        (is-present (answer (await-plan executor :content "Registry: port 80.")) "the answer posts cut down"
          (is (equal "Registry: port 80." (plan-content answer)) "and only that")
          (is (equal '("full-answer.md") (coerce (plan-field answer "files") 'list)) "with the whole answer attached"))
        (is-present (call (first asked)) "its own model cut it"
          (is (search "at most 8 words" (first call)))
          (is (equal long (second call)) "from the whole answer")
          (is (equal "chat-123-m1" (third call)) "on the lane's own session"))
        (multiple-value-bind (text file) (nck::shorten-answer host lane long nil)
          (is (equal "Registry: port 80." text))
          (is (equal long (uiop:read-file-string file)) "the file holds the whole answer"))
        (is (equal (format nil "FINAL ANSWER~%Registry: port 80.")
                   (nck::shorten-answer host lane (format nil "FINAL ANSWER~%~a" long) t)) "a final answer stays one")
        (is (equal "Port 80 works." (nck::shorten-answer host lane "Port 80 works." nil)) "an answer within the cap posts as written")
        (setf (nck::host-answer-words host) 0)
        (is (equal long (nck::shorten-answer host lane long nil)) "and with no cap, every answer does")))))
