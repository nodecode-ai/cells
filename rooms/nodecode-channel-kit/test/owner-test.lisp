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
  ;; A role every bot holds reaches all three: each kit ranks them the same
  ;; way, so one owns the ask, and every later line of its thread too. A line
  ;; that names a bot, or answers its message, puts that bot first; one that
  ;; names this bot is this bot's, and one that names several is a team's.
  (with-thread-peers ("5" "8")
    (let* ((hosts (mapcar (lambda (bot) (owner-host (car bot))) *owner-bots*))
           (ranks (loop for host in hosts
                        for (self) in *owner-bots*
                        collect (nck::ask-rank host (person-line self "<@&r1> next priority"
                                                                 :channel "100" :message "5"))))
           (owner (first (first ranks))))
      (is (every (lambda (rank) (equal rank (first ranks))) ranks) "every kit ranks the ask the same way")
      (is (equal '("555" "777" "999") (sort (copy-list (first ranks)) #'string<)) "and ranks every bot it reached")
      (is (= 1 (count owner (mapcar #'nck::host-self-id hosts) :test #'equal)) "so exactly one bot owns it")
      (loop for host in hosts
            for (self) in *owner-bots*
            do (nck::note-thread-peers host "5" (others-of self)))
      (is (every (lambda (host self)
                   (equal (first ranks)
                          (nck::ask-rank host (person-line self "and the Mac?" :bots :none :thread "5"
                                                                                :parent "100" :message "6"))))
                 hosts (mapcar #'car *owner-bots*)) "a later line of its thread, naming no one, has the same owner")
      (let ((host (first hosts)))
        (flet ((rank (text &rest keys)
                 (nck::ask-rank host (apply #'person-line "999" text
                                            (append keys (list :thread "5" :parent "100" :message "7"))))))
          (is (null (rank "<@999> you take it")) "a line that names this bot is its own")
          (is (equal "555" (first (rank "what about this" :bots :none :reply "the plan" :reply-to "40"
                                                          :replied "555"))) "a line answering a bot's message is that bot's to own")
          (is (equal "777" (first (rank "<@777> take this" :bots '(("777" . "elektra"))))) "and so is one that names a bot")
          (is (null (rank "<@777> <@555> compare notes")) "one that names several bots is a team's")
          (is (null (nck::ask-rank host (test-candidate :is-bot t :user "777" :thread "5" :parent "100"
                                                        :message "9" :text "<@&r1> hi"))) "a bot's line has no owner to rank")
          (nck::note-thread-peers host "8" (others-of "999"))
          (setf (gethash (cons "test" "8") nck::*thread-slots*) '("999" "777" "555"))
          (is (null (nck::ask-rank host (person-line "999" "go on" :bots :none :thread "8" :parent "100"
                                                     :message "10"))) "and a team's board keeps its own rules"))))))

(defun owner-line-where (self owner-p)
  "The role ping in channel 100, as the bot SELF reads it, on the first
message id where SELF owns it (OWNER-P) or does not."
  (loop with host = (owner-host self)
        for n from 5 below 60
        for line = (person-line self "<@&r1> next priority" :channel "100" :message (princ-to-string n))
        when (eq owner-p (equal self (first (nck::ask-rank host line))))
          return line))

(deftest channel-owner-the-owner-answers-and-is-told-so ()
  ;; The bot that owns an ask answers it at once, and its lane is told who
  ;; else read it and that the whole of it is this lane's.
  ;; The thread the ask opens carries the ask's own id, as on Discord: every
  ;; later line of it has the owner the ask had.
  (let* ((line (owner-line-where "999" t))
         (id (nck::source-field line "message_id")))
   (with-thread-peers (id)
    (with-ask-host (host thread-ask-host :answers ("owned") :responses (replies id "p1" "p2"))
      (setf (nck:host-policy host) (owner-policy "999"))
      (progn
        (is (eq :answer (nck:handle-candidate host line)))
        (is-present (lane (await-lane host (format nil "chat-100-t~a-m~a" id id)))
            "the owner opened the ask's lane"
          (is (search "too, and it is yours: you own it." (nck::lane-where lane)) "and its lane is told the ask is its own")
          (is (search "@elektra" (nck::lane-where lane)) "and who else read it")
          (is (null (nck::thread-slots host id)) "a role's ping opens no team's board"))
        (is (await-plan executor :content "owned") "the owner's answer posts"))))))

(deftest channel-owner-a-bot-takes-over-an-ask-its-owner-left ()
  ;; A bot ranked after the owner keeps the ask as context and posts nothing;
  ;; when its grace ends with no bot ranked before it seen in the thread, it
  ;; takes the ask over and answers -- and an owner seen at work keeps it.
  (let ((nck::+owner-grace-seconds+ 0.3))
    (with-thread-peers ("5")
      (with-ask-host (host thread-ask-host :answers ("taken over") :responses (replies "5" "p1" "p2"))
        (setf (nck:host-policy host) (owner-policy "999"))
        (let ((line (person-line "999" "<@&r1> next priority" :channel "100" :message "5"
                                 :reply "the plan" :reply-to "40" :replied "777")))
          (is (equal "777" (first (nck::ask-rank host line))) "the bot whose message it answers owns it")
          (is (eq :observe (nck:handle-candidate host line)) "so this bot only reads it")
          (sleep 0.1)
          (is (null (plan-matching (recorded-plans executor) :content "taken over")) "and posts nothing while its owner may answer")
          (is (await-plan executor :content "taken over") "until its grace ends with the owner unseen, and it takes the ask over"))))
    (with-thread-peers ("6")
      (with-ask-host (host thread-ask-host :answers ("taken over") :responses (replies "6" "p1" "p2"))
        (setf (nck:host-policy host) (owner-policy "999"))
        (let ((line (person-line "999" "<@&r1> next priority" :channel "100" :message "6"
                                 :reply "the plan" :reply-to "40" :replied "777")))
          (is (eq :observe (nck:handle-candidate host line)))
          (is (equal '("555" "777") (sort (mapcar #'car (nck::thread-peers host "6")) #'string<)) "the bots it reached are the thread's peers here too")
          (is (nck::ask-rank host (person-line "999" "and the Mac?" :bots :none :thread "6" :parent "100" :message "7")) "so a later line of the thread has an owner as well")
          ;; The owner's eye on the ask, left in the channel it was typed in.
          (nck::note-bot-activity host (test-candidate :channel "100" :message "41" :user "777" :is-bot t
                                                       :reaction "add" :reply-to "6" :text "👀"))
          (sleep 1.5)
          (is (null (plan-matching (recorded-plans executor) :content "taken over")) "an owner seen at work keeps its ask")
          (is (null (nck:find-lane (nck:host-lanes host) "chat-100-t6-m6")) "and no lane opened here"))))))

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
