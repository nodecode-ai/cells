;;;; team-test.lisp --- bots a person names together work one task, the thread their board.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A thread whose bots hold slots is a team's board: every line said there
;;;; reaches the turn running there, a peer is let in whatever this bot's
;;;; allowed_users say, a post keeps the slot its poster holds, a score comes
;;;; from the scorer alone, and the best measured result writes the one answer.

(in-package #:nodecode.test)

(defmacro with-team-thread ((host thread &key (self "999") peers) &body body)
  "Run BODY with HOST a bare test host speaking as SELF, whose THREAD has
PEERS, ((ID . NAME) ...), beside it and every bot in the slot the order
names; forget the thread after."
  `(let ((,host (test-host :name "team-lanes" :platform (test-platform :threads t)
                           :executor (nck:make-recording-executor :responses (replies "p1" "p2" "p3" "p4")))))
     (setf (nck:host-policy ,host) (channel-policy :allowed-channels '("100") :self-id ,self
                                                   :allowed-users '("u1")
                                                   :channel-match :discord
                                                   :mention-target ,self
                                                   :mention-test #'nck:discord-mention-p))
     (unwind-protect
          (progn
            (nck::note-thread-peers ,host ,thread ,peers)
            (setf (gethash (cons "test" ,thread) nck::*thread-slots*)
                  (cons ,self (mapcar #'car ,peers)))
            ,@body)
       (nck::forget-board ,host ,thread)
       (remhash (cons "test" ,thread) nck::*thread-peers*)
       (remhash (cons "test" ,thread) nck::*talk-wakes*))))

(defun said-in-thread (host text &key (user "777") (bot t) (message "9") (name "elektra"))
  "A line USER said in thread 42 of channel 100, marked a peer's where it is one."
  (nck::mark-peer host (test-candidate :is-bot bot :user user :user-name name :thread "42"
                                       :parent "100" :message message :text text)))

(defun running-thread-lane (host &optional (turn "turn-1"))
  "HOST's lane in thread 42, running TURN."
  (let ((lane (nck:intern-lane (nck:host-lanes host) "chat-100-t42-m5"
                               :target '(:channel-id "100" :thread-id "42")
                               :trigger-message-id "5")))
    (setf (nck:lane-active-turn-id lane) turn)
    lane))

(deftest channel-team-slots-follow-the-order-the-person-named ()
  ;; Each kit reads the same message, so each numbers the bots alike: by
  ;; where the text names them, this one included. A later ask that names
  ;; another bot adds it at the end; no bot ever moves from its place.
  (is (equal '("3" "1" "2") (nck::mention-order "<@3> then <@!1> and <@2>" '("1" "2" "3"))))
  (is (equal '("2" "1") (nck::mention-order "only <@2>" '("1" "2"))) "an id the text never names goes last")
  ;; A role ping names none of them: each bot gives its own id first, and
  ;; all must still reach one order.
  (is (equal '("9" "30" "100") (nck::mention-order "<@&5> go" '("100" "9" "30")))
      "ids the text never names go by id, whichever comes first")
  (is (equal '("30" "9" "100") (nck::mention-order "<@30> <@&5> go" '("100" "9" "30"))))
  (let ((host (test-host :name "team-slots")))
    (setf (nck:host-policy host) (channel-policy :self-id "999"))
    (unwind-protect
         (flet ((ask (text bots)
                  (naming-bots (test-candidate :thread "61" :parent "100" :text text) bots)))
           (let ((slots (nck::note-thread-slots host "61" (ask "<@777> <@999> <@888> go"
                                                                '(("777" . "elektra") ("888" . "hindsight"))))))
             (is (equal '("777" "999" "888") slots) "slots are the order the person named the bots in"))
           (let ((slots (nck::note-thread-slots host "61" (ask "<@555> <@999> <@888> more"
                                                                '(("555" . "new") ("888" . "hindsight"))))))
             (is (equal '("777" "999" "888" "555") slots) "a later ask adds a bot at the end and moves none"))
           (is (= 2 (nck::slot-of host "61" "999"))))
      (nck::forget-board host "61"))))

(deftest channel-team-a-peer-passes-the-user-gate-in-its-thread ()
  ;; A bot that lets in only its people still hears the peers a person called
  ;; in beside it -- there, and only as peers: a bot it never met stays out.
  (with-team-thread (host "42" :peers '(("777" . "elektra")))
    (let ((policy (nck:host-policy host)))
      (is (eq :observe (nck:decide-inbound policy (said-in-thread host "noted"))) "a peer's line is context")
      (is (eq :answer (nck:decide-inbound policy (said-in-thread host "<@999> your turn"))) "and its mention an ask")
      (let ((stranger (said-in-thread host "<@999> your turn" :user "555")))
        (is (eq :reject (nck:decide-inbound policy stranger)) "a bot that is no peer is refused")))))

(deftest channel-team-a-peers-ask-continues-the-thread-lane ()
  ;; A peer's line that names this bot is the thread's conversation: it goes
  ;; to the lane in that thread, never to a fork blind to its work.
  (with-team-thread (host "42" :peers '(("777" . "elektra")))
    (let ((lane (running-thread-lane host)))
      (is (eq lane (nck::conversation-lane host (said-in-thread host "<@999> look at this")))))))

(deftest channel-team-a-line-said-in-the-thread-reaches-the-running-turn ()
  ;; While this bot's turn runs in a team's thread, a peer's line and a
  ;; person's line said to someone else steer it, framed as what they are;
  ;; a settled lane keeps them as the context of its next ask.
  (with-team-thread (host "42" :peers '(("777" . "elektra")))
    (let ((lane (running-thread-lane host))
          (steered '()))
      ;; A room that takes a mention, as #general does for most bots.
      (setf (nck::inbound-policy-require-mention (nck:host-policy host)) t)
      (with-stubbed-fdefinition (nck::continue-lane (host lane ask)
                                  (push (list lane ask) steered))
        (is (eq :observe (nck:handle-candidate host (said-in-thread host "**CLAIM** s2 · bisect the config loader"))))
        (is-present (heard (first steered)) "a peer's post steered the running turn"
          (is (eq lane (first heard)))
          (is (search "A CLAIM post from your peer s2, elektra" (nck::ask-prompt (second heard))))
          (is (search "bisect the config loader" (nck::ask-prompt (second heard))))
          (is (null (nck::ask-message-id (second heard))) "and binds no address"))
        (nck:handle-candidate host (said-in-thread host "elektra, check the tests too"
                                                   :user "u1" :bot nil :message "10" :name "alice"))
        (is (= 2 (length steered)) "a person's line said to someone else steered it too")
        (is (search "to someone else" (nck::ask-prompt (second (first steered)))))
        (is (null (nck::lane-heard-line lane)) "a board post or a person's line is nothing to reply to")
        (nck:handle-candidate host (said-in-thread host "**ADOPT** s2 · score 1/2 · reran it" :message "12"))
        (is (= 2 (length steered)) "an ADOPT waits for the next ask")
        (nck:handle-candidate host (said-in-thread host "my whole answer" :message "13"))
        (is (= 3 (length steered)) "a peer's untagged line steers it: the turn builds on it")
        (is (search "Your answer goes up as a reply to it" (nck::ask-prompt (second (first steered)))))
        (is (equal '("13" . "turn-1") (nck::lane-heard-line lane)) "the line the next answer replies to")
        (setf (nck:lane-active-turn-id lane) nil)
        (nck:handle-candidate host (said-in-thread host "done here" :message "11"))
        (is (= 3 (length steered)) "a settled lane is not steered")
        (setf (nck::lane-heard-line lane) nil)))))

(deftest channel-team-a-final-answer-closes-the-talk ()
  ;; A peer's final answer reaches the turn running in its thread once, as
  ;; what it is; after it a peer's line neither steers nor wakes this bot,
  ;; until a person types again. Before it, every line of a peer's steers the
  ;; running turn: no count of steers runs out.
  (with-team-thread (host "42" :peers '(("777" . "elektra")))
    (let ((lane (running-thread-lane host))
          (steered '()))
      (setf (nck::inbound-policy-require-mention (nck:host-policy host)) t)
      (with-stubbed-fdefinition (nck::continue-lane (host lane ask)
                                  (push (list lane ask) steered))
        (is (eq :observe (nck:handle-candidate
                          host (said-in-thread host (format nil "<@u1> **Final answer**~%the cache it is")
                                               :message "20"))))
        (is-present (heard (first steered)) "the final answer reaches the running turn"
          (is (search "posted the thread's final answer" (nck::ask-prompt (second heard)))))
        (is (nck::talk-closed-p host "42"))
        (nck:handle-candidate host (said-in-thread host "one more thought" :message "21"))
        (is (= 1 (length steered)) "a peer's line after it steers nothing")
        (is (eq :observe (nck:handle-candidate host (said-in-thread host "<@999> and you?" :message "22"))) "and its ask wakes no one")
        (is (= 1 (length steered)))
        (nck:handle-candidate host (said-in-thread host "elektra, one more" :user "u1" :bot nil
                                                   :message "23" :name "alice"))
        (is (not (nck::talk-closed-p host "42")) "a person's line opens a new talk")
        (let ((before (length steered)))
          (loop for message from 24 below 38
                do (nck:handle-candidate host (said-in-thread host "and the TTL" :message (princ-to-string message))))
          (is (= 14 (- (length steered) before)) "past a dozen, a peer's line still steers the running turn"))
        (is (search "which wakes it" (nck::ask-prompt (second (first steered)))) "as a line its answer replies to")
        (setf (nck::lane-heard-line lane) nil)))))

(deftest channel-team-a-post-keeps-the-slot-its-poster-holds ()
  ;; A post reads as its tag, slot, score and words; it is kept once, and
  ;; only in the slot its poster holds.
  (is (equal '("FINDING" 2 "4/5" "narrowed it to the cache")
             (multiple-value-list (nck::parse-board-line "**FINDING** s2 · score 4/5 · narrowed it to the cache"))))
  (is (equal '("CLAIM" 3 nil "the parser")
             (multiple-value-list (nck::parse-board-line "**CLAIM** s3 · the parser"))))
  (is (null (nck::parse-board-line "FINDING s2: nope")))
  (is (= 4/5 (nck::score-value "4/5")))
  (is (= -12.5d0 (nck::score-value "-12.5")))
  (is (null (nck::score-value "fast")))
  (is (equal "7/9" (nck::scorer-score (format nil "running~%score: 7/9~%ok 3 4"))) "a score line wins")
  (is (equal "12" (nck::scorer-score (format nil "a 3~%took 12~%~%"))) "else the last number")
  (with-team-thread (host "42" :peers '(("777" . "elektra") ("888" . "hindsight")))
    (is (nck::note-board-post host (said-in-thread host "**FINDING** s2 · score 3/5 · a")))
    (let ((again (said-in-thread host "**FINDING** s2 · score 3/5 · a"))
          (impostor (said-in-thread host "**SOLVED** s3 · score 5/5 · me" :message "12")))
      (is (null (nck::note-board-post host again)) "the same message is kept once")
      (is (null (nck::note-board-post host impostor)) "a post in another bot's slot is not kept"))
    (is (= 1 (length (nck::board-posts host "42"))))))

(deftest channel-team-the-best-measured-result-writes-the-one-answer ()
  ;; A bot a peer outscored answers silently; the leader answers; equals go
  ;; to whoever reached the score first; a board nobody measured on folds
  ;; nothing.
  (with-team-thread (host "42" :peers '(("777" . "elektra")))
    (let ((lane (running-thread-lane host)))
      (is (equal "mine" (nck::board-answer-text host lane "mine")) "nothing measured: each answers")
      (flet ((post (user slot score message at)
               (nck::record-board-post host "42" (list :kind "FINDING" :slot slot :user user :score score
                                                      :value (nck::score-value score) :message message :at at))))
        (push (list :score "5/5" :value 1 :at 1) (gethash (cons "test" "42") nck::*board-scores*))
        (is (equal "mine" (nck::board-answer-text host lane "mine")) "a verify nobody saw posted ranks nothing")
        (post "777" 2 "4/5" "200" 10)
        (is (nck::silent-answer-p (nck::board-answer-text host lane "mine")) "a peer's posted score leads: silent")
        (post "999" 1 "4/5" "300" 5)
        (let ((tied (nck::board-answer-text host lane "mine")))
          (is (nck::silent-answer-p tied) "equals go to the post that came first in the thread, whenever this kit heard it"))
        (post "999" 1 "5/5" "400" 20)
        (is (equal "mine" (nck::board-answer-text host lane "mine")) "this bot's post leads: it answers")))))

(deftest channel-team-the-brief-names-the-slots-and-the-verbs ()
  ;; A lane in a team's thread is told its slot, its peers' and the verbs.
  (with-team-thread (host "42" :peers '(("777" . "elektra") ("888" . "hindsight")))
    (let ((where (nck::peers-where host (said-in-thread host "go" :user "u1" :bot nil) "Where you are.")))
      (is (search "You are s1; @elektra is s2, @hindsight is s3." where))
      (is (search "(nck:board-verify \"42\"" where))
      (is (search "(nck:board-post \"42\" :claim" where))
      (is (search "no single measurable task" where))
      (is (search "is talked through here instead" where) "a question is a conversation, not three answers")
      (is (search "goes up as a reply to it" where))
      (is (search "goes up as a reply to it" (nck::peers-paragraph '(("777" . "elektra")))) "peers without a board talk the same way")
      (is (search "or reply to its message: either wakes it" where) "a reply wakes the peer it answers")
      (is (search "replies to a message of yours, is an ask and" (nck::peers-paragraph '(("777" . "elektra")))))
      (is (search "The talk ends in one final answer for the person" where) "a talk has an end")
      (is (search "The talk ends in one final answer for the person" (nck::peers-paragraph '(("777" . "elektra"))))))))

(deftest channel-team-verify-is-the-only-source-of-a-score ()
  ;; The scorer's last line is the score: kept as this bot's, put on its card,
  ;; and carried by its next scored post -- which no bot can type itself.
  (with-team-thread (host "42" :peers '(("777" . "elektra")))
    (let ((lane (running-thread-lane host))
          (nck::*hosts* (list host)))
      (is (signals-error error (nck:board-post "42" :finding "fixed it" :platform "test")) "no verify, no scored post")
      (is-present (run (nck:board-verify "42" :command "printf 'ok 1\\nscore: 3/4\\n'" :platform "test")) "the scorer ran"
        (is (equal "3/4" (getf run :score)))
        (is (getf run :improved))
        (is (not (getf run :solved))))
      (is (search "score 3/4" (nck::board-card-line (nck:lane-session-id lane))) "the card shows it")
      (is (signals-error error (nck:board-post "42" :solved "done" :platform "test")) "SOLVED takes the target")
      (is (getf (nck:board-verify "42" :command "echo 4/4" :platform "test") :solved))
      (is-present (post (nck:board-post "42" :finding "the cache key missed the profile" :platform "test")) "the finding posted"
        (is (getf post :posted))
        (is (equal "**FINDING** s1 · score 4/4 · the cache key missed the profile" (getf post :line))))
      (is (equal "4/4" (getf (first (nck::board-posts host "42")) :score)) "and the board keeps it"))))

(deftest channel-team-promote-moves-the-best-branch-by-compare-and-swap ()
  ;; The best commit moves only for a better verified score.
  (with-temp-workspace (repo)
    (flet ((git (&rest args)
             (uiop:run-program (list* "git" "-C" (namestring repo) "-c" "user.name=t" "-c" "user.email=t@t" args)
                               :output :string)))
      (git "init" "-q")
      (git "commit" "-q" "--allow-empty" "-m" "one")
      (with-team-thread (host "42" :peers '(("777" . "elektra")))
        (let ((nck::*hosts* (list host)))
          (running-thread-lane host)
          (push (list :score "3/4" :value 3/4 :at 1) (gethash (cons "test" "42") nck::*board-scores*))
          (is (getf (nck:board-promote "42" repo "HEAD" :platform "test") :promoted) "the first result promotes")
          (git "commit" "-q" "--allow-empty" "-m" "two")
          (push (list :score "2/4" :value 1/2 :at 2) (gethash (cons "test" "42") nck::*board-scores*))
          (is (not (getf (nck:board-promote "42" repo "HEAD" :platform "test") :promoted)) "a worse one does not")
          (push (list :score "4/4" :value 1 :at 3) (gethash (cons "test" "42") nck::*board-scores*))
          (is (getf (nck:board-promote "42" repo "HEAD" :platform "test") :promoted) "a better one does")
          (is (equal (nlk:trimmed (git "rev-parse" "HEAD")) (nlk:trimmed (git "rev-parse" "board/42")))))))))
