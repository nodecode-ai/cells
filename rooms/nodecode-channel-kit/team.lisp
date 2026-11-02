;;;; team.lisp --- bots a person names together work one task, the thread their board.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A person who names several bots in one message asks each of them for the
;;;; whole task, in one thread (the peers half of host.lisp). This half makes
;;;; that thread the team's board: the team cell's protocol (src/cells/team),
;;;; carried from a shared folder to the room the person reads.
;;;;
;;;;   - Each bot attacks the whole task with an approach it CLAIMs, one no
;;;;     peer has.
;;;;   - One scorer per task, every bot on it runs it, and BOARD-VERIFY is the
;;;;     only source of a score: it runs the scorer, keeps the score under the
;;;;     scorer's key, and puts it on the bot's card.
;;;;   - A bot posts only measured progress -- FINDING, FAIL, ADOPT, SOLVED --
;;;;     and each post reaches every peer's running turn (HEAR-IN-RUNNING-TURN):
;;;;     a post that names no one is the board, a mention is an ask.
;;;;   - A peer's post is a claim until re-run: ADOPT carries the adopter's own
;;;;     score, never the peer's.
;;;;   - Each scorer's best verified result writes its task's one answer
;;;;     (BOARD-ANSWER-TEXT), a result ranking only against its own scorer's;
;;;;     BOARD-PROMOTE moves each scorer's best commit with a compare-and-swap.
;;;;
;;;; A slot is a bot's place in the order the person named the bots in: the
;;;; message's own text, which every kit reads the same, so no two kits race
;;;; for a number. The slots live in the thread room's state, beside its peers.

(in-package #:nodecode-channel-kit)

;;; --- slots ---------------------------------------------------------------------

(defparameter +thread-slots-key+ "thread-slots"
  "The state key a thread's room keeps its slots under: the bot ids in the
order the person named them.")

(defvar *thread-slots* (make-hash-table :test #'equal :synchronized t)
  "(platform id . thread id) -> the thread's slots, (ID ...) first named
first, or :NONE for a thread read and found to have none.")

(defun host-self-id (host &aux (policy (host-policy host)))
  "The id HOST's bot speaks as on its platform, or NIL."
  (and (typep policy 'inbound-policy) (inbound-policy-self-id policy)))

(defun mention-position (text id)
  "Where TEXT first mentions ID -- <@id> or <@!id> -- or NIL."
  (let ((plain (search (format nil "<@~a>" id) text))
        (nick (search (format nil "<@!~a>" id) text)))
    (if (and plain nick) (min plain nick) (or plain nick))))

(defun id< (a b)
  "Whether platform id A sorts before B: the shorter first, then by its
characters -- a Discord snowflake by age."
  (if (= (length a) (length b)) (and (string< a b) t) (< (length a) (length b))))

(defun mention-order (text ids)
  "IDS in the order TEXT first mentions each; the ones it never mentions --
reached by a role it pings -- last, by id (ID<)."
  ;; By id, never in the order given: each bot gives its own id first, and the
  ;; order is every bot's slot on the board, so all must reach the same one.
  (let ((placed (loop for id in ids collect (cons id (mention-position text id)))))
    (mapcar #'car (stable-sort placed
                               (lambda (a b)
                                 (cond ((and (cdr a) (cdr b)) (< (cdr a) (cdr b)))
                                       ((cdr a) t)
                                       ((cdr b) nil)
                                       (t (id< (car a) (car b)))))))))

(defun read-thread-slots (room)
  "The slots ROOM's state keeps, (ID ...), or NIL."
  (when (and room (nlk:store-open-p) (nlk:session-exists-p room))
    (handler-case
        (nlk:when-let (json (nlk:session-state-get room +thread-slots-key+))
          (let ((slots (nlk:decode-json json)))
            (and (vectorp slots) (remove-if-not #'stringp (coerce slots 'list)))))
      (error (condition)
        (warn "thread slots of ~a could not be read: ~a" room condition)
        nil))))

(defun thread-slots (host thread-id &optional room)
  "THREAD-ID's slots on HOST's platform, (ID ...): the table's, else what its
room ROOM keeps, read once."
  (when thread-id
    (let* ((key (peer-key host thread-id))
           (known (gethash key *thread-slots*)))
      (cond ((eq known :none) nil)
            (known known)
            ((null room) nil)
            (t (let ((read (read-thread-slots room)))
                 (setf (gethash key *thread-slots*) (or read :none))
                 read))))))

(defun note-thread-slots (host thread-id candidate &optional room)
  "The bots CANDIDATE's message names -- this one and the others beside it --
as THREAD-ID's slots, in the order its text names them. A thread's slots are
set once: a later ask that names more bots adds them at the end, and never
moves a bot from the place it holds. => the slots, or NIL."
  (let* ((self (host-self-id host))
         (others (mapcar #'car (candidate-mentioned-bots candidate)))
         (named (mention-order (or (gethash "text" candidate) "")
                               (remove-duplicates (remove nil (cons self others))
                                                  :test #'equal :from-end t))))
    ;; An ask one bot owns (OWNER.LISP) -- a line naming no bot by name in a
    ;; thread several bots share -- is no team's: its thread is no board. A
    ;; role's ping is a team's, as a line naming several bots by name is.
    (when (and thread-id self others (not (candidate-bot-p candidate))
               (not (ask-rank host candidate)))
      (let* ((had (thread-slots host thread-id room))
             (slots (append had (remove-if (lambda (id) (member id had :test #'equal)) named))))
        (setf (gethash (peer-key host thread-id) *thread-slots*) slots)))))

(defun save-thread-slots (host thread-id room)
  "Write THREAD-ID's slots into its room's state, ROOM, so a later image
knows them."
  (let ((slots (thread-slots host thread-id)))
    (when (and slots room (nlk:store-open-p) (nlk:session-exists-p room))
      (handler-case
          (nlk:session-state-put room +thread-slots-key+
                                 (nlk:encode-json-object (coerce slots 'vector)))
        (error (condition)
          (warn "thread slots of ~a could not be kept: ~a" room condition))))))

(defun slot-of (host thread-id id &optional room)
  "ID's slot on THREAD-ID's board, 1 for the first bot named, or NIL."
  (let ((at (position id (thread-slots host thread-id room) :test #'equal)))
    (and at (1+ at))))

;;; --- posts ---------------------------------------------------------------------

(defparameter +board-kinds+ '("CLAIM" "SCORER" "FINDING" "FAIL" "ADOPT" "SOLVED")
  "The kinds of post a board carries, as their tags read.")

(defparameter +scored-kinds+ '("FINDING" "ADOPT" "SOLVED")
  "The kinds that carry the poster's own newest verified score.")

(defparameter +board-post-cap+ 1500
  "Characters one post's words keep: a post is a finding, never a report.")

(defparameter +board-line-scanner+
  (ppcre:create-scanner
   "^\\s*(?:-#\\s*)?\\*\\*(CLAIM|SCORER|FINDING|FAIL|ADOPT|SOLVED)\\*\\*\\s+s(\\d+)(?:\\s+·\\s+score\\s+(\\S+))?(?:\\s+·\\s+scorer\\s+([0-9a-f]{6}))?(?:\\s+·\\s*|\\s*$)(.*)$"
   :single-line-mode t)
  "One board post: **KIND** sN [· score S] [· scorer K] · words.")

(defparameter +scorer-key-digits+ 6
  "Hex digits of a scorer's key: the name every kit gives one scorer command.")

(defvar *team-posts* (make-hash-table :test #'equal :synchronized t)
  "(platform id . thread id) -> the posts on the thread's board, newest
first: (:KIND :SLOT :USER :SCORE :VALUE :TEXT :MESSAGE :AT).")

(defvar *board-scores* (make-hash-table :test #'equal :synchronized t)
  "(platform id . thread id) -> what this bot's own verifies there measured,
newest first: (:SCORE :VALUE :AT :IMPROVED :SOLVED).")

(defvar *board-card-lines* (make-hash-table :test #'equal :synchronized t)
  "A lane's session id -> the score row its card shows while it works.")

(defun board-kind (kind)
  "KIND -- :finding, \"finding\" -- as its tag, FINDING; an error for a kind
no board carries."
  (let ((tag (string-upcase (string kind))))
    (or (find tag +board-kinds+ :test #'string=)
        (error "a board carries ~{~(~a~)~^, ~} posts, not ~s" +board-kinds+ kind))))

(defun board-line (kind slot text &key score scorer)
  "The words one post shows: **KIND** sN, its score when it carries one, the
key of the scorer that measured it or that it posts, then TEXT."
  (format nil "**~a** s~d~@[ · score ~a~]~@[ · scorer ~a~] · ~a" kind slot score scorer
          (nlk:clip (nlk:trimmed text) +board-post-cap+ :ellipsis "…")))

(defun parse-board-line (text)
  "(values KIND SLOT SCORE WORDS SCORER) of TEXT when it is a board post, else
NIL; SCORER is the key of the scorer the post names, NIL when it names none."
  (when (stringp text)
    (multiple-value-bind (whole parts) (ppcre:scan-to-strings +board-line-scanner+ text)
      (when whole
        (values (aref parts 0)
                (parse-integer (aref parts 1))
                (aref parts 2)
                (nlk:trimmed (or (aref parts 4) ""))
                (aref parts 3))))))

(defun scorer-command (words)
  "The command a SCORER post's WORDS name: their first span in backticks, the
backticks off, else all of them."
  (let ((code (nth-value 1 (ppcre:scan-to-strings "`([^`]+)`" words))))
    (nlk:trimmed (if code (aref code 0) words))))

(defun scorer-key (command)
  "COMMAND's key on a board: the first hex digits of its SHA-256, the same in
every kit, so a result names the scorer it was measured on."
  (nlk:short-digest (nlk:trimmed command) +scorer-key-digits+))

(defun read-decimal (text)
  "TEXT, digits with an optional sign and fraction, as the number it says."
  (let ((*read-default-float-format* 'double-float)
        (*read-eval* nil))
    (let ((value (read-from-string text)))
      (and (realp value) value))))

(defun score-value (score)
  "SCORE's number, higher is better: N/M reads as their ratio, a decimal as
itself; NIL for anything else."
  (when (stringp score)
    (multiple-value-bind (whole parts)
        (ppcre:scan-to-strings "^(-?[0-9]+(?:\\.[0-9]+)?)/([0-9]+(?:\\.[0-9]+)?)$" score)
      (cond (whole (let ((n (read-decimal (aref parts 0)))
                         (d (read-decimal (aref parts 1))))
                     (and n d (plusp d) (/ n d))))
            ((ppcre:scan "^-?[0-9]+(?:\\.[0-9]+)?$" score) (read-decimal score))))))

(defun full-score-p (score)
  "Whether SCORE is N/M with N = M: every check passing."
  (and (stringp score) (find #\/ score) (eql 1 (score-value score))))

(defun board-key (host thread-id)
  (peer-key host thread-id))

(defun board-posts (host thread-id)
  "THREAD-ID's board posts, newest first."
  (values (gethash (board-key host thread-id) *team-posts*)))

(defun record-board-post (host thread-id post)
  "Keep POST on THREAD-ID's board, once per message. => POST, or NIL when
the board had it."
  (let ((key (board-key host thread-id))
        (message (getf post :message)))
    (sb-ext:with-locked-hash-table (*team-posts*)
      (unless (and message (find message (gethash key *team-posts*)
                                 :key (lambda (p) (getf p :message)) :test #'equal))
        (push post (gethash key *team-posts*))
        post))))

(defun note-board-post (host candidate)
  "Keep CANDIDATE on its thread's board when it is a post a bot made in the
slot it holds there -- never one claiming another bot's slot. => the post,
or NIL."
  (let ((thread (source-field candidate "thread_id"))
        (user (source-field candidate "user_id"))
        (text (and (hash-table-p candidate) (gethash "text" candidate))))
    (when (and thread user (candidate-bot-p candidate) (stringp text))
      (multiple-value-bind (kind slot score words scorer) (parse-board-line text)
        (when (and kind (eql slot (slot-of host thread user (candidate-room host candidate))))
          (record-board-post host thread
                             (list :kind kind :slot slot :user user
                                   :score score :value (score-value score) :text words
                                   :scorer scorer
                                   :message (source-field candidate "message_id")
                                   :at (now-ms))))))))

(defun board-posts-in-order (host thread-id)
  "THREAD-ID's board posts in the order they came in the thread, oldest first."
  (stable-sort (reverse (board-posts host thread-id)) #'< :key #'post-order))

(defun board-scorer-posts (host thread-id)
  "THREAD-ID's SCORER posts, the first posted first."
  (remove-if-not (lambda (post) (equal "SCORER" (getf post :kind)))
                 (board-posts-in-order host thread-id)))

(defun board-scorer (host thread-id)
  "The command THREAD-ID's first SCORER post names, its backticks off, or NIL."
  (let ((post (first (board-scorer-posts host thread-id))))
    (and post (scorer-command (getf post :text)))))

(defun post-scorer-key (post)
  "The key of the scorer POST is, when it is a SCORER post -- its command's
-- else the key of the scorer it names, or NIL."
  (if (equal "SCORER" (getf post :kind))
      (scorer-key (scorer-command (getf post :text)))
      (getf post :scorer)))

(defun board-default-scorer (host thread-id)
  "The key of THREAD-ID's first scorer: the first SCORER post's, else the
first posted result's that names one, else NIL. A result that names no
scorer -- a kit from before results named theirs posted it -- ranks under it."
  (let* ((posts (board-posts-in-order host thread-id))
         (opener (or (find "SCORER" posts :key (lambda (p) (getf p :kind)) :test #'equal)
                     (find-if (lambda (p) (and (getf p :value) (getf p :scorer))) posts))))
    (and opener (post-scorer-key opener))))

(defun result-scorer (default entry)
  "The key of the scorer ENTRY -- a posted result, or a verify of this bot's
-- ranks under: the one it names, else DEFAULT, the board's first."
  (or (getf entry :scorer) default))

(defun verify-scorer (host thread-id command)
  "The key of the scorer COMMAND runs on THREAD-ID's board: the SCORER post's
whose command it is or wraps -- a cd or a variable around it -- the longest
such, else COMMAND's own."
  (let ((command (nlk:trimmed command))
        (named nil))
    (dolist (post (board-scorer-posts host thread-id))
      (let ((said (scorer-command (getf post :text))))
        (when (and (plusp (length said)) (search said command)
                   (or (null named) (> (length said) (length (car named)))))
          (setf named (cons said post)))))
    (if named (post-scorer-key (cdr named)) (scorer-key command))))

(defun board-scores-under (host thread-id scorer)
  "This bot's verifies on THREAD-ID's board measured on SCORER, a scorer's
key, newest first."
  (let ((default (board-default-scorer host thread-id)))
    (remove-if-not (lambda (entry) (equal scorer (result-scorer default entry)))
                   (board-scores host thread-id))))

(defun board-scores (host thread-id)
  "What this bot's verifies on THREAD-ID's board measured, newest first."
  (values (gethash (board-key host thread-id) *board-scores*)))

(defun best-entry (entries)
  "The entry of ENTRIES -- plists with :VALUE and :AT -- whose value is
highest, the earliest of equals; NIL when none carries a value."
  (let ((best nil))
    (dolist (entry entries best)
      (let ((value (getf entry :value)))
        (when (and value
                   (or (null best)
                       (> value (getf best :value))
                       (and (= value (getf best :value))
                            (< (getf entry :at) (getf best :at)))))
          (setf best entry))))))

(defun post-order (post)
  "Where POST sits in its thread: the platform's message id when it is a
number -- Discord's grow with time, the same for every kit -- else the
moment this kit kept it."
  (let ((message (getf post :message)))
    (or (and (stringp message) (plusp (length message)) (every #'digit-char-p message)
             (parse-integer message))
        (getf post :at))))

(defun board-races (host thread-id)
  "The keys of the scorers THREAD-ID's posted results were measured on, the
first result's first: one race each. NIL is the one race of results that
name no scorer on a board that has none."
  (let ((default (board-default-scorer host thread-id))
        (races '()))
    (dolist (post (board-posts-in-order host thread-id) (nreverse races))
      (when (getf post :value)
        (pushnew (result-scorer default post) races :test #'equal)))))

(defun board-standing (host thread-id &optional (scorer (board-default-scorer host thread-id)))
  "Each slot's best posted result on THREAD-ID's board measured on SCORER --
a scorer's key, the board's first by default -- best first: ((:SLOT :USER
:SCORE :VALUE :AT :SCORER) ...). A result counts once it is posted -- the
scores the posts carry, this bot's own included -- and ranks only against
results from its own scorer, so every kit ranks the same thread alike: by
value, and equals by which post came first."
  (let* ((default (board-default-scorer host thread-id))
         (rows (loop for user in (thread-slots host thread-id)
                     for best = (best-entry
                                 (loop for post in (board-posts host thread-id)
                                       when (and (equal user (getf post :user)) (getf post :value)
                                                 (equal scorer (result-scorer default post)))
                                         collect (list* :at (post-order post) post)))
                     when best
                       collect (list :slot (getf best :slot) :user user
                                     :score (getf best :score) :value (getf best :value)
                                     :at (getf best :at) :scorer scorer))))
    (sort rows (lambda (a b)
                 (or (> (getf a :value) (getf b :value))
                     (and (= (getf a :value) (getf b :value))
                          (< (getf a :at) (getf b :at))))))))

(defun board-leader (host thread-id &optional (scorer (board-default-scorer host thread-id)))
  "The best posted result on THREAD-ID's board measured on SCORER, the
board's first scorer by default (BOARD-STANDING), or NIL while nobody posted
a score there."
  (first (board-standing host thread-id scorer)))

(defun board-leaders (host thread-id)
  "The best posted result under each scorer THREAD-ID's results were measured
on, the first scorer's first: each task's leader."
  (loop for scorer in (board-races host thread-id)
        for leader = (board-leader host thread-id scorer)
        when leader collect leader))

;;; --- hearing the thread while a turn runs -----------------------------------------

(defun peer-who (host candidate)
  "Who said CANDIDATE, a line heard in a thread, as a lane there is told: a
peer's slot, then its name."
  (let* ((thread (source-field candidate "thread_id"))
         (user (source-field candidate "user_id"))
         (slot (and (candidate-bot-p candidate) (slot-of host thread user))))
    (format nil "~@[s~d, ~]~a" slot (or (source-field candidate "user_name") user))))

(defun heard-frame (host candidate)
  "What a line heard while a turn runs is to that turn: a post on the board,
a peer's word, or a person's line said to someone else -- context to weigh
while it keeps working, never an ask of its own."
  (let ((who (peer-who host candidate)))
    (multiple-value-bind (kind) (and (candidate-bot-p candidate)
                                     (parse-board-line (gethash "text" candidate)))
      (cond
        ((equal kind "SOLVED")
         (format nil "[A SOLVED post from your peer ~a reached you while you work. ~
Re-run it once with the board's scorer (nck:board-verify). If it reproduces, post ADOPT and end ~
your turn with your answer; if it does not, post FAIL with what you saw and keep going.]" who))
        (kind
         (format nil "[A ~a post from your peer ~a reached you on the thread's board while you ~
work: a claim until your own verify reproduces it. Weigh it and keep working.]" kind who))
        ((peer-final-p candidate)
         (format nil "[Your peer ~a posted the thread's final answer while you work: the talk is ~
closed, and nothing posted in it now wakes anyone. End your turn with exactly NO_REPLY, unless ~
the final answer is wrong or misses something you found: then say only that, in a few lines.]" who))
        ((candidate-bot-p candidate)
         (format nil "[Your peer ~a posted this in the thread while you work. Your answer goes up as ~
a reply to it, which wakes it: build on it, with only what you add, correct or dispute and never ~
a second copy of what it said, and answer exactly NO_REPLY when you have nothing to add.]" who))
        (t
         (format nil "[Said in the thread while you work, to someone else: context, not an ask ~
of you. Keep working unless it changes your task.]"))))))

(defun heard-ask (host candidate lane)
  "The ask that carries CANDIDATE, framed as a line heard (HEARD-FRAME), into
LANE's running turn. It binds no address: a reply to a peer's message is the
peer's."
  (let* ((text (or (gethash "text" candidate) ""))
         (framed (nlk:copy-json-object candidate "text"
                                       (format nil "~a~%~a" (heard-frame host candidate) text)))
         (ask (build-ask host framed (lane-session-id lane) t)))
    (setf (ask-key ask) (format nil "heard-~a" (or (source-field candidate "message_id")
                                                  (nlk:short-digest text 8)))
          (ask-message-id ask) nil)
    ask))

(defun peer-final-p (candidate)
  "Whether CANDIDATE is a bot's line that opens as its talk's final answer."
  (and (candidate-bot-p candidate) (final-answer-line-p (gethash "text" candidate))))

(defun heard-after-final-p (host candidate)
  "Whether CANDIDATE, a bot's line, came after the final answer that closed
its thread's talk -- as the final answer itself did not."
  (let* ((thread (source-field candidate "thread_id"))
         (final (and thread (gethash (peer-key host thread) *talk-finals*))))
    (and final (candidate-bot-p candidate)
         (not (equal final (source-field candidate "message_id"))))))

(defun woken-frame (host candidate &aux (who (peer-who host candidate)))
  "What a peer's ask is to the turn it wakes: a line to answer for as long as
the talk needs, or to close the talk on with its final answer."
  (format nil "[Your peer ~a wakes you with the line below. When it needs your answer -- a ~
question put to you, a claim you dispute or can correct, a part of the work it hands you -- give ~
only that: it goes up as a reply that wakes ~a, and the talk goes on for as long as that takes. ~
When it needs nothing more from you -- you agree, and nothing between you is open -- close the ~
talk: write the final answer for the person who asked -- the thread's settled result whole: what ~
holds, what was corrected, what is still open -- beginning with the line FINAL ANSWER. It goes up ~
as the thread's final answer and wakes no one. A wake is never answered with NO_REPLY: the talk ~
ends in its final answer.]" who who))

(defun woken-ask (host candidate)
  "CANDIDATE, a peer's ask PEER-RUN numbered, framed as the turn it wakes
(WOKEN-FRAME); any other candidate as it is."
  (if (integerp (gethash "peer_turn" (candidate-source candidate)))
      (nlk:copy-json-object candidate "text"
                            (format nil "~a~%~a" (woken-frame host candidate)
                                    (or (gethash "text" candidate) "")))
      candidate))

(defun final-answer-ask (host candidate lane &aux (who (peer-who host candidate)))
  "The ask that has LANE write its talk's final answer once it met CANDIDATE,
the peer's line that woke it, with silence (ASK-FOR-FINAL-ANSWER). It binds
no address and carries no message."
  (let* ((framed (nlk:copy-json-object
                  candidate "text"
                  (format nil "[Your peer ~a woke you with a line you answered NO_REPLY: nothing ~
between you is open, so the talk ends now, in its final answer. Write it for the person who ~
asked -- the thread's settled result whole: what holds, what was corrected, what is still open ~
-- beginning with the line FINAL ANSWER. It goes up as the thread's final answer and wakes no ~
one.]" who)))
         (ask (build-ask host framed (lane-session-id lane) t)))
    (setf (ask-key ask) (format nil "final-~a" (or (source-field candidate "message_id")
                                                   (nlk:short-digest (gethash "text" framed) 8)))
          (ask-message-id ask) nil)
    ask))

(defparameter +steering-kinds+ '("CLAIM" "SCORER" "FINDING" "FAIL" "SOLVED")
  "The board posts that can change what a running peer does next: an
approach taken, the scorer, a measured gain, an idea ruled out, the task
solved. An ADOPT waits for its next ask.")

(defun steering-line-p (host candidate)
  "Whether CANDIDATE is worth cutting a running turn short for: a person's
line, a peer's board post of one of +STEERING-KINDS+, or a peer's untagged
line -- its answer, or what it says to the thread -- which the turn builds on
and answers as a reply to."
  (declare (ignore host))
  (or (not (candidate-bot-p candidate))
      (let ((kind (parse-board-line (gethash "text" candidate))))
        (if kind
            (and (member kind +steering-kinds+ :test #'equal) t)
            t))))

(defun hear-in-running-turn (host candidate)
  "Steer this bot's running turn in CANDIDATE's thread with CANDIDATE -- a
peer's line, or a person's line said to someone else -- when the thread has
peers and the turn is running: a team hears every line of its thread while
it works. => T when it steered."
  ;; A line that is no ask of this bot reaches it this way and no other: a
  ;; settled lane keeps it as the context of its next ask (OBSERVE-CANDIDATE).
  (let* ((thread (source-field candidate "thread_id"))
         (lane (and thread
                    (thread-peers host thread (candidate-room host candidate))
                    (thread-lane host candidate))))
    ;; A talk its final answer closed steers nothing more; the final answer
    ;; itself reaches the running turn once.
    (when (and lane (lane-active-turn-id lane)
               (steering-line-p host candidate)
               (not (heard-after-final-p host candidate)))
      ;; A peer's untagged line is what the next answer builds on: it goes up
      ;; as a reply to it (DELIVER-ANSWER). A board post is a status line, and
      ;; a person's line said to someone else is theirs.
      (nlk:when-let (id (and (candidate-bot-p candidate)
                             (null (parse-board-line (gethash "text" candidate)))
                             (source-field candidate "message_id")))
        (setf (lane-heard-line lane) (cons id (lane-active-turn-id lane))))
      (continue-lane host lane (heard-ask host candidate lane))
      t)))

;;; --- the brief -------------------------------------------------------------------

(defun board-paragraph (host thread-id peers &optional room)
  "What a team's board is to a lane in its thread: who holds which slot, the
rules the team cell keeps, and the verbs that post, measure and read."
  (let* ((self (host-self-id host))
         (slots (thread-slots host thread-id room))
         (others (loop for id in slots
                       for n from 1
                       unless (equal id self)
                         collect (format nil "@~a is s~d"
                                         (or (cdr (assoc id peers :test #'equal)) id) n))))
    (format nil "This thread is a team's board: the person named ~d bots together. You are s~d; ~
~{~a~^, ~}. Each of you takes the whole task at once with an approach of your own, and you ~
talk only through tagged posts here: every post a peer makes reaches your running turn as a ~
note. When the ask is one task with a measurable outcome, work it this way. ~
1. Post a CLAIM naming your approach, one no peer has claimed. ~
2. One scorer per task: the ask's, else the first failing repro any of you posts as SCORER, ~
the command in backticks; an ask of several tasks gets one SCORER each. A score comes only ~
from (nck:board-verify ~s :command \"...\" :directory \"<your working copy>\"), which runs ~
your task's scorer and puts your score on your card. ~
The scorer prints the score on its last line, higher is better: N/M, or a number (negate a ~
time or a size). ~
3. Post only measured progress: FINDING when your score improved -- what changed and why it ~
works -- and FAIL for an idea ruled out, so no peer retries it. No chatter, and no ~
@mentions: a mention asks a peer for a turn. ~
4. A peer's post is a claim until your own verify reproduces it: ADOPT only then, and keep a ~
variation of your own. ~
5. When two of you collide on one approach or one file, the later poster says how you split ~
it, and moves. ~
6. After 3 verifies without an improvement, switch to a structurally different approach no ~
peer is on, and CLAIM it. ~
7. When your score reaches the target, commit, run (nck:board-promote ~s \"<repo>\" ~
\"<commit>\") and post SOLVED naming the branch. When a peer posts SOLVED, re-run it once: ~
ADOPT and end your turn if it reproduces, FAIL and keep going if not. ~
8. Each scorer's best verified result is posted as its task's answer -- a result ranks only ~
against results from its own scorer, and the kit folds the rest -- so end your turn with ~
your full answer either way. ~
Post with (nck:board-post ~s :claim \"...\") -- or :scorer, :finding, :fail, :adopt, :solved; ~
a scored post carries your newest verify's score itself -- and read the board with ~
(nck:board-read ~s). An ask that is no single measurable task -- a question, a status, an ~
opinion -- skips the board and is talked through here instead: whoever has an answer first ~
posts it whole, a peer's post reaches your running turn, and your answer then goes up as a ~
reply to it -- a reply wakes the peer it answers -- with only what you add, correct or ~
dispute, never a second copy of what it said, or exactly NO_REPLY when you have nothing to ~
add. To ask a peer something, @mention it or reply to its message: either wakes it, and it ~
answers you here. ~a"
            (length slots) (or (slot-of host thread-id self room) 0) others
            thread-id thread-id thread-id thread-id (talk-end-sentences))))

;;; --- the verbs a lane calls ---------------------------------------------------------

(defun board-host (platform)
  (or (find platform *hosts* :key #'host-id :test #'string-equal)
      (error "no ~a channel is running" platform)))

(defun post-on-board (host lane thread-id kind text &key score scorer)
  "Post one KIND line as this bot on THREAD-ID's board from LANE, naming the
key SCORER when it carries one, and keep it there. => (:POSTED :MESSAGE :LINE)."
  (let* ((slot (or (slot-of host thread-id (host-self-id host))
                   (error "this bot holds no slot on the board of ~a" thread-id)))
         (line (board-line kind slot text :score score :scorer scorer)))
    (multiple-value-bind (posted ids failure)
        (post-message host (lane-target lane) line :ping nil
                      :what (format nil "~(~a~) post on ~a's board" kind thread-id))
      (when posted
        (note-thread-spoken thread-id)
        (record-board-post host thread-id
                           (list :kind kind :slot slot :user (host-self-id host)
                                 :score score :value (score-value score)
                                 :text (nth-value 3 (parse-board-line line))
                                 :scorer scorer
                                 :message (first ids) :at (now-ms))))
      (list :posted (and posted t) :message (first ids) :line line
            :error (and (not posted) (princ-to-string failure))))))

(defun board-post (thread kind text &key (platform "discord"))
  "Post one KIND line -- :claim, :scorer, :finding, :fail, :adopt, :solved --
as this bot on THREAD's board, from the turn running there. A FINDING, ADOPT
or SOLVED carries this bot's newest verified score, never a typed one, and the
key of the scorer that measured it; a SCORER, its own command's key. SOLVED
only after a verify reached the target."
  ;; Stamped an act first: it posts, so the snippet calling it is never
  ;; watched and run again (NLE:RECEIPT).
  (nle:receipt "calls" (list :verb "board-post"))
  (multiple-value-bind (host lane) (running-answer "board-post" platform nil thread nil)
    (let* ((kind (board-kind kind))
           (newest (first (board-scores host thread)))
           (scored (member kind +scored-kinds+ :test #'string=))
           (score (when scored
                    (or (getf newest :score)
                        (error "a ~a post carries a measured score: run (nck:board-verify ~s) first"
                               kind thread)))))
      (when (and (string= kind "SOLVED") (not (getf newest :solved)))
        (error "SOLVED takes a verify that reached the target; the newest measured ~a" score))
      (post-on-board host lane thread kind text
                     :score score
                     :scorer (cond (scored (getf newest :scorer))
                                   ((string= kind "SCORER") (scorer-key (scorer-command text))))))))

(defparameter +scorer-timeout-seconds+ 900
  "How long one scorer run may take before BOARD-VERIFY calls it failed.")

(defun run-scorer (command directory timeout)
  "(values OUTPUT EXIT-CODE) of COMMAND run by bash in DIRECTORY, cut off
after TIMEOUT seconds."
  (multiple-value-bind (output error-output code)
      (uiop:run-program (list "timeout" (princ-to-string timeout) "bash" "-c" command)
                        :directory (and directory (uiop:ensure-directory-pathname directory))
                        :output :string :error-output :output :ignore-error-status t)
    (declare (ignore error-output))
    (values (or output "") code)))

(defun scorer-cut-p (code)
  "Whether exit CODE says the scorer never reached its last line: TIMEOUT's
124 when the cut fell, or 128+N when a signal ended it."
  (and (integerp code) (or (= code 124) (> code 128))))

(defun scorer-score (output)
  "The score OUTPUT ends on: the last `score: S' (or `score=S') it prints,
`score' a word of its own, else the last N/M or number on its last line that
holds one; NIL when it prints none."
  ;; The colon or equals sign is required and `score' must start a word: a
  ;; progress line naming a path like /scratch/score-141/7 is not a score.
  (let ((said nil))
    (ppcre:do-register-groups (score) ("(?i)\\bscore\\s*[:=]\\s*(-?[0-9]+(?:\\.[0-9]+)?(?:/[0-9]+(?:\\.[0-9]+)?)?)" output)
      (setf said score))
    (or said
        (loop for line in (reverse (uiop:split-string output :separator '(#\Newline)))
              for found = (let ((last nil))
                            (ppcre:do-matches-as-strings
                                (match "-?[0-9]+(?:\\.[0-9]+)?(?:/[0-9]+(?:\\.[0-9]+)?)?" line)
                              (setf last match))
                            last)
              when found return found))))

(defun card-score-line (host thread-id)
  "The row a lane's card shows for this bot on THREAD-ID's board: its newest
score, its best on the same scorer, and that scorer's leader when that is a
peer."
  (let* ((newest (first (board-scores host thread-id)))
         (scorer (result-scorer (board-default-scorer host thread-id) newest))
         (scores (board-scores-under host thread-id scorer))
         (best (best-entry scores))
         (leader (board-leader host thread-id scorer)))
    (format nil "score ~a~@[ · best ~a~]~@[ · board s~d ~a~]"
            (getf newest :score)
            (and best (not (eq best (first scores))) (getf best :score))
            (and leader (not (equal (getf leader :user) (host-self-id host))) (getf leader :slot))
            (and leader (not (equal (getf leader :user) (host-self-id host))) (getf leader :score)))))

(defun board-card-line (session-id)
  "The score row SESSION-ID's card carries, or NIL."
  (and session-id (values (gethash session-id *board-card-lines*))))

(defun board-verify (thread &key command directory target (timeout +scorer-timeout-seconds+)
                              (platform "discord"))
  "Run THREAD's scorer -- COMMAND, else the board's first SCORER post -- in
DIRECTORY, keep the score it ends on as this bot's under the key of the
scorer it ran, and put it on the card: the only source of a score on the
board. A score improves, plateaus and leads only among scores from its own
scorer. TARGET is the value that solves the task; N/M solves at M/M without
one. => (:SCORE :IMPROVED :BEST :SOLVED :PLATEAU :LEADER :SCORER :EXIT :OUTPUT)."
  ;; Stamped an act: it runs a command and changes what the card says.
  (nle:receipt "calls" (list :verb "board-verify"))
  (multiple-value-bind (host lane) (running-answer "board-verify" platform nil thread nil)
    (let* ((command (or command (board-scorer host thread)
                        (error "no scorer: pass :command, or post the command as SCORER first")))
           (scorer (verify-scorer host thread command)))
      (multiple-value-bind (output code) (run-scorer command directory timeout)
        ;; A cut run printed only what came before the cut: an early part's
        ;; N/M is not the score the whole run ends on, so it scores nothing.
        (when (scorer-cut-p code)
          (error "the scorer was cut off (exit ~a) before its last line, so it scores nothing:~%~a"
                 code (text-end output 1200)))
        (let* ((score (or (scorer-score output)
                          (error "the scorer printed no score (exit ~a):~%~a" code
                                 (text-end output 1200))))
               (value (score-value score))
               (before (best-entry (board-scores-under host thread scorer)))
               (improved (or (null before) (> value (getf before :value))))
               (solved (if target (>= value target) (full-score-p score)))
               (entry (list :score score :value value :at (now-ms)
                            :improved improved :solved solved :scorer scorer)))
          (sb-ext:with-locked-hash-table (*board-scores*)
            (push entry (gethash (board-key host thread) *board-scores*)))
          (setf (gethash (lane-session-id lane) *board-card-lines*)
                (card-score-line host thread))
          (let* ((history (board-scores-under host thread scorer))
                 (flat (and (>= (length history) 4)
                            (notany (lambda (e) (getf e :improved)) (subseq history 0 3))))
                 (leader (board-leader host thread scorer)))
            (list :score score :improved improved
                  :best (getf (best-entry history) :score)
                  :solved solved
                  :plateau (and flat "3 verifies without an improvement: switch to a structurally different approach no peer is on, and CLAIM it")
                  :leader (and leader (format nil "s~d ~a" (getf leader :slot) (getf leader :score)))
                  :scorer scorer
                  :exit code
                  :output (text-end output 1500))))))))

(defun board-read (thread &key (platform "discord"))
  "THREAD's board as this bot's kit holds it: the slots, the first scorer and
every SCORER post with its key, every post oldest first, each slot's best
measured result under each scorer, and this bot's own scores -- each named
by its scorer's key once the board ranks more than one."
  (let* ((host (board-host platform))
         (self (host-self-id host))
         (peers (thread-peers host thread))
         (default (board-default-scorer host thread))
         (races (board-races host thread))
         (several (rest races)))
    (list :you (slot-of host thread self)
          :slots (loop for id in (thread-slots host thread)
                       for n from 1
                       collect (list n (if (equal id self) "you" (or (cdr (assoc id peers :test #'equal)) id))))
          :scorer (board-scorer host thread)
          :scorers (loop for post in (board-scorer-posts host thread)
                         collect (format nil "~a s~d `~a`" (post-scorer-key post) (getf post :slot)
                                         (scorer-command (getf post :text))))
          :posts (loop for post in (reverse (board-posts host thread))
                       collect (format nil "~a s~d~@[ · score ~a~]~@[ · scorer ~a~] · ~a"
                                       (getf post :kind) (getf post :slot) (getf post :score)
                                       (and (getf post :value) (getf post :scorer)) (getf post :text)))
          :standing (loop for scorer in races
                          append (loop for row in (board-standing host thread scorer)
                                       collect (format nil "s~d ~a~@[ · scorer ~a~]" (getf row :slot)
                                                       (getf row :score) (and several scorer))))
          :yours (mapcar (lambda (e) (format nil "~a~@[ · scorer ~a~]" (getf e :score)
                                             (and several (result-scorer default e))))
                         (reverse (board-scores host thread))))))

(defparameter +board-promote-script+
  "set -eu
cd \"$1\"
common=$(git rev-parse --git-common-dir)
mkdir -p \"$common/board\"
file=\"$common/board/$2\"
exec 9>\"$file.lock\"
flock 9
old=$(git rev-parse -q --verify \"$3\" || true)
held=''
[ -f \"$file\" ] && held=$(cut -d' ' -f1 \"$file\")
if [ -n \"$held\" ] && ! awk -v a=\"$5\" -v b=\"$held\" 'BEGIN { exit !(a > b) }'; then
  echo \"kept $(cat \"$file\") $old\"; exit 0
fi
git update-ref \"$3\" \"$4\" \"${old:-0000000000000000000000000000000000000000}\"
echo \"$5 $6\" > \"$file\"
echo \"promoted $5 $6 $4\"
"
  "bash: promote $4 to ref $3 in repo $1 for board $2 when its value $5 (score $6)
beats the value the board's best holds -- under a lock, the ref moved by
compare-and-swap.")

(defun board-promote (thread repository commit &key (platform "discord"))
  "Move the best branch of the scorer this bot's newest verify ran on THREAD
-- board/THREAD-KEY in REPOSITORY, KEY that scorer's, board/THREAD for a
verify that named none -- to COMMIT when that score beats the score the
branch holds: a compare-and-swap under a lock, so no bot overwrites a better
result, and a result never competes with another scorer's.
=> (:PROMOTED :BRANCH :SCORE :HELD)."
  (nle:receipt "calls" (list :verb "board-promote"))
  (multiple-value-bind (host) (running-answer "board-promote" platform nil thread nil)
    (let* ((newest (or (first (board-scores host thread))
                       (error "nothing to promote: run (nck:board-verify ~s) on the commit first" thread)))
           (board (format nil "~a~@[-~a~]" thread (getf newest :scorer)))
           (branch (format nil "board/~a" board))
           (sha (nlk:trimmed (uiop:run-program (list "git" "-C" (namestring repository) "rev-parse" "--verify"
                                                     (format nil "~a^{commit}" commit))
                                               :output :string)))
           (said (nlk:trimmed
                  (uiop:run-program (list "bash" "-c" +board-promote-script+ "promote"
                                          (namestring repository) board
                                          (format nil "refs/heads/~a" branch) sha
                                          (format nil "~f" (getf newest :value)) (getf newest :score))
                                    :output :string :error-output :output))))
      (list :promoted (uiop:string-prefix-p "promoted" said) :branch branch
            :score (getf newest :score) :said said))))

;;; --- the one answer --------------------------------------------------------------

(defun board-answer-text (host lane text)
  "TEXT, or the silent sentinel when LANE answers in a team's thread where a
peer holds the best measured result under every scorer results were posted
on: each scorer's best verified result writes its task's one answer, and a
result ranks only against results from its own scorer. A board nobody
measured anything on folds nothing -- each bot answers for itself."
  (let* ((thread (getf (lane-target lane) :thread-id))
         (self (host-self-id host))
         (leaders (and thread (stringp text) (not (silent-answer-p text))
                       (thread-slots host thread)
                       (board-leaders host thread))))
    (cond ((or (null leaders)
               (find self leaders :key (lambda (row) (getf row :user)) :test #'equal))
           text)
          (t (note-thread-spoken thread)
             (first +silence-spellings+)))))

(defun forget-board (host thread-id)
  "Drop what HOST's kit holds of THREAD-ID's board: its slots, posts, scores
and final answer -- the room's state keeps the slots."
  (let ((key (board-key host thread-id)))
    (dolist (table (list *thread-slots* *team-posts* *board-scores* *talk-finals*))
      (remhash key table))))
