;;;; owner.lisp --- an ask that reaches several bots has one owner; answers stay short.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A person's line can reach several bots at once: a thread a person shared
;;;; with them, a line that names another bot or answers one of its messages.
;;;; Every bot it reaches used to answer it, each with its own plan. Now one
;;;; bot owns it and the others keep it as the thread's context:
;;;;
;;;;   - A line that names this bot -- its mention, a reply to its message, a
;;;;     press, a slash command -- is this bot's, as before. One that names
;;;;     several bots by name is a team's (TEAM.LISP), as before; so is a
;;;;     role's ping, which calls in every bot holding the role; and so is
;;;;     any line on a team's board.
;;;;   - Otherwise the bots it reaches are ranked (ASK-RANK): the bot it points
;;;;     at -- the author of the message it answers, then each it names, in the
;;;;     order named -- then the bots that posted in its thread before it, the
;;;;     latest first, then the rest by a digest of the thread and the bot's
;;;;     id. Each of those is what every kit in the room reads alike -- the line,
;;;;     and the thread's own messages -- so no two race and none waits on
;;;;     another to agree; and a person goes on with the bot that answered last.
;;;;   - The first ranked answers. Each bot after it waits its place times
;;;;     +OWNER-GRACE-SECONDS+ and takes the ask over only when no bot ranked
;;;;     before it showed itself in the thread meanwhile -- a card, a reaction,
;;;;     an answer (NOTE-BOT-ACTIVITY): a bot that is down never leaves an ask
;;;;     unanswered.
;;;;
;;;; A room's answer_words caps what one answer says: the lane is told the cap
;;;; up front, and an answer that still runs past it is cut down by its own
;;;; model before it posts, the whole answer riding along as a file.

(in-package #:nodecode-channel-kit)

;;; --- one owner per ask ---------------------------------------------------------

(defparameter +owner-grace-seconds+ 90
  "How long each bot ranked after an ask's owner waits, per place, before it
 takes the ask over from the bots ranked before it.")

(defparameter +owner-activity-slack-ms+ 5000
  "How long before this kit read an ask a ranked bot's sign of life still
 counts for it: the owner may react before this kit has read the line.")

(defvar *bot-activity* (make-hash-table :test #'equal :synchronized t)
  "(platform id . thread or message id) -> ((USER-ID . MS) ...): when each
 author last showed itself there -- a line, a card, a reaction.")

(defun activity-key (candidate)
  "Where CANDIDATE shows its author at work: its thread, else the message a
 reaction was left on, which is the thread an ask opens off it."
  (or (source-field candidate "thread_id")
      (and (source-field candidate "reaction") (source-field candidate "reply_to_message_id"))))

(defun note-bot-activity (host candidate)
  "Keep that CANDIDATE's author showed itself where it was said (ACTIVITY-KEY)."
  (note-author-activity host (source-field candidate "user_id") (activity-key candidate)))

(defun note-author-activity (host user key)
  "Keep that USER, another bot, showed itself at KEY now: a thread, or the
 message an ask opens its thread off. An adapter calls it for what a bot does
 that routes no line -- an eye it leaves on a person's follow-up, a card it
 edits -- since those are how an owner whose turn took a follow-up in shows it
 still works."
  (progn
    (when (and user key (not (equal user (host-self-id host))))
      (let ((now (now-ms)))
        (sb-ext:with-locked-hash-table (*bot-activity*)
          ;; Only an ask's grace reads it: an entry an hour old answers nothing.
          (when (> (hash-table-count *bot-activity*) 512)
            (loop for k being the hash-keys of *bot-activity* using (hash-value seen)
                  when (< (reduce #'max seen :key #'cdr :initial-value 0) (- now 3600000))
                    do (remhash k *bot-activity*)))
          (let ((k (peer-key host key)))
            (setf (gethash k *bot-activity*)
                  (acons user now (remove user (gethash k *bot-activity*)
                                          :key #'car :test #'equal)))))))))

(defun note-bot-presence (host seen)
  "Keep SEEN, what an adapter read of a bot at work that routes no line:
(:POST USER NAME THREAD MESSAGE), a bot's message in a thread, this bot's
own too; (:SEEN USER KEY), another bot's eye or edit at KEY. NIL notes nothing."
  (destructuring-bind (&optional kind user &rest more) seen
    (case kind
      (:post (destructuring-bind (name thread message) more
               (note-thread-post host thread user name message)
               (note-author-activity host user thread)))
      (:seen (note-author-activity host user (first more))))))

(defun active-since-p (host key ids since)
  "Whether one of IDS showed itself at KEY at or after SINCE, in ms."
  (let ((seen (gethash (peer-key host key) *bot-activity*)))
    (some (lambda (id)
            (let ((at (cdr (assoc id seen :test #'equal))))
              (and at (>= at since))))
          ids)))

(defun without-role-pings (text)
  "TEXT with every role's ping taken out."
  (ppcre:regex-replace-all "<@&[^>]+>" text ""))

(defun self-named-p (policy candidate text)
  "Whether CANDIDATE asks this bot by name: addressed to it (a reply to its
 message, a press, a slash command), or mentioning it other than through a
 role it shares."
  (or (candidate-addressed-p candidate)
      (mention-involves-p policy (without-role-pings text))))

(defun role-pinged-p (policy text)
  "Whether TEXT reaches this bot through a role it holds and not by its own
 name: a role's ping calls in every bot holding the role, a team's ask
 (TEAM.LISP) that no one bot owns."
  (and (mention-involves-p policy text)
       (not (mention-involves-p policy (without-role-pings text)))))

(defun owner-digest (key id)
  "ID's draw for the ask at KEY: the same in every kit."
  (nlk:short-digest (format nil "~a/~a" key id) 16))

;;; Who posted last in a thread is what every kit in it reads the same way:
;;; each sees every bot's message there, and a message's id says its place.
;;; A kit's own record of a thread's peers is its own -- one kit may know a
;;; peer another never heard of -- so it never decides the owner while a
;;; thread has posts to read it from.

(defvar *thread-posters* (make-hash-table :test #'equal :synchronized t)
  "(platform id . thread id) -> ((USER-ID NAME MESSAGE-ID) ...), newest
 message first: each bot that posted in the thread, this one too, by its
 latest message there.")

(defun message-number (id)
  "ID, a platform's message id -- a Discord snowflake, a Slack ts, a Telegram
 number -- as the number its place in its thread reads as, or NIL."
  (and (stringp id) (plusp (length id))
       (every (lambda (c) (or (digit-char-p c) (char= c #\.))) id)
       (let ((dot (position #\. id)))
         (if (null dot)
             (parse-integer id)
             (+ (parse-integer id :end dot :junk-allowed t)
                (if (< (1+ dot) (length id))
                    (/ (parse-integer id :start (1+ dot))
                       (expt 10 (- (length id) dot 1)))
                    0))))))

(defun message-id< (a b)
  "Whether message A was posted before message B."
  (let ((x (message-number a)) (y (message-number b)))
    (if (and x y) (< x y) (string< a b))))

(defun note-thread-post (host thread user name message-id)
  "Keep that the bot USER, called NAME, posted MESSAGE-ID in THREAD: an
 answer, a card, this bot's own posts too. An adapter calls it for every
 bot's message in a thread, its own included."
  (when (and thread user message-id)
    (sb-ext:with-locked-hash-table (*thread-posters*)
      (when (> (hash-table-count *thread-posters*) 2048)
        (clrhash *thread-posters*))
      (let* ((key (peer-key host thread))
             (had (gethash key *thread-posters*))
             (old (assoc user had :test #'equal)))
        (unless (and old (not (message-id< (third old) message-id)))
          (setf (gethash key *thread-posters*)
                (sort (cons (list user (or name user) message-id)
                            (remove user had :key #'car :test #'equal))
                      (lambda (x y) (message-id< (third y) (third x))))))))))

(defun thread-posts-known-p (host thread)
  "Whether this image keeps any bot's post in THREAD."
  (and thread (gethash (peer-key host thread) *thread-posters*) t))

(defun thread-posters (host thread before)
  "The bots that posted in THREAD before message BEFORE, ((ID . NAME) ...),
 the latest first. Before the ask, never after it: a kit that reads the ask
 late has seen the owner's card on it, and must rank it as one that read it
 at once."
  (when thread
    (loop for (id name message) in (gethash (peer-key host thread) *thread-posters*)
          when (or (null before) (message-id< message before))
            collect (cons id name))))

(defun ask-reach (host candidate)
  "The bots CANDIDATE's line reaches beside this one, ((ID . NAME) ...): the
 bots it mentions or pings by a role, those that posted in its thread before
 it, and its thread's peers."
  (let ((thread (source-field candidate "thread_id")))
    (remove-duplicates
     (append (candidate-mentioned-bots candidate)
             (thread-posters host thread (source-field candidate "message_id"))
             (and thread (thread-peers host thread (candidate-room host candidate))))
     :key #'car :test #'equal :from-end t)))

(defun ask-rank (host candidate &aux (policy (host-policy host)) (self (host-self-id host)))
  "The bots that may answer CANDIDATE, a person's line that reaches others
 beside this bot, in the order they own it: (ID ...), the owner first. NIL
 when no one bot owns it: a bot's line, one that names this bot, one that
 reaches no other bot, or a team's -- several bots named by name, a role's
 ping, a board's thread."
  (let* ((text (let ((text (gethash "text" candidate))) (if (stringp text) text "")))
         (thread (source-field candidate "thread_id"))
         (key (or thread (source-field candidate "message_id")))
         (reach (and self key (typep policy 'inbound-policy)
                     (not (candidate-bot-p candidate))
                     (not (self-named-p policy candidate text))
                     (not (role-pinged-p policy text))
                     (not (and thread (thread-slots host thread (candidate-room host candidate))))
                     (ask-reach host candidate)))
         (ids (remove self (mapcar #'car reach) :test #'equal)))
    (when ids
      (let* ((replied (let ((reply (candidate-reply candidate)))
                        (and (hash-table-p reply) (gethash "user_id" reply))))
             (named (remove-if-not (lambda (id) (mention-position text id)) ids))
             ;; Then whoever answered in the thread last: the person goes on
             ;; with the bot they were talking to.
             (posted (mapcar #'car (thread-posters host thread (source-field candidate "message_id"))))
             (picked (remove-duplicates
                      (append (and (member replied ids :test #'equal) (list replied))
                              (mention-order text named)
                              posted)
                      :test #'equal :from-end t))
             (drawn (sort (set-difference (cons self ids) picked :test #'equal)
                          #'string> :key (lambda (id) (owner-digest key id)))))
        (unless (rest named)          ; several bots named by name: a team
          (append picked drawn))))))

(defun take-over-ask (host candidate)
  "Answer CANDIDATE, an ask the bots ranked before this one left alone."
  (note-asked-peers host candidate)
  (answer-message host (woken-ask host candidate)))

(defun defer-to-owner (host candidate)
  "When another bot owns CANDIDATE's ask (ASK-RANK), leave it to that bot and
 watch: take it over once this bot's grace ends with no bot ranked before it
 seen in the thread. => T when deferred."
  (let* ((rank (ask-rank host candidate))
         (at (position (host-self-id host) rank :test #'equal)))
    (when (and at (plusp at))
      (let ((before (subseq rank 0 at))
            (key (or (source-field candidate "thread_id") (source-field candidate "message_id")))
            (since (- (now-ms) +owner-activity-slack-ms+)))
        ;; The bots it reached are the thread's peers here too -- the thread
        ;; an ask opens carries the ask's own id, as on Discord -- so the
        ;; owner's lines are its context and its later lines have an owner.
        (note-thread-peers host key (ask-reach host candidate))
        (when (source-field candidate "thread_id")
          (save-thread-peers host key (candidate-room host candidate)))
        (sb-ext:schedule-timer
         (sb-ext:make-timer
          (lambda ()
            (on-worker host
              (nlk:with-handlers ((error (condition)
                                    (warn "~a could not take over ~a: ~a" (host-id host)
                                          (source-field candidate "message_id") condition)))
                (unless (active-since-p host key before since)
                  (take-over-ask host candidate)))))
          :name "ask owner grace" :thread t)
         (* at +owner-grace-seconds+))
        t))))

(defun owner-paragraph (host candidate)
  "What CANDIDATE's ask is to the lane that answers it when it reached other
 bots too: theirs to read, this lane's to do. NIL for a line this bot alone
 was asked."
  (let* ((rank (ask-rank host candidate))
         (at (position (host-self-id host) rank :test #'equal)))
    (when at
      (let ((names (loop with known = (ask-reach host candidate)
                         for id in rank
                         unless (equal id (host-self-id host))
                           collect (format nil "@~a" (or (cdr (assoc id known :test #'equal)) id)))))
        (format nil "This ask reached ~{~a~#[~; and ~:;, ~]~} too, and it is yours: ~a They ~
read it and stay quiet unless you or the person name them, so do the whole of it, and name one ~
only to hand it a part."
                names
                (if (zerop at)
                    "you own it."
                    (format nil "the bots ranked to own it before you stayed silent for ~d s."
                            (* at +owner-grace-seconds+))))))))

;;; --- short answers ------------------------------------------------------------

(defun answer-word-count (text)
  "The words TEXT says to a person: outside fenced code, each run of
 non-space holding a letter or a digit. A FINAL ANSWER line is no word."
  (let ((code nil) (count 0))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)) count)
      (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
        (cond ((uiop:string-prefix-p "```" line) (setf code (not code)))
              (code)
              ((string-equal (string-trim "*_: " line) "final answer"))
              (t (incf count (count-if (lambda (word) (some #'alphanumericp word))
                                       (uiop:split-string line :separator '(#\Space #\Tab))))))))))

(defun answer-cap-paragraph (host)
  "What the room's answer_words asks of a lane, or NIL when it caps nothing."
  (let ((cap (host-answer-words host)))
    (when (plusp cap)
      (format nil "Answers here post at most ~d words, code blocks aside: a longer one is cut ~
down before it posts, the whole one riding along as a file. So write it short yourself: the ~
verdict and what the person must do or decide; detail goes into a file, the PR or the ticket."
              cap))))

(defun shortening-instruction (cap)
  "The side call's whole instruction for cutting an answer to CAP words."
  (format nil "You shorten a chat answer to at most ~d words, code blocks aside, for a busy ~
reader. Keep the verdict first. Keep every number, id, name, link, command and path you keep ~
exactly as written, and keep the question or choice the answer ends on. When its first line ~
is FINAL ANSWER, keep that line first. Keep every caveat about what was not checked, proven or ~
tested, every correction and every open item: they are findings, not hedges. Drop only filler: ~
background, restated asks, repeated facts, thanks. Never add a fact, a number or a claim the ~
answer does not make. Keep its markdown and its language. Reply with the shortened answer alone." cap))

(defun shorten-answer (host lane text final)
  "(values TEXT FULL-FILE): TEXT as LANE posts it under its room's
 answer_words, and the file holding the whole answer when it was cut down.
 An answer within the cap, or one its model could not shorten, posts whole."
  (let ((cap (host-answer-words host))
        (words (answer-word-count text)))
    (if (or (not (plusp cap)) (<= words cap))
        text
        (let ((short (nlk:with-handlers ((error (condition)
                                           (warn "~a could not shorten an answer of ~d words: ~a"
                                                 (host-id host) words condition)
                                           nil))
                       (nlk:trimmed (nle:complete (shortening-instruction cap) text
                                                  :session-id (lane-session-id lane)
                                                  :max-tokens 1200)))))
          (if (or (null short) (zerop (length short))
                  (>= (answer-word-count short) words))
              text
              (let ((file (merge-pathnames (format nil "nodecode-answer-~a/full-answer.md"
                                                   (nlk:short-digest text 12))
                                           (uiop:temporary-directory))))
                (ensure-directories-exist file)
                (with-open-file (out file :direction :output :if-exists :supersede
                                          :external-format :utf-8)
                  (write-string text out))
                (values (if (and final (not (final-answer-line-p short)))
                            (format nil "FINAL ANSWER~%~a" short)
                            short)
                        file)))))))
