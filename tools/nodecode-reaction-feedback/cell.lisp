;;;; cell.lisp --- the note a user's reactions become, at the turn after them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A chat surface reports what it sees -- the user's messages (ASK) and the
;;;; reactions left on the agent's own (REACT) -- and the cell keeps one ledger
;;;; per conversation. The turn after attributes each reaction to the session
;;;; whose message it sits under and hands that session a short note, once, as a
;;;; harness section of its prompt (NOTE-SECTION), which is the same place the
;;;; original plugin put it: the start of the next turn, marked as not typed by
;;;; the user. Everything lives in memory, per conversation, bounded and aged
;;;; out; stopping the cell empties it, as the original's process exit did.

(defpackage #:nodecode-reaction-feedback
  (:use #:cl)
  (:nicknames #:reaction-feedback)
  (:export
   ;; what a chat surface, or the model, calls
   #:ask #:react #:note #:chats #:clear
   ;; the one condition every refusal is
   #:reaction-feedback-error))

(in-package #:nodecode-reaction-feedback)

;;; Defines REACTION-FEEDBACK-ERROR and FAIL, *REACTION-FEEDBACK*, RUNNING-SETTINGS
;;; and WITH-REACTION-FEEDBACK-LOCK, the lock the ledger and its readers share.
(nlk:define-peripheral reaction-feedback :not-running t :lock "reaction-feedback")

;;; --- the wording, as data ---------------------------------------------------------

(defparameter +header+
  "[Reactions from the user on your messages since your last turn (reaction-feedback; not typed by the user)]"
  "The note's first line: whose words these are not.")

(defparameter +footer+
  "Treat these as lightweight feedback on those messages, not as instructions."
  "The note's last line: how much weight to give it.")

(defparameter +no-text+ "a message without text"
  "What a message with nothing written in it reads as.")

(defparameter +max-emojis+ 4
  "How many emojis one reaction is read as carrying.")

(defparameter +max-emoji-chars+ 16
  "How long one emoji may be before it is cut: a custom emoji is short.")

;;; --- the ledger -------------------------------------------------------------------

(defstruct (anchor (:constructor make-anchor (id excerpt session)))
  "One of the user's messages: the platform's id, a flat excerpt, the session it started."
  (id 0 :type integer)
  (excerpt "" :type string)
  (session nil))

(defstruct (mark (:constructor make-mark (id emojis custom excerpt session chat at)))
  "One message of the agent's, as the user's reaction on it currently reads."
  (id 0 :type integer)
  (emojis '())
  (custom 0)
  (excerpt "")
  (session nil)
  (chat nil)
  (at 0))

(defstruct (conversation (:constructor make-conversation))
  "One private chat: its anchors ascending by id, its pending reactions, its age."
  (anchors '())
  (pending '())
  (verified nil)
  (seq 0))

(defvar *chats* (make-hash-table :test #'equal)
  "Chat key (a string) -> CONVERSATION. In memory only, as the original's was.")

(defvar *seq* 0 "How many conversations have been touched: the eviction order.")

(defvar *served* nil
  "(TURN-ID . TEXT): the note a turn already took, so one turn reads it once.")

(defun clear-all ()
  "Empty the ledger: what a start and a stop both do."
  (with-reaction-feedback-lock
    (clrhash *chats*)
    (setf *served* nil))
  nil)

(defun trimmed (text)
  "TEXT without the whitespace at either end."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) text))

(defun control-p (char)
  "Whether CHAR is a control the note should not carry."
  (or (< (char-code char) 32) (= (char-code char) 127)))

(defun words-of (text)
  "TEXT's words, a control character standing in as a break."
  (loop with start = nil
        with found = '()
        for index from 0 below (length text)
        for char = (char text index)
        for space = (or (control-p char) (member char '(#\Space #\Tab) :test #'char=))
        do (cond (space (when start (push (subseq text start index) found) (setf start nil)))
                 ((null start) (setf start index)))
        finally (when start (push (subseq text start) found))
        finally (return (nreverse found))))

(defun flatten (text)
  "TEXT as one line of single spaces, its control characters gone."
  (format nil "~{~a~^ ~}" (words-of (or text ""))))

(defun quoted (text)
  "TEXT in double quotes, the two characters that would end them escaped."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text
          do (when (member char '(#\" #\\) :test #'char=) (write-char #\\ out))
             (write-char char out))
    (write-char #\" out)))

(defun excerpt (text)
  "TEXT as the note names it: one line, at most EXCERPT-CHARS, quoted, or the no-text line."
  (let* ((flat (flatten text))
         (limit (setting :excerpt-chars)))
    (cond ((zerop (length flat)) +no-text+)
          ((<= (length flat) limit) (quoted flat))
          (t (quoted (concatenate 'string
                                  (string-right-trim " " (subseq flat 0 (1- limit)))
                                  "…"))))))

(defun clean-emojis (values)
  "VALUES (an array of strings) as at most +MAX-EMOJIS+ short, distinct, space-free emojis."
  (let ((out '()))
    (loop for value across (typecase values (vector values) (list (coerce values 'vector)) (t #()))
          when (stringp value)
            do (let ((emoji (remove-if (lambda (char)
                                         (or (control-p char) (member char '(#\Space #\Tab)
                                                                       :test #'char=)))
                                       value)))
                 (when (> (length emoji) +max-emoji-chars+)
                   (setf emoji (subseq emoji 0 +max-emoji-chars+)))
                 (when (and (plusp (length emoji)) (not (member emoji out :test #'equal)))
                   (push emoji out)))
          when (= (length out) +max-emojis+) do (loop-finish))
    (nreverse out)))

(defun id-of (value)
  "VALUE as a message id: an integer, or a string of digits, or NIL."
  (typecase value
    (integer value)
    (string (let ((text (trimmed value)))
              (when (and (plusp (length text))
                         (every #'digit-char-p (if (char= (char text 0) #\-) (subseq text 1) text)))
                (parse-integer text))))
    (t nil)))

(defun chat-key (value)
  "VALUE as the string the ledger keys a conversation on, or NIL."
  (typecase value
    (string (let ((text (trimmed value))) (and (plusp (length text)) text)))
    (integer (princ-to-string value))
    (t nil)))

(defun session-id-of (value)
  "VALUE as the session a message started, or the running turn's own."
  (let ((text (typecase value
                (string (trimmed value))
                (integer (princ-to-string value))
                (t nil))))
    (cond ((and text (plusp (length text))) text)
          (t (getf (nle:turn) :session-id)))))

;;; --- the conversations ------------------------------------------------------------

(defun chat-keys ()
  "Every conversation the ledger keeps, in no order."
  (let ((keys '()))
    (maphash (lambda (key conversation) (declare (ignore conversation)) (push key keys)) *chats*)
    (nreverse keys)))

(defun evict-chats ()
  "Drop the least recently touched conversations until at most MAX-CHATS stand, the
unverified of a chat that never started a turn going first, as the original evicted."
  (let ((limit (setting :max-chats)))
    (loop while (> (hash-table-count *chats*) limit)
          for victim = (first (sort (chat-keys)
                                    (lambda (left right)
                                      (let ((a (gethash left *chats*))
                                            (b (gethash right *chats*)))
                                        (if (eq (conversation-verified a) (conversation-verified b))
                                            (< (conversation-seq a) (conversation-seq b))
                                            (not (conversation-verified a)))))))
          do (remhash victim *chats*))))

(defun touch (conversation)
  "Mark CONVERSATION as the newest, so the eviction leaves it alone."
  (setf (conversation-seq conversation) (incf *seq*))
  conversation)

(defun conversation-for (key &key create)
  "The conversation KEY names; with CREATE, one made and touched when there is none."
  (let ((conversation (gethash key *chats*)))
    (cond (conversation (touch conversation))
          (create (setf (gethash key *chats*) (touch (make-conversation))))
          (t nil))))

(defun expire (conversation now)
  "Drop CONVERSATION's reactions older than the TTL, then all but the newest MAX-PENDING."
  (let ((ttl (* 3600 (setting :ttl-hours)))
        (limit (setting :max-pending)))
    (setf (conversation-pending conversation)
          (remove-if (lambda (mark) (> (- now (mark-at mark)) ttl))
                     (conversation-pending conversation)))
    (when (> (length (conversation-pending conversation)) limit)
      (setf (conversation-pending conversation)
            (subseq (sort (conversation-pending conversation) #'> :key #'mark-at)
                    0 limit)))))

;;; --- what a chat surface reports --------------------------------------------------

(defun ask (chat message-id text &key session (now (get-universal-time)))
  "Record one of the user's messages: CHAT, its platform MESSAGE-ID, what it said.
The SESSION it started is this turn's unless given; that is the attribution the
reactions on the agent's later messages are read against."
  (let ((key (chat-key chat))
        (id (id-of message-id)))
    (unless key (fail "a message needs chat: the conversation it came from"))
    (unless id (fail "a message needs message_id: the platform's id for it, an integer"))
    (with-reaction-feedback-lock
      (let* ((conversation (conversation-for key :create t))
             (seen (find id (conversation-anchors conversation) :key #'anchor-id)))
        (unless seen
          (setf (conversation-anchors conversation)
                (sort (cons (make-anchor id (excerpt text) (session-id-of session))
                            (conversation-anchors conversation))
                      #'< :key #'anchor-id))
          (let ((overflow (- (length (conversation-anchors conversation))
                             (setting :max-anchors))))
            (when (plusp overflow)
              (setf (conversation-anchors conversation)
                    (nthcdr overflow (conversation-anchors conversation)))))
          (evict-chats))
        (format nil "noted the user's message ~d in ~a~@[ for session ~a~]"
                id key (and (not seen) (anchor-session (find id (conversation-anchors conversation)
                                                             :key #'anchor-id))))))))

(defun attributed-anchor (conversation id)
  "The user's message a reaction on message ID in CONVERSATION belongs to, or a reason it does not."
  (let ((anchor nil))
    (dolist (candidate (conversation-anchors conversation))
      (cond ((< (anchor-id candidate) id) (setf anchor candidate))
            ((= (anchor-id candidate) id) (return-from attributed-anchor :own))))
    (cond ((null anchor) :older)
          ((null (anchor-session anchor)) :no-turn)
          (t anchor))))

(defun react (chat message-id emojis &key custom (now (get-universal-time)))
  "Record the user's current reaction set on one of the agent's messages: CHAT, its
MESSAGE-ID and EMOJIS (an array; empty means the reaction was taken back). The
reaction goes to the session whose message it sits under, as the original did."
  (let ((key (chat-key chat))
        (id (id-of message-id))
        (marks (clean-emojis emojis))
        (custom (or custom 0)))
    (unless key (fail "a reaction needs chat: the conversation it came from"))
    (unless id (fail "a reaction needs message_id: the platform's id for the reacted message"))
    (with-reaction-feedback-lock
      (let ((conversation (gethash key *chats*)))
        (unless conversation
          (return-from react (format nil "nothing known yet in ~a: no message of the user's has been seen" key)))
        (touch conversation)
        (case (attributed-anchor conversation id)
          (:own (format nil "message ~d is the user's own: no turn to give the reaction to" id))
          (:older (format nil "message ~d is older than any message seen in ~a" id key))
          (:no-turn (format nil "the message before ~d started no turn: the reaction goes nowhere" id))
          (t (let ((anchor (attributed-anchor conversation id)))
               (setf (conversation-pending conversation)
                     (remove id (conversation-pending conversation) :key #'mark-id))
               (cond ((and (null marks) (zerop custom))
                      (format nil "the reaction on ~d is taken back" id))
                     (t (setf (conversation-pending conversation)
                              (cons (make-mark id marks custom (anchor-excerpt anchor)
                                               (anchor-session anchor) key now)
                                    (conversation-pending conversation)))
                        (setf (conversation-verified conversation) t)
                        (expire conversation now)
                        (evict-chats)
                        (format nil "noted ~a on message ~d, feedback for session ~a, after the user's ~a"
                                (if marks (format nil "~{~a~^ ~}" marks) "a custom emoji")
                                id (anchor-session anchor) (anchor-excerpt anchor)))))))))))

;;; --- the note ---------------------------------------------------------------------

(defun mark-line (mark)
  "One line of the note: what the user did, and which message it sits under."
  (let ((marks (mark-emojis mark))
        (custom (mark-custom mark)))
    (format nil "- ~a on a message you sent after the user's ~a"
            (cond ((and marks (plusp custom)) (format nil "~{~a~^ ~} and ~d custom emoji~:p" marks custom))
                  (marks (format nil "~{~a~^ ~}" marks))
                  (t (format nil "~d custom emoji~:p" custom)))
            (mark-excerpt mark))))

(defun note (session &key (now (get-universal-time)) (consume t))
  "The note SESSION's turn has waiting, or NIL: the reactions attributed to it, newest
first. With CONSUME (the default) they are taken, so one turn reads them once."
  (let ((key (session-id-of session)))
    (when key
      (with-reaction-feedback-lock
        (let ((due '()))
          (maphash (lambda (chat conversation)
                     (declare (ignore chat))
                     (expire conversation now)
                     (dolist (mark (conversation-pending conversation))
                       (when (equal key (mark-session mark)) (push mark due))))
                   *chats*)
          (when due
            (when consume
              (dolist (mark due)
                (let ((conversation (gethash (mark-chat mark) *chats*)))
                  (when conversation
                    (setf (conversation-pending conversation)
                          (remove mark (conversation-pending conversation)))))))
            (format nil "~a~%~{~a~%~}~a" +header+
                    (mapcar #'mark-line (sort due #'> :key #'mark-id))
                    +footer+)))))))

(defun note-section (next session-id)
  "The harness-section advice: a turn whose session has reactions waiting gets the note
among its sections; a turn that already took it this sitting reads the same text again."
  (let* ((sections (funcall next session-id))
         (turn-id (getf (nle:turn) :turn-id))
         (cached (and turn-id *served* (equal turn-id (car *served*)) (cdr *served*)))
         (text (or cached (let ((waiting (note session-id))) (when turn-id (setf *served* (cons turn-id waiting))) waiting))))
    (cond ((and text (plusp (length text)))
           (acons "reaction-feedback" text
                  (remove "reaction-feedback" sections :key #'car :test #'equal)))
          (t sections))))

;;; --- the verbs --------------------------------------------------------------------

(define-verb chats (&optional session)
  "What stands in the ledger: one line per conversation, and this session's note."
  (let ((key (and session (session-id-of session))))
    (with-reaction-feedback-lock
      (let ((keys (chat-keys)))
        (cond ((null keys) "no conversation has been reported yet")
              (t (with-output-to-string (out)
                   (format out "~d conversation~:p: ~%" (length keys))
                   (dolist (chat keys)
                     (let ((conversation (gethash chat *chats*)))
                       (format out "  ~a: ~d message~:p, ~d reaction~:p waiting~%"
                               chat
                               (length (conversation-anchors conversation))
                               (length (conversation-pending conversation)))))
                   (format out "~@[for session ~a:~%~a~]"
                           key (or (note key :consume nil)
                                   "nothing waits for this session")))))))))

(define-verb clear (&optional session)
  "Take every waiting reaction of SESSION (this turn's own when none is named)."
  (let ((key (session-id-of session))
        (taken 0))
    (with-reaction-feedback-lock
      (maphash (lambda (chat conversation)
                 (declare (ignore chat))
                 (let ((waiting (remove-if-not (lambda (mark) (equal key (mark-session mark)))
                                                (conversation-pending conversation))))
                   (when waiting
                     (incf taken (length waiting))
                     (setf (conversation-pending conversation)
                           (set-difference (conversation-pending conversation) waiting)))))
               *chats*))
    (format nil "~[nothing was waiting~:;~d reaction~:p taken~]" taken taken)))

;;; --- the tool ---------------------------------------------------------------------

(nlk:define-contract reaction-report
  (:doc "What a chat surface reported, or what the user's reactions say. Report the user's
message (action=ask) and the reactions left on the agent's own messages (action=record); the
turn after reads them as a short note (action=note), which the prompt already carries by
itself when the surface reported in time.")
  ("action" :text :required t :options '("ask" "record" "note" "status") :doc "what to do")
  ("chat" :text :doc "the conversation the message or reaction came from")
  ("message_id" :integer :doc "the platform's id for that message")
  ("text" :string :default "" :doc "ask: what the user wrote")
  ("emojis" :array :of :string :doc "record: the emojis the user's reaction now carries; empty is taken back")
  ("custom" :integer :default 0 :doc "record: how many custom emojis it carries")
  ("session" :string :doc "ask: the session the message started; note, status and clear: which one to read"))

(defun tool-answer (function)
  "FUNCTION's answer, or its refusal as the ERROR line the model repairs from."
  (handler-case (funcall function)
    (reaction-feedback-error (condition) (nle:failure (format nil "ERROR: reaction-feedback: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: reaction-feedback: ~a" condition)))))

(defun run-tool (report)
  "The reaction_feedback tool."
  (tool-answer
   (lambda ()
     (let ((action (reaction-report-action report)))
       (cond ((string= action "ask")
              (ask (reaction-report-chat report) (reaction-report-message-id report)
                   (reaction-report-text report) :session (reaction-report-session report)))
             ((string= action "record")
              (react (reaction-report-chat report) (reaction-report-message-id report)
                     (reaction-report-emojis report) :custom (reaction-report-custom report)))
             ((string= action "note")
              (or (note (reaction-report-session report))
                  "no reaction is waiting for that session"))
             ((string= action "status") (chats (reaction-report-session report)))
             (t (fail "unknown action ~s: ask, record, note or status" action)))))))

;;; --- /reaction-feedback and the manual --------------------------------------------

(defparameter +primer+
  "Reactions are read as feedback, not as asks. A chat surface reports the user's message
(action=ask: chat, message_id, text) and each reaction left on the agent's own messages
(action=record: chat, message_id, emojis), and the turn after that user's message sits
under gets a short note at the top of its prompt: which emojis, and which message of the
user's the reacted answer came after. It is labelled as not typed by the user, so it is
feedback on those messages, not an instruction. A reaction changed replaces the one before
it, a reaction taken back withdraws it, and a reaction on the user's own message or on
something older than anything reported goes nowhere. Nothing is written to disk, the ledger
is bounded (max_pending, ttl_hours, max_chats), and stopping the cell empties it.
Verbs callable from eval, each answering a string:
  (reaction-feedback:ask \"1234\" 12 \"fix the cron job\")   one of the user's messages
  (reaction-feedback:react \"1234\" 15 '((\"👍\")))       a reaction on the agent's answer
  (reaction-feedback:note \"s1\")   this turn's note, and it is taken
  (reaction-feedback:chats \"s1\")  every conversation the ledger keeps
  (reaction-feedback:clear \"s1\")  take this session's waiting reactions away
The operator types /reaction-feedback: alone, what waits for this session; `clear` takes it away."
  "What (help :reaction-feedback) answers while the cell runs.")

(defun run-command (args session-id)
  "/reaction-feedback: this session's waiting note, or `clear` to take it away."
  (let ((typed (string-downcase (trimmed (or args "")))))
    (cond ((string= typed "clear") (clear session-id))
          ((string= typed "status") (chats session-id))
          (t (let ((waiting (note session-id)))
               (cond (waiting waiting)
                     (t "no reaction is waiting for this session: react to one of the agent's
messages and the turn after reads it")))))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell reaction-feedback
  (:section ("reaction-feedback")
    (:guide "nothing here is required; a chat surface reports to the cell and the next turn reads it")
    ("excerpt_chars" :integer :default 60 :min 10 :doc "how much of the user's message the note quotes")
    ("max_anchors" :integer :default 200 :min 1 :doc "how many of the user's messages one conversation keeps")
    ("max_pending" :integer :default 20 :min 1 :doc "how many reactions wait for a turn")
    ("ttl_hours" :integer :default 48 :min 1 :doc "how long a waiting reaction keeps")
    ("max_chats" :integer :default 256 :min 1 :doc "how many conversations the ledger keeps"))
  (:tool "reaction_feedback" (nlk:contract 'reaction-report) #'run-tool)
  (:command "reaction-feedback" 'run-command
            :description "The reactions waiting on this session's messages"
            :argument-hint "[clear]")
  (:help :reaction-feedback "reaction-feedback: the note a user's reactions on the agent's messages become at the next turn" +primer+)
  (:hook 'nle::read-harness-sections #'note-section)
  (:start (lambda ()
            (clear-all)
            (nle:on-stop #'clear-all))))
