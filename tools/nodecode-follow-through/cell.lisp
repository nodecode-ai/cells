;;;; cell.lisp --- the section, the two hooks, the tool, /follow-through and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is one llm_execution middleware: it reads each final model
;;;; reply and, when the reply promises work that nothing is running, rewrites
;;;; it into a call to its own tool. Here the same read happens around
;;;; NLK:RECORD-ASSISTANT-MESSAGE -- once per round of a live turn, over the
;;;; exact message the model just wrote -- and the nudge is delivered through
;;;; NLE:SUBMIT, the one in-process ingress: the turn that promised ends and
;;;; the next one starts with the nudge in front of it, so the work really
;;;; does continue. A second hook, around NLE:SUBMIT itself, starts the count
;;;; over at every prompt that is the operator's own.

(in-package #:nodecode-follow-through)

(nlk:define-peripheral follow-through :not-running t)

(defparameter +tag+ "[follow-through]"
  "What opens every nudge this cell submits: the one thing that tells its own
prompt from the operator's, so the count starts over at the operator's.")

(defparameter +default-max-nudges+ 2
  "How many nudges one prompt earns when the section says nothing.")

(defparameter +quote-limit+ 300
  "How much of the promise a nudge quotes, as the plugin's own quote[:300] did.")

(defparameter *nudges* (make-hash-table :test #'equal)
  "SESSION-ID -> nudges given since the operator's last own prompt.")

(defparameter *on* t
  "The runtime switch /follow-through off flips, on again at the next start.")

(defparameter +blank+ (list #\Space #\Tab #\Newline #\Return #\Page))

;;; --- the count -----------------------------------------------------------

(defun max-nudges ()
  "How many times one prompt may be nudged: the section's max_nudges while
the cell runs, the default when it is not."
  (if *follow-through*
      (let ((value (ignore-errors (setting :max-nudges))))
        (if (integerp value) value +default-max-nudges+))
      +default-max-nudges+))

(defun nudges-given (session-id)
  "How many nudges SESSION-ID has had since its last own prompt."
  (gethash (or session-id "") *nudges* 0))

(defun note-nudge (session-id)
  (incf (gethash (or session-id "") *nudges* 0)))

(defun forget-chain (session-id)
  (remhash (or session-id "") *nudges*))

(defun starts-with-p (prefix text)
  (and (stringp text)
       (>= (length text) (length prefix))
       (string= prefix text :end2 (length prefix))))

(defun nudge-prompt-p (text)
  "T when TEXT is one of this cell's own nudges, not a person's prompt."
  (starts-with-p +tag+ text))

;;; --- what a nudge says ---------------------------------------------------

(defparameter +nudge-rule+
  "Before you reply again, do one of these now: (1) do that work in this turn and include its
result; or (2) if it needs longer than this turn, actually launch it now (a background evaluation, a
cron job, a delegated session) so it keeps running after you reply. Then reply with what is done, or
with exactly what is now running and what it will bring. Never describe work as started unless it is
running.")

(defun clipped (text limit)
  (let ((text (or text "")))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun nudge-text (quote)
  "The nudge for QUOTE: what the reply promised and the only two legal exits."
  (let ((quote (string-trim +blank+ (clipped quote +quote-limit+))))
    (format nil "~a Your last reply told the person that work has started or will happen: \"~a\". ~
Nothing runs after you reply: when you answer, your turn ends and your work stops. ~a"
            +tag+
            (if (plusp (length quote)) quote "the work you described")
            +nudge-rule+)))

(defun deliver-nudge (session-id text)
  "Put TEXT into SESSION-ID as its next prompt."
  (nle:submit session-id text))

;;; --- the round ------------------------------------------------------------

(defun message-role (message)
  (and (hash-table-p message) (gethash "role" message)))

(defun message-tool-calls (message)
  "The calls MESSAGE carries, or NIL: a provider answers :NULL for absent."
  (let ((calls (and (hash-table-p message) (gethash "tool_calls" message))))
    (cond ((null calls) nil)
          ((eq calls :null) nil)
          ((and (vectorp calls) (zerop (length calls))) nil)
          ((and (listp calls) (null calls)) nil)
          (t calls))))

(defun final-reply-p (message)
  "T when MESSAGE is a model round's answer that carries text and no call: the
only shape that can end a turn on a promise."
  (and (equal "assistant" (message-role message))
       (null (message-tool-calls message))
       (let ((text (nle:message-content message)))
         (and (stringp text)
              (plusp (length (string-trim +blank+ text)))))))

(defun session-of (turn)
  "The session the running round belongs to: the live turn's, else TURN's."
  (or (getf (nle:turn) :session-id)
      (and turn (ignore-errors (nlk:durable-turn-session-id turn)))))

(defun maybe-nudge (turn message)
  "The promise MESSAGE made, once it has been answered with a nudge, else NIL."
  (when (and *on* (plusp (max-nudges)) (final-reply-p message))
    (let ((session (session-of turn)))
      (when (and session (< (nudges-given session) (max-nudges)))
        (let ((quote (promise-sentence (nle:message-content message))))
          (when quote
            (note-nudge session)
            (deliver-nudge session (nudge-text quote))
            quote))))))

(defun watch-turn (next turn message &rest arguments)
  "Around NLK:RECORD-ASSISTANT-MESSAGE: a final reply that promises work
nothing is running does not end this session's work here. Fail-open: a nudge
that cannot be given leaves the turn exactly as it was."
  (multiple-value-prog1 (apply next turn message arguments)
    (ignore-errors (maybe-nudge turn message))))

(defun watch-prompt (next session-id prompt &rest arguments)
  "Around NLE:SUBMIT: a prompt that is not this cell's own starts the count
over, before the turn it admits can be nudged."
  (unless (nudge-prompt-p prompt)
    (forget-chain session-id))
  (apply next session-id prompt arguments))

;;; --- the manual and the two verbs -----------------------------------------

(defparameter +manual+
  "follow-through holds an answer that ends the work on a promise. When a final reply (text, no tool
call) says work has started or will happen -- \"I've started the deep dive and will send the report
shortly\" -- and nothing is running, the cell puts one nudge in front of the agent: do that work now
in this turn, or actually launch it (a background evaluation, a cron job, a delegated session) and
say exactly what is running. The turn that promised ends; the nudge opens the next one, so the work
really does continue. At most max_nudges (2 by default) per prompt, and a new prompt of yours
starts the count over. Offers and questions (\"If you want, I'll...\", \"Should I...?\"), negations
and honest status (\"Done: ...\") are left alone, in English and in Italian. The follow_through tool
answers the same rule for a draft the model wants to check itself; /follow-through shows it and
turns it off; (nodecode-follow-through:promise-sentence TEXT) is the reader a test or a script
calls.")

(nlk:define-contract follow-through-ask
  (:doc "Ask the follow-through cell what to do about a reply that promises work: it answers the rule and the two exits it allows, so a draft can be checked before it is sent.")
  ("quote" :text :default "" :slot quoted
             :doc "the promise sentence from the draft, as written"))

(defun read-follow-through (ask)
  "The follow_through tool: the rule and the exits, for the promise ASK quotes."
  (nudge-text (follow-through-ask-quoted ask)))

(defun status-text (session-id)
  "One line: the switch, the section's cap, and what this session has spent."
  (format nil "follow-through is ~a: a final reply that promises work nothing is running is held and turned into a nudge, at most ~d per prompt, and this session has had ~d since your last prompt. /follow-through off turns it off; on turns it back on."
          (if *on* "on" "off")
          (max-nudges)
          (nudges-given session-id)))

(defun run-slash (args session-id)
  "/follow-through [on|off|status]: what the cell holds, and the switch."
  (let ((word (string-downcase (string-trim +blank+ (or args "")))))
    (cond ((string= word "off")
           (setf *on* nil)
           "follow-through: off until you turn it on, or the cell restarts.")
          ((string= word "on")
           (setf *on* t)
           (status-text session-id))
          ((member word '("" "status") :test #'string=)
           (status-text session-id))
          (t (format nil "~s is not one of on, off, status." word)))))

;;; --- start and stop -------------------------------------------------------

(defun start ()
  "Turn the cell on and forget every session's count; the stop puts it back."
  (setf *on* t)
  (clrhash *nudges*)
  (nle:on-stop (lambda ()
                 (setf *on* nil)
                 (clrhash *nudges*))))

(nle:define-cell follow-through
  (:section ("follow-through")
    (:guide "nothing here is required; max_nudges is how many times one prompt may be held")
    ("max_nudges" :integer :default 2 :min 0
                  :doc "how many nudges one prompt may earn before a promising reply is let through"))
  (:hook 'nle::record-assistant-message #'watch-turn)
  (:hook 'nle:submit #'watch-prompt)
  (:tool "follow_through" (nlk:contract 'follow-through-ask) 'read-follow-through)
  (:command "follow-through" 'run-slash
            :description "Hold an answer that promises work nothing is running"
            :argument-hint "ON|OFF|STATUS")
  (:help :follow-through
         "follow-through: holds a reply that promises work nothing is running, until it does the work or launches it"
         +manual+)
  (:start #'start))
