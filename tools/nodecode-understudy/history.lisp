;;;; history.lisp --- the past decisions this cell replays.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A decision is one turn's question and the answer the agent actually gave
;;;; it: a user message with text, and the last assistant text that followed.
;;;; The read goes through the one seam HISTORY-DECISIONS, so a test stubs a
;;;; name and never needs a session store.

(in-package #:nodecode-understudy)

(defstruct (decision (:constructor make-decision))
  "One past turn: PROMPT asked, ANSWER the agent gave, SESSION-ID it ran in."
  (prompt "" :type string)
  (answer "" :type string)
  (session-id "" :type string))

(defun message-role (message)
  "MESSAGE's role, or NIL when it is not a wire-shaped message."
  (and (hash-table-p message) (nlk:json-value message :text "role")))

(defun message-text (message)
  "MESSAGE's content as trimmed text, empty when it carries none."
  (let ((text (and (hash-table-p message) (nle::message-content message))))
    (or (and (stringp text) (nlk:trimmed text)) "")))

(defun turn-decision (messages)
  "The turn's question and the answer it ended on, as (PROMPT ANSWER), or NIL."
  (let ((prompt nil) (answer nil))
    (dolist (message messages)
      (let ((role (message-role message))
            (text (message-text message)))
        (cond ((and (null prompt) (equal role "user") (plusp (length text)))
               (setf prompt text))
              ((and (equal role "assistant") (plusp (length text)))
               (setf answer text)))))
    (when (and prompt answer) (list prompt answer))))

(defun session-decisions (session-id)
  "SESSION-ID's decisions, oldest first; NIL when its log cannot be read."
  (let ((turns (ignore-errors
                 (nle::history-turns-for-session (nle:intern-session session-id)))))
    (loop for turn in turns
          for pair = (turn-decision (nle::history-turn-messages turn))
          when pair collect pair)))

(defun recent-enough-p (session days)
  "Whether SESSION saw activity inside the last DAYS days, when its stamp is a
number; a stamp of another shape is taken as recent."
  (let ((stamp (getf session :updated-at)))
    (or (not (integerp stamp))
        (>= stamp (- (get-local-time-nanos) (* days 86400 1000000000))))))

(defun get-local-time-nanos ()
  "Wall clock in nanoseconds, the unit the session directory stamps."
  (round (* 1000000 (get-internal-real-time)) (/ internal-time-units-per-second)))

(defun history-decisions (&key (days 14) (limit 8) (exclude ""))
  "The newest past decisions of this home, at most LIMIT of them.

EXCLUDE is the session the caller runs in, which a replay skips: a model
should not be graded on the question it is answering now."
  (let ((decisions '()))
    (dolist (session (or (ignore-errors (nlk:list-sessions)) '()))
      (let ((id (getf session :id)))
        (when (and (stringp id)
                   (plusp (length id))
                   (not (equal id exclude))
                   (recent-enough-p session days))
          (dolist (pair (session-decisions id))
            (push (make-decision :prompt (first pair) :answer (second pair)
                                 :session-id id)
                  decisions)))))
    (let* ((all (nreverse decisions))
           (count (length all)))
      (if (<= count limit) all (last all limit)))))
