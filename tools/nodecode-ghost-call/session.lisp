;;;; session.lisp --- the one open ghost call, in memory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept sessions in a SQLite file its bridge and the plugin
;;;; shared, because two processes served one call: a FastAPI bridge carried
;;;; the audio and the plugin posted the ticker. One process serves this

;;;; rewrite, so the call is a struct and an operator coaches one call at a
;;;; time -- the original refused a second live session for the same reason.

(in-package #:nodecode-ghost-call)

(defparameter +turns-kept+ 200
  "How many lines one call keeps: enough for a summary, bounded.")

(defvar *clock* #'get-universal-time
  "The cell's clock, in whole seconds of universal time. A test binds it to
something that moves faster than a phone call does.")

(defvar *session* nil
  "The open ghost call, or NIL between calls.")

(defun now ()
  "The cell's clock, in seconds."
  (funcall *clock*))

(defun make-id ()
  "A short session id, as the original's g-prefixed handle was."
  (format nil "g~(~6,'0x~)" (random #x1000000)))

(defstruct (ghost-session (:constructor make-session*))
  "One call being coached."
  (id (make-id) :read-only t)
  (goal "" :read-only t)
  (callee-label "" :read-only t)
  (target "" :read-only t)
  (user-name "" :read-only t)
  (state :live)
  (opened-at (now) :read-only t)
  (ended-at nil)
  (end-reason "")
  (turns '())
  (hints '())
  (hint-count 0)
  (last-hint-at nil)
  (intervene-allowed nil)
  (disclosed nil)
  (summary nil))

(defun stamp (seconds)
  "SECONDS as m:ss, the way a hint is stamped on the ticker."
  (let ((whole (max 0 (round (or seconds 0)))))
    (format nil "~d:~2,'0d" (floor whole 60) (mod whole 60))))

(defun elapsed (session)
  "Seconds since the call opened."
  (max 0 (- (or (ghost-session-ended-at session) (now))
            (ghost-session-opened-at session))))

(defun ended-p (session)
  "Has SESSION stopped listening?"
  (eq (ghost-session-state session) :ended))

(defun live-p (session)
  "Is SESSION still inside its duration cap?"
  (and session (not (ended-p session))
       (< (elapsed session) (setting :max-duration-s))))

(defun current-session ()
  "The session this call is about, or NIL."
  (let ((session *session*))
    (when (and session (not (ended-p session)) (not (live-p session)))
      (end-session session "duration cap"))
    (if (and session (ended-p session)) nil session)))

(defun active-session ()
  "The open session, or a refusal the model can read."
  (running-settings)
  (or (current-session)
      (fail "no ghost call is open: call ghost_start with the operator's goal first")))

(defun open-session (&key goal callee-label target user-name)
  "Open a call with GOAL. Refuses while another one is still open, as the
original refused a second live session."
  (running-settings)
  (when (current-session)
    (fail "ghost call ~a is still open; end it with ghost_hangup first"
          (ghost-session-id (current-session))))
  (when (zerop (length (nlk:trimmed (or goal ""))))
    (fail "a goal is required: what the operator wants out of this call"))
  (setf *session* (make-session* :goal (nlk:trimmed goal)
                                  :callee-label (nlk:trimmed (or callee-label ""))
                                  :target (nlk:trimmed (or target ""))
                                  :user-name (nlk:trimmed (or user-name
                                                              (setting :user-name) ""))))
  *session*)

(defun end-session (session reason)
  "Stop listening, once, and say why."
  (unless (ended-p session)
    (setf (ghost-session-state session) :ended
          (ghost-session-ended-at session) (now)
          (ghost-session-end-reason session) (or reason "")))
  session)

(defun record-turn (session speaker text)
  "Keep one line of the call, newest first."
  (push (list :speaker speaker :text text :at (elapsed session))
        (ghost-session-turns session))
  (when (> (length (ghost-session-turns session)) +turns-kept+)
    (setf (ghost-session-turns session)
          (subseq (ghost-session-turns session) 0 +turns-kept+)))
  session)

(defun transcript-turns (session)
  "The kept lines, oldest first: what a prompt reads."
  (reverse (ghost-session-turns session)))

(defun record-hint (session text)
  "Keep HINT — one clean line — and count it."
  (let ((session (or session (active-session))))
    (incf (ghost-session-hint-count session))
    (push (list :n (ghost-session-hint-count session) :text text :at (elapsed session))
          (ghost-session-hints session))
    (setf (ghost-session-last-hint-at session) (now))
    session))

(defun hint-lines (session)
  "The hints of SESSION, newest first, as text."
  (mapcar (lambda (hint) (getf hint :text)) (ghost-session-hints session)))

(defun interval-s ()
  "The floor the section asks for, never under the original's four seconds."
  (max +min-interval-floor-s+ (setting :min-hint-interval-s)))

(defun hints-left (session)
  "How many hints this call may still give."
  (max 0 (- (min +max-hints-ceiling+ (setting :max-hints))
            (ghost-session-hint-count session))))

(defun wait-s (session)
  "Seconds until the next hint may go out, 0 when it may go now."
  (let ((last (ghost-session-last-hint-at session)))
    (if last (max 0 (- (+ last (interval-s)) (now))) 0)))

(defun allow-hint-p (session)
  "May a hint go out now?"
  (and (plusp (hints-left session)) (zerop (wait-s session))))

(defun status-line (session)
  "One line about where this call stands, for a tool answer."
  (format nil "~a · ~a in · ~d/~d hints~@[ · ~d s until the next one~]~@[ · intervene allowed~]"
          (if (ended-p session) "ended" (if (live-p session) "live" "expired"))
          (stamp (elapsed session))
          (ghost-session-hint-count session)
          (min +max-hints-ceiling+ (setting :max-hints))
          (let ((wait (wait-s session))) (and (plusp wait) (ceiling wait)))
          (ghost-session-intervene-allowed session)))

(defun status-text (&optional (session *session*))
  "What /ghost status answers."
  (if (null session)
      "ghost: no call yet. Start one with /ghost start <goal>."
      (format nil "ghost ~a ~a~%goal: ~a~@[ for ~a~]~@[ · private target: ~a~]~%\
~d hint~:p so far; the last: ~a"
              (ghost-session-id session) (status-line session)
              (ghost-session-goal session)
              (let ((label (ghost-session-callee-label session)))
                (and (plusp (length label)) label))
              (let ((target (ghost-session-target session)))
                (and (plusp (length target)) target))
              (ghost-session-hint-count session)
              (or (first (hint-lines session)) "(none)"))))
