;;;; package.lisp --- the package, the prompts, and the clock the rule reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches outside the image: the check-in rule is arithmetic
;;;; on the clock, the prompts are data a layer can reword, and the state is
;;;; one readable file under the home. A check-in is a prompt the cell puts
;;;; into a session of the operator's own, so the answer goes wherever that
;;;; session's chats go -- Telegram, Discord, a browser tab -- with no wire of
;;;; this cell's own.

(in-package #:cl-user)

(defpackage #:nodecode-checkin
  (:use #:cl)
  (:export #:start-cell #:stop-cell))

(in-package #:nodecode-checkin)

;;; --- what a test, or a layer, rebinds -----------------------------------------

(defvar *clock* (lambda () (get-universal-time))
  "The clock the rule reads, in universal time. A test rebinds it.")

(defvar *state-file* nil
  "Where the check-in state lives; NIL reads as checkins/state.sexp under the home.")

(defvar *state* '()
  "The state in memory, a plist: :last-activity, :last-user, :last-nudge,
:nudges, :last-brief, :bound-at, :last-result. Guarded by *STATE-LOCK*.")

(defvar *state-lock* (bt2:make-lock :name "checkins-state")
  "One writer at a time: the worker and a :FRAME hook both touch the state.")

(defvar *our-turn* nil
  "The turn id of the check-in the cell put in, so its own answer's frames
are not read as the operator speaking again.")

(defvar *worker* nil
  "The ticker while it runs, or NIL.")

;;; --- the time the operator keeps ----------------------------------------------

(defun local-hour (utc offset-minutes)
  "The operator's clock hour at UTC, given their offset east of it."
  (mod (+ (floor utc 3600) (floor offset-minutes 60)) 24))

(defun local-midnight (utc offset-minutes)
  "The universal time their local day started at."
  (- (* (floor utc 86400) 86400) (* 60 offset-minutes)))

(defun in-window-p (start end hour)
  "Whether HOUR is inside the waking hours START..END, which may wrap midnight."
  (if (< start end)
      (and (<= start hour) (< hour end))
      (or (>= hour start) (< hour end))))

(defun window-hours (start end)
  "The local hours a check-in may fire in, as a list: 8 22 => (8 .. 21)."
  (if (< start end)
      (loop for h from start below end collect h)
      (append (loop for h from start below 24 collect h)
              (loop for h from 0 below end collect h))))

(defun next-active (utc offset-minutes start end)
  "UTC itself when it is inside their hours, else their next active_start."
  (if (in-window-p start end (local-hour utc offset-minutes))
      utc
      (let ((midnight (local-midnight utc offset-minutes)))
        (or (loop for day from 0 to 2
                  for candidate = (+ midnight (* day 86400) (* start 3600))
                  when (> candidate utc) return candidate)
            (+ utc (* 86400 1))))))

(defun wait-seconds (base-minutes nudges)
  "The quiet the rule demands: the base, doubled per unanswered check-in,
capped at a week."
  (min (* base-minutes 60 (expt 2 (min nudges 16))) (* 7 86400)))

(defun brief-at (utc offset-minutes start)
  "The local day's brief time: active_start plus 0-20 minutes fixed per day,
so a restart and a status call agree with the timer."
  (let* ((midnight (local-midnight utc offset-minutes))
         (jitter (mod (loop for ch across (prin1-to-string (floor midnight 86400))
                            sum (char-code ch))
                      21)))
    (+ midnight (* start 3600) (* jitter 60))))

(defun ago-text (seconds)
  "How long, said the way a person says it: minutes, hours or days."
  (let ((hours (/ seconds 3600.0)))
    (cond ((< hours 1.5) (format nil "~d minutes" (round (/ seconds 60))))
          ((< hours 48) (format nil "~d hours" (round hours)))
          (t (format nil "~d days" (round (/ hours 24)))))))

;;; --- how a check-in sounds ----------------------------------------------------

(defparameter *marker* "[Check-in]"
  "The line every check-in prompt opens with, so a turn can be told from the
operator's own words.")

(defparameter *brief-marker* "[Morning brief]"
  "The line every morning brief opens with.")

(defparameter *voice*
  "Write like a sharp, warm friend texting them. Plain words and contractions; skip
greetings, preambles and filler; no bullet lists, and a numbered list only for decisions you
need from them. Lead with the concrete status, then a caveat. Never mention timers,
schedules, check-ins or being prompted, and open differently from your recent messages."
  "The style a check-in and the brief both carry, shared with the voice note.")

(defparameter *safety*
  "This is not new authorization: never send external messages, spend money, change credentials
or permissions, touch production, or delete anything without asking first."
  "What a check-in may not assume it was allowed to do.")

(defparameter *check-in-prompt*
  "~a It has been ~a since your last exchange with the operator, and it is ~a for them.
Pick exactly one: (a) do one safe, reversible thing that moves their goals forward, then say
what you did; (b) offer one specific thing you could do for them; or (c) ask about a decision
they own. Build it around one specific open thread from your memory of their goals and recent
conversations, and name it. ~a Keep it to 1-3 short sentences. ~a ~a"
  "The prompt a due check-in puts into the session: the marker, how long it has
been, their clock, the rule's three moves, the voice, the length, the safety
line, and how silence is said.")

(defparameter *brief-prompt*
  "~a The operator's day is starting; it is ~a for them. Send a short morning brief with only
what applies: what you did or learned since your last exchange; anything time-sensitive
today; anything you tried that failed, and why; and up to 3 decisions you need from them as
numbered questions. Name specific open threads from your memory. ~a Keep it to a few short
lines. ~a ~a"
  "The prompt the morning brief puts into the session.")

(defparameter *silent* " If nothing is worth their attention, reply with exactly [SILENT] and nothing else."
  "How a check-in says nothing: the answer is not delivered.")

(defparameter *asked* " They asked, so answer; don't reply [SILENT]."
  "How a check-in the operator asked for differs.")

(defparameter *voice-note*
  "[Texting voice] Write like a sharp, warm friend texting them. Plain words and contractions;
skip greetings, preambles and filler such as \"Certainly!\" or \"Great question\". Lead with the
concrete status, then the caveat. Use task words, never tool names, file paths or ids unless
they ask for them. Plain text or light formatting, never headings or tables. Casual replies
and recaps: at most 3 short sentences, including any offer of detail; no lists unless asked.
Ask only when the operator owns the decision, then stop. If their own instructions ask for a
different style, theirs wins."
  "What every chat turn carries while the texting voice is on: the original
plugin's note, shortened, and the same rules its check-ins use.")
