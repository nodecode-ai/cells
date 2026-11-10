;;;; state.lisp --- what a flight's row says it is.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's analysis.py: pure classification, no clock
;;;; of its own (every function takes NOW), so a test drives time and reads
;;;; the state rather than waiting for one.

(in-package #:nodecode-muster)

(defparameter +live-states+ '("RUNNING" "STALLED")
  "The states a flight may still be in: working, or silent too long.")

(defparameter +terminal-states+ '("SUCCEEDED" "FAILED" "INTERRUPTED" "CANCELLED" "LOST")
  "The states a flight has stopped in: done, failed, cut short, stopped on
purpose, or gone with the image that ran it.")

(defparameter +status-states+
  '(("completed" . "SUCCEEDED") ("succeeded" . "SUCCEEDED") ("ok" . "SUCCEEDED")
    ("running" . "RUNNING")
    ("interrupted" . "INTERRUPTED") ("cancelled" . "CANCELLED") ("canceled" . "CANCELLED")
    ("failed" . "FAILED") ("error" . "FAILED") ("timeout" . "FAILED")
    ("rate_limited" . "FAILED"))
  "What a fork's own status word maps to. A word missing here says nothing,
and the row keeps the state it had.")

(defun status-state (status)
  "The state STATUS names, or NIL when it names none muster reads."
  (and (stringp status)
       (let ((word (string-downcase (string-trim " " status))))
         (when (plusp (length word))
           (cdr (assoc word +status-states+ :test #'string=))))))

(defun live-state-p (state)
  (and (stringp state) (member state +live-states+ :test #'string=) t))

(defun terminal-state-p (state)
  (and (stringp state) (member state +terminal-states+ :test #'string=) t))

(defun failed-state-p (state)
  "A state a re-run is worth: the flight did not finish its work."
  (and (stringp state) (member state '("FAILED" "INTERRUPTED" "LOST") :test #'string=) t))

(defun stalled-p (flight now stall-seconds)
  "A working flight whose last frame is older than STALL-SECONDS is stuck. A
stalled row that hears from its fork again is working again: the badge never
sticks."
  (and (equal (getf flight :state) "RUNNING")
       (let ((last (or (getf flight :last-seen) (getf flight :started) 0)))
         (> (- now last) stall-seconds))))

(defun stall-recovered-p (flight now stall-seconds)
  (and (equal (getf flight :state) "STALLED")
       (let ((last (or (getf flight :last-seen) 0)))
         (<= (- now last) stall-seconds))))

(defun event-age (flight now)
  "Seconds since the flight was last heard from, or NIL."
  (let ((last (or (getf flight :last-seen) (getf flight :started))))
    (and last (max 0 (- now last)))))

(defun failures-in-window (flight flights now window)
  "How many flights of the same job ended badly within WINDOW."
  (let ((label (getf flight :label))
        (ended (or (getf flight :ended) now)))
    (when (and (stringp label) (plusp (length label)))
      (loop for other in flights
            count (and (equal label (getf other :label))
                       (failed-state-p (getf other :state))
                       (let ((at (getf other :ended)))
                         (and at (<= 0 (- ended at) window))))))))

(defun spinning-p (flight flights now window threshold)
  "FAILED on repeat: the same job failed THRESHOLD times inside WINDOW."
  (and (equal (getf flight :state) "FAILED")
       (plusp threshold)
       (>= (failures-in-window flight flights now window) threshold)))
