;;;; gate.lisp --- idle, and one cycle at a time.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The wake job already holds the clock, the money and the operator's stop
;;;; on wakes. What a self-fix cycle adds is that it runs only when nobody is
;;;; using the home: no turn ran in any session of it for IDLE_MINUTES, and
;;;; none is still open. Ouroboros starts an evolution cycle only when its
;;;; queue has nothing pending or running (enqueue_evolution_task_if_needed).
;;;; The wake's own session and forks do not count; a cron job's do, since
;;;; it spends the same model.
;;;;
;;;; The lock is a file made with O_EXCL, so two processes serving one home
;;;; run one cycle between them. A lock older than *LOCK-STALE* belonged to
;;;; a process that died mid-cycle and is taken over.

(in-package #:nodecode-selffix)

(defparameter *open-turn-window* (* 6 3600)
  "How far back a turn that started and never ended still counts as running.")

(defparameter *lock-stale* (* 2 3600)
  "Seconds after which a cycle lock is taken over.")

(defparameter +activity-kinds+
  '("turn.started" "turn.tool_call_started" "turn.tool_result" "turn.completed" "turn.failed" "turn.cancelled")
  "The event kinds that show a turn at work.")

(defparameter +turn-end-kinds+ '("turn.completed" "turn.failed" "turn.cancelled"))

(defun counted-session-p (session)
  "Whether SESSION's turns keep the home busy: not the wake's, not a fork."
  (and (stringp session)
       (not (nodecode-cron::wake-session-p session))
       (not (nlk:fork-session-p session))))

(defun busy-from (recent started ended now &key (counted #'counted-session-p))
  "Why the home is busy at NOW, or NIL. RECENT is (SESSION LAST-ISO) rows of
activity inside the idle window, STARTED (SESSION TURN-ID) rows inside the
open-turn window, ENDED the turn ids among them that ended."
  (or (loop for (session last) in recent
            when (funcall counted session)
              return (format nil "a turn in ~a was at work ~ds ago" session
                             (max 0 (- now (or (nlk:iso-universal last) now)))))
      (loop for (session turn) in started
            when (and (funcall counted session) (not (member turn ended :test #'equal)))
              return (format nil "turn ~a in ~a is still running" turn session))))

(defun busy-reason (now idle-seconds)
  "Why the home is not idle at NOW, read off the event log, or NIL."
  (let ((recent (nlk:events :kind +activity-kinds+
                            :where (list "occurred_at >= ?" (iso (- now idle-seconds)))
                            :as :rows :columns '("session_id" "max(occurred_at)") :group-by '(1)))
        (window (list "occurred_at >= ?" (iso (- now *open-turn-window*)))))
    (busy-from recent
               (nlk:events :kind '("turn.started") :where window :as :rows
                           :columns '("session_id" "turn_id"))
               (mapcar #'first (nlk:events :kind +turn-end-kinds+ :where window :as :rows
                                           :columns '("turn_id")))
               now)))

;;; --- the lock ---------------------------------------------------------------------

(defun lock-path () (selffix-path "cycle.lock"))

(defun take-lock (now)
  "Whether this process now holds the cycle lock."
  (let ((path (lock-path)))
    (ensure-directories-exist path)
    (let ((written (ignore-errors (file-write-date path))))
      (when (and written (> (- now written) *lock-stale*))
        (ignore-errors (delete-file path))))
    (handler-case
        (let ((fd (sb-posix:open (namestring path)
                                 (logior sb-posix:o-creat sb-posix:o-excl sb-posix:o-wronly) #o644)))
          (sb-posix:close fd)
          (with-open-file (out path :direction :output :if-exists :append)
            (format out "~a~%" (iso now)))
          t)
      (sb-posix:syscall-error () nil))))

(defun drop-lock ()
  (ignore-errors (delete-file (lock-path))))
