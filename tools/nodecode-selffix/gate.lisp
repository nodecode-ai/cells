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
;;;; The lock is an fcntl write lock on selffix/cycle.lock, held on an open
;;;; descriptor while a cycle runs, so two processes serving one home run one
;;;; cycle between them. The kernel drops it when its process dies, so a cycle
;;;; that died mid-run frees it at once, and no age decides a takeover: two
;;;; processes taking over one stale lock file by delete and create could both
;;;; win. The file is never deleted either, since a process that opened it
;;;; before a delete would lock a file the next one no longer sees. fcntl does
;;;; not keep a process from itself, so a table under a mutex keeps a second
;;;; taker in this one out. Windows has no fcntl lock, and no bwrap to seal a
;;;; repro with, so there the lock is this process's alone.

(in-package #:nodecode-selffix)

(defparameter *open-turn-window* (* 6 3600)
  "How far back a turn that started and never ended still counts as running.")

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

(defvar *held* (make-hash-table :test 'equal)
  "The descriptors this process holds a cycle lock on, by lock file name.")

(defvar *held-mutex* (sb-thread:make-mutex :name "selffix cycle lock"))

(defun locked-fd (name)
  "NAME opened and write-locked without waiting => the descriptor, or NIL
while another process holds it."
  (let ((fd (sb-posix:open name (logior sb-posix:o-creat sb-posix:o-wronly) #o644)))
    (handler-case
        (progn
          #-win32 (sb-posix:fcntl fd sb-posix:f-setlk
                                  (make-instance 'sb-posix:flock :type sb-posix:f-wrlck
                                                                 :whence sb-posix:seek-set :start 0 :len 0))
          fd)
      (sb-posix:syscall-error ()
        (sb-posix:close fd)
        nil))))

(defun take-lock ()
  "Whether this process now holds the cycle lock."
  (let ((name (namestring (ensure-directories-exist (lock-path)))))
    (sb-thread:with-mutex (*held-mutex*)
      (unless (gethash name *held*)
        (nlk:when-let (fd (locked-fd name))
          (setf (gethash name *held*) fd)
          t)))))

(defun drop-lock ()
  "Let the cycle lock go: closing its descriptor drops the fcntl lock."
  (let ((name (namestring (lock-path))))
    (sb-thread:with-mutex (*held-mutex*)
      (nlk:when-let (fd (gethash name *held*))
        (remhash name *held*)
        (ignore-errors (sb-posix:close fd))))))
