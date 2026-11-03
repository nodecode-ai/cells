;;;; wake-digest.lisp --- what changed inside the image since a wake's cursor, read at $0.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The inward half of a wake's facts, beside wake-sources.lisp's outward
;;;; one. Two sources on *WAKE-SOURCES*:
;;;;
;;;;   WAKE-SOURCE-TURNS  the sessions whose turns ended in (SINCE, now]: how
;;;;                      many, how many failed or were stopped, how long ago
;;;;                      the last one ended -- newest first
;;;;   WAKE-SOURCE-GOALS  the goals that ended in the window: how, by whom,
;;;;                      and on what condition
;;;;
;;;; Each is one read of the event log (a grouped count of turn endings, a
;;;; scan of goal.ended), so a wake with nothing new costs two queries and no
;;;; model call. A session that is a cron job's own -- the wake's included --
;;;; is left out (its outcome is the cron source's, and a wake must not wake
;;;; itself), and so is a fork FORK made: its turns ran inside a turn this
;;;; source already counts. Like the outward sources, a fact shows once: only
;;;; what crossed into the window is read, so a cursor that moves on admission
;;;; never repeats a line and a skipped tick loses none.
;;;;
;;;; Each source is a reader and a pure LINES function; the tests drive the
;;;; pure halves with rows of the shapes the readers return, and the readers
;;;; against a store with a real turn and a real goal in it.

(in-package #:nodecode-cron)

(defparameter +turn-end-kinds+ '("turn.completed" "turn.failed" "turn.cancelled")
  "The event kinds that end a turn: done, failed, stopped.")

(defun window-where (since now)
  "The event-log predicate for (SINCE, NOW], two universal times, in whole
seconds as IN-WINDOW-P reads them: a fact stamped 12:00:00.400 falls in the
second 12:00:00, so consecutive windows share no fact and drop none."
  (flet ((iso (universal) (nlk:iso-time (- universal nlk:+unix-epoch+))))
    (list "occurred_at >= ? and occurred_at < ?" (iso (1+ since)) (iso (1+ now)))))

(defun clean-title (title)
  "TITLE without the adapter's [m.. u.. r..] bracket a channel session's
first line carries, on one line."
  (string-trim " :" (nlk:one-line (ppcre:regex-replace-all "\\[m\\d+(?: [ur]\\d*)*\\]?:?" (or title "") "")
                                  :cap 80)))

(defun session-namer ()
  "A function from a session id to its cleaned title and the id -- the handle a
wake reads the session's log by -- or the id alone when it has no title."
  (let ((titles (make-hash-table :test #'equal)))
    (dolist (session (nlk:list-sessions))
      (setf (gethash (getf session :id) titles) (getf session :title)))
    (lambda (id)
      (let ((title (clean-title (gethash id titles))))
        (if (plusp (length title)) (format nil "~a (~a)" title id) id)))))

(defun counted-session-p (session-id)
  "Whether SESSION-ID's turns are news to a wake: not a cron job's own session,
not a fork."
  (not (or (own-session-p session-id) (nlk:fork-session-p session-id))))

;;; --- turns that ended -------------------------------------------------------------

(defun turn-lines (rows now &key (name #'identity))
  "One line per counted session among ROWS -- (SESSION-ID KIND COUNT LAST-ISO),
the grouped read -- newest ending first. NAME turns a session id into what the
line calls it."
  (let ((sessions (make-hash-table :test #'equal)))
    (loop for (session kind count last) in rows
          when (counted-session-p session)
            do (let ((entry (or (gethash session sessions)
                                (setf (gethash session sessions) (list 0 0 0 nil))))
                     (at (nlk:iso-universal last)))
                 (incf (first entry) count)
                 (cond ((equal kind "turn.failed") (incf (second entry) count))
                       ((equal kind "turn.cancelled") (incf (third entry) count)))
                 (when (and at (or (null (fourth entry)) (> at (fourth entry))))
                   (setf (fourth entry) at))))
    (loop for (session ended failed stopped last)
            in (sort (loop for session being the hash-keys of sessions using (hash-value entry)
                           collect (cons session entry))
                     #'> :key (lambda (entry) (or (fifth entry) 0)))
          collect (format nil "turns in ~a: ~d ended~@[, ~d failed~]~@[, ~d stopped~]~@[, the last ~a ago~]"
                          (funcall name session) ended
                          (and (plusp failed) failed) (and (plusp stopped) stopped)
                          (and last (age-text (max 0 (- now last))))))))

(defun wake-source-turns (since)
  "The sessions whose turns ended since SINCE, and how."
  (let* ((now (get-universal-time))
         (rows (nlk:events :kind +turn-end-kinds+ :where (window-where since now)
                           :as :rows :columns '("session_id" "kind" "count(*)" "max(occurred_at)")
                           :group-by '(1 2))))
    (and rows (cap-lines (turn-lines rows now :name (session-namer))))))

;;; --- goals that ended -------------------------------------------------------------

(defun goal-lines (rows &key (name #'identity) (condition (constantly nil)))
  "One line per goal.ended row in ROWS -- (SESSION-ID PAYLOAD-JSON) -- in a
counted session. CONDITION reads a goal id's condition, or NIL."
  (loop for (session payload) in rows
        for object = (nlk:decode-json payload)
        for goal = (nlk:json-value object :string "goal")
        when (counted-session-p session)
          collect (format nil "goal ~a~@[ by ~a~] in ~a~@[: ~a~]~@[ -- ~a~]"
                          (or (nlk:json-value object :string "reason") "ended")
                          (nlk:json-value object :string "by")
                          (funcall name session)
                          (and goal (funcall condition goal))
                          (let ((detail (nlk:json-value object :string "detail")))
                            (and (plusp (length detail)) detail)))))

(defun goal-condition (goal)
  "Goal GOAL's condition, from its goal.set fact, or NIL."
  (let ((set (nlk:events :aggregate-type "goal" :aggregate-id goal :kind "goal.set"
                         :limit 1 :as :payload)))
    (and set (nlk:json-value set :string "condition"))))

(defun wake-source-goals (since)
  "The goals that ended since SINCE: how, by whom, on what."
  (let ((rows (nlk:events :kind "goal.ended" :where (window-where since (get-universal-time))
                          :as :rows :columns '("session_id" "payload"))))
    (and rows (cap-lines (goal-lines rows :name (session-namer) :condition #'goal-condition)))))

(dolist (source '(wake-source-turns wake-source-goals))
  (pushnew source *wake-sources*))
