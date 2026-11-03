;;;; cell.lisp --- the verb, /selffix, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A present "selffix" section turns the loop on, and it then runs only on
;;;; the cron wake's clock: (cron:wake :on) first. (selffix:selffix :off) is
;;;; the operator's stop: it is stored in the ledger and holds across
;;;; restarts, and only (selffix:selffix :on) from a session that is not a
;;;; cron job's lifts it, so no wake and no job can turn the loop back on.

(in-package #:nodecode-selffix)

(defun job-session-p (session)
  "Whether SESSION is a cron job's own, the wake's included."
  (and (stringp session) (nodecode-cron::own-session-p session)))

(defun calling-session () (nodecode-cron::calling-session))

(defun status-text (now)
  (let* ((rows (ledger-rows))
         (stop (stopped rows))
         (pause (breaker rows))
         (busy (ignore-errors (funcall *busy* now (* 60 (setting :idle-minutes)))))
         (found (findings rows)))
    (format nil "selffix (observe): ~a~%idle gate: ~:[idle~;~:*busy, ~a~]~%~d finding~:p~@[, the newest ~a~]~%~
                 rides the cron wake: ~a"
            (cond (stop (format nil "stopped at ~a~@[ by ~a~]" (row stop "at") (row stop "by")))
                  (pause (format nil "paused, ~a" pause))
                  (t "on"))
            busy (length found) (and found (row (car (last found)) "fp"))
            (nlk:first-line (nodecode-cron:wake :status) 120))))

(defun findings-text ()
  (let ((found (findings (ledger-rows))))
    (if (null found)
        "no findings yet"
        (format nil "~{~a~^~%~}"
                (loop for r in found
                      for fp = (row r "fp")
                      for dir = (finding-dir fp)
                      for test = (find-if (lambda (p) (not (equal (file-namestring p) "finding.md")))
                                          (uiop:directory-files dir))
                      collect (format nil "~a ~a: ~a [~a]" (row r "at") fp (row r "key")
                                      (cond ((null test) "test missing")
                                            ((equal (file-sha256 test) (row r "sha256")) "test locked")
                                            (t "test changed"))))))))

(defun selffix (&optional (action :status))
  "The loop's verb: :STATUS, :FINDINGS, :NOW (one cycle, gates held), :OFF, :ON."
  (let ((now (get-universal-time))
        (session (calling-session)))
    (ecase action
      (:status (status-text now))
      (:findings (findings-text))
      (:now (multiple-value-bind (status detail) (cycle :now now)
              (when (eq status :finding) (post-findings))
              (format nil "~(~a~): ~a" status detail)))
      (:off (record "stopped" :by session :at now)
            (nle:notice nil :key "nodecode-selffix")
            "selffix stopped. It stays stopped across restarts; only (selffix:selffix :on), from a session that is not a cron job's, starts it again.")
      (:on (when (job-session-p session)
             (fail "a cron job's session may not start selffix; the operator does"))
           (record "started" :by session :at now)
           (post-findings)
           "selffix on: it runs one cycle at a due cron wake, when the home is idle."))))

(defparameter +usage+ "/selffix [status | findings | now | on | off]")

(defun selffix-slash (args session-id)
  (declare (ignore session-id))
  (let ((head (string-downcase (or (first (nlk:split-words (or args ""))) "status"))))
    (handler-case
        (cond ((member head '("status" "findings" "now" "on" "off") :test #'equal)
               (selffix (intern (string-upcase head) :keyword)))
              (t +usage+))
      (error (condition) (format nil "ERROR: ~a" condition)))))

(nle:define-cell selffix
  (:section ("selffix")
    (:guide "idle self-fixes, observe level: on the cron wake's clock, when the home is idle, a failure that recurs becomes a finding with a failing test; no code changes")
    ("idle_minutes" :integer :default 30 :min 5 :doc "minutes with no turn in any session before a cycle may run"))
  (:command "selffix" 'selffix-slash
            :description "Idle self-fixes: status, findings, one cycle now, on, off"
            :argument-hint "[status|findings|now|on|off]")
  (:hook 'nle::agent-loop #'note-frame)
  (:hook 'nlk:fail-turn #'capture-failure)
  (:start (lambda ()
            (pushnew 'wake-source nodecode-cron::*wake-sources*)
            (nle:on-stop (lambda ()
                           (setf nodecode-cron::*wake-sources*
                                 (remove 'wake-source nodecode-cron::*wake-sources*))))
            (post-findings)
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-selffix"))))))
