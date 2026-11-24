;;;; check-test.lisp --- a job's check decides whether a due instant is worth a turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: a check that exits 1 lets the instant pass with no
;;;; turn and no notice; one that exits 0 fires that instant, its output
;;;; riding as run context; any other exit, or the timeout, is a failed check
;;;; that pauses the job at the limit and tells its origin; a check that runs
;;;; again ends a run of failed checks; a check still running skips the next
;;;; instant; the check rides the registry row and (cron:edit), and
;;;; (cron:run) fires without it. The checks are real shell commands, run on
;;;; the check's own thread; CRON-CHECK-TICK waits for them and settles.

(in-package #:nodecode.test)

(defun cron-checks-ended-p ()
  (nodecode-cron::with-cron-lock
    (every #'nodecode-cron::check-run-status nodecode-cron::*checks*)))

(defun cron-await-checks (&optional (seconds 20))
  "Wait until every check started has ended; T when they did."
  (loop repeat (* 20 seconds)
        until (cron-checks-ended-p)
        do (sleep 0.05)
        finally (return (cron-checks-ended-p))))

(defun cron-check-tick (at)
  "Tick AT, which starts the checks due, wait for them, and tick a second
later to settle them; => the fires that settling started."
  (nodecode-cron::tick at)
  (cron-await-checks)
  (nodecode-cron::tick (1+ at)))

(defun cron-check-dir ()
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "cron-check-~36r/" (random (expt 36 8)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (namestring dir)))

(deftest cron-cell-a-check-that-finds-nothing-costs-no-turn ()
  (with-cron-runtime (:submits t)
    (let* ((job (cron-add "every 30m" "Report the board" :name "board" :check "exit 1"))
           (due (+ +cron-now+ 1800)))
      (is (= 0 (nodecode-cron::tick due)) "the due instant starts the check, not a turn")
      (is (= 1 (length nodecode-cron::*checks*)) "one check runs")
      (is (= (+ due 1800) (nodecode-cron::job-next-at job)) "the schedule moved on at once")
      (is (cron-await-checks) "the check ended")
      (is (= 0 (nodecode-cron::tick (1+ due))) "exit 1: nothing fires")
      (is (null nodecode-cron::*checks*) "and the check is settled")
      (is (null (cron-submits)) "no prompt reached the ingress")
      (is-shape (nodecode-cron::job-last job) (:status "quiet") (:line "the check found nothing to do"))
      (is (= 0 (nodecode-cron::job-fires job)) "a quiet check is no fire")
      (is (null (cron-board-text "board")) "and says nothing on the board")
      (is (search "last quiet 09-04 10:30" (cron:jobs))))))

(deftest cron-cell-a-check-that-finds-work-fires-that-instant-with-its-output ()
  (with-cron-runtime (:submits t)
    (let* ((job (cron-add "every 30m" "Report the board" :name "board"
                          :check "echo 'board done: 8 of 8 pass'"))
           (due (+ +cron-now+ 1800)))
      (is (= 1 (cron-check-tick due)) "exit 0: the job fires")
      (let ((submit (first (cron-submits))))
        (is-present submit "the fire reached the seam"
          (is-shape submit (:session "cron-board")
            (:command-id (format nil "cron:board:~d" (nodecode-cron::unix-from-universal due))
             "the command id names the instant the check let through"))
          (is (search "[cron \"board\", fire 1, 2026-09-04 10:30," (getf submit :prompt)))
          (is (search (format nil "Report the board~%~%Run context: the check `echo 'board done: ~
                                   8 of 8 pass'` exited 0; it printed:~%board done: 8 of 8 pass")
                      (getf submit :prompt)) "the check's output rides as run context")))
      (is (= (+ due 1800) (nodecode-cron::job-next-at job)) "advanced once, not twice")
      (is (= 1 (nodecode-cron::job-fires job)))
      (cron-finish "cron-board" "All 8 pass.")
      (nodecode-cron::tick (+ due 60))
      (is-shape (nodecode-cron::job-last job) (:status "ok") (:line "All 8 pass.")))))

(deftest cron-cell-a-failing-check-pauses-its-job-and-tells-its-origin-once ()
  (with-cron-runtime (:store t :submits t)
    (nlk:create-session :id "s-origin")
    (let ((job (cron-add "every 30m" "Report the board" :name "board" :origin "s-origin"
                         :check "echo 'grep: board.log: No such file' >&2; exit 2")))
      (cron-check-tick (+ +cron-now+ 1800))
      (cron-check-tick (+ +cron-now+ 3600))
      (is (eq :active (nodecode-cron::job-state job)) "two failed checks: it still runs")
      (is (= 2 (nodecode-cron::job-failures job)))
      (is (equal "cron board: check exited 2: grep: board.log: No such file"
                 (cron-board-text "board")))
      (is (eq :error (third (cell-notice "cron:board"))))
      (cron-check-tick (+ +cron-now+ 5400))
      (is (eq :paused (nodecode-cron::job-state job)) "the third pauses it")
      (is (equal "check failed" (getf (nodecode-cron::job-last job) :status)))
      (is (equal "cron board paused after 3 failed checks in a row: check exited 2: grep: board.log: No such file; (cron:resume \"board\") runs it again"
                 (cron-board-text "board")))
      (is (null (cron-submits-to "cron-board")) "no check let a turn through")
      (let ((told (cron-submits-to "s-origin")))
        (is (= 1 (length told)) "the origin is told once")
        (is (search "[cron \"board\" paused 2026-09-04 11:30 after 3 failed checks in a row, schedule every 30m; its session is cron-board]: The last one failed: check exited 2: grep: board.log: No such file. The job fires no more until (cron:resume \"board\")"
                    (getf (first told) :prompt))))
      (is (= 0 (cron-check-tick (+ +cron-now+ 7200))) "paused, it checks no more")
      (is (null nodecode-cron::*checks*)))))

(deftest cron-cell-a-check-past-its-timeout-is-killed-and-failed ()
  (with-cron-runtime (:submits t :config ("check_timeout_seconds" 1))
    (let ((job (cron-add "every 30m" "Report" :name "slow" :check "sleep 30"))
          (started (get-internal-real-time)))
      (cron-check-tick (+ +cron-now+ 1800))
      (is (< (- (get-internal-real-time) started) (* 10 internal-time-units-per-second)) "timed out")
      (is-shape (nodecode-cron::job-last job) (:status "check failed")
        (:line "check timed out after 1 s"))
      (is (= 1 (nodecode-cron::job-failures job)))
      (is (null (cron-submits))))))

(deftest cron-cell-a-check-that-runs-again-ends-a-run-of-failed-checks ()
  (with-cron-runtime (:submits t)
    (let* ((dir (cron-check-dir))
           (code (merge-pathnames "code" dir))
           (job (cron-add "every 30m" "Report" :name "flaky" :cwd dir :check "exit $(cat code)")))
      (alexandria:write-string-into-file "2" code :if-exists :supersede)
      (cron-check-tick (+ +cron-now+ 1800))
      (is (= 1 (nodecode-cron::job-failures job)) "the check ran in the job's cwd and failed")
      (is (search "exited 2" (cron-board-text "flaky")))
      (alexandria:write-string-into-file "1" code :if-exists :supersede)
      (cron-check-tick (+ +cron-now+ 3600))
      (is (= 0 (nodecode-cron::job-failures job)) "a quiet check ends the run")
      (is (null (cron-board-text "flaky")) "and takes the failure off the board")
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir) :validate t))))

(deftest cron-cell-a-quiet-check-leaves-a-run-of-failed-fires-standing ()
  ;; A fire the provider refused is not cured by a check that finds nothing:
  ;; the run goes on until a fire answers.
  (with-cron-runtime (:submits t)
    (let* ((dir (cron-check-dir))
           (code (merge-pathnames "code" dir))
           (job (cron-add "every 30m" "Report" :name "gated" :cwd dir :check "exit $(cat code)")))
      (alexandria:write-string-into-file "0" code :if-exists :supersede)
      (cron-check-tick (+ +cron-now+ 1800))
      (cron-publish "turn.failed" "cron-gated" :payload (nlk:json-object "detail" "refused"))
      (nodecode-cron::tick (+ +cron-now+ 1805))
      (is (= 1 (nodecode-cron::job-failures job)) "the fire failed")
      (alexandria:write-string-into-file "1" code :if-exists :supersede)
      (cron-check-tick (+ +cron-now+ 3600))
      (is (= 1 (nodecode-cron::job-failures job)) "a quiet check is no answer")
      (is (equal "cron gated failed: refused" (cron-board-text "gated")))
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir) :validate t))))

(deftest cron-cell-a-check-still-running-skips-the-next-instant ()
  (with-cron-runtime (:submits t)
    (let ((job (cron-add "every 30m" "Report" :name "long" :check "sleep 2; echo done")))
      (nodecode-cron::tick (+ +cron-now+ 1800))
      (nodecode-cron::tick (+ +cron-now+ 3600))
      (is-shape (nodecode-cron::job-last job) (:status "overrun") (:line "previous check still running"))
      (is (= 1 (length nodecode-cron::*checks*)) "no second check beside the first")
      (is (cron-await-checks))
      (is (= 1 (nodecode-cron::tick (+ +cron-now+ 3601))) "the first check's instant fires")
      (is (equal (format nil "cron:long:~d" (nodecode-cron::unix-from-universal (+ +cron-now+ 1800)))
                 (getf (first (cron-submits)) :command-id))))))

(deftest cron-cell-a-job-paused-or-rechecked-while-its-check-ran-takes-nothing-from-it ()
  (with-cron-runtime (:submits t)
    (cron-add "every 30m" "Report" :name "held" :check "echo work")
    (cron-add "every 30m" "Report" :name "moved" :check "echo work")
    (nodecode-cron::tick (+ +cron-now+ 1800))
    (cron:pause "held")
    (cron:edit "moved" :check "exit 1")
    (cron-await-checks)
    (is (= 0 (nodecode-cron::tick (+ +cron-now+ 1801))) "neither fires")
    (is (null (cron-submits)))
    (is (null nodecode-cron::*checks*) "both checks are settled")))

(deftest cron-cell-a-check-rides-the-row-edit-show-and-run ()
  (with-cron-runtime (:store t :submits t)
    (cron-add "every 30m" "Report" :name "board" :check "grep -q 'board done' run.log")
    (setf nodecode-cron::*jobs* '())
    (nodecode-cron::load-jobs)
    (let ((back (nodecode-cron::find-job "board")))
      (is (equal "grep -q 'board done' run.log" (nodecode-cron::job-check back)) "the row carries it"))
    (is (search "check: grep -q 'board done' run.log" (cron:show "board")))
    (is (search "job board fired into session cron-board" (cron:run "board")) "no check on a run")
    (is (= 1 (length (cron-submits-to "cron-board"))))
    (is (null nodecode-cron::*checks*) "and started none")
    (cron:edit "board" :check nil)
    (is (null (nodecode-cron::job-check (nodecode-cron::find-job "board"))) ":check nil drops it")
    (is (search "check: none: every fire runs" (cron:show "board")))
    (cron:edit "board" :prompt "Report again")
    (is (null (nodecode-cron::job-check (nodecode-cron::find-job "board"))) "an edit without :check keeps it as it is")
    (is (search "gated by its check" (cron:add "every 1h" "x" :name "g" :check "true")))
    (is (search ":check must be a shell command" (refusal-text cron:cron-error (cron:add "1h" "x" :check 42))))
    (is (search ":check must be a shell command" (refusal-text cron:cron-error (cron:add "1h" "x" :check "  "))))))
