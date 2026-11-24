;;;; cell-test.lisp --- lifecycle; the tool through the wire; the reports; the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every answer here comes through the seam a model or a shell would use:
;;;; EXECUTE-WIRE-CALL dispatches pocket_watch exactly as a provider's tool
;;;; call does, and the verbs are the same functions /pocket-watch runs.
;;;; Nothing dials anything: the jobs are data, and the only file a test
;;;; writes is a jobs JSON of its own.

(in-package #:nodecode.test)

(define-test-slice "pocket-watch" "POCKET-WATCH-CELL-" :start nodecode-pocket-watch:start-cell)

(defmacro with-pocket-watch ((&rest pairs) &body body)
  "BODY with the cell started under PAIRS as its `pocket-watch' section, and
stopped on unwind."
  `(with-cell-stop ((pocket-watch-start ,@pairs))
     ,@body))

;;; --- the jobs these tests report on ------------------------------------------------

(defparameter +pw-jobs+
  (list (list :id "build" :name "Build check" :enabled t :state "active"
              :schedule (list :kind "cron" :expr "0 9 * * 1-5" :display "0 9 * * 1-5")
              :last-status "ok" :last-at 1762700000 :next-at 1762786400
              :fires 12 :completed 12 :consecutive-failures 0)
        (list :id "digest" :name "Digest" :enabled t :state "active"
              :schedule (list :kind "cron" :expr "30 17 1 * *" :display "30 17 1 * *")
              :last-status "error" :last-at 1762700000 :next-at 1762786400
              :fires 3 :completed 3 :consecutive-failures 1)
        (list :id "sweep" :name "Sweep" :enabled nil :state "paused"
              :schedule (list :kind "interval" :seconds 7200 :display "every 2h")
              :last-status nil :fires 0 :completed 0 :consecutive-failures 0)
        (list :id "fresh" :name "Fresh" :enabled t :state "active"
              :schedule (list :kind "cron" :expr "0 6 * * *" :display "0 6 * * *")
              :last-status nil :fires 0 :completed 0 :consecutive-failures 0))
  "Four jobs with the fields the reports read: one clean, one failing, one
paused and one that never ran.")

(defmacro with-stubbed-jobs ((jobs) &body body)
  "BODY reporting on JOBS instead of the store."
  `(let ((nodecode-pocket-watch::*jobs-reader* (lambda () ,jobs)))
     ,@body))

(defun pw-wire (arguments)
  "pocket_watch called through the wire; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" "pocket_watch" arguments))))

(defun pw-call (view &key status kind limit)
  "pocket_watch with VIEW and the filters given; => the result text."
  (pw-wire (format nil "{\"view\":~s~@[,\"status\":~s~]~@[,\"kind\":~s~]~@[,\"limit\":~d~]}"
                    view status kind limit)))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "pocket-watch"
  (:help :pocket-watch)
  (:command "pocket-watch")
  (:running (is (nle::find-tool "pocket_watch") "the tool is registered")
            (is (= 50 (nodecode-pocket-watch::setting :limit)) "a report lists 50 jobs by default")
            (is (= 80 (nodecode-pocket-watch::setting :warn-below)) "healthy is 80 and up"))
  (:stopped (is (null (nle::find-tool "pocket_watch")) "stop unregisters the tool"))
  (:refused ("limit" 0)
            ("warn_below" -1)
            ("warn_below" 50 "crit_below" 60)
            ("jobs_file" "/nonexistent/pocket-watch/jobs.json"))
  (:idle pocket-watch:pocket-watch-error (pocket-watch:report "health")))

;;; --- the overview ------------------------------------------------------------------

(deftest pocket-watch-cell-the-overview-counts-every-kind-of-job (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-call "status")))
        (is (search "4 jobs" text) "four jobs, counted")
        (is (search "3 enabled, 1 paused" text) "one of them paused")
        (is (search "ok 1, error 1, never run 1" text) "each state counted")
        (is (search "average health 58.8" text) "the average of 100, 50, 0 and 85")))))

(deftest pocket-watch-cell-a-job-line-says-what-it-will-do-next (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-call "jobs")))
        (is (search "build  [ok] Build check  llm  0 9 * * 1-5" text) "the schedule as it was set")
        (is (search "next 20" text) "when it runs next")
        (is (search "health 100.0" text) "a clean job scores 100")))))

(deftest pocket-watch-cell-a-filter-shows-only-what-it-names (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-call "jobs" :status "error")))
        (is (search "digest" text) "the failing job is there")
        (is (null (search "Build check" text)) "and the clean one is not"))
      (let* ((text (pw-call "jobs" :limit 1))
             (lines (remove-if-not (lambda (line) (search "  [" line))
                                    (uiop:split-string text :separator '(#\Newline)))))
        (is (= 1 (length lines)) "one line a job, and only one")))))

;;; --- health -----------------------------------------------------------------------

(deftest pocket-watch-cell-health-is-worst-first-and-says-why (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-call "health")))
        (is (< (search "sweep" text) (search "digest" text)) "the paused job comes first")
        (is (< (search "digest" text) (search "fresh" text)) "then the failing one, then the one that never ran")
        (is (search "score 50.0" text) "a failure costs 20 and 30")
        (is (search "score 85.0" text) "never run costs 10 and 5")
        (is (search "2 healthy (80 or better), 1 degraded, 1 critical (under 40)" text) "and the bands are the section's")))))

;;; --- the grids --------------------------------------------------------------------

(deftest pocket-watch-cell-the-daily-grid-places-a-job-at-its-hour (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-call "daily")))
        (is-carrying text "09:00" "build" "17:30" "digest")
        (is (null (search "sweep" text)) "a paused job is off the grid")))))

(defun pw-count (needle text)
  "How many times NEEDLE appears in TEXT."
  (do ((start 0) (count 0) (at (search needle text) (search needle text :start2 start)))
      ((null at) count)
    (incf count)
    (setf start (+ at (length needle)))))

(deftest pocket-watch-cell-the-weekly-grid-lists-a-weekday-job-on-its-days (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-call "weekly")))
        (is (search "Mon" text) "Monday is a column")
        (is (search "Fri" text) "and so is Friday")
        (is (= 5 (pw-count "build  [ok]" text)) "a weekday job is on five days, not seven")
        (is (= 7 (pw-count "digest  [" text)) "a job with no day field runs every day")))))

(deftest pocket-watch-cell-the-monthly-grid-lists-a-day-of-month-job-on-its-day (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-call "monthly")))
        (is (search "day 1" text) "the first of the month")
        (is (search "17:30  digest" text) "at the time it was set for")))))

;;; --- where the jobs come from ------------------------------------------------------

(defparameter +pw-jobs-json+
  "{\"jobs\": [{\"id\": \"file-job\", \"name\": \"From a file\", \"enabled\": true, \"schedule\": {\"expr\": \"0 8 * * *\"}, \"last_status\": \"ok\", \"repeat\": {\"completed\": 4}, \"consecutive_failures\": 0}, {\"id\": \"off\", \"name\": \"Turned off\", \"enabled\": false, \"schedule\": {\"expr\": \"0 9 * * *\"}, \"repeat\": {\"completed\": 1}}]}"
  "A jobs JSON the way a harness keeps one.")

(deftest pocket-watch-cell-a-jobs-file-is-read-and-its-own-flags-obeyed (with-temp-store ())
  (let ((path (merge-pathnames "pocket-watch-jobs.json" (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string +pw-jobs-json+ out))
    (unwind-protect
         (with-pocket-watch ("jobs_file" (namestring path))
           (let ((text (pw-call "jobs")))
             (is (search "file-job  [ok] From a file" text) "a job the file names")
             (is (search "off  [paused] Turned off" text) "a job its enabled flag turns off")))
      (ignore-errors (delete-file path)))))

(deftest pocket-watch-cell-the-cron-cells-own-store-is-read-with-no-config (with-temp-store ())
  (nlk:session-state-put "cron" "jobs" +pw-jobs-json+)
  (with-pocket-watch ()
    (let ((text (pw-call "status")))
      (is (search "2 jobs" text) "the jobs the cron cell keeps are the jobs it reports")
      (is (search "ok 1" text) "with the last outcome the store holds"))))

(deftest pocket-watch-cell-with-no-jobs-it-says-so-and-convicts-nothing (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs ('())
      (is (search "no jobs yet" (pw-call "status")) "the overview says there are none")
      (is (search "no jobs yet" (pw-call "jobs" :status "error")) "and so does a filtered list"))))

;;; --- the refusals and the command ---------------------------------------------------

(deftest pocket-watch-cell-a-view-it-does-not-know-is-refused-by-name (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((text (pw-wire "{\"view\":\"forecast\"}")))
        (is-carrying text "contract_refused" "one of")
        (is (search "forecast" text) "and the view it was given is named")))))

(deftest pocket-watch-cell-the-command-answers-the-same-report (with-temp-store ())
  (with-pocket-watch ()
    (with-stubbed-jobs (+pw-jobs+)
      (let ((health (pocket-watch:run-slash "health" "s1"))
            (overview (pocket-watch:run-slash "" "s1")))
        (is (search "score 100.0" health) "the health view, from the shell")
        (is (search "4 jobs" overview) "and the overview when it is asked for nothing"))
      (handler-case (progn (pocket-watch:report "forecast") (is nil "a bad view refuses"))
        (pocket-watch:pocket-watch-error (error)
          (is (search "status, jobs, health" (princ-to-string error))))))))
