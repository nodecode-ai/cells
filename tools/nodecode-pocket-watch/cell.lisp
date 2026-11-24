;;;; cell.lisp --- the section, the pocket_watch tool, /pocket-watch, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a dashboard pane over the cron jobs a harness keeps: an
;;;; overview, one line a job, a health score each, and the schedules as a
;;;; daily, weekly and monthly grid. The pane is the harness's; the data and
;;;; the arithmetic behind it are not, so here they are one clause of
;;;; NLE:DEFINE-CELL: a tool the model calls, a command the operator types, a
;;;; topic on the manual, and the `pocket-watch' section they run on. Stop
;;;; takes every one of them back, because the declaration derives it.
;;;;
;;;; Nothing here needs a key or a network. The jobs are read, never written:
;;;; from the jobs JSON a file holds, or, with none named, from the row the
;;;; Nodecode cron cell keeps in the store.

(in-package #:nodecode-pocket-watch)

(nlk:define-peripheral pocket-watch :not-running t)

;;; --- the words ---------------------------------------------------------------

(defparameter +primer+
  "pocket-watch: what the scheduled jobs are doing, as a report.
  pocket_watch(view, status=any, kind=any, limit=0)  one report:
      status   how many jobs there are, enabled, paused, ok, in error, never
               run, by kind, and the average health score
      jobs     one line a job: id, state, kind, schedule, next and last run,
               health score
      health   every job worst score first, then how many are healthy,
               degraded and critical
      daily    the schedules as a day: one line an hour
      weekly   the schedules as a week, Monday to Sunday
      monthly  the schedules as a month, the 1st to the 31st
A health score is 100 for a job that last ran clean: 20 off per consecutive
failure (at most 60), 30 off when the last run failed, 10 off when it never
ran, 5 off when it never completed; a paused job scores 0. `status` narrows
to ok, error, paused or never_run; `kind` to llm, script or hybrid.
The jobs are read, never touched. From eval, (pocket-watch:report \"health\")
prints the same lines and /pocket-watch in a shell asks them.
(help :pocket-watch) has this while the cell runs."
  "What (help :pocket-watch) answers while the cell runs.")

(defparameter +views+ '("status" "jobs" "health" "daily" "weekly" "monthly")
  "The six reports the tool answers, by name.")

(defparameter +statuses+ '("any" "ok" "error" "paused" "never_run")
  "The states a job can be in, and `any'.")

(defparameter +kinds+ '("any" "llm" "script" "hybrid")
  "The kinds of job, and `any'.")

(defparameter +weekday-names+ '( ("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3)
                                 ("thu" . 4) ("fri" . 5) ("sat" . 6))
  "What a five-field cron day-of-week field may spell.")

(defparameter +weekdays+ '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The weekly grid's columns, Monday first, as the original's.")

;;; --- reading the jobs ----------------------------------------------------------

(defvar *jobs-reader* nil
  "NIL reads the jobs from the section's source. A function of no arguments
answers the job plists instead, which is how a test stubs the store.")

(defun jobs-of (object)
  "The job objects in decoded JSON OBJECT: its `jobs' array, or OBJECT itself
when it is already one."
  (let ((array (if (hash-table-p object) (nlk:json-array object "jobs") object)))
    (if (vectorp array) (coerce array 'list) '())))

(defun job-enabled-p (object state)
  "Whether a job runs: its own `enabled' flag when it has one, else by its
state, where paused and done are off."
  (multiple-value-bind (flag present) (nlk:json-value object :boolean "enabled")
    (cond (present flag)
          ((member state '("paused" "done") :test #'equal) nil)
          (t t))))

(defun job-from-object (object)
  "One job, as the plist the reports read, from one decoded JSON job."
  (let* ((schedule (nlk:json-value object :object "schedule"))
         (last (nlk:json-value object :object "last"))
         (state (or (nlk:json-value object :string "state") ""))
         (expr (or (nlk:json-value schedule :string "expr") ""))
         (kind (or (nlk:json-value schedule :string "kind")
                   (and (plusp (length expr)) "cron")
                   ""))
         (display (or (nlk:json-value schedule :string "display") expr))
         (last-status (or (nlk:json-value object :string "last_status")
                          (nlk:json-value last :string "status"))))
    (list :id (or (nlk:json-value object :string "id") "")
          :name (or (nlk:json-value object :string "name") "Unnamed")
          :enabled (job-enabled-p object state)
          :state state
          :script (nlk:json-value object :string "script")
          :skill (nlk:json-value object :string "skill")
          :prompt (nlk:json-value object :string "prompt")
          :last-status last-status
          :last-at (or (nlk:json-value last :integer "at")
                       (nlk:json-value object :integer "last_at"))
          :next-at (nlk:json-value object :integer "next_at")
          :fires (or (nlk:json-value object :integer "fires") 0)
          :completed (or (nlk:json-value object :integer "repeat" "completed")
                         (nlk:json-value object :integer "fires") 0)
          :consecutive-failures (or (nlk:json-value object :integer "consecutive_failures") 0)
          :schedule (list :kind (string-downcase kind)
                          :expr expr
                          :display (if (plusp (length display)) display expr)
                          :seconds (nlk:json-value schedule :integer "seconds")))))

(defun decode-jobs (object)
  "Every job OBJECT carries, as the plists the reports read."
  (mapcar #'job-from-object (jobs-of object)))

(defun read-jobs-file (path)
  "The jobs the JSON file at PATH holds; a refusal when it is missing or is not
JSON, because the operator pointed the section at it."
  (unless (uiop:file-exists-p path)
    (fail "pocket-watch.jobs_file is ~a, and no file is there" path))
  (let ((object (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
    (unless object
      (fail "pocket-watch.jobs_file is ~a, and it does not read as JSON" path))
    (decode-jobs object)))

(defun registry-jobs ()
  "The jobs the Nodecode cron cell keeps in the store, read-only; NIL when the
store is not open here or no job was ever added."
  (let ((text (and (nlk:store-open-p)
                    (ignore-errors (nlk:session-state-get "cron" "jobs")))))
    (if (and (stringp text) (plusp (length text)))
        (or (ignore-errors (decode-jobs (nlk:decode-json text))) '())
        '())))

(defun jobs ()
  "Every job the section's source holds."
  (cond (*jobs-reader* (funcall *jobs-reader*))
        ((setting :jobs-file) (read-jobs-file (setting :jobs-file)))
        (t (registry-jobs))))

;;; --- what one job is ------------------------------------------------------------

(defun job-status (job)
  "JOB's normalized state: ok, error, paused, never_run or unknown."
  (cond ((not (getf job :enabled)) "paused")
        ((equal (getf job :state) "paused") "paused")
        ((null (getf job :last-status)) "never_run")
        ((equal (getf job :last-status) "ok") "ok")
        ((equal (getf job :last-status) "error") "error")
        (t "unknown")))

(defun job-kind (job)
  "JOB's kind: script (a script only), hybrid (a script and a prompt or a
skill) or llm."
  (let ((script (getf job :script))
        (tail (or (getf job :skill) (plusp (length (or (getf job :prompt) ""))))))
    (cond ((and script tail) "hybrid")
          (script "script")
          (t "llm"))))

(defun health-score (job)
  "JOB's 0-100 health score: the original's arithmetic, over the fields the
jobs carry."
  (if (not (getf job :enabled))
      0
      (let ((score 100))
        (decf score (min (* 20 (getf job :consecutive-failures)) 60))
        (let ((last (getf job :last-status)))
          (cond ((equal last "error") (decf score 30))
                ((null last) (decf score 10))))
        (when (eql 0 (or (getf job :completed) 0))
          (decf score 5))
        (max 0 (min 100 score)))))

(defun one-decimal (number)
  "NUMBER to one decimal place, as a float."
  (/ (float (round (* (float number) 10))) 10.0))

(defun unix-text (unix)
  "UNIX seconds as local wall time, \"2026-11-09 09:00\"; NIL as an empty
string."
  (if unix
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (+ unix nlk:+unix-epoch+))
        (declare (ignore second))
        (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute))
      ""))

;;; --- narrowing --------------------------------------------------------------------

(defun select-jobs (all &key status kind limit)
  "ALL, only the jobs STATUS and KIND name, at most LIMIT (the section's when
it is 0)."
  (let* ((kept (remove-if-not
                (lambda (job)
                  (and (or (null status) (equal status "any") (equal status (job-status job)))
                       (or (null kind) (equal kind "any") (equal kind (job-kind job)))))
                all))
         (cap (if (and limit (plusp limit)) limit (setting :limit))))
    (if (> (length kept) cap) (subseq kept 0 cap) kept)))

(defun clip (text)
  "TEXT, cut to the section's report_chars with a line saying so."
  (let ((cap (setting :report-chars)))
    (if (and (> (length text) cap) (> cap 40))
        (concatenate 'string (subseq text 0 (- cap 14)) " ... clipped")
        text)))

;;; --- cron fields ------------------------------------------------------------------

(defun join-lines (items)
  "ITEMS joined by a newline, one string."
  (with-output-to-string (out)
    (loop for (item . more) on items
          do (write-string (or item "") out)
             (when more (write-string (string #\Newline) out)))))

(defun cron-fields (expr)
  "EXPR's five cron fields, or NIL when it does not carry five."
  (let ((parts (remove "" (uiop:split-string (or expr "")
                                              :separator '(#\Space #\Tab))
                       :test #'equal)))
    (when (>= (length parts) 5) (subseq parts 0 5))))

(defun field-number (text names)
  "TEXT as the integer it names, by number or by one of NAMES."
  (let ((cell (and names (assoc (string-downcase text) names :test #'equal))))
    (if cell (cdr cell) (parse-integer text :junk-allowed t))))

(defun field-range (text low high names)
  "The (FIRST LAST) a comma-free field part names within LOW..HIGH."
  (cond ((or (string= text "*") (string= text "")) (list low high))
        (t (let ((dash (position #\- text)))
             (if dash
                 (let ((start (field-number (subseq text 0 dash) names))
                       (end (field-number (subseq text (1+ dash)) names)))
                   (and start end (list start end)))
                 (let ((value (field-number text names)))
                   (and value (list value value))))))))

(defun field-values (text low high &optional names)
  "The integers TEXT names within LOW..HIGH, sorted; NIL when it names none."
  (when (and text (plusp (length text)))
    (sort (remove-duplicates
           (loop for part in (uiop:split-string text :separator ",")
                 for slash = (position #\/ part)
                 for step = (if slash (or (field-number (subseq part (1+ slash)) nil) 1) 1)
                 for base = (if slash (subseq part 0 slash) part)
                 for range = (field-range base low high names)
                 when range
                   append (loop for value from (first range) to (second range) by (max 1 step)
                                when (and (>= value low) (<= value high)) collect value)))
          #'<)))

;;; --- the reports -------------------------------------------------------------------

(defun counts-line (all kept)
  "The one line every report opens with: how many jobs there are, and how
they are doing."
  (let* ((total (length all))
         (enabled (count-if (lambda (job) (getf job :enabled)) all))
         (by-status (lambda (name) (count name all :key #'job-status :test #'equal)))
         (by-kind (lambda (name) (count name all :key #'job-kind :test #'equal)))
         (average (if (plusp total)
                      (one-decimal (/ (reduce #'+ (mapcar #'health-score all)) total))
                      0)))
    (format nil "pocket-watch: ~d job~:p, ~d enabled, ~d paused; ok ~d, error ~d, never run ~d; llm ~d, script ~d, hybrid ~d; average health ~,1f~@[, ~d shown~]."
            total enabled (- total enabled)
            (funcall by-status "ok") (funcall by-status "error") (funcall by-status "never_run")
            (funcall by-kind "llm") (funcall by-kind "script") (funcall by-kind "hybrid")
            (float average)
            (and (< (length kept) total) (length kept)))))

(defun schedule-text (job)
  "JOB's schedule in the words it was set in."
  (let ((schedule (getf job :schedule)))
    (cond ((and schedule (plusp (length (or (getf schedule :display) ""))))
           (getf schedule :display))
          ((and schedule (getf schedule :seconds))
           (format nil "every ~ds" (getf schedule :seconds)))
          (t "no schedule"))))

(defun job-line (job)
  "One job, one line."
  (let ((next (unix-text (getf job :next-at)))
        (last-at (unix-text (getf job :last-at)))
        (failed (getf job :consecutive-failures)))
    (format nil "~a  [~a] ~a  ~a  ~a~@[  next ~a~]~@[  last ~a~@[ ~a~]~]  health ~,1f~@[, ~d in a row failed~]"
            (getf job :id) (job-status job) (getf job :name) (job-kind job)
            (schedule-text job)
            (and (plusp (length next)) next)
            (getf job :last-status)
            (and (plusp (length last-at)) last-at)
            (float (health-score job))
            (and failed (plusp failed) failed))))

(defparameter +empty+ 
  "pocket-watch: no jobs yet; the cron store holds none, and the section names no jobs file."
  "What a report says when there is nothing to read.")

(defun status-text ()
  (let ((all (jobs)))
    (if (null all) +empty+ (clip (counts-line all all)))))

(defun jobs-text (&key status kind limit)
  (let* ((all (jobs))
         (kept (select-jobs all :status status :kind kind :limit limit)))
    (cond ((null all) +empty+)
          ((null kept) "pocket-watch: no job answers the filter; the status filter is ok, error, paused or never_run, the kind filter llm, script or hybrid.")
          (t (clip (join-lines (cons (counts-line all kept) (mapcar #'job-line kept))))))))

(defun health-line (job)
  "One job's row of the health report."
  (format nil "~a  ~a  score ~,1f  ~a~@[  ~d in a row failed~]~@[  last ~a~]"
          (getf job :id) (getf job :name)
          (float (health-score job)) (job-status job)
          (let ((failed (getf job :consecutive-failures)))
            (and failed (plusp failed) failed))
          (let ((at (unix-text (getf job :last-at))))
            (and (plusp (length at)) at))))

(defun health-text (&key status kind limit)
  (let* ((all (jobs))
         (kept (sort (copy-list (select-jobs all :status status :kind kind :limit limit))
                     #'< :key #'health-score)))
    (cond ((null all) "pocket-watch: no jobs to score yet.")
          ((null kept) "pocket-watch: no job answers the filter.")
          (t (let* ((warn (setting :warn-below))
                    (crit (setting :crit-below))
                    (healthy (count-if (lambda (job) (>= (health-score job) warn)) all))
                    (critical (count-if (lambda (job) (< (health-score job) crit)) all))
                    (summary (format nil "~d healthy (~d or better), ~d degraded, ~d critical (under ~d)."
                                     healthy warn (- (length all) healthy critical) critical crit)))
               (clip (join-lines
                      (append (list (counts-line all kept))
                              (mapcar #'health-line kept)
                              (list summary)))))))))

;;; --- the grids ---------------------------------------------------------------------

(defun grid-jobs (all place)
  "ALL, each job placed by PLACE -- a function of a job answering a list of
(SLOT TIME-TEXT) -- as an alist of SLOT to lines, in slot order."
  (let ((grid '()))
    (dolist (job all)
      (when (getf job :enabled)
        (dolist (cell (funcall place job))
          (let ((entry (assoc (first cell) grid :test #'=)))
            (unless entry
              (setf entry (list (first cell))
                    grid (append grid (list entry))))
            (setf (cdr entry)
                  (append (cdr entry)
                          (list (format nil "~@[~a  ~]~a  [~a] ~a"
                                        (second cell) (getf job :id)
                                        (job-status job) (getf job :name)))))))))
    (sort grid #'< :key #'first)))

(defun slot-time (fields)
  "The wall time FIELDS name, \"09:30\"; an empty string when the hour or the
minute is not one number."
  (let ((hours (field-values (second fields) 0 23))
        (minutes (field-values (first fields) 0 59)))
    (if (and (= 1 (length hours)) (= 1 (length minutes)))
        (format nil "~2,'0d:~2,'0d" (first hours) (first minutes))
        "")))

(defun clock-text (hour minute)
  (format nil "~2,'0d:~2,'0d" hour minute))

(defun local-parts (unix)
  "The (MINUTE HOUR DAY-OF-MONTH DAY-OF-WEEK) UNIX is, Monday being 0."
  (multiple-value-bind (second minute hour day month year dow)
      (decode-universal-time (+ unix nlk:+unix-epoch+))
    (declare (ignore second month year))
    (list minute hour day (mod (+ dow 6) 7))))

(defun daily-slots (job)
  "The hours and minutes JOB occupies in a day: a list of (HOUR \"HH:MM\")."
  (let ((fields (cron-fields (getf (getf job :schedule) :expr))))
    (cond
      (fields
       (let ((hours (field-values (second fields) 0 23))
             (minutes (field-values (first fields) 0 59)))
         (loop for hour in hours
               append (loop for minute in minutes
                            collect (list hour (clock-text hour minute))))))
      ((getf job :next-at)
       (destructuring-bind (minute hour) (local-parts (getf job :next-at))
         (list (list hour (clock-text hour minute))))))))

(defun weekly-slots (job)
  "The days of the week JOB occupies, Monday being 0: a list of (DAY \"HH:MM\")."
  (let ((fields (cron-fields (getf (getf job :schedule) :expr))))
    (cond
      (fields
       (let ((days (remove-duplicates
                    (mapcar (lambda (day) (if (= day 7) 0 day))
                            (field-values (fifth fields) 0 7 +weekday-names+))))
             (time (slot-time fields)))
         (loop for day in (sort days #'<) collect (list day time))))
      ((getf job :next-at)
       (destructuring-bind (minute hour day dow) (local-parts (getf job :next-at))
         (declare (ignore day))
         (list (list dow (clock-text hour minute))))))))

(defun monthly-slots (job)
  "The days of the month JOB occupies: a list of (DAY \"HH:MM\")."
  (let ((fields (cron-fields (getf (getf job :schedule) :expr))))
    (cond
      (fields
       (let ((days (field-values (third fields) 1 31))
             (time (slot-time fields)))
         (loop for day in days collect (list day time))))
      ((getf job :next-at)
       (destructuring-bind (minute hour day dow) (local-parts (getf job :next-at))
         (declare (ignore dow))
         (list (list day (clock-text hour minute))))))))

(defun grid-text (all place slot-label row-label)
  "The grid PLACE builds from ALL: a line naming each slot that has a job."
  (let ((grid (grid-jobs all place)))
    (if (null grid)
        "pocket-watch: nothing is scheduled on that view."
        (clip (join-lines
               (cons (format nil "pocket-watch: ~d job~:p, by ~a."
                             (count-if (lambda (job) (getf job :enabled)) all)
                             row-label)
                     (loop for (slot . lines) in grid
                           append (cons (funcall slot-label slot)
                                        (mapcar (lambda (line) (format nil "    ~a" line))
                                                lines)))))))))

(defun daily-text ()
  (grid-text (jobs) #'daily-slots (lambda (hour) (format nil "~2,'0d:00" hour)) "hour"))

(defun weekly-text ()
  (grid-text (jobs) #'weekly-slots (lambda (slot) (nth slot +weekdays+)) "day of the week"))

(defun monthly-text ()
  (grid-text (jobs) #'monthly-slots (lambda (slot) (format nil "day ~d" slot)) "day of the month"))

;;; --- the one door the model, the shell and eval all come through ---------------------

(defun report (view &key status kind limit)
  "The report VIEW names, over the section's jobs. A view that is not one of
+VIEWS+ is a refusal naming them."
  (let ((view (string-downcase (nlk:trimmed (or view "status")))))
    (cond ((string= view "") (status-text))
          ((string= view "status") (status-text))
          ((string= view "jobs") (jobs-text :status status :kind kind :limit limit))
          ((string= view "health") (health-text :status status :kind kind :limit limit))
          ((string= view "daily") (daily-text))
          ((string= view "weekly") (weekly-text))
          ((string= view "monthly") (monthly-text))
          (t (fail "pocket-watch answers ~{~a~^, ~}; not ~s" +views+ view)))))

(defun run-slash (args session-id)
  "One /pocket-watch invocation: ARGS is a view name, or nothing for the
overview."
  (declare (ignore session-id))
  (report args))

;;; --- the boundary -------------------------------------------------------------------

(nlk:define-contract pocket-watch-query
  (:doc "Report on the scheduled jobs: the overview, one line a job, the health
list with a score each, or the schedules as a daily, weekly or monthly grid.
The jobs are read, never changed.")
  ("view" :text :required t
   :options '("status" "jobs" "health" "daily" "weekly" "monthly")
   :doc "status: counts and the average health; jobs: one line a job; health: every job, worst first; daily, weekly, monthly: the schedules as a grid")
  ("status" :text :default "any" :options '("any" "ok" "error" "paused" "never_run")
   :doc "only the jobs in this state")
  ("kind" :text :default "any" :options '("any" "llm" "script" "hybrid")
   :doc "only the jobs of this kind")
  ("limit" :integer :default 0
   :doc "the most jobs to list; 0 lists the section's limit"))

(defun query-tool (query)
  "The pocket_watch tool: one report, as text."
  (report (pocket-watch-query-view query)
          :status (pocket-watch-query-status query)
          :kind (pocket-watch-query-kind query)
          :limit (pocket-watch-query-limit query)))

;;; --- the section, and everything above installed by it -------------------------------

(nle:define-cell pocket-watch
  (:section ("pocket-watch")
    (:guide "nothing here is required; jobs_file points the reports at a cron jobs JSON, and with none named they read the jobs the Nodecode cron cell keeps in the store; warn_below and crit_below are where the health bands fall")
    ("jobs_file" :path
     :doc "a cron jobs JSON to report on, the way a harness keeps one: {\"jobs\": [...]}; with none named the reports read the jobs the Nodecode cron cell keeps in the store")
    ("limit" :integer :default 50 :min 1
     :doc "the most jobs one report lists")
    ("report_chars" :integer :default 4000 :min 200
     :doc "the longest report, in characters")
    ("warn_below" :integer :default 80 :min 0
     :doc "a job scoring this or better is healthy")
    ("crit_below" :integer :default 40 :min 0
     :doc "a job scoring under this is critical"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((file (getf values :jobs-file)))
                 (when (and file (not (uiop:file-exists-p file)))
                   (nlk:config-error "pocket-watch.jobs_file is ~s; no file is there" file)))
               (when (<= (getf values :warn-below) (getf values :crit-below))
                 (nlk:config-error "pocket-watch.warn_below (~d) must be above crit_below (~d)"
                                   (getf values :warn-below) (getf values :crit-below)))
               values))
  (:tool "pocket_watch" (nlk:contract 'pocket-watch-query) 'query-tool)
  (:help :pocket-watch
         "pocket-watch: the scheduled jobs as a report: counts, one line a job, a health score each, and the day, week and month grids; /pocket-watch"
         +primer+)
  (:command "pocket-watch" 'run-slash
            :catches 'pocket-watch-error
            :description "The scheduled jobs: the overview, the jobs, their health, or the day, week and month grids"))
