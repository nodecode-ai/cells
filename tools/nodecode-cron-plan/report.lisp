;;;; report.lisp --- the plan's numbers, its Markdown and the words that ask for it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; ANALYZE turns a cron folder into one JSON value: which jobs look like
;;;; one job, which fire in the same minute, which keep failing and which
;;;; deliveries were never confirmed. RENDER prints that value for a person;
;;;; --json prints it whole. REPORT parses the argument string the command
;;;; and (cron-plan:plan ...) both take.

(in-package #:nodecode-cron-plan)

(defparameter +help+
  "cron-plan: read-only plan for merging and rescheduling cron jobs.
  report [--json] [--days N] [--home PATH] [--profile NAME] [--jobs PATH]
Nothing is changed: the plan lists what could be merged, which jobs fire in the
same minute, which keep failing and which deliveries were never confirmed.
Token figures come from usage_audit.jsonl, over the last `days' days."
  "What /cron-plan help answers; lines the same facts as the manual.")

;;; --- the plan ---------------------------------------------------------------------

(defun job-usage-of (job days usage)
  "JOB's usage row, the window and the failed-run count laid over it."
  (append (list :window-days days :failed-runs 0) (gethash (job-id job) usage)))

(defun note-reason (reasons id text)
  "Add TEXT to what the plan says about ID."
  (pushnew text (gethash id reasons) :test #'string=))

(defun sorted-reasons (reasons id)
  "What the plan says about ID, in a stable order."
  (sort (copy-list (gethash id reasons)) #'string<))

(defun keeper (members)
  "The job MEMBERS should keep: the one that fired most, then the oldest, then the lowest id."
  (let ((sorted (sort (copy-list members)
                      (lambda (a b)
                        (let ((a-fires (or (getf (job-usage a) :fires) 0))
                              (b-fires (or (getf (job-usage b) :fires) 0)))
                          (cond ((/= a-fires b-fires) (> a-fires b-fires))
                                ((not (string= (job-created-at a) (job-created-at b)))
                                 (string< (job-created-at a) (job-created-at b)))
                                (t (string< (job-id a) (job-id b)))))))))
    (first sorted)))

(defun saving-of (others days)
  "The prompt-side tokens a day a merge of OTHERS could stop spending, a ceiling, or NIL.

It counts only fires the audit saw. A merged job still carries a longer
prompt and still has to produce its output, so this is a ceiling and
completion tokens are left out."
  (let ((per-day 0.0)
        (seen nil))
    (dolist (job others)
      (let ((fires (getf (job-usage job) :fires))
            (tokens (getf (job-usage job) :prompt-tokens)))
        (when (and fires tokens (plusp fires) (plusp tokens))
          (setf seen t)
          (incf per-day (/ tokens (float (or (getf (job-usage job) :window-days) days)))))))
    (and seen (nlk:json-object "prompt_tokens_per_day_ceiling" (round per-day)))))

(defun basename (path)
  "PATH's last component, as a script is named in the plan."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun truncated (text limit)
  "TEXT cut at LIMIT characters, \"\" when there is none."
  (let ((text (or text "")))
    (subseq text 0 (min limit (length text)))))

(defun enabled-jobs (jobs)
  "The jobs of JOBS that are on."
  (remove-if-not #'job-enabled jobs))

(defun same-role-groups (jobs)
  "Jobs sharing both their skills and their destination, one entry a role, in job order."
  (let ((groups '()))
    (dolist (job jobs)
      (when (and (job-skills job) (not (job-no-agent job)))
        (let ((cell (assoc (cons (job-skills job) (job-deliver job)) groups :test #'equal)))
          (if cell
              (push job (cdr cell))
              (push (cons (cons (job-skills job) (job-deliver job)) (list job)) groups)))))
    (loop for (key . members) in (nreverse groups)
          when (rest members)
            collect (cons key (nreverse members)))))

(defun same-script-groups (jobs)
  "Jobs running one script, one entry a script, in job order."
  (let ((groups '()))
    (dolist (job jobs)
      (when (plusp (length (job-script job)))
        (let ((cell (assoc (job-script job) groups :test #'string=)))
          (if cell
              (push job (cdr cell))
              (push (cons (job-script job) (list job)) groups)))))
    (loop for (script . members) in (nreverse groups)
          when (rest members)
            collect (cons script (nreverse members)))))

(defun analyze (cron-dir &key (days 14) (now (get-universal-time)))
  "CRON-DIR's plan, as one JSON value; reads jobs.json and usage_audit.jsonl only."
  (let* ((jobs (read-jobs (join-path cron-dir "jobs.json")))
         (usage (read-usage cron-dir days now))
         (enabled (enabled-jobs jobs))
         (parent (make-groups (mapcar #'job-id enabled)))
         (reasons (make-hash-table :test #'equal))
         (by-id (make-hash-table :test #'equal))
         (merges '())
         (folded 0))
    (dolist (job jobs)
      (setf (job-usage job) (job-usage-of job days usage)))
    (dolist (job enabled)
      (setf (gethash (job-id job) by-id) job))
    (dolist (pair (similar-prompt-pairs enabled))
      (destructuring-bind (first second ratio) pair
        (group-together parent (job-id first) (job-id second))
        (let ((share (format nil "prompt ~d% like " (round (* 100 ratio)))))
          (note-reason reasons (job-id first)
                       (concatenate 'string share
                                    (if (plusp (length (job-name second)))
                                        (job-name second) (job-id second))))
          (note-reason reasons (job-id second)
                       (concatenate 'string share
                                    (if (plusp (length (job-name first)))
                                        (job-name first) (job-id first)))))))
    (dolist (group (same-role-groups enabled))
      (destructuring-bind ((skills . deliver) . members) group
        (loop for other in (rest members)
              do (group-together parent (job-id (first members)) (job-id other)))
        (dolist (member members)
          (note-reason reasons (job-id member)
                       (format nil "same skills (~{~a~^, ~}) and destination (~a)"
                               skills deliver)))))
    (dolist (group (same-script-groups enabled))
      (destructuring-bind (script . members) group
        (loop for other in (rest members)
              do (group-together parent (job-id (first members)) (job-id other)))
        (dolist (member members)
          (note-reason reasons (job-id member)
                       (format nil "runs the same script (~a)" (basename script))))))
    (dolist (set (group-sets parent))
      (let* ((members (loop for id in set
                            for job = (gethash id by-id)
                            when job collect job))
             (chosen (keeper members))
             (others (remove chosen members))
             (skills (sort (remove-duplicates
                            (loop for member in members append (job-skills member))
                            :test #'string=)
                           #'string<)))
        (when chosen
          (incf folded (length others))
          (push (nlk:json-object
                 "keep" (nlk:json-object "id" (job-id chosen)
                                          "name" (job-name chosen)
                                          "schedule" (job-display chosen))
                 "fold_in" (coerce
                            (loop for other in others
                                  collect (nlk:json-object
                                           "id" (job-id other)
                                           "name" (job-name other)
                                           "schedule" (job-display other)
                                           "why" (coerce (sorted-reasons reasons (job-id other))
                                                         'vector)))
                            'vector)
                 "skills" (coerce skills 'vector)
                 "saving" (saving-of others days))
                merges))))
    (setf merges (stable-sort merges
                              (lambda (a b)
                                (> (length (nlk:json-value a :array "fold_in"))
                                   (length (nlk:json-value b :array "fold_in"))))))
    (nlk:json-object
     "profile_cron_dir" cron-dir
     "window_days" days
     "jobs" (nlk:json-object
             "total" (length jobs)
             "enabled" (length enabled)
             "with_usage_data" (count-if (lambda (job)
                                           (let ((fires (getf (job-usage job) :fires)))
                                             (and fires (plusp fires))))
                                         jobs))
     "after_merge" (- (length enabled) folded)
     "merge_candidates" (coerce merges 'vector)
     "slot_collisions" (coerce (slot-collisions jobs) 'vector)
     "failing" (coerce
                (loop for job in enabled
                      when (>= (job-failure-streak job) +failure-streak+)
                        collect (nlk:json-object
                                 "id" (job-id job)
                                 "name" (job-name job)
                                 "streak" (job-failure-streak job)
                                 "error" (truncated (job-last-error job) +error-chars+)
                                 "failed_runs_in_window"
                                 (or (getf (job-usage job) :failed-runs) 0)))
                'vector)
     "unverified_delivery" (coerce
                            (loop for job in enabled
                                  when (job-unverified job)
                                    collect (nlk:json-object "id" (job-id job)
                                                              "name" (job-name job)
                                                              "deliver" (job-deliver job)))
                            'vector)
     "untimed" (coerce
                (loop for job in enabled
                      when (and (string= (job-kind job) "cron") (null (job-timing job)))
                        collect (nlk:json-object "id" (job-id job)
                                                  "name" (job-name job)
                                                  "schedule" (job-display job)))
                'vector))))

;;; --- the Markdown a person reads ------------------------------------------------------

(defun json-number (object key)
  "OBJECT's KEY as an integer, 0 when it is not one."
  (or (nlk:json-value object :integer key) 0))

(defun item-name (item)
  "ITEM as the plan names a job in backticks: its name, else its id."
  (let ((name (nlk:json-value item :string "name")))
    (format nil "`~a`" (if (and name (plusp (length name)))
                            name
                            (nlk:json-value item :string "id")))))

(defun merge-lines (merge)
  "The lines MERGE takes: which job to keep, what to fold in, and what it saves."
  (let* ((keep (nlk:json-value merge :object "keep"))
         (fold-in (nlk:json-value merge :array "fold_in"))
         (skills (nlk:json-value merge :array "skills"))
         (saving (nlk:json-value merge :object "saving"))
         (lines (list (format nil "Keep ~a (~a) and fold in ~d job(s):"
                               (item-name keep)
                               (nlk:json-value keep :string "schedule")
                               (length fold-in)))))
    (loop for other across fold-in
          do (setf lines
                   (append lines
                           (list (format nil "   - ~a (~a): ~{~a~^; ~}"
                                         (item-name other)
                                         (nlk:json-value other :string "schedule")
                                         (coerce (nlk:json-value other :array "why") 'list))))))
    (when (plusp (length skills))
      (setf lines (append lines
                           (list (format nil "   Skills to carry over: ~{~a~^, ~}"
                                         (coerce skills 'list))))))
    (if saving
        (append lines
                (list (format nil "   Up to ~:d prompt tokens/day (ceiling: ~
the merged prompt is longer, output tokens not counted)."
                              (json-number saving "prompt_tokens_per_day_ceiling"))))
        (append lines (list "   No token data for these jobs, so no saving is claimed.")))))

(defun slot-lines (collisions)
  "One line per clock time COLLISIONS holds a group at."
  (loop for group across collisions
        for slots = (nlk:json-value group :array "slots")
        for slot = (nlk:json-value group :string "slot")
        for when = (if (> (length slots) 1)
                       (format nil "~a (and ~d other time(s))" slot (1- (length slots)))
                       slot)
        collect (format nil "at ~a: ~{~a~^, ~}"
                        when
                        (loop for job across (nlk:json-value group :array "jobs")
                              collect (format nil "~a (~a)"
                                              (item-name job)
                                              (nlk:json-value job :string "schedule"))))))

(defun render (result)
  "RESULT as the Markdown plan /cron-plan answers."
  (let* ((jobs (nlk:json-value result :object "jobs"))
         (merges (nlk:json-value result :array "merge_candidates"))
         (collisions (nlk:json-value result :array "slot_collisions"))
         (failing (nlk:json-value result :array "failing"))
         (unverified (nlk:json-value result :array "unverified_delivery"))
         (untimed (nlk:json-value result :array "untimed"))
         (lines (list "# Cron consolidation plan"
                      ""
                      (format nil "~d enabled job(s) of ~d; ~d with token data in the last ~d days. ~
Merging every candidate below would leave **~d**."
                              (json-number jobs "enabled")
                              (json-number jobs "total")
                              (json-number jobs "with_usage_data")
                              (json-number result "window_days")
                              (json-number result "after_merge"))
                      "Read-only: nothing was changed."
                      ""
                      (format nil "## Merge candidates (~d)" (length merges)))))
    (if (zerop (length merges))
        (setf lines (append lines (list "None found.")))
        (loop for merge across merges
              for number from 1
              for block = (merge-lines merge)
              do (setf lines (append lines (list "") (list (format nil "~d. ~a" number (first block)))
                                     (rest block)))))
    (setf lines (append lines
                        (list "" (format nil "## Clock-time jobs that fire in the same minute (~d)"
                                         (length collisions)))))
    (setf lines (if (zerop (length collisions))
                    (append lines (list "None found."))
                    (append lines (loop for line in (slot-lines collisions)
                                        collect (format nil "- ~a" line)))))
    (setf lines (append lines (list "" (format nil "## Failing jobs (~d)" (length failing)))))
    (setf lines (if (zerop (length failing))
                    (append lines (list "None."))
                    (append lines
                            (loop for job across failing
                                  for error = (nlk:json-value job :string "error")
                                  collect (format nil "- ~a: ~d failures in a row~@[; last error: ~a~]"
                                                  (item-name job)
                                                  (json-number job "streak")
                                                  (and error (plusp (length error)) error))))))
    (setf lines (append lines
                        (list "" (format nil "## Deliveries never confirmed (~d)" (length unverified)))))
    (setf lines (if (zerop (length unverified))
                    (append lines (list "None."))
                    (append lines
                            (loop for job across unverified
                                  collect (format nil "- ~a -> ~a"
                                                  (item-name job)
                                                  (nlk:json-value job :string "deliver"))))))
    (when (plusp (length untimed))
      (setf lines (append lines
                           (list "" "## Schedules not understood (left out of the collision check)")
                           (loop for job across untimed
                                 collect (format nil "- ~a: ~a"
                                                 (item-name job)
                                                 (nlk:json-value job :string "schedule"))))))
    (format nil "~{~a~^~%~}" lines)))

;;; --- the words that ask for a plan ----------------------------------------------------

(defun separate-words (text)
  "TEXT cut at whitespace, quotes taken off each word and empty ones dropped."
  (loop for word in (separate text #\Space)
        for trimmed = (string-trim '(#\Space #\Tab #\" #\') word)
        when (plusp (length trimmed)) collect trimmed))

(defun parse-arguments (raw)
  "The options RAW asks for as a plist, with :error when it asks for something unknown."
  (let* ((words (separate-words (or raw "")))
         (words (if (and words (string= (first words) "report")) (rest words) words)))
    (when (and words (member (first words) '("help" "-h" "--help") :test #'string=))
      (return-from parse-arguments (list :help t)))
    (let ((options (list :json nil))
          (remaining words))
      (loop while remaining
            for word = (pop remaining)
            do (cond ((string= word "--json") (setf (getf options :json) t))
                     ((member word '("--days" "--home" "--profile" "--jobs") :test #'string=)
                      (let ((value (or (pop remaining) "")))
                        (cond ((string= word "--days")
                               (let ((days (ignore-errors (parse-integer value))))
                                 (unless (and days (plusp days))
                                   (return-from parse-arguments
                                     (list :error "--days needs a number of days, at least 1")))
                                 (setf (getf options :days) days)))
                              (t (setf (getf options (intern (string-upcase (subseq word 2)) :keyword))
                                     value)))))
                     (t (return-from parse-arguments
                          (list :error (format nil "unknown argument ~s" word))))))
      options)))

(defun expand-file (path)
  "PATH as a namestring, without a trailing slash, its leading ~ read as the home."
  (let ((trimmed (string-trim '(#\Space #\Tab) (or path ""))))
    (if (and (plusp (length trimmed)) (char= #\~ (char trimmed 0)))
        (uiop:native-namestring
         (merge-pathnames (if (and (> (length trimmed) 1) (char= #\/ (char trimmed 1)))
                              (subseq trimmed 2)
                              (subseq trimmed 1))
                          (user-homedir-pathname)))
        trimmed)))

(defun resolve-cron-dir (options)
  "The cron folder OPTIONS name: --jobs's own folder, or the home's, with its profile."
  (let ((jobs (getf options :jobs))
        (home (expand-directory (or (getf options :home) (setting :home))))
        (profile (getf options :profile)))
    (cond (jobs (directory-of (expand-file jobs)))
          (profile (join-path (join-path (join-path home "profiles") profile) "cron"))
          (t (join-path home "cron")))))

(defun report (raw &key (now (get-universal-time)))
  "The plan RAW asks for, as the text /cron-plan answers."
  (let ((options (parse-arguments raw)))
    (cond ((getf options :error)
           (format nil "cron-plan: ~a~%~a" (getf options :error) +help+))
          ((getf options :help) +help+)
          (t (let* ((cron-dir (resolve-cron-dir options))
                    (jobs-file (join-path cron-dir "jobs.json")))
               (cond ((not (probe-file jobs-file))
                      (format nil "cron-plan: no cron/jobs.json under ~a ~
(is the home or the profile right?)"
                              cron-dir))
                     (t (handler-case
                            (let ((result (analyze cron-dir
                                                   :days (or (getf options :days) (setting :days))
                                                   :now now)))
                              (if (getf options :json)
                                  (nlk:pretty-json result)
                                  (render result)))
                          (error (condition)
                            (format nil "cron-plan: could not read ~a: ~a"
                                    jobs-file condition))))))))))
