;;;; plan.lisp --- read a cron folder, work out what could be merged, render the plan.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is read-only and deterministic: no model call, no
;;;; network, no write. The inputs are cron/jobs.json (required) and
;;;; cron/usage_audit.jsonl (optional); executions.db is not read, so what a
;;;; failure streak rests on is the number jobs.json records. The result is
;;;; one JSON value (NLK:JSON-OBJECT); RENDER prints it as Markdown and
;;;; --json prints it whole.

(in-package #:nodecode-cron-plan)

;;; --- what the plan weighs --------------------------------------------------------

(defparameter +duplicate-threshold+ 0.85
  "How alike two normalised prompts must be to count as one job written twice.")

(defparameter +minimum-prompt-chars+ 20
  "A normalised prompt shorter than this is never compared with another.")

(defparameter +failure-streak+ 3
  "A job failing this many times in a row is listed as failing.")

(defparameter +fixed-max-per-day+ 6
  "A schedule firing more often than this a day is a poller, not a clock time.")

(defparameter +max-jobs+ 400
  "Past this many jobs the pairwise prompt comparison, which is quadratic, is skipped.")

(defparameter +error-chars+ 160
  "How much of a failing job's last error the plan repeats.")

;;; --- paths ------------------------------------------------------------------------

(defun join-path (directory name)
  "NAME inside DIRECTORY, both namestrings, with one separator between."
  (format nil "~a/~a" (string-right-trim "/" directory) name))

(defun directory-of (path)
  "PATH's folder as a namestring, or \".\" when it names a file here."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path 0 (max 1 slash)) ".")))

(defun expand-directory (text)
  "TEXT as a directory namestring without its trailing slash; a leading ~ is the home."
  (let* ((trimmed (string-trim '(#\Space #\Tab) (or text "")))
         (merged (if (and (plusp (length trimmed)) (char= #\~ (char trimmed 0)))
                     (merge-pathnames
                      (if (and (> (length trimmed) 1) (char= #\/ (char trimmed 1)))
                          (subseq trimmed 2)
                          (subseq trimmed 1))
                      (user-homedir-pathname))
                     (uiop:parse-native-namestring (if (plusp (length trimmed)) trimmed ".")))))
    (let ((named (string-right-trim "/" (uiop:native-namestring merged))))
      (if (plusp (length named)) named "."))))

;;; --- a job, as jobs.json writes one ------------------------------------------------

(defstruct (job (:copier nil))
  "One cron job a profile holds: what the plan compares and what it reports."
  (id "" :type string)
  (name "" :type string)
  (prompt "" :type string)
  (skills '() :type list)
  (deliver "local" :type string)
  (enabled nil)
  (no-agent nil)
  (script "" :type string)
  (kind "" :type string)
  (display "" :type string)
  (created-at "" :type string)
  (failure-streak 0 :type integer)
  (last-error nil)
  (unverified nil)
  (timing nil)
  (fires-per-day nil)
  (usage '() :type list))

(defun job-flag (raw key &optional (default nil))
  "The boolean KEY of the jobs.json row RAW, or DEFAULT when the row lacks it."
  (multiple-value-bind (value present) (nlk:json-value raw :any key)
    (if present (and value t) default)))

(defun skills-of-row (raw)
  "The skill names RAW names, the singular spelling `skill' included."
  (let* ((many (nlk:json-value raw :array "skills"))
         (one (nlk:json-value raw :string "skill"))
         (names (cond (many (loop for skill across many
                                  when (stringp skill) collect skill))
                      (one (list one))
                      (t '()))))
    (sort (remove-duplicates names :test #'string=) #'string<)))

(defun job-from-json (raw)
  "One JOB out of a jobs.json row, or NIL when the row carries no id."
  (let ((id (nlk:json-value raw :string "id")))
    (when id
      (let* ((schedule (nlk:json-value raw :object "schedule"))
             (kind (or (and schedule (nlk:json-value schedule :string "kind")) ""))
             (expression (and schedule (nlk:json-value schedule :string "expr")))
             (timing nil)
             (per-day nil))
        (cond ((string= kind "cron")
               (when expression
                 (let ((parsed (ignore-errors (parse-cron expression))))
                   (when parsed
                     (setf timing parsed
                           per-day (cron-fires-per-day parsed))))))
              ((string= kind "interval")
               (let ((minutes (and schedule (nlk:json-value schedule :integer "minutes"))))
                 (when (and minutes (integerp minutes))
                   (setf per-day (/ 1440.0 (max 1 minutes)))))))
        (make-job :id id
                  :name (or (nlk:json-value raw :string "name") "")
                  :prompt (or (nlk:json-value raw :string "prompt") "")
                  :skills (skills-of-row raw)
                  :deliver (or (nlk:json-value raw :string "deliver") "local")
                  :enabled (job-flag raw "enabled" t)
                  :no-agent (job-flag raw "no_agent")
                  :script (or (nlk:json-value raw :string "script") "")
                  :kind kind
                  :display (or (nlk:json-value raw :string "schedule_display")
                               expression kind)
                  :created-at (or (nlk:json-value raw :string "created_at") "")
                  :failure-streak (or (nlk:json-value raw :integer "failure_streak") 0)
                  :last-error (nlk:json-value raw :string "last_error")
                  :unverified (job-flag raw "last_delivery_unverified")
                  :timing timing
                  :fires-per-day per-day)))))

(defun read-jobs (path)
  "The JOB records PATH holds, in file order; NIL when PATH is absent.

A row without an id, and a file that is not a row set, contribute nothing
rather than stopping the plan; a file that is not JSON at all signals."
  (let ((text (nlk:read-text path)))
    (when text
      (let* ((data (nlk:decode-json text))
             (rows (cond ((hash-table-p data) (nlk:json-value data :array "jobs"))
                         ((vectorp data) data)
                         (t nil))))
        (loop for raw across (or rows #())
              when (hash-table-p raw)
                append (let ((job (job-from-json raw)))
                         (and job (list job))))))))

;;; --- usage, from what really happened ------------------------------------------------

(defun read-usage (cron-dir days now)
  "Per job id, a plist of the fires and tokens usage_audit.jsonl logs inside DAYS of NOW."
  (let ((cutoff (- now (* 86400 days)))
        (path (join-path cron-dir "usage_audit.jsonl"))
        (table (make-hash-table :test #'equal)))
    (dolist (record (if (probe-file path) (nlk:read-json-lines path) '()))
      (let ((stamp (nlk:iso-universal (or (nlk:json-value record :string "ts") "")))
            (id (nlk:json-value record :string "job_id")))
        (when (and stamp id (>= stamp cutoff))
          (let ((row (or (gethash id table)
                         (setf (gethash id table)
                               (list :fires 0 :prompt-tokens 0 :completion-tokens 0
                                     :silent 0 :errors 0)))))
            (incf (getf row :fires))
            (incf (getf row :prompt-tokens)
                  (or (nlk:json-value record :integer "prompt_tokens") 0))
            (incf (getf row :completion-tokens)
                  (or (nlk:json-value record :integer "completion_tokens") 0))
            (when (job-flag record "response_silent") (incf (getf row :silent)))
            (when (nlk:json-value record :string "error") (incf (getf row :errors)))))))
    table))

;;; --- prompts that are the same prompt -----------------------------------------------

(defun normalize-prompt (text)
  "TEXT lowercased, its spaces collapsed, every run of digits become one #."
  (string-trim
   '(#\Space)
   (with-output-to-string (out)
     (let ((space nil) (digit nil))
       (loop for character across (string-downcase text)
             do (cond ((digit-char-p character)
                       (unless digit (write-char #\# out))
                       (setf digit t space nil))
                      ((member character '(#\Space #\Tab #\Newline #\Return #\Page))
                       (setf space t digit nil))
                      (t (when space (write-char #\Space out))
                         (write-char character out)
                         (setf space nil digit nil))))))))

(defun longest-match (first second first-start first-end second-start second-end)
  "The earliest longest block FIRST and SECOND share in those ranges: (values I J LENGTH)."
  (let* ((width (max 1 (- second-end second-start)))
         (previous (make-array width :initial-element 0))
         (current (make-array width :initial-element 0))
         (best-i first-start)
         (best-j second-start)
         (best 0))
    (loop for i from first-start below first-end
          do (fill current 0)
             (loop for j from second-start below second-end
                   for column = (- j second-start)
                   for run = (if (char= (char first i) (char second j))
                                 (1+ (if (and (> i first-start) (> j second-start))
                                         (aref previous (1- column))
                                         0))
                                 0)
                   do (setf (aref current column) run)
                      (when (> run best)
                        (setf best run
                              best-i (1+ (- i run))
                              best-j (1+ (- j run)))))
             (rotatef previous current))
    (values best-i best-j best)))

(defun matched-characters (first second first-start first-end second-start second-end)
  "How many characters of the two ranges the longest-block recursion matches."
  (multiple-value-bind (at-first at-second length)
      (longest-match first second first-start first-end second-start second-end)
    (if (zerop length)
        0
        (+ length
           (matched-characters first second first-start at-first second-start at-second)
           (matched-characters first second (+ at-first length) first-end
                                (+ at-second length) second-end)))))

(defun prompt-similarity (first second)
  "The share of two normalised prompts' characters they have in common, 0 to 1."
  (let ((total (+ (length first) (length second))))
    (if (zerop total)
        1.0
        (/ (* 2.0 (matched-characters first second 0 (length first) 0 (length second)))
           total))))

(defun similar-prompt-pairs (jobs)
  "The (FIRST SECOND RATIO) triples of enabled jobs whose prompts are near-duplicates."
  (let ((candidates
          (loop for job in jobs
                for prompt = (and (job-enabled job) (not (job-no-agent job))
                                  (normalize-prompt (job-prompt job)))
                when (and prompt (>= (length prompt) +minimum-prompt-chars+))
                  collect (cons job prompt))))
    (when (<= (length candidates) +max-jobs+)
      (loop for rest on candidates
            for (first . first-prompt) = (first rest)
            append (loop for (second . second-prompt) in (rest rest)
                         for shorter = (min (length first-prompt) (length second-prompt))
                         for longer = (max (length first-prompt) (length second-prompt))
                         when (>= (/ (float shorter) longer) +duplicate-threshold+)
                           append (let ((ratio (prompt-similarity first-prompt second-prompt)))
                                    (if (>= ratio +duplicate-threshold+)
                                        (list (list first second ratio)))))))))

;;; --- which jobs are one job -----------------------------------------------------------

(defun make-groups (ids)
  "A union-find over IDS: a hash table from each id to the id that stands for its set."
  (let ((parent (make-hash-table :test #'equal)))
    (dolist (id ids) (setf (gethash id parent) id))
    parent))

(defun group-root (parent id)
  "The id that stands for ID's set, path-compressed on the way."
  (let ((up (gethash id parent)))
    (if (or (null up) (equal up id))
        id
        (setf (gethash id parent) (group-root parent up)))))

(defun group-together (parent first second)
  "Put FIRST and SECOND in one set."
  (let ((first-root (group-root parent first))
        (second-root (group-root parent second)))
    (unless (equal first-root second-root)
      (setf (gethash first-root parent) second-root))))

(defun group-sets (parent)
  "The sets PARENT holds that have more than one member, each as a list of ids."
  (let ((sets (make-hash-table :test #'equal)))
    (loop for id being the hash-keys of parent
          do (push id (gethash (group-root parent id) sets)))
    (loop for members being the hash-values of sets
          when (> (length members) 1)
            collect (sort members #'string<))))

;;; --- the clock ------------------------------------------------------------------------

(defun fixed-time-p (job)
  "True when JOB fires at clock times a day can name, rather than as a poller."
  (and (job-enabled job)
       (job-timing job)
       (<= (* (length (first (job-timing job))) (length (second (job-timing job))))
           +fixed-max-per-day+)))

(defun slot-collisions (jobs)
  "Groups of fixed-time jobs that can fire in the same minute, one entry a clock time.

A poller such as */30 touches every slot, so it is left out: counting it
would chain unrelated jobs into one group."
  (let ((by-slot (make-hash-table :test #'equal))
        (found (make-hash-table :test #'equal))
        (out '()))
    (dolist (job (remove-if-not #'fixed-time-p jobs))
      (let ((minutes (first (job-timing job)))
            (hours (second (job-timing job))))
        (dolist (hour hours)
          (dolist (minute minutes)
            (push job (gethash (cons hour minute) by-slot))))))
    (dolist (slot (sort (loop for key being the hash-keys of by-slot collect key)
                        #'< :key (lambda (key) (+ (* 60 (car key)) (cdr key)))))
      (let ((members (gethash slot by-slot)))
        (when (rest members)
          (let ((parent (make-groups (mapcar #'job-id members))))
            (loop for rest on members
                  do (loop for other in (rest rest)
                           when (cron-overlap-p (job-timing (first rest)) (job-timing other))
                             do (group-together parent (job-id (first rest)) (job-id other))))
            (let ((by-id (make-hash-table :test #'equal))
                  (label (format nil "~2,'0d:~2,'0d" (car slot) (cdr slot))))
              (dolist (job members) (setf (gethash (job-id job) by-id) job))
              (dolist (set (group-sets parent))
                (let* ((key (format nil "~{~a~^,~}" set))
                       (entry (gethash key found)))
                  (unless entry
                    (setf entry (setf (gethash key found)
                                      (nlk:json-object
                                       "slot" label
                                       "slots" (vector)
                                       "jobs" (coerce
                                               (loop for id in set
                                                     for job = (gethash id by-id)
                                                     collect (nlk:json-object
                                                              "id" id
                                                              "name" (job-name job)
                                                              "schedule" (job-display job)))
                                               'vector))))
                    (push entry out))
                  (setf (gethash "slots" entry)
                        (concatenate 'vector (gethash "slots" entry) (vector label))))))))))
    (stable-sort (nreverse out)
                 (lambda (a b)
                   (let ((left (length (nlk:json-value a :array "jobs")))
                         (right (length (nlk:json-value b :array "jobs"))))
                     (if (= left right)
                         (string< (nlk:json-value a :string "slot")
                                  (nlk:json-value b :string "slot"))
                         (> left right)))))))
