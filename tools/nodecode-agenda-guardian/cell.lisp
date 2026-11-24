;;;; cell.lisp --- Agenda Guardian: a day planner whose blocks say why they are there.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Moodtuner997/hermes-agenda-guardian (see NOTICE). The rules
;;;; a user writes once (work hours, protected windows, the focus window, the
;;;; longest block, the tasks a day), the estimates corrected from the user's
;;;; own finished work, the placement of tasks in the free time around a
;;;; calendar's events, the all-or-nothing blocks and the "what does not fit"
;;;; line are the original's. The harness seams are Nodecode's: the rules the
;;;; user changed in the chat and the tasks live in one JSON file in the
;;;; folder the section names, a tool is one (:tool ...) clause of
;;;; NLE:DEFINE-CELL, and the original's /day slash command is
;;;; /agenda-guardian here, because the original's own README says its
;;;; harness already had a day panel under that name.
;;;;
;;;; The planner is deterministic: no model call, and the same tasks, rules
;;;; and calendar give the same plan. Nothing here reaches the network unless
;;;; the operator's calendar setting is an https address.

(defpackage #:nodecode-agenda-guardian
  (:use #:cl)
  (:nicknames #:agenda-guardian)
  (:export #:agenda-guardian-error #:agenda-guardian-error-detail))

(in-package #:nodecode-agenda-guardian)

(nlk:define-peripheral agenda-guardian :not-running t :lock "agenda-guardian")

(defparameter +step+ 5 "Minutes: every plan figure is a multiple of this, as the original's STEP is.")
(defparameter +state-name+ "agenda.json" "The one file the planner keeps, in the folder the section names.")
(defparameter +default-estimate+ 30 "Minutes a task takes when the user gave no estimate.")
(defparameter +min-finished+ 3 "Finished tasks of a kind before its estimates are corrected.")

;;; --- the file ------------------------------------------------------------------

(defun data-folder ()
  "The folder the planner's file lives in, as the section names it."
  (uiop:ensure-directory-pathname (or (setting :data-dir) "")))

(defun state-file ()
  (merge-pathnames +state-name+ (data-folder)))

(defun empty-state ()
  (list :seq 0 :tasks '() :rules '()))

(defun task-of (object)
  "One task, as the file holds it."
  (list :id (nlk:json-value object :string "id")
        :name (nlk:json-value object :string "name")
        :minutes (nlk:json-value object :integer "minutes")
        :category (or (nlk:json-value object :string "category") "general")
        :priority (or (nlk:json-value object :integer "priority") 2)
        :due (or (nlk:json-value object :string "due") "")
        :done (and (nlk:json-value object :boolean "done") t)
        :real (nlk:json-value object :integer "real")))

(defun task-json (task)
  (nlk:json-object
   "id" (getf task :id)
   "name" (getf task :name)
   "minutes" (getf task :minutes)
   "category" (getf task :category)
   "priority" (getf task :priority)
   "due" (getf task :due)
   "done" (if (getf task :done) t :false)
   "real" (or (getf task :real) :null)))

(defun wire-rule (key)
  (ecase key
    (:work-hours "work_hours")
    (:workdays "workdays")
    (:protected "protected")
    (:focus "focus")
    (:calendar "calendar")
    (:buffer-min "buffer_min")
    (:max-block-min "max_block_min")
    (:max-tasks-per-day "max_tasks_per_day")))

(defun rule-overrides (object)
  "The rules the user changed in the chat, as a plist; a rule still at the
section's value is simply absent."
  (let ((rules (nlk:json-value object :object "rules")))
    (when rules
      (loop for key in '(:work-hours :workdays :protected :focus :calendar
                         :buffer-min :max-block-min :max-tasks-per-day)
            for kind = (if (member key '(:buffer-min :max-block-min :max-tasks-per-day))
                           :integer :string)
            for value = (nlk:json-value rules kind (wire-rule key))
            when value nconc (list key value)))))

(defun read-state ()
  "The planner's file as (:seq N :tasks (TASK ...) :rules PLIST), or an empty
one the first time. A file that does not decode is an error the model reads
and a person fixes, never silently an empty plan."
  (let ((file (state-file)))
    (if (probe-file file)
        (let ((object (nlk:decode-json (uiop:read-file-string file))))
          (list :seq (or (nlk:json-value object :integer "seq") 0)
                :tasks (loop for item across (or (nlk:json-array object "tasks") #())
                             collect (task-of item))
                :rules (rule-overrides object)))
        (empty-state))))

(defun state-json (state)
  (nlk:json-object
   "version" 1
   "seq" (getf state :seq)
   "tasks" (coerce (loop for task in (getf state :tasks) collect (task-json task)) 'vector)
   "rules" (apply #'nlk:make-json-object
                  (loop for (key value) on (getf state :rules) by #'cddr
                        collect (wire-rule key) collect value))))

(defun write-state (state)
  "STATE to the planner's file atomically, under the folder's lock, so two
processes sharing the folder never write one file from two stale reads."
  (let ((file (state-file)))
    (ensure-directories-exist file)
    (nlk:with-folder-lock ((data-folder))
      (nlk:write-file-atomically file (format nil "~a~%" (nlk:encode-json-object (state-json state)))
                                  :mode :private))
    state))

(defmacro with-store ((state) &body body)
  "BODY with STATE read from the file and written back when it answers."
  `(let ((,state (read-state)))
     (prog1 (progn ,@body)
       (write-state ,state))))

;;; --- arguments ------------------------------------------------------------------

(defun argument (args name)
  "ARGS' NAME, or NIL; ARGS is NIL when a call carries no arguments at all."
  (and (hash-table-p args) (gethash name args)))

(defun argument-text (args name)
  (let ((value (argument args name)))
    (and (stringp value) value)))

(defun argument-integer (args name)
  (let ((value (argument args name)))
    (and (integerp value) value)))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model
repairs from -- the shape every tool of the original answers a refusal in."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: agenda-guardian: ~a" condition)))))

;;; --- clocks and days ------------------------------------------------------------

(defun hhmm (text)
  "TEXT, HH:MM, as minutes from midnight; a refusal naming what was wanted."
  (let ((colon (position #\: (or text ""))))
    (unless (and colon (plusp colon) (= 2 (- (length text) (1+ colon))))
      (fail "~s is not a time like 09:30" text))
    (let ((hours (parse-integer text :end colon))
          (minutes (parse-integer text :start (1+ colon))))
      (unless (and (<= 0 hours 23) (<= 0 minutes 59))
        (fail "~s is not a time like 09:30" text))
      (+ (* hours 60) minutes))))

(defun clock (minutes)
  (format nil "~2,'0d:~2,'0d" (floor minutes 60) (mod minutes 60)))

(defun parse-range (text what)
  "TEXT, HH:MM-HH:MM, as (START . END) in minutes."
  (let* ((dash (position #\- (or text "")))
         (start (and dash (hhmm (subseq text 0 dash))))
         (end (and dash (hhmm (subseq text (1+ dash))))))
    (unless (and start end (< start end))
      (fail "~a wants HH:MM-HH:MM, not ~s" what text))
    (cons start end)))

(defun split-list (text)
  "TEXT's parts, split on ; and trimmed, without the empty ones."
  (loop for part in (uiop:split-string (or text "") :separator ";")
        for trimmed = (string-trim " " part)
        when (plusp (length trimmed)) collect trimmed))

(defun date-int (year month day)
  (+ (* year 10000) (* month 100) day))

(defun date-parts (day)
  (values (floor day 10000) (mod (floor day 100) 100) (mod day 100)))

(defun today ()
  "The machine's local date, as an integer YYYYMMDD."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (date-int year month date)))

(defun now-minutes ()
  "The minute of the local day it is now."
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (+ (* hour 60) minute)))

(defun date-weekday (day)
  "DAY's ISO weekday, 1 Monday to 7 Sunday."
  (multiple-value-bind (year month date) (date-parts day)
    (1+ (nth-value 6 (decode-universal-time (encode-universal-time 0 0 12 date month year 0) 0)))))

(defun date+ (day count)
  (multiple-value-bind (year month date) (date-parts day)
    (let ((ut (+ (encode-universal-time 0 0 12 date month year 0) (* count 86400))))
      (multiple-value-bind (second minute hour dd mm yy) (decode-universal-time ut 0)
        (declare (ignore second minute hour))
        (date-int yy mm dd)))))

(defun date-diff (first second)
  "Whole days from SECOND to FIRST."
  (multiple-value-bind (y1 m1 d1) (date-parts first)
    (multiple-value-bind (y2 m2 d2) (date-parts second)
      (round (- (encode-universal-time 0 0 12 d1 m1 y1 0)
                (encode-universal-time 0 0 12 d2 m2 y2 0))
             86400))))

(defun date-string (day)
  (multiple-value-bind (year month date) (date-parts day)
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun weekday-name (day)
  (nth (1- (date-weekday day)) '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))

(defun parse-date (text fallback)
  "TEXT -- YYYY-MM-DD, today, tomorrow -- as a date; FALLBACK when empty."
  (let ((trimmed (string-downcase (string-trim " " (or text "")))))
    (cond ((zerop (length trimmed)) fallback)
          ((string= trimmed "today") (today))
          ((string= trimmed "tomorrow") (date+ (today) 1))
          ((string= trimmed "yesterday") (date+ (today) -1))
          ((and (= 10 (length trimmed))
                (char= (char trimmed 4) #\-) (char= (char trimmed 7) #\-))
           (date-int (parse-integer trimmed :start 0 :end 4)
                     (parse-integer trimmed :start 5 :end 7)
                     (parse-integer trimmed :start 8 :end 10)))
          (t (fail "~s is not a date like 2026-11-09" text)))))

(defun ceil-step (minutes)
  (* +step+ (ceiling minutes +step+)))

(defun one-day (token names)
  (let ((found (assoc token names :test #'string=)))
    (cond (found (cdr found))
          ((and (plusp (length token)) (every #'digit-char-p token)) (parse-integer token))
          (t (fail "~s is not a weekday" token)))))

(defun workdays-of (text)
  "TEXT -- mon-fri, or 1,2,3, or mon,wed -- as a sorted list of ISO weekdays."
  (let ((days '())
        (names '(("mon" . 1) ("tue" . 2) ("wed" . 3) ("thu" . 4) ("fri" . 5) ("sat" . 6) ("sun" . 7))))
    (dolist (part (uiop:split-string (string-downcase (remove #\Space (or text "mon-fri"))) :separator ","))
      (let ((dash (and (plusp (length part)) (position #\- part))))
        (cond ((zerop (length part)))
              (dash
               (let ((first (one-day (subseq part 0 dash) names))
                     (last (one-day (subseq part (1+ dash)) names)))
                 (loop for day from first to (if (< first last) last (+ last 7))
                       do (push (1+ (mod (1- day) 7)) days))))
              (t (push (one-day part names) days)))))
    (unless days (fail "~s names no weekday" text))
    (sort (remove-duplicates days) #'<)))

(defun guarded-windows (text what)
  "TEXT -- name HH:MM-HH:MM, several by ; -- as (NAME START END) lists."
  (loop for part in (split-list text)
        for space = (position #\Space part :from-end t)
        for name = (if space (string-trim " " (subseq part 0 space)) "")
        for rest = (if space (subseq part (1+ space)) part)
        for span = (parse-range rest what)
        collect (list name (car span) (cdr span))))

(defun focus-windows (text)
  (loop for part in (split-list text) collect (parse-range part "focus")))

;;; --- the rules in force ---------------------------------------------------------

(defun rule-of (state key)
  (or (getf (getf state :rules) key) (setting key)))

(defun rule-source (state key)
  (if (getf (getf state :rules) key) "changed in the chat" "from the section"))

(defun rules-of (state)
  "The rules the plan obeys, as a plist."
  (loop for key in '(:work-hours :workdays :protected :focus :buffer-min :max-block-min
                     :max-tasks-per-day :calendar)
        collect key collect (rule-of state key)))

(defun rule-summary (state)
  "Each rule in force as (KEY VALUE SOURCE), for the model and the command."
  (loop for key in '(:work-hours :workdays :protected :focus :buffer-min :max-block-min
                     :max-tasks-per-day :calendar)
        collect (list key (rule-of state key) (rule-source state key))))

(defun set-rule (state key value)
  "KEY to VALUE, refused when it does not parse, and recorded as the user's."
  (ecase key
    (:work-hours (parse-range value "work_hours"))
    (:workdays (workdays-of value))
    (:protected (guarded-windows value "protected"))
    (:focus (focus-windows value))
    (:calendar nil)
    (:buffer-min (when (or (minusp value) (> value 60)) (fail "buffer_min is 0 to 60")))
    (:max-block-min (when (or (< value 15) (> value 120)) (fail "max_block_min is 15 to 120")))
    (:max-tasks-per-day (when (or (< value 1) (> value 10)) (fail "max_tasks_per_day is 1 to 10"))))
  (setf (getf (getf state :rules) key) value)
  value)

;;; --- free time -------------------------------------------------------------------

(defun merge-spans (spans)
  "SPANS -- (START . END) minute pairs -- sorted and merged where they touch."
  (let ((out '()))
    (dolist (span (sort (copy-list spans) #'< :key #'car) (nreverse out))
      (let ((last (first out)))
        (if (and last (<= (car span) (cdr last)))
            (setf (cdr last) (max (cdr last) (cdr span)))
            (push (cons (car span) (cdr span)) out))))))

(defun subtract-spans (window busy)
  "WINDOW, (START . END), minus BUSY: the free spans left, in order."
  (let ((cursor (car window)) (free '()))
    (dolist (span (merge-spans (loop for (start . end) in busy
                                     when (and (< start (cdr window)) (> end (car window)))
                                       collect (cons (max start (car window)) (min end (cdr window))))))
      (when (> (car span) cursor) (push (cons cursor (car span)) free))
      (setf cursor (max cursor (cdr span))))
    (when (< cursor (cdr window)) (push (cons cursor (cdr window)) free))
    (nreverse free)))

(defun split-focus (free focus)
  "FREE cut at the focus window's edges: (START END FOCUS-P) slots, so every
slot is wholly inside or outside the sharpest hours."
  (let ((slots '()))
    (dolist (span free)
      (let* ((start (car span))
             (end (cdr span))
             (cuts (sort (remove-duplicates
                          (append (list start end)
                                  (loop for range in focus
                                        append (loop for point in (list (car range) (cdr range))
                                                     when (and (< start point) (< point end))
                                                       collect point))))
                         #'<)))
        (loop for (lo hi) on cuts
              when hi
                do (when (>= (- hi lo) +step+)
                     (push (list lo hi (loop for range in focus
                                             thereis (and (<= (car range) lo) (<= hi (cdr range)))))
                           slots)))))
    (nreverse slots)))

(defun chunk-sizes (planned max-block)
  "PLANNED minutes as the fewest blocks of at most MAX-BLOCK, evenly."
  (let* ((units (/ planned +step+))
         (count (max 1 (ceiling planned max-block)))
         (base (floor units count))
         (extra (mod units count)))
    (loop for index below count
          collect (* +step+ (+ base (if (< index extra) 1 0))))))

(defun place-chunks (slots chunks)
  "CHUNKS placed in SLOTS, each in a focus slot when one is free: (values
BLOCKS REST-SLOTS PLACED-P), all of them or none, as the original plans."
  (let ((blocks '()) (rest slots) (placed t))
    (dolist (chunk chunks)
      (let ((slot (or (find-if (lambda (slot) (and (third slot) (>= (- (second slot) (first slot)) chunk))) rest)
                      (find-if (lambda (slot) (>= (- (second slot) (first slot)) chunk)) rest))))
        (if (null slot)
            (setf placed nil)
            (let ((end (+ (first slot) chunk)))
              (push (cons (first slot) end) blocks)
              (setf rest (loop for other in rest
                               if (eq other slot)
                                 append (when (>= (- (second other) end) +step+)
                                          (list (list end (second other) (third other))))
                               else collect other))))))
    (values (if placed (sort (nreverse blocks) #'< :key #'car) nil) rest placed)))

(defun median (numbers)
  (let* ((sorted (sort (copy-list numbers) #'<))
         (count (length sorted)))
    (if (evenp count)
        (/ (+ (nth (1- (/ count 2)) sorted) (nth (/ count 2) sorted)) 2)
        (nth (floor count 2) sorted))))

(defun factor-of (tasks category)
  "The original's correction: (values FACTOR COUNT) -- 1 until three tasks of
the kind are finished, then the median of the real-to-estimated ratios."
  (let ((ratios (loop for task in tasks
                      when (and (getf task :done)
                                (string= (getf task :category) category)
                                (getf task :real) (getf task :minutes)
                                (plusp (getf task :minutes)))
                        collect (/ (getf task :real) (getf task :minutes)))))
    (if (>= (length ratios) +min-finished+)
        (values (median ratios) (length ratios))
        (values 1 (length ratios)))))

(defun open-tasks (state)
  (loop for task in (getf state :tasks) unless (getf task :done) collect task))

(defun task-order (task)
  "Due first, then priority, then the order the user added them in."
  (list (if (plusp (length (getf task :due))) (getf task :due) "9999-99-99")
        (getf task :priority)
        (getf task :id)))

(defun ordered-tasks (state)
  (sort (open-tasks state) #'string< :key (lambda (task) (format nil "~{~a~^|~}" (task-order task)))))

;;; --- the calendar ---------------------------------------------------------------

(defun calendar-text (location)
  "LOCATION -- an .ics file or an https address -- as (values TEXT NOTE)."
  (cond ((zerop (length (or location ""))) (values nil nil))
        ((and (>= (length location) 8) (string-equal (subseq location 0 8) "https://"))
         (multiple-value-bind (body status) (nlk:http :get location :timeout 20)
           (if (and (integerp status) (< status 300) (stringp body))
               (values body nil)
               (values nil (format nil "the calendar address answered ~a" status)))))
        (t
         (if (probe-file location)
             (values (uiop:read-file-string location) nil)
             (values nil (format nil "no calendar file at ~a" location))))))

(defun ics-lines (text)
  "TEXT unfolded: a line starting with a space or a tab continues the one before."
  (let ((lines '()) (current ""))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((line (string-right-trim '(#\Return) line)))
        (if (and (plusp (length line)) (member (char line 0) '(#\Space #\Tab)))
            (setf current (concatenate 'string current (string-trim " " line)))
            (progn (when (plusp (length current)) (push current lines))
                   (setf current line)))))
    (when (plusp (length current)) (push current lines))
    (nreverse lines)))

(defun ics-raw-events (text)
  "TEXT's VEVENTs as plists (:summary :start :end), the values as written."
  (let ((events '()) (current nil))
    (dolist (line (ics-lines text))
      (cond ((string-equal line "BEGIN:VEVENT") (setf current (list :summary "" :start nil :end nil)))
            ((string-equal line "END:VEVENT")
             (when (and current (getf current :start)) (push current events))
             (setf current nil))
            ((null current))
            ((and (> (length line) 8) (string-equal (subseq line 0 8) "DTSTART:"))
             (setf (getf current :start) (subseq line 8)))
            ((and (> (length line) 6) (string-equal (subseq line 0 6) "DTEND:"))
             (setf (getf current :end) (subseq line 6)))
            ((and (> (length line) 8) (string-equal (subseq line 0 8) "SUMMARY:"))
             (setf (getf current :summary) (subseq line 8)))))
    (nreverse events)))

(defun ics-moment (value)
  "VALUE -- 20261107T090000, or with a Z, or 20261107 -- as (values DAY
MINUTES ALL-DAY-P); a Z is read as the UTC moment it names, shifted to the
machine's own clock, and a bare time is the wall clock the file states."
  (let* ((text (string-trim " " (or value "")))
         (utc (and (plusp (length text)) (char-equal (char text (1- (length text))) #\Z)))
         (body (if utc (subseq text 0 (1- (length text))) text)))
    (cond ((= (length body) 8)
           (values (date-int (parse-integer body :start 0 :end 4)
                              (parse-integer body :start 4 :end 6)
                              (parse-integer body :start 6 :end 8))
                   0 t))
          ((>= (length body) 13)
           (let ((day (date-int (parse-integer body :start 0 :end 4)
                                 (parse-integer body :start 4 :end 6)
                                 (parse-integer body :start 6 :end 8)))
                 (minutes (+ (* 60 (parse-integer body :start 9 :end 11))
                             (parse-integer body :start 11 :end 13))))
             (if utc
                 (multiple-value-bind (year month date) (date-parts day)
                   (multiple-value-bind (second minute hour dd mm yy)
                       (decode-universal-time (encode-universal-time 0 minutes 0 date month year 0))
                     (declare (ignore second))
                     (values (date-int yy mm dd) (+ (* hour 60) minute) nil)))
                 (values day minutes nil))))
          (t (values nil 0 nil)))))

(defun calendar-events (location day)
  "The events of DAY, as (:summary :start :end :all-day) with minutes from
its midnight, and what could not be read as a second value."
  (multiple-value-bind (text note) (calendar-text location)
    (let ((events '()))
      (dolist (raw (and text (ics-raw-events text)))
        (multiple-value-bind (start-day start-minute all-day) (ics-moment (getf raw :start))
          (multiple-value-bind (end-day end-minute) (ics-moment (getf raw :end))
            (when start-day
              (let* ((start (+ (* 1440 (date-diff start-day day)) start-minute))
                     (end (if end-day
                              (+ (* 1440 (date-diff end-day day)) end-minute)
                              (+ start (if all-day 1440 60)))))
                (when (and (< start 1440) (> end 0))
                  (push (list :summary (getf raw :summary)
                              :start (max 0 start)
                              :end (min end 1440)
                              :all-day all-day)
                        events)))))))
      (values (nreverse events) (and note (list note))))))

;;; --- the day ---------------------------------------------------------------------

(defun block-why (task planned factor count index total)
  "Why one block is where it is: the correction, the due date, the priority,
the focus window and the split, each said only when it is true."
  (let ((why '()))
    (push (if (>= count +min-finished+)
              (if (/= factor 1)
                  (format nil "your estimate ~d min, ~d finished of ~a run ~,2fx: ~d min"
                          (getf task :minutes) count (getf task :category) (float factor) planned)
                  (format nil "your estimate ~d min; ~d finished of ~a held it" (getf task :minutes) count (getf task :category)))
              (format nil "your estimate ~d min, ~d finished of ~a so far" (getf task :minutes) count (getf task :category)))
          why)
    (let ((due (getf task :due)))
      (unless (zerop (length due))
        (push (format nil "due ~a" due) why)))
    (push (format nil "priority ~d of 3" (getf task :priority)) why)
    (when index
      (push (format nil "block ~d of ~d, split to the longest block" (1+ index) total) why))
    why))

(defun plan-day (state day)
  "DAY's plan as a plist: the window, the events, the free time, the blocks
placed with their reasons, and what does not fit."
  (let* ((rules (rules-of state))
         (buffer (getf rules :buffer-min))
         (max-block (getf rules :max-block-min))
         (cap (getf rules :max-tasks-per-day))
         (focus (focus-windows (getf rules :focus)))
         (protected (guarded-windows (getf rules :protected) "protected"))
         (work (parse-range (getf rules :work-hours) "work_hours")))
    (multiple-value-bind (events notes) (calendar-events (getf rules :calendar) day)
      (let ((ordered (ordered-tasks state)))
        (if (not (member (date-weekday day) (workdays-of (getf rules :workdays))))
            (list :day day :workday-p nil :window work :events events :notes notes
                  :free-minutes 0 :left 0 :blocks '()
                  :dropped (loop for task in ordered
                                 collect (list :name (getf task :name)
                                               :why "not a workday")))
            (let* ((start (car work))
                   (start (if (and (= day (today)) (> (now-minutes) start))
                              (min (cdr work) (ceil-step (now-minutes)))
                              start))
                   (window (cons start (cdr work)))
                   (busy (append (loop for event in events
                                       unless (getf event :all-day)
                                         collect (cons (- (getf event :start) buffer)
                                                       (+ (getf event :end) buffer)))
                                 (loop for window in protected
                                       collect (cons (second window) (third window)))))
                   (pool (split-focus (subtract-spans window busy) focus))
                   (free (loop for (from to sharp) in pool sum (- to from)))
                   (chosen (subseq ordered 0 (min cap (length ordered))))
                   (over (nthcdr cap ordered))
                   (blocks '())
                   (dropped (loop for task in over
                                  collect (list :name (getf task :name)
                                                :why (format nil "the cap: ~d task~:p a day" cap)))))
              (dolist (task chosen)
                (multiple-value-bind (factor count) (factor-of (getf state :tasks) (getf task :category))
                  (let* ((estimate (or (getf task :minutes) +default-estimate+))
                         (planned (ceil-step (* estimate factor)))
                         (chunks (chunk-sizes planned max-block)))
                    (multiple-value-bind (placed rest ok) (place-chunks pool chunks)
                      (if (not ok)
                          (push (list :name (getf task :name)
                                      :why (format nil "no free stretch of ~d min before ~a"
                                                   max-block (clock (cdr window))))
                                dropped)
                          (progn
                            (setf pool rest)
                            (loop for (from . to) in placed
                                  for index from 0
                                  do (push (list :start from :end to
                                                 :name (getf task :name)
                                                 :task (getf task :id)
                                                 :minutes planned
                                                 :why (block-why task planned factor count
                                                                  (if (> (length placed) 1) index nil)
                                                                  (length placed)))
                                           blocks))))))))
              (list :day day :workday-p t :window window :events events :notes notes
                    :free-minutes free
                    :left (loop for (from to sharp) in pool sum (- to from))
                    :blocks (sort blocks #'< :key (lambda (block) (getf block :start)))
                    :dropped (nreverse dropped))))))))

(defun minutes-text (minutes)
  (cond ((not (integerp minutes)) (format nil "~a" minutes))
        ((>= minutes 60) (format nil "~dh~2,'0d" (floor minutes 60) (mod minutes 60)))
        (t (format nil "~d min" minutes))))

(defun rule-lines (state)
  (loop for (key value source) in (rule-summary state)
        collect (format nil "~a ~a (~a)" (wire-rule key)
                        (if (listp value) (format nil "~{~a~^, ~}" value) value)
                        source)))

(defun plan-text (plan state)
  "PLAN as the lines a person reads: each block with its reason, and what
does not fit."
  (let ((window (getf plan :window))
        (day (getf plan :day)))
    (with-output-to-string (out)
      (format out "~a ~a, work ~a-~a~%" (weekday-name day) (date-string day)
              (clock (car window)) (clock (cdr window)))
      (dolist (event (getf plan :events))
        (format out "  ~a ~a-~a~@[ (all day)~]~%" (getf event :summary)
                (clock (getf event :start)) (clock (getf event :end))
                (getf event :all-day)))
      (dolist (note (getf plan :notes)) (format out "  ! ~a~%" note))
      (if (getf plan :workday-p)
          (progn
            (format out "Free: ~a~%" (minutes-text (getf plan :free-minutes)))
            (dolist (block (getf plan :blocks))
              (format out "~a-~a ~a~%" (clock (getf block :start)) (clock (getf block :end))
                      (getf block :name))
              (dolist (why (getf block :why)) (format out "    ~a~%" why)))
            (when (null (getf plan :blocks)) (format out "Nothing placed.~%")))
          (format out "Not a workday: nothing placed.~%"))
      (dolist (drop (getf plan :dropped))
        (format out "Not placed: ~a (~a)~%" (getf drop :name) (getf drop :why)))
      (when (getf plan :workday-p)
        (format out "~a of free time left.~%" (minutes-text (getf plan :left))))
      (format out "Rules: ~{~a~^; ~}.~%" (rule-lines state)))))

(defun tasks-text (state)
  (with-output-to-string (out)
    (let ((open (ordered-tasks state)))
      (if (null open)
          (format out "No open task.~%")
          (dolist (task open)
            (format out "~a [~a] ~a, priority ~d~@[ (due ~a)~]: ~a~%"
                    (getf task :id) (getf task :category)
                    (minutes-text (getf task :minutes)) (getf task :priority)
                    (unless (zerop (length (getf task :due))) (getf task :due))
                    (getf task :name))))
      (format out "~d finished.~%" (count-if (lambda (task) (getf task :done)) (getf state :tasks))))))

;;; --- the tools -------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "additionalProperties" :false
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *add-schema*
  (tool-schema
   "Add a task to the day planner. An estimate in minutes makes the plan honest; without one the planner assumes 30 and says so. The category groups tasks so their finished times correct this one's estimate."
   '(("name" "string" "The task, in the user's words." t)
     ("minutes" "integer" "How long the user thinks it takes, 5 to 480.")
     ("category" "string" "A kind of work: writing, admin, sport. Default general.")
     ("priority" "integer" "1 is done first, 3 last. Default 2.")
     ("due" "string" "YYYY-MM-DD, when it is due.")))
  "The agenda_add_task tool.")

(defparameter *tasks-schema*
  (tool-schema
   "List the open tasks in the order the planner would take them, with their estimates and due dates, and how many are finished."
   '())
  "The agenda_tasks tool.")

(defparameter *done-schema*
  (tool-schema
   "Report how long a task really took. From the third finished task of a kind the planner corrects that kind's estimates by the median of the user's own errors, and says so."
   '(("id" "string" "The task's id, from agenda_tasks." t)
     ("minutes" "integer" "The real time it took, 1 to 960." t)))
  "The agenda_done tool.")

(defparameter *plan-schema*
  (tool-schema
   "Plan a day: the tasks placed in the free time around the calendar's events, inside the user's rules, each block with the reason it is there, and what does not fit. Deterministic: the same tasks, rules and calendar give the same plan."
   '(("day" "string" "YYYY-MM-DD, today or tomorrow. Default today.")))
  "The agenda_plan tool.")

(defparameter *rules-schema*
  (tool-schema
   "Read the rules the plan obeys, and change any of them. A rule changed here is the user's, and outranks the section's value; the answer says where each rule came from."
   '(("work_hours" "string" "HH:MM-HH:MM, the window tasks may be placed in.")
     ("workdays" "string" "mon-fri, or 1,2,3.")
     ("protected" "string" "name HH:MM-HH:MM, several by ;, never planned.")
     ("focus" "string" "HH:MM-HH:MM, the sharpest hours, filled first.")
     ("calendar" "string" "An .ics file or an https address; empty for none.")
     ("buffer_min" "integer" "Minutes kept free on each side of an event, 0 to 60.")
     ("max_block_min" "integer" "The longest block, 15 to 120 minutes.")
     ("max_tasks_per_day" "integer" "How many tasks a day takes, 1 to 10.")))
  "The agenda_rules tool.")

(defun tool-add (args)
  (answering
   (let ((name (string-trim " " (or (argument-text args "name") "")))
         (minutes (argument-integer args "minutes"))
         (priority (argument-integer args "priority")))
     (when (zerop (length name)) (fail "name is required"))
     (when (and minutes (or (< minutes 5) (> minutes 480))) (fail "minutes is 5 to 480"))
     (when (and priority (or (< priority 1) (> priority 3))) (fail "priority is 1 to 3"))
     (with-store (state)
       (let* ((id (format nil "t~d" (1+ (getf state :seq))))
              (task (list :id id :name name
                          :minutes (or minutes +default-estimate+)
                          :category (or (argument-text args "category") "general")
                          :priority (or priority 2)
                          :due (or (argument-text args "due") "")
                          :done nil :real nil)))
         (setf (getf state :seq) (1+ (getf state :seq)))
         (push task (getf state :tasks))
         (nlk:encode-json-object
          (nlk:json-object
           "ok" t
           "id" id
           "task" (task-json task)
           "message" (format nil "~a ~@[, ~a~], priority ~d~@[, due ~a~]"
                              (getf task :name)
                              (if minutes (minutes-text (getf task :minutes))
                                  (format nil "~a assumed" (minutes-text +default-estimate+)))
                              (getf task :priority)
                              (unless (zerop (length (getf task :due))) (getf task :due))))))))))

(defun tool-tasks (args)
  (declare (ignore args))
  (answering
   (let ((state (read-state)))
     (nlk:encode-json-object
      (nlk:json-object
       "ok" t
       "open" (coerce (loop for task in (ordered-tasks state) collect (task-json task)) 'vector)
       "finished" (count-if (lambda (task) (getf task :done)) (getf state :tasks))
       "text" (tasks-text state))))))

(defun tool-done (args)
  (answering
   (let ((id (argument-text args "id"))
         (real (argument-integer args "minutes")))
     (unless (and id (plusp (length id))) (fail "id is required"))
     (unless (and real (<= 1 real 960)) (fail "minutes is required: the real time it took, 1 to 960"))
     (with-store (state)
       (let ((task (find id (getf state :tasks) :key (lambda (item) (getf item :id)) :test #'string=)))
         (unless task (fail "no task ~a" id))
         (setf (getf task :done) t)
         (setf (getf task :real) real)
         (multiple-value-bind (factor count) (factor-of (getf state :tasks) (getf task :category))
           (nlk:encode-json-object
            (nlk:json-object
             "ok" t
             "task" (task-json task)
             "category" (getf task :category)
             "finished" count
             "factor" (float factor)
             "message" (if (>= count +min-finished+)
                            (format nil "~d finished of ~a: their real times run ~,2fx your estimates, and the plan now uses that"
                                    count (getf task :category) (float factor))
                            (format nil "~d finished of ~a; from the third the plan corrects that kind's estimates"
                                    count (getf task :category)))))))))))

(defun tool-plan (args)
  (answering
   (let* ((state (read-state))
          (day (parse-date (argument-text args "day") (today)))
          (plan (plan-day state day)))
     (nlk:encode-json-object
      (nlk:json-object
       "ok" t
       "day" (date-string day)
       "workday" (if (getf plan :workday-p) t :false)
       "text" (plan-text plan state)
       "blocks" (coerce (loop for block in (getf plan :blocks)
                               collect (nlk:json-object
                                        "start" (clock (getf block :start))
                                        "end" (clock (getf block :end))
                                        "name" (getf block :name)
                                        "why" (coerce (getf block :why) 'vector)))
                        'vector)
       "dropped" (coerce (loop for drop in (getf plan :dropped)
                                collect (nlk:json-object "name" (getf drop :name)
                                                          "why" (getf drop :why)))
                          'vector)
       "free_minutes" (getf plan :free-minutes)
       "warnings" (coerce (getf plan :notes) 'vector))))))

(defun tool-rules (args)
  (answering
   (with-store (state)
     (loop for (wire key) in '(("work_hours" :work-hours) ("workdays" :workdays)
                               ("protected" :protected) ("focus" :focus)
                               ("calendar" :calendar) ("buffer_min" :buffer-min)
                               ("max_block_min" :max-block-min)
                               ("max_tasks_per_day" :max-tasks-per-day))
           for value = (argument args wire)
           when value
             do (cond ((member key '(:buffer-min :max-block-min :max-tasks-per-day))
                       (if (integerp value)
                           (set-rule state key value)
                           (fail "~a wants an integer" wire)))
                      ((stringp value) (set-rule state key value))
                      (t (fail "~a wants text" wire))))
     (nlk:encode-json-object
      (nlk:json-object
       "ok" t
       "rules" (coerce (loop for (key value source) in (rule-summary state)
                               collect (nlk:json-object
                                        "key" (wire-rule key)
                                        "value" (if (listp value) (format nil "~a" value) value)
                                        "source" source))
                        'vector))))))

;;; --- the command -----------------------------------------------------------------

(defun parse-duration (text)
  "TEXT -- 30m, 1h, 1h30, 45 -- as minutes, or NIL when it is not a length."
  (let* ((lower (string-downcase (or text "")))
         (hours (position #\h lower))
         (minutes (position #\m lower)))
    (cond ((and hours (plusp hours) (every #'digit-char-p (subseq lower 0 hours)))
           (+ (* 60 (parse-integer lower :end hours))
              (if (and minutes (> minutes (1+ hours))
                       (every #'digit-char-p (subseq lower (1+ hours) minutes)))
                  (parse-integer lower :start (1+ hours) :end minutes)
                  0)))
          ((and minutes (plusp minutes) (every #'digit-char-p (subseq lower 0 minutes)))
           (parse-integer lower :end minutes))
          ((and (plusp (length lower)) (every #'digit-char-p lower))
           (parse-integer lower))
          (t nil))))

(defun command-add (words)
  (let* ((minutes (parse-duration (first words)))
         (name (string-trim " " (format nil "~{~a~^ ~}" (if minutes (rest words) words)))))
    (when (zerop (length name)) (fail "add wants a task: add 30m write the report"))
    (with-store (state)
      (let* ((id (format nil "t~d" (1+ (getf state :seq))))
             (task (list :id id :name name :minutes (or minutes +default-estimate+)
                         :category "general" :priority 2 :due "" :done nil :real nil)))
        (setf (getf state :seq) (1+ (getf state :seq)))
        (push task (getf state :tasks))
        (format nil "~a added as ~a, ~a~@[ (no estimate given, ~a assumed)~]."
                name id (minutes-text (getf task :minutes)) (null minutes)
                (minutes-text +default-estimate+))))))

(defun command-done (words)
  (let ((id (first words))
        (real (parse-duration (second words))))
    (unless (and id real) (fail "done wants the id and the real time: done t3 45m"))
    (with-store (state)
      (let ((task (find id (getf state :tasks) :key (lambda (item) (getf item :id)) :test #'string=)))
        (unless task (fail "no task ~a" id))
        (setf (getf task :done) t)
        (setf (getf task :real) real)
        (multiple-value-bind (factor count) (factor-of (getf state :tasks) (getf task :category))
          (format nil "~a finished in ~a, estimated ~a. ~d finished of ~a~@[, the plan now uses your ~,2fx~]."
                  (getf task :name) (minutes-text real) (minutes-text (getf task :minutes))
                  count (getf task :category)
                  (and (>= count +min-finished+) (/= factor 1) (float factor))))))))

(defun command-set (words)
  (let* ((text (format nil "~{~a~^ ~}" words))
         (equals (position #\= text)))
    (unless equals (fail "set wants key=value, as set max_tasks_per_day=2"))
    (let* ((key-text (string-downcase (substitute #\_ #\- (string-trim " " (subseq text 0 equals)))))
           (value (string-trim " " (subseq text (1+ equals))))
           (key (cdr (assoc key-text
                             '(("work_hours" . :work-hours) ("workdays" . :workdays)
                               ("protected" . :protected) ("focus" . :focus)
                               ("calendar" . :calendar) ("buffer_min" . :buffer-min)
                               ("max_block_min" . :max-block-min)
                               ("max_tasks_per_day" . :max-tasks-per-day))
                             :test #'string=))))
      (unless key (fail "~s is not one of the rules" key-text))
      (with-store (state)
        (set-rule state key (if (member key '(:buffer-min :max-block-min :max-tasks-per-day))
                                (or (parse-integer value :junk-allowed t)
                                    (fail "~a wants a number" key-text))
                                value))
        (format nil "~a is now ~s, changed in the chat; the section's value is kept behind it."
                key-text value)))))

(defun command-summary (state)
  (format nil "~{~a~%~}"
          (list "Agenda Guardian: a day planned from your rules, your calendar and your own real times."
                "/agenda-guardian plan [day] - the day's blocks, each with its reason, and what does not fit"
                "/agenda-guardian tasks - the open tasks in the order the planner takes them"
                "/agenda-guardian add 30m write the report - a task, with its estimate"
                "/agenda-guardian done t1 45m - the real time it took"
                (format nil "/agenda-guardian rules - the rules in force; set max_tasks_per_day=2 changes one (~d task~:p a day now)"
                        (rule-of state :max-tasks-per-day)))))

(defun run-command (args session-id)
  "One /agenda-guardian call: its text, or the refusal AGENDA-GUARDIAN-ERROR."
  (declare (ignore session-id))
  (let* ((state (read-state))
         (words (uiop:split-string (string-trim " " (or args "")) :separator " "))
         (verb (string-downcase (or (first words) "")))
         (rest (rest words)))
    (cond ((member verb '("" "help") :test #'string=) (command-summary state))
          ((string= verb "plan") (plan-text (plan-day state (parse-date (or (first rest) "") (today))) state))
          ((member verb '("today" "tomorrow") :test #'string=)
           (plan-text (plan-day state (parse-date verb (today))) state))
          ((string= verb "tasks") (tasks-text state))
          ((string= verb "rules") (format nil "~{~a~%~}" (rule-lines state)))
          ((string= verb "add") (command-add rest))
          ((string= verb "done") (command-done rest))
          ((string= verb "set") (command-set rest))
          (t (fail "~s is not one of plan, tasks, add, done, rules or set" verb)))))

;;; --- the cell --------------------------------------------------------------------

(defparameter +primer+
  "Agenda Guardian plans a day the way a person with more tasks than hours needs it: the rules are the user's (work hours, protected windows, the sharpest hours, the longest block, the tasks a day), every task carries an estimate, and from the third finished task of a kind the planner corrects that kind's estimates by the median of the user's own errors and says so. A plan places tasks in the free time around the calendar's events, splits a task into blocks no longer than max_block_min, prefers the focus window, and says what does not fit instead of overbooking. Nothing is invented: the same tasks, rules and calendar give the same plan, and no model call is made. The calendar is an .ics file or an https address the section names; only the plan reads it. agenda_plan answers the day's text and its blocks, agenda_add_task and agenda_done are the estimate and the real time, agenda_tasks lists what is open, and agenda_rules reads or changes a rule in the chat (a changed rule outranks the section's, and the answer says so). /agenda-guardian is the same planner without the model: plan, tasks, add, done, rules, set.")

(nle:define-cell agenda-guardian
  (:section ("agenda-guardian")
    (:guide "data_dir is the folder the planner keeps its tasks in; calendar is an .ics file or an https address (empty plans around nothing); the rules the plan obeys are work_hours, workdays, protected, focus, buffer_min, max_block_min and max_tasks_per_day, and /agenda-guardian plan shows the first alone")
    ("data_dir" :path :required t
     :doc "the folder the planner keeps agenda.json in")
    ("calendar" :string :default ""
     :doc "an .ics file or an https address whose events the plan goes around; empty plans around nothing")
    ("work_hours" :string :default "09:00-18:00"
     :doc "the window tasks may be placed in, HH:MM-HH:MM")
    ("workdays" :string :default "mon-fri"
     :doc "the days worked: mon-fri, or 1,2,3")
    ("protected" :string :default "lunch 12:30-13:30"
     :doc "windows never planned: name HH:MM-HH:MM, several by ;")
    ("focus" :string :default "09:00-11:30"
     :doc "the sharpest hours, filled first: HH:MM-HH:MM, several by ;")
    ("buffer_min" :integer :default 10 :min 0
     :doc "minutes kept free on each side of an event")
    ("max_block_min" :integer :default 50 :min 15
     :doc "the longest one block, in minutes")
    ("max_tasks_per_day" :integer :default 3 :min 1
     :doc "how many tasks a day takes"))
  (:help :agenda-guardian "agenda-guardian: a day planned around your rules, your calendar and your own real times -- agenda_add_task, agenda_tasks, agenda_done, agenda_plan, agenda_rules, /agenda-guardian" +primer+)
  (:command "agenda-guardian" 'run-command
            :catches 'agenda-guardian-error
            :description "The day's plan and its reasons, the tasks, the rules, or a task added or finished"
            :argument-hint "plan [day] | tasks | add 30m NAME | done ID MINUTES | rules | set KEY=VALUE")
  (:tool "agenda_add_task" *add-schema* 'tool-add)
  (:tool "agenda_tasks" *tasks-schema* 'tool-tasks)
  (:tool "agenda_done" *done-schema* 'tool-done)
  (:tool "agenda_plan" *plan-schema* 'tool-plan)
  (:tool "agenda_rules" *rules-schema* 'tool-rules)
  (:start (lambda ()
            ;; A planner needs a folder before it can have a file: a section
            ;; that names none refuses at start, naming the member, the way the
            ;; original's missing data folder does.
            (unless (plusp (length (or (setting :data-dir) "")))
              (error 'nlk:config-refusal
                     :detail "agenda-guardian: data_dir names the folder the planner keeps its tasks in; set it in the agenda-guardian section of config.jsonc or in /settings")))))
