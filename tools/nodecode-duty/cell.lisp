;;;; cell.lisp --- the board of what waits on the person, and the words it says.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One read-only view of the things a person comes back to: a conversation
;;;; whose newest answer asked them something and waits, a turn running right
;;;; now (flagged once it has been quiet too long), a turn that started and
;;;; left no answer, background work whose result was never delivered, the
;;;; cron jobs that failed, were paused or are owed an answer, and the
;;;; standing goals of this home.
;;;;
;;;; Nothing is changed and nothing is written: the session directory, the
;;;; cron cell's registry row and the goal facts are read through their
;;;; public readers, and every source may be absent -- an empty home reads as
;;;; all clear. Two ways in: the `duty' tool (the model reads it and tells
;;;; the operator) and /duty in the shell.

(in-package #:nodecode-duty)

(nlk:define-peripheral duty :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "(duty) reads what is waiting on the person in this home, read-only: conversations whose newest
answer asked something and waits, with how long it has waited (Waiting on you), the turns running
right now with their running time and a quiet flag past duty.stall_minutes (Running), turns that
started and left no answer (Started, no answer), background work whose result was never delivered
and cron jobs that failed, were paused or are owed an answer (Cron trouble), and the standing goals
of this home with their turn budget (Standing goals). It calls no model and reads no message text:
what a conversation waits on is the question the harness recorded on it. Everything is read-only; a
source that cannot be read is named in the Not read footer instead of failing the read. The tool's
section argument narrows it to waiting, running, unfinished, cron or goals; /duty takes the same
word, and /duty counts prints the one-line count instead. (NODECODE-DUTY:SNAPSHOT) answers the
same data as a plist, and (NODECODE-DUTY:DUTY-TEXT :ONLY \"waiting\") is the text."
  "What (help :duty) answers while the cell runs.")

(defparameter +sections+ '("waiting" "running" "unfinished" "cron" "goals")
  "The sections the tool and /duty can be narrowed to, in the order they print.")

(defparameter +section-options+ (cons "" +sections+)
  "What the tool's section member may be, empty for the whole board.")

(nlk:define-contract duty-query
  (:doc "Read the duty board: what is waiting on the person in this Nodecode home -- conversations
whose newest answer asked a question and waits, turns running now with a quiet flag, turns that
started and left no answer, background results not yet delivered, cron jobs that failed, were paused
or are owed an answer, and the standing goals. Read-only, model-free and cheap: read it when the
operator asks what is waiting on them, what got stuck or what broke while they were away.")
  ("section" :text :default "" :options +section-options+
             :doc "one section of the board, or empty for all of it"))

;;; --- small text helpers --------------------------------------------------------

(defparameter +unix-epoch+ 2208988800
  "Universal time at 1970-01-01T00:00:00Z: unix seconds plus this, and back.")

(defparameter +turn-started+ "turn.started"
  "The turn fact the session directory records on a session whose newest turn
has no ending yet: `turn.started' while it runs, or while nothing ever ended it.")

(defparameter +goal-set+ "goal.set"
  "The fact a goal is set in, as the goal cell writes it.")

(defparameter +goal-ended+ "goal.ended"
  "The fact a goal is over in, as the goal cell writes it.")

(defun flatten-line (text &optional (max 200))
  "TEXT on one line, at most MAX characters: a failure carries newlines."
  (let* ((parts (uiop:split-string (princ-to-string (or text ""))
                                    :separator '(#\Newline #\Return)))
         (flat (format nil "~{~a~^ ~}" (remove "" parts :test #'string=)))
         (trimmed (string-trim '(#\Space #\Tab) flat)))
    (if (> (length trimmed) max) (subseq trimmed 0 max) trimmed)))

(defun age-text (seconds)
  "A duration the way the board writes it: 40s, 12m, 3h13m, 2d4h."
  (let ((s (if (and (realp seconds) (plusp seconds)) (floor seconds) 0)))
    (cond ((< s 60) (format nil "~ds" s))
          ((< s 3600) (format nil "~dm" (floor s 60)))
          ((< s 86400) (multiple-value-bind (hours rest) (floor s 3600)
                         (format nil "~dh~2,'0dm" hours (floor rest 60))))
          (t (multiple-value-bind (days rest) (floor s 86400)
               (format nil "~dd~dh" days (floor rest 3600)))))))

(defun stamp-seconds (value)
  "VALUE, a stamp out of the session directory, as how many seconds ago it was;
NIL when it is not a stamp."
  (cond ((integerp value)
         (let ((now (get-universal-time)))
           (cond ((> value 100000000000) (max 0 (- now (floor value 1000))))
                 ((> value 3000000000) (max 0 (- now value)))
                 (t (max 0 (- (- now +unix-epoch+) value))))))
        (t nil)))

(defun past-text (seconds)
  "How long ago SECONDS was, or a question mark when it is not a number."
  (if (realp seconds) (age-text seconds) "?"))

(defun ahead-text (unix)
  "How far away a unix-seconds stamp is."
  (cond ((not (integerp unix)) "?")
        ((> unix 100000000000)
         (age-text (max 0 (- (floor unix 1000) (- (get-universal-time) +unix-epoch+)))))
        (t (age-text (max 0 (- (+ unix +unix-epoch+) (get-universal-time)))))))

(defun first-n (list n)
  "The first N of LIST."
  (if (and (integerp n) (plusp n) (> (length list) n)) (subseq list 0 n) list))

(defun wanted-p (name only)
  "Whether section NAME prints: every section when ONLY is empty, else one."
  (or (null only)
      (zerop (length (string-trim " " (or only ""))))
      (string= name only)))

(defun session-name (session)
  "A session as a name: its title when it has one, else its id."
  (let ((title (getf session :title)))
    (if (and (stringp title) (plusp (length title))) title (getf session :id))))

;;; --- the readers ----------------------------------------------------------------

(defun read-sessions ()
  "The session directory, newest activity first: what waits on the person and
which turn is the newest one."
  (cond ((not (nlk:store-open-p)) (values '() "absent (no store)"))
        (t (handler-case
                 (values (loop for row in (nlk:list-sessions)
                               collect (list :id (getf row :id)
                                             :title (or (getf row :title) "")
                                             :source (let ((source (getf row :source)))
                                                       (if (stringp source) source "?"))
                                             :updated-at (getf row :updated-at)
                                             :turn-kind (getf row :turn-kind)
                                             :waits (let ((waits (getf row :waits)))
                                                      (and (stringp waits) waits))))
                         "read")
               (error () (values '() "unreadable (session directory)"))))))

(defun read-cron ()
  "The cron registry row, when jobs were ever written: their state, the last
outcome and the answer still owed. Read, never written."
  (cond ((not (nlk:store-open-p)) (values '() "absent (no store)"))
        (t (let ((text (nlk:session-state-get "cron" "jobs")))
             (cond ((null text) (values '() "absent (no cron jobs)"))
                   (t (handler-case
                          (let ((document (nlk:decode-json text)))
                            (values
                             (loop for job across (or (nlk:json-array document "jobs") #())
                                   for last = (nlk:json-value job :object "last")
                                   for report = (nlk:json-value job :object "report")
                                   collect (list :kind "cron"
                                                 :id (or (nlk:json-value job :string "id") "?")
                                                 :name (or (nlk:json-value job :string "name")
                                                           (nlk:json-value job :string "id")
                                                           "?")
                                                 :state (or (nlk:json-value job :string "state") "?")
                                                 :next-at (nlk:json-value job :integer "next_at")
                                                 :last-status (nlk:json-value last :string "status")
                                                 :last-line (let ((line (nlk:json-value last :string "line")))
                                                              (and line (flatten-line line 160)))
                                                 :report (and report
                                                              (flatten-line
                                                               (or (nlk:json-value report :string "answer") "")
                                                               100))))
                             "read"))
                        (error () (values '() "unreadable (cron registry)")))))))))

(defun goal-ended-p (id)
  "Whether the goal ID has ended, as the goal cell records it."
  (plusp (or (nlk:events :aggregate-type "goal" :aggregate-id id :kind +goal-ended+
                          :as :count)
             0)))

(defun read-goals ()
  "The goals set in this home that have not ended, oldest first."
  (cond ((not (nlk:store-open-p)) (values '() "absent (no store)"))
        (t (handler-case
                 (values (loop for (session payload) in (nlk:events :kind +goal-set+ :as :rows
                                                                    :columns '("session_id" "payload"))
                               for object = (ignore-errors (nlk:decode-json payload))
                               for id = (and object (nlk:json-value object :string "goal"))
                               when (and id (not (goal-ended-p id)))
                                 collect (list :kind "goal" :id id :session session
                                               :condition (or (nlk:json-value object :string "condition") "")
                                               :check (nlk:json-value object :string "check")
                                               :turns (nlk:json-value object :integer "turns")))
                         "read")
               (error () (values '() "unreadable (goal facts)"))))))

(defvar *sessions-reader* #'read-sessions
  "The session directory reader; a test stands in for it.")
(defvar *cron-reader* #'read-cron
  "The cron registry reader; a test stands in for it.")
(defvar *goals-reader* #'read-goals
  "The goal facts reader; a test stands in for it.")
(defvar *active-turn-p* #'nlk:active-turn-p
  "Whether a session has a turn running; a test stands in for it.")

;;; --- the snapshot --------------------------------------------------------------

(defparameter +ok-cron-statuses+ '(nil "" "ok" "success")
  "A cron run that finished well: anything else needs attention.")

(defun cron-failed-p (job)
  "Whether a cron job's last run failed."
  (let ((status (getf job :last-status)))
    (and status (not (member status +ok-cron-statuses+ :test #'equal)))))

(defun default-limit () (or (ignore-errors (setting :limit)) 20))
(defun default-stall () (or (ignore-errors (setting :stall-minutes)) 10))
(defun default-window-hours () (or (ignore-errors (setting :window-hours)) 72))

(defun fresh-p (session window)
  "Whether SESSION was touched inside the WINDOW of seconds this board watches."
  (let ((age (stamp-seconds (getf session :updated-at))))
    (or (null age) (<= age window))))

(defun snapshot (&key (limit nil))
  "The whole board as a plist: :GENERATED-AT, the five sections and :SOURCES.
Nothing is changed and nothing is written."
  (let ((n (or limit (default-limit)))
        (window (* 3600 (default-window-hours)))
        (waiting '()) (running '()) (unfinished '()) (trouble '()) (goals '()))
    (multiple-value-bind (sessions sessions-note) (funcall *sessions-reader*)
      (multiple-value-bind (jobs cron-note) (funcall *cron-reader*)
        (multiple-value-bind (standing goals-note) (funcall *goals-reader*)
          (dolist (session sessions)
            (when (fresh-p session window)
              (let* ((running-p (and (funcall *active-turn-p* (getf session :id)) t))
                     (age (stamp-seconds (getf session :updated-at)))
                     (item (list* :kind "session" :age age session)))
                (cond (running-p (push item running))
                      ((getf session :waits) (push item waiting))
                      ((equal (getf session :turn-kind) +turn-started+) (push item unfinished))))))
          (dolist (job jobs)
            (when (or (cron-failed-p job)
                      (equal (getf job :state) "paused")
                      (getf job :report))
              (push job trouble)))
          (dolist (goal standing) (push goal goals))
          (list :generated-at (- (get-universal-time) +unix-epoch+)
                :waiting (first-n (sort waiting #'> :key (lambda (item) (or (getf item :age) 0))) n)
                :running (first-n (nreverse running) n)
                :unfinished (first-n (sort unfinished #'> :key (lambda (item) (or (getf item :age) 0))) n)
                :cron (first-n (nreverse trouble) n)
                :goals (first-n (nreverse goals) n)
                :sources (list (cons "sessions" sessions-note)
                               (cons "cron" cron-note)
                               (cons "goals" goals-note))))))))

;;; --- the glyphs ---------------------------------------------------------------

(defparameter +diamond+ (string (code-char #x25C6)) "The section mark.")
(defparameter +half+ (string (code-char #x25D0)) "A turn running now.")
(defparameter +cross+ (string (code-char #x2717)) "A turn that left no answer.")
(defparameter +warn+ (string (code-char #x26A0)) "A cron job in trouble.")
(defparameter +ring+ (string (code-char #x25CE)) "A goal still standing.")
(defparameter +dot+ (string (code-char #x00B7)) "The footer's bullet.")

;;; --- the text ------------------------------------------------------------------

(defparameter +source-labels+
  '(("sessions" . "the session directory (store)")
    ("cron" . "cron jobs (the cron registry row)")
    ("goals" . "standing goals (the goal facts)"))
  "What the Not read footer calls each source.")

(defun describe-waiting (item)
  "A conversation whose answer waits on the person, and how long it has."
  (format nil "~a  [~a]  waiting ~a~@[  ~s~]"
          (session-name item) (getf item :source) (past-text (getf item :age))
          (let ((waits (getf item :waits)))
            (and waits (plusp (length waits)) (flatten-line waits 120)))))

(defun describe-running (item stall)
  "A turn running now: how long, and whether it has gone quiet."
  (let ((age (getf item :age)))
    (format nil "~a  [~a]  running ~a~@[  quiet for ~a (stalled)~]"
            (session-name item) (getf item :source) (past-text age)
            (and stall (realp age) (past-text age)))))

(defun describe-unfinished (item)
  "A turn that started and left no answer."
  (format nil "~a  [~a]  started ~a ago, no answer yet"
          (session-name item) (getf item :source) (past-text (getf item :age))))

(defun describe-cron (job)
  "One cron job that needs the person: the failure, the pause, the owed answer."
  (format nil "cron '~a'~@[  last run FAILED: ~a~]~@[  last status: ~a~]~@[  paused~]~@[  answer awaiting delivery~]~@[  next in ~a~]"
          (getf job :name)
          (getf job :last-line)
          (let ((status (getf job :last-status)))
            (and status (not (member status +ok-cron-statuses+ :test #'equal)) status))
          (equal (getf job :state) "paused")
          (getf job :report)
          (and (getf job :next-at) (ahead-text (getf job :next-at)))))

(defun describe-goal (goal)
  "A standing goal: what it waits for, and the turn budget it is on."
  (format nil "goal ~a  [~a]~@[  ~d turn~:p~]~@[  ~a~]~@[  check: ~a~]"
          (getf goal :id) (getf goal :session) (getf goal :turns)
          (let ((condition (getf goal :condition)))
            (and condition (plusp (length condition)) (flatten-line condition 100)))
          (getf goal :check)))

(defun section-text (title items mark empty describe &key always)
  "One section of the board, or nothing when it has no rows and is not ALWAYS
printed."
  (when (or items always)
    (with-output-to-string (out)
      (format out "  ~a ~a (~d)~%" +diamond+ title (length items))
      (if items
          (dolist (item items) (format out "    ~a ~a~%" mark (funcall describe item)))
          (format out "    ~a~%" empty)))))

(defun duty-text (&key only (limit nil))
  "The board as a person reads it: the count line, the sections, and a Not
read footer naming every source that could not be read. ONLY narrows it to one
of +SECTIONS+; LIMIT bounds each list."
  (let* ((data (snapshot :limit limit))
         (stall (* 60 (default-stall)))
         (out (make-string-output-stream)))
    (labels ((put (text) (when text (princ text out) (terpri out))))
      (put (counts-text :data data))
      (when (wanted-p "waiting" only)
        (put (section-text "Waiting on you" (getf data :waiting) "?" "nothing waits on you"
                           #'describe-waiting :always t)))
      (when (wanted-p "running" only)
        (put (section-text "Running" (getf data :running) +half+ "nothing is running"
                           (lambda (item)
                             (describe-running item (and (getf item :age)
                                                         (> (getf item :age) stall))))
                           :always t)))
      (when (wanted-p "unfinished" only)
        (put (section-text "Started, no answer" (getf data :unfinished) +cross+ ""
                           #'describe-unfinished)))
      (when (wanted-p "cron" only)
        (put (section-text "Cron trouble" (getf data :cron) +warn+ "" #'describe-cron)))
      (when (wanted-p "goals" only)
        (put (section-text "Standing goals" (getf data :goals) +ring+ "" #'describe-goal)))
      (let ((skipped (loop for (name . note) in (getf data :sources)
                           unless (equal note "read")
                             collect (format nil "~a: ~a"
                                             (or (cdr (assoc name +source-labels+ :test #'equal)) name)
                                             note))))
        (when (and skipped (or (null only) (zerop (length (string-trim " " (or only ""))))))
          (format out "  Not read:~%")
          (dolist (line skipped) (format out "    ~a ~a~%" +dot+ line)))))
    (get-output-stream-string out)))

(defun counts-text (&key data (limit nil))
  "The one line over the whole board: what is waiting, running, failing."
  (let ((data (or data (snapshot :limit limit))))
    (format nil "duty: ~d waiting on you, ~d running, ~d unanswered, ~d cron in trouble, ~d goal~:p"
            (length (getf data :waiting)) (length (getf data :running))
            (length (getf data :unfinished)) (length (getf data :cron))
            (length (getf data :goals)))))

;;; --- the tool and /duty --------------------------------------------------------

(defun read-duty (query)
  "The duty tool: the whole board, or the one section QUERY names."
  (let ((section (duty-query-section query)))
    (or (duty-text :only section) "the duty board is empty")))

(defun run-slash (args session-id)
  "/duty [SECTION|counts]: the board, one section of it, or the count line."
  (declare (ignore session-id))
  (let ((argument (string-downcase (string-trim " " (or args "")))))
    (cond ((string= argument "counts") (counts-text))
          ((member argument +section-options+ :test #'string=)
           (let ((text (duty-text :only argument)))
             (if (plusp (length text)) text "the duty board is empty")))
          (t (format nil "~a is not one of the sections: ~{~a~^, ~}, or counts"
                     argument +sections+)))))

;;; --- the declaration -----------------------------------------------------------

(nle:define-cell duty
  (:section ("duty")
    (:guide "nothing here is required; window_hours is how far back the board looks, stall_minutes when a running turn is called stalled, limit how many rows a section prints")
    ("window_hours" :integer :default 72 :min 1
                    :doc "how far back a conversation still counts as in flight, in hours")
    ("stall_minutes" :integer :default 10 :min 1
                     :doc "a running turn quiet this long is flagged stalled")
    ("limit" :integer :default 20 :min 1
             :doc "how many rows each section of the board prints"))
  (:tool "duty" (nlk:contract 'duty-query) 'read-duty)
  (:command "duty" 'run-slash
            :description "What waits on you, what runs, what broke"
            :argument-hint "WAITING|RUNNING|UNFINISHED|CRON|GOALS|COUNTS")
  (:help :duty
         "duty: what waits on you, what runs, what broke, read-only"
         +manual+))
