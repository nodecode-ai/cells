;;;; snapshot.lisp --- what is in flight, read-only, and the text it renders as.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four readers, one snapshot, one renderer. Every reader answers
;;;; (values ITEMS NOTE): NOTE is `read' when the source answered, else a
;;;; short reason it was not read (`absent', `unreadable (...)'), which the
;;;; text view prints in its Not read footer. Nothing here writes: the
;;;; session directory, the cron registry row and the evaluation registry are
;;;; read through their public readers, and the standing-notice board is a
;;;; copy. No source is required: an empty home renders as all clear.
;;;;
;;;; The readers sit in variables so a test -- or a person in a debugger --
;;;; can stand in for one without a store behind it.

(in-package #:nodecode-inbox)

;;; --- small text helpers -------------------------------------------------------

(defparameter +unix-epoch+ 2208988800
  "Universal time at 1970-01-01T00:00:00Z: unix seconds plus this, and back.")

(defparameter +sections+ '("attention" "progress" "finished" "scheduled" "sessions")
  "The sections the tool and /inbox can be narrowed to, in the order they print.")

(defun flatten-line (text &optional (max 200))
  "TEXT on one line, at most MAX characters: a notice or a failure carries newlines."
  (let* ((parts (uiop:split-string (or text "") :separator '(#\Newline #\Return)))
         (flat (format nil "~{~a~^ ~}" (remove "" parts :test #'string=)))
         (trimmed (string-trim '(#\Space #\Tab) flat)))
    (if (> (length trimmed) max) (subseq trimmed 0 max) trimmed)))

(defun age-text (seconds)
  "A duration the way the inbox writes it: 40s, 12m, 3h13m, 2d4h."
  (let ((s (if (and (realp seconds) (plusp seconds)) (floor seconds) 0)))
    (cond ((< s 60) (format nil "~ds" s))
          ((< s 3600) (format nil "~dm" (floor s 60)))
          ((< s 86400) (multiple-value-bind (hours rest) (floor s 3600)
                         (format nil "~dh~2,'0dm" hours (floor rest 60))))
          (t (multiple-value-bind (days rest) (floor s 86400)
               (format nil "~dd~dh" days (floor rest 3600)))))))

(defun past-text (unix)
  "How long ago a unix-seconds stamp was, or a question mark when it is not one."
  (if (integerp unix)
      (age-text (max 0 (- (get-universal-time) +unix-epoch+ unix)))
      "?"))

(defun ahead-text (unix)
  "How far away a unix-seconds stamp is."
  (if (integerp unix)
      (age-text (max 0 (- (+ unix +unix-epoch+) (get-universal-time))))
      "?"))

(defun stamp-text (value)
  "A session directory stamp: a number as an age, a text as it came."
  (cond ((integerp value)
         (if (> value 2200000000)
             (age-text (max 0 (- (get-universal-time) value)))
             (past-text value)))
        ((stringp value) (subseq value 0 (min 19 (length value))))
        (t "?")))

(defun first-n (list n)
  "The first N of LIST."
  (if (and (integerp n) (plusp n) (> (length list) n))
      (subseq list 0 n)
      list))

(defun wanted-p (name only)
  "Whether section NAME prints: every section when ONLY is empty, else one."
  (or (null only) (zerop (length (string-trim " " (or only "")))) (string= name only)))

(defun section-name (title)
  "The section a printed TITLE belongs to, for the narrowing the tool offers."
  (cond ((uiop:string-prefix-p "Needs" title) "attention")
        ((uiop:string-prefix-p "In progress" title) "progress")
        ((uiop:string-prefix-p "Finished" title) "finished")
        ((uiop:string-prefix-p "Scheduled" title) "scheduled")
        ((uiop:string-prefix-p "Open" title) "sessions")
        (t "all")))

;;; --- the readers -------------------------------------------------------------

(defun read-notices ()
  "What the cells last said under a key: the standing-notice board."
  ;; A refusal or a warning a part raised stands here until it is cleared, which
  ;; is this home's own needs-attention.
  (handler-case
      (values (loop for (key text level) in (nlk:standing-notices)
                    for line = (flatten-line text 160)
                    unless (zerop (length line))
                      collect (list :kind "notice" :id key :text line
                                    :level (and level (string-downcase (string level)))))
              "read")
    (error () (values '() "unreadable (notice board)"))))

(defun read-sessions ()
  "The session directory, newest activity first: what this home has been doing,
and which answers wait on the person."
  (cond ((not (nlk:store-open-p)) (values '() "absent (no store)"))
        (t (handler-case
               (values (loop for row in (nlk:list-sessions)
                             collect (list :kind "session"
                                           :id (getf row :id)
                                           :title (or (getf row :title) "")
                                           :source (let ((source (getf row :source)))
                                                     (if (stringp source) source "?"))
                                           :updated-at (getf row :updated-at)
                                           :waits (let ((waits (getf row :waits)))
                                                    (and (stringp waits) waits))))
                       "read")
             (error () (values '() "unreadable (session directory)"))))))

(defun read-cron ()
  "The cron registry row, when jobs were ever written: what failed, what is
paused, what is scheduled and what answer is still owed."
  ;; Jobs are one JSON document in one session-state row (the cron cell's own
  ;; store). Read, never written.
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

(defun read-evals ()
  "This image's evaluations, background ones included."
  ;; LIST-EVALS is the harness verb the model already has; the cell asks the
  ;; same reader. Without it the section says so instead of failing.
  (let ((reader (find-symbol "LIST-EVALS" "NODECODE.EVOLVED")))
    (cond ((or (null reader) (not (fboundp reader)))
           (values '() "absent (no evaluation registry)"))
          (t (handler-case
                 (values (loop for entry in (funcall reader)
                               collect (list :kind "eval"
                                             :id (getf entry :id)
                                             :status (let ((status (getf entry :status)))
                                                       (if status
                                                           (string-downcase (string status))
                                                           "?"))
                                             :form (flatten-line
                                                    (princ-to-string (getf entry :form)) 100)))
                       "read")
               (error () (values '() "unreadable (evaluation registry)")))))))

(defvar *notices-reader* #'read-notices
  "The notice board reader; a test stands in for it.")
(defvar *sessions-reader* #'read-sessions
  "The session directory reader; a test stands in for it.")
(defvar *cron-reader* #'read-cron
  "The cron registry reader; a test stands in for it.")
(defvar *evals-reader* #'read-evals
  "The evaluation registry reader; a test stands in for it.")

;;; --- the snapshot ------------------------------------------------------------

(defparameter +ok-cron-statuses+ '(nil "" "ok" "success")
  "A cron run that finished well: anything else needs attention.")

(defun cron-failed-p (job)
  "Whether a cron job's last run failed."
  (let ((status (getf job :last-status)))
    (and status (not (member status +ok-cron-statuses+ :test #'equal)))))

(defun default-limit ()
  "The section's row limit, or 20 outside a running cell."
  (or (ignore-errors (setting :limit)) 20))

(defun snapshot (&key (limit nil))
  "The whole inbox as a plist: :GENERATED-AT, the five sections and :SOURCES.
Nothing is changed and nothing is written."
  (let ((n (or limit (default-limit))))
    (multiple-value-bind (notices notices-note) (funcall *notices-reader*)
      (multiple-value-bind (sessions sessions-note) (funcall *sessions-reader*)
        (multiple-value-bind (jobs cron-note) (funcall *cron-reader*)
          (multiple-value-bind (evals evals-note) (funcall *evals-reader*)
            (let ((attention '()) (progress '()) (finished '())
                  (scheduled '()) (open '()))
              (dolist (notice notices) (push notice attention))
              (dolist (session sessions)
                (when (getf session :waits)
                  (push (list* :waiting t session) attention))
                (push session open))
              (dolist (job jobs)
                (let ((state (getf job :state)))
                  (when (member state '("paused" "done") :test #'equal)
                    (push job attention))
                  (when (cron-failed-p job) (push job attention))
                  (when (getf job :report) (push job finished))
                  (when (and (equal state "active") (getf job :next-at))
                    (push job scheduled))))
              (dolist (eval evals)
                (when (equal (getf eval :status) "running") (push eval progress)))
              (list :generated-at (- (get-universal-time) +unix-epoch+)
                    :attention (first-n (nreverse attention) n)
                    :in-progress (first-n (nreverse progress) n)
                    :finished (first-n (nreverse finished) n)
                    :scheduled (first-n (nreverse scheduled) n)
                    :sessions (first-n (nreverse open) n)
                    :sources (list (cons "notices" notices-note)
                                   (cons "sessions" sessions-note)
                                   (cons "cron" cron-note)
                                   (cons "evals" evals-note))))))))))

;;; --- the text ----------------------------------------------------------------

(defparameter +source-labels+
  '(("notices" . "standing notices (the cells board)")
    ("sessions" . "the session directory (store)")
    ("cron" . "cron jobs (the cron registry row)")
    ("evals" . "evaluations (this image's registry)"))
  "What the Not read footer calls each source.")

(defun describe-cron (item)
  "One cron job's line: the failure, the pause, the owed answer, the next fire."
  (format nil "cron '~a'~@[  last run FAILED: ~a~]~@[  last status: ~a~]~@[  paused~]~@[  answer awaiting delivery~]~@[  next in ~a~]"
          (getf item :name)
          (getf item :last-line)
          (let ((status (getf item :last-status)))
            (and status (not (member status +ok-cron-statuses+ :test #'equal)) status))
          (equal (getf item :state) "paused")
          (getf item :report)
          (and (getf item :next-at) (ahead-text (getf item :next-at)))))

(defun describe-item (item)
  "ITEM as one line of the view."
  (let ((kind (getf item :kind)))
    (cond
      ((equal kind "notice")
       (format nil "notice ~a~@[ [~a]~]  ~a"
               (getf item :id) (getf item :level) (getf item :text)))
      ((equal kind "session")
       (format nil "session [~a]  ~a~@[  waiting on you: ~a~]  (~a ago)"
               (getf item :source)
               (let ((title (getf item :title)))
                 (if (and (stringp title) (plusp (length title))) title (getf item :id)))
               (and (getf item :waiting) (getf item :waits))
               (stamp-text (getf item :updated-at))))
      ((equal kind "cron") (describe-cron item))
      ((equal kind "eval")
       (format nil "eval ~a  [~a]  ~a"
               (getf item :id) (getf item :status) (getf item :form)))
      (t (format nil "~a" item)))))

(defun inbox-text (&key only (limit nil))
  "The inbox as a person reads it: sections, glyphs and a Not read footer.
ONLY narrows it to one of +SECTIONS+; LIMIT bounds each list."
  (let* ((data (snapshot :limit limit))
         (out (make-string-output-stream)))
    (labels ((put (title items mark empty &key always)
               (when (and (wanted-p (section-name title) only) (or items always))
                 (format out "  ◆ ~a (~d)~%" title (length items))
                 (if items
                     (dolist (item items)
                       (format out "    ~a ~a~%" mark (describe-item item)))
                     (format out "    ~a~%" empty))
                 (terpri out))))
      (put "Needs attention" (getf data :attention) "⚠" "nothing — all clear" :always t)
      (put "In progress" (getf data :in-progress) "◐" "nothing running in the background" :always t)
      (put "Finished, result not yet delivered" (getf data :finished) "✔" "")
      (put (format nil "Scheduled (~d active job(s))" (length (getf data :scheduled)))
           (getf data :scheduled) "⏱" "")
      (put "Open surfaces" (getf data :sessions) "◎" "")
      (let ((skipped (loop for (name . note) in (getf data :sources)
                           unless (equal note "read")
                             collect (format nil "~a: ~a"
                                             (or (cdr (assoc name +source-labels+ :test #'equal))
                                                 name)
                                             note))))
        (when (and skipped (wanted-p "all" only))
          (format out "  Not read:~%")
          (dolist (line skipped) (format out "    · ~a~%" line))
          (terpri out))))
    (get-output-stream-string out)))
