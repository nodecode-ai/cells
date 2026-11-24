;;;; cell.lisp --- Ollama Cloud's off-peak window and the ledger of approved work.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The window is Ollama's own definition: off-peak pricing applies outside
;;;; 12:00 and 18:00 UTC on weekdays and all day on weekends, so peak is
;;;; Monday to Friday 12:00-18:00 UTC and every other moment is off-peak.
;;;; Nothing here reaches the network: the clock is the image's, and the
;;;; ledger is the user's own markdown file, which only /off-peak approve
;;;; ever writes, and then atomically.

(defpackage #:nodecode-off-peak
  (:use #:cl)
  (:export #:off-peak-error #:window #:status-text #:status-object
           #:approve-text #:plan-text #:ledger-items #:queue-counts))

(in-package #:nodecode-off-peak)

(nlk:define-peripheral off-peak :not-running t)

(defun trim (text)
  "TEXT as a string with the whitespace around it taken off."
  (string-trim '(#\Space #\Tab #\Newline #\Return) (princ-to-string (or text ""))))

;;; --- the words -------------------------------------------------------------------

(defparameter +primer+
  "Ollama Cloud is cheaper off-peak: outside 12:00-18:00 UTC on weekdays, and
all day at the weekend. This cell says which side of that line you are on, how
long until it moves, and what your own ledger of queued work holds.
/off-peak            the window, the countdown and the ledger's counts
/off-peak json       the same as one JSON line, for a script or a badge
/off-peak approve ID,ID   mark those ledger items approved -- only the user
                     approves, and nothing enters the ledger by itself
/off-peak plan       the cron job the window suggests, with its hash; nothing
                     is installed, the cron cell installs it if the user says so
The ledger is off-peak.queue_file: markdown you write, a table row
`| ID | job | owner | evidence | status | window |` or a bullet
`- id: K-001 | status: awaiting | ...`. Statuses are awaiting, approved, ran
and skipped; durum, onayli, kosuldu and atlandi are read as the same words.
Approved work runs when the window is off-peak; while it is peak it waits."
  "What (help :off-peak) answers while the cell runs.")

(defparameter +plan-schedule+ "5 18 * * 1-5"
  "The cron schedule the plan suggests, in UTC: five past the hour the off-peak window opens.")

(defparameter +plan-marker+ "OFF-PEAK-QUEUE-RUNNER"
  "The marker line that finds this cell's job again in the cron cell's registry.")

(defparameter *status-words*
  '(("approved" . "approved") ("onaylı" . "approved") ("onayli" . "approved")
    ("awaiting" . "awaiting") ("onay-bekliyor" . "awaiting")
    ("onay-bekleyen" . "awaiting") ("bekliyor" . "awaiting")
    ("ran" . "ran") ("koşuldu" . "ran") ("kosuldu" . "ran")
    ("skipped" . "skipped") ("atlandı" . "skipped") ("atlandi" . "skipped"))
  "Every status word the ledger may use, and the canonical one it means.")

(defun canonical-status (word)
  "WORD as one of the four canonical statuses, or its lowercase self."
  (let ((word (string-downcase (trim word))))
    (or (cdr (assoc word *status-words* :test #'string=)) word)))

;;; --- the clock -------------------------------------------------------------------

(defconstant +peak-start-hour+ 12 "UTC hour the peak window opens on a weekday.")
(defconstant +peak-end-hour+ 18 "UTC hour the peak window closes on a weekday.")

(defun midnight-utc (now)
  "00:00:00 UTC on NOW's own date, as a universal time."
  (multiple-value-bind (seconds minutes hours date month year) (decode-universal-time now 0)
    (declare (ignore seconds minutes hours))
    (encode-universal-time 0 0 0 date month year 0)))

(defun weekday-utc (universal-time)
  "UNIVERSAL-TIME's weekday, 0 Monday to 6 Sunday."
  (nth-value 6 (decode-universal-time universal-time 0)))

(defun peak-p (&optional (now (get-universal-time)))
  "Is NOW inside Ollama Cloud's peak window: a weekday between 12:00 and 18:00 UTC."
  (multiple-value-bind (seconds minutes hour) (decode-universal-time now 0)
    (declare (ignore seconds minutes))
    (and (< (weekday-utc now) 5) (<= +peak-start-hour+ hour) (< hour +peak-end-hour+))))

(defun next-change (&optional (now (get-universal-time)))
  "The universal time the window next changes: the off-peak start while peak,
else the next weekday's peak start. UTC has no daylight saving, so a day is
86400 seconds and the arithmetic is exact."
  (if (peak-p now)
      (+ (midnight-utc now) (* +peak-end-hour+ 3600))
      (loop with midnight = (midnight-utc now)
            for day from 0 below 8
            for candidate = (+ midnight (* day 86400) (* +peak-start-hour+ 3600))
            when (and (> candidate now) (< (weekday-utc candidate) 5))
              return candidate
            finally (return (+ midnight (* 8 86400) (* +peak-start-hour+ 3600))))))

(defun human-duration (seconds)
  "SECONDS as \"2d 3h\", \"3h 05m\" or \"14m\"."
  (let* ((total (max 0 (round seconds)))
         (days (floor total 86400))
         (hours (floor (mod total 86400) 3600))
         (minutes (floor (mod total 3600) 60)))
    (cond ((plusp days) (format nil "~dd ~dh" days hours))
          ((plusp hours) (format nil "~dh ~2,'0dm" hours minutes))
          (t (format nil "~dm" minutes)))))

(defun iso-utc (universal-time)
  "UNIVERSAL-TIME as an ISO 8601 instant in UTC."
  (multiple-value-bind (seconds minutes hours date month year) (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hours minutes seconds)))

(defun window (&optional (now (get-universal-time)))
  "The window NOW sits in, as a plist: :peak-p, :now, :change-at, :change-kind,
:seconds and :countdown. The one authority both verbs and the notice read."
  (let* ((peak (peak-p now))
         (at (next-change now))
         (seconds (- at now)))
    (list :peak-p peak
          :now now
          :change-at at
          :change-kind (if peak "off-peak" "peak")
          :seconds seconds
          :countdown (human-duration seconds))))

;;; --- the ledger ------------------------------------------------------------------

(defun expand-home (path)
  "PATH, a string, with a leading ~ read as the home directory."
  (let ((path (princ-to-string path)))
    (if (and (plusp (length path)) (char= (char path 0) #\~))
        (namestring (merge-pathnames (string-left-trim "/" (subseq path 1))
                                      (uiop:ensure-directory-pathname (user-homedir-pathname))))
        path)))

(defun ledger-path ()
  "The ledger's pathname, or a refusal naming the setting that is missing."
  (let ((raw (setting :queue-file)))
    (when (or (null raw) (zerop (length (trim (princ-to-string raw)))))
      (fail "off-peak.queue_file is not set: point it at your own markdown ledger and (restart-cells \"nodecode-off-peak\")"))
    (pathname (expand-home raw))))

(defun split-on (character string)
  "STRING split at every CHARACTER, the empty pieces kept."
  (loop for start = 0 then (1+ at)
        for at = (position character string :start start)
        collect (subseq string start at)
        while at))

(defun split-lines (text)
  "TEXT as lines, a trailing #\\Return dropped from each."
  (loop for start = 0 then (1+ at)
        for at = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start at))
        while at))

(defun join-lines (lines)
  "LINES as one text, a newline after each -- and no blank line where the
ledger already ended with one."
  (with-output-to-string (out)
    (dolist (line (if (and lines (zerop (length (car (last lines))))) (butlast lines) lines))
      (write-string line out)
      (terpri out))))

(defun field-span (line keys)
  "The (start . end) of the value the first of KEYS carries in LINE, or NIL.
A field is `key: value`, and the value runs to the next separator."
  (let ((lower (string-downcase line)))
    (loop for key in keys
          for pattern = (concatenate 'string key ":")
          for at = (search pattern lower)
          when at
            do (let* ((head (+ at (length pattern)))
                      (start (loop for index = head then (1+ index)
                                   while (and (< index (length line))
                                              (member (char line index) '(#\Space #\Tab)))
                                   finally (return index)))
                      (end (or (position-if (lambda (c) (find c "·|;,")) lower :start start)
                               (length line))))
                 (loop while (and (> end start) (member (char line (1- end)) '(#\Space #\Tab)))
                       do (decf end))
                 (return (cons start end))))))

(defun field-value (line keys)
  "The value LINE carries for the first of KEYS, in its own case."
  (let ((span (field-span line keys)))
    (when span (subseq line (car span) (cdr span)))))

(defun separator-row-p (cells)
  "Is CELLS a markdown table's rule row? An empty cell is a table's edge, not a dash."
  (and cells
       (some (lambda (cell) (plusp (length (trim cell)))) cells)
       (every (lambda (cell)
                (let ((cell (trim cell)))
                  (or (zerop (length cell))
                      (every (lambda (c) (find c "-: ")) cell))))
              cells)))

(defun row-cells (line)
  "LINE's table cells when it is a table row, else NIL."
  (let ((line (trim line)))
    (when (and (plusp (length line)) (char= (char line 0) #\|))
      (let ((cells (split-on #\| line)))
        (when (and (>= (length cells) 6) (not (separator-row-p cells)))
          cells)))))

(defun line-item (line)
  "The item LINE carries as a plist (:id :status), or NIL.
A table row keeps its id in the first cell and its status in the fifth; a
bullet or a dash line names them as `id:` and `status:` (or `durum:`)."
  (if (row-cells line)
      (let* ((cells (row-cells line))
             (id (trim (second cells)))
             (status (trim (nth 5 cells))))
        (when (and (plusp (length id)) (not (string-equal id "id")))
          (list :id id :status (canonical-status status) :table-p t)))
      (let* ((line (trim line))
             (id (and (plusp (length line))
                      (member (char line 0) '(#\- #\* #\+))
                      (or (field-value line '("id")) (field-value line '("kimlik")))))
             (status (and id (or (field-value line '("status")) (field-value line '("durum"))))))
        (when (and id (plusp (length id)))
          (list :id (trim id) :status (canonical-status (or status "")) :table-p nil)))))

(defun ledger-items (path)
  "The items PATH's ledger holds, in the file's order; NIL when it is not there."
  (when (probe-file path)
    (loop for line in (split-lines (uiop:read-file-string path))
          for item = (line-item line)
          when item collect item)))

(defun queue-counts (items)
  "ITEMS counted by status, as a plist of counts."
  (let ((counts (list :total (length items) :approved 0 :awaiting 0 :ran 0 :skipped 0 :other 0)))
    (dolist (item items counts)
      (let ((key (case (intern (string-upcase (getf item :status)) :keyword)
                   (:approved :approved) (:awaiting :awaiting) (:ran :ran) (:skipped :skipped)
                   (t :other))))
        (incf (getf counts key))))))

;;; --- approval --------------------------------------------------------------------

(defun repad (cell value)
  "CELL with its trimmed content replaced by VALUE, its padding and width kept."
  (let* ((inner (trim cell))
         (left (subseq cell 0 (- (length cell) (length (string-left-trim '(#\Space #\Tab) cell)))))
         (right (subseq cell (length (string-right-trim '(#\Space #\Tab) cell))))
         (fill (make-string (max 0 (- (length inner) (length value))) :initial-element #\Space)))
    (concatenate 'string left value fill right)))

(defun rewrite-line (line status)
  "LINE whose own status becomes STATUS, in the shape it already has."
  (if (row-cells line)
      (let ((cells (split-on #\| line)))
        (setf (nth 5 cells) (repad (nth 5 cells) status))
        (format nil "~{~a~^|~}" cells))
      (let ((span (field-span line '("status" "durum"))))
        (if span
            (concatenate 'string (subseq line 0 (car span)) status (subseq line (cdr span)))
            line))))

(defun approve-text (text ids)
  "TEXT with each item named in IDS set to approved.
Values: the new text, and the ids that actually changed -- an id the ledger
does not carry, or one already approved or ran, changes nothing."
  (let ((wanted (mapcar (lambda (id) (string-downcase (trim id))) ids))
        (changed '()))
    (values
     (join-lines
      (loop for line in (split-lines text)
            for item = (line-item line)
            collect (if (and item
                             (member (string-downcase (getf item :id)) wanted :test #'string=)
                             (not (member (getf item :status) '("approved" "ran") :test #'string=)))
                        (progn (push (getf item :id) changed)
                               (rewrite-line line "approved"))
                        line)))
     (nreverse changed))))

(defun write-ledger (path text)
  "TEXT as PATH's whole content, written beside it and renamed over it."
  (let ((temporary (concatenate 'string (namestring path) ".tmp")))
    (with-open-file (out temporary :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string text out))
    (uiop:rename-file-overwriting-target temporary path)))

;;; --- the report ------------------------------------------------------------------

(defun status-object (&optional (now (get-universal-time)))
  "The status as one JSON object: the same data the report prints."
  (running-settings)
  (let* ((window (window now))
         (path (ledger-path))
         (counts (queue-counts (ledger-items path))))
    (nlk:make-json-object
     "window" (if (getf window :peak-p) "peak" "off-peak")
     "utc_time" (iso-utc now)
     "next_change" (getf window :change-kind)
     "changes_at" (iso-utc (getf window :change-at))
     "seconds_until_change" (getf window :seconds)
     "countdown" (getf window :countdown)
     "remind_minutes" (setting :remind-minutes)
     "queue_file" (namestring path)
     "queue_exists" (if (probe-file path) t :false)
     "queue" (nlk:make-json-object
              "total" (getf counts :total)
              "approved" (getf counts :approved)
              "awaiting" (getf counts :awaiting)
              "ran" (getf counts :ran)
              "skipped" (getf counts :skipped)))))

(defun status-text (&optional (now (get-universal-time)))
  "The window, the countdown and the ledger's counts, in words."
  (running-settings)
  (let* ((window (window now))
         (path (ledger-path))
         (counts (queue-counts (ledger-items path))))
    (with-output-to-string (out)
      (format out "~a~%"
              (if (getf window :peak-p)
                  (format nil "Ollama Cloud: PEAK now (weekdays 12:00-18:00 UTC) -- off-peak opens in ~a, at ~a."
                          (getf window :countdown) (iso-utc (getf window :change-at)))
                  (format nil "Ollama Cloud: OFF-PEAK now -- cheap work runs; peak starts in ~a, at ~a."
                          (getf window :countdown) (iso-utc (getf window :change-at)))))
      (format out "~a~%"
              (if (probe-file path)
                  (format nil "Ledger ~a: ~d item~:p -- ~d approved, ~d awaiting, ~d ran, ~d skipped."
                          (namestring path) (getf counts :total) (getf counts :approved)
                          (getf counts :awaiting) (getf counts :ran) (getf counts :skipped))
                  (format nil "Ledger ~a: not written yet -- one table row or one bullet an item."
                          (namestring path))))
      (when (and (getf window :peak-p) (plusp (getf counts :approved)))
        (format out "~d approved item~:p wait for the window.~%" (getf counts :approved)))
      (format out "Only you approve: /off-peak approve ID,ID marks items approved."))))

;;; --- the plan, and its hash --------------------------------------------------------

(defun plan-prompt (path)
  "The prompt the suggested cron job hands its agent."
  (format nil "~a -- ledger: ~a~%~a~a"
          +plan-marker+ (namestring path)
          "Run only the ledger items whose status is approved; set an item to ran when its evidence exists, and to skipped with the reason when it cannot run. Never change an item's status to approved -- only the user approves. If no item is approved, run nothing and write one status line. Keep the "
          +plan-marker+))

(defun fnv1a (text)
  "TEXT's FNV-1a 32-bit hash, a number."
  (let ((hash #x811c9dc5))
    (loop for character across text
          do (setf hash (logand (* (logxor hash (logand (char-code character) #xff))
                                   #x01000193)
                                #xffffffff))
          finally (return hash))))

(defun plan-text ()
  "The cron job this cell suggests, in full, with the hash an approval is bound to.
Nothing is installed: the plan is printed, and the cron cell installs it."
  (running-settings)
  (let* ((path (ledger-path))
         (prompt (plan-prompt path))
         (hash (format nil "~8,'0x" (fnv1a prompt))))
    (format nil "~{~a~%~}"
            (list "Off-peak queue runner -- plan (nothing is installed)"
                  (format nil "  schedule: ~a (UTC): five past the hour the off-peak window opens on weekdays" +plan-schedule+)
                  "  name:     off-peak-queue"
                  (format nil "  prompt:   ~a" prompt)
                  (format nil "  hash:     ~a" hash)
                  (format nil "Nothing is installed by this cell: ask the agent to add that prompt to the cron cell with the schedule above. The hash ~a is what the plan above hashes to, so an approval names this plan and no other." hash)))))

;;; --- the reminder -----------------------------------------------------------------

(defvar *reminded-for* nil
  "The change time the one-shot notice was already said for, so it says it once.")

(defun remind ()
  "Say once, remind_minutes before the window opens, that it is about to open."
  (handler-case
      (let* ((now (get-universal-time))
             (window (window now)))
        (when (and (getf window :peak-p)
                   (<= (getf window :seconds) (* 60 (setting :remind-minutes)))
                   (not (eql *reminded-for* (getf window :change-at))))
          (setf *reminded-for* (getf window :change-at))
          (let ((approved (handler-case (getf (queue-counts (ledger-items (ledger-path))) :approved)
                            (off-peak-error () 0))))
            (nle:notice
             (format nil "off-peak: Ollama Cloud's off-peak window opens in ~a~@[ -- ~d approved item~:p queued~]."
                     (getf window :countdown) (and (plusp approved) approved))
             :key "nodecode-off-peak" :level :info))))
    (error () nil)))

;;; --- the verbs -------------------------------------------------------------------

(defun split-words (text)
  "TEXT split at whitespace and commas, the empty pieces dropped."
  (let ((words '()) (start nil))
    (flet ((cut (end)
             (when start
               (let ((word (subseq text start end)))
                 (unless (zerop (length word)) (push word words)))
               (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (member character '(#\Space #\Tab #\Newline #\Return #\,))
                   (cut index)
                   (unless start (setf start index))))
      (cut (length text)))
    (nreverse words)))

(defparameter +usage+ "/off-peak [status|json|approve ID,ID|plan] -- status is the default."
  "The line an unknown argument answers with.")

(defun approve-command (ids)
  "Approve the ledger items IDS names, and answer what changed."
  (let ((path (ledger-path)))
    (cond
      ((null ids)
       "Nothing to approve: /off-peak approve ID,ID names the items, e.g. /off-peak approve K-001,K-004.")
      ((not (probe-file path))
       (format nil "No ledger at ~a yet: write one table row or one bullet an item, then approve again."
               (namestring path)))
      (t (multiple-value-bind (text changed) (approve-text (uiop:read-file-string path) ids)
           (if (null changed)
               (format nil "Nothing changed: ~{~a~^, ~} is either not in the ledger or already approved or ran."
                       ids)
               (progn
                 (write-ledger path text)
                 (let ((counts (queue-counts (ledger-items path))))
                   (format nil "Approved ~{~a~^, ~}. The ledger now holds ~d approved and ~d awaiting; ~a"
                           changed (getf counts :approved) (getf counts :awaiting)
                           (if (peak-p)
                               "the window is peak, so they wait for it to open."
                               "the window is off-peak now, so they may run."))))))))))

(defun answer (function &rest arguments)
  "FUNCTION's answer, or the text of the refusal it signals."
  (handler-case (apply function arguments)
    (off-peak-error (condition) (off-peak-error-detail condition))))

(defun run-command (args session-id)
  "One /off-peak invocation => its text."
  (declare (ignore session-id))
  (let* ((words (split-words (or args "")))
         (head (string-downcase (or (first words) "status"))))
    (cond
      ((member head '("status" "durum" "window" "pencere") :test #'string=)
       (answer #'status-text))
      ((member head '("json" "makine") :test #'string=)
       (answer (lambda () (shasht:write-json (status-object) nil))))
      ((member head '("approve" "onayla") :test #'string=)
       (answer #'approve-command (rest words)))
      ((member head '("plan" "kur") :test #'string=)
       (answer #'plan-text))
      (t (format nil "Unknown argument ~s. ~a" (first words) +usage+)))))

(defparameter +schema+
  (list :description
        (concatenate 'string
                     "Ollama Cloud's off-peak window and the ledger of queued work: "
                     "answers the window (peak or off-peak), the UTC instant it next changes, "
                     "the countdown to it, and how many ledger items are approved, awaiting, "
                     "ran and skipped. Nothing reaches the network. Read it before running "
                     "cheap bulk work, so the run falls in the off-peak window.")
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object))))

(defun status-tool (arguments)
  "The off_peak tool: the same status as one JSON line."
  (declare (ignore arguments))
  (shasht:write-json (status-object) nil))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell off-peak
  (:section ("off-peak")
    (:guide "queue_file is your own markdown ledger of queued work; remind_minutes is how early the one-shot notice about the window says so")
    ("queue_file" :path :required t
                  :doc "the markdown ledger the counts are read from and /off-peak approve writes")
    ("remind_minutes" :integer :default 30 :min 1
                      :doc "how many minutes before the off-peak window opens the notice fires, once"))
  (:help :off-peak "off-peak: Ollama Cloud's cheap window, its countdown, and the ledger of approved work" +primer+)
  (:tool "off_peak" +schema+ #'status-tool)
  (:command "off-peak" #'run-command
            :description "Ollama Cloud's off-peak window, the countdown and the approved-work ledger"
            :argument-hint "status | json | approve ID,ID | plan")
  (:start (lambda ()
            (setf *reminded-for* nil)
            (let ((worker (nlk:worker-start "off-peak-reminder" #'remind :wake 300)))
              (nle:on-stop (lambda ()
                             (nlk:worker-stop worker)
                             (setf *reminded-for* nil)
                             (nle:notice nil :key "nodecode-off-peak")))))))
