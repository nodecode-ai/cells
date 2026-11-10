;;;; cell.lisp --- four rooms, five tools, the block a turn carries, /mindhall.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A mind is one JSON file -- ~/.mindhall/mindhall.json unless the section
;;;; names another -- holding four rooms: About you, Work, Dates and a Change
;;;; Log. Every write lands in the Change Log in the same write that makes it,
;;;; signed with the name this cell runs as, so the file says which agent
;;;; changed what. Being one plain document, an AI outside this machine can be
;;;; pointed at the same file.
;;;;
;;;; A verb reads the file, changes one record and writes the file back whole,
;;;; so a refused request writes nothing. The standing harness section is the
;;;; original's pre-llm-call block -- the user's about-you notes, the work not
;;;; done, the next days' dates and the latest changes, cut to a ceiling --
;;;; and /mindhall shows all of it.

(in-package #:nodecode-mindhall)

;;; --- the words, as data so a layer can reword them ----------------------------

(defparameter *primer*
  "Mindhall is one mind every AI the user runs reads and writes, in four rooms.
  About you  lasting facts and rules about the user: how they want answers written.
  Work       a project, client or course, its status (Active, Parked, Done) and one next step.
  Dates      a day and what happens on it, and optionally the work it belongs to.
  Change Log every write, signed with the name the writing agent runs as, and when.
The mind is one JSON file, ~/.mindhall/mindhall.json unless mindhall.store names another,
so an AI outside this machine can be pointed at the same file.
  The five tools: mindhall_read (one room, or all of it), mindhall_add_note,
  mindhall_update_work, mindhall_set_date and mindhall_remove. Save what the user tells
you in the same turn, and only what they actually said. A date with the name of one
already in Dates is moved, not duplicated. mindhall.agent is the name every change is
signed with. Every turn's standing context also carries the mind's short block: when it
and your own memory disagree, Mindhall is right. /mindhall shows the whole mind.
ERROR: MINDHALL-ERROR names a refused request; nothing is written when it is."
  "What (help :mindhall) answers while the cell runs.")

(defparameter +statuses+ '("Active" "Parked" "Done")
  "The three states a piece of work is in, in the order they are shown.")

(defparameter +history-limit+ 200
  "How many changes the file keeps; the oldest fall off past this.")

(defparameter +context-header+
  "Mindhall: the user's shared mind, kept by every AI they use. If it disagrees with your own memory, Mindhall is right."
  "The first line of the block a turn carries.")

(defparameter +rules-line+
  "When the user says something that changes their work, a date or how they want answers, save it with the mindhall tools in the same turn, then confirm briefly. Only what they actually said."
  "What the block asks the model to do with what it hears.")

(defparameter +empty-line+
  "Mindhall is empty: nothing is in About you, Work or Dates yet. Save what the user tells you about their work, dates and preferences with mindhall_add_note, mindhall_update_work and mindhall_set_date."
  "The block of an empty mind: the tools are still worth reaching for.")

;;; --- the one file ------------------------------------------------------------

(defun store-path ()
  "The mind's file: mindhall.store, or ~/.mindhall/mindhall.json -- the folder
whose SQLite file the original kept, so both harnesses a person runs can share
one mind."
  (let ((configured (setting :store)))
    (if (and (stringp configured) (plusp (length (nlk:trimmed configured))))
        (nlk:expand-home (nlk:trimmed configured))
        (merge-pathnames ".mindhall/mindhall.json" (user-homedir-pathname)))))

(defun empty-state ()
  "A mind with nothing in it."
  (list :about #() :work #() :dates #() :changes #()))

(defun state-of (plist)
  "PLIST, as read back, with every room a vector."
  (flet ((room-vector (key)
           (let ((value (getf plist key)))
             (if (vectorp value) value #()))))
    (list :about (room-vector :about) :work (room-vector :work) :dates (room-vector :dates)
          :changes (room-vector :changes))))

(defun read-state ()
  "The mind at (STORE-PATH), or an empty one. A file that does not read as a
mind refuses rather than being overwritten."
  (let* ((path (store-path))
         (text (nlk:read-text path)))
    (cond
      ((null text) (empty-state))
      ((zerop (length (nlk:trimmed text))) (empty-state))
      (t (let ((decoded (ignore-errors (nlk:decode-json text :whole t))))
           (cond ((hash-table-p decoded) (state-of (nlk:json-plist decoded)))
                 (t (fail "~a does not read as a Mindhall file: move it aside and the next write makes a new mind."
                          (namestring path)))))))))

(defun write-state (state)
  "STATE written to (STORE-PATH) in one atomic replace, indented for a person to read."
  (nlk:write-file-atomically (store-path) (nlk:pretty-json (nlk:plist-json state))))

;;; --- small pieces ---------------------------------------------------------------

(defun as-text (value)
  (cond ((stringp value) value) ((null value) "") (t (princ-to-string value))))

(defun agent-name ()
  "The name this cell signs changes with."
  (let ((name (nlk:trimmed (as-text (setting :agent)))))
    (if (plusp (length name)) name "unknown")))

(defun next-id (records)
  (1+ (reduce #'max (map 'list (lambda (row) (or (getf row :id) 0)) records) :initial-value 0)))

(defun plist-set (plist key value)
  "PLIST with KEY's value replaced by VALUE, copying the spine so a caller's
state is left as it was: the store threads a new state out of every change."
  (let ((copy (copy-list plist)))
    (setf (getf copy key) value)
    copy))

(defun with-record (state room row)
  (plist-set state room (concatenate 'vector (getf state room) (vector row))))

(defun without-record (state room id)
  (plist-set state room
             (coerce (remove-if (lambda (row) (eql (getf row :id) id))
                                (coerce (getf state room) 'list))
                     'vector)))

(defun replacing-record (state room id row)
  (plist-set state room
             (map 'vector (lambda (old) (if (eql (getf old :id) id) row old))
                  (getf state room))))

(defun record-of (state room id)
  (find id (getf state room) :key (lambda (row) (getf row :id)) :test #'eql))

(defun trimmed-to (vector limit)
  (if (> (length vector) limit) (subseq vector (- (length vector) limit)) vector))

(defun signed (state room summary)
  "STATE with one change in the Change Log: what happened, in which room, by
whom and when. The log keeps its newest at the end."
  (let ((changes (getf state :changes)))
    (plist-set state :changes
               (trimmed-to
                (concatenate 'vector changes
                             (vector (list :id (next-id changes)
                                           :at (nlk:iso-time (nlk:unix-now) :millis nil)
                                           :by (agent-name)
                                           :room room
                                           :summary summary)))
                +history-limit+))))

(defun today ()
  (subseq (nlk:iso-time (nlk:unix-now) :millis nil) 0 10))

(defun horizon (days)
  "The day DAYS after today, as YYYY-MM-DD."
  (let ((start (nlk:iso-universal (format nil "~aT12:00:00Z" (today)))))
    (subseq (nlk:iso-time (+ start (* 86400 days)) :millis nil) 0 10)))

(defun checked-text (value what)
  "VALUE as one line of text, or a refusal naming WHAT when it is empty or
longer than mindhall.max_text."
  (let ((text (nlk:trimmed (as-text value))))
    (cond ((zerop (length text)) (fail "The ~a is empty." what))
          ((> (length text) (setting :max-text))
           (fail "The ~a is too long: keep it under ~d characters." what (setting :max-text)))
          (t text))))

(defun checked-day (value)
  "VALUE as a calendar day written YYYY-MM-DD, or a refusal."
  (let ((text (nlk:trimmed (as-text value))))
    (unless (and (= 10 (length text))
                 (char= (char text 4) #\-)
                 (char= (char text 7) #\-)
                 (every #'digit-char-p
                        (concatenate 'string (subseq text 0 4) (subseq text 5 7) (subseq text 8 10))))
      (fail "Dates are written as YYYY-MM-DD, for example 2026-11-20."))
    (let* ((day (parse-integer text :start 8 :end 10))
           (month (parse-integer text :start 5 :end 7))
           (year (parse-integer text :start 0 :end 4))
           (real (handler-case
                     (multiple-value-bind (second minute hour d mo y)
                         (decode-universal-time (encode-universal-time 0 0 12 day month year 0) 0)
                       (declare (ignore second minute hour))
                       (and (= d day) (= mo month) (= y year)))
                   (error () nil))))
      (unless real (fail "~a is not a day on the calendar." text))
      text)))

(defun checked-status (value)
  (let ((match (find (nlk:trimmed (as-text value)) +statuses+ :test #'string-equal)))
    (or match (fail "Status must be one of: ~{~a~^, ~}." +statuses+))))

;;; --- reading it out ------------------------------------------------------------

(defun mind-empty-p (state)
  (and (zerop (length (getf state :about)))
       (zerop (length (getf state :work)))
       (zerop (length (getf state :dates)))))

(defun work-sorted (work)
  (sort (coerce work 'list)
        (lambda (a b)
          (let ((ia (or (position (as-text (getf a :status)) +statuses+ :test #'string-equal) 3))
                (ib (or (position (as-text (getf b :status)) +statuses+ :test #'string-equal) 3)))
            (if (= ia ib)
                (string-lessp (as-text (getf a :name)) (as-text (getf b :name)))
                (< ia ib))))))

(defun dates-sorted (dates)
  (sort (coerce dates 'list) #'string< :key (lambda (row) (as-text (getf row :day)))))

(defun recent-changes (state &optional (limit 20))
  "The newest LIMIT changes, newest first."
  (let* ((changes (getf state :changes))
         (wanted (min (length changes) (max 1 (min 200 (or limit 20)))))
         (first-index (- (length changes) wanted)))
    (loop for index from (1- (length changes)) downto first-index
          collect (aref changes index))))

(defun upcoming-dates (state)
  "The dates from today to mindhall.days_ahead days on, in order."
  (let ((start (today)) (end (horizon (setting :days-ahead))))
    (remove-if-not (lambda (row)
                     (let ((day (as-text (getf row :day))))
                       (and (string<= start day) (string<= day end))))
                   (dates-sorted (getf state :dates)))))

(defun work-line (row)
  (let ((step (as-text (getf row :step))))
    (format nil "  ~a [~a~@[, next: ~a~]]"
            (as-text (getf row :name)) (as-text (getf row :status))
            (and (plusp (length step)) step))))

(defun date-line (row &optional id)
  (let ((work (as-text (getf row :work))))
    (format nil "  ~@[~d. ~]~a  ~a~@[ (~a)~]"
            id (as-text (getf row :day)) (as-text (getf row :what))
            (and (plusp (length work)) work))))

(defun change-line (row &optional id)
  (let ((at (as-text (getf row :at))))
    (format nil "  ~@[~d. ~]~a  ~a: ~a"
            id (if (>= (length at) 10) (subseq at 0 10) at)
            (as-text (getf row :by)) (as-text (getf row :summary)))))

(defun joined (lines empty)
  (if lines (format nil "~{~a~%~}" lines) (format nil "~a~%" empty)))

(defun block-text (state)
  "The mind's short block: about-you notes, the work not done, the next days'
dates and the latest changes, cut to mindhall.max_chars."
  (if (mind-empty-p state)
      (format nil "~a~%~a~%~a" +context-header+ +empty-line+ +rules-line+)
      (let ((text
              (with-output-to-string (out)
                (format out "~a~%~a~%" +context-header+ +rules-line+)
                (let ((about (getf state :about)))
                  (when (plusp (length about))
                    (format out "~%About the user:~%")
                    (loop for row across about
                          do (format out "  - ~a~%" (as-text (getf row :note))))))
                (let ((work (remove-if (lambda (row)
                                         (string-equal (as-text (getf row :status)) "Done"))
                                       (work-sorted (getf state :work)))))
                  (when work
                    (format out "~%Work:~%")
                    (dolist (row work) (format out "~a~%" (work-line row)))))
                (let ((dates (upcoming-dates state)))
                  (when dates
                    (format out "~%Upcoming dates:~%")
                    (dolist (row dates) (format out "~a~%" (date-line row)))))
                (let ((changes (recent-changes state (setting :block-changes))))
                  (when changes
                    (format out "~%Latest changes:~%")
                    (dolist (row changes) (format out "~a~%" (change-line row))))))))
        (let ((limit (setting :max-chars)))
          (if (<= (length text) limit)
              (nlk:trimmed text)
              (format nil "~a~%(more in Mindhall: mindhall_read)"
                      (subseq text 0 (max 0 (- limit 40)))))))))

(defun summary-text ()
  "What /mindhall shows a person: the whole mind, and where it lives."
  (let* ((state (read-state))
         (body
           (with-output-to-string (out)
             (format out "Mindhall~%")
             (format out "~%About you~%")
             (if (plusp (length (getf state :about)))
                 (loop for row across (getf state :about)
                       do (format out "  ~d. ~a~%" (getf row :id) (as-text (getf row :note))))
                 (format out "  Nothing yet.~%"))
             (format out "~%Work~%")
             (let ((work (work-sorted (getf state :work))))
               (if work
                   (dolist (row work) (format out "~a~%" (work-line row)))
                   (format out "  Nothing yet.~%")))
             (format out "~%Coming up~%")
             (let ((dates (upcoming-dates state)))
               (if dates
                   (dolist (row dates) (format out "~a~%" (date-line row)))
                   (format out "  No dates in this window.~%")))
             (format out "~%Latest changes~%")
             (let ((changes (recent-changes state 10)))
               (if changes
                   (dolist (row changes) (format out "~a~%" (change-line row)))
                   (format out "  No changes yet.~%"))))))
    (format nil "~a~%Store: ~a. Changes sign as ~a.~%"
            (nlk:trimmed body) (namestring (store-path)) (agent-name))))

(defun room-answer (state room &key include-past (limit 20))
  "One room as the model reads it, ids included so it can name one back."
  (cond
    ((string= room "about")
     (joined (loop for row across (getf state :about)
                   collect (format nil "  ~d. ~a" (getf row :id) (as-text (getf row :note))))
             "About you is empty."))
    ((string= room "work")
     (joined (mapcar #'work-line (work-sorted (getf state :work)))
             "Work is empty."))
    ((string= room "dates")
     (joined (mapcar (lambda (row) (date-line row (getf row :id)))
                     (if include-past
                         (dates-sorted (getf state :dates))
                         (remove-if (lambda (row) (string< (as-text (getf row :day)) (today)))
                                    (dates-sorted (getf state :dates)))))
             "No dates from today on. The ones that passed show with include_past."))
    ((string= room "changes")
     (joined (mapcar (lambda (row) (change-line row (getf row :id)))
                     (recent-changes state limit))
             "No changes yet."))
    (t (block-text state))))

;;; --- the verbs ------------------------------------------------------------------

(define-verb read-mind (room &key include-past limit)
  "Read ROOM of the user's shared mind: about, work, dates, changes, or all of
it. Answers a STRING; every line that carries an id takes it back."
  (room-answer (read-state) (or room "all")
               :include-past include-past :limit (or limit 20)))

(define-verb add-note (note)
  "Save NOTE in About you, signed with this cell's name. => what was saved."
  (let* ((note (checked-text note "note"))
         (state (read-state))
         (id (next-id (getf state :about)))
         (state (signed (with-record state :about (list :id id :note note))
                        "About you" (format nil "Added note: ~a" note))))
    (write-state state)
    (format nil "Saved as note ~d: ~a" id note)))

(define-verb update-work (name &key status step)
  "Add NAME to Work, or change its status or next step. => what changed."
  (let* ((name (checked-text name "work name"))
         (status (and status (checked-status status)))
         (step (and step (checked-text step "next step")))
         (state (read-state))
         (found (find name (getf state :work)
                      :key (lambda (row) (as-text (getf row :name))) :test #'string-equal)))
    (cond
      ((null found)
       (let* ((row (list :id (next-id (getf state :work)) :name name
                         :status (or status (first +statuses+)) :step (or step "")))
              (state (signed (with-record state :work row) "Work"
                             (format nil "Added ~a (~a)" name (getf row :status)))))
         (write-state state)
         (format nil "Added ~a (~a) as work ~d." name (getf row :status) (getf row :id))))
      (t
       (let ((old-status (as-text (getf found :status)))
             (old-step (as-text (getf found :step)))
             (id (getf found :id)))
         (let ((new-status (or status old-status))
               (new-step (or step old-step)))
           (if (and (string= old-status new-status) (string= old-step new-step))
               (format nil "~a is already ~a~@[, next: ~a~]."
                       (as-text (getf found :name)) new-status
                       (and (plusp (length new-step)) new-step))
               (let* ((said (remove nil
                                     (list (unless (string= old-status new-status)
                                             (format nil "status ~a to ~a" old-status new-status))
                                           (unless (string= old-step new-step)
                                             (format nil "next step: ~a"
                                                     (if (plusp (length new-step)) new-step "none"))))))
                      (stored (as-text (getf found :name)))
                      (row (list :id id :name stored :status new-status :step new-step))
                      (state (signed (replacing-record state :work id row) "Work"
                                     (format nil "Updated ~a: ~{~a~^; ~}" stored said))))
                 (write-state state)
                 (format nil "Updated ~a: ~{~a~^; ~}." stored said)))))))))

(define-verb set-date (what day &key work id)
  "Add a date, or move the one with the same name or that id: a date with the
same name is moved, not duplicated. => what happened."
  (let* ((day (checked-day day))
         (work (if (and (stringp work) (plusp (length (nlk:trimmed work))))
                   (checked-text work "work name")
                   ""))
         (state (read-state))
         (found (if id
                    (or (record-of state :dates id) (fail "There is no date with id ~a." id))
                    (let ((label (and what (plusp (length (nlk:trimmed (as-text what))))
                                      (checked-text what "date label"))))
                      (when label
                        (let ((matches (remove-if-not
                                        (lambda (row) (string-equal (as-text (getf row :what)) label))
                                        (coerce (getf state :dates) 'list))))
                          (cond ((> (length matches) 1)
                                 (fail "More than one date is called ~s. Pass its id to move it." label))
                                (t (first matches)))))))))
    (cond
      ((null found)
       (let* ((label (checked-text what "date label"))
              (row (list :id (next-id (getf state :dates)) :day day :what label :work work))
              (state (signed (with-record state :dates row) "Dates"
                             (format nil "Added ~a on ~a" label day))))
         (write-state state)
         (format nil "Added ~a on ~a (date ~d)." label day (getf row :id))))
      (t
       (let* ((old-day (as-text (getf found :day)))
              (old-what (as-text (getf found :what)))
              (old-work (as-text (getf found :work)))
              (label (if (and id (stringp what) (plusp (length (nlk:trimmed what))))
                         (checked-text what "date label")
                         old-what))
              (new-work (if (plusp (length work)) work old-work))
              (row (list :id (getf found :id) :day day :what label :work new-work)))
         (if (and (string= old-day day) (string= old-what label) (string= old-work new-work))
             (format nil "~a is already on ~a." label day)
             (let* ((summary (if (string= old-day day)
                                 (format nil "Updated ~a" label)
                                 (format nil "Moved ~a from ~a to ~a" label old-day day)))
                    (state (signed (replacing-record state :dates (getf row :id) row)
                                   "Dates" summary)))
               (write-state state)
               (format nil "~a (date ~d)." summary (getf row :id)))))))))

(define-verb remove-entry (room id)
  "Remove the note or the date with ID from ROOM (about or dates). => what went."
  (let* ((room (string-downcase (nlk:trimmed (as-text room))))
         (id (or id (fail "Which id?")))
         (state (read-state)))
    (cond
      ((string= room "about")
       (let ((row (record-of state :about id)))
         (unless row (fail "There is no note with id ~a." id))
         (let ((state (signed (without-record state :about id) "About you"
                              (format nil "Removed note: ~a" (as-text (getf row :note))))))
           (write-state state)
           (format nil "Removed note ~d: ~a" id (as-text (getf row :note))))))
      ((string= room "dates")
       (let ((row (record-of state :dates id)))
         (unless row (fail "There is no date with id ~a." id))
         (let ((state (signed (without-record state :dates id) "Dates"
                              (format nil "Removed ~a (~a)"
                                      (as-text (getf row :what)) (as-text (getf row :day))))))
           (write-state state)
           (format nil "Removed ~a (~a), date ~d."
                   (as-text (getf row :what)) (as-text (getf row :day)) id))))
      (t (fail "Room must be about or dates, not ~s." room)))))

;;; --- the tools, the block, the command -------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR line the model repairs from."
  `(handler-case (progn ,@body)
     (error (condition) (nle:failure (format nil "ERROR: mindhall: ~a" condition)))))

(nlk:define-contract mindhall-read-arguments
  (:doc "Read the user's shared mind (Mindhall): lasting facts about the user, the work in progress, the dates ahead and the change log that says which agent changed what. Use it when the block in front of the turn is not enough - for every date, the work already done, or a longer history.")
  ("room" :string :options '("all" "about" "work" "dates" "changes") :slot area
   :doc "which room to read; all of it when unset")
  ("include_past" :boolean
   :doc "for dates: include the dates that have already passed")
  ("limit" :integer
   :doc "for changes: how many to return, at most 200"))

(nlk:define-contract mindhall-update-work-arguments
  (:doc "Add a project, client or course to the user's Work, or change its status or next step. Use the exact name already in Mindhall when updating.")
  ("name" :text :required t
   :doc "the work's name, as the user said it")
  ("status" :string :options '("Active" "Parked" "Done")
   :doc "where it stands")
  ("next_step" :string
   :doc "the single next step, in a few words"))

(nlk:define-contract mindhall-set-date-arguments
  (:doc "Add a date to the user's Dates, or move one already there. A date with the same name is moved, not duplicated, so 'the launch moved to the 22nd' changes the existing launch.")
  ("what" :text
   :doc "what happens that day, for example Launch")
  ("day" :text :required t
   :doc "the day, written YYYY-MM-DD")
  ("work" :string
   :doc "the work it belongs to, when it belongs to one")
  ("id" :integer
   :doc "the id of the date to change, when two dates share a name"))

(nlk:define-contract mindhall-add-note-arguments
  (:doc "Save a lasting fact or rule about the user to About you - how they like answers written, what they are working towards. Only what the user said.")
  ("note" :text :required t
   :doc "one short sentence"))

(nlk:define-contract mindhall-remove-arguments
  (:doc "Remove a note from About you, or a date from Dates, by the id mindhall_read showed. Only when the user asks.")
  ("room" :text :required t :options '("about" "dates") :slot area
   :doc "which room the id is in")
  ("id" :integer :required t
   :doc "the id to remove"))

(defun tool-read (arguments)
  (answering
    (read-mind (mindhall-read-arguments-area arguments)
               :include-past (mindhall-read-arguments-include-past arguments)
               :limit (mindhall-read-arguments-limit arguments))))

(defun tool-update-work (arguments)
  (answering
    (update-work (mindhall-update-work-arguments-name arguments)
                 :status (mindhall-update-work-arguments-status arguments)
                 :step (mindhall-update-work-arguments-next-step arguments))))

(defun tool-set-date (arguments)
  (answering
    (set-date (mindhall-set-date-arguments-what arguments)
              (mindhall-set-date-arguments-day arguments)
              :work (mindhall-set-date-arguments-work arguments)
              :id (mindhall-set-date-arguments-id arguments))))

(defun tool-add-note (arguments)
  (answering (add-note (mindhall-add-note-arguments-note arguments))))

(defun tool-remove (arguments)
  (answering
    (remove-entry (mindhall-remove-arguments-area arguments)
                  (mindhall-remove-arguments-id arguments))))

(defun standing-section (next session-id)
  "Every turn's standing sections, with the mind's block in place of any older one.
A mind that cannot be read costs the turn nothing."
  (let ((sections (funcall next session-id)))
    (if (and *mindhall* (setting :inject))
        (let ((text (ignore-errors (block-text (read-state)))))
          (if (and text (plusp (length text)))
              (acons "mindhall" text (remove "mindhall" sections :key #'car :test #'equal))
              sections))
        sections)))

(defun run-command (args session-id)
  "One /mindhall: the whole mind, or one room of it."
  (declare (ignore session-id))
  (let ((what (string-downcase (nlk:trimmed (as-text args)))))
    (handler-case
        (cond ((zerop (length what)) (summary-text))
              ((member what '("about" "work" "dates" "changes") :test #'string=)
               (read-mind what :include-past t))
              (t (format nil "~a~%Usage: /mindhall, or /mindhall about|work|dates|changes.~%"
                         (summary-text))))
      (error (condition) (format nil "ERROR: mindhall: ~a" condition)))))

;;; --- the one declaration ---------------------------------------------------------

(nle:define-cell mindhall
  (:section ("mindhall")
    (:guide "nothing here has to be answered: the mind is ~/.mindhall/mindhall.json, shared with the user's other AIs; agent is the name every change is signed with, store moves the file, inject puts the mind in front of every turn; next step: /mindhall shows it")
    ("store" :path
     :doc "the mind's file; ~/.mindhall/mindhall.json when unset, shared with any other AI pointed at it")
    ("agent" :string :default "Nodecode"
     :doc "the name this agent signs changes with, as the Change Log shows it")
    ("inject" :boolean :default t
     :doc "put the mind's short block in every turn's standing context")
    ("max_chars" :integer :default 1800 :min 200
     :doc "the character ceiling on that block")
    ("block_changes" :integer :default 5 :min 0
     :doc "how many recent changes the block carries")
    ("days_ahead" :integer :default 14 :min 1
     :doc "how many days ahead the block's dates reach")
    ("max_text" :integer :default 500 :min 20
     :doc "the longest note, work name, next step or date label"))
  (:tool "mindhall_read" (nlk:contract 'mindhall-read-arguments) #'tool-read)
  (:tool "mindhall_update_work" (nlk:contract 'mindhall-update-work-arguments) #'tool-update-work)
  (:tool "mindhall_set_date" (nlk:contract 'mindhall-set-date-arguments) #'tool-set-date)
  (:tool "mindhall_add_note" (nlk:contract 'mindhall-add-note-arguments) #'tool-add-note)
  (:tool "mindhall_remove" (nlk:contract 'mindhall-remove-arguments) #'tool-remove)
  (:command "mindhall" 'run-command
            :description "Show the shared mind: about you, work, dates and the change log"
            :argument-hint "ROOM")
  (:hook 'nle::read-harness-sections #'standing-section)
  (:help :mindhall "mindhall: one shared mind of about-you notes, work, dates and a signed change log, in front of every turn; mindhall_read, mindhall_add_note, mindhall_update_work, mindhall_set_date, mindhall_remove" *primer*)
  (:start (lambda ()
            (let ((path (store-path)))
              (when (uiop:directory-exists-p path)
                (error 'nlk:config-refusal
                       :detail (format nil "mindhall.store names the folder ~a: it must name the mind's file"
                                        (namestring path))))
              (read-state)))))
