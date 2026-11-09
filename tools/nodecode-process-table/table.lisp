;;;; table.lisp --- rows into one table, the table into the answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What a caller asked for is one call's filters; what the answer says is
;;;; what those filters left. The one judgement that is this cell's own is
;;;; PARENT-STATE: a system records a parent PID, not a handle, so once the
;;;; parent exits its number is free for reuse and a child that keeps
;;;; running points at an unrelated newer process. A parent that started
;;;; AFTER its child cannot have spawned it, so that is `reused', not `alive'.

(in-package #:nodecode-process-table)

(defparameter +path-limit+ 220
  "How much of an image path one entry carries; the rest is elided.")

(defparameter +max-rows+ 500
  "The ceiling on how many entries one call answers, whatever it asked for.")

(defparameter *system-prefixes*
  '("/usr/" "/bin/" "/sbin/" "/lib" "/etc/" "/System/" "/Library/" "/opt/homebrew/"
    "/snap/" "c:\\windows\\" "c:\\program files\\windows")
  "Where an image is the system's rather than the user's: session and
bootstrap processes live there and are started by a parent that exits by
design, so they hold a dead parent PID forever without being a leak. A
`system_prefixes' list in the section replaces this one.")

;;; --- one entry -------------------------------------------------------------

(defstruct (process-entry (:constructor make-process-entry))
  "One process as the table prints it: what the rows said, and what the
rest of the table makes of it."
  (pid 0 :read-only t)
  (parent-pid 0 :read-only t)
  (name "" :read-only t)
  (path nil :read-only t)
  (started nil :read-only t)
  (threads nil :read-only t)
  (parent-name nil)
  (state "unknown")
  (system nil)
  (context nil))

(defun parent-state (child-started parent-pid parent-started parent-present)
  "Whether the recorded parent is the one that spawned the child: `root',
`gone', `reused', `alive' or `unknown'."
  (cond ((zerop parent-pid) "root")
        ((not parent-present) "gone")
        ((and child-started parent-started)
         (if (> parent-started child-started) "reused" "alive"))
        (t "unknown")))

(defun system-prefixes ()
  "The prefixes this run treats as the system's own images."
  (or (setting :system-prefixes) *system-prefixes*))

(defun system-image-p (path)
  "Whether PATH is an image the system owns, or one this account cannot read
at all -- which is what the session process of another account looks like."
  (if (or (null path) (zerop (length path)))
      t
      (let ((prefixes (mapcar (lambda (prefix) (string-downcase prefix)) (system-prefixes)))
            (lower (string-downcase path)))
        (some (lambda (prefix) (uiop:string-prefix-p prefix lower)) prefixes))))

(defun short-path (path)
  "PATH cut to +PATH-LIMIT+ characters, or NIL."
  (when (and path (plusp (length path)))
    (if (<= (length path) +path-limit+)
        path
        (concatenate 'string (subseq path 0 +path-limit+) "..."))))

(defun entry-of (row rows)
  "ROW as an entry, its parent read off ROWS (a hash of PID to row)."
  (let* ((parent-pid (or (getf row :parent-pid) 0))
         (parent (gethash parent-pid rows)))
    (make-process-entry
     :pid (getf row :pid)
     :parent-pid parent-pid
     :name (or (getf row :name) "")
     :path (short-path (getf row :path))
     :started (getf row :started)
     :threads (getf row :threads)
     :parent-name (and parent (getf parent :name))
     :state (parent-state (getf row :started) parent-pid
                          (and parent (getf parent :started))
                          (and parent t))
     :system (system-image-p (getf row :path)))))

(defun table-of (raw-rows)
  "RAW-ROWS as a hash of PID to entry, the first row of a PID winning."
  (let ((rows (make-hash-table)) (table (make-hash-table)))
    (dolist (row raw-rows)
      (when (getf row :pid)
        (setf (gethash (getf row :pid) rows) row)))
    (maphash (lambda (pid row) (setf (gethash pid table) (entry-of row rows))) rows)
    table))

;;; --- what a call asked for --------------------------------------------------

(defun wanted-pids (pid pids)
  "The PIDs a call named, as a hash, or NIL when it named none."
  (let ((wanted (make-hash-table)) (any nil))
    (dolist (value (append (and pid (list pid)) (and pids (coerce pids 'list))))
      (let ((number (and (integerp value) (not (minusp value)) value)))
        (when number (setf any t (gethash number wanted) t))))
    (and any wanted)))

(defun call-limit (limit)
  "How many entries this call answers: its own LIMIT, else the section's,
clamped to 1..+MAX-ROWS+."
  (let ((number (or (and (integerp limit) limit) (setting :limit) 100)))
    (max 1 (min +max-rows+ number))))

(defun matches-p (entry wanted process parent orphans-only include-system)
  "Whether ENTRY is one the call asked for."
  (and (or (null wanted) (gethash (process-entry-pid entry) wanted))
       (or (null parent) (= parent (process-entry-parent-pid entry)))
       (or include-system (not (process-entry-system entry)))
       (or (null process)
           (search process (format nil "~a ~a" (process-entry-name entry)
                                   (or (process-entry-path entry) ""))
                   :test #'char-equal))
       (or (not orphans-only)
           (member (process-entry-state entry) '("gone" "reused") :test #'string=))))

(defun by-age (a b)
  "Oldest first, the entries with no start time last, then by PID."
  (let ((one (process-entry-started a)) (two (process-entry-started b)))
    (cond ((and one two) (or (< one two) (and (= one two) (< (process-entry-pid a)
                                                              (process-entry-pid b)))))
          (one t)
          (two nil)
          (t (< (process-entry-pid a) (process-entry-pid b))))))

(defun ancestry-of (entries table)
  "The ancestor chain of every entry in ENTRIES, marked as context rows, one
row per ancestor however many children point at it."
  (let ((seen (make-hash-table)) (context '()))
    (dolist (entry entries)
      (setf (gethash (process-entry-pid entry) seen) t))
    (dolist (entry entries)
      (loop with pid = (process-entry-parent-pid entry)
            while (and pid (plusp pid) (not (gethash pid seen)))
            do (setf (gethash pid seen) t)
               (let ((ancestor (gethash pid table)))
                 (unless ancestor (return))
                 (setf (process-entry-context ancestor) t)
                 (push ancestor context)
                 (setf pid (process-entry-parent-pid ancestor)))))
    (nreverse context)))

;;; --- the answer -------------------------------------------------------------

(defun entry-json (entry now)
  "ENTRY as the object one entry is in the answer."
  (nlk:json-object
   "pid" (process-entry-pid entry)
   "parent_pid" (process-entry-parent-pid entry)
   "process" (process-entry-name entry)
   "path" (or (process-entry-path entry) :null)
   "started" (or (iso-time (process-entry-started entry)) :null)
   "age_seconds" (if (process-entry-started entry)
                     (max 0 (- now (process-entry-started entry)))
                     :null)
   "threads" (or (process-entry-threads entry) :null)
   "parent_process" (or (process-entry-parent-name entry) :null)
   "parent_state" (process-entry-state entry)
   "system" (if (process-entry-system entry) t :false)
   :when (process-entry-context entry) "context" t))

(defun collect (&key pid pids process parent orphans-only with-ancestry
                  (include-system t) limit)
  "The table, filtered as the call asked.
=> a plist: :ENTRIES (oldest first, at most the limit), :CONTEXT (the
ancestors asked for), :TOTAL, :GONE, :REUSED, :SYSTEM-ORPHANS, :NOTES."
  (let* ((wanted (wanted-pids pid pids))
         (wanted-process (let ((text (and (stringp process) (nlk:trimmed process))))
                           (and text (plusp (length text)) (string-downcase text))))
         (parent-pid (and (integerp parent) (not (minusp parent)) parent))
         (entries-limit (call-limit limit))
         (table (table-of (read-rows)))
         (candidates '())
         (gone 0)
         (reused 0)
         (system-orphans 0))
    (maphash (lambda (pid entry)
               (declare (ignore pid))
               (let ((state (process-entry-state entry)))
                 (when (member state '("gone" "reused") :test #'string=)
                   (if (string= state "gone") (incf gone) (incf reused))
                   (when (process-entry-system entry) (incf system-orphans)))
                 (when (matches-p entry wanted wanted-process parent-pid
                                  orphans-only include-system)
                   (push entry candidates))))
             table)
    (let* ((sorted (sort candidates #'by-age))
           (entries (if (> (length sorted) entries-limit)
                        (subseq sorted 0 entries-limit)
                        sorted))
           (context (and with-ancestry entries (ancestry-of entries table)))
           (total (hash-table-count table))
           (notes '()))
      (push (format nil "~d of ~d process(es) matched" (length entries) total) notes)
      (when (plusp (+ gone reused))
        (push (format nil "~d process(es) have a parent that is gone (~d) or whose PID was reused (~d)"
                      (+ gone reused) gone reused)
              notes))
      (when (plusp system-orphans)
        (push (format nil "~d of them are system session or bootstrap processes whose parent exits by design; set include_system=false to leave them out"
                      system-orphans)
              notes))
      (when (> (length sorted) (length entries))
        (push (format nil "stopped at limit=~d; raise it or narrow the filter" entries-limit)
              notes))
      (when (null entries)
        (push "nothing matched; drop the filters to see the whole table, or set orphans_only=false"
              notes))
      (list :entries entries :context context :total total :gone gone :reused reused
            :system-orphans system-orphans :notes (nreverse notes)))))

(defun failure (condition)
  "CONDITION as the answer of a call that could not read the table."
  (nlk:encode-json-object
   (nlk:json-object "ok" :false "error" (format nil "~a" condition))))

(defun report (&key pid pids process parent orphans-only with-ancestry
                 (include-system t) limit)
  "The table, filtered and rendered as the JSON text a tool answers with:
{\"ok\": true} with entries, ancestry, the counts and the notes, or
{\"ok\": false, \"error\": ...} when the machine could not be read."
  (handler-case
      (let ((collected (collect :pid pid :pids pids :process process :parent parent
                                :orphans-only orphans-only :with-ancestry with-ancestry
                                :include-system include-system :limit limit))
            (now (get-universal-time)))
        (nlk:encode-json-object
         (nlk:json-object
          "ok" t
          "matched" (length (getf collected :entries))
          "total_processes" (getf collected :total)
          "orphans" (+ (getf collected :gone) (getf collected :reused))
          "orphans_gone" (getf collected :gone)
          "orphans_reused" (getf collected :reused)
          "entries" (mapcar (lambda (entry) (entry-json entry now))
                             (getf collected :entries))
          "ancestry" (mapcar (lambda (entry) (entry-json entry now))
                              (getf collected :context))
          "notes" (coerce (getf collected :notes) 'vector))))
    (process-table-error (condition) (failure condition))
    (error (condition) (failure condition))))

(defun described (entry now)
  "One entry as the lines a person reads: PID, parent, name, state, image."
  (declare (ignore now))
  (format nil "~6d ~6d  ~a~%              ~a~@[  started ~a~]~@[  ~d thread~:p~]~
~@[              parent: ~a~]~@[  (system)~]~%"
          (process-entry-pid entry) (process-entry-parent-pid entry)
          (process-entry-name entry)
          (format nil "~a~@[  ~a~]" (process-entry-state entry)
                  (process-entry-path entry))
          (iso-time (process-entry-started entry))
          (process-entry-threads entry)
          (process-entry-parent-name entry)
          (and (process-entry-system entry) t)))

(defun describe-table (&key process orphans-only limit)
  "The same table, filtered, as the text a person reads."
  (handler-case
      (let* ((collected (collect :process process :orphans-only orphans-only :limit limit))
             (now (get-universal-time)))
        (with-output-to-string (out)
          (loop for entry in (getf collected :entries)
                do (write-string (described entry now) out))
          (loop for note in (getf collected :notes)
                do (format out "- ~a~%" note))))
    (process-table-error (condition) (format nil "~a" condition))
    (error (condition) (format nil "process lookup failed: ~a" condition))))
