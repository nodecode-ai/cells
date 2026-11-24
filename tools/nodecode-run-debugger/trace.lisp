;;;; trace.lisp --- one selected file in, one run out, nothing executed.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original plugin's import lane, kept as it behaves there:
;;;;
;;;; * JSONL only, and the source is a FILE the caller names. A directory,
;;;;   an archive or a raw database is refused before a byte is parsed.
;;;; * Bounded: so many bytes read, so long a line, so many records, so deep
;;;;   a record. A refusal names which bound it broke.
;;;; * Strict: the first record must be the trace envelope of schema
;;;;   hermes-run-debugger/trace version 1, and every event's kind, status
;;;;   and provenance must be one of the format's own words. Unknown fields
;;;;   are ignored rather than refused (see NOTICE).
;;;; * Reported, never observed: an imported log is testimony. Every event
;;;;   keeps the provenance the source declared, and an unknown value stays
;;;;   unknown instead of becoming zero.
;;;; * The digest is of the bytes actually read, so a retry of a source that
;;;;   grew can never blend its new suffix into an earlier count.
;;;;
;;;; A run is kept as one JSON file under the home, so a restart still knows
;;;; what was imported; retention deletes only files this cell wrote.

(in-package #:nodecode-run-debugger)

;;; --- the format's own words ---------------------------------------------------

(defparameter +schema+ "hermes-run-debugger/trace"
  "The one schema id this build reads.")

(defparameter +version+ 1
  "The one schema version this build reads.")

(defparameter +event-kinds+ '("model_call" "tool_call" "subagent_handoff" "retry"
                              "approval_request" "approval_response" "interrupt"
                              "session_lifecycle" "provider_error" "note")
  "The event kinds the format names; anything else is refused, not guessed.")

(defparameter +statuses+ '("ok" "failed" "interrupted" "skipped" "unknown")
  "What one event may report about itself.")

(defparameter +provenances+ '("observed" "reported" "inferred" "unknown")
  "Where one event's byte came from. An import is REPORTED at best.")

(defparameter +outcomes+ '("completed" "failed" "interrupted" "unknown")
  "An attempt's reported outcome: a finished run, never a passed task.")

(defparameter +completeness+ '("complete" "partial" "unknown")
  "What the envelope claims about itself.")

(defparameter +archive-suffixes+ '(".zip" ".tar" ".tar.gz" ".tgz" ".tar.bz2" ".tbz2"
                                   ".tar.xz" ".txz" ".gz" ".bz2" ".xz" ".7z" ".rar" ".zst")
  "Refused before anything else: this build never extracts an archive.")

(defparameter +database-suffixes+ '(".db" ".sqlite" ".sqlite3" ".parquet" ".arrow"
                                    ".pkl" ".pickle")
  "Refused too: a raw database is not a trace file.")

(defparameter +id-max+ 128 "How long an id in this format may be.")

;;; --- the run store -------------------------------------------------------------

(defvar *runs* nil
  "RUN-ID -> run plist, while the cell runs. NIL before a start, which every
verb refuses through RUNNING-SETTINGS.")

(defun runs-directory ()
  "Where this cell keeps its runs: under the home, never beside the sources."
  (nlk:home "run-debugger/runs/"))

(defun run-file (run-id)
  "The file RUN-ID is kept in. Its name is a digest, so no id can walk out
of the directory."
  (merge-pathnames (format nil "~a.json" (nlk:short-digest run-id))
                   (runs-directory)))

(defun write-run (run)
  "Write RUN as one JSON object under the home, 0600. => the pathname."
  (let ((path (run-file (getf run :run-id))))
    (ensure-directories-exist path)
    (nlk:write-file-atomically
     path (nlk:encode-json-object (run->object run)) :mode #o600 :directory-mode #o700)
    path))

(defun run->object (run)
  "RUN as the JSON object it is kept as."
  (nlk:json-object
   "run_id" (getf run :run-id)
   "imported_at" (getf run :imported-at)
   "format" (getf run :format)
   "digest" (getf run :digest)
   "completeness" (getf run :completeness)
   "source" (nlk:json-object
             "name" (getf (getf run :source) :name)
             "size" (getf (getf run :source) :size)
             "lines" (getf (getf run :source) :lines))
   "attempts" (coerce (loop for attempt in (getf run :attempts)
                             collect (nlk:json-object
                                      "id" (getf attempt :id)
                                      "outcome" (getf attempt :outcome)
                                      "sequence" (getf attempt :sequence)
                                      "model" (getf attempt :model)))
                      'vector)
   "gaps" (coerce (loop for gap in (getf run :gaps)
                         collect (nlk:json-object "kind" (getf gap :kind)
                                                   "detail" (getf gap :detail)))
                  'vector)
   "events" (coerce (loop for event in (getf run :events)
                           collect (event->object event))
                    'vector)))

(defun event->object (event)
  "EVENT as the JSON object it is kept as."
  (nlk:json-object
   "id" (getf event :id)
   "attempt" (getf event :attempt)
   "kind" (getf event :kind)
   "status" (getf event :status)
   "provenance" (getf event :provenance)
   "sequence" (getf event :sequence)
   "wall" (getf event :wall)
   "name" (getf event :name)
   "error" (getf event :error)
   "tokens_in" (getf event :tokens-in)
   "tokens_out" (getf event :tokens-out)
   "cost" (getf event :cost)
   "parents" (coerce (getf event :parents) 'vector)))

(defun strings-of (object key)
  "KEY's member of OBJECT as a list of strings, or NIL."
  (let ((value (nlk:json-value object :array key)))
    (when value
      (loop for item across value when (stringp item) collect item))))

(defun object->run (object)
  "The run OBJECT was kept as, or NIL when it is not one."
  (let ((id (nlk:json-value object :string "run_id")))
    (when id
      (list :run-id id
            :imported-at (nlk:json-value object :string "imported_at")
            :format (nlk:json-value object :string "format")
            :digest (nlk:json-value object :string "digest")
            :completeness (nlk:json-value object :string "completeness")
            :source (let ((source (nlk:json-value object :object "source")))
                      (list :name (nlk:json-value source :string "name")
                            :size (nlk:json-value source :integer "size")
                            :lines (nlk:json-value source :integer "lines")))
            :attempts (loop for item across (or (nlk:json-value object :array "attempts") #())
                            collect (list :id (nlk:json-value item :string "id")
                                          :outcome (nlk:json-value item :string "outcome")
                                          :sequence (nlk:json-value item :integer "sequence")
                                          :model (nlk:json-value item :string "model")))
            :gaps (loop for item across (or (nlk:json-value object :array "gaps") #())
                        collect (list :kind (nlk:json-value item :string "kind")
                                      :detail (nlk:json-value item :string "detail")))
            :events (loop for item across (or (nlk:json-value object :array "events") #())
                          collect (list :id (nlk:json-value item :string "id")
                                        :attempt (nlk:json-value item :string "attempt")
                                        :kind (nlk:json-value item :string "kind")
                                        :status (or (nlk:json-value item :string "status") "unknown")
                                        :provenance (or (nlk:json-value item :string "provenance")
                                                         "unknown")
                                        :sequence (or (nlk:json-value item :integer "sequence") 0)
                                        :wall (nlk:json-value item :string "wall")
                                        :name (nlk:json-value item :string "name")
                                        :error (nlk:json-value item :string "error")
                                        :tokens-in (nlk:json-value item :integer "tokens_in")
                                        :tokens-out (nlk:json-value item :integer "tokens_out")
                                        :cost (nlk:json-value item :number "cost")
                                        :parents (strings-of item "parents")))))))

(defun stored-run-ids ()
  "Every run id under the home, newest file first."
  (let ((files (sort (ignore-errors (uiop:directory-files (runs-directory)))
                     #'> :key (lambda (path) (or (ignore-errors (file-write-date path)) 0)))))
    (loop for path in files
          for object = (ignore-errors
                        (nlk:decode-json (uiop:read-file-string path :external-format :utf-8)))
          when object collect (nlk:json-value object :string "run_id"))))

(defun run (run-id)
  "The run RUN-ID, read from the store when this image has not seen it yet."
  (when *runs*
    (or (gethash (or run-id "") *runs*)
        (let ((path (run-file (or run-id ""))))
          (when (probe-file path)
            (let ((run (object->run (nlk:decode-json
                                      (uiop:read-file-string path :external-format :utf-8)))))
              (when run (setf (gethash (getf run :run-id) *runs*) run))))))))

(defun runs ()
  "Every run this cell holds, newest import first, each (RUN-ID IMPORTED-AT
EVENTS GAPS)."
  (let ((found '()))
    (when *runs*
      (maphash (lambda (id run) (declare (ignore id)) (push run found)) *runs*))
    (sort found #'string> :key (lambda (run) (or (getf run :imported-at) "")))))

(defun prune-runs ()
  "Delete the run files older than the section's retention, and only those:
  every path is one PRUNE-RUNS wrote, by its own digest name."
  (let* ((days (setting :retention-days))
         (cutoff (- (get-universal-time) (* 86400 (max 1 days)))))
    (dolist (path (ignore-errors (uiop:directory-files (runs-directory))))
      (let ((written (ignore-errors (file-write-date path))))
        (when (and written (< written cutoff))
          (ignore-errors (delete-file path)))))))

;;; --- the guards ----------------------------------------------------------------

(defun suffix-of (path)
  "PATH's name lowercased, from its first dot, or NIL."
  (let* ((name (string-downcase (file-namestring path)))
         (dot (position #\. name)))
    (and dot (subseq name dot))))

(defun suffix-member (suffix list)
  "The entry of LIST SUFFIX ends with, when it does."
  (loop for candidate in (sort (copy-list list) #'> :key #'length)
        when (and suffix (<= (length candidate) (length suffix))
                  (string= candidate suffix :start2 (- (length suffix) (length candidate))))
          return candidate))

(defun guard-source (path)
  "Refuse what is not an explicitly selected, readable trace FILE: a
  directory, an archive, a database, or nothing at all."
  (let ((path (pathname path)))
    (when (uiop:directory-pathname-p path)
      (fail "~a is a directory: this cell imports files you name, one by one; it never
scans a folder. Name the trace file itself." path))
    (unless (probe-file path)
      (fail "there is no file ~a." path))
    (let ((archive (suffix-member (suffix-of path) +archive-suffixes+)))
      (when archive
        (fail "~a is an archive (~a): this build imports JSONL and never extracts an archive.
Point it at the trace file itself." path archive)))
    (let ((database (suffix-member (suffix-of path) +database-suffixes+)))
      (when database
        (fail "~a is a raw database (~a): this build imports JSONL only. Export the run to a
JSONL trace first." path database)))))

(defun read-source (path)
  "PATH's text, bounded by the section's own limits. => (values TEXT SIZE DIGEST)."
  (let* ((limit (setting :max-bytes))
         (size (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8))
                                (file-length in))))
         (text (ignore-errors (uiop:read-file-string path :external-format :utf-8))))
    (unless text
      (fail "~a could not be read as UTF-8 text; nothing was imported." path))
    (when (or (and size (> size limit)) (> (length text) limit))
      (fail "~a is larger than the ~:d-byte import bound (run-debugger.max_bytes); nothing was
imported. Select a smaller trace or raise the bound." (truename path) limit))
    (values text (or size (length text)) (nlk:sha256-text text))))

(defun split-lines (text)
  "TEXT's lines, and whether it ended on a newline."
  (let ((lines (nlk:lines text)))
    (values lines (and (plusp (length text)) (char= (char text (1- (length text))) #\Newline)))))

;;; --- validation ----------------------------------------------------------------

(defun parse-line (line number)
  "LINE, record NUMBER of the source, as a JSON object; a refusal says which
line and why, and never repeats the line's content."
  (let ((object (ignore-errors (nlk:decode-json line))))
    (unless (hash-table-p object)
      (fail "line ~d of the source is not one JSON object (or the trace ends mid-record);
nothing was imported. Set run-debugger.allow_partial to keep the valid prefix." number))
    object))

(defun member-string (object key where)
  "KEY of OBJECT as a non-empty string, or a refusal naming WHERE."
  (let ((value (nlk:json-value object :string key)))
    (unless (and value (plusp (length value)))
      (fail "~a: \"~a\" is required and must be a non-empty string." where key))
    value))

(defun member-in (object key allowed where &key (default "unknown"))
  "KEY of OBJECT as one of ALLOWED; absent takes DEFAULT, anything else refuses."
  (let ((value (nlk:json-value object :string key)))
    (cond ((null value) default)
          ((member value allowed :test #'string=) value)
          (t (fail "~a: \"~a\" is ~s, which this format does not name (it names ~{~a~^, ~})."
                   where key value allowed)))))

(defun parse-envelope (object path digest)
  "OBJECT, the first record, as the run's envelope; anything else refuses."
  (let ((where (format nil "~a (first record)" path)))
    (unless (equal "trace" (nlk:json-value object :string "kind"))
      (fail "~a: the first record is not the trace envelope (\"kind\": \"trace\"); this cell
reads traces in its own format. Nothing was imported." path))
    (unless (equal +schema+ (nlk:json-value object :string "schema"))
      (fail "~a: schema ~s is not ~s: an unknown schema is refused, never parsed on a hope."
             where (nlk:json-value object :string "schema") +schema+))
    (let ((version (nlk:json-value object :integer "version")))
      (unless (eql version +version+)
        (fail "~a: version ~s is not ~d." where version +version+)))
    (let ((run-id (member-string object "run_id" where)))
      (unless (<= (length run-id) +id-max+)
        (fail "~a: run_id is longer than ~d characters." where +id-max+))
      (list :run-id run-id
            :digest digest
            :completeness (member-in (nlk:json-value object :object "completeness")
                                     "status" +completeness+ where :default "unknown")))))

(defun parse-attempt (object where)
  "OBJECT as one attempt record."
  (list :id (member-string object "attempt_id" where)
        :outcome (member-in object "outcome" +outcomes+ where)
        :sequence (or (nlk:json-value object :integer "sequence") 0)
        :model (nlk:json-value object :string "model")))

(defun parse-event (object run-id where)
  "OBJECT as one event record. Every word it reports must be one this format
names, or the whole import refuses."
  (let ((kind (member-in object "event_kind" +event-kinds+ where)))
    (list :id (member-string object "event_id" where)
          :run run-id
          :attempt (nlk:json-value object :string "attempt_id")
          :kind kind
          :status (member-in object "reported_status" +statuses+ where)
          :provenance (member-in object "semantic_provenance" +provenances+ where)
          :sequence (or (nlk:json-value object :integer "source_sequence") 0)
          :wall (nlk:json-value object :string "wall_time")
          :name (nlk:json-value object :string "name")
          :error (nlk:json-value object :string "reported_error")
          :tokens-in (nlk:json-value object :integer "usage" "input_tokens")
          :tokens-out (nlk:json-value object :integer "usage" "output_tokens")
          :cost (nlk:json-value object :number "usage" "cost")
          :parents (strings-of object "causal_parents"))))

;;; --- the import ----------------------------------------------------------------

(defun import-trace (files &key format allow-partial)
  "Import the trace FILES the caller names; => the text a caller reads.
FILES is a path or a list of them. Nothing is executed and nothing leaves
the machine."
  (running-settings)
  (let ((files (cond ((stringp files) (list files))
                      ((vectorp files) (coerce files 'list))
                      ((listp files) files)
                      (t (list files))))
        (imported '()))
    (unless files
      (fail "name at least one trace file to import."))
    (dolist (file files)
      (push (import-one file format allow-partial) imported))
    (format nil "imported ~d run~:p:~%~{~a~}"
            (length imported) (mapcar #'import-line (nreverse imported)))))

(defun import-line (run)
  (format nil "  run ~a: ~d event~:p, ~d attempt~:p, ~d gap~:p, completeness ~a~@[ (PARTIAL import:
  the valid prefix was kept)~], kept in ~a"
          (getf run :run-id) (length (getf run :events)) (length (getf run :attempts))
          (length (getf run :gaps)) (getf run :completeness)
          (and (equal "partial" (getf run :completeness)) t)
          (getf (getf run :source) :name)))

(defun import-one (path format allow-partial)
  "PATH read and stored as one run; => the run plist."
  (guard-source path)
  (when (and format (not (member (string-downcase format) '("v1") :test #'string=)))
    (fail "format ~s is not one this build reads; it reads \"v1\" (the trace
format this cell's own debut defines)." format))
  (multiple-value-bind (text size digest) (read-source path)
    (multiple-value-bind (lines terminated-p) (split-lines text)
      (let* ((partial (or allow-partial (setting :allow-partial)))
             (objects '())
             (notes '())
             (gaps '())
             (kept lines))
        (when (> (length lines) (setting :max-events))
          (fail "~a has ~d records, over the ~d-record bound (run-debugger.max_events);
nothing was imported." (truename path) (length lines) (setting :max-events)))
        ;; A last line the file never ended is the one truncation this
        ;; reader can see. With ALLOW-PARTIAL the valid prefix is kept, marked.
        (when (and lines (not terminated-p)
                   (not (hash-table-p (ignore-errors
                                        (nlk:decode-json (car (last lines)))))))
          (unless partial
            (fail "~a ends mid-record: the last line is not a complete JSON object. Set
run-debugger.allow_partial (or the call's allow_partial) to keep the valid prefix, or
select a complete export." (truename path)))
          (setf kept (butlast lines))
          (push (list :kind "truncation"
                      :detail "the final line was truncated; only the valid prefix was kept")
                gaps)
          (push "import is PARTIAL: the source ended mid-record" notes))
        (loop for line in kept
              for number from 1
              when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
                do (when (> (length line) (setting :max-line-bytes))
                     (fail "line ~d is longer than the ~:d-byte line bound
(run-debugger.max_line_bytes); nothing was imported."
                           number (setting :max-line-bytes)))
                   (push (parse-line line number) objects))
        (let* ((objects (nreverse objects))
               (envelope (parse-envelope (or (first objects)
                                              (fail "~a is empty: nothing to import."
                                                    (truename path)))
                                          path digest))
               (run-id (getf envelope :run-id))
               (events '())
               (attempts '()))
          (dolist (object (rest objects))
            (let ((kind (nlk:json-value object :string "kind")))
              (cond
                ((equal kind "event")
                 (let ((event (parse-event object run-id
                                            (format nil "~a, event ~a"
                                                    path (nlk:json-value object :string "event_id")))))
                   (when (and (getf event :run) (not (equal (getf event :run) run-id)))
                     (fail "~a carries an event whose run_id is not the envelope's (~a)."
                           path run-id))
                   (push event events)))
                ((equal kind "attempt") (push (parse-attempt object path) attempts))
                ((member kind '("trace") :test #'string=) nil)
                (t (fail "~a: record kind ~s is not one this format names (trace, attempt,
event)." path kind)))))
          (setf events (nreverse events) attempts (nreverse attempts))
          ;; A parent the trace does not carry is a GAP, never an inferred edge.
          (let ((known (mapcar (lambda (event) (getf event :id)) events)))
            (dolist (event events)
              (dolist (parent (getf event :parents))
                (unless (member parent known :test #'string=)
                  (push (list :kind "missing_parent"
                              :detail (format nil "~a names parent ~a, which this trace does not
carry; it stays a root with a gap" (getf event :id) parent))
                        gaps)))))
          (let ((run (list :run-id run-id
                           :imported-at (nlk:iso-now)
                           :format "v1"
                           :digest digest
                           :completeness (if gaps "partial" (getf envelope :completeness))
                           :source (list :name (file-namestring path) :size size
                                         :lines (length kept))
                           :attempts attempts
                           :events events
                           :gaps (nreverse gaps))))
            (write-run run)
            (setf (gethash run-id *runs*) run)
            run))))))
