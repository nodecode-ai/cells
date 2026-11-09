;;;; store.lisp --- one JSON document on this machine, and the run it keeps.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept runs, events, findings and silences in SQLite behind a
;;;; background writer thread. A cell has no database driver and no second
;;;; process, so the same tables are one JSON document under the Nodecode
;;;; home, written whole and atomically after each finished turn: the recorder
;;;; never blocks a turn on a disk, and a store that will not read is a
;;;; refusal naming the file rather than a store quietly replaced.

(in-package #:nodecode-flightlog)

(nlk:define-peripheral flightlog :not-running t :lock "flightlog")

;;; --- one run, while it runs --------------------------------------------------

(defstruct (run (:conc-name run-))
  "One turn's flight record. A turn is a run: the same thing the original
called a session's run, keyed by the turn it belongs to."
  (id "" :read-only t)
  (session "" :read-only t)
  (job nil :read-only t)
  (started (get-universal-time))
  (ended nil)
  (outcome "ok")
  (model "")
  (tokens 0)
  (tool-calls 0)
  (tool-errors 0)
  (answer "")
  (events '())
  (seq 0)
  (findings nil))

(defun run-ok-p (run)
  "T when the run ended the way its job wanted: it completed and nothing
said otherwise."
  (equal (run-outcome run) "ok"))

(defun run-duration-ms (run)
  (when (run-ended run) (round (* 1000 (- (run-ended run) (run-started run))))))

;;; --- the document ------------------------------------------------------------

(defparameter +store-name+ "flightlog.json"
  "The one file a home's flight record lives in.")

(defvar *store* nil
  "The store this process holds, or NIL while the cell is not started.")

(defun store-directory ()
  "The folder the store lives in: the section's dir, else flightlog/ under the home."
  (uiop:ensure-directory-pathname (or (ignore-errors (setting :dir)) (nlk:home "flightlog/"))))

(defun store-file () (merge-pathnames +store-name+ (store-directory)))

(defun empty-store ()
  (nlk:json-object "version" 1 "runs" #() "silenced" (nlk:json-object)))

(defun load-store ()
  "The store on disk, or an empty one. A file that will not read is a refusal
naming the path: a start never quietly replaces what it cannot read."
  (let ((path (store-file)))
    (if (probe-file path)
        (let ((text (ignore-errors (nlk:read-text path))))
          (let ((object (and text (plusp (length (nlk:trimmed text)))
                             (ignore-errors (nlk:decode-json text)))))
            (if (hash-table-p object)
                object
                (fail "flightlog: ~a will not read as a store; move it aside and start again" (namestring path)))))
        (empty-store))))

(defun save-store (store)
  "STORE written whole and atomically, readable by its owner only. => its path."
  (let ((path (store-file)))
    (nlk:write-file-atomically path (format nil "~a~%" (nlk:encode-json-object store))
                               :mode #o600 :directory-mode #o700)
    path))

(defun runs-of (store) (coerce (or (nlk:json-value store :array "runs") #()) 'list))
(defun (setf runs-of) (rows store) (setf (gethash "runs" store) (coerce rows 'vector)))

(defun silenced-of (store)
  (let ((table (gethash "silenced" store)))
    (if (hash-table-p table) table (setf (gethash "silenced" store) (nlk:json-object)))))

(defun silenced-rules (job)
  "The rules acknowledged away for JOB, as a list of names."
  (if (null job)
      '()
      (coerce (gethash job (silenced-of *store*)) 'list)))

(defun silence-rule (job rule)
  (when job
    (let* ((table (silenced-of *store*))
           (rules (coerce (gethash job table) 'list)))
      (unless (member rule rules :test #'equal)
        (setf (gethash job table) (coerce (cons rule rules) 'vector))))))

;;; --- a row, as both halves of the cell read it --------------------------------

(defun row-string (row key) (or (nlk:json-value row :string key) ""))
(defun row-integer (row key) (or (nlk:json-value row :integer key) 0))
(defun row-array (row key) (coerce (or (nlk:json-value row :array key) #()) 'list))

(defun event-from (object)
  "One recorded step, as a plist."
  (list :seq (row-integer object "seq")
        :kind (row-string object "kind")
        :name (row-string object "name")
        :status (row-string object "status")
        :ts (nlk:json-value object :number "ts")
        :duration-ms (nlk:json-value object :integer "duration_ms")
        :tokens (nlk:json-value object :integer "tokens")
        :target (row-string object "target")
        :detail (row-string object "detail")))

(defun finding-from (object)
  "One finding, as a plist, with the id an acknowledgement takes."
  (list :id (row-string object "id")
        :rule (row-string object "rule")
        :summary (row-string object "summary")
        :report (row-string object "report")
        :acked (and (nlk:json-value object :boolean "acked") t)
        :evidence (mapcar (lambda (value) (and (integerp value) value))
                          (row-array object "evidence"))))

(defun run-findings-of (row)
  (mapcar #'finding-from (row-array row "findings")))

(defun run-events-of (row) (mapcar #'event-from (row-array row "events")))

(defun encode-event (event)
  (nlk:json-object "seq" (getf event :seq)
                    "kind" (getf event :kind)
                    "name" (or (getf event :name) "")
                    "status" (or (getf event :status) "ok")
                    "ts" (or (getf event :ts) (nlk:unix-now))
                    :opt "duration_ms" (getf event :duration-ms)
                    :opt "tokens" (getf event :tokens)
                    :opt "target" (let ((text (getf event :target)))
                                     (and text (plusp (length text)) text))
                    :opt "detail" (let ((text (getf event :detail)))
                                    (and text (plusp (length text)) text))))

(defun encode-finding (item)
  (nlk:json-object "id" (getf item :id)
                    "rule" (getf item :rule)
                    "summary" (getf item :summary)
                    "report" (or (getf item :report) "")
                    "acked" (and (getf item :acked) t)
                    "evidence" (coerce (remove nil (getf item :evidence)) 'vector)))

(defun encode-run (run findings)
  "RUN as the row a report and a listing read."
  (nlk:json-object "id" (run-id run)
                    "session" (run-session run)
                    :opt "job" (run-job run)
                    "started" (run-started run)
                    "ended" (or (run-ended run) (run-started run))
                    "outcome" (run-outcome run)
                    "model" (run-model run)
                    "tokens" (run-tokens run)
                    "tool_calls" (run-tool-calls run)
                    "tool_errors" (run-tool-errors run)
                    "answer" (or (run-answer run) "")
                    "events" (coerce (mapcar #'encode-event (run-events run)) 'vector)
                    "findings" (coerce (mapcar #'encode-finding findings) 'vector)))

;;; --- writing and reading the record -------------------------------------------

(defun record-run (row)
  "ROW at the front of the record, the oldest past keep_runs dropped, the file
written. => the row."
  (let* ((limit (ignore-errors (setting :keep-runs)))
         (limit (or limit 200))
         (rows (cons row (runs-of *store*))))
    (setf (runs-of *store*) (if (> (length rows) limit) (subseq rows 0 limit) rows))
    (save-store *store*)
    row))

(defun recent-runs (&key (limit 5) job)
  "The newest LIMIT runs, newest first, or JOB's own."
  (let ((rows (runs-of *store*)))
    (when job (setf rows (remove-if-not (lambda (row) (equal job (row-string row "job"))) rows)))
    (if (> (length rows) limit) (subseq rows 0 limit) rows)))

(defun find-runs (reference)
  "The runs REFERENCE names: an id, a session, or a prefix of either, oldest first."
  (let ((wanted (nlk:trimmed reference)))
    (sort (remove-if-not (lambda (row)
                           (or (string= wanted (row-string row "id"))
                               (string= wanted (row-string row "session"))
                               (and (< (length wanted) (length (row-string row "id")))
                                    (eql 0 (search wanted (row-string row "id"))))
                               (and (plusp (length wanted))
                                    (< (length wanted) (length (row-string row "session")))
                                    (eql 0 (search wanted (row-string row "session"))))))
                         (runs-of *store*))
          #'< :key (lambda (row) (or (nlk:json-value row :number "started") 0)))))

(defun healthy-rows (job &key (limit 7))
  "JOB's recent runs that finished cleanly and earned no finding: the sample
a baseline is read from."
  (let ((rows (remove-if-not (lambda (row)
                                (and (equal job (row-string row "job"))
                                     (equal "ok" (row-string row "outcome"))
                                     (null (run-findings-of row))))
                              (runs-of *store*))))
    (when (> (length rows) limit) (setf rows (subseq rows 0 limit)))
    (mapcar (lambda (row)
              (list :tool-calls (or (nlk:json-value row :integer "tool_calls") 0)
                    :tokens (or (nlk:json-value row :integer "tokens") 0)
                    :duration-ms (round (* 1000 (- (or (nlk:json-value row :number "ended") 0)
                                                    (or (nlk:json-value row :number "started") 0))))))
            rows)))

(defun ack-finding (reference)
  "Acknowledge the finding REFERENCE names by id or prefix, and silence its
rule for that finding's job. => the finding, or NIL."
  (let ((wanted (nlk:trimmed reference)))
    (dolist (row (runs-of *store*) nil)
      (dolist (object (row-array row "findings") nil)
        (let ((id (row-string object "id")))
          (when (and (plusp (length id))
                     (or (string= wanted id)
                         (and (plusp (length wanted)) (< (length wanted) (length id))
                              (eql 0 (search wanted id)))))
            (setf (gethash "acked" object) t)
            (silence-rule (row-string row "job") (row-string object "rule"))
            (save-store *store*)
            (return-from ack-finding (finding-from object))))))))

(defun purge-store ()
  "Forget every run, every silence, and write the empty document."
  (setf *store* (empty-store))
  (save-store *store*))

(defun runs-count-since (seconds)
  "How many runs ended in the last SECONDS, and how many of them carry an
unacknowledged finding: two values."
  (let ((cutoff (- (nlk:unix-now) seconds))
        (runs 0) (flagged 0))
    (dolist (row (runs-of *store*))
      (when (>= (or (nlk:json-value row :number "ended") 0) cutoff)
        (incf runs)
        (when (find nil (run-findings-of row) :key (lambda (item) (getf item :acked)))
          (incf flagged))))
    (values runs flagged)))
