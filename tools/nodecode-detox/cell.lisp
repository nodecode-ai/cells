;;;; cell.lisp --- the whole cell: the hook, the three tools, /detox, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of kellycryptos's hermes-detox. One hook on the
;;;; :TOOL point wraps every tool dispatch: an answer over `min_size'
;;;; characters is redacted, kept as an evidence record under the home, and
;;;; replaced by a compact report that names the record and the line numbers
;;;; of the original text. Three tools and /detox read it back.
;;;;
;;;; It fails open on every path: nothing in here may lose a tool result. A
;;;; store that cannot be written, a report that does not come out small
;;;; enough, an error anywhere -- the original answer stands, as though the
;;;; cell were not there.

(in-package #:nodecode-detox)

;;; --- what this cell owns ----------------------------------------------------

(defparameter +own-tools+ '("detox_inspect" "detox_search" "detox_status")
  "This cell's own three tools, never intercepted, so retrieval cannot recurse.")

(defparameter +editors+ '("write_file" "edit" "patch")
  "Tools whose answer is the file they wrote, which no report describes better.")

(defparameter +usage+
  "/detox                     what the firewall has done; what is kept
/detox inspect ID [N] [M]   lines N to M of the record ID, as numbered lines
/detox search PHRASE [ID]   where PHRASE appears, with record and line number
/detox purge [DAYS]         drop records older than DAYS (default: the section's)
"
  "What /detox answers, one line each.")

(defparameter +manual+
  "detox: huge tool results arrive as a compact report instead of flooding the context.
A tool call answering more than the section's min_size characters has its full text redacted and kept under the home as an evidence record, and the model is shown a short report: the tool, the size kept, the first and last lines with the original's own line numbers, every error and warning line, and the evidence id. The full text is never thrown away, only put aside.
The model reads a line number in a report, then calls detox_inspect(evidence_id, start_line, end_line) for those exact lines, or detox_search(query, evidence_id) to find a phrase. detox_status counts what has been intercepted and what it saved; /detox does the same from the operator's side.
A result smaller than min_size, a tool in excluded_tools, an image, audio or base64 payload, and this cell's own three tools are never intercepted. A report that does not come out at most max_report_percent of the original (and under max_report_chars) is dropped, and the original answer stands.
The store is one text file and one metadata file per record, in the detox folder under the home, unencrypted and readable by anything running as this user: retention_days and max_store_mb bound it, /detox purge empties it, and redaction is pattern-based, so a secret of a shape the patterns do not know is kept as it arrived."
  "(help :detox) answers this.")

;;; --- the contexts a call runs in ---------------------------------------------

(defun current-session ()
  "The session a call runs in, or NIL outside a turn."
  (getf (nle:turn) :session-id))

(defun excluded-tools ()
  "The tools never intercepted: this cell's own, the file editors, and the section's list."
  (remove-duplicates (append (or (setting :excluded-tools) '()) +editors+ +own-tools+)
                     :test #'string-equal))

(defun media-p (text)
  "Whether TEXT opens as an image, a video, audio, or a base64 payload."
  (let ((head (clip text 500)))
    (or (not (null (search "data:image/" head)))
        (not (null (search "data:video/" head)))
        (not (null (search "data:audio/" head)))
        (not (null (search ";base64," head))))))

(defun session-ids ()
  "The evidence ids this session kept; every id when it runs in no session."
  (let ((session (current-session)))
    (if session
        (remove-if-not (lambda (id) (equal session (getf (record-meta id) :session)))
                       (evidence-ids))
        (evidence-ids))))

;;; --- the interception -------------------------------------------------------

(defun compact-if-large (tool text)
  "TEXT's compact report, kept as evidence under a fresh id, or NIL when the original stands."
  (when (and (stringp tool)
             (setting :enabled)
             (not (member tool (excluded-tools) :test #'string-equal))
             (> (length text) (setting :min-size))
             (not (media-p text))
             (not (report-text-p text)))
    (let* ((clean (redact-secrets text))
           (type (content-type clean))
           (status (if (and (>= (length clean) 6) (string= "ERROR:" clean :end2 6)) "error" "success"))
           (session (current-session))
           (id (save-evidence clean :tool tool :content-type type
                                     :session session :status status)))
      (when id
        (let* ((report (fit-report tool clean id type
                                    :head (setting :head-lines)
                                    :tail (setting :tail-lines)
                                    :max-chars (setting :max-report-chars)))
               (size (if report (length report) 0)))
          (write-meta id :tool tool :type type :session session :status status
                         :chars (length clean) :lines (text-line-count clean) :report size)
          (when (and report (<= (* 100 size) (* (setting :max-report-percent) (length clean))))
            report))))))

(defun intercept (op next)
  "A :TOOL point hook: NEXT's answer, compacted when it is a large output.
OP is (:name TOOL-NAME :arguments HASH :call-id ID); anything but an error
in here leaves NEXT's answer exactly as it was."
  (let ((text (funcall next op)))
    (if (not (stringp text))
        text
        (or (handler-case (compact-if-large (getf op :name) text) (error () nil))
            text))))

;;; --- reading evidence back ---------------------------------------------------

(define-verb inspect-evidence (id &key start end max-chars)
  "The lines START to END (1-based, inclusive) of the record ID keeps, numbered as the original's."
  (unless (evidence-id-p id)
    (fail "~s is not an evidence id: the ones a report prints look like ev_4f1b8a92e10c" id))
  (let ((text (record-text id)))
    (unless text
      (fail "no evidence ~a is kept here; detox:status says which ones are" id))
    (let* ((lines (text-lines text))
           (total (length lines))
           (from (max 1 (or start 1)))
           (to (min total (or end total))))
      (when (> from to)
        (fail "~a holds ~d lines: ~d to ~d is empty" id total from to))
      (let* ((body (format nil "~{~a~^~%~}"
                           (loop for line in (subseq lines (1- from) to)
                                 for number from from
                                 collect (format nil "~4d | ~a" number (clip line +line-clip+)))))
             (head (format nil "~a, lines ~d-~d of ~d:" id from to total))
             (room (and max-chars (max 0 (- max-chars (1+ (length head)))))))
        (cond ((and room (< room (length body)))
               (format nil "~a~%~a~%... ~d characters asked for and ~d answered: a narrower range, or a larger max_chars, answers the rest"
                       head (clip body room) (length body) room))
              (t (format nil "~a~%~a" head body)))))))

(define-verb search-evidence (query &key evidence-id case-sensitive (limit 20))
  "Where QUERY appears: one line per match, the record and the original line number."
  (unless (and (stringp query) (plusp (length query)))
    (fail "search for something: a phrase, found as a plain substring"))
  (let* ((ids (cond (evidence-id
                     (unless (evidence-id-p evidence-id)
                       (fail "~s is not an evidence id" evidence-id))
                     (if (probe-file (record-path evidence-id))
                         (list evidence-id)
                         (fail "no evidence ~a is kept here" evidence-id)))
                    (t (session-ids))))
         (limit (max 1 (or limit 20)))
         (needle (if case-sensitive query (string-downcase query)))
         (found '()))
    (dolist (id ids)
      (let ((text (ignore-errors (record-text id))))
        (when text
          (loop for line in (text-lines text)
                for number from 1
                while (< (length found) limit)
                when (not (null (search needle (if case-sensitive line (string-downcase line)))))
                  do (push (format nil "~a:~d: ~a" id number
                                   (clip (string-trim '(#\Space #\Tab) line) 200))
                           found)))))
    (if found
        (format nil "~{~a~^~%~}" (nreverse found))
        (format nil "nothing matches ~s in ~d record~:p" query (length ids)))))

(define-verb status (&key session-only)
  "What the firewall has intercepted and kept: records, characters in and out, saved."
  (let* ((ids (if session-only (session-ids) (evidence-ids)))
         (metas (mapcar #'record-meta ids))
         (in (reduce #'+ metas :key (lambda (meta) (or (getf meta :chars) 0)) :initial-value 0))
         (out (reduce #'+ metas :key (lambda (meta) (or (getf meta :report) 0)) :initial-value 0))
         (saved (max 0 (- in out))))
    (format nil "detox: ~d record~:p~@[, this session~], ~:d characters in, ~:d out, ~:d saved~@[ (~d%)~]~%kept in the detox folder under the home: ~:d bytes~@[, holding records to ~d days and ~d MB~]"
            (length ids) (and session-only t) in out saved
            (and (plusp in) (round (* 100 saved) in))
            (store-bytes)
            (not (null ids)) (setting :retention-days) (setting :max-store-mb))))

(define-verb purge (&optional days)
  "Drop records older than DAYS, then the oldest until the store fits max_store_mb."
  (let* ((days (or days (setting :retention-days)))
         (by-age (purge-older-than days))
         (by-size (enforce-size-cap (setting :max-store-mb))))
    (format nil "detox: ~d record~p older than ~d day~:p dropped, ~d more to stay under ~d MB; ~d kept, ~:d bytes"
            by-age by-age days by-size (setting :max-store-mb)
            (length (evidence-ids)) (store-bytes))))

;;; --- /detox and the tools ---------------------------------------------------

(defun number-or-nil (text)
  "TEXT as an integer when it starts with one, else NIL."
  (and text (parse-integer text :junk-allowed t)))

(defun run-slash (args session-id)
  "One /detox invocation: status, inspect, search, purge, or what else it says."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))))
         (verb (string-downcase (or (first words) "status"))))
    (cond ((or (string= verb "status") (string= verb "")) (status))
          ((string= verb "inspect")
           (if (second words)
               (inspect-evidence (second words)
                                  :start (number-or-nil (third words))
                                  :end (number-or-nil (fourth words)))
               (format nil "which record? ~%~a" +usage+)))
          ((string= verb "search")
           (if (second words)
               (search-evidence (second words) :evidence-id (third words))
               (format nil "which phrase? ~%~a" +usage+)))
          ((string= verb "purge")
           (purge (number-or-nil (second words))))
          (t (format nil "detox does not know ~s.~%~a" (first words) +usage+)))))

(defun tool-inspect (report)
  "The detox_inspect tool: the record the arguments name, NUMBered lines."
  (inspect-evidence (detox-inspect-evidence-id report)
                    :start (detox-inspect-start-line report)
                    :end (detox-inspect-end-line report)
                    :max-chars (detox-inspect-max-chars report)))

(defun tool-search (report)
  "The detox_search tool: where the query appears, in which record and at which line."
  (search-evidence (detox-search-query report)
                   :evidence-id (detox-search-evidence-id report)
                   :case-sensitive (detox-search-case-sensitive report)
                   :limit (detox-search-limit report)))

(defun tool-status (report)
  "The detox_status tool: what was intercepted, and what it saved."
  (status :session-only (detox-status-session-only report)))

;;; --- the two boundaries ------------------------------------------------------

(nlk:define-contract detox-inspect
  (:doc "Read exact lines of a tool output that detox kept, by the evidence id a compact report named. Line numbers are the original's, so a number read in a report is the number to ask for.")
  ("evidence_id" :text :required t
   :doc "the evidence id a report printed, like ev_4f1b8a92e10c")
  ("start_line" :integer :default 1
   :doc "the 1-based first line to answer")
  ("end_line" :integer
   :doc "the 1-based last line, inclusive; left out reads to the end of the record")
  ("max_chars" :integer
   :doc "a ceiling on the characters answered; left out answers the whole range"))

(nlk:define-contract detox-search
  (:doc "Find a phrase in what detox kept: one line per match, the evidence id and the original's line number.")
  ("query" :text :required t
   :doc "the phrase to find, matched as a plain substring")
  ("evidence_id" :text
   :doc "search only this record; left out searches this session's records")
  ("case_sensitive" :boolean :default nil
   :doc "match case as typed; default matches either case")
  ("limit" :integer :default 20
   :doc "the most matching lines to answer"))

(nlk:define-contract detox-status
  (:doc "What the detox firewall has intercepted and kept: records, characters in and out, characters saved, and how much the store holds.")
  ("session_only" :boolean :default nil
   :doc "count this session's records only; default counts every record kept here"))

(nle:define-cell detox
  (:section ("detox")
    (:guide "nothing here is required; min_size is the length at which a tool result becomes a report, and max_report_percent how small that report must be; retention_days and max_store_mb bound what is kept")
    ("enabled" :boolean :default t
     :doc "intercept large tool results at all")
    ("min_size" :integer :default 6000 :min 100
     :doc "a tool result longer than this, in characters, becomes a report")
    ("max_report_chars" :integer :default 3000 :min 500
     :doc "a report longer than this is cut back, and dropped if even the barest does not fit")
    ("max_report_percent" :integer :default 55 :min 10
     :doc "a report must be at most this percent of the original, or the original stands")
    ("head_lines" :integer :default 5 :min 1
     :doc "how many leading lines a report shows")
    ("tail_lines" :integer :default 5 :min 1
     :doc "how many trailing lines a report shows")
    ("excluded_tools" :list
     :doc "tools whose results are never intercepted, beside this cell's own three and the file editors")
    ("retention_days" :integer :default 30 :min 1
     :doc "records older than this are dropped at start and by /detox purge")
    ("max_store_mb" :integer :default 100 :min 5
     :doc "the oldest records go once the store is larger than this"))
  (:tool "detox_inspect" (nlk:contract 'detox-inspect) 'tool-inspect)
  (:tool "detox_search" (nlk:contract 'detox-search) 'tool-search)
  (:tool "detox_status" (nlk:contract 'detox-status) 'tool-status)
  (:help :detox "detox: huge tool results arrive as a compact report - (detox:status)" +manual+)
  (:command "detox" 'run-slash
            :description "What the tool-output firewall kept, and the lines behind a report"
            :argument-hint "status | inspect ID [N] [M] | search PHRASE [ID] | purge [DAYS]")
  (:hook :tool #'intercept)
  (:start (lambda ()
            (ignore-errors (ensure-directories-exist (store-directory)))
            (ignore-errors (purge)))))
