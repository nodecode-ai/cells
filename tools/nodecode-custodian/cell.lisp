;;;; cell.lisp --- the known-failure fingerprints, the scan, the issue ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Hermes plugin named custodian watched one harness's logs, cron registry
;;;; and skill journals, matched each line against a table of fingerprints it
;;;; learned from real failures, and kept an issue ledger. This cell keeps the
;;;; part that is not Hermes's own: the fingerprint table (35 entries, each
;;;; with the tier that says whether a repair is routine, a diagnosis or an
;;;; escalation, and the fix its author found), the scan that reports a match
;;;; once -- skipping lines that merely quote a pattern and lines older than
;;;; the window -- with the matching line as evidence, and an issue ledger
;;;; that bumps a seen issue instead of filing it twice.
;;;;
;;;; What is Hermes's is left out (see NOTICE): the storage is this home's
;;;; `custodian/' directory, the sources are whatever log files the operator
;;;; names, and nothing repairs anything by itself. A finding carries the fix
;;;; in words; the model or the operator applies it.

(defpackage #:nodecode-custodian
  (:use #:cl)
  (:nicknames #:custodian)
  (:export #:*fingerprints*
           #:scan-text #:scan-files #:run-scan
           #:issues #:status #:custodian-error))

(in-package #:nodecode-custodian)

(nlk:define-peripheral custodian :not-running t)

;;; --- the fingerprints -------------------------------------------------------------
;;; Data, so a layer can add, mute or reword one without touching the scanner.
;;; :TIER 1 is a known routine repair, 2 a diagnosis, 3 an escalation, 4 a dead
;;; process; :FIX is what the original's fix engine ran, in words.

(defun fingerprint (id tier source fix &rest patterns)
  (list :id id :tier tier :source source :fix fix :patterns patterns))

(defparameter *fingerprints*
  (list
   ;; --- scheduled jobs ---------------------------------------------------------
   (fingerprint "oc_cron_disabled_transient" 1 "cron_log"
                "Re-enable the disabled job (resume it in the scheduler)."
                "job\\s+disabl"
                "job\\s+'[^']+'\\s+(?:has been |was )?disabl"
                "disabl(?:ing|ed)\\s+(?:cron\\s+)?job\\s+'"
                "cron job[^\\n]*\\benabled\\b[^\\n]*\\bfalse\\b")
   (fingerprint "oc_cron_stuck_missed" 1 "cron_log"
                "Force-run the missed job."
                "^(?!.*\\b(Running now|re-anchored)\\b).*missed its scheduled time"
                "^(?!.*\\b(Running now|re-anchored)\\b).*\\bjob\\s+'[^']+'\\s+is stuck"
                "overdue.*\\bnot running\\b")
   (fingerprint "oc_cron_no_agent_mismatch" 1 "cron_log"
                "Remove and re-create the job to reset scheduler state."
                "no_agent=True but no script is set"
                "no_agent.*script.*not set")
   (fingerprint "oc_cron_dead_skill_ref" 1 "cron_log"
                "Remove the dead skill reference from the job, or delete the job."
                "skill.*not found"
                "skill directory.*missing"
                "no such file.*skills/")
   (fingerprint "oc_cron_dead_script_ref" 1 "cron_log"
                "Update the script path, or delete the job."
                "script.*not found"
                "no such file.*script"
                "cannot execute.*script")
   (fingerprint "oc_cron_duplicate_function" 1 "ocas_data"
                "Delete the duplicate job, keeping the canonical name and the earliest id."
                "duplicate.*cron"
                "identical.*job")
   (fingerprint "oc_cron_orphaned_job" 1 "ocas_data"
                "Remove the orphaned job, or declare it where it belongs."
                "job.*not declared.*SKILL.md"
                "orphaned.*cron")
   (fingerprint "oc_cron_next_run_at_none" 1 "cron_log"
                "Pause and resume the job so the scheduler recalculates its next run."
                "next_run_at.*None"
                "next_run_at.*null")
   (fingerprint "oc_cron_stale_empty_error" 1 "cron_log"
                "Pause and resume the job to reset the stale state."
                "status.*error.*last_error.*(null|empty)")
   (fingerprint "oc_cron_failure_streak" 1 "cron_log"
                "Pause the job once the streak reaches five, and read its last error."
                "failure_streak.*[3-9]"
                "failure_streak.*1[0-9]")
   (fingerprint "oc_cron_execute_code_in_cron" 1 "cron_log"
                "Replace the sandbox code with a script the scheduler runs unattended."
                "execute_code runs arbitrary local Python.*Cron jobs run without a user present"
                "cron_mode.*deny"
                "approval_pending.*execute_code")
   (fingerprint "oc_cron_google_mcp_down" 1 "cron_log"
                "Restart the gateway so the MCP servers reconnect."
                "google.*workspace.*mcp.*(unavailable|not.*running|not.*registered)"
                "mcp.*server.*(not.*responding|not.*available)"
                "failed to parse JSONRPC message from server")
   (fingerprint "oc_cron_google_auth_expired" 1 "cron_log"
                "Re-authorize the account's OAuth token."
                "invalid_grant"
                "token has been expired or revoked"
                "gmail\\.api.*401")
   (fingerprint "oc_cron_rate_limit_429" 2 "cron_log"
                "Stagger the schedules, or lower the number that run at once."
                "HTTP 429"
                "429.*rate.limit"
                "too many concurrent requests")
   (fingerprint "oc_cron_timeout" 2 "cron_log"
                "Raise the job's ceiling, or cut the job down to less work."
                "\\[cron_[a-f0-9]+_[0-9_]+\\][^\\n]*idle for.*limit.*s"
                "\\[cron_[a-f0-9]+_[0-9_]+\\][^\\n]*TimeoutError"
                "\\[cron_[a-f0-9]+_[0-9_]+\\][^\\n]*upstream idle timeout"
                "\\[cron_[a-f0-9]+_[0-9_]+\\][^\\n]*agent\\.tool_executor[^\\n]*timed out after")
   (fingerprint "oc_cron_response_truncated" 2 "cron_log"
                "Ask for a shorter answer, or split the job in two."
                "Response remained truncated"
                "truncated after.*continuation")
   (fingerprint "oc_background_task_missing" 1 "ocas_data"
                "Register the missing scheduled entry."
                "missing.*cron.*job"
                "background task.*not registered")
   (fingerprint "oc_http_429_concurrent" 1 "cron_log"
                "Stagger the schedules: offset each job's start minute."
                "too many concurrent requests")
   (fingerprint "oc_http_401_nous_api_key" 1 "cron_log"
                "Point the auxiliary provider at a credential that still works."
                "401.*nous"
                "Nous.*credential.*expired")
   ;; --- journals and data directories ------------------------------------------
   (fingerprint "oc_journal_dir_missing" 1 "skill_journal"
                "Create the journal directory."
                "ENOENT.*journals"
                "journal.*directory.*missing"
                "cannot write.*journal")
   (fingerprint "oc_skill_data_dir_missing" 1 "ocas_data"
                "Create the directory and a default config.json in it."
                "ENOENT.*data/ocas-"
                "data directory.*missing"
                "config\\.json.*not found")
   (fingerprint "oc_skill_uninitialized" 1 "ocas_data"
                "Create its storage directory, default config and empty journal files."
                "skill.*uninitialized"
                "missing.*data directory.*config")
   (fingerprint "oc_jsonl_oversized" 1 "ocas_data"
                "Rotate the file with a date suffix."
                "file size exceeded"
                "jsonl.*too large"
                "rotation needed")
   (fingerprint "oc_jsonl_malformed_lines" 1 "ocas_data"
                "Quarantine the malformed lines to an .error file."
                "JSON parse error"
                "malformed.*jsonl"
                "invalid JSON.*line")
   (fingerprint "oc_state_db_oversized" 2 "ocas_data"
                "Vacuum the state database, or archive what it no longer needs."
                "state\\.db.*(oversized|too large|10GB)")
   (fingerprint "oc_disk_full" 3 "gateway_log"
                "Free space on the volume before anything else."
                "database or disk is full"
                "No space left on device")
   ;; --- the gateway and its providers ------------------------------------------
   (fingerprint "oc_gateway_token_missing" 1 "gateway_log"
                "Generate a gateway token."
                "gateway token.*missing"
                "authentication.*gateway.*failed"
                "no gateway token")
   (fingerprint "oc_gateway_process_down" 4 "gateway_log"
                "Start the gateway."
                "gateway.*not running"
                "ECONNREFUSED.*18789")
   (fingerprint "oc_vision_model_incompatible" 1 "gateway_log"
                "Set the vision provider explicitly."
                "vision.*incompatible"
                "vision.*provider.*mismatch")
   (fingerprint "oc_http_429_rate_limit" 2 "cron_log"
                "Wait out the window, then retry, with fewer calls in flight."
                "HTTP 429: Provider returned error"
                "429.*rate-limit"
                "temporarily rate-limited")
   (fingerprint "oc_http_502_provider_unavailable" 2 "errors_log"
                "Retry later, or route to another provider."
                "HTTP 502.*provider_unavailable"
                "error_type.*provider_unavailable")
   (fingerprint "oc_mcp_stdio_parse_error" 2 "errors_log"
                "Make the MCP server print only JSON on stdout."
                "Failed to parse JSONRPC message from server"
                "input_value=.*x1b")
   (fingerprint "oc_cron_null_field_crash" 2 "execute_code"
                "Guard the null fields the job's script formats."
                "TypeError.*NoneType.*format")
   (fingerprint "oc_read_file_too_large" 2 "cron_log"
                "Read the file in pieces."
                "exceeds the safety limit"
                "Read produced.*characters which exceeds")
   (fingerprint "oc_old_path_reference" 2 "cron_log"
                "Point the job at the path the install actually used."
                "usr/local/lib/hermes-agent/"))
  "The fingerprint table: each a plist of :ID, :TIER, :SOURCE, :FIX and
:PATTERNS (cl-ppcre scanners' source, matched case-insensitively).")

;;; --- patterns and the two filters --------------------------------------------------

(defvar *scanners* (make-hash-table :test #'equal)
  "A compiled scanner per pattern, so one scan of one log compiles nothing twice.")

(defun scanner (pattern)
  "PATTERN compiled for one line at a time, case-insensitively; a pattern the
engine cannot read answers NIL and is skipped, as the original skipped it."
  (multiple-value-bind (cached present) (gethash pattern *scanners*)
    (if present
        cached
        (setf (gethash pattern *scanners*)
              (ignore-errors (cl-ppcre:create-scanner
                              pattern :case-insensitive-mode t :multi-line-mode t))))))

(defparameter *echo-markers*
  '("oc_cron_scan" "fingerprint_id" "FINGERPRINT:" "match_patterns" "evidence=" "auto_fix")
  "What a line that reports a finding carries. A scan that read its own report
back would call a repaired issue live on every pass, so a line with one of
these is not evidence.")

(defun echo-line-p (line)
  (let ((low (string-downcase line)))
    (some (lambda (marker) (search (string-downcase marker) low)) *echo-markers*)))

(defun echo-path-p (path)
  "True when PATH is inside the directory this cell writes to."
  (let ((base (string-downcase (namestring (nlk:home "custodian/")))))
    (uiop:string-prefix-p base (string-downcase (uiop:native-namestring path)))))

(defparameter +stamp+ "^([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])[ T]([0-9][0-9]):([0-9][0-9]):([0-9][0-9])"
  "The leading YYYY-MM-DD HH:MM:SS of a log line, which is how a line is dated.")

(defun line-time (line)
  "LINE's own leading timestamp as a universal time, or NIL."
  (multiple-value-bind (match parts) (ignore-errors (cl-ppcre:scan-to-strings
                                                      +stamp+ (string-left-trim '(#\Space #\Tab) line)))
    (declare (ignore match))
    (when (and parts (plusp (length parts)))
      (destructuring-bind (year month day hour minute second)
          (map 'list (lambda (text) (parse-integer text :junk-allowed t)) parts)
        (ignore-errors (encode-universal-time second minute hour day month year 0))))))

(defun recent-lines (text max-age-hours &aux (keep '()) (owner nil) (now (get-universal-time))
                                          (window (and max-age-hours (plusp max-age-hours)
                                                       (* (round max-age-hours) 3600))))
  "TEXT's lines, without the echoes and without what is older than the window.
A continuation line -- a traceback body -- carries no timestamp of its own, so
it inherits the last dated line above it: otherwise an old error's body reads
as live forever. Past the window a line with no dated line above it goes."
  (dolist (line (uiop:split-string text :separator '(#\Newline)))
    (let ((time (line-time line)))
      (when time (setf owner time))
      (unless (echo-line-p line)
        (when (or (null window) (and owner (<= (- now owner) window)))
          (push line keep)))))
  (format nil "~{~a~^~%~}" (nreverse keep)))

(defun first-matching-line (text pattern)
  "The first line of TEXT that matches PATTERN and is not an echo, trimmed;
the evidence a finding carries, which is the line itself, not the top of the file."
  (let ((rx (scanner pattern)))
    (when rx
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (when (and (cl-ppcre:scan rx line) (not (echo-line-p line)))
          (return (string-trim '(#\Space #\Tab #\Return) line)))))))

;;; --- the scan ----------------------------------------------------------------------

(defparameter *evidence-chars* 500
  "How much of the matching line one finding carries.")

(defun scan-text (text &key (fingerprints *fingerprints*) (max-age-hours nil))
  "TEXT scanned once. => a list of findings, each a plist of :ID, :TIER,
:SOURCE, :FIX and :EVIDENCE, in table order."
  (let ((clean (recent-lines text max-age-hours))
        (found '()))
    (when (plusp (length clean))
      (dolist (entry fingerprints)
        (let ((pattern
                (loop for pattern in (getf entry :patterns)
                      for rx = (scanner pattern)
                      when (and rx (cl-ppcre:scan rx clean)) return pattern)))
          (when pattern
            (push (list :id (getf entry :id)
                        :tier (getf entry :tier)
                        :source (getf entry :source)
                        :fix (getf entry :fix)
                        :evidence (let ((line (first-matching-line clean pattern)))
                                    (if (and line (> (length line) *evidence-chars*))
                                        (subseq line 0 *evidence-chars*)
                                        (or line ""))))
                  found)))))
    (nreverse found)))

(defun source-files (source)
  "The files SOURCE names: itself when it names a file, and every .log and
.jsonl it holds when it names a directory."
  (let ((path (probe-file source)))
    (if (and path (uiop:directory-pathname-p path))
        (sort (append (directory (merge-pathnames "*.log" path))
                      (directory (merge-pathnames "*.jsonl" path)))
              #'string< :key #'namestring)
        (list (or path source)))))

(defun read-tail (path tail-lines)
  (let ((text (uiop:read-file-string path :external-format :utf-8)))
    (if (and tail-lines (plusp tail-lines))
        (format nil "~{~a~^~%~}"
                (last (uiop:split-string text :separator '(#\Newline)) tail-lines))
        text)))

(defun scan-files (sources &key (max-age-hours nil) (tail-lines 500))
  "Every file SOURCES name, scanned. => (VALUES FINDINGS SCANNED ERRORS): what
matched, the files read, and what could not be read."
  (let ((findings '()) (scanned '()) (errors '()))
    (dolist (source sources)
      (dolist (path (source-files source))
        (let ((name (uiop:native-namestring path)))
          (cond ((echo-path-p path))
                ((not (probe-file path)) (push (format nil "~a: not found" name) errors))
                (t (handler-case
                       (progn (dolist (finding (scan-text (read-tail path tail-lines)
                                                          :max-age-hours max-age-hours))
                                (pushnew finding findings :test #'equal))
                              (push name scanned))
                     (error (condition)
                       (push (format nil "~a: ~a" name condition) errors))))))))
    (values (nreverse findings) (nreverse scanned) (nreverse errors))))

;;; --- the ledger ----------------------------------------------------------------------

(defun store-path (relative)
  (nlk:home (format nil "custodian/~a" relative)))

(defun ledger-path ()
  (store-path "issues.jsonl"))

(defun now-stamp ()
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

(defun date-stamp ()
  (subseq (now-stamp) 0 10))

(defun json-text-value (object key)
  (nlk:json-value object :string key))

(defun json-number-value (object key)
  (nlk:json-value object :integer key))

(defun read-jsonl (path)
  "PATH's records, a hash table each; a line that does not decode is skipped,
as the original skipped it."
  (let ((records '()))
    (when (probe-file path)
      (handler-case
          (dolist (line (uiop:split-string (uiop:read-file-string path :external-format :utf-8)
                                            :separator '(#\Newline)))
            (let ((text (string-trim '(#\Space #\Tab #\Return) line)))
              (when (plusp (length text))
                (let ((record (ignore-errors (nlk:decode-json text))))
                  (when (hash-table-p record) (push record records))))))
        (error () nil)))
    (nreverse records)))

(defun write-jsonl (path records)
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
    (dolist (record records)
      (write-line (nlk:encode-json-object record) stream))))

(defun issue-id (finding)
  "One issue per fingerprint and matching line: a stable id, so a second scan
bumps the issue it already has instead of filing it again."
  (format nil "~a-~a" (getf finding :id) (subseq (nlk:sha256-text (getf finding :evidence)) 7 13)))

(defun record-findings (findings)
  "File FINDINGS in the ledger, bumping an issue already open. => how many were new."
  (let ((records (read-jsonl (ledger-path)))
        (stamp (now-stamp))
        (new 0))
    (dolist (finding findings)
      (let* ((id (issue-id finding))
             (existing (find id records :key (lambda (record) (json-text-value record "id"))
                                    :test #'equal)))
        (cond (existing
               (setf (gethash "last_seen" existing) stamp
                     (gethash "count" existing) (1+ (or (json-number-value existing "count") 1))))
              (t (incf new)
                 (push (nlk:make-json-object
                        "id" id
                        "fingerprint" (getf finding :id)
                        "tier" (getf finding :tier)
                        "source" (getf finding :source)
                        "evidence" (getf finding :evidence)
                        "status" "open"
                        "first_seen" stamp
                        "last_seen" stamp
                        "count" 1)
                       records)))))
    (write-jsonl (ledger-path) (reverse records))
    new))

(defun open-issues ()
  (remove-if (lambda (record) (equal "resolved" (json-text-value record "status")))
             (read-jsonl (ledger-path))))

(defun resolve-issue (id)
  "Mark issue ID resolved. => (VALUES TEXT RESOLVED-P)"
  (let* ((records (read-jsonl (ledger-path)))
         (found (find id records :key (lambda (record) (json-text-value record "id"))
                             :test #'equal)))
    (if found
        (progn (setf (gethash "status" found) "resolved"
                     (gethash "resolved_at" found) (now-stamp))
               (write-jsonl (ledger-path) records)
               (values (format nil "custodian: ~a is resolved." id) t))
        (values (format nil "custodian: no issue ~a in the ledger; `issues list' names the open ones." id) nil))))

(defun journal-path ()
  (store-path (format nil "journal/~a/run-~a.json"
                      (date-stamp)
                      (substitute #\- #\: (subseq (now-stamp) 11)))))

(defun finding-object (finding)
  (nlk:make-json-object "id" (getf finding :id)
                         "tier" (getf finding :tier)
                         "source" (getf finding :source)
                         "evidence" (getf finding :evidence)
                         "fix" (getf finding :fix)))

(defun write-journal (findings scanned errors)
  "One journal file per scan, under this home's custodian/journal/DAY/. => its path."
  (let ((path (journal-path)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object
                     (nlk:make-json-object
                      "run_id" (pathname-name path)
                      "created_at" (now-stamp)
                      "fingerprints_tracked" (length *fingerprints*)
                      "finding_count" (length findings)
                      "sources" (coerce scanned 'vector)
                      "errors" (coerce errors 'vector)
                      "findings" (coerce (mapcar #'finding-object findings) 'vector)))
                    stream))
    path))

(defun last-scan ()
  "The created_at of the newest journal file, or \"never\"."
  (let ((files (ignore-errors
                 (sort (loop for day in (directory (nlk:home "custodian/journal/*/"))
                             nconc (directory (merge-pathnames "*.json" day)))
                       #'string> :key #'namestring))))
    (or (and files
             (ignore-errors (json-text-value (nlk:decode-json (uiop:read-file-string (first files)))
                                              "created_at")))
        "never")))

;;; --- what a scan answers --------------------------------------------------------------

(defun finding-line (finding)
  (format nil "~a (tier ~d, ~a)~%  ~a~@[~%  fix: ~a~]"
          (getf finding :id) (getf finding :tier) (getf finding :source)
          (getf finding :evidence) (getf finding :fix)))

(defun scan-report (findings scanned errors fresh)
  (with-output-to-string (out)
    (format out "custodian: ~d finding~:p in ~d source~:p scanned~@[, ~d new in the ledger~].~%"
            (length findings) (length scanned) (and (plusp fresh) fresh))
    (if findings
        (dolist (finding findings) (write-string (finding-line finding) out) (terpri out))
        (format out "Nothing matched the ~d known fingerprints.~%" (length *fingerprints*)))
    (dolist (error errors) (format out "unreadable: ~a~%" error))
    (format out "ledger: ~a" (namestring (ledger-path)))))

(defun run-scan (&key max-age-hours tail-lines)
  "Scan every configured source and file what matched.
=> (VALUES FINDINGS SCANNED ERRORS FRESH JOURNAL)"
  (multiple-value-bind (findings scanned errors)
      (scan-files (coerce (setting :sources) 'list)
                  :max-age-hours (or max-age-hours (setting :max-age-hours))
                  :tail-lines (or tail-lines (setting :tail-lines)))
    (let ((fresh (record-findings findings))
          (journal (write-journal findings scanned errors)))
      (values findings scanned errors fresh journal))))

;;; --- the readings ---------------------------------------------------------------------

(defun tier-counts ()
  (let ((counts (make-hash-table)))
    (dolist (entry *fingerprints*)
      (incf (gethash (getf entry :tier) counts 0)))
    counts))

(defun status ()
  "custodian_status: what this cell watches, what it knows, and what is open."
  (with-output-to-string (out)
    (let ((counts (tier-counts))
          (issues (open-issues)))
      (format out "custodian: ~d fingerprints tracked"
              (length *fingerprints*))
      (loop for tier in (sort (loop for tier being the hash-keys of counts collect tier) #'<)
            do (format out "~@[~a~]~a tier ~d" ", " (gethash tier counts) tier))
      (format out ".~%open issues: ~d~%last scan: ~a~%sources: ~{~a~^, ~}~%ledger: ~a~%"
              (length issues) (last-scan)
              (or (coerce (setting :sources) 'list) '("none configured"))
              (namestring (ledger-path))))))

(defun issues-text (&key (action "list") issue-id)
  "custodian_issues: the ledger, listed, counted, or with one issue resolved."
  (cond
    ((equal action "summary")
     (let ((records (read-jsonl (ledger-path)))
           (by-tier (make-hash-table))
           (open 0) (resolved 0))
       (dolist (record records)
         (if (equal "resolved" (json-text-value record "status"))
             (incf resolved)
             (incf open))
         (incf (gethash (or (json-number-value record "tier") 0) by-tier 0)))
       (with-output-to-string (out)
         (format out "custodian ledger: ~d open, ~d resolved.~%" open resolved)
         (loop for tier being the hash-keys of by-tier using (hash-value count)
               do (format out "  tier ~d: ~d~%" tier count)))))
    ((equal action "resolve")
     (if (or (null issue-id) (zerop (length issue-id)))
         "custodian: resolve takes an issue id; `issues list' names the open ones."
         (resolve-issue issue-id)))
    (t
     (let ((issues (open-issues)))
       (with-output-to-string (out)
         (format out "custodian ledger: ~d open issue~:p.~%" (length issues))
         (dolist (record issues)
           (format out "~a  ~a (tier ~d, ~a) x~d  last seen ~a~%  ~a~%"
                   (json-text-value record "id")
                   (json-text-value record "fingerprint")
                   (or (json-number-value record "tier") 0)
                   (json-text-value record "source")
                   (or (json-number-value record "count") 1)
                   (json-text-value record "last_seen")
                   (json-text-value record "evidence"))))))))

;;; --- the tools ------------------------------------------------------------------------

(nlk:define-contract custodian-scan
  (:doc "Scan the log files this cell watches for known failure patterns, file
what matched in the issue ledger, and answer each finding with the line that
matched and the fix known for it. Run it when something went wrong and the log
would say what, and after a fix to see the finding go.")
  ("max_age_hours" :integer
                   :doc "report only matches newer than this many hours; absent or 0 uses the section's own window")
  ("tail_lines" :integer
                :doc "how many closing lines of each file to read; absent or 0 uses the section's own"))

(nlk:define-contract custodian-issues
  (:doc "The issue ledger: the open issues with their evidence, a count by
state and tier, or one issue marked resolved.")
  ("action" :text :default "list" :options '("list" "summary" "resolve")
            :doc "list the open issues, summarize the ledger, or resolve one")
  ("issue_id" :text :default ""
              :doc "the issue to resolve, as `list` printed it"))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object))))

(defun scan-tool (call)
  (let ((age (custodian-scan-max-age-hours call))
        (tail (custodian-scan-tail-lines call)))
    (multiple-value-bind (findings scanned errors fresh journal)
        (run-scan :max-age-hours (and age (plusp age) age)
                  :tail-lines (and tail (plusp tail) tail))
      (declare (ignore journal))
      (scan-report findings scanned errors fresh))))

(defun issues-tool (call)
  (issues-text :action (custodian-issues-action call)
               :issue-id (custodian-issues-issue-id call)))

(defun status-tool (arguments)
  (declare (ignore arguments))
  (status))

;;; --- /custodian ---------------------------------------------------------------------

(defparameter +primer+
  "Custodian watches the log files this home names for failures it knows by
shape -- 35 fingerprints taken from real ones -- and keeps them in an issue
ledger. Each finding names the pattern, the tier (1 routine, 2 needs a
diagnosis, 3 escalation, 4 a dead process), the log line that matched, and the
fix that worked last time.
  custodian_scan    read every watched file and answer what matched. Do this
                    when something failed and the log would say why, and again
                    after a fix. A match older than max_age_hours is not
                    reported, and a line that merely quotes a pattern (a report
                    about a finding) is not a finding.
  custodian_issues  the ledger: `list` the open issues, `summary` the counts,
                    or `resolve` one by id once it is actually fixed.
  custodian_status  what is watched, how many fingerprints there are, and when
                    the last scan ran.
A finding is evidence, not a diagnosis: read the line, decide whether the fix
applies, apply it, then resolve the issue and scan again. Nothing repairs
anything by itself, and nothing leaves this machine.
The operator reads the same with /custodian status, /custodian scan, and
/custodian issues. ~a"
  "What (help :custodian) answers while the cell runs.")

(defun run-command (args session-id)
  "/custodian: status, scan, issues list|summary|resolve ID, or help."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (parts (and (plusp (length text)) (uiop:split-string text :separator '(#\Space))))
         (word (and parts (string-downcase (first parts)))))
    (cond ((null word) (status))
          ((member word '("status" "show") :test #'string=) (status))
          ((string= word "scan")
           (multiple-value-bind (findings scanned errors fresh journal)
               (run-scan)
             (declare (ignore journal))
             (scan-report findings scanned errors fresh)))
          ((string= word "issues")
           (let ((rest (rest parts)))
             (cond ((null rest) (issues-text :action "list"))
                   ((string= (string-downcase (first rest)) "summary") (issues-text :action "summary"))
                   ((string= (string-downcase (first rest)) "resolve")
                    (issues-text :action "resolve" :issue-id (or (second rest) "")))
                   (t (issues-text :action "list")))))
          ((string= word "help")
           (format nil "~a~%~%~a"
                   "/custodian status | scan | issues list | issues summary | issues resolve <id> | help" +primer+))
          (t (format nil "custodian takes status, scan, issues list|summary|resolve <id>, or help; it read ~s" text)))))

;;; --- the declaration ------------------------------------------------------------------

(nle:define-cell custodian
  (:section ("custodian")
    (:guide "list the log files to watch in sources -- one path each, or a directory and every .log and .jsonl in it is read; nothing else is needed; a scan writes its findings to a ledger under the home")
    ("sources" :list
     :doc "log files or directories to scan for known failure patterns; a directory is read as every .log and .jsonl in it")
    ("max_age_hours" :integer :default 24 :min 1
     :doc "a match older than this many hours is not reported; a line with no timestamp of its own is judged by the last dated line above it")
    ("tail_lines" :integer :default 500 :min 0
     :doc "how many closing lines of each file a scan reads; 0 reads the whole file"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (unless (getf values :sources)
                 (nlk:config-error
                  "custodian.sources is empty: name at least one log file or directory to watch, in the custodian section of the config (or /settings), then (restart-cells \"nodecode-custodian\")"))
               values))
  (:help :custodian
         "custodian: known-failure fingerprints over the logs you watch - (help :custodian)"
         (format nil +primer+
                 "Nothing leaves this machine: a scan reads the files you named and writes only under the home's custodian/ directory."))
  (:tool "custodian_scan" (nlk:contract 'custodian-scan) #'scan-tool)
  (:tool "custodian_issues" (nlk:contract 'custodian-issues) #'issues-tool)
  (:tool "custodian_status"
         (no-arguments "What Custodian watches, how many fingerprints it knows, the open issues and the last scan.")
         #'status-tool)
  (:command "custodian" 'run-command
            :catches 'custodian-error
            :description "Custodian: scan the watched logs, and read the issue ledger"
            :argument-hint "status|scan|issues"))
