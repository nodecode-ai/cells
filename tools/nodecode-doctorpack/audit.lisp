;;;; audit.lisp --- redaction, the findings, the home's evidence, the report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One design rule, kept from the original: never turn missing evidence into
;;;; a hard failure. A finding is ok, warn, fail or unknown, with a severity, a
;;;; confidence and the evidence it was read from; `unknown' means the audit
;;;; lacked evidence, never that the home is broken. Every string that leaves
;;;; this file -- a returned finding, a config value, a log line, a report
;;;; body -- has passed REDACT so a key pasted into config.jsonc or an address
;;;; in a log line does not ride back into the model's context.

(in-package #:nodecode-doctorpack)

;;; --- the records ---------------------------------------------------------------

(defstruct evidence source message timestamp confidence)

(defstruct finding id title status severity confidence summary
  (evidence '()) (recommendation ""))

;;; --- redaction -----------------------------------------------------------------

(defparameter *ansi*
  (format nil "~c\\[[0-9;]*[A-Za-z]" #\Escape)
  "An ANSI colour sequence, which a piped log keeps and a report should not.")

(defparameter *secret-rules*
  (list
   ;; a bearer or basic credential first, so the scheme is not taken for the
   ;; value the assignment rule masks
   (cons "(\\b(?:bearer|basic)\\s+)[^\\s'\",;{}]+" "\\1[REDACTED]")
   ;; a key, token, secret, password or authorization assigned a value: the
   ;; value goes, the key stays, so the finding still says which one it was
   (cons "((?:api[_-]?key|token|secret|password|passwd|pwd|authorization)[\"']?[ \t]*[:=][ \t]*)(\"[^\"\\r\\n]*\"|'[^'\\r\\n]*'|[^\\s'\",;{}]+)"
         "\\1[REDACTED]")
   ;; token shapes that name their own issuer
   (cons "(github_pat_[A-Za-z0-9_]{20,}|sk-[A-Za-z0-9_-]{20,}|xox[baprs]-[A-Za-z0-9-]{20,}|gh[pousr]_[A-Za-z0-9_]{20,}|AIza[0-9A-Za-z_-]{20,})"
         "[REDACTED]")
   ;; credentials carried in a URL's userinfo
   (cons "(https?://)[^\\s/@:]+:[^\\s/@]+@" "\\1[REDACTED]@"))
  "Each rule: a scanner pattern (built case-insensitively) and its replacement.")

(defparameter *email* "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
  "An address, how one is written in a config or a log line.")

(defparameter *phone* "(?<![\\w:])(?:\\+?\\d{1,3}[- .])?(?:\\(?\\d{3}\\)?[- .]\\d{3}[- .]\\d{4})(?!\\w)"
  "A phone shape, deliberately not so wide that it eats an ISO timestamp.")

(defparameter *secret-key* "(?i)(key|token|secret|password|passwd|pwd|authorization|webhook)"
  "A config member whose value is a credential whatever it looks like.")

(defun rule-scan (pattern)
  (cl-ppcre:create-scanner pattern :case-insensitive-mode t))

(defun redact (text)
  "TEXT with secrets and direct identifiers masked. A string always, so what
reaches a finding is text even when the input was not."
  (let ((out (cl-ppcre:regex-replace-all (rule-scan *ansi*) (princ-to-string (or text "")) "")))
    (loop for (pattern . replacement) in *secret-rules*
          for scanner = (rule-scan pattern)
          do (setf out (cl-ppcre:regex-replace-all scanner out replacement)))
    (setf out (cl-ppcre:regex-replace-all (rule-scan *email*) out "[REDACTED_EMAIL]"))
    (setf out (cl-ppcre:regex-replace-all (rule-scan *phone*) out "[REDACTED_PHONE]"))
    out))

(defun secret-key-p (key)
  (and (stringp key) (cl-ppcre:scan (rule-scan *secret-key*) key) t))

(defun redact-object (object)
  "OBJECT with every string REDACTed and every credential-named member masked,
whether it is a hash object, an array, a string or a plain value."
  (cond ((hash-table-p object)
         (let ((out (make-hash-table :test 'equal)))
           (maphash (lambda (key value)
                      (setf (gethash (if (stringp key) (redact key) key) out)
                            (if (secret-key-p key) "[REDACTED]" (redact-object value))))
                    object)
           out))
        ((stringp object) (redact object))
        ((vectorp object) (map 'vector #'redact-object object))
        ((consp object) (mapcar #'redact-object object))
        (t object)))

;;; --- the home and its files ----------------------------------------------------

(defun home-directory (&optional explicit)
  "The Nodecode home a call inspects: EXPLICIT when one was given, else the
`doctorpack.home' setting, else the home this organism is running in."
  (let* ((text (or (and (stringp explicit) (plusp (length (nlk:trimmed explicit)))
                        (nlk:trimmed explicit))
                   (setting :home)))
         (path (if text (nlk:expand-home text) (nlk:home))))
    (uiop:ensure-directory-pathname path)))

(defun home-relative (path home)
  "PATH as the home names it, so a finding says `logs/agent.log' and not a
whole filesystem path."
  (let ((path (uiop:native-namestring path))
        (home (uiop:native-namestring home)))
    (if (uiop:string-prefix-p home path)
        (subseq path (length home))
        path)))

(defun config-file (home)
  (merge-pathnames "config.jsonc" home))

(defparameter *log-names*
  '("logs/agent.log" "logs/gateway.log" "logs/errors.log"
    "agent.log" "gateway.log" "errors.log")
  "The log files a home may keep, by its own naming.")

(defun log-files (home)
  "Every log file the home keeps: the named ones, then any other *.log at its
root or under its logs/ folder."
  (remove-duplicates
   (remove nil
           (append (loop for name in *log-names*
                         for path = (merge-pathnames name home)
                         when (probe-file path) collect path)
                   (ignore-errors (uiop:directory-files home "*.log"))
                   (ignore-errors (uiop:directory-files (merge-pathnames "logs/" home) "*.log"))))
   :test #'equal))

(defun file-tail-lines (path &key (limit 256000) (lines 500))
  "PATH's text, at most LIMIT bytes of it, as its last LINES lines."
  (let ((text (nlk:read-text path limit)))
    (when text
      (let ((all (nlk:lines text)))
        (if (> (length all) lines) (last all lines) all)))))

(defparameter *stamp* "(\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}:\\d{2})"
  "The ISO-8601 stamp a log line may open with.")

(defun line-stamp-text (line)
  "LINE's ISO stamp as it is written, or NIL when it carries none."
  ;; SCAN and a SUBSEQ, not SCAN-TO-STRINGS: the answer is the text the log
  ;; wrote, which is also what a reader wants in the report.
  (multiple-value-bind (start end) (cl-ppcre:scan *stamp* line)
    (when start
      (subseq line start end))))

(defun line-stamp (line)
  "LINE's ISO timestamp, read as UTC, or NIL when it carries none."
  (let ((text (line-stamp-text line)))
    (and text (nlk:iso-universal text))))

(defun recent-line-p (line cutoff)
  "Whether LINE is evidence inside the lookback. A line with no stamp is
recent: a tail's untimed lines are weak evidence, but still evidence."
  (let ((stamp (line-stamp line)))
    (or (null stamp) (>= stamp cutoff))))

(defparameter *error-words* '("error" "exception" "traceback" "failed" "failure"
                              "unauthorized" "forbidden"))
(defparameter *warn-words* '("warning" "warn" "deprecated" "retry" "rate limit" "timeout"))
(defparameter *cell-words* '("cell" "cells" "asdf" "load" "register"))

(defun words-present-p (line words)
  (let ((line (string-downcase line)))
    (some (lambda (word) (search word line)) words)))

(defun scan-logs (home &key (lookback-hours 24) (focus '()))
  "The recent warning and error lines of HOME's logs, redacted, oldest first
within each file: triage evidence, never a root cause by itself."
  (let ((cutoff (- (get-universal-time) (* 3600 (max 1 lookback-hours))))
        (found '()))
    (dolist (path (log-files home))
      (dolist (line (file-tail-lines path))
        (when (and (or (null focus) (words-present-p line focus))
                   (recent-line-p line cutoff)
                   (words-present-p line (append *error-words* *warn-words*)))
          (let ((stamp (line-stamp line)))
            (push (make-evidence :source (home-relative path home)
                                 :message (nlk:clip (redact line) 600)
                                 :timestamp (line-stamp-text line)
                                 :confidence (if stamp "high" "medium"))
                  found)))))
    (subseq (nreverse found) 0 (min 50 (length found)))))

;;; --- the findings ---------------------------------------------------------------

(defun finding (id title status severity confidence summary
                &key evidence recommendation)
  (make-finding :id id :title title :status status :severity severity
                :confidence confidence :summary summary
                :evidence evidence :recommendation (or recommendation "")))

(defun status-counts (findings)
  (let ((counts (nlk:json-object "ok" 0 "warn" 0 "fail" 0 "unknown" 0)))
    (dolist (f findings)
      (setf (gethash (finding-status f) counts)
            (1+ (gethash (finding-status f) counts 0))))
    counts))

(defun overall-status (findings)
  "The one word the whole audit answers, as the original ranked it: a fail
anywhere outranks a warn, and `unknown' alone is not `ok'."
  (let ((counts (status-counts findings)))
    (cond ((plusp (gethash "fail" counts)) "fail")
          ((plusp (gethash "warn" counts)) "warn")
          ((and (plusp (gethash "unknown" counts)) (zerop (gethash "ok" counts))) "unknown")
          (t "ok"))))

(defun evidence-json (e)
  (nlk:json-object "source" (evidence-source e)
                    "message" (evidence-message e)
                    "timestamp" (evidence-timestamp e)
                    "confidence" (evidence-confidence e)))

(defun finding-json (f)
  (nlk:json-object "id" (finding-id f)
                    "title" (finding-title f)
                    "status" (finding-status f)
                    "severity" (finding-severity f)
                    "confidence" (finding-confidence f)
                    "summary" (finding-summary f)
                    "evidence" (coerce (mapcar #'evidence-json (finding-evidence f)) 'vector)
                    "recommendation" (finding-recommendation f)))

(defun result (kind home findings &key (extra (nlk:json-object)))
  "The payload every tool answers, redacted whole: what the audit found, how
the counts fell out, and the promises it keeps."
  (let ((payload (nlk:json-object
                  "success" t
                  "kind" kind
                  "status" (overall-status findings)
                  "generated_at" (nlk:iso-now)
                  "host" (nlk:json-object "system" (format nil "~a ~a" (software-type) (software-version))
                                           "lisp" (lisp-implementation-type))
                  "nodecode_home" (uiop:native-namestring home)
                  "finding_counts" (status-counts findings)
                  "findings" (coerce (mapcar #'finding-json findings) 'vector)
                  "guarantees" (coerce '("local-first: no network calls are made"
                                         "secrets and direct identifiers are redacted before evidence is returned"
                                         "missing evidence is reported as unknown, not fail"
                                         "log matches are triage evidence, not root-cause claims")
                                       'vector))))
    (maphash (lambda (key value) (setf (gethash key payload) value)) extra)
    (redact-object payload)))

(defun json-text (payload)
  "PAYLOAD as the indented JSON text a tool answers."
  (nlk:pretty-json payload))

;;; --- config.jsonc ---------------------------------------------------------------

(defparameter *section-key* "^[ \\t]+\"([A-Za-z0-9_-]+)\"[ \\t]*:"
  "A top-level member of config.jsonc, which sits one indent in.")

(defparameter *inline-secret*
  "((?:api[_-]?key|token|secret|password|passwd|client_secret|webhook)[\"']?[ \\t]*:[ \\t]*\")([^\"\\r\\n]{8,})\""
  "A credential pasted inline into the config, rather than named by reference.")

(defun config-sections (text)
  "The top-level members TEXT names, in the order they are written."
  (let ((names '()))
    (dolist (line (nlk:lines text))
      (multiple-value-bind (whole groups) (cl-ppcre:scan-to-strings *section-key* line)
        (declare (ignore whole))
        (when groups (push (aref groups 0) names))))
    (nreverse names)))

(defun logs-finding (home lookback)
  "The one finding the log tail makes: the evidence it holds, or the word
for having none. Reached with or without a config, as the original did."
  (let ((evidence (scan-logs home :lookback-hours lookback))
        (logs (log-files home)))
    (if evidence
        (finding "logs.recent_issues" "recent warning/error log evidence" "warn" "medium" "medium"
                 (format nil "Found ~d recent redacted warning/error line~:p in the home's logs. This is evidence, not a root cause by itself."
                         (length evidence))
                 :evidence (subseq evidence 0 (min 20 (length evidence)))
                 :recommendation "read the timestamps and the lines around them before naming a cause")
        (finding "logs.recent_issues"
                 (if logs "no recent warning/error evidence found" "logs unavailable")
                 (if logs "ok" "unknown") "info" "medium"
                 (if logs
                     "No recent warning or error line matched in the inspected log tail."
                     "No log file was found in the home, so runtime health cannot be read from logs.")))))

(defun config-audit (&optional home (lookback-hours nil))
  "Audit HOME's config.jsonc and, when the section says so, its recent logs,
over LOOKBACK-HOURS or the section's own lookback."
  (let* ((home (or home (home-directory)))
         (lookback (or lookback-hours (setting :lookback-hours) 24))
         (path (config-file home))
         (text (nlk:read-text path 512000))
         (findings '()))
    (flet ((add (f) (push f findings)))
      (if text
          (let ((sections (config-sections text)))
            (add (finding "config.present" "config.jsonc present" "ok" "info" "high"
                          "Found the home's config.jsonc."
                          :evidence (list (make-evidence :source (home-relative path home)
                                                          :message "config file exists"
                                                          :confidence "high"))))
            (add (finding "config.sections" "config sections named" "ok" "info" "high"
                          (if sections
                              (format nil "The config names ~d top-level member~:p, values redacted."
                                      (length sections))
                              "The config names no top-level member.")
                          :evidence (list (make-evidence :source (home-relative path home)
                                                          :message (nlk:clip (format nil "~{~a~^, ~}" sections) 500)
                                                          :confidence "high"))))
            (let ((match (cl-ppcre:scan-to-strings (rule-scan *inline-secret*) text)))
              (if match
                  (add (finding "config.inline_secret" "a credential is pasted into the config" "warn" "medium" "high"
                                "config.jsonc holds a credential-shaped value written inline."
                                :evidence (list (make-evidence :source (home-relative path home)
                                                                :message (format nil "~a[REDACTED]\""
                                                                                 (aref match 0))
                                                                :confidence "high"))
                                :recommendation "name the environment variable or a file under the home's secrets folder instead of pasting the value"))
                  (add (finding "config.inline_secret" "no inline credential found" "ok" "info" "medium"
                                "No credential-shaped value was found written inline in the config."))))
            (when (setting :include-logs) (add (logs-finding home lookback))))
          (add (finding "config.present" "config.jsonc missing" "unknown" "medium" "high"
                        "No config.jsonc was found at the inspected home; config-specific risks cannot be audited."
                        :evidence (list (make-evidence :source "config.jsonc"
                                                        :message "file not found"
                                                        :confidence "high"))
                        :recommendation "check doctorpack.home, or pass a home to the call")))
      (when (setting :include-logs) (add (logs-finding home lookback)))
      (result "config_audit" home (nreverse findings)))))

;;; --- cells ----------------------------------------------------------------------

(defun cell-folders (home)
  "The folders under HOME's cells/ directory whose name is not dotted."
  (let ((root (merge-pathnames "cells/" home)))
    (remove-if (lambda (path) (char= #\. (char (car (last (pathname-directory path))) 0)))
               (ignore-errors (uiop:subdirectories root)))))

(defun cell-manifest (folder)
  "FOLDER's .asd, or NIL when the folder carries none: a cell whose presence
loads nothing."
  (ignore-errors (first (uiop:directory-files folder "*.asd"))))

(defun cell-check (&optional home (target "") (lookback-hours nil))
  "Inspect HOME's cells folder and the recent cell-related log evidence, and
answer for TARGET by name when one was given."
  (let* ((home (or home (home-directory)))
         (lookback (or lookback-hours (setting :lookback-hours) 24))
         (target (string-downcase (nlk:trimmed (or target ""))))
         (folders (cell-folders home))
         (named (remove nil (mapcar (lambda (folder)
                                      (let ((name (car (last (pathname-directory folder)))))
                                        (and (stringp name) name)))
                                    folders)))
         (broken (remove-if #'cell-manifest folders))
         (findings '()))
    (flet ((add (f) (push f findings)))
      (if named
          (add (finding "cells.discovered" "cell folders discovered" "ok" "info" "high"
                        (format nil "Found ~d cell folder~:p under cells/." (length named))
                        :evidence (list (make-evidence :source "cells/"
                                                        :message (nlk:clip (format nil "~{~a~^, ~}" (sort (copy-list named) #'string<)) 500)
                                                        :confidence "high"))))
          (add (finding "cells.discovered" "no cell folder discovered" "unknown" "info" "high"
                        "No folder was found under cells/, so nothing of this home is added to the shipped cells.")))
      (when broken
        (add (finding "cells.no_manifest" "a cell folder has no .asd" "warn" "medium" "high"
                      "A folder under cells/ carries no .asd, so its presence loads nothing."
                      :evidence (mapcar (lambda (folder)
                                          (make-evidence :source (home-relative folder home)
                                                          :message ".asd not found"
                                                          :confidence "high"))
                                        (subseq broken 0 (min 5 (length broken))))
                      :recommendation "give the folder its .asd, or take the folder out of cells/")))
      (when (plusp (length target))
        (let* ((folder (merge-pathnames (format nil "~a/" target) (merge-pathnames "cells/" home)))
               (installed (member target named :test #'string=)))
          (cond ((and installed (cell-manifest folder))
                 (add (finding "cells.target_installed" (format nil "~a is installed" target) "ok" "info" "high"
                               (format nil "~a sits under cells/ and carries an .asd." target))))
                (installed
                 (add (finding "cells.target_unloadable" (format nil "~a is installed but carries no .asd" target) "warn" "medium" "high"
                               (format nil "~a sits under cells/ but nothing in the folder names a system, so it loads nothing." target)
                               :recommendation "put the folder's .asd at its top, named after the folder")))
                (t
                 (add (finding "cells.target_not_found" (format nil "~a not found" target) "unknown" "medium" "medium"
                               (format nil "~a is not a folder under cells/. It may be a shipped cell or one of another profile." target)
                               :recommendation "read (cells) for every folder this home loads"))))))
      (let ((evidence (scan-logs home :lookback-hours lookback :focus *cell-words*)))
        (if evidence
            (add (finding "cells.log_evidence" "recent cell log warnings/errors" "warn" "medium" "medium"
                          (format nil "Found ~d recent cell-related warning/error line~:p."
                                  (length evidence))
                          :evidence (subseq evidence 0 (min 20 (length evidence)))
                          :recommendation "read the lines around these before naming a cause"))
            (add (finding "cells.log_evidence" "no recent cell warning/error evidence"
                          (if (log-files home) "ok" "unknown") "info" "medium"
                          "No recent cell-related warning or error line matched."))))
      (result "cell_check" home (nreverse findings)
              :extra (nlk:json-object "cells" (coerce (sort (copy-list named) #'string<) 'vector)
                                       "unloadable" (coerce (mapcar (lambda (f) (car (last (pathname-directory f)))) broken) 'vector))))))

;;; --- the postmortem -------------------------------------------------------------

(defun slug (text)
  "TEXT as a filename: lowercased, every run outside letters, digits, `_',
`.' and `-' folded to one `-', at most 60 characters."
  (let* ((slug (string-downcase (redact (or text "incident"))))
         (slug (cl-ppcre:regex-replace-all "[^A-Za-z0-9_.-]+" slug "-")))
    (or (string-trim "-" (subseq slug 0 (min 60 (length slug)))) "incident")))

(defun report-body (title home lookback findings evidence)
  "The markdown of one postmortem: the summary, each finding with its
evidence, and the appendix every snippet was drawn from."
  (let* ((counts (status-counts findings))
         (lines (list (format nil "# ~a" (redact title)) ""
                     (format nil "Generated: ~a" (nlk:iso-now))
                     "Nodecode home: `[REDACTED_HOME]`"
                     (format nil "Lookback: ~dh" lookback)
                     ""
                     "## Executive summary" ""
                     (format nil "Findings: fail=~d, warn=~d, unknown=~d, ok=~d."
                             (gethash "fail" counts) (gethash "warn" counts)
                             (gethash "unknown" counts) (gethash "ok" counts))
                     ""
                     "This report is evidence-backed. `unknown` means Doctorpack lacked evidence; it is not treated as healthy or broken."
                     ""
                     "## Findings" "")))
    (dolist (f findings)
      (setf lines (append lines
                           (list (format nil "### ~a --- ~a" (string-upcase (finding-status f)) (redact (finding-title f)))
                                 ""
                                 (format nil "- Severity: ~a" (finding-severity f))
                                 (format nil "- Confidence: ~a" (finding-confidence f))
                                 (format nil "- Summary: ~a" (redact (finding-summary f))))
                           (when (plusp (length (finding-recommendation f)))
                             (list (format nil "- Recommendation: ~a" (redact (finding-recommendation f)))))
                           (list "")))
      (when (finding-evidence f)
        (setf lines (append lines (list "Evidence:")))
        (dolist (e (subseq (finding-evidence f) 0 (min 5 (length (finding-evidence f)))))
          (setf lines (append lines
                               (list (format nil "- `~a`~@[ (~a)~]: ~a"
                                             (redact (evidence-source e))
                                             (evidence-timestamp e)
                                             (redact (evidence-message e))))))))
      (setf lines (append lines (list ""))))
    (setf lines (append lines (list "## Evidence appendix" "")))
    (if (null evidence)
        (setf lines (append lines (list "No log or config evidence snippet was available.")))
        (dolist (e evidence)
          (setf lines (append lines
                               (list (format nil "- `~a`~@[ (~a)~]: ~a"
                                             (redact (evidence-source e))
                                             (evidence-timestamp e)
                                             (redact (evidence-message e))))))))
    (setf lines (append lines
                         (list ""
                               "## What this rewrites"
                               ""
                               "- The original audits a Hermes home; this one reads the Nodecode home: config.jsonc, cells/ and the home's logs."
                               "- Findings are conservative, statused and evidence-backed; a missing file is `unknown', never `fail'."
                               "- Everything written here has passed the same redaction the tools answer with.")))
    (format nil "~{~a~%~}" lines)))

(defun postmortem (&optional home (title "") (max-evidence nil) (lookback-hours nil))
  "Write one redacted postmortem under HOME/doctorpack/reports/ and answer the
payload, with the report file named."
  (let* ((home (or home (home-directory)))
         (lookback (or lookback-hours (setting :lookback-hours) 24))
         (max-evidence (or max-evidence (setting :max-evidence) 20))
         (title (let ((title (nlk:trimmed (or title ""))))
                  (if (plusp (length title)) (redact title) "Nodecode diagnostic incident")))
         (audit (config-audit home lookback))
         (cells (cell-check home "" lookback))
         (findings (append (findings-of audit) (findings-of cells)))
         (evidence (subseq (loop for f in findings append (finding-evidence f))
                           0 (min max-evidence (length (loop for f in findings append (finding-evidence f))))))
         (directory (merge-pathnames "doctorpack/reports/" home))
         (stem (format nil "~a-~a" (cl-ppcre:regex-replace-all "[:-]" (nlk:iso-now) "") (slug title))))
    (ensure-directories-exist (merge-pathnames "keep" directory))
    (let ((path (loop for suffix = nil then (if suffix (1+ suffix) 2)
                       for name = (if suffix (format nil "~a-~d.md" stem suffix) (format nil "~a.md" stem))
                       for path = (merge-pathnames name directory)
                       unless (probe-file path)
                         do (nlk:write-file-atomically path (report-body title home lookback findings evidence))
                            (return path))))
      (let ((payload (result "postmortem" home findings
                             :extra (nlk:json-object
                                     "report_path" (uiop:native-namestring path)
                                     "report_filename" (file-namestring path)
                                     "evidence_count" (length evidence)
                                     "message" "Redacted postmortem report written."))))
        (setf (gethash "report_path_redacted" payload)
              (not (equal (gethash "report_path" payload) (uiop:native-namestring path))))
        payload))))

(defun findings-of (payload)
  "The findings a tool's payload carries, back as records, so the postmortem
folds the two audits into one report."
  (loop for f across (nlk:json-value payload :array "findings")
        collect (make-finding
                 :id (nlk:json-value f :string "id")
                 :title (nlk:json-value f :string "title")
                 :status (nlk:json-value f :string "status")
                 :severity (nlk:json-value f :string "severity")
                 :confidence (nlk:json-value f :string "confidence")
                 :summary (nlk:json-value f :string "summary")
                 :recommendation (or (nlk:json-value f :string "recommendation") "")
                 :evidence (loop for e across (nlk:json-array f "evidence")
                                 collect (make-evidence
                                          :source (nlk:json-value e :string "source")
                                          :message (nlk:json-value e :string "message")
                                          :timestamp (nlk:json-value e :string "timestamp")
                                          :confidence (nlk:json-value e :string "confidence"))))))
