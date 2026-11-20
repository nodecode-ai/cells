;;;; engine.lisp --- the pinned scanner as a process, and its report as values.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The engine is archivegenocide's own scanner, vendored unmodified at a
;;;; pinned commit under vendor/ (see vendor/VENDOR.md). This file runs it and
;;;; reads what it wrote; it re-implements none of its checks. The seam is
;;;; *RUN*, so a test drives the ladder without a Python on the machine, and
;;;; every value here comes out of the report the tool itself wrote -- a gap
;;;; stays a gap, and a clean result is never widened into a verdict.

(in-package #:nodecode-zero-breach)

(defparameter *engine-subdirectory* "vendor/scan/"
  "Where the vendored scanner sits inside this cell's folder.")

(defparameter *iocs-subdirectory* "vendor/iocs/"
  "Where the vendored indicator set sits: read for the indicator verb only.")

(defparameter *exit-meanings*
  '((0 . "no actionable indicator matches, and every check in scope ran")
    (1 . "a setup or output error: read the error text, the report may be incomplete")
    (2 . "indicator matches beyond saved references were found")
    (3 . "no actionable matches, but some checks could not run")
    (4 . "nothing was scanned"))
  "The engine's exit codes, said in words (scan/zbtriage/contract.py).")

(defun exit-meaning (code)
  "CODE as the engine's own meaning, or NIL for a code it does not use."
  (cdr (assoc code *exit-meanings*)))

(defun engine-directory ()
  "The scanner's directory: the toolkit setting when it names one, else the vendored copy."
  (let ((named (ignore-errors (setting :toolkit))))
    (if (and named (stringp named) (plusp (length named)))
        (uiop:ensure-directory-pathname named)
        (merge-pathnames *engine-subdirectory*
                         (asdf:system-source-directory "nodecode-zero-breach")))))

(defun engine-script ()
  "The scanner's entry point, as a namestring; REFUSED when it is not there."
  (let ((script (merge-pathnames "zero-breach-toolkit.py" (engine-directory))))
    (unless (probe-file script)
      (fail "the scanner is not at ~a: point the toolkit setting at the toolkit's scan folder, or reinstall the cell with its vendor/ folder"
            script))
    (namestring script)))

;;; --- the seam --------------------------------------------------------------------

(defvar *environment* nil
  "The environment a scan's process is given, replacing the inherited one; NIL inherits.
A test binds it to point the engine at a fixture and a scratch home.")

(defun run-command (argv &key directory)
  "Run ARGV with no shell of ours => (VALUES OUTPUT ERROR-OUTPUT EXIT).
A program that is not installed answers 127, as a shell would."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program argv
                            :output :string
                            :error-output :string
                            :ignore-error-status t
                            :input nil
                            :directory directory
                            :env *environment*)
        (values (or out "") (or err "") code))
    (error () (values "" "" 127))))

(defparameter *run* #'run-command
  "How the engine is run: (funcall *RUN* ARGV) => (VALUES OUTPUT ERROR-OUTPUT EXIT).")

;;; --- the clock on one scan --------------------------------------------------------

(defun timeout-program ()
  "A `timeout' the shell can find, or NIL: it is coreutils, not every system has it."
  (loop for path in '("/usr/bin/timeout" "/bin/timeout" "/usr/local/bin/timeout"
                      "/opt/homebrew/bin/gtimeout")
        when (probe-file path) return path))

(defun timeout-argv (seconds)
  "The prefix that bounds a run at SECONDS, or NIL when unbounded or unbounded here."
  (when (and (integerp seconds) (plusp seconds) (timeout-program))
    (list (timeout-program) (princ-to-string seconds))))

;;; --- one scan ----------------------------------------------------------------------

(defun scope-arguments (scope target)
  "The engine arguments SCOPE and TARGET ask for. => (VALUES ARGUMENTS SCAN-TYPE)"
  (let ((scope (or scope "quick")))
    (cond
      ((string-equal scope "quick") (values '("--quick") "quick"))
      ((member scope '("standard" "default") :test #'string-equal)
       (values '("--scan-type" "default") "default"))
      ((string-equal scope "full") (values (list "--scan-type" "full") "full"))
      ((string-equal scope "browser") (values (list "--scan-type" "browser") "browser"))
      ((string-equal scope "targeted")
       (unless (and target (plusp (length target)))
         (fail "a targeted scan needs a target folder: it reads the copies of startup files in it"))
       (values (list "--scan-type" "targeted" "--target" target) "targeted"))
      (t (fail "~a is not one of the scan scopes: quick, standard, full, browser, targeted" scope)))))

(defun report-directory ()
  "Where the engine writes its JSON: the report_directory setting, else this profile's
zero-breach folder. A report carries machine paths, so it never lands in the cell's own
folder."
  (let ((named (ignore-errors (setting :report-directory))))
    (ensure-directories-exist
     (uiop:ensure-directory-pathname
      (if (and named (stringp named) (plusp (length named)))
          named
          (merge-pathnames "zero-breach/" nlk:*profiles-directory*))))))

(defun stamp ()
  "A file-name stamp no two runs share."
  (format nil "~d-~d" (get-universal-time) (random 1000000)))

(defun gather-scan (&key scope target phone patterns feeds timeout)
  "Run one scan and read what it wrote.
=> a plist: :status :exit :full-report :summary-report :scope :seconds :stdout
:stderr :hits :gaps :modules :checks :worth-a-look :doors :indicator-set :next-steps
:must-relay. The report paths are namestrings; the hits, gaps and modules are read
out of the engine's own JSON, never rebuilt from a summary."
  (let* ((script (engine-script))
         (directory (report-directory))
         (base (stamp))
         (full (merge-pathnames (format nil "zero-breach-~a.full.json" base) directory))
         (summary (merge-pathnames (format nil "zero-breach-~a.summary.json" base) directory))
         (started (get-internal-real-time))
         (scan-type nil)
         argv)
    (ensure-directories-exist full)
    (multiple-value-bind (scope-args engine-scan-type) (scope-arguments scope target)
      (setf scan-type engine-scan-type)
      (setf argv (append (timeout-argv (or timeout (setting :timeout)))
                         (list (setting :python) "-X" "utf8" script
                               "--no-banner" "--no-menu"
                               "--json" (namestring full)
                               "--share-json" (namestring summary))
                         (unless feeds '("--no-feeds"))
                         (when (and (setting :iocs) (plusp (length (setting :iocs))))
                           (list "--iocs" (setting :iocs)))
                         (when patterns '("--patterns"))
                         (when (and phone (plusp (length phone)))
                           (list "--phone" phone))
                         scope-args)))
    (multiple-value-bind (out err code) (funcall *run* argv)
      (let* ((seconds (/ (- (get-internal-real-time) started)
                         (float internal-time-units-per-second)))
             (report (and (probe-file full) (read-report full)))
             (share (and (probe-file summary) (read-report summary))))
        (cond
          ((null report)
           (fail "the scanner wrote no report (exit ~d): ~a"
                 code (trimmed-output err out)))
          ((and (= code 1) (null (report-status report)))
           (fail "the scanner stopped with an error (exit 1): ~a" (trimmed-output err out))))
        (plist-report report share :exit code :seconds seconds
                      :scope scope :scan-type scan-type
                      :full (namestring full) :summary (namestring summary)
                      :stdout out :stderr err)))))

(defun trimmed-output (err out)
  "The last few lines of ERR (or OUT), one line, for an error answer."
  (let* ((text (if (plusp (length err)) err out))
         (lines (remove-if (lambda (line) (zerop (length line)))
                           (uiop:split-string text :separator '(#\Newline)))))
    (if lines
        (format nil "~{~a~^ / ~}" (last lines (min 3 (length lines))))
        "no output")))

;;; --- the report as values ------------------------------------------------------------

(defun read-report (path)
  "PATH read as decoded JSON (the kernel's own decoder)."
  (nlk:decode-json (uiop:read-file-string path)))

(defun read-json (text)
  "TEXT, a JSON document already in hand, decoded."
  (nlk:decode-json text))

(defun field (object type &rest path)
  "One typed read out of decoded JSON, NIL when the path is absent."
  (apply #'nlk:json-value object type path))

(defun report-status (report)
  "The engine's own verdict string, e.g. NO_KNOWN_INDICATORS_WITH_GAPS."
  (field report :string "verdict" "status"))

(defun hit-rows (report)
  "Every hit the scan recorded, as a plist each: :module :indicator :family :tier
:confidence :location :detail. Nothing here is graded by this cell: the tier and
the confidence are the engine's."
  (loop for name being the hash-keys of (or (field report :object "modules") (make-hash-table))
        using (hash-value module)
        append (loop for hit across (or (field module :array "hits") #())
                     collect (list :module name
                                   :indicator (field hit :string "indicator")
                                   :family (field hit :string "family")
                                   :tier (field hit :string "tier")
                                   :confidence (field hit :string "confidence")
                                   :location (field hit :string "location")
                                   :detail (field hit :string "detail")))))

(defun gap-rows (report)
  "The engine's own sentences for what could not be checked."
  (coerce (or (field report :array "verdict" "gaps") #()) 'list))

(defun module-rows (report)
  "One row per module: (NAME STATUS CHECKED DURATION SECONDS), worst first."
  (sort (loop for name being the hash-keys of (or (field report :object "modules") (make-hash-table))
              using (hash-value module)
              collect (list name
                            (field module :string "status")
                            (or (field module :integer "checked") 0)
                            (or (field module :number "duration_s") 0)))
        #'string< :key #'first))

(defun failed-checks (report)
  "The checks that did not run, as sentences built from the module's own check rows."
  (loop for name being the hash-keys of (or (field report :object "modules") (make-hash-table))
        using (hash-value module)
        append (loop for check across (or (field module :array "checks") #())
                     unless (field check :boolean "ran")
                       collect (format nil "~a/~a: ~a" name
                                       (or (field check :string "name") "check")
                                       (or (field check :string "reason") "did not run")))))

(defun worth-rows (report)
  "Items under the engine's ``worth a look'': not findings."
  (loop for row across (or (field report :array "worth_a_look") #())
        collect (list :what (field row :string "what")
                      :location (field row :string "location")
                      :usually (field row :string "usual_reason")
                      :observed (field row :string "observed"))))

(defun door-rows (report)
  "The engine's ``doors to close'': prevention, never a finding."
  (loop for row across (or (field report :array "doors") #())
        when (string-equal (or (field row :string "state") "open") "open")
          collect (list :title (field row :string "title")
                        :why (field row :string "why")
                        :fix (field row :string "fix"))))

(defun caveats (share)
  "The engine's must_relay sentences, verbatim: the caveats an answer has to pass on."
  (loop for row across (or (field share :array "must_relay") #())
        collect (field row :string "text")))

(defun next-steps (share)
  "The engine's next_steps, verbatim and capped at four."
  (loop for row across (or (field share :array "next_steps") #())
        for text = (field row :string "text")
        when text collect text into steps
        finally (return (subseq steps 0 (min 4 (length steps))))))

(defun indicator-set (report)
  "The indicator set the scan ran on: count, age, staleness, families."
  (let ((set (field report :object "ioc_set")))
    (list :count (field set :integer "count")
          :age-days (field set :integer "age_days")
          :stale (field set :boolean "stale")
          :freshness-unknown (field set :boolean "freshness_unknown")
          :families (loop for name being the hash-keys of (or (field set :object "families")
                                                              (make-hash-table))
                          collect name))))

(defun plist-report (report share &key exit seconds full summary stdout stderr scope scan-type)
  "The report and its summary as the plist the tools answer from. SCOPE is the scope the
caller asked for; SCAN-TYPE the engine's own word for what it then ran. The two differ on
a quick scan: that is the engine's default scope with the file fingerprints left out, and
it says so as a gap."
  (list :status (report-status report)
        :exit exit
        :exit-meaning (exit-meaning exit)
        :seconds seconds
        :full-report full
        :summary-report summary
        :scope (or scope (field report :string "scope" "scan_type"))
        :scan-type scan-type
        :tool-version (field report :string "tool" "version")
        :platform (field report :string "platform" "system")
        :machine (field report :string "platform" "machine")
        :checked (or (field report :integer "coverage" "checked_count")
                     (field share :integer "coverage" "checked_count"))
        :gap-count (length (gap-rows report))
        :hits (hit-rows report)
        :gaps (gap-rows report)
        :failed-checks (failed-checks report)
        :modules (module-rows report)
        :worth-a-look (worth-rows report)
        :doors (door-rows report)
        :indicator-set (indicator-set report)
        :caveats (caveats share)
        :next-steps (next-steps share)
        :stdout stdout
        :stderr stderr))
