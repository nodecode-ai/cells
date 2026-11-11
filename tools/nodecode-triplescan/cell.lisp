;;;; cell.lisp --- one folder, three local scanners, one honest verdict.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT IT RUNS. Gitleaks (leaked secrets), OSV-Scanner (vulnerable
;;;; dependencies) and Semgrep CE (static-analysis findings) are programs the
;;;; operator already has. Each is launched without a shell, from the folder
;;;; being audited, and only reads it. What each printed is normalized into
;;;; one result whose every scanner answers PASS, FAIL, NOT_APPLICABLE or
;;;; UNRESOLVED, and whose COVERAGE_INCOMPLETE says whether the audit could
;;;; really see everything -- so "nothing found" never reads like "could not
;;;; verify".
;;;;
;;;; WHAT IT KEEPS. Gitleaks runs with --redact=100, and only its rule id,
;;;; description, file, line and fingerprint are copied out of its report: a
;;;; secret's value is never in a result. Nothing here writes to the folder,
;;;; to the config or anywhere else.
;;;;
;;;; THE ONE SEAM. *RUN* is (ARGS TIMEOUT) => (VALUES STDOUT STDERR
;;;; EXIT-CODE); the tests replace it, and nothing else does.

(in-package #:nodecode-triplescan)

(nlk:define-peripheral triplescan :not-running t)

;;; --- what a scan is -------------------------------------------------------------

(defparameter +type+ "SECURITY_AUDIT"
  "The result's own type member, as the plugin's result carried it.")

(defparameter +capture+ (* 10 1024 1024)
  "How much of one scanner's output is kept: ten mebibytes, as the plugin had.")

(defparameter +scanners+ '("gitleaks" "osv" "semgrep")
  "The three scanners, in the order a report names them.")

(defparameter +labels+ '(("gitleaks" . "GITLEAKS") ("osv" . "OSV-SCANNER")
                          ("semgrep" . "SEMGREP"))
  "What a scanner is called in a report and in a refusal.")

(defparameter +programs+ '(("gitleaks" . "Gitleaks") ("osv" . "OSV-Scanner")
                            ("semgrep" . "Semgrep"))
  "What a scanner is called when its program is missing.")

(defparameter +minimum-timeout+ 10
  "A scanner's floor: below this a run is a guess.")

(defparameter +maximum-timeout+ 1800
  "A scanner's ceiling: half an hour, as the plugin clamped it.")

(defparameter *standing*
  "triplescan: one folder audited by three local scanners -- Gitleaks for leaked secrets, OSV-Scanner for vulnerable dependencies, Semgrep for static-analysis findings. Every scanner answers PASS, FAIL, NOT_APPLICABLE or UNRESOLVED, and coverage_incomplete says whether the audit saw everything: UNRESOLVED is not a clean result. Secret values are redacted and never returned, and nothing is written to the folder."
  "The standing section every request carries while the cell runs.")

(defparameter *primer*
  "triplescan runs three programs the operator installs -- Gitleaks, OSV-Scanner and Semgrep -- over one folder and answers one structured result. Call triplescan with the folder; the operator's /triplescan PATH audits the folder they name.

The result carries, per scanner, a status:
  PASS            the scanner ran and found nothing
  FAIL            the scanner ran and reported findings
  NOT_APPLICABLE  there was nothing for it to inspect (OSV: no package sources)
  UNRESOLVED      coverage cannot be trusted: the program is missing, timed out,
                  printed something unreadable, or failed
and an overall of FAIL, UNRESOLVED or PASS, in that precedence. UNRESOLVED is
not a pass: say what could not be verified instead of calling the folder clean.
Gitleaks findings name the rule, the file and the line, never the secret.
Nothing is installed, updated, auto-fixed or written; a folder with no git
history is scanned as it stands."
  "What (help :triplescan) answers while the cell runs.")

;;; --- the programs ---------------------------------------------------------------

(defun program-file (directory name)
  (ignore-errors
    (probe-file (merge-pathnames name (uiop:ensure-directory-pathname
                                        (if (plusp (length directory)) directory "."))))))

(defun program-names (name)
  "NAME, and on a machine that insists, the extensions it may carry."
  (if (or (find #\/ name) (find #\\ name) (find #\. name))
      (list name)
      (list name (concatenate 'string name ".exe")
            (concatenate 'string name ".cmd") (concatenate 'string name ".bat"))))

(defun program-path (name)
  "Where NAME is: as a path when it names one, else the first file of that
name on PATH; NIL when there is none."
  (when (and (stringp name) (plusp (length name)))
    (if (or (find #\/ name) (find #\\ name))
        (ignore-errors (probe-file name))
        (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                    :separator '(#\: #\;))
              for found = (loop for candidate in (program-names name)
                                for file = (program-file directory candidate)
                                when file return file)
              when found return found))))

(defun scanner-program (name)
  "The program NAME runs, resolved on PATH, or NIL."
  (program-path
   (cond ((equal name "gitleaks") (setting :gitleaks))
         ((equal name "osv") (setting :osv-scanner))
         (t (setting :semgrep)))))

(defun clamp-timeout (seconds)
  "SECONDS held between the floor and the ceiling."
  (min +maximum-timeout+ (max +minimum-timeout+ (or seconds +minimum-timeout+))))

;;; --- one program, run -----------------------------------------------------------

(defun run-program (args timeout)
  "ARGS run without a shell => (VALUES STDOUT STDERR EXIT-CODE)."
  (uiop:run-program args
                     :input ""
                     :output :string
                     :error-output :string
                     :timeout timeout
                     :ignore-error-status t
                     :force-shell nil))

(defvar *run* #'run-program
  "The one seam that starts a scanner: (ARGS TIMEOUT) => (VALUES STDOUT STDERR
EXIT-CODE). The tests replace it.")

(defun scanner-run (exe args timeout)
  "EXE with ARGS run => (VALUES STDOUT STDERR EXIT-CODE). A run that could not
start, timed out, or printed more than the capture limit is a refusal."
  (multiple-value-bind (out err code)
      (handler-case (funcall *run* (cons exe args) timeout)
        (triplescan-error (condition) (error condition))
        (serious-condition (condition)
          (fail "~a did not finish: ~a" (file-namestring exe) condition)))
    (when (> (length (or out "")) +capture+)
      (fail "~a printed more than the 10 MiB this cell keeps" (file-namestring exe)))
    (values (or out "") (or err "") code)))

(defun short-error (code stdout stderr)
  "What a scanner said when what it printed was unusable."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return #\Nul)
                            (if (plusp (length stderr)) stderr stdout))))
    (if (plusp (length text))
        (subseq text 0 (min 2000 (length text)))
        (format nil "scanner exited with code ~d" code))))

;;; --- JSON, read leniently and written once ---------------------------------------

(defun parse-json (text)
  "TEXT as decoded JSON, or NIL when it is not JSON."
  (let ((trimmed (and (stringp text)
                       (string-trim '(#\Space #\Tab #\Newline #\Return #\Nul) text))))
    (when (and trimmed (plusp (length trimmed)))
      (when (char= (char trimmed 0) (code-char #xFEFF))
        (setf trimmed (subseq trimmed 1)))
      (ignore-errors (nlk:decode-json trimmed)))))

(defun member-value (object &rest keys)
  "OBJECT's value at the first of KEYS it carries, or NIL."
  (when (hash-table-p object)
    (loop for key in keys
          for value = (gethash key object)
          when value return value)))

(defun object (&rest pairs)
  "A JSON object from alternating KEY VALUE strings; a NIL value's member is
left out, so a finding carries only what the scanner said."
  (apply #'nlk:make-json-object
         (loop for (key value) on pairs by #'cddr when value append (list key value))))

(defun scanner-result (status findings &key note error coverage)
  "One scanner's result: its STATUS, its FINDINGS, and why it could not see
everything when it could not."
  (object "status" status
          "findings" (coerce findings 'vector)
          "note" note
          "error" error
          "coverage_incomplete" (and coverage t)))

;;; --- the three scanners -----------------------------------------------------------

(defun scan-gitleaks (exe directory timeout)
  "Gitleaks over DIRECTORY => its result. Only the rule, the file and the line
are copied out: --redact=100 keeps the secret out of the report, and the
report's own Secret member is never read."
  (multiple-value-bind (out err code)
      (scanner-run exe (list "dir" (uiop:native-namestring directory)
                              "--report-format" "json" "--report-path" "-"
                              "--redact=100" "--no-color")
                    timeout)
    (let ((parsed (parse-json out)))
      (if (not (vectorp parsed))
          (scanner-result "UNRESOLVED" '()
                          :error (format nil "Gitleaks returned malformed JSON. ~a"
                                          (short-error code out err)))
          (let ((findings
                  (loop for item across (subseq parsed 0 (min (length parsed)
                                                               (setting :max-findings)))
                        when (hash-table-p item)
                          collect (object "rule_id" (member-value item "RuleID" "RuleId")
                                          "description" (member-value item "Description")
                                          "file" (member-value item "File")
                                          "start_line" (member-value item "StartLine")
                                          "fingerprint" (member-value item "Fingerprint")))))
            (cond ((plusp (length findings)) (scanner-result "FAIL" findings))
                  ((eql code 0) (scanner-result "PASS" '()))
                  (t (scanner-result "UNRESOLVED" '()
                                      :error (short-error code out err)))))))))

(defun walk-vulnerabilities (value)
  "Every object under a \"vulnerabilities\" array anywhere in VALUE, in order.
OSV-Scanner nests them under results, packages and groups, so the walk is
recursive as the plugin's reader was."
  (let ((found '()))
    (labels ((walk (node)
               (cond
                 ((hash-table-p node)
                  (let ((list (gethash "vulnerabilities" node)))
                    (when (vectorp list)
                      (loop for item across list when (hash-table-p item) do (push item found))))
                  (loop for value being the hash-values of node do (walk value)))
                 ((and (vectorp node) (not (stringp node)))
                  (loop for item across node do (walk item))))))
      (walk value))
    (nreverse found)))

(defun scan-osv (exe directory timeout)
  "OSV-Scanner over DIRECTORY => its result. Exit code 128 is the scanner's
\"no package sources found\", which is NOT_APPLICABLE, not a failure."
  (multiple-value-bind (out err code)
      (scanner-run exe (list "scan" "source" "-r" (uiop:native-namestring directory)
                              "--format" "json")
                    timeout)
    (if (eql code 128)
        (scanner-result "NOT_APPLICABLE" '()
                        :note "No supported package sources were found.")
        (let ((parsed (parse-json out)))
          (if (not (or (hash-table-p parsed) (vectorp parsed)))
              (scanner-result "UNRESOLVED" '()
                              :error (format nil "OSV-Scanner returned malformed JSON. ~a"
                                              (short-error code out err)))
              (let ((findings '())
                    (seen (make-hash-table :test #'equal))
                    (limit (setting :max-findings)))
                (loop for vulnerability in (walk-vulnerabilities parsed)
                      for id = (princ-to-string (or (member-value vulnerability "id" "ID")
                                                    "unknown"))
                      for summary = (member-value vulnerability "summary")
                      for key = (format nil "~a~c~a" id #\Nul (or summary ""))
                      while (< (length findings) limit)
                      unless (gethash key seen)
                        do (setf (gethash key seen) t)
                           (push (object "id" id "summary" summary
                                          "details" (member-value vulnerability "details"))
                                 findings))
                (setf findings (nreverse findings))
                (cond ((plusp (length findings)) (scanner-result "FAIL" findings))
                      ((eql code 0) (scanner-result "PASS" '()))
                      (t (scanner-result "UNRESOLVED" '()
                                          :error (short-error code out err))))))))))

(defun semgrep-errors (errors)
  "The first five analysis errors Semgrep reported, and how many more there were."
  (let* ((shown (subseq errors 0 (min 5 (length errors))))
         (text (format nil "~{~a~^; ~}"
                       (loop for item across shown
                             collect (if (hash-table-p item)
                                         (princ-to-string
                                          (or (member-value item "message" "type" "code")
                                              "unspecified Semgrep error"))
                                         (princ-to-string item))))))
    (concatenate 'string (subseq text 0 (min 1800 (length text)))
                 (if (> (length errors) 5)
                     (format nil " (+~d more)" (- (length errors) 5))
                     ""))))

(defun scan-semgrep (exe directory timeout)
  "Semgrep over DIRECTORY with the configured rules => its result. A run that
reported analysis errors did not see everything: findings beside them are a
FAIL with incomplete coverage, and errors alone are UNRESOLVED."
  (multiple-value-bind (out err code)
      (scanner-run exe (list "scan" "--config" (setting :semgrep-config)
                              "--json" "--quiet" (uiop:native-namestring directory))
                    timeout)
    (let ((parsed (parse-json out)))
      (if (not (and (hash-table-p parsed)
                    (vectorp (gethash "results" parsed))
                    (let ((errors (gethash "errors" parsed)))
                      (or (null errors) (vectorp errors)))))
          (scanner-result "UNRESOLVED" '()
                          :error (format nil "Semgrep returned malformed JSON. ~a"
                                          (short-error code out err)))
          (let* ((results (gethash "results" parsed))
                 (errors (gethash "errors" parsed))
                 (findings
                   (loop for item across (subseq results 0 (min (length results)
                                                                 (setting :max-findings)))
                         when (hash-table-p item)
                           collect (object "check_id" (member-value item "check_id")
                                           "path" (member-value item "path")
                                           "line" (member-value (member-value item "start") "line")
                                           "message" (member-value (member-value item "extra") "message")
                                           "severity" (member-value (member-value item "extra") "severity"))))
                 (failure (and (vectorp errors) (plusp (length errors))
                               (format nil "Semgrep reported analysis errors: ~a"
                                       (semgrep-errors errors)))))
            (cond ((and failure (plusp (length findings)))
                   (scanner-result "FAIL" findings :error failure :coverage t))
                  (failure (scanner-result "UNRESOLVED" '() :error failure :coverage t))
                  ((plusp (length findings)) (scanner-result "FAIL" findings))
                  ((eql code 0) (scanner-result "PASS" '()))
                  (t (scanner-result "UNRESOLVED" '()
                                      :error (short-error code out err)))))))))

(defun missing-scanner (name)
  "NAME's program is nowhere: the plugin's UNRESOLVED, in words that say what
to do about it."
  (scanner-result "UNRESOLVED" '()
                  :error (format nil "~a was not found. Install it, or name its path in the triplescan section."
                                  (cdr (assoc name +programs+ :test #'equal)))))

(defun scan-one (name directory timeout)
  "NAME's scanner over DIRECTORY => its result object."
  (let ((exe (scanner-program name)))
    (if (null exe)
        (missing-scanner name)
        (handler-case
            (cond ((equal name "gitleaks") (scan-gitleaks exe directory timeout))
                  ((equal name "osv") (scan-osv exe directory timeout))
                  (t (scan-semgrep exe directory timeout)))
          (triplescan-error (condition)
            (scanner-result "UNRESOLVED" '() :error (princ-to-string condition)))))))

;;; --- the folder, and the audit ----------------------------------------------------

(defun expand-home (path)
  "PATH with a leading ~ taken to the home folder."
  (if (and (plusp (length path)) (char= #\~ (char path 0)))
      (let ((rest (subseq path 1)))
        (namestring (merge-pathnames (if (and (plusp (length rest)) (find (char rest 0) "/\\"))
                                         (subseq rest 1)
                                         rest)
                                      (user-homedir-pathname))))
      path))

(defun workspace-directory (path)
  "PATH as a directory that is really there, or NIL. A relative path is read
from the folder the Nodecode process runs in."
  (let ((name (expand-home (nlk:trimmed (or path "")))))
    (when (plusp (length name))
      (let ((directory (ignore-errors (uiop:truename* (uiop:parse-native-namestring name)))))
        (and directory (uiop:directory-pathname-p directory) directory (probe-file directory)
             directory)))))

(defun wanted-scanners (names)
  "The scanners NAMES asks for, all three when it names none."
  (let ((names (cond ((null names) +scanners+)
                     ((stringp names) (list names))
                     (t (coerce names 'list)))))
    (dolist (name names)
      (unless (member name +scanners+ :test #'equal)
        (fail "~s is not a scanner: name gitleaks, osv or semgrep, or leave it out for all three"
              name)))
    (remove-duplicates names :test #'equal)))

(defun overall-status (scanners)
  "FAIL beats UNRESOLVED beats PASS, the plugin's own precedence."
  (let ((statuses (loop for (nil . result) in scanners collect (gethash "status" result))))
    (cond ((member "FAIL" statuses :test #'equal) "FAIL")
          ((or (member "UNRESOLVED" statuses :test #'equal)
               (member "BLOCKED" statuses :test #'equal))
           "UNRESOLVED")
          (t "PASS"))))

(defun coverage-incomplete-p (scanners)
  "Whether any scanner could not see everything it was asked to."
  (loop for (nil . result) in scanners
        thereis (or (equal "UNRESOLVED" (gethash "status" result))
                    (and (gethash "coverage_incomplete" result) t))))

(defun summary-text (workspace scanners)
  "The audit in words: one line per scanner, and what each of them said."
  (with-output-to-string (out)
    (format out "Workspace: ~a~%~%" workspace)
    (loop for (name . result) in scanners
          for status = (gethash "status" result)
          for detail = (or (gethash "note" result) (gethash "error" result))
          do (format out "~a: ~a~a~%"
                     (or (cdr (assoc name +labels+ :test #'equal)) name)
                     status
                     (cond (detail (format nil " - ~a" detail))
                           ((equal status "PASS") " - no findings")
                           (t (format nil " - ~d finding(s)"
                                      (length (gethash "findings" result)))))))))

(defun run-audit (workspace &key scanners timeout)
  "WORKSPACE audited => the result as a plist: :OVERALL, :COVERAGE,
:WORKSPACE, :SCANNERS and :SUMMARY."
  (let ((directory (workspace-directory workspace)))
    (if (null directory)
        (list :overall "BLOCKED" :coverage t
              :workspace (nlk:trimmed (or workspace ""))
              :scanners '()
              :summary (format nil "Workspace validation failed: ~a is not a folder this machine can read."
                               (nlk:trimmed (or workspace ""))))
        (let* ((target (uiop:native-namestring directory))
               (wanted (wanted-scanners scanners))
               (ceiling (clamp-timeout (or timeout (setting :timeout-seconds))))
               (results (loop for name in wanted
                              collect (cons name (scan-one name directory ceiling)))))
          (list :overall (overall-status results)
                :coverage (coverage-incomplete-p results)
                :workspace target
                :scanners results
                :summary (summary-text target results))))))

(defun result-json (result)
  "RESULT as the JSON text the tool answers, in the plugin's own shape."
  (nlk:pretty-json
   (nlk:make-json-object
    "type" +type+
    "overall" (getf result :overall)
    ;; Every member is written, coverage_incomplete included: a false one is
    ;; the answer "the audit saw everything", not a missing member.
    "coverage_incomplete" (and (getf result :coverage) t)
    "workspace" (getf result :workspace)
    "scanners" (apply #'nlk:make-json-object
                      (loop for (name . scanner) in (getf result :scanners)
                            append (list name scanner)))
    "summary" (getf result :summary))))

;;; --- the verbs -------------------------------------------------------------------

(define-verb audit (workspace &key scanners timeout)
  "Audit WORKSPACE with Gitleaks, OSV-Scanner and Semgrep => the result as
pretty JSON, the shape the model's tool answers."
  (result-json (run-audit workspace :scanners scanners :timeout timeout)))

(define-verb report (workspace &key scanners timeout)
  "Audit WORKSPACE and answer what the scanners said, in words."
  (getf (run-audit workspace :scanners scanners :timeout timeout) :summary))

(defun audit-tool (request)
  "The model's tool: one folder in, the structured result out."
  (result-json (run-audit (triplescan-audit-workspace request)
                           :scanners (triplescan-audit-scanners request)
                           :timeout (triplescan-audit-timeout-seconds request))))

;;; --- the approval gate -------------------------------------------------------------

(defparameter +allow-once+ "Allow once"
  "The answer that lets one scan run.")

(defparameter +deny+ "Deny"
  "The answer that lets nothing run.")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  ;; Read by name: this cell also loads in a core released before the question
  ;; board, where a question then asks nobody and the scan is refused.
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun ask-before-scan (workspace)
  "With `ask' on, ask whether a model call may run the scanners => the answer,
or NIL and why: :OFF with the setting off, :NOBODY with no one to ask."
  (let ((ask (and (setting :ask) (ask-operator-function))))
    (if (null ask)
        (values nil :off)
        (funcall ask "Run the security audit? Gitleaks, OSV-Scanner and Semgrep will read this folder."
                 :detail workspace
                 :choices (list +allow-once+ +deny+)
                 :seconds (max 5 (or (setting :ask-seconds) 120))))))

(defun refusal-text (answer why)
  "The model's words for a scan that did not run."
  (cond ((equal answer +deny+)
         "Refused by triplescan: the operator was asked and chose Deny; no scanner ran -- ask them what they want instead, and do not retry it.")
        ((eq why :timeout)
         "Refused by triplescan: nobody answered in time; no scanner ran -- ask the operator to run /triplescan PATH themselves.")
        (t
         "Refused by triplescan: there was nobody to allow the scan; no scanner ran -- ask the operator to run /triplescan PATH themselves.")))

(defun approve (op next)
  "The :TOOL point: with `ask' on, a triplescan call waits for the operator's
Allow once before a scanner starts; Deny, or nobody there, runs nothing and
NEXT never runs."
  (if (not (equal "triplescan" (getf op :name)))
      (funcall next op)
      (let ((arguments (getf op :arguments)))
        (multiple-value-bind (answer why)
            (ask-before-scan (or (and (hash-table-p arguments) (gethash "workspace" arguments))
                                 "the folder named in the call"))
          (cond ((equal answer +allow-once+) (funcall next op))
                ((eq why :off) (funcall next op))
                (t (nle:failure (refusal-text answer why))))))))

;;; --- the command and the standing paragraph ----------------------------------------

(defun run-slash (args session-id)
  "/triplescan PATH audits a folder; /triplescan alone audits the folder the
Nodecode process runs in; /triplescan help says what the scanners answer."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text)) (report "."))
          ((member (string-downcase text) '("help" "?") :test #'string=) *primer*)
          (t (report text)))))

(defun standing-primer (next-fn session-id)
  "Every request's harness sections, with triplescan's paragraph."
  (acons "triplescan" *standing*
         (remove "triplescan" (funcall next-fn session-id) :key #'car :test #'equal)))

;;; --- the declaration ---------------------------------------------------------------

(nlk:define-contract triplescan-audit
  (:doc "Run a read-only security audit of one folder with Gitleaks, OSV-Scanner
and Semgrep, and answer one result carrying PASS, FAIL, NOT_APPLICABLE or
UNRESOLVED per scanner and whether coverage was complete.")
  ("workspace" :text :required t
   :doc "the folder to audit: an absolute path, or one relative to the folder Nodecode runs in")
  ("scanners" :array :of :string
   :doc "which to run: gitleaks, osv, semgrep; all three when absent")
  ("timeout_seconds" :integer
   :doc "one scanner's ceiling in seconds, between 10 and 1800; the section's when absent"))

(nle:define-cell triplescan
  (:section ("triplescan")
    (:guide "install Gitleaks, OSV-Scanner and Semgrep and keep them on PATH; semgrep_config is semgrep's --config and its default auto fetches rules over the network -- point it at a local rules file to keep a scan offline; ask makes a model call wait for Allow once")
    ("gitleaks" :string :default "gitleaks"
     :doc "the Gitleaks program: its name on PATH, or a path to it")
    ("osv_scanner" :string :default "osv-scanner"
     :doc "the OSV-Scanner program: its name on PATH, or a path to it")
    ("semgrep" :string :default "semgrep"
     :doc "the Semgrep program: its name on PATH, or a path to it")
    ("semgrep_config" :string :default "auto"
     :doc "semgrep's --config: auto, a registry pack such as p/default, or a local rules file or folder")
    ("timeout_seconds" :integer :default 300 :min 10
     :doc "one scanner's ceiling in seconds; 10 to 1800")
    ("max_findings" :integer :default 200 :min 1
     :doc "how many findings one scanner reports at most")
    ("ask" :boolean :default nil
     :doc "ask the operator Allow once or Deny before a model call runs the scanners")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long a question waits for an answer"))
  (:tool "triplescan" (nlk:contract 'triplescan-audit) 'audit-tool)
  (:command "triplescan" 'run-slash
            :catches 'triplescan-error
            :description "Audit a folder with Gitleaks, OSV-Scanner and Semgrep"
            :argument-hint "PATH")
  (:help :triplescan
         "triplescan: Gitleaks, OSV-Scanner and Semgrep over one folder - /triplescan PATH"
         *primer*)
  (:hook :tool #'approve)
  (:hook 'nle::read-harness-sections #'standing-primer))
