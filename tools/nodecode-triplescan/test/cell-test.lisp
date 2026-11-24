;;;; cell-test.lisp --- the tool, the command, and what a scanner's answer means.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring against stubbed scanners:
;;;; that a folder is audited by the three programs the plugin ran, with the
;;;; same argv; that their answers are read the way the plugin read them -- a
;;;; Gitleaks report, an OSV walk, a Semgrep analysis error beside findings --
;;;; that a secret's value never reaches the result, that a missing program or
;;;; an unreadable report is UNRESOLVED and not a clean scan, that a folder
;;;; that is not one is BLOCKED, and that /triplescan answers the same audit
;;;; in words. No scanner is installed for any of it and nothing dials out.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *triplescan-calls* '()
  "The ARGS of every stubbed run this test made, newest first.")

(defvar *triplescan-responder* nil
  "What a stubbed run answers: a function of ARGS => (VALUES STDOUT STDERR EXIT-CODE).")

(defparameter *triplescan-programs* nil
  "The stand-in program files, made once a run, as (NAME . PATH).")

(defun triplescan-program (name)
  "A file named NAME on disk, made once a run: the cell resolves it as a
program, and the stub is what answers instead of running it."
  (let ((known (assoc name *triplescan-programs* :test #'equal)))
    (or (cdr known)
        (let ((path (merge-pathnames name (merge-pathnames "triplescan-stub/"
                                                           (uiop:temporary-directory)))))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
            (format out "#!/bin/sh~%exit 0~%"))
          (setf *triplescan-programs* (acons name path *triplescan-programs*))
          path))))

(defmacro with-triplescan-run ((&key (answer "[]") (code 0)) &body body)
  "BODY with every scanner stubbed: each run answers ANSWER with CODE, and
pushes its ARGS on *TRIPLESCAN-CALLS*. A test that needs one answer per
scanner setfs *TRIPLESCAN-RESPONDER* instead."
  `(let ((*triplescan-calls* '())
         (*triplescan-responder* (lambda (args)
                                    (declare (ignore args))
                                    (values ,answer "" ,code)))
         (nodecode-triplescan::*run* (lambda (args timeout)
                                       (declare (ignore timeout))
                                       (push args *triplescan-calls*)
                                       (funcall *triplescan-responder* args))))
     ,@body))

(defun triplescan-asked-scanner (args)
  "Which scanner a stubbed run is, off the argv the cell built."
  (cond ((equal "dir" (second args)) "gitleaks")
        ((and (equal "scan" (second args)) (equal "source" (third args))) "osv")
        (t "semgrep")))

(defun triplescan-answer-by-scanner (&key gitleaks osv semgrep)
  "A responder answering each scanner its own (STDOUT . CODE)."
  (lambda (args)
    (let ((answer (cdr (assoc (triplescan-asked-scanner args)
                               (remove nil (list (and gitleaks (cons "gitleaks" gitleaks))
                                                 (and osv (cons "osv" osv))
                                                 (and semgrep (cons "semgrep" semgrep))))
                               :test #'equal))))
      (values (car answer) "" (cdr answer)))))

(defun triplescan-tool (arguments)
  "Run the registered triplescan tool with ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool "triplescan")))
    (assert entry () "no tool named triplescan")
    (funcall (third entry) (nlk:contract-decode (second entry) arguments))))

(defun triplescan-audit-json (workspace &rest pairs)
  "The tool's answer for WORKSPACE, decoded, with PAIRS laid over the call."
  (nlk:decode-json
   (triplescan-tool (apply #'nlk:make-json-object "workspace" workspace pairs))))

(defun triplescan-workspace ()
  "A folder that is really there, made once a run."
  (let ((directory (merge-pathnames "triplescan-workspace/" (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "probe.txt" directory))
    (uiop:native-namestring directory)))

(defun triplescan-scanner (answer name)
  "ANSWER's scanner member NAME."
  (gethash name (gethash "scanners" answer)))

(defun triplescan-status (answer name)
  (gethash "status" (triplescan-scanner answer name)))

;;; --- the slice ------------------------------------------------------------------

(define-test-slice "triplescan" "TRIPLESCAN-CELL-" :start nodecode-triplescan:start-cell
  :defaults (("gitleaks" (namestring (triplescan-program "gitleaks")))
             ("osv_scanner" (namestring (triplescan-program "osv-scanner")))
             ("semgrep" (namestring (triplescan-program "semgrep")))))

(define-cell-lifecycle-tests "triplescan"
  (:help :triplescan)
  (:command "triplescan")
  (:running (is (nle::find-tool "triplescan") "the audit tool is registered")
            (is (= 300 (nodecode-triplescan::setting :timeout-seconds)) "five minutes a scanner")
            (is (equal '("gitleaks" "osv" "semgrep") nodecode-triplescan:+scanners+) "three scanners"))
  (:stopped (is (null (nle::find-tool "triplescan")) "stop unregisters the tool"))
  (:refused ("timeout_seconds" 5) ("max_findings" 0))
  (:idle nodecode-triplescan:triplescan-error (nodecode-triplescan:report "anything")))

;;; --- one folder, three scanners ---------------------------------------------------

(deftest triplescan-cell-one-folder-is-audited-by-all-three ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (setf *triplescan-responder*
            (triplescan-answer-by-scanner
             :gitleaks (cons "[]" 0)
             :osv (cons "{\"results\":[]}" 0)
             :semgrep (cons "{\"results\":[],\"errors\":[]}" 0)))
      (let ((answer (triplescan-audit-json (triplescan-workspace))))
        (is (equal "SECURITY_AUDIT" (gethash "type" answer)) "the plugin's result type")
        (is (equal "PASS" (gethash "overall" answer)) "three quiet scanners are a pass")
        (is (null (gethash "coverage_incomplete" answer)) "and complete coverage")
        (is (equal "PASS" (triplescan-status answer "gitleaks")))
        (is (equal "PASS" (triplescan-status answer "osv")))
        (is (equal "PASS" (triplescan-status answer "semgrep")))
        (is (= 3 (length *triplescan-calls*)) "three programs ran")
        (let ((gitleaks (first (reverse *triplescan-calls*))))
          (is (equal "dir" (second gitleaks)) "Gitleaks' directory mode")
          (is (member "--report-format" gitleaks :test #'equal))
          (is (member "--redact=100" gitleaks :test #'equal) "and full redaction"))
        (is (search "GITLEAKS: PASS" (gethash "summary" answer)) "the summary names each scanner")))))

(deftest triplescan-cell-a-secret-value-never-reaches-the-result ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (setf *triplescan-responder*
            (triplescan-answer-by-scanner
             :gitleaks (cons "[{\"RuleID\":\"generic-api-key\",\"Description\":\"API key\",\"File\":\"app.py\",\"StartLine\":4,\"Secret\":\"hunter2-secret\",\"Fingerprint\":\"abc:app.py:generic-api-key:4\"}]" 1)
             :osv (cons "{\"results\":[]}" 0)
             :semgrep (cons "{\"results\":[],\"errors\":[]}" 0)))
      (let* ((answer (triplescan-audit-json (triplescan-workspace)))
             (finding (aref (gethash "findings" (triplescan-scanner answer "gitleaks")) 0)))
        (is (equal "FAIL" (gethash "overall" answer)) "a leaked secret is a failure")
        (is (equal "generic-api-key" (gethash "rule_id" finding)) "the rule is named")
        (is (equal "app.py" (gethash "file" finding)) "and the file")
        (is (= 4 (gethash "start_line" finding)) "and the line")
        (is (null (search "hunter2" (nlk:pretty-json answer))) "the secret's value is not in the result")))))

(deftest triplescan-cell-a-missing-program-is-unresolved-not-clean ()
  (with-cell-stop ((triplescan-start "gitleaks" "definitely-no-such-program-xyz"))
    (with-triplescan-run ()
      (setf *triplescan-responder*
            (triplescan-answer-by-scanner
             :osv (cons "{\"results\":[]}" 0)
             :semgrep (cons "{\"results\":[],\"errors\":[]}" 0)))
      (let ((answer (triplescan-audit-json (triplescan-workspace))))
        (is (equal "UNRESOLVED" (triplescan-status answer "gitleaks")) "a missing program is not a pass")
        (is (search "was not found" (gethash "error" (triplescan-scanner answer "gitleaks"))))
        (is (equal "UNRESOLVED" (gethash "overall" answer)) "and the audit says so overall")
        (is (eq t (gethash "coverage_incomplete" answer)) "coverage is incomplete")
        (is (= 2 (length *triplescan-calls*)) "the other two still ran")))))

(deftest triplescan-cell-an-unreadable-report-is-unresolved ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run (:answer "not json at all" :code 2)
      (let ((answer (triplescan-audit-json (triplescan-workspace))))
        (is (equal "UNRESOLVED" (triplescan-status answer "gitleaks")))
        (is (search "malformed JSON" (gethash "error" (triplescan-scanner answer "gitleaks"))))
        (is (search "not json at all" (gethash "error" (triplescan-scanner answer "gitleaks"))) "and what it printed is quoted back")))))

(deftest triplescan-cell-osv-with-no-package-sources-is-not-applicable ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (setf *triplescan-responder*
            (triplescan-answer-by-scanner
             :gitleaks (cons "[]" 0)
             :osv (cons "" 128)
             :semgrep (cons "{\"results\":[],\"errors\":[]}" 0)))
      (let ((answer (triplescan-audit-json (triplescan-workspace))))
        (is (equal "NOT_APPLICABLE" (triplescan-status answer "osv")) "exit 128 is OSV's own no-packages")
        (is (search "No supported package sources"
                    (gethash "note" (triplescan-scanner answer "osv"))))
        (is (equal "PASS" (gethash "overall" answer)) "and it does not spoil the audit")))))

(deftest triplescan-cell-semgrep-findings-beside-errors-are-incomplete-coverage ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (setf *triplescan-responder*
            (triplescan-answer-by-scanner
             :gitleaks (cons "[]" 0)
             :osv (cons "{\"results\":[]}" 0)
             :semgrep (cons "{\"results\":[{\"check_id\":\"rules.x\",\"path\":\"app.py\",\"start\":{\"line\":9},\"extra\":{\"message\":\"bad\",\"severity\":\"ERROR\"}}],\"errors\":[{\"message\":\"a rule timed out\"}]}" 1)))
      (let ((answer (triplescan-audit-json (triplescan-workspace))))
        (is (equal "FAIL" (triplescan-status answer "semgrep")) "a finding is a failure")
        (is (eq t (gethash "coverage_incomplete" answer)) "but the analysis was incomplete")
        (is (search "analysis errors" (gethash "error" (triplescan-scanner answer "semgrep"))))))))

(deftest triplescan-cell-a-vulnerability-is-found-under-its-package ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (setf *triplescan-responder*
            (triplescan-answer-by-scanner
             :gitleaks (cons "[]" 0)
             :osv (cons "{\"results\":[{\"packages\":[{\"vulnerabilities\":[{\"id\":\"GHSA-1\",\"summary\":\"a flaw\"},{\"id\":\"GHSA-1\",\"summary\":\"a flaw\"}]}]}]}" 1)
             :semgrep (cons "{\"results\":[],\"errors\":[]}" 0)))
      (let* ((answer (triplescan-audit-json (triplescan-workspace)))
             (findings (gethash "findings" (triplescan-scanner answer "osv"))))
        (is (equal "FAIL" (triplescan-status answer "osv")))
        (is (= 1 (length findings)) "the same advisory under two packages is one finding")
        (is (equal "GHSA-1" (gethash "id" (aref findings 0))))))))

(deftest triplescan-cell-a-folder-that-is-not-one-is-blocked ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (let ((answer (triplescan-audit-json "/definitely/not/a/folder/here")))
        (is (equal "BLOCKED" (gethash "overall" answer)))
        (is (eq t (gethash "coverage_incomplete" answer)))
        (is (null *triplescan-calls*) "and no scanner was started")))))

(deftest triplescan-cell-one-scanner-asked-for-is-the-one-that-runs ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (setf *triplescan-responder* (triplescan-answer-by-scanner
                                     :osv (cons "{\"results\":[]}" 0)))
      (let ((answer (triplescan-audit-json (triplescan-workspace) "scanners" (vector "osv"))))
        (is (= 1 (length *triplescan-calls*)) "one program ran")
        (is (equal "osv" (triplescan-asked-scanner (first *triplescan-calls*))))
        (is (equal "PASS" (gethash "overall" answer)))))))

(deftest triplescan-cell-a-scanner-it-does-not-know-is-a-repair ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (is-carrying (text (refusal-text nodecode-triplescan:triplescan-error
                                        (nodecode-triplescan:audit (triplescan-workspace)
                                                                    :scanners '("gitleaks" "nmap"))))
                   ("not a scanner") ("gitleaks, osv or semgrep")))))

(deftest triplescan-cell-the-command-answers-the-same-audit-in-words ()
  (with-cell-stop ((triplescan-start))
    (with-triplescan-run ()
      (setf *triplescan-responder*
            (triplescan-answer-by-scanner
             :gitleaks (cons "[]" 0)
             :osv (cons "{\"results\":[]}" 0)
             :semgrep (cons "{\"results\":[],\"errors\":[]}" 0)))
      (let* ((entry (nle::find-registered-command "triplescan"))
             (run (nle::slash-command-headless entry))
             (text (funcall run (triplescan-workspace) "s1")))
        (is (search "Workspace:" text) "the command audits the folder it names")
        (is (search "GITLEAKS: PASS - no findings" text))
        (is (search "SEMGREP: PASS" text))
        (is (= 3 (length *triplescan-calls*)) "three scanners ran")))))
