;;;; cell-test.lisp --- the custodian cell over log files the test writes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reads a real log or waits on a clock: each test writes the
;;;; file it wants scanned under the scratch home, dates its lines for today,
;;;; and asks the cell through the tools the model would call.

(in-package #:nodecode.test)

(define-test-slice "custodian" "CUSTODIAN-CELL-"
  :start nodecode-custodian:start-cell
  :defaults (("sources" (vector (namestring (nlk:home "logs/watched.log"))))))

(define-cell-lifecycle-tests "custodian"
  (:help :custodian)
  (:command "custodian")
  (:running
   (is (nle::find-tool "custodian_scan") "the scan tool is registered")
   (is (nle::find-tool "custodian_issues") "and the ledger's")
   (is (nle::find-tool "custodian_status") "and the status's")
   (is (search "custodian/" (namestring (nodecode-custodian::ledger-path))) "the ledger lives under the home"))
  (:stopped
   (is (null (nle::find-tool "custodian_scan")) "and stop takes every one back")
   (is (null (nle::find-tool "custodian_status"))))
  (:refused ("sources" #()) ("max_age_hours" 0) ("tail_lines" -1)))

;;; --- the fixtures -------------------------------------------------------------

(defun custodian-log (text &optional (name "watched.log"))
  "TEXT written to NAME under the scratch home's logs/; => its namestring."
  (let ((path (nlk:home (format nil "logs/~a" name))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    (namestring path)))

(defun custodian-line (message)
  "MESSAGE as a log line dated today, so the recency window keeps it."
  (format nil "~a 10:00:00 ~a" (nodecode-custodian::date-stamp) message))

(defun custodian-reset ()
  "An empty store: no ledger, no journals. Every test that scans starts here."
  (ignore-errors (delete-file (nodecode-custodian::ledger-path)))
  (dolist (day (ignore-errors (directory (nlk:home "custodian/journal/*/"))))
    (ignore-errors (uiop:delete-directory-tree day :validate t :if-does-not-exist :ignore))))

(defun custodian-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "custodian-call" name arguments)))

;;; --- the scan ----------------------------------------------------------------

(deftest custodian-cell-a-scan-reports-the-line-that-matched ()
  (custodian-reset)
  (custodian-log (format nil "~a~%~a~%"
                         (custodian-line "WARNING Job 'nightly' has been disabled after 3 failures")
                         (custodian-line "ERROR something else entirely")))
  (with-cell-stop ((custodian-start))
    (let ((answer (custodian-call "custodian_scan" "{}")))
      (is (search "oc_cron_disabled_transient" answer) "the fingerprint is named")
      (is (search "tier 1" answer) "with its tier")
      (is (search "Job 'nightly' has been disabled" answer) "and the line that matched, not the top of the file")
      (is (search "fix: Re-enable" answer) "and the fix known for it")
      (is (= 1 (length (nodecode-custodian::open-issues))) "one issue is filed")
      (custodian-call "custodian_scan" "{}")
      (is (= 1 (length (nodecode-custodian::open-issues))) "a second scan bumps that issue rather than filing another")
      (is (= 2 (nodecode-custodian::json-number-value
                (first (nodecode-custodian::open-issues)) "count"))
          "and counts the second sighting"))))

(deftest custodian-cell-a-line-that-quotes-a-pattern-is-not-a-finding ()
  (custodian-reset)
  (custodian-log (custodian-line
                  "custodian report: fingerprint_id oc_cron_timeout match_patterns auto_fix for job disabl"))
  (with-cell-stop ((custodian-start))
    (let ((answer (custodian-call "custodian_scan" "{}")))
      (is (search "Nothing matched" answer) "a report about a finding is not one, or a repair re-reports forever")
      (is (null (nodecode-custodian::open-issues)) "and nothing is filed"))))

(deftest custodian-cell-a-stale-line-and-its-continuation-are-not-findings ()
  (custodian-reset)
  (custodian-log (format nil "2020-01-01 08:00:00 ERROR old trouble~%  No space left on device~%~a~%  Job 'fresh' has been disabled after a timeout~%"
                         (custodian-line "INFO resumed")))
  (with-cell-stop ((custodian-start))
    (let ((answer (custodian-call "custodian_scan" "{}")))
      (is (not (search "oc_disk_full" answer))
          "a continuation line inherits the date above it, so an old error's body is stale too")
      (is (search "oc_cron_disabled_transient" answer) "the recent one still reports")
      (is (search "fresh" answer) "with its own line as the evidence"))))

(deftest custodian-cell-an-empty-source-list-refuses ()
  (is (signals-error nlk:config-refusal (custodian-start "sources" #()))
      "a cell with nothing to watch refuses rather than starting mute")
  (is (search "sources"
              (handler-case (custodian-start "sources" #())
                (nlk:config-refusal (condition) (princ-to-string condition))))
      "and the refusal names the setting to fill in"))

;;; --- the ledger ---------------------------------------------------------------

(deftest custodian-cell-the-ledger-lists-counts-and-resolves ()
  (custodian-reset)
  (custodian-log (custodian-line "ERROR HTTP 502 provider_unavailable"))
  (with-cell-stop ((custodian-start))
    (custodian-call "custodian_scan" "{}")
    (let* ((listed (custodian-call "custodian_issues" "{}"))
           (issue (first (nodecode-custodian::open-issues)))
           (id (nodecode-custodian::json-text-value issue "id")))
      (is (search id listed) "the open issue is listed by its id")
      (is (search "oc_http_502_provider_unavailable" listed) "with its fingerprint")
      (is (search "tier 2" listed) "and the tier a reader acts on")
      (is (search "1 open" (custodian-call "custodian_issues" "{\"action\": \"summary\"}"))
          "the summary counts what is open")
      (is (search "resolved"
                  (custodian-call "custodian_issues"
                                  (format nil "{\"action\": \"resolve\", \"issue_id\": ~s}" id)))
          "resolve says so")
      (is (null (nodecode-custodian::open-issues)) "and the ledger has nothing open after it"))))

(deftest custodian-cell-the-command-and-the-status-answer ()
  (custodian-reset)
  (with-cell-stop ((custodian-start))
    (let ((status (nodecode-custodian::status)))
      (is (search (format nil "~d fingerprints tracked" (length nodecode-custodian::*fingerprints*))
                  status)
          "the status counts the fingerprints")
      (is (search "last scan: never" status) "and says when the last scan ran")
      (is (search "tier 1" status) "with the tiers"))
    (is (search "custodian_scan" (nodecode-custodian::run-command "help" nil)) "the manual names the tools")
    (is (search "custodian ledger" (nodecode-custodian::run-command "issues" nil)) "issues reads the ledger")
    (is (search "custodian takes" (nodecode-custodian::run-command "frobnicate" nil))
        "and a word it does not know is told what it takes")))
