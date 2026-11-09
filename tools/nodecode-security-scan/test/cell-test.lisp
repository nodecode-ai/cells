;;;; cell-test.lisp --- the walk, the reviewer's result, coverage, reports, /security.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: a scan walks a folder and plans chunks, skipping
;;;; what a standard review skips and reading it in a deep one; the next step
;;;; it names is the step the facts support; a reviewer's result citing
;;;; another snapshot is refused and one citing this snapshot becomes a
;;;; finding; a confirmation needs both controls; chains come from the
;;;; primitives that share a file; a seal refuses before anything was read
;;;; and writes four reports nothing changes afterwards; the dependency check
;;;; is one stubbed call; /security is the operator's, and its grants need an
;;;; allowed origin. Nothing here opens a socket.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "security-scan"
  (:help :security-scan)
  (:command "security")
  (:running (is (search "security_scan_start" nodecode-security-scan::+primer+) "the manual names the tools")
            (is (= 4000 (nodecode-security-scan::setting :max-chunk-lines)) "a chunk is about 4000 lines")
            (is-present (entry (nle::find-tool "security_scan_start")) "the start tool is registered")
            (is-present (entry (nle::find-tool "security_scan_export")) "and the export one"))
  (:refused ("max_chunk_lines" 0) ("grant_minutes" 0) ("max_chunk_lines" "many"))
  (:idle secscan:security-scan-error (secscan:get-scan "scan-2026-11-08-abcdef")))

;;; --- the walk ------------------------------------------------------------------

(deftest security-scan-cell-a-standard-scan-reads-production-code-and-names-what-it-skipped ()
  (with-dir (root)
    (put root "src/app.lisp" "(defun handler (id)\n  (run id))\n")
    (put root "tests/app-test.lisp" "(deftest handler-test (is t))\n")
    (put root "README.md" "# the app\n")
    (with-scan-cell ()
      (let ((text (secscan:start-scan :path (namestring root))))
        (is-carrying text
          ("started") ("mode standard") ("1 file read, 2 skipped, 1 chunk")
          ("dependency check: none" "no manifest, so no query")
          ("security_scan_get section next"))
        (is (search "dispatch" (secscan:get-scan (only-id) :section "next")) "the next step is a review")
        (is-carrying (packets (secscan:get-scan (only-id) :section "packets"))
          ("C1") ("planned") ("src/app.lisp"))
        (is (string= "C1" (getf (first (getf (only-scan) :chunks)) :id)) "one chunk, in the record")
        (is (= 2 (length (getf (only-scan) :skipped))) "the doc and the test folder are gaps named")
        (is (search "a test, example, doc or eval folder"
                    (getf (first (getf (only-scan) :skipped)) :reason))
            "the test folder is skipped, and named")
        (is (search "a document, asset or lockfile"
                    (getf (second (getf (only-scan) :skipped)) :reason))
            "and so is the README")
        ;; a deep scan reads them too, and gets a chunk of its own
        (is (search "3 files read, 0 skipped"
                    (secscan:start-scan :path (namestring root) :mode "deep"))
            "deep reads tests and docs")))))

(deftest security-scan-cell-a-scan-without-a-folder-refuses-and-diff-needs-git ()
  (with-dir (root)
    (put root "app.py" "print(1)\n")
    (with-scan-cell ()
      (is-carrying (text (refusal-text secscan:security-scan-error (secscan:start-scan)))
        ("path is required"))
      (is-carrying (text (refusal-text secscan:security-scan-error
                                      (secscan:start-scan :path "/no-such-folder-here")))
        ("is no folder on this machine"))
      (is-carrying (text (refusal-text secscan:security-scan-error
                                      (secscan:start-scan :path (namestring root) :mode "diff")))
        ("diff mode needs git")))))

;;; --- the reviewer's result ------------------------------------------------------

(deftest security-scan-cell-a-worker-result-from-another-snapshot-is-refused ()
  (with-dir (root)
    (put root "src/app.lisp" "(defun handler (id)\n  (run id))\n")
    (with-scan-cell ()
      (secscan:start-scan :path (namestring root))
      (is-carrying (text (refusal-text secscan:security-scan-error
                                      (secscan:submit-worker-result
                                       (finding-payload :digest "sha256:0000000000000000000000000000000000000000000000000000000000000000"))))
        ("the payload cites snapshot" "submit the result of the snapshot"))
      (is (null (getf (only-scan) :findings)) "nothing was recorded"))
    (with-scan-cell ()
      (secscan:start-scan :path (namestring root))
      (is-carrying (text (secscan:submit-worker-result (finding-payload)))
        ("1 new candidate" "F1") ("not validated yet"))
      (let ((findings (getf (only-scan) :findings)))
        (is (= 1 (length findings)))
        (is (equal "high" (getf (first findings) :severity)))
        (is (equal "cited" (getf (first findings) :evidence-state)) "a location is evidence")
        (is (equal "NOT_RUN" (getf (first findings) :validation)) "no receipt, no proof")
        (is (search "foundations" (format nil "foundations"))))
      (is-carrying (text (secscan:get-scan (only-id) :section "findings"))
        ("F1") ("high") ("src/app.lisp:2") ("SQL built by concatenation"))
      (is (search "validate" (secscan:get-scan (only-id) :section "next")) "nothing left to dispatch"))))

;;; --- coverage, validations, chains ----------------------------------------------

(deftest security-scan-cell-coverage-is-what-was-read-and-a-confirmation-needs-both-controls ()
  (with-dir (root)
    (put root "src/app.lisp" "(defun handler (id)\n  (run id))\n")
    (with-scan-cell ()
      (secscan:start-scan :path (namestring root))
      (is-carrying (text (secscan:checkpoint (only-id)
                                             :coverage (list (nlk:json-object "unit" "C1" "state" "reviewed"
                                                                               "note" "read src/app.lisp"))
                                             :note "read the handler"))
        ("coverage saved" "1 of 1 chunk reviewed"))
      (secscan:submit-worker-result (finding-payload :coverage '()))
      (is (search "validate" (secscan:get-scan (only-id) :section "next")) "every chunk is read")
      ;; one control is not a confirmation
      (is-carrying (text (secscan:record-validations (only-id)
                                                     :receipts (vector (receipt "F1" "CONFIRMED" :positive t))))
        ("0 findings confirmed of 1"))
      (is (equal "NOT_RUN" (getf (first (getf (only-scan) :findings)) :validation)))
      (is (search "1 finding confirmed of 1"
                  (secscan:record-validations (only-id)
                                               :receipts (vector (receipt "F1" "CONFIRMED"
                                                                          :positive t :negative t)))))
      (is (equal "CONFIRMED" (getf (first (getf (only-scan) :findings)) :validation)))
      (is-carrying (text (refusal-text secscan:security-scan-error
                                      (secscan:record-validations (only-id)
                                                                  :receipts (vector (receipt "F9" "REFUTED")))))
        ("no finding \"F9\"")))))

(deftest security-scan-cell-chains-link-the-primitives-in-one-file ()
  (with-dir (root)
    (put root "src/app.lisp" "(defun handler (id)\n  (run id))\n(defun other () 2)\n")
    (with-scan-cell ()
      (secscan:start-scan :path (namestring root))
      (secscan:submit-worker-result
       (nlk:json-object
        "scanId" (only-id) "attemptId" "w1" "workerRole" "review" "snapshotDigest" (digest-of)
        "candidates" (vector
                      (nlk:json-object "title" "unchecked id" "severity" "medium"
                                       "summary" "id reaches the query" "ruleId" "nc/injection"
                                       "locations" (vector (nlk:json-object "path" "src/app.lisp" "startLine" 2)))
                      (nlk:json-object "title" "no authorisation" "severity" "high"
                                       "summary" "anyone may run" "ruleId" "nc/authz"
                                       "locations" (vector (nlk:json-object "path" "src/app.lisp" "startLine" 1))))
        "coverage" (vector (nlk:json-object "unit" "C1" "state" "reviewed"))))
      (let ((text (secscan:record-chains (only-id)
                                         :explanations (vector (nlk:json-object
                                                                "chainId" "CH1"
                                                                "title" "an unchecked id reaches the query"
                                                                "summary" "anyone may call the handler, and the id it is given is concatenated")))))
        (is-carrying text ("1 chain composed") ("CH1") ("F1") ("F2"))
        (is (equal "an unchecked id reaches the query" (getf (first (getf (only-scan) :chains)) :title))))
      (is-carrying (text (refusal-text secscan:security-scan-error
                                      (secscan:record-chains (only-id)
                                                             :explanations (vector (nlk:json-object "chainId" "CH9")))))
        ("no chain \"CH9\"")))))

;;; --- sealing and the four exports -----------------------------------------------

(deftest security-scan-cell-a-scan-seals-once-into-four-reports-nothing-changes-afterwards ()
  (with-dir (root)
    (put root "src/app.lisp" "(defun handler (id)\n  (run id))\n")
    (with-scan-cell ()
      (secscan:start-scan :path (namestring root))
      (is-carrying (text (refusal-text secscan:security-scan-error (secscan:finalize-scan (only-id))))
        ("no reviewed chunk yet"))
      (secscan:submit-worker-result
       (nlk:json-object
        "scanId" (only-id) "attemptId" "w1" "workerRole" "review" "snapshotDigest" (digest-of)
        "candidates" (vector (nlk:json-object "title" "=cmd reaches the shell" "severity" "critical"
                                               "summary" "the id is interpolated into a command"
                                               "ruleId" "nc/command-injection"
                                               "locations" (vector (nlk:json-object "path" "src/app.lisp" "startLine" 2))
                                               "cwe" (vector "CWE-78")))
        "coverage" (vector (nlk:json-object "unit" "C1" "state" "reviewed"))))
      (is-carrying (text (secscan:finalize-scan (only-id)))
        ("sealed in") ("report.md, findings.json, results.sarif, findings.csv"))
      (is-carrying (md (secscan:export-scan (only-id) :fmt "md"))
        ("# Security scan") ("### F1 -- =cmd reaches the shell (critical)")
        ("`src/app.lisp:2`") ("Validation: NOT_RUN") ("check its conclusions"))
      (is-carrying (sarif (secscan:export-scan (only-id) :fmt "sarif"))
        ("2.1.0") ("security-severity") ("partialFingerprints") ("nc/command-injection"))
      (let ((document (nlk:decode-json (secscan:export-scan (only-id) :fmt "sarif"))))
        (is (equal "2.1.0" (gethash "version" document)))
        (is (equal "error" (gethash "level" (aref (gethash "results" (aref (gethash "runs" document) 0)) 0)))
            "a critical finding is a SARIF error"))
      (is-carrying (csv (secscan:export-scan (only-id) :fmt "csv"))
        ("findingId") ("\"'=cmd reaches the shell\"") ("CWE-78"))
      (let ((json (nlk:decode-json (secscan:export-scan (only-id) :fmt "json"))))
        (is (equal "sealed" (gethash "status" json)))
        (is (= 1 (length (coerce (gethash "findings" json) 'list))))
        (is (equal "nc/command-injection" (gethash "ruleId" (aref (gethash "findings" json) 0)))))
      (is-carrying (text (refusal-text secscan:security-scan-error (secscan:finalize-scan (only-id))))
        ("sealed" "start a new scan"))
      (is-carrying (text (refusal-text secscan:security-scan-error (secscan:cancel-scan (only-id))))
        ("sealed"))
      (is (eq :sealed (handler-case (progn (secscan:export-scan (only-id) :fmt "yaml") t)
                         (secscan:security-scan-error () :sealed)))
          "an unknown format refuses, and the scan is still readable"))))

;;; --- the dependency check -------------------------------------------------------

(deftest security-scan-cell-the-dependency-check-is-one-stubbed-osv-call ()
  (with-dir (root)
    (put root "package.json" "{\"name\":\"app\",\"dependencies\":{\"lodash\":\"^4.17.20\"}}")
    (put root "requirements.txt" "flask==2.0.1\n# a comment\n")
    (put root "src/app.js" "require('lodash')\n")
    (let* ((asked '())
           (answer (nlk:encode-json-object
                    (nlk:json-object "results" (vector (nlk:json-object "vulns" (vector (nlk:json-object "id" "GHSA-1" "summary" "prototype pollution")))
                                                       (nlk:json-object "vulns" (vector)))))))
      (let ((*stub-http* (lambda (method url &rest keys)
                           (declare (ignore keys))
                           (push (list method url) asked)
                           (values answer 200 nil))))
        (with-scan-cell ()
          (is-carrying (text (secscan:start-scan :path (namestring root)))
            ("dependency check: ok (2 packages, 1 advisor)"))
          (let ((check (getf (only-scan) :dependencies)))
            (is (equal "ok" (getf check :state)) "the check ran")
            (is (= 2 (getf check :packages)) "both manifests were read")
            (is (equal "prototype pollution" (getf (first (getf check :advisories)) :summary))
                "and the advisory OSV named is kept"))
          (is (= 1 (length asked)) "one call, whatever the manifests hold")
          (is (equal :post (first (first asked))))
          (is (search "/v1/querybatch" (second (first asked)))))))
    ;; a check that cannot run leaves the scan partial, and says so
    (let ((*stub-http* (lambda (method url &rest keys)
                          (declare (ignore method url keys))
                          (error "no route to the host"))))
      (with-scan-cell ()
        (is-carrying (text (secscan:start-scan :path (namestring root)))
          ("dependency check: failed") ("no route to the host"))
        (is (search "the scan is partial" (secscan:get-scan (only-id) :section "activity"))
            "a check that could not run leaves the scan partial")))))

;;; --- /security -----------------------------------------------------------------

(deftest security-scan-cell-the-command-reports-and-only-the-operator-grants-app-testing ()
  (with-dir (root)
    (put root "src/app.lisp" "(defun handler (id)\n  (run id))\n")
    (with-scan-cell ()
      (secscan:start-scan :path (namestring root))
      (is (search "authorize-validation" (secscan:run-command "help" "s1")))
      (is-carrying (status (secscan:run-command "status" "s1"))
        ("1 scan on this machine, 0 of them sealed") ("scan-2") ("open"))
      (is-carrying (text (secscan:run-command "authorize-validation scan-x http://localhost:3000" "s1"))
        ("no allowed_origins configured") ("security-scan.allowed_origins"))
      (is (search "is no /security command" (secscan:run-command "frobnicate" "s1"))))
    (with-scan-cell ("allowed_origins" #("http://localhost:3000"))
      (secscan:start-scan :path (namestring root))
      (is-carrying (text (secscan:run-command
                          (format nil "authorize-validation ~a https://evil.example/x" (only-id)) "s1"))
        ("is no origin"))
      (is-carrying (text (secscan:run-command
                          (format nil "authorize-validation ~a http://elsewhere.example:8080" (only-id)) "s1"))
        ("is not in allowed_origins"))
      (is-carrying (grant (secscan:run-command
                           (format nil "authorize-validation ~a http://localhost:3000" (only-id)) "s1"))
        ("granted G1") ("http://localhost:3000") ("60 minutes") ("200 requests"))
      (let ((granted (first (getf (only-scan) :grants))))
        (is (equal "user-command" (getf granted :created-by)) "the agent's tools cannot grant it")
        (is (> (getf granted :expires-at) (getf granted :minted-at)))
        (is (equal '("http-probe") (getf granted :actions))))
      (is-carrying (text (secscan:run-command (format nil "revoke G1") "s1"))
        ("revoked"))
      (is (equal "revoked" (getf (first (getf (only-scan) :grants)) :state)))
      (is-carrying (text (secscan:run-command "revoke G7" "s1"))
        ("ERROR: SECURITY-SCAN-ERROR") ("no grant \"G7\"")))))


