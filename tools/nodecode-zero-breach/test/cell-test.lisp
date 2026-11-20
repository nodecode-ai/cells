;;;; cell-test.lisp --- the lifecycle; the argv; the gap/hit split; the last
;;;; report; the description; and one real run of the vendored engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own work: that a scan asks the engine
;;;; for a bounded, offline, report-writing run in the scope the caller named;
;;;; that a gap stays a gap in the words (a clean result is never widened);
;;;; that a hit keeps the engine's own family and tier; that a run that wrote
;;;; no report is said in words rather than answered empty; that the newest
;;;; report is the one re-read; that --describe is read as the engine wrote
;;;; it; and, at the end, that the vendored engine really reports a planted
;;;; indicator out of a browser profile it is pointed at.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "zero-breach"
  (:config "scope" "quick")
  (:help :zero-breach)
  (:command "zero-breach")
  (:running (is (equal "quick" (nodecode-zero-breach::setting :scope)) "the section's default scope is the running one")
            (is (plusp (length (namestring (nodecode-zero-breach::engine-directory)))) "the engine directory answers")))

;;; --- the argv the engine is handed ---------------------------------------------------

(deftest zero-breach-cell-a-scan-asks-for-a-bounded-offline-run ()
  (with-zero-breach ()
    (nodecode-zero-breach:scan-answer :scope "standard" :timeout 42)
    (let ((argv (first *zb-calls*)))
      (is (member "python3" argv :test #'string=) "the interpreter is the program the engine runs")
      (is (some (lambda (word) (search "zero-breach-toolkit.py" word)) argv) "the vendored engine's own entry point, by name")
      (is (member "--scan-type" argv :test #'string=) "the scope is named")
      (is (equal "default" (zb-argument argv "--scan-type")) "standard is the engine's default scan")
      (is (member "--no-feeds" argv :test #'string=) "the bundled set alone, so a scan contacts nothing")
      (is (member "--no-banner" argv :test #'string=) "no banner in the captured output")
      (is (member "--no-menu" argv :test #'string=) "no menu after the scan")
      (is (search "zero-breach-" (or (zb-argument argv "--json") "")) "the report is written to the cell's own name")
      (is (probe-file (zb-argument argv "--json")) "the report was written where the cell asked")
      (is (member "42" argv :test #'string=) "the caller's timeout bounds the run"))))

(deftest zero-breach-cell-a-quick-scan-says-what-it-skipped ()
  (with-zero-breach ()
    (nodecode-zero-breach:scan-answer :scope "quick")
    (let ((argv (first *zb-calls*)))
      (is (member "--quick" argv :test #'string=) "quick is asked for as --quick")
      (is (not (member "--scan-type" argv :test #'string=)) "and not as a scan type"))))

;;; --- the gap is never the clean answer ------------------------------------------------

(deftest zero-breach-cell-a-gap-is-not-a-clean-result ()
  (with-zero-breach ()
    (setf *zb-status* "NO_KNOWN_INDICATORS_WITH_GAPS"
          *zb-gaps* '("browser/chrome:profiles: the profile could not be read"
                      "hash/file-fingerprints: skipped by --quick"))
    (let ((text (nodecode-zero-breach:scan-answer :scope "quick")))
      (is (search "some checks could not run" text) "the verdict says so in words")
      (is (search "NO_KNOWN_INDICATORS_WITH_GAPS" text) "and the raw status is kept")
      (is (search "2 check(s) could not run" text))
      (is (search "the profile could not be read" text) "the engine's own sentence for the gap")
      (is (search "skipped by --quick" text))
      (is (not (search "every check in scope ran" text)) "the clean wording is not used"))))

(deftest zero-breach-cell-the-engine-s-caveats-ride-every-answer ()
  (with-zero-breach ()
    (let ((text (nodecode-zero-breach:scan-answer :scope "quick")))
      (is (search "not a clean bill of health" text) "must_relay travels with the answer")
      (is (search "Nothing was cleared in those places" text)))))

(deftest zero-breach-cell-a-hit-keeps-the-engine-s-family-and-tier ()
  (with-zero-breach ()
    (setf *zb-hits* '(("awake-wood.io" "Paragon Graphite" "infrastructure" "/home/x/History"))
          *zb-status* "INDICATORS_FOUND")
    (let ((text (nodecode-zero-breach:scan-answer :scope "quick")))
      (is (search "known indicators were found" text))
      (is (search "awake-wood.io" text))
      (is (search "Paragon Graphite" text) "the engine's family name, not one of ours")
      (is (search "infrastructure" text) "and its tier"))))

(deftest zero-breach-cell-a-run-that-wrote-nothing-is-said-in-words ()
  (with-zero-breach ()
    (setf nodecode-zero-breach::*run*
          (lambda (argv) (declare (ignore argv)) (values "" "python3: not found" 127)))
    (let ((text (handler-case (nodecode-zero-breach:scan-answer)
                  (error (e) (princ-to-string e)))))
      (is (search "wrote no report" text))
      (is (search "python3: not found" text) "the engine's own error text, not a guess"))))

;;; --- the newest report, and the description ------------------------------------------

(deftest zero-breach-cell-last-reads-the-newest-report ()
  (with-zero-breach ()
    (let ((directory (namestring (nodecode-zero-breach::report-directory))))
      (with-open-file (out (merge-pathnames "zero-breach-1-1.full.json" directory)
                           :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (zb-report-text :status "NO_KNOWN_INDICATORS" :hits nil :gaps nil) out))
      (sleep 1.1)
      (with-open-file (out (merge-pathnames "zero-breach-2-2.full.json" directory)
                           :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (zb-report-text :status "NO_KNOWN_INDICATORS_WITH_GAPS"
                                      :hits nil
                                      :gaps '("hash/file-fingerprints: skipped"))
                      out))
      (let ((text (nodecode-zero-breach:last-answer :summary-only t)))
        (is (search "zero-breach-2-2.full.json" text) "the newest file of the two")
        (is (search "some checks could not run" text) "and its verdict, not the older one's")))))

(deftest zero-breach-cell-describe-reads-the-engine-s-own-list ()
  (with-zero-breach ()
    (let ((text (nodecode-zero-breach:describe-text)))
      (is (search "browser" text))
      (is (search "cookies, caches" text) "what the check reads")
      (is (search "cannot see: a profile this account cannot read" text) "and what it cannot")
      (is (search "3: checks were incomplete" text) "the exit codes in the engine's words")
      (is (search "Never call a gap clean" text) "the tool's own rules for an assistant"))))

(deftest zero-breach-cell-describe-narrows-to-one-check ()
  (with-zero-breach ()
    (let ((text (nodecode-zero-breach:describe-text :module "phone")))
      (is (search "phones-backup" text))
      (is (not (search "cookies, caches" text)) "the other check is not listed"))))

;;; --- the real engine, on a browser profile a test planted -----------------------------

(defun zb-python ()
  "A python3 this machine has, as a path, or NIL."
  (let ((found (ignore-errors
                 (string-trim '(#\Space #\Newline)
                              (uiop:run-program '("sh" "-c" "command -v python3")
                                                :output :string :ignore-error-status t)))))
    (and found (plusp (length found)) found)))

(defun zb-environment (root config)
  "The environment the engine's child gets: a scratch HOME, a pointed XDG_CONFIG_HOME."
  (list (cons :PATH (or (uiop:getenv "PATH") "/usr/bin:/bin"))
        (cons :HOME (namestring root))
        (cons :XDG_CONFIG_HOME (namestring config))
        (cons :LANG "C.UTF-8")))

(defun zb-plant-profile (config-root)
  "A Chromium profile under CONFIG-ROOT whose history visited a fixture domain."
  (let ((script "import sqlite3,sys,os
p=os.path.join(sys.argv[1],'chromium','Default')
os.makedirs(p,exist_ok=True)
db=sqlite3.connect(os.path.join(p,'History'))
db.execute('CREATE TABLE urls(url TEXT,title TEXT,last_visit_time INTEGER)')
db.execute('INSERT INTO urls VALUES(?,?,?)',('https://zb-fixture.example/','',13000000000000000))
db.commit()"))
    (uiop:run-program (list (zb-python) "-c" script (namestring config-root))
                       :output :string :error-output :string :ignore-error-status t)
    config-root))

(deftest zero-breach-cell-the-vendored-engine-reports-a-planted-indicator ()
  (let ((python (zb-python)))
    (if (null python)
        (is t "no python3 on this machine: the real-engine run is skipped")
        (let* ((root (zb-scratch-directory))
               (config (merge-pathnames "config/" root))
               (feed (merge-pathnames "fixture.txt" root)))
          (unwind-protect
               (progn
                 (ensure-directories-exist feed)
                 (zb-plant-profile config)
                 (with-open-file (out feed :direction :output :if-exists :supersede
                                            :if-does-not-exist :create)
                   (write-line "zb-fixture.example	infrastructure	primary	fixture	Test Family" out))
                 (with-zero-breach ("python" python
                                    "iocs" (namestring feed)
                                    "report_directory" (namestring (merge-pathnames "reports/" root)))
                   (let ((nodecode-zero-breach::*environment* (zb-environment root config))
                         (nodecode-zero-breach::*run* #'nodecode-zero-breach::run-command))
                     (let* ((report (nodecode-zero-breach:gather-scan :scope "browser" :timeout 300))
                            (hit (first (getf report :hits))))
                       (is (equal "INDICATORS_FOUND" (getf report :status)) "the engine's own verdict on the planted indicator")
                       (is (equal "zb-fixture.example" (getf hit :indicator)))
                       (is (equal "Test Family" (getf hit :family)) "the family the feed named")))))
            (ignore-errors (uiop:delete-directory-tree root :validate t)))))))
