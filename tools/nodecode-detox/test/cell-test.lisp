;;;; cell-test.lisp --- what the firewall does to a big result, and what it gives back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that a large result becomes a report naming the
;;;; record it was kept as, and that the record holds the text and answers
;;;; exact lines by the numbers the report printed; that a small result, an
;;;; excluded tool, a media payload and a report already made pass through
;;;; untouched; that a report which cannot come out small enough leaves the
;;;; original standing; that a store which cannot be written loses nothing;
;;;; that known secrets are redacted before anything is kept; that search,
;;;; status and purge answer what they say; and that an id which is not an
;;;; evidence id refuses instead of reaching the filesystem.

(in-package #:nodecode.test)

;;; --- lifecycle --------------------------------------------------------------

(define-cell-lifecycle-tests "detox"
  (:hooks :tool)
  (:help :detox)
  (:command "detox")
  (:running (is (= 6000 (nodecode-detox::setting :min-size)) "min_size defaults to 6000")
            (is (= 55 (nodecode-detox::setting :max-report-percent)) "a report to 55 percent")
            (is (stringp (detox:status)) "status answers with nothing kept"))
  (:refused ("min_size" 10) ("head_lines" 0) ("max_report_percent" 1))
  (:idle nodecode-detox:detox-error
         (detox:status)
         (detox:inspect-evidence "ev_4f1b8a92e10c")))

;;; --- the interception --------------------------------------------------------

(deftest detox-cell-a-large-result-becomes-a-report-and-is-kept ()
  (with-detox ()
    (let* ((text (sample-log))
           (answer (intercept-text "sh" text))
           (id (report-id answer)))
      (is (< (length answer) (length text)) "the report is shorter than the output")
      (is-present id "the report names the record it was kept as"
        (is (equal text (nodecode-detox::record-text id)) "the record holds the whole text"))
      (is-carrying answer
        "[DETOX: sh]"
        ("(tests)" "the shape of the text is named")
        "Original output"
        "--- Head (lines 1-5) ---"
        "--- Retrieval ---"
        "detox_inspect(evidence_id="))))

(deftest detox-cell-a-number-in-the-report-is-the-originals-line ()
  (with-detox ()
    (let* ((text (sample-log))
           (answer (intercept-text "sh" text))
           (id (report-id answer))
           (bad-line (line-of text "error: cannot find symbol"))
           (range (detox:inspect-evidence id :start bad-line :end bad-line)))
      (is (= 43 bad-line) "the error sits on the line the fixture put it on")
      (let ((wanted (format nil "~4d [ERROR] | error: cannot find symbol" bad-line))
            (asked (format nil "~4d | error: cannot find symbol" bad-line)))
        (is (search wanted answer) "the diagnostic line carries its original number")
        (is (search asked range) "retrieval answers that line under the same number")))))

(deftest detox-cell-a-small-result-is-left-exactly-as-it-was ()
  (with-detox ()
    (let ((small "three failures, one fix: that is all this line says"))
      (is (equal small (intercept-text "sh" small)))
      (is (= 0 (length (detox::evidence-ids))) "and nothing was kept"))))

(deftest detox-cell-excluded-and-own-tools-pass-through ()
  (with-detox ("excluded_tools" (vector "noisy_tool"))
    (let* ((text (sample-log))
           (answer (intercept-text "sh" text))
           (id (report-id answer)))
      (is (equal text (intercept-text "detox_status" text)) "its own tool is never compacted")
      (is (equal text (intercept-text "write_file" text)) "nor a file editor")
      (is (equal text (intercept-text "noisy_tool" text)) "nor what the section excludes")
      (is-present id "a tool nobody excluded still is"
        (is (= 1 (length (detox::evidence-ids))) "and exactly one record was kept")))))

(deftest detox-cell-an-image-or-a-report-passes-through ()
  (with-detox ()
    (let ((image (concatenate 'string "data:image/png;base64," (sample-log))))
      (is (equal image (intercept-text "sh" image)) "a data URI is never touched"))
    (let* ((text (sample-log))
           (once (intercept-text "sh" text))
           (twice (intercept-text "sh" once)))
      (is (equal once twice) "a result already carrying a report is not wrapped again")
      (is (= 1 (length (detox::evidence-ids))) "and nothing was kept the second time"))))

(deftest detox-cell-a-report-that-cannot-come-out-small-enough-leaves-the-original ()
  (with-detox ("max_report_percent" 10)
    (let* ((text (error-log))
           (answer (intercept-text "sh" text)))
      (is (> (length text) 6000) "the fixture is over min_size")
      (is (equal text answer) "the original stands")
      (is (= 1 (length (detox::evidence-ids))) "though the text was kept, as the original kept it"))))

(deftest detox-cell-a-store-that-cannot-be-written-loses-nothing ()
  (with-detox-home
    (ensure-directories-exist (nlk:home))
    ;; a file where the folder should be: every write under it fails
    (with-open-file (out (nlk:home "detox") :direction :output :if-exists :supersede
                                            :if-does-not-exist :create)
      (write-string "not a folder" out))
    (with-cell-stop ((detox-start))
      (let ((text (sample-log)))
        (is (equal text (intercept-text "sh" text)) "the answer is the original, untouched")))))

(deftest detox-cell-known-secrets-are-redacted-before-anything-is-kept ()
  (with-detox ()
    (let* ((text (concatenate 'string (sample-log)
                               "using sk-abcdefghijklmnopqrstuvwx and ghp_abcdefghijklmnopqrstuvwxyz0123\n"))
           (answer (intercept-text "sh" text))
           (id (report-id answer)))
      (is-present id "the text was kept"
        (let ((kept (nodecode-detox::record-text id)))
          (is (not (null (search "[redacted]" kept))) "the token's place is marked")
          (is (null (search "sk-abcdefghijklmnopqrstuvwx" kept)) "and the token itself is gone")
          (is (null (search "ghp_abcdefghijklmnopqrstuvwxyz0123" kept)) "each shape the patterns know"))))))

;;; --- reading it back ---------------------------------------------------------

(deftest detox-cell-search-finds-a-phrase-with-its-line ()
  (with-detox ()
    (let* ((text (sample-log :needle "the build is green"))
           (answer (intercept-text "sh" text))
           (id (report-id answer))
           (found (detox:search-evidence "build is green"))
           (line (line-of text "the build is green"))
           (scoped (detox:search-evidence "build is green" :evidence-id id))
           (lower (detox:search-evidence "BUILD IS GREEN"))
           (exact (detox:search-evidence "BUILD IS GREEN" :case-sensitive t)))
      (is (integerp line) "the phrase is in the fixture")
      (let ((where (format nil "~a:~d:" id line))
            (which (format nil "~d: the build is green" line))
            (sorted (detox:search-evidence "build is green" :evidence-id id)))
        (is (search where found) "the match names the record and the line")
        (is (search which found) "the original number rides the match")
        (is (equal found sorted) "scoped to the one record, the same answer"))
      (is (equal found scoped) "scoped to the one record, the same answer")
      (is-carrying lower ("the build is green" "matching ignores case unless asked"))
      (is-carrying exact "nothing matches"))))

(deftest detox-cell-status-counts-what-it-kept-and-the-command-says-so ()
  (with-detox ()
    (let* ((text (sample-log))
           (id (report-id (intercept-text "sh" text)))
           (status-text (detox:status))
           (as-command (detox:run-slash "status" "s-1"))
           (unknown (detox:run-slash "nonsense" "s-1")))
      (is-carrying status-text
        "detox: 1 record"
        ("characters in" "the characters in are counted")
        "saved")
      (is-carrying as-command "detox: 1 record")
      (is-carrying unknown
        ("does not know")
        ("inspect ID" "the usage is spelled out"))
      (is-present id "there is a record to read"
        (let ((shown (detox:run-slash (format nil "inspect ~a" id) "s-1"))
              (hits (detox:run-slash (format nil "search green ~a" id) "s-1")))
          (is-carrying shown ("lines 1-" "the whole record is the range asked for"))
          (is (search (format nil "~a:" id) hits) "the command answers the record"))))))

(deftest detox-cell-purge-drops-what-is-old-and-a-delete-takes-both-files ()
  (with-detox ()
    (let ((id (report-id (intercept-text "sh" (sample-log)))))
      (is-present id "a record to drop"
        (is (not (null (probe-file (nodecode-detox::record-path id)))) "the text was kept")
        (is-carrying (purged (detox:purge))
          ("0 records older than 30 days dropped")
          ("1 kept" "the fresh record is not old"))
        (is (nodecode-detox::delete-record id) "a record goes as a whole")
        (is (null (probe-file (nodecode-detox::record-path id "meta"))) "metadata and all")
        (is (= 0 (length (detox::evidence-ids))))))))

(deftest detox-cell-an-id-that-is-not-one-refuses-instead-of-reaching-the-disk ()
  (with-detox ()
    (is (signals-error nodecode-detox:detox-error
                       (detox:inspect-evidence "../../etc/passwd")))
    (is (signals-error nodecode-detox:detox-error (detox:search-evidence "")))
    (is (signals-error nodecode-detox:detox-error
                       (detox:search-evidence "here" :evidence-id "ev_000000000000")))
    (let ((said (handler-case (detox:inspect-evidence "../../etc/passwd")
                  (nodecode-detox:detox-error (condition) (princ-to-string condition)))))
      (is-carrying said ("is not an evidence id" "the refusal names what it wanted")))))
