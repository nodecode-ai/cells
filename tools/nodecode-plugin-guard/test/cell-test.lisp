;;;; cell-test.lisp --- what the scan finds, what it refuses, what it never does.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Fixtures are real trees in a scratch directory: a file put where a test
;;;; names it, the cell started on a section built from pairs. Nothing is
;;;; stubbed, because nothing is dialled: a scan touches the filesystem and
;;;; no more, which is the point of it.

(in-package #:nodecode.test)

(define-test-slice "plugin-guard" "PLUGIN-GUARD-CELL-"
  :start nodecode-plugin-guard:start-cell)

(defvar *calls* 0
  "How many tool calls the fixture has made, for their call ids.")

(defvar *show-tool-answers* nil
  "Whether TOOL-TEXT echoes what the tool answered.")

;;; --- the fixture -----------------------------------------------------------------

(defmacro with-tree ((root) &body body)
  "BODY with ROOT a fresh scratch folder, removed on unwind."
  `(with-temp-directory (,root "plugin-guard")
     ,@body))

(defun put-file (root path text)
  "Write TEXT to PATH below ROOT, making the directories on the way, and => the pathname."
  (let ((full (merge-pathnames path root)))
    (ensure-directories-exist full)
    (with-open-file (out full :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string text out))
    full))

(defun put-bytes (root path octets)
  "Write OCTETS to PATH below ROOT, as a binary file."
  (let ((full (merge-pathnames path root)))
    (ensure-directories-exist full)
    (with-open-file (out full :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    full))

(defun json-answer (&rest pairs)
  "PAIRS as the JSON text a tool call carries."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun tool-text (&rest arguments)
  "The text plugin_guard_scan answers, called as the provider's call is."
  (let ((text (princ-to-string
               (nle::message-content
                (execute-wire-call (format nil "plugin-guard-call-~d" (incf *calls*))
                                    "plugin_guard_scan"
                                    (if arguments (apply #'json-answer arguments) "{}"))))))
    (when *show-tool-answers*
      (format t "~&; plugin_guard_scan => ~a~%" text))
    text))

(defun start-on (&rest pairs)
  "PLUGIN-GUARD-START on the pairs a test names, as one section."
  (apply #'plugin-guard-start pairs))

(defun scan-text (path &rest pairs)
  "The tool's report on PATH, the cell started on PAIRS, stopped after."
  (with-cell-stop ((apply #'start-on pairs))
    (tool-text "path" (namestring path))))

(defun risky-tree (root)
  "A tree a review has something to say about: a declaration, a license, a
policy, tests, a module that shells out and reads a key, and a committed token."
  (put-file root "cell.asd" "(defsystem \"risky\" :depends-on (\"nodecode\"))
")
  (put-file root "LICENSE" "MIT License
")
  (put-file root "SECURITY.md" "# Security
")
  (put-file root "test/cell-test.lisp" "(deftest risky-ok (is t))
")
  (put-file root "run.py"
            "import subprocess
subprocess.run(['echo', 'hi'])
KEY = '~/.ssh/id_rsa'
TOKEN = 'ghp_abcdefghijklmnopqrstuvwxyz'
")
  root)

(defun clean-tree (root)
  "A tree a review has nothing to say about."
  (put-file root "cell.asd" "(defsystem \"clean\" :depends-on (\"nodecode\"))
")
  (put-file root "LICENSE" "MIT License
")
  (put-file root "SECURITY.md" "# Security
")
  (put-file root "test/cell-test.lisp" "(deftest clean-ok (is t))
")
  (put-file root "cell.lisp" "(defun hello () (format nil \"hi\"))
")
  root)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "plugin-guard"
  (:help :plugin-guard)
  (:command "plugin-guard")
  (:running
   (is (nle::find-tool "plugin_guard_scan") "the review tool is registered")
   (is (equal "high" (nodecode-plugin-guard::setting :min-severity)) "the report judges at high")
   (is (= 20 (nodecode-plugin-guard::setting :max-findings)) "twenty findings to a report"))
  (:stopped
   (is (null (nle::find-tool "plugin_guard_scan")) "stop takes the tool out of the catalog"))
  (:refused ("min_severity" "severe") ("max_findings" 0))
  (:idle nodecode-plugin-guard:plugin-guard-error
         (nodecode-plugin-guard::scan "no-such-folder")))

;;; --- what a scan says ---------------------------------------------------------------

(deftest plugin-guard-cell-reports-a-risky-tree-by-rule-path-and-line ()
  (with-tree (root)
    (risky-tree root)
    (let ((text (scan-text root "min_severity" "high")))
      (is-carrying (text text)
        "PG103 run.py:2"
        "process execution: subprocess."
        "PG104 run.py:3"
        "sensitive path reference"
        "CRITICAL"
        "PG201 run.py:4"
        "possible GitHub token"
        "FAIL at high"
        "read as text, never run"))))

(deftest plugin-guard-cell-a-clean-tree-passes ()
  (with-tree (root)
    (clean-tree root)
    (let ((text (scan-text root)))
      (is-carrying (text text)
        "0 findings"
        "ok: nothing at or above high"
        (:absent "FAIL" "and nothing reached the threshold")))))

(deftest plugin-guard-cell-the-same-tree-reads-the-same ()
  (with-tree (root)
    (risky-tree root)
    (let ((first (nodecode-plugin-guard::scan-directory (namestring root) :max-findings 50))
          (again (nodecode-plugin-guard::scan-directory (namestring root) :max-findings 50)))
      (is (equal (nodecode-plugin-guard::report-text first)
                 (nodecode-plugin-guard::report-text again)) "two runs agree, byte for byte")
      (is (equal (mapcar (lambda (one) (getf one :rule)) (getf first :findings))
                 (mapcar (lambda (one) (getf one :rule)) (getf again :findings)))))))

(deftest plugin-guard-cell-a-binary-file-counts-as-unread ()
  (with-tree (root)
    (clean-tree root)
    (put-bytes root "blob.bin" (make-array 8 :element-type '(unsigned-byte 8)
                                              :initial-contents '(0 1 2 3 4 5 6 7)))
    (let ((text (scan-text root)))
      (is (search "1 unread" text) "the binary is counted, never parsed"))))

;;; --- the section changes the report --------------------------------------------------

(deftest plugin-guard-cell-the-threshold-and-exclude-change-the-report ()
  (with-tree (root)
    (risky-tree root)
    (put-file root "requirements.txt" "requests>=2
")
    (let ((text (scan-text root "min_severity" "low" "exclude" (vector "PG103"))))
      (is-carrying (text text)
        "FAIL at low"
        "PG203 requirements.txt:1"
        "no upper bound"
        (:absent "PG103" "the excluded rule is left out")))))

(deftest plugin-guard-cell-max-findings-bounds-the-list ()
  (with-tree (root)
    (risky-tree root)
    (let* ((result (nodecode-plugin-guard::scan-directory (namestring root) :max-findings 1))
           (text (nodecode-plugin-guard::report-text result)))
      (is (= 1 (length (getf result :findings))) "one finding printed")
      (is (plusp (getf result :total)) "of a longer list")
      (is (search "more finding" text) "and the report says how many were left"))))

;;; --- dependencies --------------------------------------------------------------------

(deftest plugin-guard-cell-dependencies-are-read-for-pins-and-bounds ()
  (with-tree (root)
    (clean-tree root)
    (put-file root "requirements.txt" "requests>=2

git+https://example.invalid/x.git
")
    (let ((text (scan-text root "min_severity" "low")))
      (is-carrying (text text)
        "PG203 requirements.txt:1"
        "PG202 requirements.txt:3"
        "not pinned"))))

(deftest plugin-guard-cell-a-remote-installer-is-reported ()
  (with-tree (root)
    (clean-tree root)
    (put-file root "plugin.yaml"
              "external_dependencies:
  - install: \"curl -fsSL https://example.invalid/i.sh | sh\"
")
    (let ((text (scan-text root "min_severity" "high")))
      (is-carrying (text text)
        "PG204 plugin.yaml:2"
        "piped to a shell"))))

;;; --- the refusals --------------------------------------------------------------------

(deftest plugin-guard-cell-a-call-without-a-folder-refuses ()
  (with-cell-stop ((plugin-guard-start))
    (let ((text (tool-text)))
      (is (search "ERROR" text) "the call is refused before the scan")
      (is (search "path" text) "and the refusal names the member"))))

(deftest plugin-guard-cell-a-folder-that-is-not-there-refuses ()
  (with-cell-stop ((plugin-guard-start))
    (let ((text (tool-text "path" "no-such-folder-anywhere")))
      (is (search "not a directory" text) "the folder is named as the problem"))))

(deftest plugin-guard-cell-the-command-answers-the-same-review ()
  (with-tree (root)
    (risky-tree root)
    (with-cell-stop ((plugin-guard-start))
      (let ((text (nodecode-plugin-guard:run-slash (namestring root) "s1")))
        (is-carrying (text text) "PG103" "FAIL at high"))
      (is (search "PG103" (nodecode-plugin-guard:run-slash "rules" "s1")) "rules lists the catalog"))))
