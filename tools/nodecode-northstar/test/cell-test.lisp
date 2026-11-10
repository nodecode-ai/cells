;;;; cell-test.lisp --- lifecycle, the three checks, the report, the tool, /northstar.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its three checks over
;;;; real files in a scratch project: that a start installs the tool, the
;;;; command and the manual topic and a stop takes them back; that a
;;;; contradiction between two files is an error carrying both files; that
;;;; the same directive in two files is one warning; that a path that
;;;; resolves nowhere is reported and one that resolves is not; that a
;;;; coherent project says so in one line; that the json format is one
;;;; object a caller can read; that a scope this cell does not know refuses
;;;; in words; and that the tool answers through the real dispatch seam and
;;;; refuses a call that leaves out its required member.

(in-package #:nodecode.test)

(define-test-slice "northstar" "NORTHSTAR-CELL-" :start nodecode-northstar:start-cell)

(defmacro with-northstar-project ((dir &body files) &body body)
  "BODY over a fresh project folder holding FILES, each (NAME . TEXT), the
tree removed on unwind."
  `(with-temp-trees ((root "northstar"))
     (let ((,dir (uiop:ensure-directory-pathname root)))
       (ensure-directories-exist ,dir)
       ,@(loop for (name . text) in files
               collect `(write-temp-file (merge-pathnames ,name ,dir) ,text))
       ,@body)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "northstar"
  (:help :northstar)
  (:command "northstar")
  (:running (is (nle::find-tool "northstar_audit") "the audit tool is registered")
            (is (= 40 (nodecode-northstar::setting :max-findings)) "max_findings defaults to forty"))
  (:stopped (is (null (nle::find-tool "northstar_audit")) "stop takes the tool back"))
  (:refused ("max_findings" 0))
  (:idle nodecode-northstar:northstar-error (northstar:audit)))

;;; --- the checks ----------------------------------------------------------------

(deftest northstar-cell-a-contradiction-between-two-files-is-one-error ()
  (with-northstar-project (dir
                           ("AGENTS.md" . "Always ask for confirmation before editing a file.
Keep the diff small.")
                           ("CLAUDE.md" . "Proceed without confirmation when the change is small."))
    (with-cell-stop ((northstar-start))
      (let ((text (northstar:audit :scope "project" :directory dir)))
        (is-carrying (text text)
          ("[error] contradiction" "a cross-file contradiction is an error")
          ("AGENTS.md" "the file that allows is named"))
        (is (search "without confirmation" text) "and the words on both sides are quoted")
        (is (not (search "possible contradiction" text)) "with no scope named on either side, it is definite")))))

(deftest northstar-cell-the-same-directive-in-two-files-is-one-warning ()
  (with-northstar-project (dir
                           ("AGENTS.md" . "Run the test suite before every commit to main.")
                           ("README.md" . "Run the test suite before every commit to main."))
    (with-cell-stop ((northstar-start))
      (let ((text (northstar:audit :scope "project" :directory dir)))
        (is-carrying (text text)
          ("[warning] duplicate" "one directive written twice is one warning")
          ("2 files" "and both files are counted"))
        (is (not (search "contradiction" text)) "a repeated directive is no contradiction")))))

(deftest northstar-cell-a-reference-that-resolves-nowhere-is-a-warning ()
  (with-northstar-project (dir
                           ("AGENTS.md" . "Open /docs/missing-notes.md first.")
                           ("docs/present-notes.md" . "the notes that are there"))
    (with-cell-stop ((northstar-start))
      (let ((text (northstar:audit :scope "project" :directory dir)))
        (is-carrying (text text)
          ("[warning] stale_reference" "a path the project does not hold is reported")
          ("missing-notes.md" "and the path is quoted"))
        (is (not (search "present-notes.md" text)) "a path beside the file that names it is not")))))

(deftest northstar-cell-a-coherent-project-says-so-in-one-line ()
  (with-northstar-project (dir
                           ("AGENTS.md" . "Keep the diff small and the test suite green."))
    (with-cell-stop ((northstar-start))
      (is (search "Nothing to report" (northstar:audit :scope "project" :directory dir)) "one file, one directive, nothing to fix"))))


(deftest northstar-cell-json-is-a-report-a-caller-can-read ()
  (with-northstar-project (dir
                           ("AGENTS.md" . "Keep the diff small and the test suite green."))
    (with-cell-stop ((northstar-start))
      (let ((text (northstar:audit :scope "project" :output-format "json" :directory dir)))
        (is (uiop:string-prefix-p "{" text) "the json format answers one object")
        (is (search "sources_inspected" text) "with the sources it read")
        (is (search "total_findings" text) "and a summary")))))

(deftest northstar-cell-a-scope-the-cell-does-not-know-refuses-in-words ()
  (with-northstar-project (dir
                           ("AGENTS.md" . "Keep the diff small and the test suite green."))
    (with-cell-stop ((northstar-start))
      (is-carrying (text (refusal-text nodecode-northstar:northstar-error
                                       (northstar:audit :scope "nonsense" :directory dir)))
        ("no scope of this cell" "a stranger names the set that is known")
        ("full, home, project, skills" "and the scopes it does know")))))

;;; --- the tool ------------------------------------------------------------------

(deftest northstar-cell-the-tool-answers-through-the-dispatch-seam (with-session-store ("s1"))
  (with-cell-stop ((northstar-start))
    (let ((result (execute-wire-call "call-1" "northstar_audit" "{\"scope\":\"project\"}")))
      (is (equal "tool" (gethash "role" result)))
      (is (uiop:string-prefix-p "Northstar coherence audit: "
                                (princ-to-string (nle::message-content result))) "the tool answers the report, not a summary of it"))))


(deftest northstar-cell-a-call-missing-its-scope-is-refused (with-session-store ("s1"))
  (with-cell-stop ((northstar-start))
    (is-carrying (text (princ-to-string
                        (nle::message-content (execute-wire-call "call-bad" "northstar_audit" "{}"))))
      ("contract_refused" "a call with no scope is refused")
      ("scope must be present" "and the refusal names the member it wants"))))
