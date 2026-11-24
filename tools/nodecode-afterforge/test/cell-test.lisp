;;;; cell-test.lisp --- the six tools, the loop they run, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the loop the plugin ran, as the cell keeps it: a
;;;; recorded case lands on a shelf of its own and is listed; only an approved
;;;; case gets a check; a check runs only with the digest it was drafted with;
;;;; its exit code decides verified or regressed, and its output is kept as
;;;; evidence; a case with no project refuses at start, and a verify that
;;;; leaves out its digest refuses at the boundary. One test runs the real
;;;; runner, for the exit code a stub cannot prove.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "afterforge"
  (:help :afterforge)
  (:command "afterforge")
  (:running (is (= 120 (nodecode-afterforge::setting :timeout-seconds)) "two minutes is one check's ceiling")
            (is (= 1200 (nodecode-afterforge::setting :output-chars)) "and twelve hundred characters of output"))
  (:refused ("project" "") ("project" "/no-such-project-folder-xyz") ("timeout_seconds" 1))
  (:idle afterforge:afterforge-error
         (afterforge:report)
         (afterforge:cases)))

;;; --- the loop ----------------------------------------------------------------------

(deftest afterforge-cell-a-recorded-case-is-on-its-own-shelf ()
  (with-lab ()
    (let ((answer (afterforge:record "parser test flakes" "failure"
                                     "test_parser failed once and passed on the rerun"
                                     "test_parser passes every time")))
      (is (search "case-001" answer) "the first case is named")
      (is (search "afterforge_review" answer) "and the next step is named"))
    (let ((shelf (afterforge:cases)))
      (is (search "case-001" shelf) "the shelf lists it")
      (is (search "candidate" shelf) "as a candidate")
      (is (search "parser test flakes" shelf) "by its title"))
    (is (search "case-002" (afterforge:record "the operator renamed a flag" "correction" "" "")) "the next case gets the next name")
    (is (search "2 cases" (afterforge:report)) "the report counts the shelf")
    (is (search "candidate" (afterforge:cases "candidate")) "a status filter answers")
    (is (search "no case is verified" (afterforge:cases "verified")) "and an empty one says so")))

(deftest afterforge-cell-only-an-approved-case-gets-a-check ()
  (with-lab ()
    (afterforge:record "parser test flakes")
    (is (search "not approved" (refusal-text afterforge:afterforge-error (afterforge:recipe "case-001" "make check"))) "a candidate gets no check")
    (is (search "reviewed" (afterforge:review "case-001" "approve" "the operator asked for it")) "approval moves it on")
    (let ((drafted (afterforge:recipe "case-001" "make check")))
      (is (search "digest" drafted) "a drafted check names its digest")
      (is (search (digest "case-001" "make check") drafted) "which is the command's own digest"))
    (let ((evidence (afterforge:report "case-001")))
      (is (search "make check" evidence) "the report keeps the check")
      (is (search "the operator asked for it" evidence) "and the note it was approved with")
      (is (search "no run yet" evidence) "with nothing run yet"))))

(deftest afterforge-cell-a-check-runs-only-with-the-digest-it-was-drafted-with ()
  (with-lab (:answer "1 passed" :code 0)
    (afterforge:record "parser test flakes")
    (afterforge:review "case-001" "approve")
    (afterforge:recipe "case-001" "make check")
    (is (search "not \"made-up\"" (refusal-text afterforge:afterforge-error (afterforge:verify "case-001" "made-up"))) "a digest that is not the one drafted refuses")
    (is (null *runs*) "and nothing ran")
    (let ((passed (afterforge:verify "case-001" (digest "case-001" "make check"))))
      (is (search "verified" passed) "the check passed")
      (is (search "1 passed" passed) "and its output came back")
      (is (equal "make check" (first (first *runs*))) "the check ran, through the seam")
      (is (search (string-right-trim "/" (namestring (nodecode-afterforge::project-dir))) (second (first *runs*))) "in the project folder it was drafted for")
      (let ((evidence (afterforge:report "case-001")))
        (is (search "pass exit 0" evidence) "the report keeps the run")
        (is (search "1 passed" evidence) "with what it printed")))
    (setf *code* 1 *answer* "1 failed")
    (is (search "regressed" (afterforge:verify "case-001" (digest "case-001" "make check"))) "a later failure is a regression")
    (is (search "fail exit 1" (afterforge:report "case-001")) "and is kept as evidence")))

(deftest afterforge-cell-a-check-that-prints-nothing-is-still-evidence ()
  (with-lab (:answer "" :code 0)
    (afterforge:record "the command that lied" "failure" "it exited 0 and did nothing" "it prints its summary")
    (afterforge:review "case-001" "approve")
    (afterforge:recipe "case-001" "true")
    (is (search "(the check printed nothing)" (afterforge:verify "case-001" (digest "case-001" "true"))) "silence is named, not hidden")
    (afterforge:record "already retired")
    (is (search "no run yet" (afterforge:report "case-002")) "a case with no check yet says so")
    ;; a rejected case gets no check
    (afterforge:review "case-002" "reject" "not worth a check")
    (is (search "rejected" (refusal-text afterforge:afterforge-error (afterforge:recipe "case-002" "make check"))) "a rejected case drafts nothing")))

(deftest afterforge-cell-the-command-answers-the-shelf ()
  (with-lab ()
    (afterforge:record "parser test flakes")
    (is (search "case-001" (afterforge:run-slash "status" "s1")) "/afterforge status lists it")
    (is (search "parser test flakes" (afterforge:run-slash "report case-001" "s1")) "/afterforge report names the case")
    (is (search "case-001" (afterforge:run-slash "cases" "s1")) "/afterforge cases answers the shelf")
    (is (search "not a subcommand" (afterforge:run-slash "wat" "s1")) "an unknown subcommand says so")))

(deftest afterforge-cell-the-real-runner-keeps-the-exit-code ()
  (with-lab ()
    (multiple-value-bind (output code)
        (nodecode-afterforge::run-shell "printf hi; exit 3" (nodecode-afterforge::project-dir) 10)
      (is (equal "hi" (string-trim '(#\Space #\Newline #\Return #\Tab) output)) "the output comes back")
      (is (= 3 code) "and the exit code with it"))))

;;; --- the boundaries -----------------------------------------------------------------

(deftest afterforge-cell-a-missing-project-refuses ()
  (is (search "project" (refusal-text nlk:config-refusal (afterforge-start "project" ""))) "an unset project is named")
  (is (search "no such folder" (refusal-text nlk:config-refusal (afterforge-start "project" "/no-such-project-folder-xyz"))) "and so is one that does not exist"))

(deftest afterforge-cell-verify-refuses-without-its-digest ()
  (is (signals-error nlk:contract-refusal (nlk:contract-decode (nlk:contract 'nodecode-afterforge::afterforge-verify) (nlk:make-json-object "case_id" "case-001"))) "the digest is required at the boundary")
  (is (signals-error nlk:contract-refusal (nlk:contract-decode (nlk:contract 'nodecode-afterforge::afterforge-case) (nlk:make-json-object "kind" "failure"))) "and so is a case's title"))
