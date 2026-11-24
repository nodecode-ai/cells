;;;; cell-test.lisp --- the tools, the command, the log, the refusals, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs both tools, /redteam-gates
;;;; and (help :redteam-gates) and takes them back on stop; a gate passes only
;;;; when its artifacts hold what the schema names; one that does not passes
;;;; nothing and says which check is open, in words the model can act on; a
;;;; gate that reads one artifact of its own asks for it instead of guessing;
;;;; an unknown mode or stage answers a line and writes no row; every verdict
;;;; appends exactly one row to gate-log.md; the command does the same for the
;;;; operator; a call outside the contract is refused at the boundary; and the
;;;; section's workspace and log_rows are what the reads keep to.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "redteam-gates"
  (:config)
  (:help :redteam-gates)
  (:command "redteam-gates")
  (:running
   (is (= 20 (nodecode-redteam-gates::setting :log-rows)) "a verdict shows the log's newest twenty rows")
   (is (equal "." (nodecode-redteam-gates::setting :workspace)) "and reads the directory it is run in"))
  (:refused ("log_rows" 0)))

;;; --- the schema --------------------------------------------------------------------

(deftest redteam-gates-cell-the-schema-is-the-originals ()
  (is (= 8 (length (nodecode-redteam-gates:mode-ids))) "eight modes, as the original's schema had")
  (is (= 32 (reduce (function +)
                    (mapcar (lambda (mode) (length (nodecode-redteam-gates:gate-ids mode)))
                            nodecode-redteam-gates:+gate-modes+))) "thirty-two gates, as the original's schema had")
  (is (= 5 (length (nodecode-redteam-gates:gate-ids
                    (nodecode-redteam-gates:find-mode "attack-defense")))) "five stages in the attack and defence mode")
  (is (null (nodecode-redteam-gates:find-mode "redteam")) "a mode with no gates of its own is not a gate mode")
  (is (equal "P1" (getf (nodecode-redteam-gates:find-gate
                          (nodecode-redteam-gates:find-mode "PENTEST") "p1")
                         :stage)) "a mode and a stage read whatever their case"))

;;; --- a gate that passes --------------------------------------------------------------

(deftest redteam-gates-cell-a-gate-passes-on-a-full-set ()
  (with-workspace (dir)
    (rg-write dir "assets.md" +rg-assets+)
    (rg-write dir "evidence-index.md" +rg-evidence+)
    (with-cell-stop ((redteam-gates-start "workspace" dir))
      (let ((answer (rg-tool "rg-1" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P1\"}")))
        (is (search "stage gate pentest/P1" answer) "the gate it checked")
        (is (search "result: pass" answer) "a full set of artifacts passes")
        (is (search "carries every marker" answer) "the markers were found")
        (is (search "a structural pass is not a full pass" answer) "and the model is told what a pass is worth"))
      (let ((log (rg-log-text dir)))
        (is (search "| pentest/P1 | pass |" log) "one pass row in the log")))))

;;; --- a gate that does not -----------------------------------------------------------

(deftest redteam-gates-cell-a-gate-names-what-is-missing ()
  (with-workspace (dir)
    (rg-write dir "assets.md" "# assets

| asset | note |
|---|---|
| app.example.com | WAF in front |
")
    (with-cell-stop ((redteam-gates-start "workspace" dir))
      (let ((answer (rg-tool "rg-2" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P1\"}")))
        (is (search "result: FAIL" answer) "an artifact that is not what the schema asks for does not pass")
        (is (search "lacks rate limit" answer) "the missing marker is named")
        (is (search "evidence-index.md is missing" answer) "and so is the file that was never written")
        (is (search "a structural pass is not a full pass" answer) "and the model is told what a structural pass is not"))
      (let ((answer (rg-tool "rg-3" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P1\", \"workspace\": \"/nonexistent-engagement\"}")))
        (is (search "result: FAIL" answer) "a workspace with nothing in it passes nothing"))
      (rg-write dir "assets.md" "# assets

| asset | note |
|---|---|
| app.example.com | WAF, rate limit |
| half filled |
")
      (let ((answer (rg-tool "rg-3b" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P1\"}")))
        (is (search "rows short of 2 cells on line 6" answer) "a table row short of its cells is named by line"))
      (is (search "| pentest/P1 | fail |" (rg-log-text dir)) "and the failure is in the log"))))

;;; --- the gate that wants a file -------------------------------------------------------

(deftest redteam-gates-cell-a-gate-that-reads-one-file-asks-for-it ()
  (with-workspace (dir)
    (with-cell-stop ((redteam-gates-start "workspace" dir))
      (let ((answer (rg-tool "rg-4" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P2\"}")))
        (is (search "needs the file argument" answer) "the gate answers the hint, not a verdict")
        (is (search "report file of that finding" answer) "and says what the file is"))
      (rg-write dir "finding-1.md" "# finding

The baseline response, the diff once the parameter is added, the marker echoed back,
and the reviewer's record of it.
")
      (let ((answer (rg-tool "rg-5" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P2\", \"file\": \"finding-1.md\"}")))
        (is (search "result: pass" answer) "a finding holding the comparison set passes the structural check")
        (is (search "the reviewer's call" answer) "and the judgement left to a reviewer is still named")))))

;;; --- what it cannot know --------------------------------------------------------------

(deftest redteam-gates-cell-an-unknown-mode-or-stage-answers-a-line ()
  (with-workspace (dir)
    (with-cell-stop ((redteam-gates-start "workspace" dir))
      (let ((answer (rg-tool "rg-6" "stage_gate"
                             "{\"mode\": \"redteam\", \"stage\": \"P1\"}")))
        (is (search "no mode called redteam" answer) "the mode is not there")
        (is (search "pentest" answer) "the modes there are, named"))
      (let ((answer (rg-tool "rg-7" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P9\"}")))
        (is (search "no stage called P9" answer) "the stage is not there")
        (is (search "P1, P2, P3" answer) "the stages there are, named"))
      (is (null (rg-log-text dir)) "and neither answer wrote a row"))))

;;; --- the listing ----------------------------------------------------------------------

(deftest redteam-gates-cell-gates_list-lays-out-the-schema ()
  (with-cell-stop ((redteam-gates-start))
    (let ((answer (rg-tool "rg-8" "gates_list" "{}")))
      (is (search "pentest" answer) "every mode is listed")
      (is (search "ctf-solver" answer) "including the last of them")
      (is (search "assets.md" answer) "with the files each gate reads"))
    (let ((answer (rg-tool "rg-9" "gates_list" "{\"mode\": \"cloud-security\"}")))
      (is (search "C7 Report completeness" answer) "one mode in full")
      (is (search "manual:" answer) "carrying the reviewer's lines"))))

;;; --- the command ------------------------------------------------------------------------

(deftest redteam-gates-cell-the-command-answers-the-operator ()
  (with-workspace (dir)
    (rg-write dir "assets.md" +rg-assets+)
    (rg-write dir "evidence-index.md" +rg-evidence+)
    (with-cell-stop ((redteam-gates-start "workspace" dir))
      (is (search "gates: P1, P2, P3" (nodecode-redteam-gates:run-slash "" nil)) "bare, /redteam-gates lists the modes and their gates")
      (is (search "P1 Asset and environment baseline" (nodecode-redteam-gates:run-slash "pentest" nil)) "with a mode, its gates")
      (is (search "result: pass" (nodecode-redteam-gates:run-slash "pentest P1" nil)) "with a mode and a stage, the verdict")
      (is (search "no mode called hops" (nodecode-redteam-gates:run-slash "hops" nil)) "and a mode it does not have is a line, not a failure"))))

;;; --- the boundary -----------------------------------------------------------------------

(deftest redteam-gates-cell-a-call-without-a-stage-is-refused ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-redteam-gates::stage-gate-arguments)
                             (nlk:make-json-object "mode" "pentest"))) "a call with no stage is refused by the declaration")
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-redteam-gates::stage-gate-arguments)
                             (nlk:make-json-object "mode" "" "stage" "P1"))) "and so is an empty mode"))

;;; --- the section ------------------------------------------------------------------------

(deftest redteam-gates-cell-the-section-bounds-the-log-read-back ()
  (with-workspace (dir)
    (rg-write dir "assets.md" +rg-assets+)
    (rg-write dir "evidence-index.md" +rg-evidence+)
    (with-cell-stop ((redteam-gates-start "workspace" dir "log_rows" 1))
      (let ((answer (rg-tool "rg-10" "stage_gate"
                             "{\"mode\": \"pentest\", \"stage\": \"P1\"}")))
        (is (search "newest 1 row" answer) "log_rows bounds what a verdict reads back"))
      (let ((answer (rg-tool "rg-11" "stage_gate"
                             "{\"mode\": \"assay\", \"stage\": \"P1\"}")))
        (is (search "no mode called assay" answer) "a misread mode is still a line")))))
