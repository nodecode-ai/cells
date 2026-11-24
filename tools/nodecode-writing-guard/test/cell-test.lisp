;;;; cell-test.lisp --- lifecycle, the advice on the stream, the tool and /writing-guard.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: a start installs one
;;;; advice on the stream, one on the harness sections, one tool, one command
;;;; and one manual topic, and a stop takes them all back; a section the
;;;; declaration cannot read refuses and leaves nothing behind; report mode
;;;; changes nothing but the line, fix mode replaces the dashes, off mode and
;;;; a paused cell leave the answer exactly as the model said it; the tool
;;;; answers the draft it was handed; and the command reports, pauses and
;;;; checks.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "writing-guard"
  (:hooks 'nle::emit-stream-part 'nle::read-harness-sections)
  (:command "writing-guard")
  (:help :writing-guard)
  (:running
   (is (equal "report" (nodecode-writing-guard::setting :mode)) "the default changes nothing but the line")
   (is (eq t (nodecode-writing-guard::setting :footer)) "and shows the line")
   (is (nle::find-tool "writing_guard_check") "the tool is registered")
   (is (search "writing-guard: before you deliver"
               (nle::harness-overlay-prompt "" (nle::read-harness-sections "s1"))) "the primer rides every request"))
  (:stopped
   (is (null (nle::find-tool "writing_guard_check")) "stop unregisters the tool"))
  (:refused ("mode" "chatty")))

(deftest writing-guard-refuses-a-mode-it-does-not-know ()
  (with-saved-globals (nle:*tools*)
    (is (signals-error nlk:config-refusal (writing-guard-start "mode" "chatty")) "a mode outside its options refuses")
    (is (null (nle::find-tool "writing_guard_check")) "and the refused start installed nothing")
    (is (null (cell-hook "nodecode-writing-guard" 'nle::emit-stream-part)) "not even the advice")))

(deftest writing-guard-report-mode-changes-nothing-but-the-line ()
  (with-cell-stop ((writing-guard-start))
    (let* ((text "This is seamless, and the build is done.")
           (parts (wg-answer text))
           (answer (wg-text parts)))
      (is (search text answer) "the answer is as the model said it")
      (is (search "*writing-guard: 1 AI phrase" answer) "with one italic line under it")
      (is (search "\"seamless\"" answer) "naming what read as machine written")
      (is (eq :finish (nle::stream-part-kind (car (last parts)))) "and the stream still finishes after it"))
    (let ((answer (wg-text (wg-answer "Der Satz ist ruhig und klar."))))
      (is (equal "Der Satz ist ruhig und klar." answer) "an answer with nothing to report is left exactly as it was"))))

(deftest writing-guard-fix-mode-replaces-the-dashes-as-they-stream ()
  (with-cell-stop ((writing-guard-start "mode" "fix"))
    (let* ((parts (append (wg-parts :stream-start)
                          (wg-parts :text-delta :id "s1" :text "Das ist ")
                          (wg-parts :text-delta :id "s1" :text "— so.")
                          (wg-parts :finish)))
           (answer (wg-text parts)))
      (is (search "Das ist - so." answer) "the two fragments join to one sentence, the dash a spaced hyphen")
      (is (not (search "—" answer)) "and no dash survives in the answer")
      (is (search "Gedankenstrich ersetzt" answer) "the line says what it replaced, in the answer's language"))
    (is (search "1 dash replaced" (wg-slash "")) "and the counters followed the stream")))

(deftest writing-guard-off-and-paused-leave-the-answer-alone ()
  (with-cell-stop ((writing-guard-start "mode" "off"))
    (is (equal "This is seamless." (wg-text (wg-answer "This is seamless."))) "off changes nothing at all"))
  (with-cell-stop ((writing-guard-start))
    (is (search "paused" (wg-slash "off")) "/writing-guard off pauses it")
    (is (equal "This is seamless." (wg-text (wg-answer "This is seamless."))) "and the next answer comes through whole")
    (is (search "on -" (wg-slash "on")) "/writing-guard on resumes")
    (is (search "writing-guard: 1 AI phrase" (wg-text (wg-answer "This is seamless."))) "and the line is back")
    (is (search "cleared" (wg-slash "reset")) "/writing-guard reset clears the counters")))

(deftest writing-guard-the-tool-checks-the-draft-it-is-handed ()
  (with-cell-stop ((writing-guard-start))
    (let ((answer (wg-tool "{\"text\": \"This is seamless and I hope this helps.\"}")))
      (is (search "2 AI phrases" answer) "the phrases are counted and named")
      (is (search "seamless" answer) "with the phrase itself quoted back"))
    (let ((answer (wg-tool "{\"text\": \"Das ist — so.\", \"fix\": true}")))
      (is (search "1 Gedankenstrich ersetzt" answer) "fix mode counts the dashes it replaced")
      (is (search "Das ist, so." answer) "and answers the draft with them replaced"))
    (let ((answer (wg-tool "{\"text\": \"Der Satz ist ruhig und klar.\"}")))
      (is (search "no findings" answer) "a clean draft says so")))
  (with-cell-stop ((writing-guard-start "address" "du"))
    (is (search "statt du"
                (wg-tool "{\"text\": \"Ich finde, Sie machen das gut.\"}")) "a form the operator fixed is what the tool checks against")))

(deftest writing-guard-the-command-reports-and-checks-a-text ()
  (with-cell-stop ((writing-guard-start))
    (let ((report (wg-slash "")))
      (is (search "writing-guard: on" report) "bare, it reports its state")
      (is (search "mode report" report) "the mode it runs in")
      (is (search "0 fragments read" report) "and nothing done yet"))
    (wg-answer "This is seamless.")
    (is (search "1 fragments read" (wg-slash "")) "the counters follow the answers")
    (is (search "*writing-guard: 2 dashes.*" (wg-slash "a — b — c")) "a text typed after the command is checked and reported")
    (is (search "no findings" (wg-slash "check a plain sentence")) "with the original's check word allowed before it")))
