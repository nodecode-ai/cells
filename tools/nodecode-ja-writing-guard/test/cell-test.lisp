;;;; cell-test.lisp --- lifecycle, the line under the answer, the tool and the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: a start installs one
;;;; advice on the stream, one tool, one command and one manual topic, and a
;;;; stop takes them all back; a section the declaration cannot read refuses
;;;; and leaves nothing behind; report mode adds one italic line under a
;;;; Japanese answer and changes nothing else; off mode and a paused cell
;;;; leave the answer exactly as the model said it; the tool answers the
;;;; draft it was handed; and the command reports, pauses and checks.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "ja-writing-guard"
  (:hooks 'nle::emit-stream-part)
  (:command "ja-writing-guard")
  (:help :ja-writing-guard)
  (:running
   (is (equal "report" (nodecode-ja-writing-guard::setting :mode)) "the default reports and never rewrites")
   (is (eq t (nodecode-ja-writing-guard::setting :footer)) "with the line shown")
   (is (nle::find-tool "ja_writing_check") "the tool is registered"))
  (:stopped
   (is (null (nle::find-tool "ja_writing_check")) "stop unregisters the tool"))
  (:refused ("surface" "novel")))

(deftest ja-writing-guard-refuses-a-surface-it-does-not-know ()
  (with-saved-globals (nle:*tools*)
    (is (signals-error nlk:config-refusal (ja-writing-guard-start "surface" "novel")) "an unknown surface refuses")
    (is (null (nle::find-tool "ja_writing_check")) "and the refused start installed nothing")
    (is (null (cell-hook "nodecode-ja-writing-guard" 'nle::emit-stream-part)) "not even the advice")))

(deftest ja-writing-guard-report-mode-adds-only-the-line ()
  (with-cell-stop ((ja-writing-guard-start))
    (let* ((text +ai-answer+)
           (parts (jg-answer text))
           (answer (jg-text parts)))
      (is (search text answer) "the answer is as the model said it")
      (is (search "*ja-writing-guard: AI的な言い回し" answer) "with one italic line under it")
      (is (eq :finish (nle::stream-part-kind (car (last parts)))) "and the stream still finishes after it"))
    (let* ((clean "明日の会議は10時からです。資料は昨日送りました。")
           (answer (jg-text (jg-answer clean))))
      (is (equal clean answer) "a clean answer is left exactly as it was"))))

(deftest ja-writing-guard-off-and-paused-leave-the-answer-alone ()
  (with-cell-stop ((ja-writing-guard-start "mode" "off"))
    (is (equal +ai-answer+ (jg-text (jg-answer +ai-answer+))) "off changes nothing at all"))
  (with-cell-stop ((ja-writing-guard-start))
    (is (search "paused" (jg-slash "off")) "/ja-writing-guard off pauses it")
    (is (equal +ai-answer+ (jg-text (jg-answer +ai-answer+))) "and the next answer comes through whole")
    (is (search "on -" (jg-slash "on")) "/ja-writing-guard on resumes")
    (is (search "*ja-writing-guard:" (jg-text (jg-answer +ai-answer+))) "and the line is back")
    (is (search "cleared" (jg-slash "reset")) "reset clears the counters")))

(deftest ja-writing-guard-the-tool-checks-the-draft-it-is-handed ()
  (with-cell-stop ((ja-writing-guard-start))
    (let ((answer (jg-tool "{\"text\": \"この機能はシームレスに統合されており、いかがでしたか。\"}")))
      (is (search "AI的な言い回し" answer) "the score is reported")
      (is (search "いかがでしたか" answer) "with the phrase quoted back")
      (is (search "直し方:" answer) "and a fix hint for each finding"))
    (let ((answer (jg-tool "{\"text\": \"明日の会議は10時からです。資料は昨日送りました。\"}")))
      (is (search "指摘なし" answer) "a clean draft says so"))
    (let ((answer (jg-tool "{\"text\": \"明日の会議は10時からです。資料は昨日送りました。\", \"surface\": \"email\"}")))
      (is (search "email" answer) "the surface the call names is the one it is judged on"))))

(deftest ja-writing-guard-the-command-reports-and-checks-a-text ()
  (with-cell-stop ((ja-writing-guard-start))
    (let ((report (jg-slash "")))
      (is (search "ja-writing-guard: on" report) "bare, it reports its state")
      (is (search "mode report" report) "the mode it runs in")
      (is (search "0 answers checked" report) "and nothing done yet"))
    (jg-answer +ai-answer+)
    (is (search "1 answers checked" (jg-slash "")) "the counters follow the answers")
    (is (search "AI的な言い回し" (jg-slash +ai-answer+)) "a text typed after the command is checked")
    (is (search "基準 35" (jg-slash (concatenate 'string "business_email " +ai-answer+))) "a surface typed before it is used")
    (is (search "使い方" (jg-slash "help")) "help explains the surfaces")))
