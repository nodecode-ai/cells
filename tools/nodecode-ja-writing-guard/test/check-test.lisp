;;;; check-test.lisp --- the catalog, the masking, the surfaces and the score.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defparameter +ai-answer+
  "この機能はシームレスに統合されており、業務の効率を大きく高めることができます。重要な役割を果たします。いかがでしたか。"
  "A Japanese answer that carries several of the catalog's rules.")

(deftest ja-writing-guard-plain-japanese-scores-low ()
  (let* ((report (jg-report "明日の会議は10時からです。資料は昨日送りました。"))
         (findings (nodecode-ja-writing-guard:report-findings report)))
    (is (nodecode-ja-writing-guard:report-japanese report) "plain Japanese is recognised as Japanese")
    (is (plusp (nodecode-ja-writing-guard:report-threshold report)) "and carries its surface's threshold")
    (is (< (nodecode-ja-writing-guard:report-score report) 40) "with a score under the chat threshold")
    (is (null findings) "and nothing to report")))

(deftest ja-writing-guard-reports-a-tell-with-line-column-and-hint ()
  (let* ((report (jg-report +ai-answer+))
         (findings (nodecode-ja-writing-guard:report-findings report))
         (first (first findings))
         (lines (mapcar #'nodecode-ja-writing-guard:finding-label findings)))
    (is (plusp (length findings)) "an AI-flavoured answer has findings")
    (is (member "「いかがでしたか」の締め" lines :test #'string=) "the closing question is named")
    (is (member "宣伝調の大げさな言葉" lines :test #'string=) "so is the inflated word")
    (is (plusp (nodecode-ja-writing-guard:finding-line first)) "each finding sits on a line")
    (is (plusp (nodecode-ja-writing-guard:finding-col first)) "and a column")
    (is (plusp (length (nodecode-ja-writing-guard:finding-hint first))) "and carries a fix hint")
    (is (plusp (nodecode-ja-writing-guard:report-score report)) "the score rises with the findings")
    (is (search "直し方:" (jg-shown +ai-answer+)) "the formatted report says how to fix each one")))

(deftest ja-writing-guard-the-surfaces-judge-differently ()
  (let ((chat (jg-report +ai-answer+ :surface "chat"))
        (email (jg-report +ai-answer+ :surface "email"))
        (docs (jg-report +ai-answer+ :surface "tech_doc"))
        (plain (jg-report "明日の会議は十時からだ。資料は昨日送ったので、前もって目を通しておいてほしい。"
                          :surface "business_email")))
    (is (= 40 (nodecode-ja-writing-guard:report-threshold chat)) "chat's threshold is the original's 40")
    (is (= 35 (nodecode-ja-writing-guard:report-threshold email)) "email is stricter, and the alias resolves")
    (is (= 45 (nodecode-ja-writing-guard:report-threshold docs)) "tech docs allow more of it")
    (is (plusp (nodecode-ja-writing-guard:report-register-issues plain)) "常体 in an outside-facing surface is a register finding")))

(deftest ja-writing-guard-masks-code-and-quotes ()
  (let* ((report (jg-report "設定は次のとおりです。\n\n```\nシームレス — 革新的\n```\n\n`革新的` と書いてあります。"))
         (dashes (remove-if-not (lambda (f) (string= "format.em_dash" (nodecode-ja-writing-guard:finding-rule f)))
                                 (nodecode-ja-writing-guard:report-findings report))))
    (is (null dashes) "a dash inside a code fence is not counted"))
  (let ((quoted (jg-report "彼は「シームレスに統合された」と説明しました。")))
    (is (null (nodecode-ja-writing-guard:report-findings quoted)) "a phrase inside 「」 is the writer's quotation")))

(deftest ja-writing-guard-leaves-other-languages-alone ()
  (let* ((report (jg-report "The deployment finished and the tests are green."))
         (verdict (format nil "~a" (nodecode-ja-writing-guard::verdict report))))
    (is (not (nodecode-ja-writing-guard:report-japanese report)) "English is not checked")
    (is (zerop (nodecode-ja-writing-guard:report-score report)) "and scores nothing")
    (is (search "日本語の文ではない" (jg-shown "The deployment finished.")) "the summary says so")
    (is (search "not_japanese" verdict) "and the verdict is not_japanese")))
