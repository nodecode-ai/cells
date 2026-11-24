;;;; guard-test.lisp --- the rules themselves: dashes, masking, phrases, du/Sie.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: a dash is counted only outside code, URLs and
;;;; quotes; the fix keeps a range and a bullet and turns a spoken dash into
;;;; a comma or a spaced hyphen; the phrase table reads both languages and
;;;; one rule can be dropped by id; and du/Sie counts what is used, not what
;;;; is talked about, and reports a paragraph that mixes both.

(in-package #:nodecode.test)

(deftest writing-guard-counts-dashes-only-in-prose ()
  (is (= 2 (nodecode-writing-guard:count-dashes "a — b – c")) "an em and an en dash are both dashes")
  (is (zerop (nodecode-writing-guard:count-dashes "a - b")) "a plain hyphen is punctuation, not a dash")
  (is (= 1 (nodecode-writing-guard:count-dashes "a – b")) "the en dash counts from one")
  (is (zerop (nodecode-writing-guard:report-dashes
              (nodecode-writing-guard:check-text "`a — b` and https://x.test/a—b"))) "inline code and a URL are masked before the count")
  (is (zerop (nodecode-writing-guard:report-dashes
              (nodecode-writing-guard:check-text (format nil "> quoted — line")))) "a quoted line is left alone, as skip_quotes says")
  (is (= 1 (nodecode-writing-guard:report-dashes
            (nodecode-writing-guard:check-text (format nil "> quoted — line") :skip-quotes nil))) "and counted when the operator asks for it")
  (is (zerop (nodecode-writing-guard:report-dashes
              (nodecode-writing-guard:check-text (format nil "```~%code — here~%```")))) "a fenced block is code and nothing else"))

(deftest writing-guard-fixes-dashes-by-rule ()
  (is (equal "Das ist, so." (nodecode-writing-guard:fix-dashes "Das ist — so.")) "a dash between words becomes a comma and a space")
  (is (equal "Das ist - so." (nodecode-writing-guard:fix-dashes "Das ist — so." :style "hyphen")) "or a spaced hyphen when the operator says so")
  (is (equal "10-20 Tage" (nodecode-writing-guard:fix-dashes "10–20 Tage")) "a range keeps a hyphen")
  (is (equal "- ein Punkt" (nodecode-writing-guard:fix-dashes "— ein Punkt")) "a bullet stays a bullet")
  (is (equal "Mo-Fr" (nodecode-writing-guard:fix-dashes "Mo–Fr")) "a compound written with an en dash keeps its hyphen")
  (multiple-value-bind (fixed count) (nodecode-writing-guard:fix-dashes "a — b — c")
    (is (= 2 count) "both dashes are counted")
    (is (equal "a, b, c" fixed) "and both are replaced"))
  (multiple-value-bind (fixed count) (nodecode-writing-guard:fix-dashes "no dash here")
    (is (zerop count) "text without a dash is counted zero")
    (is (equal "no dash here" fixed) "and comes back as it was"))
  (let ((report (nodecode-writing-guard:check-text "Ein Satz — mit Strich." :fix t)))
    (is (search "Ein Satz, mit Strich." (nodecode-writing-guard:report-text report)) "the report carries the fixed text, whole")
    (is (= 1 (nodecode-writing-guard:report-dashes-fixed report)) "and how much it replaced")))

(deftest writing-guard-leaves-code-alone-when-it-fixes ()
  (let* ((text "a — b `c — d`")
         (report (nodecode-writing-guard:check-text text :fix t)))
    (is (equal "a, b `c — d`" (nodecode-writing-guard:report-text report)) "the prose is fixed and the inline code is not")
    (is (= 1 (nodecode-writing-guard:report-dashes-fixed report)) "one dash, not two")))

(deftest writing-guard-finds-stock-phrases-in-both-languages ()
  (let ((hits (nodecode-writing-guard:find-phrases
               "This is seamless, and I hope this helps.")))
    (is (find "en.seamless" hits :key (lambda (hit) (getf hit :id)) :test #'string=) "an English stock phrase is named by its rule id")
    (is (find "en.hope_helps" hits :key (lambda (hit) (getf hit :id)) :test #'string=) "and so is the second")
    (is (find "seamless" hits :key (lambda (hit) (getf hit :text)) :test #'string=) "and the phrase itself is quoted back, not only its id"))
  (is (find "de.wichtig_zu_beachten"
            (nodecode-writing-guard:find-phrases "Es ist wichtig zu beachten, dass es regnet.")
            :key (lambda (hit) (getf hit :id)) :test #'string=) "a German stock phrase is found")
  (is (null (nodecode-writing-guard:find-phrases "delving" :ignore '("en.delve"))) "a rule the operator ignores is not reported")
  (is (find "custom.1"
            (nodecode-writing-guard:find-phrases "so ein Quatsch" :extra '("Quatsch"))
            :key (lambda (hit) (getf hit :id)) :test #'string=) "an extra phrase of the operator's own is reported as custom")
  (is (null (nodecode-writing-guard:find-phrases "delving" :ignore '("delving"))) "an exact phrase can be ignored instead of its id"))

(deftest writing-guard-reads-du-and-sie ()
  (multiple-value-bind (du sie)
      (nodecode-writing-guard:address-counts "Hast du gesehen, was Sie gemacht haben?")
    (is (= 1 du) "the informal form is counted once")
    (is (= 1 sie) "and the formal one, mid-sentence, once"))
  (multiple-value-bind (du sie)
      (nodecode-writing-guard:address-counts "Sie sind dran. Sie können das ändern.")
    (is (zerop du) "no informal form")
    (is (zerop sie) "a Sie that only ever opens a sentence is the pronoun, not the formal address"))
  (multiple-value-bind (du sie)
      (nodecode-writing-guard:address-counts "Ich habe Ihnen geschrieben.")
    (is (zerop du) "no informal form in it")
    (is (= 1 sie) "an Ihnen mid-sentence is the formal address"))
  (multiple-value-bind (du sie)
      (nodecode-writing-guard:address-counts "Hier gilt: du/Sie, je nach Leser.")
    (is (and (zerop du) (zerop sie)) "talking about the forms uses neither"))
  (let ((report (nodecode-writing-guard:check-text
                 (format nil "Du machst das gut.~%~%Ich finde, Sie machen das gut."))))
    (is (equal "de" (nodecode-writing-guard:report-language report)) "the text is read as German")
    (is (null (nodecode-writing-guard:report-mixed report)) "two paragraphs, one form each, are not a mixed paragraph")
    (is (equal "mixed" (nodecode-writing-guard:report-form report)) "but the answer does use both"))
  (let ((report (nodecode-writing-guard:check-text
                 "Du machst das gut, und Sie machen das auch gut." :address "du")))
    (is (= 1 (length (nodecode-writing-guard:report-mixed report))) "one paragraph mixes both")
    (is (plusp (nodecode-writing-guard:report-wrong-form report)) "with a form fixed, the other one is the wrong one"))
  (let ((report (nodecode-writing-guard:check-text "Du machst das gut." :previous-form "sie")))
    (is (equal "sie" (nodecode-writing-guard:report-switched-from report)) "an answer that switches against the last one says so")))

(deftest writing-guard-writes-the-line-in-the-answers-language ()
  (let ((english (nodecode-writing-guard:footer-line
                  (nodecode-writing-guard:check-text "This is seamless."))))
    (is (search "*writing-guard:" english) "the line is italic and signed")
    (is (search "1 AI phrase" english) "an English answer is reported in English"))
  (let ((german (nodecode-writing-guard:footer-line
                 (nodecode-writing-guard:check-text (format nil "Ein Satz — mit Strich.~%~%Du bist dran.")))))
    (is (search "Gedankenstrich" german) "a German answer is reported in German"))
  (is (equal "" (nodecode-writing-guard:footer-line
                 (nodecode-writing-guard:check-text "Der Satz ist ruhig und klar."))) "nothing found says nothing at all"))
