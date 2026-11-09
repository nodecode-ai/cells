;;;; strip-test.lisp --- what is a leak, what is a language, and what tidying.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the ranges the original named are the ranges taken
;;;; out, and no other script's - Japanese kana and Korean Hangul survive a
;;;; run of Chinese beside them; the punctuation a stripped run leaves behind
;;;; goes with it, and can be kept; a switch turned off leaves the text
;;;; alone; and the whitespace tidying the original collapsed is here, with
;;;; the number of blank lines it keeps now a setting rather than a constant.

(in-package #:nodecode.test)

(deftest cjk-sanitizer-takes-out-chinese-and-cyrillic-and-keeps-kana ()
  (multiple-value-bind (clean stripped)
      (nodecode-cjk-sanitizer:sanitize-text "the build is готово 完成 and かな and 한글")
    (is (search "the build is" clean) "the words it keeps")
    (is (not (search "完成" clean)) "the Chinese is gone")
    (is (not (search "готово" clean)) "and the Cyrillic")
    (is (search "かな" clean) "Japanese kana stay")
    (is (search "한글" clean) "Korean Hangul stays")
    (is (= 8 stripped) "two hanzi and six Cyrillic letters, counted")
    (is (not (search "  " clean)) "and the gap they left is one space")))

(deftest cjk-sanitizer-punctuation-goes-with-the-run-it-belonged-to ()
  (multiple-value-bind (clean stripped)
      (nodecode-cjk-sanitizer:sanitize-text "hello、world。")
    (is (equal "helloworld" clean) "the marks left orphaned go too")
    (is (= 2 stripped) "and are counted"))
  (multiple-value-bind (clean stripped)
      (nodecode-cjk-sanitizer:sanitize-text "hello、world。"
                                            :chinese nil :cyrillic nil :punctuation nil)
    (is (equal "hello、world。" clean) "every switch off leaves the text alone")
    (is (zerop stripped)))
  (multiple-value-bind (clean stripped)
      (nodecode-cjk-sanitizer:sanitize-text "完成。ok" :punctuation nil)
    (is (equal "。ok" clean) "punctuation kept by itself")
    (is (= 2 stripped))))

(deftest cjk-sanitizer-tidies-what-a-run-left-behind ()
  (is (equal "a b" (nodecode-cjk-sanitizer:sanitize-text "a   b"))
      "a run of spaces comes to one")
  (is (equal (format nil "a~%~%b")
             (nodecode-cjk-sanitizer:sanitize-text (format nil "a~%~%~%~%~%~%b")))
      "five newlines keep one blank line, as the original's three did")
  (is (equal (format nil "a~%b")
             (nodecode-cjk-sanitizer:sanitize-text (format nil "a~%~%~%~%b") :blank-lines 1))
      "blank_lines 1 keeps no blank line")
  (is (equal "a   b" (nodecode-cjk-sanitizer:sanitize-text "a   b" :collapse nil))
      "and collapse_space off leaves the spacing as it came"))
