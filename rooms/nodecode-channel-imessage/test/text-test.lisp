;;;; text-test.lisp --- Markdown as plain text, pure.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest channel-imessage-markdown-reads-as-typed ()
  (is (equal "Title" (nci:markdown-plain "## Title ##")) "a heading is its words")
  (is (equal "Some bold and italic text" (nci:markdown-plain "Some **bold** and *italic* text")))
  (is (equal "snake_case_name and 2*3*4" (nci:markdown-plain "snake_case_name and 2*3*4"))
      "marks inside words are not emphasis")
  (is (equal "keep co**de** as is" (nci:markdown-plain "keep `co**de**` as is")) "a code span is kept, its ticks gone")
  (is (equal "see the docs (https://x.com/a)" (nci:markdown-plain "see [the docs](https://x.com/a)")))
  (is (equal "https://y.com" (nci:markdown-plain "[https://y.com](https://y.com)")) "a bare link is its address")
  (is (equal (format nil "~a one~%~a done~%~a open" (code-char #x2022) (code-char #x2611) (code-char #x2610))
             (nci:markdown-plain (format nil "- one~%- [x] done~%* [ ] open"))))
  (is (equal (format nil "(defun **x** ())~%after")
             (nci:markdown-plain (format nil "```lisp~%(defun **x** ())~%```~%after")))
      "a fenced block's lines are kept as they are, its fences gone")
  (is (equal (format nil "| a | b |~%| 1 | 2 |~%~a" (code-char #x2014))
             (nci:markdown-plain (format nil "| a | b |~%|---|:---:|~%| 1 | 2 |~%---")))
      "a table loses its separator row; a rule is a dash")
  (is (equal (format nil "a~%~%b") (nci:markdown-plain (format nil "a~%~%~%~%b")))))
