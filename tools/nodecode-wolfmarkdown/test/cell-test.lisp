;;;; cell-test.lisp --- lifecycle; the three checks; the three tools; /markdown.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the start installs one manual topic, one command
;;;; and three tools and the stop takes them back; a clean document passes; an
;;;; unclosed fence and a lost URL fail; a messy document formats to the
;;;; canonical form and formats to itself; an ordered list that was reordered
;;;; fails structure preservation; and the three tools answer through the wire
;;;; exactly as a provider's call reaches them, writing only when told to.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "wolfmarkdown"
  (:config)
  (:help :markdown)
  (:command "markdown")
  (:running
   (is (equal "-" (nodecode-wolfmarkdown::list-style)) "the default marker is a dash")
   (is (= 20 (nodecode-wolfmarkdown::max-issues)) "twenty findings are printed")))

(deftest wolfmarkdown-a-clean-document-passes ()
  (let* ((text (wolfmarkdown-doc "# Title"
                                 ""
                                 "Some prose with https://example.com/a and version 1.2.3."
                                 ""
                                 "## Section"
                                 ""
                                 "- one"
                                 "- two"
                                 ""
                                 "~~~lisp"
                                 "(defun x () 1)"
                                 "~~~"))
         (report (nodecode-wolfmarkdown:verify-text text :path "clean.md")))
    (is (search "Result: PASS" report) "a clean document passes")
    (is (search "Fences: Pass" report) "its fences closed")
    (is (search "Formatting: Pass" report) "and it is already canonical")
    (is (search "Content integrity: Skipped" report) "with no source, integrity is skipped")))

(deftest wolfmarkdown-an-unclosed-fence-fails ()
  (let ((report (nodecode-wolfmarkdown:verify-text
                 (wolfmarkdown-doc "# Title" "" "```" "code"))))
    (is (search "Result: FAIL" report) "an unclosed fence fails the document")
    (is (search "Unclosed fence starting on line 3" report) "and the finding names the line")
    (is (search "Close the opening fence" report) "with the remediation")))

(deftest wolfmarkdown-integrity-catches-a-lost-and-a-changed-token ()
  (let* ((source (wolfmarkdown-doc "Built with https://example.com/docs and version 2.4.1"))
         (lost (nodecode-wolfmarkdown:verify-text
                (wolfmarkdown-doc "Built with version 2.4.1") :source source))
         (changed (nodecode-wolfmarkdown:verify-text
                   (wolfmarkdown-doc "Built with https://example.com/docs and version 2.4.2")
                   :source source)))
    (is (search "Content integrity: Fail" lost) "a lost URL fails integrity")
    (is (search "https://example.com/docs" lost) "the report names the token")
    (is (search "2.4.1" changed) "a version that moved is reported by name")
    (is (search "Structure: Pass" lost) "no ordered list moved")))

(deftest wolfmarkdown-clean-is-canonical-and-idempotent ()
  (let* ((messy (wolfmarkdown-doc "# T   " "" "" "" "text [6][26] here" "* one" "* two" "" "```" "code   " "```" ""))
         (cleaned (nodecode-wolfmarkdown:clean-text messy)))
    (is (search "[6] [26]" cleaned) "adjacent citations are separated")
    (is (search "- one" cleaned) "and the marker is the one this document writes")
    (is (null (search "# T   " cleaned)) "trailing whitespace is gone")
    (multiple-value-bind (again fixes) (nodecode-wolfmarkdown:clean-text cleaned)
      (declare (ignore fixes))
      (is (equal cleaned again) "a second pass changes nothing"))
    (is (search "code   " cleaned) "a fenced block is kept as it was written")))

(deftest wolfmarkdown-structure-keeps-an-ordered-sequence-in-place ()
  (let* ((source (wolfmarkdown-doc "# Rank" "" "1. Alpha" "2. Beta" "3. Gamma"))
         (same (wolfmarkdown-doc "# Rank" "" "1. Alpha" "2. Beta" "3. Gamma"))
         (moved (wolfmarkdown-doc "# Rank" "" "1. Gamma" "2. Alpha" "3. Beta")))
    (let ((kept (nodecode-wolfmarkdown:verify-text same :source source))
          (reordered (nodecode-wolfmarkdown:verify-text moved :source source)))
      (is (search "Structure: Pass" kept) "an unchanged sequence is preserved")
      (is (search "Structure: Fail" reordered) "a reordered one is not")
      (is (search "position" reordered) "and the finding names the position that moved"))))

(deftest wolfmarkdown-the-three-tools-answer-through-the-wire ()
  (with-cell-stop ((wolfmarkdown-start))
    (let* ((path (wolfmarkdown-file "doc.md"
                                    (wolfmarkdown-doc "# Notes" "" "see https://example.com/a" "" "* item")))
           (verified (wolfmarkdown-answer "wolf-1" "markdown_verify"
                                          (format nil "{\"path\": ~s}" path)))
           (written (wolfmarkdown-answer "wolf-2" "markdown_clean"
                                         (format nil "{\"path\": ~s, \"write_file\": true}" path)))
           (outline (wolfmarkdown-answer "wolf-3" "markdown_outline"
                                         (format nil "{\"path\": ~s}" path))))
      (is (search "Operation: Verify" verified) "markdown_verify answers a report")
      (is (search "Result:" verified) "and a verdict")
      (is (search "Wrote: " written) "markdown_clean says whether it wrote")
      (is (null (search "Wrote:" verified)) "a verify writes nothing")
      (is (search "- item" (wolfmarkdown-contents path)) "the file now carries the canonical marker")
      (is (search "Operation: Outline" outline) "markdown_outline answers a source map")
      (is (search "Headings: 1" outline) "with the heading it found"))))

(deftest wolfmarkdown-a-call-with-no-document-refuses ()
  (with-cell-stop ((wolfmarkdown-start))
    (let ((answer (wolfmarkdown-answer "wolf-4" "markdown_verify" "{}")))
      (is (search "give path" answer) "a call with neither path nor text says which to give"))))

(deftest wolfmarkdown-slash-reports-and-checks-a-file ()
  (with-cell-stop ((wolfmarkdown-start))
    (let* ((path (wolfmarkdown-file "slash.md" (wolfmarkdown-doc "# Title" "" "text")))
           (bare (nodecode-wolfmarkdown:run-slash "" nil))
           (checked (nodecode-wolfmarkdown:run-slash (format nil "verify ~a" path) nil)))
      (is (search "markdown: verify, clean and prove" bare) "/markdown bare says what it does")
      (is (search "Result: PASS" checked) "/markdown verify PATH checks that file"))))

