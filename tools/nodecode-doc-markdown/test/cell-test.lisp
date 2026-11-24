;;;; cell-test.lisp --- the lifecycle, the tool as a provider calls it, /doc.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the start installs one tool, one command and one
;;;; manual topic and the stop takes all three back; a refused section
;;;; installs nothing; doc_convert answers a real file through the wire, and
;;;; its refusals answer rather than signal; and /doc answers the same file
;;;; from the shell, and says what it wants when it is typed bare.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "doc-markdown"
  (:config)
  (:help :doc-markdown)
  (:command "doc")
  (:running
   (is (= 20971520 (nodecode-doc-markdown::setting :max-file-bytes)) "20 MiB is the cap")
   (is (= 200000 (nodecode-doc-markdown::setting :max-output-chars)) "200000 characters is the cut")
   (is (eq t (nodecode-doc-markdown::setting :refuse-hidden)) "a hidden component is refused"))
  (:refused ("max_file_bytes" 0) ("max_output_chars" 0)))

(deftest doc-markdown-the-tool-answers-a-file-through-the-wire ()
  (with-cell-stop ((doc-markdown-start))
    (with-doc-file (path "notes.txt" "hello from a text file")
      (is-carrying (answer (doc-call path))
        ("Converted notes.txt (txt, 22 bytes)" "the answer names the file, its type and its size")
        ("hello from a text file")))
    (with-doc-bytes (path "a.docx" (docx-bytes))
      (is-carrying (answer (doc-call path))
        ("Converted a.docx (docx") ("# Report title")))
    (with-doc-bytes (path "b.xlsx" (xlsx-bytes))
      (is-carrying (answer (doc-call path)) ("| apples | 3 |")))))

(deftest doc-markdown-the-tool-answers-its-refusals ()
  (with-cell-stop ((doc-markdown-start))
    (is-carrying (text (doc-call "/tmp/nl-doc-markdown-nowhere.pdf"))
      ("ERROR: doc_convert: file_not_found" "a missing file is answered, not signalled"))
    (with-doc-file (path "notes.txt" "hello")
      (is-carrying (answer (doc-call path :max-chars 0))
        ("max_chars must be 1 or more"))
      (is (search "\"path\"" (nle::message-content
                              (execute-wire-call "doc-call-required" "doc_convert" "{}"))) "a call with no path at all is refused"))))

(deftest doc-markdown-the-command-answers-a-file ()
  (with-cell-stop ((doc-markdown-start))
    (with-doc-file (path "notes.txt" "hello from a text file")
      (is-carrying (text (nodecode-doc-markdown:run-doc (namestring path) nil))
        ("doc: notes.txt (txt, 22 bytes)") ("hello from a text file")))
    (is-carrying (text (nodecode-doc-markdown:run-doc "" nil))
      ("doc: name a file to convert" "typed bare, it says what it wants"))
    (is-carrying (text (nodecode-doc-markdown:run-doc "/tmp/nl-doc-markdown-nowhere.pdf" nil))
      ("doc: file_not_found" "a refusal is answered in the same line"))))
