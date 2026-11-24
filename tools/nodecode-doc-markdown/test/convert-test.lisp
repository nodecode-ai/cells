;;;; convert-test.lisp --- the readers, the guard, and the cut.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: a DEFLATE stream and a zip of stored members come
;;;; apart; a .docx, a .xlsx, a .pptx, a PDF, an HTML page and a CSV read as
;;;; the Markdown they carry; the guard refuses what the original refused;
;;;; and max_output_chars is a cut with a visible marker.

(in-package #:nodecode.test)

(deftest doc-markdown-inflates-a-deflate-stream-and-reads-a-zip ()
  (let* ((text (doc-octets "hello"))
         (stored (make-array (+ 5 (length text)) :element-type '(unsigned-byte 8))))
    ;; a final stored block: BFINAL 1, BTYPE 00, then LEN, its complement, bytes
    (setf (aref stored 0) #x01
          (aref stored 1) (length text)
          (aref stored 2) 0
          (aref stored 3) (- 256 (length text) 1)
          (aref stored 4) #xFF)
    (replace stored text :start1 5)
    (is (equalp (doc-octets "hello") (nodecode-doc-markdown:inflate stored)) "a stored block inflates"))
  (let ((archive (doc-zip (list (cons "a.txt" "one") (cons "b/c.txt" "two")))))
    (is (equal '("a.txt" "b/c.txt") (nodecode-doc-markdown:zip-names archive)) "the directory lists the members")
    (is (equal "two" (nodecode-doc-markdown::utf8-text
                      (nodecode-doc-markdown:zip-part archive "b/c.txt") "b/c.txt")))
    (is (null (nodecode-doc-markdown:zip-part archive "nope")) "a member it does not hold is NIL")))

(deftest doc-markdown-reads-text-and-csv ()
  (with-cell-stop ((doc-markdown-start))
    (with-doc-file (path "notes.md" (format nil "# Notes~%~%one line"))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("# Notes") ("one line" "a markdown file is handed on as it is")))
    (with-doc-file (path "table.csv"
                          (format nil "name,count,note~%apples,3,\"two, in quotes\""))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("| name | count | note |") ("| --- | --- | --- |")
        ("| apples | 3 | two, in quotes |" "a quoted comma stays in its field")))))

(deftest doc-markdown-reads-a-docx-as-the-zip-it-is ()
  (with-cell-stop ((doc-markdown-start))
    (with-doc-bytes (path "report.docx" (docx-bytes))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("# Report title" "the heading style became a heading")
        ("the first sentence" "two runs of one paragraph are one line")
        (:absent "<w:" "no XML is left in the answer")
        ("name | count" "a table row is one line of cells")
        ("apples | 3")))))

(deftest doc-markdown-reads-a-workbook-as-a-table ()
  (with-cell-stop ((doc-markdown-start))
    (with-doc-bytes (path "fruit.xlsx" (xlsx-bytes))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("## Fruit" "the sheet name is the heading")
        ("| name | count |")
        ("| --- | --- |")
        ("| apples | 3 |" "a shared string and a number are both cells")))))

(deftest doc-markdown-reads-slides-and-a-pdf ()
  (with-cell-stop ((doc-markdown-start))
    (with-doc-bytes (path "talk.pptx" (pptx-bytes))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("## Slide 1") ("Opening slide")))
    (with-doc-bytes (path "report.pdf" (pdf-bytes))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("Quarterly report" "the text operators are read")
        ("second line" "a T* starts a new line")))))

(deftest doc-markdown-reads-html-without-its-markup ()
  (with-cell-stop ((doc-markdown-start))
    (with-doc-file (path "page.html"
                          (concatenate 'string
                                       "<html><head><title>T</title><style>p{x:1}</style></head><body>"
                                       "<h1>Title</h1><p>first &amp; second</p>"
                                       "<ul><li>one</li></ul><script>var x = 1;</script></body></html>"))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("# Title") ("first & second" "the one character reference a page uses is resolved")
        ("- one")
        (:absent "<p>") (:absent "var x") (:absent "p{x:1}")))))

(deftest doc-markdown-cuts-the-answer-at-max-output-chars ()
  (with-cell-stop ((doc-markdown-start "max_output_chars" 40))
    (with-doc-file (path "long.txt" (make-string 200 :initial-element #\x))
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        ("[truncated: first 40 of 200 characters]")))))

(deftest doc-markdown-a-short-file-is-not-truncated ()
  (with-cell-stop ((doc-markdown-start "max_output_chars" 40))
    (with-doc-file (path "short.txt" "short enough")
      (is-carrying (text (nodecode-doc-markdown:convert-file path))
        (:absent "truncated") ("short enough")))))

(deftest doc-markdown-the-guard-refuses-what-the-original-refused ()
  (with-cell-stop ((doc-markdown-start))
    (flet ((refused (path needle)
             (is (search needle
                         (refusal-text nodecode-doc-markdown:doc-markdown-error
                                       (nodecode-doc-markdown:convert-file path))))))
      (refused "notes.txt" "is not an absolute path")
      (refused "https://example.com/a.pdf" "reads local files only")
      (refused "/tmp/nl-doc-markdown-there-is-no-such-file.pdf" "no file at the given path"))
    (with-doc-file (path "tool.exe" "MZ")
      (is (search "is not in this cell's supported set"
                  (refusal-text nodecode-doc-markdown:doc-markdown-error
                                (nodecode-doc-markdown:convert-file path)))))
    (with-temp-trees ((dir "doc"))
      (ensure-directories-exist dir)
      (let ((hidden (format nil "~a.hidden/kept.txt" (namestring (uiop:ensure-directory-pathname dir)))))
        (ensure-directories-exist hidden)
        (doc-write hidden (doc-octets "text"))
        (is (search "starts with '.'"
                    (refusal-text nodecode-doc-markdown:doc-markdown-error
                                  (nodecode-doc-markdown:convert-file hidden))))))))

(deftest doc-markdown-a-file-inside-the-nodecode-home-is-refused ()
  (with-cell-stop ((doc-markdown-start "refuse_hidden" nil))
    (let* ((home (namestring (merge-pathnames ".nodecode/docs/" (user-homedir-pathname))))
           (path (format nil "~akept.txt" home)))
      (ensure-directories-exist path)
      (doc-write path (doc-octets "text"))
      (is (search "inside the Nodecode home folder are not read"
                  (refusal-text nodecode-doc-markdown:doc-markdown-error
                                (nodecode-doc-markdown:convert-file path))) "the home is refused with the hidden check off"))))
