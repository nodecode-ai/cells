;;;; support.lisp --- the doc-markdown test slice, and the fixtures it builds.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a DOC-MARKDOWN
;;;; name prefix; RUN-DOC-MARKDOWN-TESTS runs exactly that slice.
;;;;
;;;; Nothing is dialled and nothing of the cell is stubbed: the fixtures are
;;;; files this process writes. A .docx, .xlsx or .pptx is built here as the
;;;; zip it is -- stored members, so the fixture needs no compressor of its
;;;; own -- and a PDF with an uncompressed content stream, which is exactly
;;;; what the reader has to find.

(in-package #:nodecode.test)

(define-test-slice "doc-markdown" "DOC-MARKDOWN" :start nodecode-doc-markdown:start-cell)

;;; --- bytes on disk -----------------------------------------------------------------

(defun doc-octets (string)
  (nodecode-doc-markdown::octets-of-string string))

(defun doc-write (path octets)
  "OCTETS written to PATH, a namestring whose folder already exists."
  (with-open-file (out path :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :supersede)
    (write-sequence octets out))
  path)

(defmacro with-doc-bytes ((place name octets) &body body)
  "BODY with PLACE naming a fresh file called NAME holding OCTETS."
  `(with-temp-trees ((dir "doc"))
     (ensure-directories-exist dir)
     (let ((,place (format nil "~a~a" (namestring (uiop:ensure-directory-pathname dir)) ,name)))
       (doc-write ,place ,octets)
       ,@body)))

(defmacro with-doc-file ((place name contents) &body body)
  "BODY with PLACE naming a fresh file called NAME holding CONTENTS as UTF-8."
  `(with-doc-bytes (,place ,name (doc-octets ,contents))
     ,@body))

;;; --- a zip of stored members ---------------------------------------------------------
;;;
;;; Every offset and size the format asks for, so the cell's own reader finds
;;; the central directory the way it finds a real document's. The checksums
;;; are left zero: a reader that verifies them would want a CRC-32 table, and
;;; this cell reads documents rather than checking them.

(defun doc-put-u16 (out value)
  (vector-push-extend (ldb (byte 8 0) value) out)
  (vector-push-extend (ldb (byte 8 8) value) out))

(defun doc-put-u32 (out value)
  (dotimes (index 4) (vector-push-extend (ldb (byte 8 (* 8 index)) value) out)))

(defun doc-put-octets (out octets)
  (loop for byte across octets do (vector-push-extend byte out)))

(defun doc-zip (entries)
  "ENTRIES, an alist of (name . string), as the bytes of a zip of stored
members, in the order given."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
        (records '()))
    (dolist (entry entries)
      (let* ((name (doc-octets (car entry)))
             (data (doc-octets (cdr entry)))
             (offset (fill-pointer out)))
        (doc-put-u32 out #x04034b50)
        (doc-put-u16 out 20)
        (doc-put-u16 out 0)
        (doc-put-u16 out 0)
        (doc-put-u16 out 0)
        (doc-put-u16 out 0)
        (doc-put-u32 out 0)
        (doc-put-u32 out (length data))
        (doc-put-u32 out (length data))
        (doc-put-u16 out (length name))
        (doc-put-u16 out 0)
        (doc-put-octets out name)
        (doc-put-octets out data)
        (push (list name data offset) records)))
    (let ((directory (fill-pointer out)))
      (dolist (record (reverse records))
        (destructuring-bind (name data offset) record
          (doc-put-u32 out #x02014b50)
          (doc-put-u16 out 20)
          (doc-put-u16 out 20)
          (doc-put-u16 out 0)
          (doc-put-u16 out 0)
          (doc-put-u16 out 0)
          (doc-put-u16 out 0)
          (doc-put-u32 out 0)
          (doc-put-u32 out (length data))
          (doc-put-u32 out (length data))
          (doc-put-u16 out (length name))
          (doc-put-u16 out 0)
          (doc-put-u16 out 0)
          (doc-put-u16 out 0)
          (doc-put-u16 out 0)
          (doc-put-u32 out 0)
          (doc-put-u32 out offset)
          (doc-put-octets out name)))
      (let ((size (- (fill-pointer out) directory))
            (count (length records)))
        (doc-put-u32 out #x06054b50)
        (doc-put-u16 out 0)
        (doc-put-u16 out 0)
        (doc-put-u16 out count)
        (doc-put-u16 out count)
        (doc-put-u32 out size)
        (doc-put-u32 out directory)
        (doc-put-u16 out 0)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; --- the fixtures ----------------------------------------------------------------------

(defparameter +docx-document+
  (concatenate 'string
               "<w:document xmlns:w=\"urn:w\"><w:body>"
               "<w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr>"
               "<w:r><w:t>Report title</w:t></w:r></w:p>"
               "<w:p><w:r><w:t>the first </w:t></w:r><w:r><w:t>sentence</w:t></w:r>"
               "<w:r><w:tab/><w:t>after a tab</w:t></w:r></w:p>"
               "<w:tbl><w:tr>"
               "<w:tc><w:p><w:r><w:t>name</w:t></w:r></w:p></w:tc>"
               "<w:tc><w:p><w:r><w:t>count</w:t></w:r></w:p></w:tc>"
               "</w:tr><w:tr>"
               "<w:tc><w:p><w:r><w:t>apples</w:t></w:r></w:p></w:tc>"
               "<w:tc><w:p><w:r><w:t>3</w:t></w:r></w:p></w:tc>"
               "</w:tr></w:tbl>"
               "</w:body></w:document>")
  "A .docx body: one heading, one paragraph of two runs and a tab, and a two-row table.")

(defun docx-bytes ()
  (doc-zip (list (cons "[Content_Types].xml" "<Types/>")
                 (cons "word/document.xml" +docx-document+))))

(defparameter +xlsx-sheet+
  (concatenate 'string
               "<worksheet xmlns=\"urn:x\"><sheetData>"
               "<row r=\"1\"><c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\" t=\"s\"><v>1</v></c></row>"
               "<row r=\"2\"><c r=\"A2\" t=\"inlineStr\"><is><t>apples</t></is></c>"
               "<c r=\"B2\"><v>3</v></c></row>"
               "</sheetData></worksheet>")
  "One worksheet: a shared-string header row and one row of an inline string and a number.")

(defun xlsx-bytes ()
  (doc-zip (list (cons "xl/workbook.xml"
                        "<workbook xmlns:r=\"urn:r\"><sheets><sheet name=\"Fruit\" sheetId=\"1\"/></sheets></workbook>")
                 (cons "xl/sharedStrings.xml"
                       "<sst><si><t>name</t></si><si><t>count</t></si></sst>")
                 (cons "xl/worksheets/sheet1.xml" +xlsx-sheet+))))

(defun pptx-bytes ()
  (doc-zip (list (cons "ppt/slides/slide1.xml"
                        (concatenate 'string
                                     "<p:sld xmlns:p=\"urn:p\" xmlns:a=\"urn:a\"><p:cSld>"
                                     "<a:p><a:r><a:t>Opening slide</a:t></a:r></a:p>"
                                     "</p:cSld></p:sld>")))))

(defun pdf-bytes ()
  "A one-page PDF whose content stream is not compressed: the shape a reader
finds without inflating anything."
  (doc-octets
   (concatenate 'string
                "%PDF-1.4\n"
                "1 0 obj\n<< /Length 60 >>\nstream\n"
                "BT /F1 12 Tf 72 720 Td (Quarterly report) Tj T* (second line) Tj ET\n"
                "endstream\nendobj\n"
                "trailer\n<< /Root 1 0 R >>\n%%EOF\n")))

;;; --- the tool, as the provider calls it ------------------------------------------------

(defun doc-call (path &key max-chars)
  "PATH passed to doc_convert as a provider's tool call passes it; the answer
as the model reads it."
  (nle::message-content
   (execute-wire-call "doc-call" "doc_convert"
                      (format nil "{\"path\": ~s~@[, \"max_chars\": ~d~]}"
                              (namestring path) max-chars))))

(defun doc-refusal (path)
  "The text a refused doc_convert answers the model."
  (doc-call path))
