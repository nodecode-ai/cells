;;;; cell-test.lisp --- the office cell against files it makes itself.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test writes its own .docx or .pptx with the cell's own ZIP
;;;; writer, so nothing is read from the machine, nothing is downloaded and
;;;; no Office program is asked for anything. Each tool is called the way a
;;;; model's call reaches it - through the real dispatch, contract and all -
;;;; so the schema the provider is shown and the body that runs are one
;;;; shape.

(in-package #:nodecode.test)

(define-test-slice "office" "OFFICE-CELL-"
  :start nodecode-office:start-cell
  :defaults (("folders" (vector (namestring (nlk:home "office-test/"))))))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +document+ "word/document.xml")

(defun office-desk-dir ()
  (let ((dir (nlk:home "office-test/")))
    (ensure-directories-exist (merge-pathnames "x.docx" dir))
    (namestring dir)))

(defun office-entry (name xml)
  (nodecode-office::make-zip-entry name :data (nodecode-office::octets-of xml)))

(defun office-write-docx (path &optional (text "Hello world"))
  "A one-paragraph Word document holding TEXT, written by the cell's writer."
  (nodecode-office::write-octets
   path
   (nodecode-office::repack
    (list (office-entry "[Content_Types].xml" "<Types/>")
          (office-entry +document+
                         (concatenate 'string "<w:document><w:body><w:p><w:r><w:t>"
                                      text "</w:t></w:r></w:p></w:body></w:document>"))))))

(defun office-write-pptx (path)
  (nodecode-office::write-octets
   path
   (nodecode-office::repack
    (list (office-entry "ppt/slides/slide1.xml"
                        "<p:sld><a:p><a:r><a:t>A deck title</a:t></a:r></a:p></p:sld>")
          (office-entry "ppt/slides/slide2.xml"
                        "<p:sld><a:p><a:r><a:t>Second slide</a:t></a:r></a:p></p:sld>")))))

(defun office-fixture ()
  "The desk directory, holding a fresh Word and PowerPoint file."
  (let ((dir (office-desk-dir)))
    (office-write-docx (merge-pathnames "x.docx" dir))
    (office-write-pptx (merge-pathnames "y.pptx" dir))
    dir))

(defun office-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun office-object (id tool arguments)
  (nlk:decode-json (office-answer id tool arguments)))

(defun office-text (path)
  (nodecode-office::zip-entry-text
   (nodecode-office::find-part (nodecode-office::archive path) +document+)))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "office"
  (:help :office)
  (:command "office")
  (:refused ("folders" (vector)))
  (:idle nodecode-office:office-error (nodecode-office::require-running)))

;;; --- office_inspect and office_search ------------------------------------------------

(deftest office-cell-inspects-a-word-file ()
  (let ((dir (office-fixture)))
    (with-cell-stop ((office-start))
      (let ((object (office-object "office-inspect-1" "office_inspect"
                                    (format nil "{\"path\":~s}"
                                            (namestring (merge-pathnames "x.docx" dir))))))
        (is (eq t (nlk:json-value object :boolean "ok")))
        (is (equal "docx" (nlk:json-value object :string "kind")))
        (is (= 1 (nlk:json-value object :integer "blocks")) "one paragraph, one block")
        (is (search "Hello world" (nlk:encode-json-object object)) "the paragraph's text is answered back")))))

(deftest office-cell-inspects-a-deck ()
  (let ((dir (office-fixture)))
    (with-cell-stop ((office-start))
      (let ((object (office-object "office-inspect-2" "office_inspect"
                                    (format nil "{\"path\":~s}"
                                            (namestring (merge-pathnames "y.pptx" dir))))))
        (is (equal "pptx" (nlk:json-value object :string "kind")))
        (is (= 2 (nlk:json-value object :integer "slides")) "both slides are counted")
        (is (search "A deck title" (nlk:encode-json-object object)) "a slide's text is read")))))

(deftest office-cell-searches-by-location ()
  (let ((dir (office-fixture)))
    (with-cell-stop ((office-start))
      (let ((object (office-object "office-search-1" "office_search"
                                    (format nil "{\"path\":~s,\"query\":\"world\"}"
                                            (namestring (merge-pathnames "x.docx" dir))))))
        (is (= 1 (nlk:json-value object :integer "count")))
        (is (equal "paragraph 0"
                   (nlk:json-value (aref (nlk:json-array object "hits") 0) :string "where")) "a hit says where it is, not only that it is"))
      (let ((answer (office-answer "office-search-2" "office_search"
                                    (format nil "{\"path\":~s,\"query\":\"nowhere\"}"
                                            (namestring (merge-pathnames "x.docx" dir))))))
        (is (search "\"count\":0" answer) "a phrase that is not there answers no hit")))))

;;; --- office_replace and office_receipt ------------------------------------------------

(deftest office-cell-replaces-and-reverts ()
  (let* ((dir (office-fixture))
         (path (merge-pathnames "x.docx" dir)))
    (with-cell-stop ((office-start))
      (let ((object (office-object "office-replace-1" "office_replace"
                                    (format nil "{\"path\":~s,\"find\":\"Hello\",\"replace\":\"Goodbye\"}"
                                            (namestring path)))))
        (is (eq t (nlk:json-value object :boolean "ok")))
        (is (= 1 (nlk:json-value object :integer "replaced")))
        (is (search "Goodbye world" (office-text path)) "the file on disk really changed")
        (let ((id (nlk:json-value object :integer "receipt")))
          (is (integerp id) "the write left a receipt")
          (is (search "Goodbye"
                      (getf (first (nodecode-office::document-blocks
                                    (nodecode-office::archive path) :docx))
                            :text)) "and the document reads back with the new word")
          (let ((revert (office-object "office-receipt-1" "office_receipt"
                                        (format nil "{\"action\":\"revert\",\"id\":~d}" id))))
            (is (eq t (nlk:json-value revert :boolean "ok")))
            (is (= id (nlk:json-value revert :integer "reverted")))
            (is (search "Hello world" (office-text path)) "revert put the file back byte for byte")))))))

(deftest office-cell-refuses-text-that-is-not-one-run ()
  (let ((dir (office-fixture)))
    (with-cell-stop ((office-start))
      (let ((answer (office-answer "office-replace-2" "office_replace"
                                    (format nil "{\"path\":~s,\"find\":\"absent phrase\",\"replace\":\"x\"}"
                                            (namestring (merge-pathnames "x.docx" dir))))))
        (is (search "office_search" answer) "the refusal says how to find the text first")))))

;;; --- docx_tracked_edit ----------------------------------------------------------------

(deftest office-cell-leaves-a-tracked-change ()
  (let* ((dir (office-fixture))
         (path (merge-pathnames "x.docx" dir)))
    (with-cell-stop ((office-start))
      (let ((object (office-object "office-tracked-1" "docx_tracked_edit"
                                    (format nil "{\"path\":~s,\"find\":\"Hello\",\"replace\":\"Goodbye\",\"author\":\"Reviewer\"}"
                                            (namestring path)))))
        (is (eq t (nlk:json-value object :boolean "ok")))
        (is (= 1 (nlk:json-value object :integer "revisions")) "one revision is pending")
        (is (equal "Reviewer" (nlk:json-value object :string "author")))
        (let ((xml (office-text path)))
          (is (search "<w:del " xml) "the old text is a tracked deletion")
          (is (search "<w:ins " xml) "the new text is a tracked insertion")
          (is (search "w:author=\"Reviewer\"" xml) "both carry the author")
          (is (search "Goodbye" xml) "and the new text is in the file"))))))

(deftest office-cell-refuses-a-tracked-edit-to-a-deck ()
  (let ((dir (office-fixture)))
    (with-cell-stop ((office-start))
      (let ((answer (office-answer "office-tracked-2" "docx_tracked_edit"
                                    (format nil "{\"path\":~s,\"find\":\"A deck title\",\"replace\":\"x\"}"
                                            (namestring (merge-pathnames "y.pptx" dir))))))
        (is (search "office_replace" answer) "a .pptx is sent to the tool that edits it")))))

;;; --- the desk guard ----------------------------------------------------------------------------------------------------------------------------

(deftest office-cell-refuses-a-file-off-the-desk ()
  (let ((dir (office-fixture)))
    (with-cell-stop ((office-start))
      (let ((answer (office-answer "office-inspect-3" "office_inspect"
                                    (format nil "{\"path\":~s}" (namestring (merge-pathnames "elsewhere.docx" (nlk:home "not-on-the-desk/")))))))
        (is (search "not inside a folder on the desk" answer) "a write outside the desk never starts"))
      (is (search "Hello world" (office-text (merge-pathnames "x.docx" dir))) "and the file beside it is untouched"))))

(deftest office-cell-refuses-a-missing-path ()
  (with-cell-stop ((office-start))
    (let ((answer (office-answer "office-inspect-4" "office_inspect" "{}")))
      (is (search "path" answer) "the contract refuses before the body runs"))))
