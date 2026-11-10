;;;; verbs.lisp --- the three tools the model calls, their arguments, their answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin registered edinet_search_filings, edinet_financials and
;;;; edinet_save_document under the same names, so a model that read the
;;;; plugin's skill finds them here unchanged. Each answers text: the model
;;;; reads it, cites it, and calls again with a narrower range.

(in-package #:nodecode-edinet)

(nlk:define-contract search-call
  (:doc "Search EDINET, Japan's disclosure system, for the documents filed on a range of FILE
DATES (the day a document was submitted, not its fiscal period). Filter by company_name,
sec_code, edinet_code or corporate_number — at least one is required — and optionally by
doc_types. Answers one line per filing: doc id, securities code, filer, document kind and
description, the submission time, and which files it carries; then the source line to cite.
company_name is a substring match, so prefer sec_code or edinet_code for one issuer.")
  ("date_from" :text :required t
   :doc "the first file date, YYYY-MM-DD, inclusive")
  ("date_to" :text :required t
   :doc "the last file date, YYYY-MM-DD, inclusive; EDINET files nothing later than today in Japan")
  ("company_name" :string
   :doc "part of the filer's name, in Japanese or English (トヨタ, Sony); a substring match")
  ("sec_code" :string
   :doc "the securities code: four digits (7203) or EDINET's five (72030)")
  ("edinet_code" :string
   :doc "the issuer's EDINET code, such as E02144")
  ("corporate_number" :string
   :doc "the 13-digit corporate number (法人番号)")
  ("doc_types" :array :of :string
   :doc "document kinds: yuho or annual (the annual securities report), quarterly, semiannual, extraordinary, amendment — or a docTypeCode such as 120")
  ("limit" :integer :default 50
   :doc "how many filings to answer with, 1 to 200; the scan stops at the limit"))

(nlk:define-contract financial-call
  (:doc "Read one EDINET filing's headline figures from its CSV (the type=5 body): revenue,
operating income, net income, total assets, net assets or equity, and operating cash flow,
each with its unit, its row label and the accounting standard the filing is in (Japan GAAP,
IFRS or US GAAP). Take the doc_id from edinet_search_filings. A figure this cell cannot find
in a consolidated current-year row is reported missing, never guessed as zero; the PDF
(type=2) is where to look it up by hand.")
  ("doc_id" :text :required t
   :doc "the eight-character EDINET document id, such as S100TR7I"))

(nlk:define-contract save-call
  (:doc "Save one EDINET document under the Nodecode home: type 2 is the filing's PDF, type 1
the submission ZIP, type 5 the CSV ZIP. Answers the absolute path, the byte count and the
source line to cite. Nothing is summarised here: read the saved file for its contents.")
  ("doc_id" :text :required t
   :doc "the eight-character EDINET document id, such as S100TR7I")
  ("type" :integer :default 2 :slot document-type
   :doc "2 the PDF, 1 the submission ZIP, 5 the CSV ZIP"))

(defparameter +doc-id-length+ 8
  "An EDINET document id is eight characters: S100TR7I.")

(defun normalize-doc-id (text)
  "TEXT as an EDINET document id, or a refusal naming the shape."
  (let ((text (and (stringp text) (string-upcase (nlk:trimmed text)))))
    (unless (and text
                 (= (length text) +doc-id-length+)
                 (char= (char text 0) #\S)
                 (every (lambda (char) (or (digit-char-p char) (find char "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
                        text))
      (fail "doc_id is the eight-character EDINET document id (for example S100TR7I); the call said ~s."
            (or text "nothing")))
    text))

;;; --- edinet_search_filings ----------------------------------------------------

(defun search-filings (call)
  "Search file dates for one company's filings: an EDINET-SEARCH-CALL answered as text."
  (search-days call))

;;; --- edinet_financials ---------------------------------------------------------

(defun financials (call)
  "Read one filing's headline figures: a FINANCIAL-CALL answered as text."
  (let* ((doc-id (normalize-doc-id (financial-call-doc-id call)))
         (octets (fetch-document doc-id 5)))
    (financials-text (csv-rows octets) :doc-id doc-id)))

;;; --- edinet_save_document ------------------------------------------------------

(defparameter +document-types+
  '((1 . "zip") (2 . "pdf") (5 . "zip"))
  "The document types this cell saves, each as its file extension.")

(defun document-path (doc-id type)
  "Where DOC-ID of TYPE is written: a folder of this cell's own under the home."
  (nlk:home (format nil "edinet/docs/~a_type~a.~a" doc-id type (cdr (assoc type +document-types+)))))

(defun save-document (call)
  "Save one document byte for byte: a SAVE-CALL answered as text."
  (let* ((doc-id (normalize-doc-id (save-call-doc-id call)))
         (type (or (save-call-document-type call) 2)))
    (unless (assoc type +document-types+)
      (fail "type is 2 (the PDF), 1 (the submission ZIP) or 5 (the CSV ZIP); the call said ~a." type))
    (let ((path (document-path doc-id type))
          (octets (fetch-document doc-id type)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
        (write-sequence octets out))
      (format nil "saved ~a (~:d bytes)~%~a~%~a"
              (namestring path) (length octets)
              (case type
                (2 "the filing as the company submitted it, financial statements included")
                (1 "the submission ZIP: the XBRL and the PDF together")
                (5 "the CSV ZIP: one tab-separated row per XBRL fact (UTF-16)")
                (t ""))
              (citation doc-id)))))
