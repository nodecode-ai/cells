;;;; cell-test.lisp --- the iwork cell with the apps stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here is a Mac, no app is asked for anything and nothing is
;;;; downloaded: the cell's one outside seam, the osascript runner, is rebound
;;;; to a function that answers a canned object per operation and records what
;;;; it was asked. Each tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "iwork" "IWORK-CELL-"
  :start nodecode-iwork:start-cell
  :defaults (("folders" (vector (namestring (nlk:home "iwork-test/"))))))

;;; --- fixtures ----------------------------------------------------------------------

(defun iwork-octets (text) "TEXT as the octets a file holds."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun iwork-folder () "The folder the test's files live in, made afresh."
  (let ((dir (nlk:home "iwork-test/")))
    (ensure-directories-exist (merge-pathnames "x.numbers" dir))
    (namestring dir)))

(defun iwork-file (&optional (name "x.numbers") (content "PK original")) "A file of the test's own, holding CONTENT. It is not a real iWork file:
no app ever opens it, and the stubs answer as an app would."
  (let ((path (merge-pathnames name (iwork-folder))))
    (nodecode-iwork::write-octets path (iwork-octets content))
    path))

(defun iwork-answer (id tool arguments) "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun iwork-object (id tool arguments)
  (let ((answer (iwork-answer id tool arguments)))
    (or (ignore-errors (nlk:decode-json answer))
        (error "the tool refused: ~a" answer))))



(defparameter *iwork-calls* nil "What the stub was asked to run, newest first: (OPERATION . PAYLOAD).")
(defun iwork-asked (op) "The payload the stub was last asked to run OP with, decoded."
  (nlk:decode-json (cdr (assoc op *iwork-calls*))))

(defmacro with-iwork-stub ((&rest answer-pairs) &body body) "Run BODY with the osascript seam answering ANSWER-PAIRS, each (OPERATION FORM):
FORM's value is the object to answer, or a function of the payload to call."
  `(let ((nodecode-iwork::*osascript*
           (lambda (op script payload)
             (declare (ignore script))
             (push (cons op payload) *iwork-calls*)
             (let ((answer (case op
                             ,@(loop for (key form) in answer-pairs
                                     collect `(,key ,form))
                             (t (error "the stub has no answer for ~s" op)))))
               (if (functionp answer) (funcall answer payload) answer))))
         (*iwork-calls* nil))
     ,@body))

(defun iwork-editing-app (bytes) "A stub standing in for the app's edit: it writes BYTES to the file the
payload names, the way Numbers saves what it was told."
  (lambda (payload)
    (let* ((object (nlk:decode-json payload))
           (path (nlk:json-value object :string "path")))
      (nodecode-iwork::write-octets path (iwork-octets bytes))
      (nlk:json-object "ok" t "wrote" bytes))))

(defun iwork-table-answer (cells) "A read-numbers answer holding one table whose first cells are CELLS."
  (nlk:json-object "ok" t "kind" "numbers" "sheets" (vector "Sheet 1") "tables" (vector "Sheet 1/Table 1") "table" "Table 1" "rows" (vector (coerce cells 'vector))))

;;; --- lifecycle ------------------------------------------------------------------------

(define-cell-lifecycle-tests "iwork"
  (:help :iwork)
  (:command "iwork")
  (:refused ("folders" (vector)))
  (:idle nodecode-iwork:iwork-error (nodecode-iwork::require-running)))

;;; --- iwork_read -----------------------------------------------------------------------

(deftest iwork-cell-reads-a-table ()
  (let ((path (iwork-file)))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:read-numbers (iwork-table-answer '("Region" "Sales" "North" "1200"))))
        (let ((object (iwork-object "iwork-read-1" "iwork_read"
                                     (format nil "{\"path\":~s,\"range\":\"A1:B2\"}"
                                             (namestring path)))))
          (is (eq t (nlk:json-value object :boolean "ok")))
          (is (equal "numbers" (nlk:json-value object :string "kind")))
          (is (search "1200" (nlk:encode-json-object object)) "the cells are answered back")
          (is (equal "A1:B2" (nlk:json-value (iwork-asked :read-numbers) :string "range")) "the range asked for is the range the app is given")
          (is (equal (namestring path)
                     (nlk:json-value (iwork-asked :read-numbers) :string "path")) "and the file named is the one fenced and asked for"))))))

(deftest iwork-cell-reads-a-deck ()
  (let ((path (iwork-file "deck.key" "PK deck")))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:read-keynote
                         (nlk:json-object "ok" t "kind" "keynote" "slides" (vector (nlk:json-object "slide" 1 "title" "Quarterly" "body" "Revenue is up")))))
        (let ((object (iwork-object "iwork-read-2" "iwork_read"
                                     (format nil "{\"path\":~s}" (namestring path)))))
          (is (equal "keynote" (nlk:json-value object :string "kind")))
          (is (search "Quarterly" (nlk:encode-json-object object)) "a slide's title is read"))))))

(deftest iwork-cell-reads-pages-text ()
  (let ((path (iwork-file "letter.pages" "PK pages")))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:read-pages (nlk:json-object "ok" t "kind" "pages" "text" "Dear Ada, the report is attached.")))
        (let ((object (iwork-object "iwork-read-3" "iwork_read"
                                     (format nil "{\"path\":~s}" (namestring path)))))
          (is (equal "pages" (nlk:json-value object :string "kind")))
          (is (search "Dear Ada" (nlk:json-value object :string "text")) "the body text is read"))))))

;;; --- iwork_set_cell --------------------------------------------------------------------

(deftest iwork-cell-sets-a-cell-and-puts-it-back ()
  (let* ((path (iwork-file))
         (before (nodecode-iwork::read-octets path)))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:set-cell (iwork-editing-app "PK edited"))
                        (:read-numbers (iwork-table-answer '("42"))))
        (let ((object (iwork-object "iwork-set-1" "iwork_set_cell"
                                     (format nil "{\"path\":~s,\"ref\":\"B2\",\"value\":\"42\"}"
                                             (namestring path)))))
          (is (eq t (nlk:json-value object :boolean "ok")))
          (is (equal "42" (nlk:json-value object :string "value")))
          (is (equal "B2" (nlk:json-value (iwork-asked :set-cell) :string "ref")) "the cell asked for is the cell the app is given")
          (is (string= "PK edited"
                       (map 'string #'code-char (nodecode-iwork::read-octets path))) "the app's write is what the file holds")
          (let ((id (nlk:json-value object :integer "receipt")))
            (is (integerp id) "the write left a receipt")
            (let ((list (iwork-object "iwork-backups-1" "iwork_backups" "{}")))
              (is (= 1 (nlk:json-value list :integer "count")))
              (is (search (file-namestring path)
                          (nlk:json-value (aref (nlk:json-array list "receipts") 0) :string "path")) "the receipt names the file"))
            (let ((back (iwork-object "iwork-backups-2" "iwork_backups"
                                      (format nil "{\"action\":\"restore\",\"id\":~d}" id))))
              (is (eq t (nlk:json-value back :boolean "ok")))
              (is (equalp before (nodecode-iwork::read-octets path)) "restore put the file back byte for byte"))))))))

(deftest iwork-cell-takes-back-a-cell-the-app-did-not-keep ()
  (let* ((path (iwork-file))
         (before (nodecode-iwork::read-octets path)))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:set-cell (iwork-editing-app "PK edited"))
                        (:read-numbers (iwork-table-answer '("0"))))
        (let ((answer (iwork-answer "iwork-set-2" "iwork_set_cell"
                                     (format nil "{\"path\":~s,\"ref\":\"B2\",\"value\":\"42\"}"
                                             (namestring path)))))
          (is (search "back as it was" answer) "a cell the app did not keep is a refusal that says so")
          (is (equalp before (nodecode-iwork::read-octets path)) "and the file is the one that was there before the write"))))))

(deftest iwork-cell-dry-run-touches-nothing ()
  (let ((path (iwork-file)))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:set-cell (iwork-editing-app "PK edited")))
        (let ((object (iwork-object "iwork-set-3" "iwork_set_cell"
                                     (format nil "{\"path\":~s,\"ref\":\"B2\",\"value\":\"7\",\"dry_run\":true}"
                                             (namestring path)))))
          (is (eq t (nlk:json-value object :boolean "dry_run")))
          (is (null *iwork-calls*) "no script was run at all")
          (is (string= "PK original"
                       (map 'string #'code-char (nodecode-iwork::read-octets path))) "and the file was not touched"))))))

(deftest iwork-cell-refuses-a-cell-in-a-deck ()
  (let ((path (iwork-file "deck.key" "PK deck")))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:set-cell (iwork-editing-app "PK edited")))
        (let ((answer (iwork-answer "iwork-set-4" "iwork_set_cell"
                                     (format nil "{\"path\":~s,\"ref\":\"B2\",\"value\":\"1\"}"
                                             (namestring path)))))
          (is (search "iwork_set_cell edits .numbers files" answer))
          (is (null *iwork-calls*) "a deck is refused before any app is asked"))))))

;;; --- iwork_slides ----------------------------------------------------------------------

(deftest iwork-cell-adds-a-slide ()
  (let ((path (iwork-file "deck.key" "PK deck")))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:slide
                         (lambda (payload)
                             (let* ((object (nlk:decode-json payload))
                                    (path (nlk:json-value object :string "path")))
                               (nodecode-iwork::write-octets path (iwork-octets "PK two slides"))
                               (nlk:json-object "ok" t "kind" "keynote" "action" "add" "slide" 2 "slides" (vector (nlk:json-object "slide" 1)
                                                                 (nlk:json-object "slide" 2)))))))
        (let ((object (iwork-object "iwork-slides-1" "iwork_slides"
                                     (format nil "{\"path\":~s,\"action\":\"add\",\"title\":\"Costs\"}"
                                             (namestring path)))))
          (is (eq t (nlk:json-value object :boolean "ok")))
          (is (equal "add" (nlk:json-value (iwork-asked :slide) :string "action")))
          (is (equal "Costs" (nlk:json-value (iwork-asked :slide) :string "title")) "the title asked for is the title the app is given")
          (is (string= "PK two slides"
                       (map 'string #'code-char (nodecode-iwork::read-octets path)))))))))

(deftest iwork-cell-refuses-an-empty-slide-write ()
  (let ((path (iwork-file "deck.key" "PK deck")))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:slide (nlk:json-object "ok" t)))
        (let ((answer (iwork-answer "iwork-slides-2" "iwork_slides"
                                     (format nil "{\"path\":~s,\"action\":\"add\"}"
                                             (namestring path)))))
          (is (search "needs a title" answer) "a slide with nothing on it is not made"))))))

;;; --- iwork_export ----------------------------------------------------------------------

(deftest iwork-cell-exports-a-pdf ()
  (let* ((path (iwork-file))
         (output (merge-pathnames "x.pdf" (iwork-folder))))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:export
                         (lambda (payload)
                             (let* ((object (nlk:decode-json payload))
                                    (path (nlk:json-value object :string "output")))
                               (nodecode-iwork::write-octets path (iwork-octets "%PDF-1.4 printed"))
                               (nlk:json-object "ok" t "kind" "export" "output" path "as" "PDF")))))
        (let ((object (iwork-object "iwork-export-1" "iwork_export"
                                     (format nil "{\"path\":~s,\"format\":\"pdf\"}"
                                             (namestring path)))))
          (is (eq t (nlk:json-value object :boolean "ok")))
          (is (equal (namestring output) (nlk:json-value object :string "output")))
          (is (probe-file output) "the exported file is there")
          (is (equal "PDF" (nlk:json-value (iwork-asked :export) :string "as")) "the exporter's own name for the format is what the app is given")))
      (is (search "exports" (iwork-answer "iwork-export-2" "iwork_export"
                                           (format nil "{\"path\":~s,\"format\":\"pptx\"}"
                                                   (namestring path)))) "a .numbers file is not exported to PowerPoint"))))

;;; --- the fence, and the contract ---------------------------------------------------------

(deftest iwork-cell-refuses-a-file-outside-the-fence ()
  (let ((path (iwork-file)))
    (with-cell-stop ((iwork-start))
      (with-iwork-stub ((:read-numbers (iwork-table-answer '("x"))))
        (let ((answer (iwork-answer "iwork-read-4" "iwork_read"
                                     (format nil "{\"path\":~s}"
                                             (namestring (merge-pathnames "elsewhere.numbers"
                                                                           (nlk:home "not-in-the-fence/")))))))
          (is (search "not inside a folder this cell may touch" answer))
          (is (null *iwork-calls*) "an outside file is refused before any app is asked"))
        (is (probe-file path) "and the file in the fence is untouched")))))

(deftest iwork-cell-refuses-a-missing-path ()
  (with-cell-stop ((iwork-start))
    (let ((answer (iwork-answer "iwork-read-5" "iwork_read" "{}")))
      (is (search "path" answer) "the contract refuses before the body runs"))))
