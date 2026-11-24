;;;; cell-test.lisp --- lifecycle, the three tools and /edinet, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches EDINET, needs a
;;;; live key or touches the network. What is proved is the cell's own
;;;; wiring: the section (a healthy start, and a missing key refused), the
;;;; two GETs it speaks, the three tools through the contract the model calls
;;;; them by, the CSV ZIP it reads without a ZIP library, the file it saves,
;;;; and the operator's command.

(in-package #:nodecode.test)

(define-test-slice "edinet" "EDINET-CELL-" :start nodecode-edinet:start-cell
  :defaults (("api_key" "test-key") ("pace_seconds" 0)))

(define-cell-lifecycle-tests "edinet"
  (:help :edinet)
  (:command "edinet")
  (:running (is (equal nodecode-edinet::+primer+ (nle:help :edinet)) "(help :edinet) is the primer"))
  (:refused ("api_key" ""))
  (:idle edinet:edinet-error (edinet:run-command "" nil)))

;;; --- the stubbed wire ------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values BODY STATUS).")

(defmacro with-edinet-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered by *REPLY*."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected EDINET request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun edinet-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun edinet-url (request)
  "The URL of a stubbed REQUEST."
  (second request))

(defun edinet-replies-with (body &optional (status 200))
  "Answer every request with BODY at STATUS."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values body status nil nil))))

(defparameter +list-json+
  "{\"metadata\":{\"status\":\"200\",\"message\":\"OK\",\"resultset\":{\"count\":2}},
    \"results\":[
      {\"docID\":\"S100TR7I\",\"filerName\":\"トヨタ自動車株式会社\",\"secCode\":\"72030\",
       \"edinetCode\":\"E02144\",\"JCN\":\"1180301018771\",\"docTypeCode\":\"120\",
       \"docDescription\":\"有価証券報告書\",\"submitDateTime\":\"2024-06-25 09:01\",
       \"csvFlag\":\"1\",\"pdfFlag\":\"1\"},
      {\"docID\":\"S100ZZZZ\",\"filerName\":\"Another Company\",\"secCode\":\"99990\",
       \"edinetCode\":\"E99999\",\"docTypeCode\":\"120\",\"docDescription\":\"有価証券報告書\",
       \"submitDateTime\":\"2024-06-25 10:00\",\"csvFlag\":\"0\",\"pdfFlag\":\"1\"}]}"
  "A file date's list as EDINET answers one: two filings, one of them the searched issuer's.")

(defun edinet-search-call (&rest pairs)
  "A SEARCH-CALL record from PAIRS."
  (edinet:decode-search-call (apply #'nlk:make-json-object pairs)))

(defun edinet-financial-call (&rest pairs)
  "A FINANCIAL-CALL record from PAIRS."
  (edinet:decode-financial-call (apply #'nlk:make-json-object pairs)))

(defun edinet-save-call (&rest pairs)
  "A SAVE-CALL record from PAIRS."
  (edinet:decode-save-call (apply #'nlk:make-json-object pairs)))

;;; --- a CSV ZIP, built here ---------------------------------------------------------

(defun edinet-u16 (vector at value)
  "VALUE at AT in VECTOR, little-endian."
  (setf (aref vector at) (logand value #xFF)
        (aref vector (1+ at)) (logand (ash value -8) #xFF)))

(defun edinet-u32 (vector at value)
  "VALUE at AT in VECTOR, little-endian."
  (edinet-u16 vector at (logand value #xFFFF))
  (edinet-u16 vector (+ at 2) (logand (ash value -16) #xFFFF)))

(defun edinet-zip (name data)
  "A ZIP archive holding one STORED member NAME with DATA, as octets: what a
type=5 body is, without needing a compressor to build the fixture."
  (let* ((name-bytes (map '(vector (unsigned-byte 8)) #'char-code name))
         (name-size (length name-bytes))
         (size (length data))
         (archive (make-array (+ 30 name-size size 46 name-size 22)
                              :element-type '(unsigned-byte 8)))
         (central (+ 30 name-size size))
         (end (+ central 46 name-size)))
    (setf (aref archive 0) #x50 (aref archive 1) #x4B (aref archive 2) 3 (aref archive 3) 4)
    (edinet-u32 archive 18 size)
    (edinet-u32 archive 22 size)
    (edinet-u16 archive 26 name-size)
    (replace archive name-bytes :start1 30)
    (replace archive data :start1 (+ 30 name-size))
    (setf (aref archive central) #x50 (aref archive (1+ central)) #x4B
          (aref archive (+ central 2)) 1 (aref archive (+ central 3)) 2)
    (edinet-u32 archive (+ central 20) size)
    (edinet-u32 archive (+ central 24) size)
    (edinet-u16 archive (+ central 28) name-size)
    (replace archive name-bytes :start1 (+ central 46))
    (setf (aref archive end) #x50 (aref archive (1+ end)) #x4B
          (aref archive (+ end 2)) 5 (aref archive (+ end 3)) 6)
    (edinet-u16 archive (+ end 8) 1)
    (edinet-u16 archive (+ end 10) 1)
    (edinet-u32 archive (+ end 12) (+ 46 name-size))
    (edinet-u32 archive (+ end 16) central)
    archive))

(defun edinet-utf-16le (text)
  "TEXT as UTF-16LE octets with a byte-order mark, the encoding EDINET's CSV members use."
  (let ((bytes (list #xFF #xFE)))
    (loop for char across text
          for code = (char-code char)
          do (setf bytes (append bytes (list (logand code #xFF) (logand (ash code -8) #xFF)))))
    (coerce bytes '(vector (unsigned-byte 8)))))

(defun edinet-tsv (&rest rows)
  "ROWS — lists of fields — as one tab-separated text, a line each."
  (format nil "~{~a~%~}"
          (mapcar (lambda (row)
                    (with-output-to-string (out)
                      (loop for (field . rest) on row
                            do (write-string field out)
                            when rest do (write-char #\Tab out))))
                  rows)))

(defparameter +csv-text+
  (edinet-tsv '("要素ID" "項目名" "コンテキストID" "連結・個別" "単位" "値")
              '("jpcrp_cor:NetSalesSummaryOfBusinessResults" "売上高、経営指標等"
                "CurrentYearDuration" "連結" "円" "45095325000000")
              '("jppfs_cor:Assets" "資産合計" "CurrentYearInstant" "連結" "円" "90114296000000")
              '("jpdei_cor:AccountingStandardsDEI" "会計基準" "CurrentYearDuration" "" "－" "IFRS"))
  "A type=5 CSV as EDINET writes one: a header, two headline rows and the standard.")

;;; --- the search --------------------------------------------------------------------

(deftest edinet-cell-searches-a-file-date ()
  (with-cell-stop ((edinet-start))
    (with-edinet-wire ()
      (edinet-replies-with +list-json+)
      (let ((answer (edinet:search-filings
                     (edinet-search-call "date_from" "2024-06-25" "date_to" "2024-06-25"
                                         "sec_code" "7203"))))
        (is (search "S100TR7I" answer) "the searched issuer's filing")
        (is (not (search "S100ZZZZ" answer)) "and only that one: the code filtered it here")
        (is (search "WZEK0040.aspx?S100TR7I" answer) "cited from its EDINET filing page")
        (is (= 1 (length (edinet-asked))) "one file date, one request")
        (is (search "date=2024-06-25" (edinet-url (first (edinet-asked)))) "the day asked for")
        (is (search "Subscription-Key=test-key" (edinet-url (first (edinet-asked)))) "with the key")))))

(deftest edinet-cell-refuses-what-edinet-could-not-answer ()
  (with-cell-stop ((edinet-start))
    (with-edinet-wire ()
      (flet ((ask (&rest pairs) (edinet:search-filings (apply #'edinet-search-call pairs))))
        (let ((short-date (lambda () (ask "date_from" "2024-6-25" "date_to" "2024-06-25"
                                          "sec_code" "7203")))
              (over-cap (lambda () (ask "date_from" "2024-06-01" "date_to" "2024-07-31"
                                        "sec_code" "7203")))
              (no-filter (lambda () (ask "date_from" "2024-06-25" "date_to" "2024-06-25"))))
          (is (signals-error edinet:edinet-error (funcall short-date)) "a date that is not YYYY-MM-DD")
          (is (signals-error edinet:edinet-error (funcall over-cap)) "a range wider than the cap")
          (is (signals-error edinet:edinet-error (funcall no-filter)) "no company filter at all")
          (is (null (edinet-asked)) "and not one of them reached the network")
          (edinet-replies-with +list-json+ 404)
          (let ((answer (ask "date_from" "2024-06-25" "date_to" "2024-06-25" "sec_code" "7203")))
            (is (search "no list" answer) "a day outside retention is reported, not fatal")))))))

;;; --- the financials ------------------------------------------------------------------

(deftest edinet-cell-reads-a-csv-zip ()
  (with-cell-stop ((edinet-start))
    (with-edinet-wire ()
      (edinet-replies-with (edinet-zip "XBRL_TO_CSV/jpcrp030000-asr-001.csv" (edinet-utf-16le +csv-text+)))
      (let ((answer (edinet:financials (edinet-financial-call "doc_id" "S100TR7I"))))
        (is (search "45,095,325,000,000" answer) "the revenue row, as the filing states it")
        (is (search "90,114,296,000,000" answer) "and the total assets row")
        (is (search "IFRS" answer) "with the accounting standard it read")
        (is (search "missing" answer) "a figure no row carries is reported, never invented")
        (is (search "type=5" (edinet-url (first (edinet-asked)))) "one CSV ZIP fetch")
        (is (search "S100TR7I" answer) "and the answer names the document")))))

;;; --- the saved document ----------------------------------------------------------------

(deftest edinet-cell-saves-a-document ()
  (with-cell-stop ((edinet-start))
    (with-edinet-wire ()
      (let ((path (nodecode-edinet::document-path "S100TR7I" 2))
            (pdf "%PDF-1.4 a filing"))
        (uiop:delete-file-if-exists path)
        (edinet-replies-with (map '(vector (unsigned-byte 8)) #'char-code pdf))
        (let ((answer (edinet:save-document (edinet-save-call "doc_id" "S100TR7I"))))
          (is (probe-file path) "the PDF is on disk")
          (is (search (namestring path) answer) "and the answer names it")
          (let ((written (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
            (is (= (length pdf) written) "byte for byte"))
          (is (search "type=2" (edinet-url (first (edinet-asked)))) "fetched as the PDF"))
        (uiop:delete-file-if-exists path)))))

;;; --- the operator's command --------------------------------------------------------------

(deftest edinet-cell-command-reports-the-connection-and-searches ()
  (with-cell-stop ((edinet-start))
    (with-edinet-wire ()
      (let ((status (cell-entry "nodecode-edinet" "edinet" "" nil)))
        (is (search "key set" status) "alone, it says whether the key is set")
        (is (search "api.edinet-fsa.go.jp" status) "and the base it talks to"))
      (is (null (edinet-asked)) "saying so asks EDINET nothing")
      (edinet-replies-with +list-json+)
      (let ((answer (cell-entry "nodecode-edinet" "edinet" "7203 2024-06-25" nil)))
        (is (search "S100TR7I" answer) "a code and a date search that day")
        (is (search "date=2024-06-25" (edinet-url (first (edinet-asked)))) "the day as typed")))))
