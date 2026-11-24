;;;; cell-test.lisp --- the vt-check cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one network call is NODECODE-VT-CHECK::HTTP; WITH-SCRIPTED-VT stubs
;;;; that name, records every url asked for, and answers a fixture -- so no
;;;; test opens a socket. The lifecycle tests are the machinery's own: a start
;;;; installs the manual topic, the four tools and /vt-check, a stop takes them
;;;; all back, a second start leaves one of everything, and a bad setting
;;;; refuses. The rest check what a person gets: a file hashed and looked up
;;;; without ever being uploaded, a defanged link routed to its own report, a
;;;; key that is missing refusing in words, and an archive read from its own
;;;; headers.

(in-package #:nodecode.test)

(define-test-slice "vt-check" "VT-CHECK-CELL-" :start nodecode-vt-check:start-cell)

;;; --- the scripted seam ---------------------------------------------------------------

(defmacro with-scripted-vt ((urls &key answer) &body body-forms)
  "Run BODY-FORMS with NODECODE-VT-CHECK::HTTP recording each url into URLS
and answering (values BODY STATUS): ANSWER is a function of the method and
the url."
  (let ((url (gensym "URL")) (method (gensym "METHOD")))
    `(let ((,urls '()))
       (with-stubbed-fdefinition (nodecode-vt-check::http (,method ,url &rest arguments)
                                   (declare (ignore arguments))
                                   (setf ,urls (append ,urls (list ,url)))
                                   (funcall ,answer ,method ,url))
         ,@body-forms))))

(defun vt-json (object)
  "OBJECT as the JSON text VirusTotal answers with."
  (nlk:encode-json-object object))

(defun vt-object (&rest pairs)
  "A decoded JSON object from alternating key/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun vt-file-answer (method url)
  "A detected file report, whatever hash was asked about."
  (declare (ignore method))
  (declare (ignore url))
  (values (vt-json (vt-object "data" (vt-object
                                       "attributes" (vt-object
                                                     "meaningful_name" "setup.exe"
                                                     "type_description" "Win32 EXE"
                                                     "size" 1048576
                                                     "reputation" -34
                                                     "times_submitted" 120
                                                     "last_analysis_stats" (vt-object
                                                                             "malicious" 12
                                                                             "suspicious" 1
                                                                             "harmless" 55
                                                                             "undetected" 2)))))
          200))

(defun vt-url-answer (method url)
  "A clean indicator report; URL names the endpoint that was asked."
  (declare (ignore method url))
  (values (vt-json (vt-object "data" (vt-object
                                       "attributes" (vt-object
                                                     "reputation" 5
                                                     "times_submitted" 40
                                                     "categories" (vt-object "engine" "phishing")
                                                     "last_analysis_stats" (vt-object
                                                                             "malicious" 0
                                                                             "suspicious" 0
                                                                             "harmless" 70)))))
          200))

;;; --- a file on disk ----------------------------------------------------------------

(defmacro with-temp-binary ((path &rest octets) &body body-forms)
  "Run BODY-FORMS with PATH a file holding OCTETS."
  `(uiop:with-temporary-file (:pathname ,path :type "bin")
     (with-open-file (out ,path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
       (write-sequence (coerce (list ,@octets) '(vector (unsigned-byte 8))) out))
     ,@body-forms))

;;; --- a ZIP container built here, so the forensics run offline --------------------

(defun push-u16 (vector value)
  "VALUE as two little-endian octets on the end of VECTOR."
  (vector-push-extend (logand value 255) vector)
  (vector-push-extend (logand (ash value -8) 255) vector))

(defun push-u32 (vector value)
  "VALUE as four little-endian octets on the end of VECTOR."
  (dolist (shift '(0 8 16 24)) (vector-push-extend (logand (ash value (- shift)) 255) vector)))

(defun push-octets (vector octets)
  "OCTETS on the end of VECTOR."
  (loop for octet across octets do (vector-push-extend octet vector)))

(defun stored-zip (entries)
  "A ZIP container of ENTRIES -- (NAME . CONTENT) -- each stored, not deflated."
  (let ((locals (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (central (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (offset 0))
    (dolist (entry entries)
      (let* ((name (sb-ext:string-to-octets (car entry) :external-format :utf-8))
             (data (sb-ext:string-to-octets (cdr entry) :external-format :utf-8))
             (size (length data))
             (start offset))
        (push-u32 locals #x04034b50)
        (push-u16 locals 20)
        (push-u16 locals 0)
        (push-u16 locals 0)
        (push-u16 locals 0)
        (push-u16 locals 0)
        (push-u32 locals 0)
        (push-u32 locals size)
        (push-u32 locals size)
        (push-u16 locals (length name))
        (push-u16 locals 0)
        (push-octets locals name)
        (push-octets locals data)
        (incf offset (+ 30 (length name) size))
        (push-u32 central #x02014b50)
        (push-u16 central 20)
        (push-u16 central 20)
        (push-u16 central 0)
        (push-u16 central 0)
        (push-u16 central 0)
        (push-u16 central 0)
        (push-u32 central 0)
        (push-u32 central size)
        (push-u32 central size)
        (push-u16 central (length name))
        (push-u16 central 0)
        (push-u16 central 0)
        (push-u16 central 0)
        (push-u16 central 0)
        (push-u32 central 0)
        (push-u32 central start)
        (push-octets central name)))
    (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
          (count (length entries)))
      (push-octets out locals)
      (push-octets out central)
      (push-u32 out #x06054b50)
      (push-u16 out 0)
      (push-u16 out 0)
      (push-u16 out count)
      (push-u16 out count)
      (push-u32 out (length central))
      (push-u32 out (length locals))
      (push-u16 out 0)
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

;;; --- the lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "vt-check"
  (:help :vt-check)
  (:command "vt-check")
  (:refused ("cache_ttl_hours" -1) ("timeout_seconds" 0))
  (:idle nodecode-vt-check:vt-check-error (vt-check:check-ip "203.0.113.9")))

;;; --- what a person gets ----------------------------------------------------------------

(deftest vt-check-cell-hashes-a-file-and-says-it-never-uploaded-it ()
  (with-temp-binary (path 77 90 144 0 3 0 0 0)
    (with-cell-stop ((vt-check-start "api_key" "0123456789abcdef"))
      (with-scripted-vt (urls :answer #'vt-file-answer)
        (let ((block (vt-check:check-file (namestring path))))
          (is (cl:search "FILE " block) "the block names the file")
          (is (cl:search "MALICIOUS" block) "and its verdict, from the engine counts")
          (is (cl:search "PE/Windows executable" block) "the content is read from its magic bytes")
          (is (cl:search "sha256: " block) "with the three hashes")
          (is (cl:search "12/70 engines flag it malicious" block) "and VirusTotal's own words")
          (is (cl:search "never uploaded" block) "and the privacy line")
          (is (cl:search "/api/v3/files/" (first urls)) "one hash lookup and nothing else")
          (is (= 1 (length urls)) "exactly one request"))))))

(deftest vt-check-cell-checks-a-link-as-the-url-its-defanging-names ()
  (with-cell-stop ((vt-check-start "api_key" "0123456789abcdef"))
    (with-scripted-vt (urls :answer #'vt-url-answer)
      (let ((block (vt-check:check-url "hxxps://evil[.]com/x")))
        (is (cl:search "LINK https://evil.com/x (queried as url)" block) "the defanged link is undone")
        (is (cl:search (format nil "urls/~a" (nodecode-vt-check::base64-url "https://evil.com/x"))
                       (first urls)) "and looked up by its base64 url id")
        (is (cl:search "read-only lookup; nothing was submitted" block) "read-only, as promised")
        (is (cl:search "CLEAN" block) "with the engine verdict")))))

(deftest vt-check-cell-a-missing-key-is-a-refusal-naming-the-key ()
  (with-cell-stop ((vt-check-start "api_key_env" "VT_CHECK_TEST_ABSENT_KEY"))
    (let ((refusal (signals-error nodecode-vt-check:vt-check-error
                      (vt-check:check-url "https://example.com"))))
      (is (typep refusal 'nodecode-vt-check:vt-check-error) "a link check refuses rather than guessing")
      (is (cl:search "my-apikey" (nodecode-vt-check::vt-check-error-detail refusal)) "naming where a free key comes from"))))

(deftest vt-check-cell-status-says-missing-without-a-key ()
  (with-cell-stop ((vt-check-start "api_key_env" "VT_CHECK_TEST_ABSENT_KEY"))
    (let ((block (vt-check:status)))
      (is (cl:search "key: missing" block) "the key is reported missing")
      (is (cl:search "never uploaded" block) "and the privacy line still stands"))))

(deftest vt-check-cell-an-archive-is-read-from-its-own-headers ()
  (let* ((raw (stored-zip (list (cons "../evil.sh" "echo hi")
                                (cons "invoice.pdf.exe" "MZ"))))
         (findings (nodecode-vt-check::zip-findings raw "bundle.zip" 0))
         (text (format nil "~{~a~^ ~}" (mapcar (lambda (finding) (getf finding :why)) findings))))
    (is (cl:search "path traversal entry '../evil.sh'" text) "a traversing entry is an alert")
    (is (cl:search "double extension 'invoice.pdf.exe'" text) "a document-named executable is one too")
    (is (cl:search "normal in source and installer archives" text) "and code inside it is only info")
    (is (equal "alert" (getf (first findings) :level)) "the loudest finding is an alert")))

(deftest vt-check-cell-the-command-answers-status ()
  (with-cell-stop ((vt-check-start "api_key_env" "VT_CHECK_TEST_ABSENT_KEY"))
    (let ((replied (cell-entry "nodecode-vt-check" "vt-check" "status" "vt-check-cell-command")))
      (is (cl:search "VirusTotal key: missing" replied) "/vt-check status reports the key")
      (is (cl:search "usage: /vt-check" (cell-entry "nodecode-vt-check" "vt-check" "" "s")) "and typed alone it says how to use itself"))))
