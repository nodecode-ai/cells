;;;; support.lisp --- the test slice: a stubbed Yandex Search API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; YANDEX-CELL- name prefix; RUN-YANDEX-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-YANDEX::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing ever
;;;; dials.

(in-package #:nodecode.test)

(define-test-slice "yandex" "YANDEX-CELL-" :start nodecode-yandex:start-cell)

;;; --- the canned API --------------------------------------------------------------

(defparameter +key+ "yandex-test-key"
  "The API key a test's section carries.")

(defparameter +folder+ "test-folder-id"
  "The folder id a test's section carries.")

(defparameter +report+
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<yandexsearch version=\"1.0\">
<response>
<results>
<grouping>
<group>
<doc id=\"1\">
<url>https://example.org/moscow</url>
<domain>example.org</domain>
<title>Moscow <hlword>weather</hlword> today</title>
<headline>Moscow weather</headline>
<passages><passage>The forecast for <hlword>Moscow</hlword> is mild.</passage><passage>More rain later.</passage></passages>
</doc>
<doc id=\"2\">
<url>https://example.net/second</url>
<domain>example.net</domain>
<title>Second page</title>
<passages><passage>Second snippet &amp; more.</passage></passages>
</doc>
<doc id=\"3\">
<title>No url here</title>
</doc>
</group>
</grouping>
</results>
</response>
</yandexsearch>"
  "What the stubbed /v2/web/search answers, as XML: two usable documents and
one with no url, with a highlight tag, two passages and an entity.")

(defun raw-data-answer (xml)
  "A /v2/web/search answer carrying XML as its Base64 rawData, as Yandex sends it."
  (nlk:encode-json-object
   (nlk:json-object "rawData"
                     (cl-base64:usb8-array-to-base64-string
                      (sb-ext:string-to-octets xml :external-format :utf-8)))))

(defparameter +generative-answer+
  (concatenate
   'string
   "{\"message\":{\"content\":\"The first metro opened in 1863.\"},"
   "\"sources\":[{\"url\":\"https://example.org/metro\",\"title\":\"Metro history\","
   "\"usedText\":\"The first underground railway opened in London in 1863.\"},"
   "{\"url\":\"https://example.net/second\"}],"
   "\"searchQueries\":[{\"text\":\"first metro opened\"},\"first underground railway\"],"
   "\"fixedMisspellQuery\":\"first metro opened\","
   "\"isAnswerRejected\":false,\"isBulletAnswer\":false}")
  "What the stubbed /v2/gen/search answers: one answer, two sources of which
one has no title, two search queries in both shapes.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:url :headers :content).")

(defvar *answer* nil
  "A function answering (values BODY STATUS), or NIL for the canned report.")

(defun stub-http (url &key headers content timeout)
  (declare (ignore timeout))
  (push (list :url url :headers headers :content content) *calls*)
  (if *answer*
      (funcall *answer*)
      (values (raw-data-answer +report+) 200)))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-yandex::http (url &key headers content timeout)
                                (stub-http url :headers headers :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defun yandex-run-start (&rest pairs)
  "START-CELL with a key and a folder unless PAIRS name them."
  (apply #'yandex-start
         (append (unless (member "api_key" pairs :test #'equal) (list "api_key" +key+))
                 (unless (member "folder_id" pairs :test #'equal) (list "folder_id" +folder+))
                 (copy-list pairs))))

(defmacro with-yandex ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((yandex-run-start ,@pairs))
       ,@body)))

(defun yandex-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ---------------------------------------------

(defun yandex-body (call)
  "CALL's JSON body, decoded."
  (nlk:decode-json (getf call :content)))

(defun yandex-header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

;;; --- what a canned answer says ----------------------------------------------------

(defun stub-json (text &optional (status 200))
  "A stub answer carrying TEXT as its body."
  (lambda () (values text status)))
