;;;; support.lisp --- the test slice: a stubbed Bento API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; BENTO-CELL- name prefix; RUN-BENTO-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-BENTO::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing dials.

(in-package #:nodecode.test)

(define-test-slice "bento" "BENTO-CELL-" :start nodecode-bento:start-cell
  :defaults (("publishable_key" bento-test-key)
             ("secret_key" bento-test-secret)
             ("site_uuid" bento-test-site)))

;;; --- the canned API --------------------------------------------------------------

(defparameter bento-test-key "pk_test_0000"
  "The publishable key a test's section carries.")
(defparameter bento-test-secret "sk_test_0000"
  "The secret key a test's section carries.")
(defparameter bento-test-site "site-test-0001"
  "The site uuid a test's section carries.")

(defparameter bento-test-tags "{\"data\":[{\"id\":1,\"name\":\"vip\"}]}"
  "What the stubbed tags endpoint answers.")

(defparameter bento-test-subscriber "{\"data\":{\"id\":7,\"attributes\":{\"email\":\"ada@example.test\"}}}"
  "What the stubbed subscriber endpoint answers.")

;;; --- the stub -------------------------------------------------------------------

(defvar bento-calls '()
  "Every request the stub took, newest first: (:method :url :headers :content).")

(defvar bento-answer nil
  "A function of one call answering (values BODY STATUS), or NIL for a 200 {}.")

(defun bento-stub-http (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :method method :url url :headers headers :content content)))
    (push call bento-calls)
    (if bento-answer (funcall bento-answer call) (values "{}" 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((bento-calls '())
         (bento-answer nil))
     (clrhash nodecode-bento::*cache*)
     (with-stubbed-fdefinition (nodecode-bento::http
                                (method url &key headers content timeout)
                                (bento-stub-http method url :headers headers
                                                       :content content :timeout timeout))
       ,@body)))

(defmacro with-bento ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((bento-start ,@pairs))
       ,@body)))

;;; --- calling it -------------------------------------------------------------------

(defun bento-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun bento-token (text)
  "The confirm_token a dry run's TEXT carries."
  (let* ((marker "confirm_token: ")
         (start (cl:search marker text)))
    (and start
         (let* ((rest (subseq text (+ start (length marker))))
                (end (or (position #\Newline rest) (length rest))))
           (subseq rest 0 end)))))

(defun bento-iso (seconds-ahead)
  "An ISO-8601 UTC timestamp SECONDS-AHEAD from now."
  (nodecode-bento::iso-of (+ (get-universal-time) seconds-ahead)))

;;; --- reading the request a call made ---------------------------------------------

(defun bento-params (url)
  "URL's query string as an alist of decoded (NAME . VALUE) pairs."
  (loop with query = (quri:uri-query (quri:uri url))
        for pair in (and query (uiop:split-string query :separator '(#\&)))
        for equals = (position #\= pair)
        collect (cons (quri:url-decode (subseq pair 0 equals) :encoding :utf-8)
                      (quri:url-decode (subseq pair (1+ equals)) :encoding :utf-8))))

(defun bento-param (call name)
  "The value CALL's url carries for NAME, or NIL."
  (cdr (assoc name (bento-params (getf call :url)) :test #'string=)))

(defun bento-header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun bento-auth ()
  "The basic token the test's keys make."
  (cl-base64:string-to-base64-string
   (format nil "~a:~a" bento-test-key bento-test-secret)))
