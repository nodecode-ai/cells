;;;; support.lisp --- the test slice: a stubbed Brave Search API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; BRAVE-SEARCH-CELL- name prefix; RUN-BRAVE-SEARCH-TESTS runs exactly that
;;;; slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-BRAVE-SEARCH::HTTP, the
;;;; cell's only way to the network. WITH-WIRE replaces it with a canned
;;;; answer and records every call, so a test asserts on the request and
;;;; nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "brave-search" "BRAVE-SEARCH-CELL-" :start nodecode-brave-search:start-cell)

;;; --- the canned API --------------------------------------------------------------

(defparameter +key+ "brave-test-key"
  "The token a test's section carries.")

(defparameter +answer+
  (nlk:encode-json-object
   (nlk:json-object
    "web"
    (nlk:json-object
     "results"
     (vector
      (nlk:json-object
       "title" "Rust &amp; Cargo: 1.78.0 <b>is out</b>"
       "url" "https://blog.rust-lang.org/releases/1.78.0"
       "description" "<strong>Announcing</strong> Rust 1.78.0: the release notes&#x27; details &amp; more"
       "age" "2 weeks ago"
       "language" "en"
       "profile" (nlk:json-object "long_name" "Rust Blog")
       "meta_url" (nlk:json-object "display_url" "blog.rust-lang.org"))
      (nlk:json-object
       "title" "Rust  1.78 changelog"
       "url" "https://example.test/rust-1-78"
       "description" "The merged pull requests.")))))
  "What the stubbed API answers: two results, the first escaped and tagged.")

(defparameter +error+
  "{\"type\":\"ErrorResponse\",\"error\":{\"code\":\"SUBSCRIPTION_TOKEN_INVALID\",\"detail\":\"The subscription token is invalid.\"}}"
  "What a refused token answers.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:url :headers).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for +ANSWER+.")

(defun stub-http (url &key headers timeout)
  (declare (ignore timeout))
  (let ((call (list :url url :headers headers)))
    (push call *calls*)
    (if *answer* (funcall *answer* call) (values +answer+ 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-brave-search::http
                                (url &key headers timeout)
                                (stub-http url :headers headers :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defun brave-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'brave-search-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-brave ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((brave-run-start ,@pairs))
       ,@body)))

(defun brave-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ---------------------------------------------

(defun params-of (url)
  "URL's query string as an alist of decoded (NAME . VALUE) pairs."
  (loop with query = (quri:uri-query (quri:uri url))
        for pair in (and query (uiop:split-string query :separator '(#\&)))
        for equals = (position #\= pair)
        collect (cons (quri:url-decode (subseq pair 0 equals) :encoding :utf-8)
                      (quri:url-decode (subseq pair (1+ equals)) :encoding :utf-8))))

(defun param (call name)
  "The value CALL's url carries for NAME, or NIL when it does not carry it."
  (cdr (assoc name (params-of (getf call :url)) :test #'string=)))

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
