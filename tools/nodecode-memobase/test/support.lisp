;;;; support.lisp --- the test slice: a stubbed Memobase server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; MEMOBASE-CELL- name prefix; RUN-MEMOBASE-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-MEMOBASE::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing ever
;;;; dials.

(in-package #:nodecode.test)

(define-test-slice "memobase" "MEMOBASE-CELL-" :start nodecode-memobase:start-cell)

;;; --- the canned server -------------------------------------------------------------

(defparameter +base+ "http://memobase.test/api/v1"
  "The base url a test's section carries.")

(defparameter +key+ "memobase-test-key"
  "The bearer token a test's section carries.")

(defparameter +user+ "7e90d29c-fbfe-5fe1-92b5-5c90bf384026"
  "uuid5(NAMESPACE_DNS, \"user\" + \"memobase_client\"): the id the original
derived from its default user name, proved against an outside oracle.")

(defparameter +context-answer+
  (nlk:encode-json-object
   (nlk:json-object
    "errno" 0
    "errmsg" ""
    "data"
    (nlk:json-object
     "context" "## User Profile
- drinks tea, no coffee

## Past Events
- the build is green on main")))
  "What the stubbed context endpoint answers: a profile and one event.")

(defparameter +profile-answer+
  (nlk:encode-json-object
   (nlk:json-object
    "errno" 0
    "data"
    (nlk:json-object
     "topics"
     (vector (nlk:json-object
              "topic" "work"
              "sub_topics"
              (vector (nlk:json-object "name" "role" "content" "engineer")))))))
  "What the stubbed profile endpoint answers: one topic with one sub-topic.")

(defparameter +errno-answer+
  "{\"errno\":1001,\"errmsg\":\"user not found\",\"data\":null}"
  "What Memobase answers with HTTP 200 when its business layer refused.")

;;; --- the stub ------------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers :content).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for +CONTEXT-ANSWER+.")

(defun stub-http (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :method method :url url :headers headers :content content)))
    (push call *calls*)
    (if *answer* (funcall *answer* call) (values +context-answer+ 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-memobase::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers :content content
                                                   :timeout timeout))
       ,@body)))

;;; --- starting it, calling it ----------------------------------------------------------

(defun memobase-run-start (&rest pairs)
  "START-CELL with a base url and a key unless PAIRS name them."
  (apply #'memobase-start
         (append (unless (member "base_url" pairs :test #'equal) (list "base_url" +base+))
                 (unless (member "api_key" pairs :test #'equal) (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-memobase ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((memobase-run-start ,@pairs))
       ,@body)))

(defun memobase-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made -----------------------------------------------------

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

(defun path-of (call)
  "CALL's url without its query string."
  (let ((url (getf call :url)))
    (subseq url 0 (or (position #\? url) (length url)))))
