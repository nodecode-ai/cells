;;;; support.lisp --- the test slice: a stubbed Grounder search API.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; GROUNDER-CELL- name prefix; RUN-GROUNDER-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-GROUNDER::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing ever
;;;; dials.

(in-package #:nodecode.test)

(define-test-slice "grounder" "GROUNDER-CELL-" :start nodecode-grounder:start-cell)

;;; --- the canned API --------------------------------------------------------------

(defparameter +key+ "grounder-test-key"
  "The key a test's section carries.")

(defparameter +answer+
  (nlk:encode-json-object
   (nlk:json-object
    "query" "offshore wind"
    "results"
    (vector
     (nlk:json-object
      "title" "Offshore wind: the state of the fleet"
      "url" "https://example.test/wind"
      "summary" "A summary
   of the passage"
      "text" "text the summary wins over"
      "rank" 1
      "provenance" (nlk:json-object "found_by" (vector "brave")))
     (nlk:json-object
      "title" "Second source"
      "url" "https://example.test/two"
      "text" (make-string 900 :initial-element #\#)
      "rank" 2))))
  "What the stubbed API answers: a result with a summary over several lines and a
result whose text runs past the budget.")

(defparameter +error+
  "{\"error\":{\"code\":\"unauthorized\",\"message\":\"the search key expired\"}}"
  "What a refused key answers.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:url :headers :content :timeout).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for +ANSWER+.")

(defun stub-http (method url &key headers content timeout)
  (push (list :method method :url url :headers headers :content content :timeout timeout) *calls*)
  (if *answer*
      (funcall *answer* (first *calls*))
      (values +answer+ 200)))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-grounder::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers :content content
                                                  :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defun grounder-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'grounder-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-grounder ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((grounder-run-start ,@pairs))
       ,@body)))

(defun grounder-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ---------------------------------------------

(defun sent-body (call)
  "CALL's request body, decoded as JSON."
  (let ((content (getf call :content)))
    (and content (nlk:decode-json (if (stringp content)
                                       content
                                       (sb-ext:octets-to-string content :external-format :utf-8))))))

(defun sent-header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
