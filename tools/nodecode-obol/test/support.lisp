;;;; support.lisp --- the obol test runner and the one seam the tests stub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's,
;;;; under an OBOL-CELL- name prefix; RUN-OBOL-TESTS runs exactly that slice.
;;;; The one outside thing this cell does is a Stripe exchange, so that is
;;;; what is stubbed: NODECODE-OBOL::*HTTP* answers the routes a test hands
;;;; it and records every address. No test needs a network.

(in-package #:nodecode.test)

(define-test-slice "obol" "OBOL-CELL-" :start nodecode-obol:start-cell :defaults (("api_key" "sk_test_demo")))

(defvar *requests* '()
  "What the stub was asked for, newest first, as (METHOD PATH CONTENT).")

(defun path-of (url)
  "URL's path, without the query."
  (let* ((start (or (search "//" url) -1))
         (rest (subseq url (min (length url) (+ start 2))))
         (slash (position #\/ rest))
         (path (if slash (subseq rest slash) "/")))
    (subseq path 0 (or (position #\? path) (length path)))))

(defun route (method path body &optional (status 200))
  "One stubbed answer: METHOD on PATH is BODY, a JSON string, at STATUS."
  (list (list method path) body status))

(defun stub-stripe (routes)
  "A function NODECODE-OBOL::*HTTP* can be: it answers the route for the
method and path it is asked for, and records every call."
  (lambda (method url &key headers content timeout)
    (declare (ignore headers timeout))
    (let ((path (path-of url)))
      (push (list method path content) *requests*)
      (let ((found (find (list method path) routes :test #'equal :key #'first)))
        (if found
            (values (second found) (third found) nil nil)
            (values "{\"error\":{\"message\":\"no stub for this route\"}}" 404 nil nil))))))

(defmacro with-stripe ((&rest routes) &body body)
  "BODY with the Stripe seam stubbed to ROUTES, and the calls recorded afresh."
  `(let ((nodecode-obol::*http* (stub-stripe (list ,@routes)))
         (*requests* '()))
     ,@body))

(defun fresh-ledger ()
  "This home's ledger forgotten, so a test starts with nothing billed."
  (let ((path (nlk:home "obol/ledger.json")))
    (when (probe-file path) (delete-file path))))

(defun call-tool (name pairs)
  "The cell's tool NAME called with PAIRS, as the registry calls it."
  (let* ((entry (nle::find-tool name))
         (call (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs))))
    (funcall (third entry) call)))

(defun refusal (function &rest arguments)
  "The text of the refusal FUNCTION signals for ARGUMENTS."
  (handler-case (progn (apply function arguments) "no refusal")
    (error (condition) (princ-to-string condition))))

(defparameter *customer* "{\"data\":[{\"id\":\"cus_1\"}]}"
  "A Stripe account that already knows the client.")

(defparameter *invoice* "{\"id\":\"in_1\",\"number\":\"INV-0001\",\"status\":\"draft\",\"currency\":\"usd\",\"hosted_invoice_url\":\"\"}"
  "A fresh draft.")

(defparameter *sent* "{\"id\":\"in_1\",\"number\":\"INV-0001\",\"status\":\"open\",\"currency\":\"usd\",\"hosted_invoice_url\":\"https://invoice.stripe.com/i/one\"}"
  "The same invoice, finalized.")

(defparameter *empty* "{}"
  "An answer with nothing in it.")
