;;;; support.lisp --- the test slice: a stubbed Sigrex API and hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; SIGREX-CELL- name prefix; RUN-SIGREX-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-SIGREX::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing ever
;;;; dials.

(in-package #:nodecode.test)

(define-test-slice "sigrex" "SIGREX-CELL-" :start nodecode-sigrex:start-cell)

;;; --- the canned platform -----------------------------------------------------------

(defparameter +key+ "sigrex-test-key"
  "The API key a test's section carries.")

(defparameter +secret+ "sigrex-test-secret"
  "The API secret a test's section carries.")

(defparameter +api-host+ "https://api.sigrex.io"
  "The host a test's section names.")

(defparameter +hook-host+ "https://hook.sigrex.io"
  "The hook host a test's section names.")

(defparameter +strategies-answer+
  "[{\"id\":34,\"name\":\"BTC momentum\",\"status\":\"ACTIVE\",\"sendSignal\":true,\"lastTrigger\":\"2026-11-01 09:12\"},{\"id\":35,\"name\":\"ETH range\",\"status\":\"INACTIVE\"}]"
  "What the stubbed API answers a strategy list: two strategies, one active.")

(defparameter +signal-answer+
  "{\"status\":\"ok\",\"webhook\":\"bot-1\",\"bots\":1}"
  "What the stubbed hook answers a signal.")

(defparameter +unauthorized+
  "{\"message\":\"Invalid API key\"}"
  "What a refused key answers, as the API reference observed live.")

;;; --- the stub ------------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers :content).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for the list answer.")

(defun stub-http (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :method method :url url :headers headers :content content)))
    (push call *calls*)
    (if *answer* (funcall *answer* call) (values +strategies-answer+ 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-sigrex::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers
                                                  :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -------------------------------------------------------

(defun sigrex-run-start (&rest pairs)
  "START-CELL with both credentials unless PAIRS name them."
  (apply #'sigrex-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (unless (member "api_secret" pairs :test #'string=)
                   (list "api_secret" +secret+))
                 (copy-list pairs))))

(defmacro with-sigrex ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((sigrex-run-start ,@pairs))
       ,@body)))

(defun sigrex-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made -------------------------------------------------

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun path-of (call)
  "CALL's url without its host: what the signature signs."
  (let ((url (getf call :url)))
    (subseq url (or (cl:search "/api/" url) (cl:search "/bot/" url)))))

(defun signed-string (call)
  "The message CALL's signature is over, rebuilt from the call itself."
  (format nil "~a~a~a~a"
          (string-upcase (symbol-name (getf call :method)))
          (path-of call)
          (or (getf call :content) "")
          (header call "timestamp")))
