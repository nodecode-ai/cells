;;;; support.lisp --- the test slice: a stubbed Cloudflare Web Search API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; CLOUDFLARE-WEB-SEARCH-CELL- name prefix; RUN-CLOUDFLARE-WEB-SEARCH-TESTS
;;;; runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-CLOUDFLARE-WEB-SEARCH::HTTP,
;;;; the cell's only way to the network. WITH-WIRE replaces it with a canned
;;;; answer and records every call, so a test asserts on the request and
;;;; nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "cloudflare-web-search" "CLOUDFLARE-WEB-SEARCH-CELL-" :start nodecode-cloudflare-web-search:start-cell)

;;; --- the canned API --------------------------------------------------------------

(defparameter +token+ "cf-test-token"
  "The token a test's section carries.")

(defparameter +account+ "0123456789abcdef0123456789abcdef"
  "A 32-character account id, as the API's path takes it.")

(defparameter +answer+
  (nlk:encode-json-object
   (nlk:json-object
    "items"
    (vector
     (nlk:json-object
      "url" "https://example.test/salt-lake-fall"
      "title" "Fall in Salt Lake City: A Local's Guide"
      "description" "From scenic drives up Big Cottonwood Canyon to pumpkin patches.")
     (nlk:json-object
      "url" "https://example.test/second"
      "title" "Second result"
      "description" "The one after the first."))
    "metadata"
    (nlk:json-object
     "query" "things to do in salt lake city"
     "requestId" "req-cf-1"
     "latencyMs" 612)))
  "What the stubbed API answers: two results and the gateway's metadata.")

(defparameter +refusal+
  "{\"result\":null,\"success\":false,\"errors\":[{\"code\":10000,\"message\":\"Authentication error\"}],\"messages\":[]}"
  "What a refused token answers, in Cloudflare's own envelope.")

(defparameter +no-items+ "{\"metadata\":{\"requestId\":\"req-cf-2\"}}"
  "What a malformed answer looks like: no items array.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:url :headers :content).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for +ANSWER+ and 200.")

(defun stub-http (url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :url url :headers headers :content content)))
    (push call *calls*)
    (if *answer* (funcall *answer* call) (values +answer+ 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-cloudflare-web-search::http
                                (url &key headers content timeout)
                                (stub-http url :headers headers :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -------------------------------------------------------

(defun cf-run-start (&rest pairs)
  "START-CELL with a token and an account unless PAIRS name them."
  (flet ((given (key) (member key pairs :test #'string=)))
    (apply #'cloudflare-web-search-start
           (append (unless (given "api_token") (list "api_token" +token+))
                   (unless (given "account_id") (list "account_id" +account+))
                   (copy-list pairs)))))

(defmacro with-cloudflare ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS -- a token and an account unless
PAIRS name them -- its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((cf-run-start ,@pairs))
       ,@body)))

(defun cf-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made -----------------------------------------------

(defun call-body (call)
  "CALL's request body, decoded."
  (nlk:decode-json (getf call :content)))

(defun body-value (call &rest path)
  "The value CALL's request body carries at PATH."
  (apply #'nlk:json-value (call-body call) :any path))

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
