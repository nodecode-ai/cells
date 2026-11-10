;;;; support.lisp --- the test slice: a stubbed Parcel API.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; PARCEL-CELL- name prefix; RUN-PARCEL-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-PARCEL::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer per
;;;; route and records every call, so a test asserts on the request and
;;;; nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "parcel" "PARCEL-CELL-" :start nodecode-parcel:start-cell)

;;; --- the canned account -----------------------------------------------------------

(defparameter +parcel-base+ "https://api.parcel.app/external"
  "The API base a test's section names.")

(defparameter +parcel-key+ "test-key"
  "The API key a test's section carries.")

(defparameter +parcel-deliveries+
  (concatenate 'string
               "{\"success\": true, \"deliveries\": ["
               "{\"tracking_number\": \"1Z999AA10123456784\", \"carrier_code\": \"ups\","
               " \"description\": \"Running shoes\", \"status_code\": 4,"
               " \"date_expected\": \"2026-11-11\", \"timestamp_expected\": 1794000000,"
               " \"events\": [{\"event\": \"Out for delivery\", \"date\": \"2026-11-11 07:12\","
               " \"location\": \"Portland\"},"
               " {\"event\": \"Arrived at facility\", \"date\": \"2026-11-10 22:40\"}]},"
               "{\"tracking_number\": \"TBA123456789000\", \"carrier_code\": \"amzlus\","
               " \"description\": \"Coffee filter\", \"status_code\": 2}]}")
  "Two deliveries: one out for delivery with two events, one in transit.")

(defparameter +parcel-carriers+
  (concatenate 'string
               "{\"ups\": {\"name\": \"UPS\", \"extra_required\": false,"
               " \"name_variations\": [\"United Parcel Service\"]},"
               " \"usps\": {\"name\": \"USPS\", \"extra_required\": false,"
               " \"name_variations\": []},"
               " \"amzlus\": {\"name\": \"Amazon Logistics (US)\", \"extra_required\": true,"
               " \"name_variations\": [\"Amazon\"]}}")
  "Parcel's carrier table, three codes.")

(defvar *parcel-calls* '()
  "Every request the stub took, newest first: (:route :method :url :headers :body).")

(defvar *parcel-answers* nil
  "A route to the JSON text it answers.")

(defvar *parcel-status* 200
  "The HTTP status the stub answers with.")

(defun parcel-route-of (url)
  "URL's route, the key a test answers: the path under the API base."
  (cond ((search "/deliveries/" url) "/deliveries/")
        ((search "/supported_carriers.json" url) "/supported_carriers.json")
        ((search "/add-delivery/" url) "/add-delivery/")
        (t url)))

(defun parcel-answer (route text)
  "Have the stub answer ROUTE with TEXT, already JSON."
  (setf (gethash route *parcel-answers*) text))

(defun parcel-stub-http (method url &key content headers timeout)
  "The cell's one HTTP seam, answered from the canned account."
  (declare (ignore timeout))
  (let ((route (parcel-route-of url))
        (body (and content (ignore-errors (nlk:decode-json content)))))
    (push (list :route route :method method :url url :headers headers :body body)
          *parcel-calls*)
    (values (or (gethash route *parcel-answers*) "{}")
            *parcel-status*
            '())))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*parcel-calls* '())
         (*parcel-answers* (make-hash-table :test #'equal))
         (*parcel-status* 200))
     (with-stubbed-fdefinition (nodecode-parcel::http (method url &key content headers timeout)
                                (parcel-stub-http method url :content content
                                                       :headers headers :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -------------------------------------------------------

(defmacro with-parcel ((&rest pairs) &body body)
  "Run BODY with the cell started on the test's key and base plus PAIRS, its
wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((parcel-start "api_key" +parcel-key+ "api_base" +parcel-base+ ,@pairs))
       ,@body)))

(defmacro with-standard-parcel (&body body)
  "BODY inside a started cell that answers the deliveries and carriers routes."
  `(with-parcel ()
     (parcel-answer "/deliveries/" +parcel-deliveries+)
     (parcel-answer "/supported_carriers.json" +parcel-carriers+)
     ,@body))

(defun parcel-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ------------------------------------------------

(defun parcel-calls-of (route)
  "Every request the stub took for ROUTE, newest first."
  (remove-if-not (lambda (call) (equal route (getf call :route))) *parcel-calls*))

(defun parcel-call-of (route)
  "The newest request the stub took for ROUTE."
  (find route *parcel-calls* :key (lambda (call) (getf call :route)) :test #'equal))

(defun parcel-header (call name)
  "CALL's request header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun parcel-member (text key)
  "KEY's value in the JSON answer TEXT, as a Lisp value."
  (nlk:json-value (nlk:decode-json text) :any key))
