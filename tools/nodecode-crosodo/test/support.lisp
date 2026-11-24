;;;; support.lisp --- the test slice: a stubbed Crosodo endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; CROSODO-CELL- name prefix; RUN-CROSODO-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-CROSODO::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned endpoint
;;;; and records every POST, so a test asserts on the request and nothing
;;;; ever dials. The canned answers are the shapes the endpoint really
;;;; answers: one JSON-RPC reply a POST, the payload as a text block and
;;;; again as structured content.

(in-package #:nodecode.test)

(define-test-slice "crosodo" "CROSODO-CELL-" :start nodecode-crosodo:start-cell)

;;; --- the canned endpoint ---------------------------------------------------------

(defparameter +headers+ '(("content-type" . "application/json"))
  "What the endpoint answers a JSON-RPC POST with.")

(defun reply (result &key (id 2))
  "One JSON-RPC reply carrying RESULT, as the endpoint writes it."
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun tool-result (text &key structured error)
  "An MCP tools/call result: one text block, the same payload as structured
content, and ISERROR when ERROR."
  (nlk:json-object
   "content" (vector (nlk:json-object "type" "text" "text" text))
   "structuredContent" (or structured (nlk:json-object))
   "isError" (and error t)))

(defparameter +initialize+
  (reply (nlk:json-object "protocolVersion" "2025-06-18"
                           "capabilities" (nlk:json-object)
                           "serverInfo" (nlk:json-object "name" "crosodo" "version" "1.0.3"))
         :id 1)
  "What the handshake is answered.")

(defparameter +state-answer+
  (reply (tool-result
          "{\"state\":\"Virginia\",\"abbrev\":\"VA\",\"sales_cap\":\"Tiered\",\"statute\":\"Va. Code 3.2-5130\",\"state_dept\":\"Virginia Department of Agriculture\"}"
          :structured (nlk:json-object "state" "Virginia" "abbrev" "VA")))
  "What lookup_state_law answers for VA.")

(defparameter +county-answer+
  (reply (tool-result
          "{\"state\":\"VA\",\"county\":\"Fairfax\",\"verification_status\":\"verified\",\"contact\":\"health department\"}"
          :structured (nlk:json-object "county" "Fairfax")))
  "What lookup_county answers for VA, Fairfax.")

(defparameter +products-answer+
  (reply (tool-result
          "{\"products\":[{\"id\":\"free_report\",\"price_display\":\"$0\"},{\"id\":\"permit_guide\",\"price_display\":\"$26\",\"purchasable_via_api\":true}]}"
          :structured (nlk:json-object "products" (vector))))
  "What list_products answers.")

(defparameter +checkout-answer+
  (reply (tool-result
          "{\"checkoutUrl\":\"https://checkout.example.test/pay/cs_test_1\",\"product\":\"permit_guide\",\"sessionId\":\"cs_test_1\"}"
          :structured (nlk:json-object "checkoutUrl" "https://checkout.example.test/pay/cs_test_1")))
  "What start_checkout answers.")

(defparameter +cert-answer+
  (reply (tool-result
          "{\"certId\":\"CC-VA-26-1042\",\"holder\":\"A Bakery\",\"status\":\"active\"}"
          :structured (nlk:json-object "status" "active")))
  "What verify_certification answers for a real credential.")

(defparameter +cert-failure+
  (reply (tool-result "Credential not found" :error t))
  "What verify_certification answers for an unknown credential.")

(defparameter +session-lost+
  "{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32001,\"message\":\"Session not found\"}}"
  "What an endpoint that has forgotten the session answers.")

;;; --- the stub -------------------------------------------------------------------

(defvar *posted* '()
  "Every POST body the stub took, oldest first.")

(defvar *session-lost* nil
  "When true, the first tools/call is answered as a forgotten session.")

(defun stub-http (method url &rest options)
  "The endpoint: a JSON-RPC method answered by the body it carries."
  (declare (ignore method url))
  (let ((text (or (getf options :content) "")))
    (push text *posted*)
    (cond ((search "\"initialize\"" text) (values +initialize+ 200 +headers+))
          ((search "notifications/initialized" text) (values "" 202 +headers+))
          ((and *session-lost* (search "\"tools/call\"" text))
           (setf *session-lost* nil)
           (values +session-lost+ 200 +headers+))
          ((search "lookup_state_law" text) (values +state-answer+ 200 +headers+))
          ((search "lookup_county" text) (values +county-answer+ 200 +headers+))
          ((search "list_products" text) (values +products-answer+ 200 +headers+))
          ((search "start_checkout" text) (values +checkout-answer+ 200 +headers+))
          ((search "verify_certification" text)
           (values (if (search "CC-VA-26-1042" text) +cert-answer+ +cert-failure+) 200 +headers+))
          ((search "log_sale" text) (values +checkout-answer+ 200 +headers+))
          (t (error "unexpected request: ~a" text)))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its POSTs recorded."
  `(let ((*posted* '())
         (*session-lost* nil))
     (with-stubbed-fdefinition (nodecode-crosodo::http
                                (method url &rest options)
                                (apply #'stub-http method url options))
       ,@body)))

(defmacro with-crosodo (&body body)
  "Run BODY with the cell started on its defaults, its wire stubbed, stopped
on unwind."
  `(with-wire
     (with-cell-stop ((crosodo-start))
       ,@body)))

;;; --- reading what a call sent ----------------------------------------------------

(defun posted-messages ()
  "Every POST body as a decoded message object, oldest first."
  (loop for text in (reverse *posted*)
        for message = (and (plusp (length (nlk:trimmed text)))
                           (handler-case (nlk:decode-json text) (error () nil)))
        when (hash-table-p message) collect message))

(defun posted-methods ()
  "The JSON-RPC method of every POST that named one, oldest first."
  (loop for message in (posted-messages)
        for method = (nlk:json-value message :string "method")
        when method collect method))

(defun posted-arguments (tool)
  "The arguments object the call to TOOL carried, or NIL when none did."
  (loop for message in (posted-messages)
        when (equal tool (nlk:json-value message :string "params" "name"))
          return (nlk:json-value message :object "params" "arguments")))

(defun by-name (name &optional (text nil))
  "NAME's value in TEXT -- the whole body when TEXT is NIL -- or NIL."
  (let ((text (or text (format nil "~{~a~^~}" (reverse *posted*)))))
    (let ((found (search (format nil "\"~a\":\"" name) text)))
      (and found
           (let* ((start (+ found (length name) 4))
                  (end (or (position #\" text :start start) (length text))))
             (subseq text start end))))))

;;; --- calling a tool the way the model does ---------------------------------------

(defun crosodo-call (name arguments)
  "NAME called through the real dispatch, its contract and all; => the result
text as the model reads it."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun crosodo-parameter (tool name)
  "NAME as the argument the last call to TOOL sent, or NIL."
  (let ((arguments (posted-arguments tool)))
    (and arguments (gethash name arguments))))
