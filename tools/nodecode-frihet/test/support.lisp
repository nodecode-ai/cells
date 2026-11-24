;;;; support.lisp --- the test slice: a stubbed Frihet MCP endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; FRIHET-CELL- name prefix; RUN-FRIHET-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-FRIHET::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned endpoint
;;;; that answers by the JSON-RPC method it is sent, and records every
;;;; exchange, so a test asserts on the request and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "frihet" "FRIHET-CELL-" :start nodecode-frihet:start-cell)

;;; --- the canned endpoint ----------------------------------------------------------

(defparameter +key+ "fri_test_key_0001"
  "The key a test's section carries.")

(defparameter +session-id+ "session-abc"
  "The session id the stubbed handshake hands back.")

(defun wire-json (object)
  (nlk:encode-json-object object))

(defparameter +initialize+
  (wire-json (nlk:json-object
               "jsonrpc" "2.0" "id" 1
               "result" (nlk:json-object
                          "protocolVersion" "2025-06-18"
                          "serverInfo" (nlk:json-object "name" "frihet" "version" "1.4.0")
                          "capabilities" (nlk:json-object))))
  "What the stubbed endpoint answers a handshake.")

(defparameter +tools+
  (wire-json (nlk:json-object
               "jsonrpc" "2.0" "id" 2
               "result" (nlk:json-object
                          "tools" (vector (nlk:json-object "name" "list_invoices")
                                           (nlk:json-object "name" "get_invoice")
                                           (nlk:json-object "name" "send_invoice")))))
  "What the stubbed endpoint advertises: two reads and one send.")

(defparameter +call+
  (wire-json (nlk:json-object
               "jsonrpc" "2.0" "id" 2
               "result" (nlk:json-object
                          "content" (vector (nlk:json-object "type" "text"
                                                              "text" "INV-1 draft, 120.00 EUR"))
                          "isError" :false)))
  "What the stubbed endpoint answers a tools/call.")

(defparameter +session-headers+ (list (cons "mcp-session-id" +session-id+))
  "What the handshake's answer carries, as the wire hands headers back.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every exchange the stub took, newest first: (:method :request :headers).")

(defvar *answer* nil
  "A function of (METHOD REQUEST) answering (values BODY STATUS HEADERS), or NIL
for the canned endpoint.")

(defun stub-http (method url &key headers content timeout)
  (declare (ignore url timeout))
  (let* ((request (ignore-errors (nlk:decode-json (or content "{}"))))
         (name (nlk:json-value request :string "method")))
    (push (list :method (and method (string-upcase (string method)))
                :name name :request request :headers headers)
          *calls*)
    (multiple-value-bind (body status) 
        (if *answer*
            (funcall *answer* name request)
            (cond ((equal name "initialize") (values +initialize+ 200))
                  ((equal name "notifications/initialized") (values "" 202))
                  ((equal name "tools/list") (values +tools+ 200))
                  ((equal name "tools/call") (values +call+ 200))
                  (t (values "{}" 400))))
      (values body (or status 200) (and (equal name "initialize") +session-headers+)))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its exchanges recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-frihet::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers
                                           :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defun frihet-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'frihet-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-frihet ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((frihet-run-start ,@pairs))
       ,@body)))

(defun frihet-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- the gate, without a question board --------------------------------------------

(defvar *asked* '()
  "Every question the stand-in board was asked, newest first.")

(defun answer-with (answer)
  "A stand-in ASK-OPERATOR that answers ANSWER and records the question."
  (lambda (title &key detail choices seconds)
    (declare (ignore detail choices seconds))
    (push title *asked*)
    (values answer nil)))

(defun hook-verdict (name arguments)
  "The installed hook's answer for a call: :ALLOWED, or the refusal text."
  (funcall (cell-hook "nodecode-frihet" :tool)
           (list :name name :arguments arguments :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun gate-verdict (operation &key confirm)
  "The installed hook's answer for a frihet_mcp call: :ALLOWED, or the refusal."
  (let ((arguments (nlk:json-object "operation" operation)))
    (when confirm (setf (gethash "confirm" arguments) t))
    (hook-verdict "frihet_mcp" arguments)))

;;; --- reading a request the cell made ------------------------------------------------

(defun calls-named (name)
  "Every exchange the stub took for JSON-RPC NAME, oldest first."
  (reverse (remove name *calls* :key (lambda (call) (getf call :name)) :test-not #'equal)))

(defun call-named (name)
  "The first exchange the stub took for JSON-RPC NAME, or NIL."
  (first (calls-named name)))

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
