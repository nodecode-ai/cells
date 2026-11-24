;;;; support.lisp --- the test slice: a stubbed LovedOne MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; LOVEDONE-CELL- name prefix; RUN-LOVEDONE-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-LOVEDONE::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned server and
;;;; records every request, so a test asserts on the JSON-RPC that went out
;;;; and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "lovedone" "LOVEDONE-CELL-" :start nodecode-lovedone:start-cell)

;;; --- the canned server ------------------------------------------------------------

(defparameter +key+ "lovedone-test-key"
  "The key a test's section carries.")

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers :body).")

(defvar *answer* nil
  "A function of one request answering (values BODY STATUS HEADERS), or NIL
for the default server.")

(defun lovedone-json-text (object)
  (nlk:encode-json-object object))

(defun result-body (text &key is-error)
  "A JSON-RPC answer carrying TEXT as one text content block."
  (lovedone-json-text (nlk:json-object
              "jsonrpc" "2.0" "id" 2
              "result" (nlk:json-object
                        "content" (vector (nlk:json-object "type" "text" "text" text))
                        :when is-error "isError" t))))

(defun request-method (request)
  (nlk:json-value request :string "method"))

(defun sent-arguments-line (request)
  "The arguments object of a tools/call as JSON text, or NIL when it carried none."
  (let ((arguments (nlk:json-value request :object "params" "arguments")))
    (when (and arguments (plusp (hash-table-count arguments)))
      (lovedone-json-text arguments))))

(defun default-answer (request)
  "What the real server answers a well-formed request: a handshake naming a
session, a text result naming the tool it ran, or 202 for a notification."
  (let ((method (request-method request)))
    (cond
      ((equal method "initialize")
       (values (lovedone-json-text (nlk:json-object
                            "jsonrpc" "2.0" "id" 1
                            "result" (nlk:json-object
                                      "protocolVersion" "2025-06-18"
                                      "capabilities" (nlk:json-object)
                                      "serverInfo" (nlk:json-object "name" "lovedone-family"))))
               200 (list (cons "mcp-session-id" "session-1"))))
      ((equal method "notifications/initialized")
       (values "" 202 nil))
      ((equal method "tools/call")
       (values (result-body
                (format nil "ran ~a~@[: ~a~]"
                        (nlk:json-value request :string "params" "name")
                        (sent-arguments-line request)))
               200 (list (cons "content-type" "application/json"))))
      (t (values "" 400 nil)))))

(defun stub-http (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((request (and (stringp content) (ignore-errors (nlk:decode-json content)))))
    (push (list :method method :url url :headers headers :body request) *calls*)
    (if *answer* (funcall *answer* request) (default-answer request))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its requests recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-lovedone::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers
                                           :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it ---------------------------------------------------------

(defun lovedone-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'lovedone-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-lovedone (&body body)
  "Run BODY with the cell started on a test key and its wire stubbed."
  `(with-wire
     (with-cell-stop ((lovedone-run-start))
       ,@body)))

(defun lovedone-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the requests a call made ------------------------------------------------

(defun oldest-first (calls)
  (reverse calls))

(defun requests (&optional method)
  "The requests the stub took, oldest first, optionally of one METHOD."
  (let ((calls (oldest-first *calls*)))
    (if method (remove-if-not (lambda (call) (equal method (getf call :method))) calls) calls)))

(defun calls-of (&optional (index 0))
  "The INDEXth tools/call the stub took, oldest first."
  (let ((calls (remove-if-not (lambda (call) (equal "tools/call" (request-method (getf call :body))))
                               (oldest-first *calls*))))
    (nth index calls)))

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun called-tool (call)
  (nlk:json-value (getf call :body) :string "params" "name"))

(defun sent-arguments (call)
  (nlk:json-value (getf call :body) :object "params" "arguments"))
