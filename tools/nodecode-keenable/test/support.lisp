;;;; support.lisp --- the test slice: a stubbed Keenable MCP endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; KEENABLE-CELL- name prefix; RUN-KEENABLE-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-KEENABLE::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned MCP answer
;;;; and records every POST, so a test asserts on the JSON-RPC message a call
;;;; sent and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "keenable" "KEENABLE-CELL-" :start nodecode-keenable:start-cell)

;;; --- the canned answers -----------------------------------------------------------

(defparameter +search-text+
  (format nil "Title: Keenable MCP server~%URL: https://docs.keenable.ai/mcp-server~%~
Acquired: 2026-11-01~%Snippets:~%Search the web from an agent, keyless.~2%~~
Title: Rate limits~%URL: https://docs.keenable.ai/rate-limits~%~
Published: 2026-10-29~%Acquired: 2026-11-07~%Snippets:~%1,000 requests an hour.")
  "What a stubbed search answers, in the shape the server answers it.")

(defparameter +page-text+
  "The unauthenticated pool answers 1,000 requests an hour and at most 10 a second."
  "What a stubbed fetch answers.")

(defparameter +refusal+
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Unknown parameter(s): numResults\"}}"
  "What the server answers an argument it does not know.")

(defun content-answer (text)
  "TEXT as a tools/call result, as the server wraps it."
  (nlk:encode-json-object
   (nlk:json-object
    "jsonrpc" "2.0" "id" 1
    "result" (nlk:json-object
              "content" (vector (nlk:json-object "type" "text" "text" text))))))

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every POST the stub took, newest first: the parsed message with its headers.")

(defvar *answer* nil
  "A function of one parsed message answering (values BODY STATUS), or NIL
for the canned answers.")

(defun tool-name-of (message)
  (nlk:json-value (nlk:json-value message :object "params") :string "name"))

(defun default-answer (message)
  "The canned answer to MESSAGE: a handshake when asked, else the tool's text."
  (let ((method (nlk:json-value message :string "method")))
    (cond
      ((equal method "initialize")
       (values (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0" "id" 1
                                 "result" (nlk:json-object "protocolVersion" "2025-06-18")))
               200))
      ((equal method "notifications/initialized") (values "" 202))
      ((equal method "tools/call")
       (values (content-answer (if (equal "fetch_page_content" (tool-name-of message))
                                   +page-text+
                                   +search-text+))
               200))
      (t (values "{}" 200)))))

(defun stub-http (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((message (ignore-errors (nlk:decode-json content))))
    (push (list :method method :url url :headers headers :content content :message message)
          *calls*)
    (if *answer*
        (funcall *answer* message)
        (default-answer message))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its POSTs recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-keenable::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers
                                                :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defmacro with-keenable ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS (as START-CELL takes them), its
wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((keenable-start ,@pairs))
       ,@body)))

(defun keenable-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the message a call sent -----------------------------------------------

(defun request-of (call)
  (getf call :message))

(defun request-method (call)
  (nlk:json-value (request-of call) :string "method"))

(defun request-parameters (call)
  (nlk:json-value (request-of call) :object "params"))

(defun request-tool (call)
  (nlk:json-value (request-parameters call) :string "name"))

(defun request-arguments (call)
  (nlk:json-value (request-parameters call) :object "arguments"))

(defun argument (call name)
  "The value CALL's arguments carry for NAME, or NIL when it carries none."
  (let ((arguments (request-arguments call)))
    (and arguments (gethash name arguments))))

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
