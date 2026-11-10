;;;; wire.lisp --- the one way out: JSON-RPC 2.0 over Streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin carried no code of its own: its mcp.json pointed the
;;;; harness at a hosted MCP endpoint, and the server there served the tools.
;;;; This file is the client half -- handshake, the session header, tools/call
;;;; and the event-stream framing the transport allows -- written against the
;;;; image's own HTTP call. Nothing here is fetched or compiled at run time.

(in-package #:nodecode-lovedone)

(defconstant +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks: initialize, tools/call, text results.")

(defvar *session* nil
  "The Mcp-Session-Id an initialize answered with, or NIL before the first call.")

(defvar *rpc-id* 0
  "The JSON-RPC request id the last request carried.")

(defvar *handshaked* nil
  "True once an initialize has been answered -- a server may answer a call on
no session at all, and still wants the one handshake.")

(defun http (method url &key headers content timeout)
  "The cell's one way to the network: the image's own HTTP call, answering
(values BODY STATUS HEADERS TRUNCATED-P)."
  (nlk:http method url :headers headers :content content :timeout timeout))

(defun endpoint ()
  "The endpoint the section names."
  (setting :url))

(defun request-headers (&optional session)
  "What every request carries: the family's MCP key as a bearer token, and
the session the handshake named."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (setting :api-key))))
          (and session (list (cons "mcp-session-id" session)))))

(defun response-header (headers name)
  "NAME's value in HEADERS -- the hash table the image's call answers, or the
alist a stub answers."
  (cond ((hash-table-p headers)
         (or (gethash name headers)
             (gethash (string-downcase name) headers)))
        ((consp headers)
         (cdr (assoc name headers :test #'string-equal)))))

(defun clip (text &optional (limit 300))
  "TEXT trimmed and shortened, for a condition's one sentence."
  (let ((text (and (stringp text)
                    (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (cond ((null text) nil)
          ((<= (length text) limit) text)
          (t (concatenate 'string (subseq text 0 limit) "...")))))

(defun event-data (body)
  "The payload of the last `data:' line BODY carries, or NIL."
  (let ((data nil))
    (with-input-from-string (stream body)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((line (string-right-trim '(#\Return) line)))
                 (when (uiop:string-prefix-p "data:" line)
                   (setf data (string-left-trim '(#\Space) (subseq line 5)))))))
    data))

(defun decode-answer (body)
  "The JSON-RPC object BODY carries -- a plain body, or the last event of an
event stream -- or NIL when it carries none."
  (let* ((trimmed (and (stringp body) (nlk:trimmed body)))
         (text (cond ((null trimmed) nil)
                     ((char= #\{ (char trimmed 0)) trimmed)
                     (t (event-data trimmed)))))
    (when (and text (plusp (length (nlk:trimmed text))))
      (handler-case (nlk:decode-json text) (error () nil)))))

(defun rpc-error-text (answer)
  "The reason ANSWER's error member gives, or its first bytes."
  (or (nlk:json-value answer :string "error" "message")
      (clip (nlk:encode-json-object answer))
      "no reason given"))

(defun rpc (payload &key session)
  "POST one JSON-RPC PAYLOAD to the section's endpoint.
=> (values ANSWER SESSION-ID); a transport refusal signals LOVEDONE-ERROR."
  (multiple-value-bind (body status headers)
      (http :post (endpoint)
            :headers (request-headers session)
            :content (nlk:encode-json-object payload)
            :timeout (setting :timeout-seconds))
    (let ((answer (decode-answer body)))
      (cond ((null answer)
             (fail "~a answered HTTP ~a with nothing this client reads~@[: ~a~]"
                   (endpoint) (or status "no status") (clip body)))
            ((and status (>= status 400) (null (nlk:json-value answer :object "result")))
             (fail "~a refused the call: HTTP ~d, ~a"
                   (endpoint) status (rpc-error-text answer)))
            (t (values answer (response-header headers "mcp-session-id")))))))

(defun initialize ()
  "One MCP handshake. => the session id the server named, or NIL."
  (multiple-value-bind (answer session)
      (rpc (nlk:json-object
            "jsonrpc" "2.0"
            "id" (incf *rpc-id*)
            "method" "initialize"
            "params" (nlk:json-object
                      "protocolVersion" +protocol-version+
                      "capabilities" (nlk:json-object)
                      "clientInfo" (nlk:json-object
                                    "name" "nodecode"
                                    "version" "1"))))
    (declare (ignore answer))
    session))

(defun ensure-session ()
  "The session this cell talks on, handshaking once and remembering it."
  (unless *handshaked*
    (let ((session (initialize)))
      (setf *session* session
            *handshaked* t)
      (when session
        (ignore-errors
          (rpc (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized")
               :session session)))))
  *session*)

(defun one-answer (tool arguments)
  "One tools/call, on the session this cell already has."
  (multiple-value-bind (answer session)
      (rpc (nlk:json-object
            "jsonrpc" "2.0"
            "id" (incf *rpc-id*)
            "method" "tools/call"
            "params" (nlk:json-object
                      "name" tool
                      "arguments" (or arguments (nlk:json-object))))
           :session *session*)
    (setf *session* (or session *session*))
    answer))

(defun tool-answer (tool arguments)
  "TOOL's whole JSON-RPC answer. A server that dropped the session is asked
again once, on a fresh handshake, before the model hears about it."
  (ensure-session)
  (handler-case (one-answer tool arguments)
    (lovedone-error (condition)
      (if (cl:search "session" (princ-to-string condition) :test #'char-equal)
          (progn (setf *session* nil
                       *handshaked* nil)
                 (ensure-session)
                 (one-answer tool arguments))
          (error condition)))))

(defun result-text (tool answer)
  "ANSWER's result as the text a model reads: its content blocks joined."
  (let ((error (nlk:json-value answer :object "error")))
    (when error
      (fail "~a failed: ~a" tool (rpc-error-text answer))))
  (let* ((result (nlk:json-value answer :object "result"))
         (text (with-output-to-string (out)
                 (loop for block across (or (nlk:json-value result :array "content") #())
                       for piece = (nlk:json-value block :string "text")
                       when piece
                         do (write-string piece out)
                            (terpri out)))))
    (cond ((plusp (length (nlk:trimmed text)))
           (let ((text (string-right-trim '(#\Newline) text)))
             (if (nlk:json-value result :boolean "isError")
                 (format nil "ERROR: ~a" text)
                 text)))
          ((null result) (format nil "~a answered no result" tool))
          (t (nlk:encode-json-object result)))))

(defun call (tool &optional arguments)
  "Call the LovedOne MCP TOOL with ARGUMENTS -- a JSON object the server
takes as the tool's arguments, or NIL when it takes none. => the server's
own text, the one thing the original plugin's tools answered with."
  (result-text tool (tool-answer tool arguments)))
