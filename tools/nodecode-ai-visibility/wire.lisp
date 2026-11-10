;;;; wire.lisp --- the one network seam: one tools/call on the hosted server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the qqq-ai-visibility plugin for Hermes Agent. See
;;;; NOTICE.
;;;;
;;;; The plugin is a portable manifest with no code of its own: it points a
;;;; harness at the remote MCP server https://qqq.ee/mcp, whose three tools are
;;;; read-only and need no key. This speaks the same protocol by hand -- one
;;;; JSON-RPC 2.0 POST per call, with the headers a Streamable HTTP server
;;;; wants -- and reads the answer either as one JSON object or as the
;;;; server-sent events the server may answer with. The text the server wrote
;;;; for a person is what the model reads: it already carries its links, so it
;;;; is passed on whole, only bounded in length.
;;;;
;;;; NLK:HTTP is the one seam: a test stubs that name and never opens a socket.

(in-package #:nodecode-ai-visibility)

(nlk:define-peripheral ai-visibility :not-running t)

(defparameter +endpoint+ "https://qqq.ee/mcp"
  "Where the QQQ checker answers tools/call.")
(defparameter +user-agent+ "nodecode-ai-visibility/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "What qqq.ee sees: the cell's name and version and where it comes from, nothing about the machine.")
(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision the request names, so a server that reads it picks the shape this reads.")
(defparameter +languages+ '("en" "et")
  "The two languages the server answers in: English, and Estonian for an Estonian site.")
(defparameter +max-response-bytes+ (* 4 1024 1024) "A bigger answer is not read.")
(defparameter +max-text-chars+ 12000
  "How much of the server's text one answer carries; the rest is on the linked page.")

(defvar *request-id* 0
  "The JSON-RPC id of the last call. Counted up per call, so an answer can be matched to its request.")

;;; --- the one network call ------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub this name."
  (apply #'nlk:http method url arguments))

(defun endpoint ()
  "The URL this cell calls: the section's, or the hosted server's."
  (let ((named (and (setting :endpoint) (nlk:trimmed (setting :endpoint)))))
    (if (and named (plusp (length named))) named +endpoint+)))

(defun body-text (body)
  "BODY -- a string, or the octets MAX-BYTES answers with -- as text."
  (cond ((stringp body) body)
        ((and (vectorp body) (not (stringp body)))
         (or (ignore-errors (sb-ext:octets-to-string body :external-format :utf-8)) ""))
        (t "")))

(defun one-line (text &optional (cap 300))
  "TEXT as one short line, cut at CAP characters."
  (let ((line (nlk:one-line (or text ""))))
    (if (> (length line) cap)
        (concatenate 'string (subseq line 0 cap) " [...]")
        line)))

(defun bounded (text)
  "TEXT cut to what one answer carries."
  (if (> (length text) +max-text-chars+)
      (concatenate 'string (subseq text 0 +max-text-chars+)
                   (format nil "~%[... cut at ~d characters]" +max-text-chars+))
      text))

(defun readable (condition)
  "CONDITION as one short line."
  (one-line (or (ignore-errors (princ-to-string condition)) (type-of condition))))

(defun post (body timeout)
  "POST one JSON-RPC BODY to the endpoint; => (values TEXT STATUS)."
  (multiple-value-bind (reply status)
      (handler-case
          (http :post (endpoint)
                :headers (list (cons "content-type" "application/json")
                               (cons "accept" "application/json, text/event-stream")
                               (cons "user-agent" +user-agent+)
                               (cons "mcp-protocol-version" +protocol-version+))
                :content body :timeout timeout :max-bytes +max-response-bytes+)
        (nlk:turn-cancelled-condition (condition) (error condition))
        (error (condition)
          (fail "could not reach ~a: ~a" (endpoint) (readable condition))))
    (values (body-text reply) status)))

;;; --- reading the answer ---------------------------------------------------------

(defun decode-json (text)
  "TEXT as decoded JSON, or NIL when it is not one JSON value."
  (and (plusp (length text)) (ignore-errors (nlk:decode-json text :whole t))))

(defun sse-payloads (text)
  "TEXT's event-stream data payloads, in the order they arrived: each `data:'
line, with consecutive ones joined, as the event-stream format says."
  (let ((payloads '()) (current '()))
    (labels ((flush ()
               (when current
                 (setf payloads (append payloads (list (format nil "~{~a~^~%~}" (reverse current)))))
                 (setf current nil))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (let ((line (string-right-trim '(#\Return) line)))
          (cond ((zerop (length line)) (flush))
                ((uiop:string-prefix-p "data:" line)
                 (setf current (cons (string-left-trim " " (subseq line 5)) current)))
                ;; "event:", "id:", "retry:" and ":" comments say nothing this cell reads.
                (t nil))))
      (flush))
    payloads))

(defun envelope (text)
  "The JSON-RPC object TEXT carries, whether it came as JSON or as events."
  (let ((whole (decode-json text)))
    (if (hash-table-p whole)
        whole
        (let ((objects (loop for payload in (sse-payloads text)
                             for object = (decode-json payload)
                             when (hash-table-p object) collect object)))
          (or (find-if (lambda (object)
                         (or (nlk:json-value object :object "result")
                             (nlk:json-value object :object "error")))
                       (reverse objects))
              (first objects))))))

(defun envelope-error (object)
  "OBJECT's JSON-RPC error as one line, or NIL when it carries none."
  (let ((error (nlk:json-value object :object "error")))
    (when error
      (let ((message (nlk:json-value error :string "message")))
        (if (and message (plusp (length (nlk:trimmed message))))
            (nlk:trimmed message)
            "an error with no message")))))

(defun result-text (result)
  "The text in RESULT's content, or its structured content, or NIL."
  (let ((pieces (loop for item across (nlk:json-array result "content")
                      for text = (nlk:json-value item :string "text")
                      when (and text (plusp (length (nlk:trimmed text))))
                        collect (nlk:trimmed text))))
    (or (and pieces (format nil "~{~a~^~%~%~}" pieces))
        (let ((structured (nlk:json-value result :object "structuredContent")))
          (and structured (ignore-errors (nlk:encode-json-object structured)))))))

(defun rpc-request (identifier tool arguments)
  "The JSON-RPC 2.0 tools/call body for TOOL with ARGUMENTS."
  (nlk:encode-json-object
   (nlk:make-json-object
    "jsonrpc" "2.0"
    "id" identifier
    "method" "tools/call"
    "params" (nlk:make-json-object "name" tool "arguments" arguments))))

;;; --- one tool, one answer --------------------------------------------------------

(defun call-tool (tool arguments)
  "Call the server's TOOL with ARGUMENTS; => the text it wrote for the person.
An answer the server marks as an error is a refusal carrying its own words,
which name the reason and still carry a link."
  (let* ((identifier (incf *request-id*))
         (body (rpc-request identifier tool arguments))
         (answer (multiple-value-bind (text status) (post body (setting :timeout-seconds))
                   (cons text status)))
         (text (car answer))
         (status (cdr answer))
         (object (envelope text)))
    (when (and (null object) (>= status 400))
      (fail "~a answered HTTP ~d for ~a~@[~:;: ~a~]" (endpoint) status tool
            (and (plusp (length (nlk:trimmed text))) (one-line text))))
    (unless object
      (fail "~a answered ~a with nothing that is a tool result (HTTP ~d)"
            (endpoint) tool status))
    (let ((refusal (envelope-error object)))
      (when refusal
        (fail "~a refused ~a: ~a" (endpoint) tool (one-line refusal))))
    (let ((result (nlk:json-value object :object "result")))
      (unless result
        (fail "~a answered ~a with no result (HTTP ~d)" (endpoint) tool status))
      (let ((rendered (result-text result)))
        (unless rendered
          (fail "~a answered ~a with no text this cell could read (HTTP ~d)"
                (endpoint) tool status))
        (if (nlk:json-value result :boolean "isError")
            (fail "~a" (bounded rendered))
            (bounded rendered))))))
