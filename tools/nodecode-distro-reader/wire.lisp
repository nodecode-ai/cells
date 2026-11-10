;;;; wire.lisp --- the one network seam, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin pointed Hermes at Distro's hosted MCP server and let the MCP
;;;; client do the talking. A cell has no MCP client, and the endpoint needs
;;;; none: it is stateless Streamable HTTP, so one JSON-RPC POST per call is
;;;; the whole transport (no handshake, no session id). Every byte this cell
;;;; moves goes through RPC, so a test stubs NLK:HTTP and never touches the
;;;; network.
;;;;
;;;; A JSON-RPC error, a refused status and an answer that carries no result
;;;; are DISTRO-READER-ERROR, which the model sees as the tool result and
;;;; repairs from. A tool result the server itself marks failed (isError) is
;;;; not a condition: its text is the answer, prefixed with ERROR:.

(in-package #:nodecode-distro-reader)

(defparameter +endpoint+ "https://reader.distro.media/mcp/reader/stream"
  "Distro's public reader, where every call goes unless the section names another.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision the endpoint speaks; it rides every request as a header.")

(defvar *rpc-id* 0
  "The last JSON-RPC id this process sent. Ids only have to be distinct.")

;;; --- the section's own values ------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun check-endpoint (given)
  "GIVEN -- the section's endpoint -- as the root this cell dials, a trailing
slash gone; anything that is not an http(s) address is a config refusal."
  (let ((text (and (stringp given) (nlk:trimmed given))))
    (unless (and (plusp (length text))
                 (or (uiop:string-prefix-p "https://" text)
                     (uiop:string-prefix-p "http://" text)))
      (error 'nlk:config-refusal
             :detail (concatenate
                      'string
                      "distro-reader: \"endpoint\" must be the reader's http(s) address; "
                      "leave it alone for " +endpoint+
                      ", and name another only to go through a mirror of your own")))
    (string-right-trim "/" text)))

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with the endpoint
proved to be an address this image can dial."
  (declare (ignore table))
  (list :endpoint (check-endpoint (getf values :endpoint))
        :timeout-seconds (getf values :timeout-seconds)
        :html-delivery (getf values :html-delivery)
        :page-limit (getf values :page-limit)
        :max-chars (getf values :max-chars)))

(defun endpoint ()
  "The reader endpoint this cell talks to."
  (or (setting :endpoint) +endpoint+))

(defun plain (value)
  "VALUE as the JSON boolean or value a request carries: NIL becomes JSON
false, never JSON null, so a member the call turned off arrives turned off."
  (if (eq value t) t (if (null value) :false value)))

;;; --- one exchange ------------------------------------------------------------

(defun post (message)
  "POST MESSAGE -- a JSON-RPC object -- to the endpoint => (values BODY STATUS).
A failure below HTTP -- no address, no answer -- is DISTRO-READER-ERROR."
  (let ((timeout (setting :timeout-seconds)))
    (handler-case
        (nlk:http :post (endpoint)
                  :headers (list (cons "content-type" "application/json")
                                 (cons "accept" "application/json, text/event-stream")
                                 (cons "mcp-protocol-version" +protocol-version+))
                  :content (nlk:encode-json-object message)
                  :timeout timeout
                  :connect-timeout (min 10 timeout))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "distro-reader: ~a" (nle:transport-failure-label condition (endpoint)))))))

(defun snippet (answer)
  "ANSWER -- an HTTP body -- as one line, cut to 200 characters."
  (let ((text (and (stringp answer)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) answer))))
    (cond ((null text) nil)
          ((<= (length text) 200) text)
          (t (concatenate 'string (subseq text 0 200) "...")))))

(defun sse-p (text)
  "TEXT -- an HTTP body -- an event stream rather than one JSON document?"
  (let ((head (string-left-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (or (uiop:string-prefix-p "data:" head)
        (uiop:string-prefix-p "event:" head)
        (uiop:string-prefix-p "id:" head))))

(defun sse-message (text)
  "The first JSON-RPC message in TEXT -- an event stream -- as an object."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for trimmed = (string-left-trim '(#\Space #\Tab #\Return) line)
        when (uiop:string-prefix-p "data:" trimmed)
          do (let* ((payload (string-trim '(#\Space #\Tab #\Return) (subseq trimmed 5)))
                    (message (ignore-errors (nlk:decode-json payload))))
               (when (and (hash-table-p message)
                          (or (gethash "result" message) (gethash "error" message)))
                 (return message)))
        finally (return (nlk:json-object))))

(defun message-of (answer)
  "ANSWER -- the HTTP body, a JSON document or an event stream -- as the
JSON-RPC message it carries."
  (cond ((not (stringp answer)) (nlk:json-object))
        ((sse-p answer) (sse-message answer))
        (t (or (ignore-errors (nlk:decode-json answer)) (nlk:json-object)))))

(defun rpc (method &optional params)
  "One JSON-RPC call to the reader => the message's result object. A refused
status, a JSON-RPC error and a message with no result all signal
DISTRO-READER-ERROR, in the words the model repairs from."
  (let ((message (nlk:make-json-object "jsonrpc" "2.0"
                                        "id" (incf *rpc-id*)
                                        "method" method
                                        "params" (or params (nlk:json-object))))
        (answer nil)
        (status nil))
    (multiple-value-setq (answer status) (post message))
    (unless (and (integerp status) (< status 300))
      (cond ((eql status 429)
             (fail "distro-reader: ~a: HTTP 429, the reader rate-limits (60 requests a minute); wait a moment before asking again"
                   method))
            (t (fail "distro-reader: ~a: HTTP ~d~@[: ~a~]" method status (snippet answer)))))
    (let ((reply (message-of answer)))
      (let ((problem (nlk:json-value reply :object "error")))
        (when problem
          (fail "distro-reader: ~a: ~a" method
                (or (nlk:json-value problem :text "message")
                    (nlk:json-value reply :text "error")
                    "the reader refused the call"))))
      (or (nlk:json-value reply :object "result")
          (fail "distro-reader: ~a answered no result~@[: ~a~]" method (snippet answer))))))

;;; --- a tool's answer ---------------------------------------------------------

(defun structured-text (result)
  "RESULT's structuredContent as text, when the content array carried none."
  (let ((structured (nlk:json-value result :object "structuredContent")))
    (and structured (or (ignore-errors (nlk:encode-json-object structured)) nil))))

(defun result-text (result)
  "RESULT -- a tools/call result -- as the text the model reads."
  (let ((parts (loop for item across (or (nlk:json-value result :array "content") #())
                     for text = (nlk:json-value item :text "text")
                     when text collect text)))
    (cond (parts (format nil "~{~a~^~%~%~}" parts))
          ((structured-text result))
          (t "(the reader answered nothing)"))))

(defun call-tool (name arguments)
  "NAME -- one of the reader's eight tools -- called with ARGUMENTS, a JSON
object => the text the model reads. A tool result the server marked failed
comes back as ERROR: and is not a condition."
  (let* ((params (nlk:make-json-object "name" name
                                        "arguments" (or arguments (nlk:json-object))))
         (result (rpc "tools/call" params))
         (text (result-text result)))
    (if (nlk:json-value result :boolean "isError")
        (concatenate 'string "ERROR: " (nlk:trimmed text))
        text)))

(defun arguments (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL VALUE: a
tool call carries only what the call asked for. A boolean goes through
PLAIN first, so a turned-off member is JSON false and not dropped."
  (let ((object (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr
          when (not (null value))
            do (setf (gethash key object) value))
    object))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left; LIMIT 0 or NIL
leaves it whole."
  (cond ((null text) nil)
        ((not (and (integerp limit) (plusp limit))) text)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters; html_delivery \"link\" answers the url of the whole page]"
                   (subseq text 0 limit) (- (length text) limit)))))
