;;;; wire.lisp --- the one network seam: one MCP tools/call over HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin shipped Developh's hosted MCP server through a manifest, so the
;;;; harness discovered four tools the server owns. This cell makes the same
;;;; call itself: one JSON-RPC POST per tool call, no session, which is what
;;;; the plugin's own skill does with curl when no MCP client is around (see
;;;; NOTICE). The answer is either a JSON body or an SSE stream of
;;;; `event: message' and `data: {...}' lines; both are read here.
;;;;
;;;; Every byte goes through DEVELOPH-CALL, so a test stubs NLK:HTTP and never
;;;; touches the network. A JSON-RPC error, a refused status, a tool result
;;;; marked as an error, or an answer with no text signals DEVELOPH-ERROR
;;;; carrying the service's own words, which is what the model repairs from.

(in-package #:nodecode-developh)

(defparameter +endpoint+ "https://www.developh.co/mcp"
  "Developh's MCP endpoint, where every tool goes unless the section names another.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun endpoint ()
  "The MCP endpoint this cell posts to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :endpoint) ""))))
    (if (plusp (length given)) given +endpoint+)))

(defun check-config ()
  "Refuse a section whose endpoint is not an http(s) address, naming the member."
  (let ((address (endpoint)))
    (unless (or (uiop:string-prefix-p "https://" address)
                (uiop:string-prefix-p "http://" address))
      (error 'nlk:config-refusal
             :detail (format nil "developh: \"endpoint\" must be an http:// or https:// address, not ~s; ~
the service it names is ~a" address +endpoint+)))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun bolt (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL VALUE: a
tool call carries only the members the person actually gave."
  (loop for (key value) on pairs by #'cddr
        when value append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

;;; --- reading the answer --------------------------------------------------------

(defun sse-data-lines (body)
  "BODY's `data:' payloads, in order: an SSE answer is several lines and the
one JSON-RPC message among them."
  (let ((payloads '()))
    (with-input-from-string (stream body)
      (loop for line = (read-line stream nil nil)
            while line
            for clean = (string-right-trim '(#\Return #\Space #\Tab) line)
            do (when (and (>= (length clean) 5)
                          (string-equal "data:" clean :end2 5))
                 (push (string-left-trim " " (subseq clean 5)) payloads))))
    (nreverse payloads)))

(defun rpc-message (answer)
  "ANSWER -- an HTTP body -- as the JSON-RPC message it carries, or an empty
object when it carries none. A plain JSON body and an SSE stream both read."
  (let* ((text (cond ((stringp answer) answer)
                      ((vectorp answer) (nlk:encode-json-object answer))
                      (t "")))
         (payloads (sse-data-lines text))
         (chosen nil))
    (dolist (payload (if payloads payloads (list text)))
      (let ((value (and (plusp (length (nlk:trimmed payload)))
                        (ignore-errors (nlk:decode-json payload)))))
        (when (and (hash-table-p value)
                   (or (nlk:json-value value :object "result")
                       (nlk:json-value value :object "error")))
          (setf chosen value))))
    (or chosen (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :text "detail")
        (nlk:json-value body :text "message")
        (nlk:json-value body :object "error")
        (and (stringp answer)
             (plusp (length answer))
             (subseq answer 0 (min 200 (length answer)))))))

(defun result-text (result)
  "RESULT -- an MCP tool result -- as the text of its content parts, and
whether the tool marked itself as an error, as two values."
  (let ((content (nlk:json-value result :array "content")))
    (values (format nil "~{~a~^~%~}"
                    (loop for part across (or content #())
                          when (hash-table-p part)
                            collect (or (nlk:json-value part :text "text") "")))
            (not (null (nlk:json-value result :boolean "isError"))))))

(defun developh-call (tool arguments &key timeout)
  "TOOL -- a name on Developh's MCP server -- called with ARGUMENTS, a JSON
object => its answer as text. A transport failure, a refused status, a JSON-RPC
error or an empty answer signals DEVELOPH-ERROR."
  (let* ((url (endpoint))
         (limit (or timeout (setting :timeout-seconds)))
         (body (nlk:make-json-object
                "jsonrpc" "2.0"
                "id" 1
                "method" "tools/call"
                "params" (nlk:make-json-object "name" tool "arguments" arguments)))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post url
                    :headers '(("content-type" . "application/json")
                               ("accept" . "application/json, text/event-stream"))
                    :content (nlk:encode-json-object body)
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "developh ~a: ~a" tool (nle:transport-failure-label condition url))))
    (when (and (integerp status) (>= status 300))
      (fail "developh ~a: HTTP ~d~@[: ~a~]" tool status (body-error-text answer)))
    (let* ((message (rpc-message answer))
           (refusal (nlk:json-value message :object "error")))
      (when refusal
        (fail "developh ~a: the service refused the call: ~a (code ~a)"
              tool
              (or (nlk:json-value refusal :text "message") "no message")
              (or (nlk:json-value refusal :integer "code") "none")))
      (multiple-value-bind (text is-error) (result-text (nlk:json-value message :object "result"))
        (let ((said (nonempty text)))
          (unless said
            (fail "developh ~a: the service answered with no text" tool))
          (if is-error
              (fail "developh ~a: ~a" tool said)
              said))))))
