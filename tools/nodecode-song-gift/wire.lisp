;;;; wire.lisp --- the one network seam: JSON-RPC over MCP's streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin pointed Hermes at a hosted MCP server; a cell has no MCP
;;;; client, so this file is the small one it needs. Every JSON-RPC message is
;;;; POSTed to the one endpoint URL. A request's POST is answered either with
;;;; one JSON message or with an SSE body that carries the reply; the
;;;; Mcp-Session-Id header the initialize answer issues is echoed on the rest
;;;; of that call's messages. Nothing is kept between calls: one tool call is
;;;; initialize, notifications/initialized, tools/call, and the session ends
;;;; with the answer, so a restart never holds a stale id.
;;;;
;;;; Every byte goes through POST, so a test stubs NLK:HTTP and never touches
;;;; the network. A refused status or a transport failure is SONG-GIFT-ERROR
;;;; with the service's own words in it, which is what the model repairs
;;;; from.

(in-package #:nodecode-song-gift)

(defparameter +endpoint+ "https://customsonggift.org/api/mcp"
  "The public catalog the plugin's mcp.json names: where every tool call goes.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks; the server may answer with one it prefers.")

(defparameter +client-name+ "nodecode-song-gift")
(defparameter +client-version+ "0.1.0")

(defvar *request-id* 0
  "The JSON-RPC id of the last message this cell sent.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun http-scheme-p (url)
  "Whether URL names an http:// or https:// address."
  (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url)))

(defun endpoint ()
  "The catalog endpoint this cell talks to."
  (let ((given (setting :endpoint)))
    (or (nonempty (nlk:trimmed given)) +endpoint+)))

(defun message-headers (session-id)
  "The headers every message carries: JSON, and both answer shapes; the
session id and protocol version once initialize answered with them."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream"))
          (when (nonempty session-id)
            (list (cons "mcp-session-id" session-id)
                  (cons "mcp-protocol-version" +protocol-version+)))))

(defun response-header (headers name)
  "NAME's value from a decoded header table."
  (and (hash-table-p headers) (gethash name headers)))

(defun decode-message (text)
  "TEXT as one JSON-RPC message, or NIL when it is not a JSON object."
  (let ((value (and (stringp text)
                    (plusp (length (nlk:trimmed text)))
                    (handler-case (nlk:decode-json text) (error () nil)))))
    (and (hash-table-p value) value)))

(defun reply-message-p (message)
  "Whether MESSAGE answers a request: it carries a result or an error."
  (and message (or (nth-value 1 (gethash "result" message))
                   (nth-value 1 (gethash "error" message)))))

(defun sse-messages (text)
  "The `data:' events of an SSE body TEXT, as decoded messages, in order."
  (let ((messages '())
        (data '()))
    (flet ((dispatch ()
             (when data
               (let ((message (decode-message
                               (format nil "~{~a~^~%~}" (nreverse (shiftf data '()))))))
                 (when message (push message messages))))))
      (with-input-from-string (stream text)
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((line (string-right-trim '(#\Return) line)))
                   (cond ((zerop (length line)) (dispatch))
                         ((uiop:string-prefix-p "data:" line)
                          (push (string-left-trim " " (subseq line 5)) data)))))
      (dispatch)))
    (nreverse messages)))

(defun post (message &key session-id)
  "POST one JSON-RPC MESSAGE to the endpoint => (values MESSAGES STATUS
SESSION-ID). An SSE answer is read up to its reply; a notification's 202
carries none."
  (let ((timeout (setting :timeout-seconds))
        (body nil)
        (status nil)
        (headers nil))
    (handler-case
        (multiple-value-setq (body status headers)
          (nlk:http :post (endpoint)
                    :headers (message-headers session-id)
                    :content message
                    :timeout timeout
                    :connect-timeout (min 10 timeout)
                    :stream t))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "song-gift: ~a" (nle:transport-failure-label condition (endpoint)))))
    (let ((messages '())
          (type (string-downcase (or (response-header headers "content-type") ""))))
      (flet ((collect (message) (when message (push message messages))))
        (cond
          ((streamp body)
           (unwind-protect
                (cond ((member status '(202 204)) nil)
                      ((uiop:string-prefix-p "text/event-stream" type)
                       (nlk:read-events body
                                         (lambda (text)
                                           (let ((message (decode-message text)))
                                             (collect message)
                                             (reply-message-p message)))
                                         :idle timeout))
                      (t (collect (decode-message (nlk:body-text body (* 2 1024 1024))))))
             (ignore-errors (close body :abort t))))
          ((stringp body)
           (if (uiop:string-prefix-p "text/event-stream" type)
               (mapc #'collect (sse-messages body))
               (collect (decode-message body))))
          ((vectorp body) (collect (decode-message (nlk:decode-octets body type))))))
      (values (nreverse messages) status
              (response-header headers "mcp-session-id")))))

(defun json-line (value limit)
  "VALUE -- decoded JSON -- as one clipped line, for a failure's report."
  (let ((text (handler-case (nlk:encode-json-object value) (error () (format nil "~s" value)))))
    (if (and (stringp text) (> (length text) limit))
        (concatenate 'string (subseq text 0 limit) "...")
        (format nil "~a" text))))

(defun error-text (error)
  "ERROR -- a JSON-RPC error object -- as the one line the model reads."
  (or (and (hash-table-p error) (nlk:json-value error :text "message"))
      (and (hash-table-p error) (nlk:json-value error :text "data"))
      (json-line error 300)))

(defun request (method params &key session-id)
  "One JSON-RPC request METHOD => (values RESULT SESSION-ID)."
  (let* ((id (incf *request-id*))
         (text (nlk:encode-json-object
                (nlk:make-json-object "jsonrpc" "2.0" "id" id
                                       "method" method "params" params))))
    (multiple-value-bind (messages status session)
        (post text :session-id session-id)
      (let ((reply (find-if #'reply-message-p messages)))
        (cond
          ((null reply)
           (fail "song-gift ~a: HTTP ~a and no JSON-RPC answer~@[: ~a~]"
                 method status
                 (and messages (json-line (first messages) 300))))
          ((nth-value 1 (gethash "error" reply))
           (fail "song-gift ~a: ~a" method (error-text (gethash "error" reply))))
          (t (values (gethash "result" reply) session)))))))

(defun notify (method)
  "One JSON-RPC notification: the server answers 202 and nothing else."
  (post (nlk:encode-json-object
         (nlk:make-json-object "jsonrpc" "2.0" "method" method "params" (nlk:json-object)))))

(defun open-session ()
  "initialize, then notifications/initialized => the session id the server issued."
  (multiple-value-bind (result session)
      (request "initialize"
               (nlk:make-json-object
                "protocolVersion" +protocol-version+
                "capabilities" (nlk:json-object)
                "clientInfo" (nlk:make-json-object "name" +client-name+
                                                    "version" +client-version+)))
    (declare (ignore result))
    (notify "notifications/initialized")
    session))

(defun call-tool (name arguments)
  "Call the server's tool NAME with ARGUMENTS -- a JSON object -- => its result."
  (let ((session (open-session)))
    (request "tools/call"
             (nlk:make-json-object "name" name
                                    "arguments" (or arguments (nlk:json-object)))
             :session-id session)))
