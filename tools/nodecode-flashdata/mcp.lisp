;;;; mcp.lisp --- the one network seam: JSON-RPC over FlashData's streamable-HTTP MCP endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was declarative: an mcp.json naming the hosted server,
;;;; a key in an X-API-Key header, and fifteen Skills over its tools. Hermes
;;;; owned the transport, so the package itself moved no bytes. This cell owns
;;;; it: one POST of one JSON-RPC message, the handshake MCP asks for, and the
;;;; answer read whether the server frames it as JSON or as an event stream.
;;;;
;;;; Every byte goes through REQUEST, so a test stubs that one name and never
;;;; touches the network. A refused status and a JSON-RPC error are both
;;;; FLASHDATA-ERROR, with the key redacted out of every byte the model sees.

(in-package #:nodecode-flashdata)

(defparameter +default-url+ "https://data.flashdata.dev/mcp"
  "FlashData's hosted MCP endpoint: where every call goes unless the section names another.")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client asks for, the one the hosted server answers.")

(defvar *session-id* nil
  "The Mcp-Session-Id the server handed this process, or NIL before a handshake.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request; one per message, never reused.")

(defvar *session-lock* (bt2:make-lock :name "flashdata session")
  "One exchange at a time: a session opens once, and a retry cannot race a second.")

;;; --- the section, read ------------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, trimmed, else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun api-key ()
  "The key the section names inline or by variable, else FLASHDATA_API_KEY."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "FLASHDATA_API_KEY"))))

(defun check-config ()
  "Refuse a section that would reach FlashData with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "flashdata: this cell needs a FlashData key: set FLASHDATA_API_KEY, "
                                 "or \"api_key\" (or \"api_key_env\") in the flashdata section; a key "
                                 "is made at flashdata.dev/app/api-keys, and every data query spends "
                                 "that account's Credits"))))

(defun root ()
  "The endpoint this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-url+)))

(defun redact (text)
  "TEXT with the key replaced, so no answer ever carries it."
  (let ((key (api-key)))
    (if (and key (stringp text))
        (with-output-to-string (out)
          (loop with start = 0
                for found = (cl:search key text :start2 start)
                do (if found
                       (progn (write-string text out :start start :end found)
                              (write-string "[redacted]" out)
                              (setf start (+ found (length key))))
                       (progn (write-string text out :start start)
                              (return)))))
        text)))

;;; --- one POST ---------------------------------------------------------------------

(defun octets (text)
  "TEXT as the UTF-8 octets a JSON body goes out as."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun header-value (headers name)
  "HEADERS -- a hash table or an alist, as NLK:HTTP answers -- carrying NAME, case-insensitively."
  (cond ((null headers) nil)
        ((hash-table-p headers)
         (or (gethash name headers)
             (gethash (string-downcase name) headers)
             (loop for key being the hash-keys of headers
                   when (string-equal (string key) name)
                     return (gethash key headers))))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun request (payload &key timeout)
  "One POST of PAYLOAD -- a JSON text -- to the endpoint. => (values TEXT STATUS HEADERS).
The seam every test stubs: nothing else in this cell reaches the network."
  (nlk:http :post (root)
            :headers (append (list (cons "content-type" "application/json")
                                   (cons "accept" "application/json, text/event-stream")
                                   (cons "x-api-key" (or (api-key) ""))
                                   (cons "mcp-protocol-version" +protocol-version+))
                             (when *session-id*
                               (list (cons "mcp-session-id" *session-id*))))
            :content (octets payload)
            :timeout timeout
            :connect-timeout (min 10 timeout)))

(defun status-failure (status text)
  "The refusal a refused HTTP status is, or NIL for a status that answers."
  (cond ((or (null status) (and (integerp status) (< status 300))) nil)
        ((eql status 401)
         "HTTP 401: FlashData refused the key; check api_key (or FLASHDATA_API_KEY) in the flashdata section")
        ((eql status 403)
         "HTTP 403: this key may not use that source; check the key's sources at flashdata.dev")
        ((eql status 404)
         "HTTP 404: nothing answers at that path; check base_url in the flashdata section")
        ((eql status 429)
         "HTTP 429: the account's rate limit answered; wait before calling again")
        ((and (integerp status) (>= status 500))
         (format nil "HTTP ~d: FlashData had an upstream error; try again shortly" status))
        (t (format nil "HTTP ~d~@[ - ~a~]" status
                   (and (stringp text) (plusp (length (nlk:trimmed text)))
                        (subseq text 0 (min 300 (length text))))))))

(defun sse-messages (text)
  "The JSON texts TEXT's server-sent events carry, oldest first."
  (let ((messages '())
        (current '()))
    (flet ((flush ()
             (when current
               (push (format nil "~{~a~}" (nreverse current)) messages)
               (setf current nil))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (let ((line (string-right-trim '(#\Return) line)))
          (cond ((zerop (length (nlk:trimmed line))) (flush))
                ((and (>= (length line) 5) (string-equal "data:" (subseq line 0 5)))
                 (push (string-left-trim " " (subseq line 5)) current)))))
      (flush))
    (nreverse messages)))

(defun message-of (text)
  "TEXT as the JSON-RPC message it carries -- a JSON body, or the first event that parses."
  (let* ((trimmed (string-left-trim '(#\Space #\Newline #\Return #\Tab) text))
         (candidates (if (and (plusp (length trimmed)) (char= #\{ (char trimmed 0)))
                         (list trimmed)
                         (sse-messages text))))
    (loop for candidate in candidates
          for value = (handler-case (nlk:decode-json candidate :whole t) (error () nil))
          when (hash-table-p value)
            return value)))

(defun rpc (method params &key timeout)
  "One JSON-RPC request. => (values RESULT ERROR-TEXT); both NIL for a notification's empty answer."
  (let ((message (nlk:encode-json-object
                  (nlk:make-json-object "jsonrpc" "2.0" "id" (incf *request-id*)
                                         "method" method
                                         "params" (or params (nlk:make-json-object))))))
    (multiple-value-bind (text status headers) (request message :timeout timeout)
      (let ((session (header-value headers "mcp-session-id")))
        (when session (setf *session-id* session)))
      (let ((failure (status-failure status text)))
        (when failure (return-from rpc (values nil (redact failure)))))
      (let ((answer (and (stringp text) (plusp (length (nlk:trimmed text)))
                         (message-of text))))
        (unless answer (return-from rpc (values nil nil)))
        (let ((error (nlk:json-value answer :any "error")))
          (if (hash-table-p error)
              (values nil (redact (or (nlk:json-value error :text "message")
                                      "the server refused the call")))
              (values (nlk:json-value answer :any "result") nil)))))))

(defun notify (method &key timeout)
  "One JSON-RPC notification -- no id, no result -- with its failure ignored."
  (handler-case
      (let ((message (nlk:encode-json-object
                      (nlk:make-json-object "jsonrpc" "2.0" "method" method
                                             "params" (nlk:make-json-object)))))
        (multiple-value-bind (text status headers) (request message :timeout timeout)
          (declare (ignore text))
          (let ((session (header-value headers "mcp-session-id")))
            (when session (setf *session-id* session)))
          (when (status-failure status nil)
            (setf *session-id* nil))))
    (error () nil)))

(defun initialize (&key timeout)
  "Open the session MCP asks for: the initialize request, then its notification."
  (multiple-value-bind (result error)
      (rpc "initialize"
           (nlk:make-json-object
            "protocolVersion" +protocol-version+
            "capabilities" (nlk:make-json-object)
            "clientInfo" (nlk:make-json-object "name" "nodecode-flashdata" "version" "0.1.0"))
           :timeout timeout)
    (declare (ignore result))
    (when error
      (fail "the handshake was refused - ~a" error)))
  (notify "notifications/initialized" :timeout timeout)
  *session-id*)

(defun session-expired-p (error)
  "Whether ERROR is the server having forgotten the session this client names."
  (and (stringp error) (plusp (length error))
       (cl:search "session" (string-downcase error))))

(defun tools-call (name arguments &key timeout)
  "One tools/call. => the RESULT object. A session the server forgot is opened once more."
  (let ((timeout (or timeout (setting :timeout-seconds))))
    (bt2:with-lock-held (*session-lock*)
      (flet ((once ()
               (rpc "tools/call"
                    (nlk:make-json-object "name" name
                                           "arguments" (or arguments (nlk:make-json-object)))
                    :timeout timeout)))
        (unless *session-id* (initialize :timeout timeout))
        (multiple-value-bind (result error) (once)
          (when (session-expired-p error)
            (setf *session-id* nil)
            (initialize :timeout timeout)
            (multiple-value-setq (result error) (once)))
          (when error
            (fail "~a: ~a" name error))
          (or result
              (fail "~a: the server answered an empty result" name)))))))
