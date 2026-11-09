;;;; wire.lisp --- MCP's streamable HTTP: one POST a message, one reply back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The endpoint is one URL. A request is POSTed to it as JSON-RPC and the
;;;; reply comes back in the same response -- as application/json, or as a
;;;; text/event-stream whose first reply-shaped event is the answer. The
;;;; handshake (initialize, then notifications/initialized) goes out once per
;;;; start, and the Mcp-Session-Id it issues, when the server issues one, is
;;;; echoed on every later request. A session the server has forgotten (a 404,
;;;; or an error that names the session) costs one more handshake, not a round.
;;;;
;;;; Nothing here reads a file, an environment variable or a key: the only way
;;;; out is CROSODO::HTTP, the one seam a test stubs.

(in-package #:nodecode-crosodo)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client names when the server answers none.")

(defparameter +client-name+ "nodecode-crosodo")
(defparameter +client-version+ "0.1.0")

(defparameter +frame-chars+ (* 1024 1024)
  "How much of one event stream a call reads before it gives up.")

(defparameter +detail-chars+ 400
  "How much of a failed answer's body a refusal quotes back.")

(defvar *session-id* nil
  "The Mcp-Session-Id this connection was issued, or NIL when it issued none.")

(defvar *initialized-p* nil
  "Whether this start has said its handshake already.")

(defvar *negotiated-version* nil
  "The protocol revision the server answered the handshake with.")

(defvar *request-id* 0
  "The last JSON-RPC id this process used; a call takes the next one.")

;;; --- the seam -------------------------------------------------------------------

(defun http (method url &rest options)
  "The one way out of this cell: NLK:HTTP, and whatever a test has put in its
place. => (values BODY STATUS HEADERS), as NLK:HTTP answers them."
  (apply #'nlk:http method url options))

(defun next-id ()
  "A JSON-RPC id no earlier call used."
  (incf *request-id*))

(defun endpoint ()
  "The endpoint this start talks to."
  (setting :base-url))

(defun deadline ()
  "How long one exchange may take, in seconds."
  (setting :timeout-seconds))

(defun reset-wire ()
  "Forget the handshake and the session: a start begins a new connection."
  (setf *session-id* nil
        *initialized-p* nil
        *negotiated-version* nil)
  nil)

;;; --- headers -------------------------------------------------------------------

(defun header-value (headers name)
  "NAME's value in HEADERS -- a dexador hash table or an alist -- or NIL."
  (when headers
    (cond ((hash-table-p headers)
           (or (gethash name headers) (gethash (string-downcase name) headers)))
          ((listp headers)
           (cdr (assoc name headers :test #'string-equal))))))

(defun note-session (headers)
  "Remember the Mcp-Session-Id HEADERS carry, when they carry one."
  (let ((session (header-value headers "mcp-session-id")))
    (when (and (stringp session) (plusp (length (nlk:trimmed session))))
      (setf *session-id* (nlk:trimmed session))))
  *session-id*)

(defun request-headers ()
  "What every POST carries: the JSON body, both answer shapes, the revision
this client speaks, and the session once there is one."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream"))
          (list (cons "mcp-protocol-version" (or *negotiated-version* +protocol-version+)))
          (when *session-id*
            (list (cons "mcp-session-id" *session-id*)))))

;;; --- one message, one reply ---------------------------------------------------

(defun message-object (text)
  "TEXT as one JSON-RPC message object, the first of a batch, or NIL."
  (let ((text (and text (nlk:trimmed text)))
        (value nil))
    (when (and text (plusp (length text)))
      (setf value (handler-case (nlk:decode-json text) (error () nil))))
    (cond ((hash-table-p value) value)
          ((vectorp value) (find-if #'hash-table-p value))
          (t nil))))

(defun reply-p (message)
  "Whether MESSAGE answers a request: it carries an id and names no method."
  (and (hash-table-p message)
       (nth-value 1 (gethash "id" message))
       (null (gethash "method" message))
       t))

(defun reply-in (text)
  "The first reply in TEXT -- one message or a batch -- or NIL."
  (let ((message (message-object text)))
    (and (reply-p message) message)))

(defun json-error (message)
  "MESSAGE's error member, or NIL."
  (let ((error (and (hash-table-p message) (gethash "error" message))))
    (and error (if (hash-table-p error) error (nlk:json-object)))))

(defun error-text (message)
  "What the endpoint said went wrong, in its own words."
  (let* ((error (gethash "error" (or message (nlk:json-object))))
         (said (and (hash-table-p error) (nlk:json-value error :string "message")))
         (code (and (hash-table-p error) (nlk:json-value error :any "code"))))
    (format nil "the endpoint refused the call: ~a~@[ (code ~a)~]"
            (or (and (stringp said) (plusp (length (nlk:trimmed said))) (nlk:trimmed said))
                (nlk:clip (nlk:encode-json-object message) +detail-chars+))
            code)))

(defun read-events-answer (stream)
  "The first reply on the SSE STREAM, or NIL when none arrived."
  (let ((found nil))
    (handler-case
        (nlk:read-events stream
                          (lambda (text)
                            (let ((message (reply-in text)))
                              (when message (setf found message) t)))
                          :limit +frame-chars+
                          :idle (max 1 (deadline)))
      (error () nil))
    found))

(defun post-message (message)
  "POST one JSON-RPC MESSAGE. => its reply, or NIL for a notification.
A refusal from the endpoint is a CROSODO-ERROR naming what it said."
  (let ((body (nlk:encode-json-object message))
        (id (gethash "id" message))
        (stream nil)
        (text nil))
    (unwind-protect
         (handler-case
             (multiple-value-bind (answer status headers)
                 (http :post (endpoint) :headers (request-headers)
                       :content body :timeout (deadline) :stream t)
               (setf stream answer)
               (note-session headers)
               (let* ((content-type (or (header-value headers "content-type") ""))
                      (events (search "event-stream" content-type :test #'char-equal))
                      (reply (if events
                                 (read-events-answer answer)
                                 (setf text (nlk:body-text answer)
                                       reply (reply-in text)))))
                 (cond ((and reply (gethash "error" reply))
                        (error 'crosodo-error :detail (error-text reply)))
                       (reply reply)
                       ((null id) nil)
                       ((>= (or status 200) 400)
                        (error 'crosodo-error
                               :detail (format nil "HTTP ~d from ~a~@[: ~a~]"
                                               status (endpoint)
                                               (and text (nlk:clip (nlk:one-line text) +detail-chars+)))))
                       (t (error 'crosodo-error
                                 :detail (format nil "~a answered no reply (HTTP ~a)" endpoint status))))))
           (crosodo-error (condition) (error condition))
           (error (condition)
             (error 'crosodo-error
                    :detail (format nil "~a: ~a" (endpoint) (nlk:one-line (princ-to-string condition))))))
      (when (streamp stream)
        (ignore-errors (close stream :abort t))))))

(defun notify (method &optional params)
  "Send one JSON-RPC notification, whose POST the endpoint answers 202 to."
  (post-message (if params
                     (nlk:json-object "jsonrpc" "2.0" "method" method "params" params)
                     (nlk:json-object "jsonrpc" "2.0" "method" method)))
  nil)

(defun summarize-version (reply)
  "Remember the revision the initialize REPLY negotiated."
  (let ((version (and reply (nlk:json-value reply :string "result" "protocolVersion"))))
    (when (and (stringp version) (plusp (length version)))
      (setf *negotiated-version* version))))

(defun ensure-session ()
  "Say the handshake once per start: initialize, then notifications/initialized."
  (unless *initialized-p*
    ;; Set first: the two POSTs below must not re-enter here.
    (setf *initialized-p* t)
    (summarize-version
     (post-message (nlk:json-object
                    "jsonrpc" "2.0" "id" (next-id) "method" "initialize"
                    "params" (nlk:json-object
                              "protocolVersion" +protocol-version+
                              "capabilities" (nlk:json-object)
                              "clientInfo" (nlk:json-object
                                            "name" +client-name+
                                            "version" +client-version+)))))
    (notify "notifications/initialized"))
  t)

(defun forgetful-session-p (condition)
  "Whether CONDITION is the endpoint having forgotten the session."
  (let ((said (string-downcase (or (crosodo-error-detail condition) ""))))
    (or (search "http 404" said)
        (search "session" said))))

(defun mcp-call (name arguments &key (retry t))
  "Call the endpoint's tool NAME with ARGUMENTS, an object. => the MCP result
object. A session the endpoint has forgotten costs one more handshake."
  (ensure-session)
  (handler-case
      (let ((reply (post-message (nlk:json-object
                                  "jsonrpc" "2.0" "id" (next-id) "method" "tools/call"
                                  "params" (nlk:json-object
                                            "name" name
                                            "arguments" (or arguments (nlk:json-object)))))))
        (or (nlk:json-value reply :object "result")
            (fail "~a answered no result" name)))
    (crosodo-error (condition)
      (if (and retry (forgetful-session-p condition))
          (progn (reset-wire) (mcp-call name arguments :retry nil))
          (error condition)))))

;;; --- the result as the model reads it ------------------------------------------

(defun result-failed-p (result)
  "Whether the endpoint marked RESULT a failure."
  (and (nlk:json-value result :boolean "isError") t))

(defun result-blocks (result)
  "The text of every content block of RESULT."
  (let ((content (nlk:json-value result :array "content")))
    (loop for block across (or content #())
          for text = (nlk:json-value block :string "text")
          when (and (stringp text) (plusp (length text)))
            collect text)))

(defun result-text (result)
  "RESULT as the model reads it: its text blocks, else its structured
content, clipped to the section's answer_chars."
  (let* ((blocks (result-blocks result))
         (structured (nlk:json-value result :any "structuredContent"))
         (text (cond ((plusp (length blocks)) (format nil "~{~a~^~%~}" blocks))
                     (structured (nlk:encode-json-object structured))
                     (t "crosodo: the endpoint answered with nothing."))))
    (nlk:clip text (setting :answer-chars) :disclose t)))
