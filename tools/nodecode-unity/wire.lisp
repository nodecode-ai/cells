;;;; wire.lisp --- the one network seam, the session, and what the editor answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through RPC-POST, so a test stubs the one
;;;; name and never needs a Unity editor. The editor's side is MCP for Unity's
;;;; streamable-HTTP server: one handshake that answers a session id, then a
;;;; JSON-RPC POST per call, answered as JSON or as an SSE stream.

(in-package #:nodecode-unity)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision the handshake offers, as the original bridge offered it.")

(defparameter +default-url+ "http://127.0.0.1:8080/mcp"
  "MCP for Unity's default HTTP endpoint, used when the section names none.")

(defvar *session* nil
  "The editor session this cell holds: a plist (:URL :ID), or NIL before the
handshake. A handshake is taken once and kept for every call after it.")

(defvar *tools* nil
  "The editor's tools from tools/list, cached; (editor-tools :force t) reads
them again. A Unity editor restarts, so a call that fails clears this too.")

(defvar *rpc-id* 0
  "The JSON-RPC id of the last request; each call takes the next one.")

;;; --- small readers ---------------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, with a note when it was."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a... (~d more characters)" (subseq text 0 limit) (- (length text) limit)))))

(defun first-line (text limit)
  "TEXT at its first line break, cut to LIMIT."
  (let* ((text (or text ""))
         (end (or (position #\Newline text) (length text))))
    (clip (string-right-trim '(#\Space #\Return) (subseq text 0 end)) limit)))

(defun json-array (object key)
  "OBJECT's KEY as a list of decoded values, or NIL."
  (let ((value (nlk:json-value object :array key)))
    (and value (coerce value 'list))))

(defun http-header (headers name)
  "HEADERS -- an alist a DIALLER answers -- as the value under NAME, or NIL."
  (loop for entry in headers
        for key = (if (consp entry) (car entry) entry)
        for value = (if (consp entry) (cdr entry) nil)
        when (and (stringp key) (string-equal (string key) name))
          return (if (listp value) (car value) value)))

;;; --- the seam ---------------------------------------------------------------------

(defun rpc-post (url body &key session-id (timeout 120))
  "POST BODY, a JSON string, to URL => (values TEXT STATUS HEADERS). The
cell's one network seam: a test stubs this name and nothing dials the editor."
  (nlk:http :post url
            :headers (append (list (cons "content-type" "application/json")
                                   (cons "accept" "application/json, text/event-stream")
                                   (cons "mcp-protocol-version" +protocol-version+))
                             (and session-id (list (cons "mcp-session-id" session-id))))
            :content body
            :timeout timeout))

(defun sse-answer (text)
  "TEXT's last `data:' payload that is a JSON object, or NIL: MCP for Unity
answers a POST as JSON, or as one event when it keeps the stream."
  (let ((found nil))
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil)
            while line
            do (when (and (>= (length line) 5) (string= "data:" line :end2 5))
                 (let ((payload (string-trim '(#\Space #\Tab #\Return)
                                             (subseq line 5))))
                   (when (and (plusp (length payload)) (char= (char payload 0) #\{))
                     (setf found payload))))))
    found))

(defun answer-message (text)
  "TEXT -- a JSON-RPC answer, or an SSE stream carrying one -- decoded, or NIL."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))
         (json (cond ((zerop (length trimmed)) nil)
                     ((find (char trimmed 0) "{[") trimmed)
                     (t (sse-answer trimmed)))))
    (when json (ignore-errors (nlk:decode-json json)))))

(defun error-text (error-object)
  "ERROR-OBJECT -- a JSON-RPC error -- as the one line the model reads."
  (or (nlk:json-value error-object :text "message")
      (nlk:json-value error-object :text "data")
      "the editor refused the request"))

(defun session-lost-p (status text)
  "Whether STATUS and TEXT say the session id this cell holds is gone."
  (and (member status '(400 404 410) :test #'=)
       (or (null text)
           (search "session" (string-downcase text)))))

;;; --- the session --------------------------------------------------------------------

(defun endpoint ()
  "The MCP for Unity endpoint this cell talks to."
  (or (nonempty (setting :url)) +default-url+))

(defun call-timeout ()
  "How long one call waits for the editor."
  (or (setting :timeout-seconds) 120))

(defun rpc-body (method params)
  "One JSON-RPC request as its text, with the next id."
  (nlk:encode-json-object
   (nlk:make-json-object "jsonrpc" "2.0" "id" (incf *rpc-id*)
                          "method" method "params" (or params (nlk:json-object)))))

(defun initialized-p ()
  "Whether this cell holds a session for the endpoint now configured."
  (and *session* (equal (getf *session* :url) (endpoint)) t))

(defun forget-session ()
  "Drop the handshake and the tool list: the next call takes a new one."
  (setf *session* nil *tools* nil))

(defun handshake ()
  "Initialize a session with the editor and keep its id. => the initialize result."
  (setf *session* nil *tools* nil)
  (let* ((body (nlk:encode-json-object
                (nlk:make-json-object
                 "jsonrpc" "2.0" "id" (incf *rpc-id*) "method" "initialize"
                 "params" (nlk:make-json-object
                           "protocolVersion" +protocol-version+
                           "capabilities" (nlk:json-object)
                           "clientInfo" (nlk:make-json-object
                                         "name" "nodecode-unity" "version" "0.1.0"))))))
    (multiple-value-bind (text status headers)
        (rpc-post (endpoint) body :timeout (call-timeout))
      (let ((message (answer-message text)))
        (unless (and (integerp status) (< status 300) message)
          (error 'unity-error
                 :detail (format nil "MCP for Unity at ~a did not answer the handshake: HTTP ~a~@[ ~a~]. Is the editor open, with Window > MCP for Unity started?"
                                 (endpoint) status (and (stringp text) (first-line text 200)))))
        (let ((failure (nlk:json-value message :object "error")))
          (when failure
            (error 'unity-error :detail (format nil "MCP for Unity at ~a refused the handshake: ~a"
                                                (endpoint) (error-text failure)))))
        (let ((id (http-header headers "mcp-session-id")))
          (setf *session* (list :url (endpoint) :id (and (stringp id) id)))
          (ignore-errors
           (rpc-post (endpoint) (nlk:encode-json-object
                                  (nlk:make-json-object "jsonrpc" "2.0"
                                                         "method" "notifications/initialized"))
                     :session-id (getf *session* :id) :timeout (call-timeout)))
          (nlk:json-value message :object "result"))))))

(defun ensure-session (&key force)
  (unless (and (initialized-p) (not force)) (handshake)))

(defun rpc (method params &key (retry t))
  "METHOD on the editor's MCP server => its result object. A session the
server forgot is taken again once; any other failure is UNITY-ERROR."
  (let ((id (getf *session* :id)))
    (multiple-value-bind (text status headers)
        (rpc-post (endpoint) (rpc-body method params) :session-id id :timeout (call-timeout))
      (declare (ignore headers))
      (let ((message (answer-message text)))
        (cond
          ((and message (nlk:json-value message :object "error"))
           (if (and retry (search "session" (string-downcase (error-text (nlk:json-value message :object "error")))))
               (progn (forget-session) (ensure-session) (rpc method params :retry nil))
               (error 'unity-error :detail (format nil "~a: ~a" method
                                                   (error-text (nlk:json-value message :object "error"))))))
          ((and (integerp status) (< status 300))
           (nlk:json-value message :object "result"))
          ((and retry (session-lost-p status text))
           (forget-session) (ensure-session) (rpc method params :retry nil))
          (t
           (if (string= method "initialize")
               (handshake)
               (error 'unity-error
                      :detail (format nil "the editor answered ~a with HTTP ~a~@[ ~a~]"
                                      method status (and (stringp text) (first-line text 200)))))))))))

;;; --- the editor's tools ---------------------------------------------------------------

(defun tool-name (tool) (or (nlk:json-value tool :string "name") "?"))

(defun tool-description (tool)
  (or (nlk:json-value tool :string "description") ""))

(defun editor-tools (&key force)
  "The tools the editor advertises, as a list of decoded objects."
  (when (or force (null *tools*))
    (ensure-session)
    (setf *tools* (json-array (rpc "tools/list" (nlk:json-object)) "tools")))
  *tools*)

(defun call-editor (name arguments)
  "NAME, an editor tool, called with ARGUMENTS -- a decoded object -- => its
text. The editor's own isError answer is this cell's refusal, in its words."
  (ensure-session)
  (let* ((result (rpc "tools/call" (nlk:make-json-object
                                     "name" name "arguments" (or arguments (nlk:json-object)))))
         (parts (json-array result "content"))
         (text (format nil "~{~a~^~%~}"
                        (remove nil (mapcar (lambda (part) (nlk:json-value part :text "text"))
                                            parts)))))
    (if (nlk:json-value result :boolean "isError")
        (error 'unity-error :detail (or (nonempty text) (format nil "~a refused the call" name)))
        (or text ""))))
