;;;; client.lisp --- NVIDIA App's local MCP server: the address, the token, the JSON-RPC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NVIDIA App (11.0.0 and later) runs an MCP server of its own on loopback,
;;;; hosted by NvContainer, and publishes where it listens in a file it
;;;; rewrites whenever it changes:
;;;;
;;;;   %LOCALAPPDATA%\NVIDIA Corporation\NVIDIA App\McpServer\server.json
;;;;
;;;; That file carries the `http' address (a port that differs between
;;;; builds and machines, and differs again after the app restarts) and, on
;;;; the builds that use one, a `token'. This file is the only place that
;;;; reads it and the only place that talks to the server, so the app's own
;;;; restarts need nothing from the operator: every handshake resolves the
;;;; address and the token again.
;;;;
;;;; The transport is MCP's Streamable HTTP: one POST of a JSON-RPC object
;;;; to the published address, answered with JSON or with an event stream
;;;; whose last `data:' line is the answer, and a `Mcp-Session-Id' header
;;;; the server hands out at the handshake and expects back. The HTTP function
;;;; below is the one seam a test stubs, so no test opens a socket.
;;;;
;;;; A section may name `url' and `token' itself; the app's published file
;;;; wins when it can be read, because a stale copy of a port is a client
;;;; that dials nothing.

(in-package #:nodecode-nvidia-app)

;;; --- the seams -------------------------------------------------------------

(defparameter +protocol-version+ "2025-06-18"
  "The MCP protocol revision this client speaks.")

(defparameter +client-version+ "0.1.0"
  "What the handshake names this client.")

(defparameter +published-file+ "NVIDIA Corporation/NVIDIA App/McpServer/server.json"
  "Where NVIDIA App publishes its MCP server's address and token, under the
operator's local application data.")

(defparameter +download-page+ "https://www.nvidia.com/en-us/software/nvidia-app/"
  "Where NVIDIA App comes from, said in the message that finds it absent.")

(defvar *session* nil
  "The MCP session id the server answered the handshake with, or NIL.")

(defvar *handshaked* nil
  "True once INITIALIZE and its notification went out on this start.")

(defvar *request-id* 0
  "The JSON-RPC id of the last request this cell sent.")

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub this name."
  (apply #'nlk:http method url arguments))

(defun header-value (headers name)
  "The value HEADERS carries for NAME, case-insensitively: dexador's hash
table, or an alist."
  (typecase headers
    (hash-table (or (gethash (string-downcase name) headers)
                    (gethash name headers)))
    (list (cdr (assoc name headers
                      :test (lambda (wanted key) (string-equal (string wanted) (string key))))))))

(defun trim-space (text)
  (and text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun response-json (text headers)
  "The JSON-RPC object in TEXT: a plain body, or the last data: line of an
event stream, which is how a Streamable HTTP server may answer a call."
  (let ((type (header-value headers "content-type")))
    (if (and type (search "text/event-stream" type :test #'char-equal))
        (let ((answer nil))
          (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
            (let ((line (trim-space line)))
              (when (and line (> (length line) 5) (string-equal "data:" (subseq line 0 5)))
                (let ((payload (trim-space (subseq line 5))))
                  (when (and payload (plusp (length payload))) (setf answer payload))))))
          (and answer (ignore-errors (nlk:decode-json answer))))
        (ignore-errors (nlk:decode-json text)))))

(defun error-text (answer)
  "What a JSON-RPC error object says, with a session-expired one marked."
  (let ((error (and answer (nlk:json-value answer :object "error"))))
    (and error (nlk:json-value error :string "message"))))

;;; --- what NVIDIA App published ------------------------------------------------

(defun local-app-data ()
  "The directory Windows calls the operator's local application data."
  (or (let ((value (uiop:getenv "LOCALAPPDATA")))
        (and value (plusp (length value)) value))
      (namestring (merge-pathnames "AppData/Local/" (user-homedir-pathname)))))

(defun configured (name)
  "NAME's value off the settings plist, or NIL when the section does not name it.
Read here rather than through SETTING, which refuses before the start: FAIL
renders its own detail through REDACT, which reads this."
  (when (boundp '*nvidia-app*) (getf *nvidia-app* name)))

(defun path-text (value)
  "VALUE as a non-empty path string, or NIL."
  (typecase value
    (string (and (plusp (length value)) value))
    (pathname (namestring value))
    (t nil)))

(defun configured-text (name)
  "CONFIGURED's NAME as a non-empty string, or NIL."
  (path-text (configured name)))

(defun server-json-path (&optional given)
  "The file NVIDIA App publishes its MCP address and token in: GIVEN when a
caller has the section in hand (the start does, before the settings are
installed), else the section's own `server_json', else the app's standard place."
  (let ((given (or (path-text given) (configured-text :server-json))))
    (if given
        (nlk:expand-home given)
        (merge-pathnames +published-file+ (uiop:ensure-directory-pathname (local-app-data))))))

(defun published-object (&optional given)
  "The object NVIDIA App published in GIVEN's file, or NIL when it is not there or not JSON."
  (let* ((text (ignore-errors (uiop:read-file-string (server-json-path given))))
         (object (and text (ignore-errors (nlk:decode-json text)))))
    (and (hash-table-p object) object)))

(defun published-field (object names)
  "The first of NAMES OBJECT carries as a non-empty string, at its top level
or inside an object it holds: builds have nested the address both ways."
  (flet ((look (table)
           (loop for name in names
                 for value = (ignore-errors (nlk:json-value table :string name))
                 when (and value (plusp (length value))) return value)))
    (and object
         (or (look object)
             (loop for value being the hash-values of object
                   thereis (and (hash-table-p value) (look value)))))))

(defun published-url (&optional given)
  "The address NVIDIA App published, or NIL."
  (published-field (published-object given) '("url" "http" "endpoint")))

(defun published-token (&optional given)
  "The token NVIDIA App published, or NIL: some builds use none."
  (published-field (published-object given) '("token" "access_token")))

(defun override (name)
  "The section's own NAME, when it names one."
  (configured-text name))

(defun redact (text)
  "TEXT with this cell's token taken out of it: a failure the model reads
never carries the app's own secret."
  (if (and (stringp text) (plusp (length text)))
      (let ((tokens (remove-duplicates (remove nil (list (override :token) (published-token)))
                                        :test #'equal)))
        (dolist (token tokens text)
          (when (> (length token) 3)
            (setf text (uiop:frob-substrings text (list token) "[redacted]")))))
      text))

;;; --- where one exchange goes ---------------------------------------------------

(defun connection ()
  "VALUES: the address this call dials and its token, NIL when the app published none.
The published file wins over the section, so an NVIDIA App restart needs nothing."
  ;; The not-running refusal comes first, so a verb called from eval says that
  ;; rather than that no address was published.
  (running-settings)
  (values (or (published-url) (override :url))
          (or (published-token) (override :token))))

(defun not-published-text (&optional given)
  "What a call says when there is no address to dial."
  (format nil "NVIDIA App's MCP server is not published: ~a is not there, and the nvidia-app ~
section names no url. Start NVIDIA App and turn on MCP server access in its Settings, then ~
(restart-cells \"nodecode-nvidia-app\"). NVIDIA App itself: ~a"
          (server-json-path given) +download-page+))

(defun post (body &key session)
  "One POST of BODY to the published address => (values TEXT STATUS HEADERS)."
  (multiple-value-bind (url token) (connection)
    (unless url (fail "~a" (not-published-text)))
    (http :post url
          :headers (append '(("content-type" . "application/json")
                             ("accept" . "application/json, text/event-stream")
                             ("user-agent" . "nodecode-nvidia-app/0.1.0"))
                           (when token (list (cons "authorization" (format nil "Bearer ~a" token))))
                           (when session (list (cons "mcp-session-id" session))))
          :content body
          :timeout (setting :timeout-seconds))))

;;; --- the handshake and one call -------------------------------------------------

(defun initialize ()
  "The MCP handshake: INITIALIZE, then the notification the protocol requires."
  (setf *handshaked* t *session* nil)
  (multiple-value-bind (text status headers)
      (post (nlk:encode-json-object
             (nlk:json-object "jsonrpc" "2.0" "id" (incf *request-id*)
                               "method" "initialize"
                               "params" (nlk:json-object
                                         "protocolVersion" +protocol-version+
                                         "capabilities" (nlk:json-object)
                                         "clientInfo" (nlk:json-object "name" "nodecode-nvidia-app"
                                                                        "version" +client-version+)))))
    (unless (<= 200 status 299)
      (fail "NVIDIA App answered ~a to the MCP handshake: ~a"
            status (nlk:one-line (or text "") :cap 300)))
    (setf *session* (header-value headers "mcp-session-id"))
    (post (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0"
                                                   "method" "notifications/initialized"))
          :session *session*)))

(defun ensure-session ()
  (unless *handshaked* (initialize)))

(defun session-expired-p (message)
  "Whether a JSON-RPC error says the server forgot the session: NVIDIA App
restarts its server, and with it the token, on its own."
  (and (stringp message)
       (let ((message (string-downcase message)))
         (or (search "session" message) (search "expired" message)))))

(defun rpc (method &optional params)
  "One JSON-RPC call of METHOD on the MCP session => its result object.
A server that forgot the session is handed the handshake once more, which
is what an NVIDIA App restart under a call looks like."
  (ensure-session)
  (flet ((send ()
           (post (nlk:encode-json-object
                  (nlk:json-object "jsonrpc" "2.0" "id" (incf *request-id*)
                                    "method" method
                                    "params" (or params (nlk:json-object))))
                 :session *session*)))
    (multiple-value-bind (text status headers) (send)
      (let ((answer (response-json text headers)))
        (when (or (= status 404) (session-expired-p (error-text answer)))
          (setf *handshaked* nil)
          (ensure-session)
          (multiple-value-setq (text status headers) (send))
          (setf answer (response-json text headers)))
        (unless (<= 200 status 299)
          (fail "NVIDIA App answered ~a to ~a: ~a" status method
                (or (error-text answer) (nlk:one-line (or text "") :cap 300))))
        (when (error-text answer)
          (fail "NVIDIA App refused ~a: ~a" method (error-text answer)))
        (or (and answer (nlk:json-value answer :object "result"))
            (fail "NVIDIA App answered ~a nothing this client could read: ~a" method
                  (nlk:one-line (or text "") :cap 300)))))))

(defun call-text (result)
  "RESULT's text blocks, joined."
  (let ((parts (loop for block across (nlk:json-array result "content")
                     for text = (nlk:json-value block :string "text")
                     when text collect text)))
    (if parts
        (format nil "~{~a~^~%~}" parts)
        (nlk:one-line (nlk:encode-json-object result) :cap 2000))))

(defun call-tool (name &optional arguments)
  "Call NVIDIA App's tool NAME with ARGUMENTS -- a decoded object, or pairs
as NLK:JSON-OBJECT takes them -- and answer its text. A tool that reports
its own failure answers a string starting ERROR:, which carries the app's
error code: overlay_not_available, service_disabled, access_level_restricted,
overlay_timeout, invalid_params and the rest."
  (let* ((arguments (cond ((null arguments) (nlk:json-object))
                          ((hash-table-p arguments) arguments)
                          (t (apply #'nlk:make-json-object arguments))))
         (result (rpc "tools/call" (nlk:json-object "name" (string name)
                                                     "arguments" arguments)))
         (text (call-text result)))
    (if (nlk:json-value result :boolean "isError")
        (nle:failure (format nil "ERROR: ~a" (redact text)))
        (redact text))))

(defun tools ()
  "What this NVIDIA App build exposes: the server's own tool list, one line each."
  (let* ((result (rpc "tools/list"))
         (lines (loop for tool across (nlk:json-array result "tools")
                      for name = (nlk:json-value tool :string "name")
                      for about = (nlk:json-value tool :string "description")
                      when name collect (format nil "~a~@[ - ~a~]" name
                                                (and about (nlk:one-line about :cap 120))))))
    (if lines
        (format nil "~d tool~:p at ~a:~%~{~a~^~%~}" (length lines) (or (connection) "the app")
                lines)
        (format nil "NVIDIA App lists no tools for this build or access level at ~a."
                (or (connection) "its MCP server")))))
