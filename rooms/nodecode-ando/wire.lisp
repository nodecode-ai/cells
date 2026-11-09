;;;; wire.lisp --- one MCP exchange with Ando, over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ando speaks MCP. A connection carries the api key the invitation
;;;; answered, the workspace and agent it belongs to, and the MCP URL that
;;;; invitation named; every exchange is one JSON-RPC message POSTed to that
;;;; URL with the key in both headers the invitation contract asks for. The
;;;; session id the initialize answered rides every later call, and is
;;;; dropped and taken again when Ando has forgotten it.

(in-package #:nodecode-ando)

(defparameter +client-name+ "Ando Nodecode"
  "What this cell names itself on MCP initialize.")

(defparameter +client-version+ "0.1.0")

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this cell asks for; Ando answers the one it speaks.")

(defparameter +rpc-timeout+ 30
  "Seconds one exchange may take, connect and read.")

(defvar *http* nil
  "NIL dials through NLK:HTTP. A test binds a function of (METHOD URL &KEY
HEADERS CONTENT) answering (values BODY STATUS HEADERS); nothing leaves the
machine then.")

(defun dial (method url &key headers content)
  "One exchange with Ando, through the seam a test binds."
  (if *http*
      (funcall *http* method url :headers headers :content content)
      (nlk:http method url :headers headers :content content
                       :timeout +rpc-timeout+ :connect-timeout (min 10 +rpc-timeout+))))

(defun header-value (headers name)
  "HEADERS' value for NAME, case aside."
  (cdr (assoc name headers :test #'string-equal)))

(defun stream-payload (text)
  "TEXT's last server-sent data line, or TEXT itself when it is plain JSON."
  (let ((found nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((line (string-trim '(#\Return #\Space #\Tab) line)))
        (when (and (> (length line) 5) (string-equal "data:" (subseq line 0 5)))
          (let ((data (string-trim '(#\Space) (subseq line 5))))
            (unless (member data '("" "[DONE]") :test #'string=)
              (setf found data))))))
    (or found text)))

(defun rpc (connection method params &key notification)
  "POST one JSON-RPC message to CONNECTION => (values RESULT STATUS).
A JSON-RPC error is this cell's own refusal, with the server's sentence."
  (let* ((id (unless notification (incf (ando-connection-next-id connection))))
         (body (if notification
                   (nlk:make-json-object "jsonrpc" "2.0" "method" method "params" params)
                   (nlk:make-json-object "jsonrpc" "2.0" "id" id
                                          "method" method "params" params)))
         (session (ando-connection-session-id connection))
         (headers (append (list (cons "content-type" "application/json")
                                (cons "accept" "application/json, text/event-stream")
                                (cons "authorization"
                                      (format nil "Bearer ~a" (ando-connection-api-key connection)))
                                (cons "x-api-key" (ando-connection-api-key connection)))
                          (when session (list (cons "mcp-session-id" session))))))
    (multiple-value-bind (answer status answer-headers)
        (dial :post (ando-connection-mcp-url connection) :headers headers
              :content (nlk:encode-json-object body))
      (let ((answered (and answer-headers (header-value answer-headers "mcp-session-id"))))
        (when answered
          (setf (ando-connection-session-id connection) answered)))
      (when notification
        (return-from rpc (values nil status)))
      (let* ((text (if (stringp answer) answer (princ-to-string answer)))
             (object (handler-case (nlk:decode-json (stream-payload text))
                       (error () nil)))
             (problem (and object (nlk:json-value object :object "error"))))
        (when problem
          (fail "Ando refused ~a: ~a" method
                (or (nlk:json-value problem :string "message") "no reason given")))
        (values (and object (nlk:json-value object :object "result")) status)))))

(defun tool-text (result)
  "RESULT's first content block's text, or NIL."
  (let* ((content (nlk:json-array result "content"))
         (block (when (plusp (length content)) (aref content 0))))
    (nlk:json-value block :string "text")))

(defun tool-result (result name)
  "RESULT's structured content, else its first text block decoded; NIL for neither."
  (when (and result (nlk:json-value result :boolean "isError"))
    (fail "Ando refused the ~a call: ~a" name (or (tool-text result) "no reason given")))
  (or (nlk:json-value result :object "structuredContent")
      (let ((text (tool-text result)))
        (when text
          (handler-case (nlk:decode-json text) (error () nil))))))

(defun initialize-connection (connection)
  "Take a session id from Ando, or none when it keeps no session."
  (multiple-value-bind (result status)
      (rpc connection "initialize"
           (nlk:make-json-object "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:make-json-object)
                                  "clientInfo" (nlk:make-json-object "name" +client-name+
                                                                      "version" +client-version+)))
    (declare (ignore result))
    (unless (and (>= status 200) (< status 300))
      (fail "Ando did not accept the MCP handshake (HTTP ~a)" status))
    (setf (ando-connection-initialized-p connection) t)
    (ignore-errors
      (rpc connection "notifications/initialized" (nlk:make-json-object) :notification t))
    connection))

(defun call-tool (connection name &optional arguments)
  "Tool NAME with ARGUMENTS, a JSON object => its result, an empty object
when it carries none. A session Ando forgot is taken again, once."
  (let ((arguments (or arguments (nlk:make-json-object))))
    (unless (ando-connection-initialized-p connection)
      (initialize-connection connection))
    (let ((params (nlk:make-json-object "name" name "arguments" arguments)))
      (multiple-value-bind (result status) (rpc connection "tools/call" params)
        (when (= status 404)
          (setf (ando-connection-session-id connection) nil
                (ando-connection-initialized-p connection) nil)
          (initialize-connection connection)
          (multiple-value-setq (result status) (rpc connection "tools/call" params)))
        (unless (and (>= status 200) (< status 300))
          (fail "Ando did not answer ~a (HTTP ~a)" name status))
        (or (tool-result result name) (nlk:make-json-object))))))
