;;;; wire.lisp --- the Glorya MCP server, spoken directly.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Streamable HTTP JSON-RPC: initialize, the Mcp-Session-Id the server
;;;; hands back, tools/call, and an answer that is either a JSON object or a
;;;; one-event stream. NLK:HTTP is the one seam, and the token rides as a
;;;; bearer header on every request: never in a URL, never in a log line.

(in-package #:nodecode-glorya)

(defparameter *session-id* nil
  "The Mcp-Session-Id the server answered initialize with, or NIL.")

(defparameter *negotiated* nil
  "The protocol revision the server settled on at initialize.")

(defparameter *initialized* nil
  "Whether this image signed in to the server already.")

(defparameter *request-id* 0
  "The JSON-RPC id of the next request; one counter for the process.")

(defun next-id () (incf *request-id*))

(defun forget-session ()
  "Drop the handshake: the next call signs in again."
  (setf *session-id* nil *negotiated* nil *initialized* nil)
  nil)

(defun header-value (headers name)
  "NAME's value in the response HEADERS nlk:http answers, or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers) (gethash (string-downcase name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun token-in-file (path)
  "The token PATH holds on its first non-blank line, or NIL."
  (let ((text (and path (probe-file path) (ignore-errors (nlk:read-text path)))))
    (and text (nonblank (nlk:one-line (nlk:trimmed text))))))

(defun token ()
  "The Glorya token: the variable token_env names, else the file token_file names."
  (or (let ((variable (nonblank (setting :token-env))))
        (and variable (nonblank (nle::credential-env variable))))
      (token-in-file (nonblank (setting :token-file)))))

(defun require-token ()
  "The token every request carries, refused before anything is sent when there
is none."
  (or (token)
      (fail "no Glorya token: set the variable ~a, or point the glorya section's token_file at a file holding one. Nothing was sent."
            (or (nonblank (setting :token-env)) "GLORYA_TOKEN"))))

(defun request-headers ()
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "user-agent" +user-agent+)
                (cons "authorization" (format nil "Bearer ~a" (require-token))))
          (when *session-id* (list (cons "mcp-session-id" *session-id*)))
          (when *negotiated* (list (cons "mcp-protocol-version" *negotiated*)))))

(defun post-json (body)
  "One HTTPS POST of BODY to the server => (values TEXT STATUS HEADERS)."
  (handler-case
      (nlk:http :post (setting :endpoint)
                :headers (request-headers)
                :content body
                :timeout (setting :timeout-seconds)
                :connect-timeout (min 20 (setting :timeout-seconds)))
    (error (condition)
      (fail "Glorya's server at ~a did not answer: ~a"
            (setting :endpoint) (nlk:one-line (princ-to-string condition))))))

(defun stream-json (text)
  "The last JSON object a one-event stream body carries, or NIL."
  (let ((last nil))
    (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
          do (let ((line (string-right-trim '(#\Return) line)))
               (when (and (> (length line) 5) (string-equal "data:" (subseq line 0 5)))
                 (let ((payload (nlk:trimmed (subseq line 5))))
                   (when (and (plusp (length payload)) (char= (char payload 0) #\{))
                     (setf last payload))))))
    last))

(defun rpc-object (text)
  "The JSON-RPC object in TEXT: a bare body, or the last data line of a stream."
  (let* ((text (and (stringp text) (nlk:trimmed text)))
         (text (cond ((null text) nil)
                     ((zerop (length text)) nil)
                     ((char= (char text 0) #\{) text)
                     (t (stream-json text)))))
    (when (and text (plusp (length text)))
      (ignore-errors (nlk:decode-json text)))))

(defun rpc-error-text (object)
  "The words a JSON-RPC error object carries, or NIL."
  (let ((error (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (when error
      (or (nlk:json-value error :string "message")
          (nlk:one-line (nlk:encode-json-object error))))))

(defun fail-on-status (status object)
  (cond
    ((member status '(401 403))
     (fail "Glorya refused the token (HTTP ~a): it is missing, expired or revoked. Sign in at https://glorya.ai, copy the token into the variable ~a, then (restart-cells \"nodecode-glorya\")."
           status (or (nonblank (setting :token-env)) "GLORYA_TOKEN")))
    (t
     (fail "Glorya's MCP server answered HTTP ~a~@[: ~a~]" status (rpc-error-text object)))))

(defun send-rpc (method params)
  "One JSON-RPC request of METHOD with PARAMS => (values OBJECT STATUS)."
  (let ((id (next-id)))
    (multiple-value-bind (text status headers)
        (post-json (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                     "params" (or params (nlk:make-json-object)))))
      (let ((session (header-value headers "mcp-session-id")))
        (when session
          (setf *session-id* (if (stringp session) session (princ-to-string session)))))
      (values (rpc-object text) status))))

(defun send-note (method)
  "The one no-answer message MCP wants after initialize; a 202 is the whole answer."
  (ignore-errors
    (post-json (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0" "method" method
                                 "params" (nlk:make-json-object))))
    t))

(defun initialize ()
  (multiple-value-bind (object status)
      (send-rpc "initialize"
                (nlk:json-object "protocolVersion" +protocol-version+
                                  "capabilities" (nlk:make-json-object)
                                  "clientInfo" (nlk:json-object "name" "nodecode-glorya"
                                                                 "version" +client-version+)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (setf *negotiated*
          (or (nlk:json-value (nlk:json-value object :object "result")
                              :string "protocolVersion")
              +protocol-version+))
    (send-note "notifications/initialized")
    (setf *initialized* t)
    t))

(defun ensure-session ()
  (unless *initialized* (initialize)))

(defun mcp-call (method &optional params)
  "One MCP method, signing in first and once more after a session the server
forgot => the result object."
  (ensure-session)
  (multiple-value-bind (object status) (send-rpc method params)
    (when (equal status 404)
      (forget-session)
      (ensure-session)
      (multiple-value-bind (again again-status) (send-rpc method params)
        (setf object again status again-status)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail-on-status status object))
    (let ((reason (rpc-error-text object)))
      (when reason
        (fail "Glorya refused ~a: ~a" method reason)))
    (or (nlk:json-value object :object "result")
        (nlk:make-json-object))))

(defun content-text (result)
  "The text parts of one tools/call RESULT, joined by a newline."
  (with-output-to-string (out)
    (let ((first t))
      (loop for part across (nlk:json-array result "content")
            when (equal "text" (nlk:json-value part :string "type"))
              do (let ((text (nlk:json-value part :string "text")))
                   (when text
                     (unless first (terpri out))
                     (setf first nil)
                     (write-string text out)))))))

(defun render-result (result)
  "One tools/call RESULT as the model reads it => (values TEXT ERROR-P)."
  (let* ((structured (nlk:json-value result :object "structuredContent"))
         (errorp (and (nlk:json-value result :boolean "isError") t))
         (text (or (and structured (nlk:encode-json-object structured))
                   (content-text result))))
    (values (if (and text (plusp (length text))) text (nlk:encode-json-object result))
            errorp)))

(defun sentence-of (text)
  "TEXT as one sentence: a tool failure is sometimes a message and then a JSON
object of details, and sometimes the object alone."
  (let* ((text (or text ""))
         (cut (position #\Newline text :from-end t)))
    (flet ((object (candidate)
             (let* ((candidate (and (nonblank candidate) (nlk:trimmed candidate)))
                    (parsed (and candidate (char= (char candidate 0) #\{)
                                  (ignore-errors (nlk:decode-json candidate)))))
               (and (hash-table-p parsed) parsed))))
      (let* ((details (and cut (object (subseq text (1+ cut)))))
             (bare (and (null cut) (object text))))
        (cond (details
               (or (nlk:json-value details :string "message")
                   "Glorya returned an error."))
              (bare (or (nlk:json-value bare :string "message")
                        (nlk:json-value bare :string "error")
                        "Glorya returned an error."))
              (t (or (nonblank (nlk:one-line text)) "Glorya returned an error.")))))))

(defun glorya-tool (tool &optional arguments)
  "One Glorya MCP tool => its JSON object, or a refusal saying why."
  (multiple-value-bind (text errorp)
      (render-result (mcp-call "tools/call"
                               (nlk:json-object "name" tool
                                                 "arguments" (or arguments (nlk:make-json-object)))))
    (when errorp
      (fail "~a" (sentence-of text)))
    (let ((object (and (nonblank text) (ignore-errors (nlk:decode-json (nlk:trimmed text))))))
      (or (and (hash-table-p object) object)
          (fail "Glorya sent an answer this cell could not read: ~a"
                (nlk:clip (nlk:one-line text) 300))))))
