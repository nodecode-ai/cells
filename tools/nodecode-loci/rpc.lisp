;;;; rpc.lisp --- the Loci app's MCP endpoint, and the one call over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Loci app on a Mac publishes a streamable-HTTP MCP server on loopback
;;;; and a token for it. Discovery lives here too: the app writes both into one
;;;; JSON file, and the section may name another file or the token itself.
;;;;
;;;; HTTP is the only seam that leaves this image, so it is one function --
;;;; POST here -- that a test replaces whole. A reply is one JSON-RPC message,
;;;; either the whole body or the answer event of a server-sent stream.

(in-package #:nodecode-loci)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks; a server answers its own if it differs.")

(defparameter +client-version+ "0.1.0"
  "This client's version, as the handshake reports it.")

(defparameter *token* nil
  "The bearer token the running cell sends. Never printed, never in a condition.")

(defparameter *tools* nil
  "The tools the app last advertised: a plist (:name ... :description ...) each.")

;;; --- reading the section --------------------------------------------------------

(defun slot-text (value)
  "VALUE as a string: the empty string for anything that is not one."
  (if (stringp value) value ""))

(defun nonblank (value)
  "VALUE as a trimmed string, or NIL when it is empty or not a string."
  (let ((trimmed (and (stringp value)
                       (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (if (and trimmed (plusp (length trimmed))) trimmed nil)))

(defun prefix-p (prefix string)
  "Whether STRING starts with PREFIX."
  (and (stringp string) (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun one-line (text &optional (cap 300))
  "TEXT with its line breaks flattened and cut to CAP characters."
  (let* ((flat (map 'string (lambda (char)
                               (if (member char '(#\Newline #\Return #\Tab)) #\Space char))
                     (or text "")))
         (squeezed (string-trim " " flat)))
    (if (> (length squeezed) cap) (subseq squeezed 0 cap) squeezed)))

(defun discovery-path ()
  "Where the Loci app publishes its Agent Access URL and token on macOS."
  (merge-pathnames "Library/Application Support/LociRust/agent-api.json"
                   (user-homedir-pathname)))

(defun file-text (path)
  "PATH's whole text, or NIL when it cannot be read. The seam a test stubs."
  (and path (ignore-errors (uiop:read-file-string path))))

(defun json-object-p (value)
  (and (hash-table-p value) value))

(defun token-in (path)
  "The token PATH carries: a discovery file's token member, else its trimmed text."
  (let ((trimmed (nonblank (file-text path))))
    (when trimmed
      (let ((json (json-object-p (ignore-errors (nlk:decode-json trimmed)))))
        (if json
            (nonblank (nlk:json-value json :string "token"))
            trimmed)))))

(defun resolve-token ()
  "The section's own token, else the one its file (the app's, by default) holds."
  (or (getf (running-settings) :token)
      (token-in (or (getf (running-settings) :token-file) (discovery-path)))))


(defun post (url content headers timeout)
  "POST CONTENT to URL: (values BODY STATUS HEADERS). The cell's only way out."
  (nlk:http :post url :content content :headers headers :timeout timeout))

(defun endpoint ()
  "The MCP endpoint this cell talks to."
  (setting :endpoint))

(defun header (headers name)
  "NAME's value from a response's header table; both backends lowercase every name."
  (and (hash-table-p headers) (gethash name headers)))

(defun request-headers (&optional session)
  "The headers every message carries: JSON in, JSON or a stream out, the token."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream")
            ("user-agent" . "nodecode-loci/0.1.0"))
          (when session (list (cons "mcp-session-id" session)))
          (list (cons "authorization" (format nil "Bearer ~a" (or *token* ""))))))

;;; --- one message ----------------------------------------------------------------

(defun reply-in (body content-type)
  "The JSON-RPC reply BODY carries: the body itself, or the answer event of a stream."
  (cond
    ((not (stringp body)) nil)
    ((and content-type (search "text/event-stream" content-type))
     (loop for line in (uiop:split-string body :separator '(#\Newline))
           for payload = (and (prefix-p "data:" line) (subseq line 5))
           for message = (and payload
                              (json-object-p (ignore-errors (nlk:decode-json payload))))
           when (and message (nth-value 1 (gethash "id" message)))
             return message))
    (t (json-object-p (ignore-errors (nlk:decode-json body))))))

(defun http-hint (status)
  "What to do about a refused STATUS, or NIL."
  (case status
    (401 "the token is stale: turn Agent Access off and on in Loci, then (restart-cells)")
    (403 "Agent Access is not on in Loci: Settings -> Advanced -> Agent Access")
    (404 "the endpoint is not a Loci Agent Access endpoint; check `endpoint` in the loci section")))

(defun send (id method params session)
  "One JSON-RPC request on SESSION: => (values RESULT SESSION-ID)."
  (let* ((message (nlk:json-object "jsonrpc" "2.0" "method" method
                                    "params" (or params (nlk:json-object))))
         (timeout (setting :request-timeout)))
    (setf (gethash "id" message) id)
    (multiple-value-bind (body status headers)
        (handler-case
            (post (endpoint) (nlk:encode-json-object message) (request-headers session) timeout)
          (loci-error (condition) (error condition))
          (error (condition)
            (fail "cannot reach Loci at ~a (~a): open the Loci app on this Mac, turn on ~
Settings -> Advanced -> Agent Access, keep it running, then ask for loci_status again"
                  (endpoint) (one-line (princ-to-string condition)))))
      (let ((session-id (or (header headers "mcp-session-id") session)))
        (unless (and (integerp status) (<= 200 status 299))
          (fail "Loci answered HTTP ~a for ~a~@[, ~a~]~@[: ~a~]"
                status method (http-hint status) (one-line body 200)))
        (let ((reply (reply-in body (header headers "content-type"))))
          (unless reply
            (fail "Loci answered no JSON-RPC message for ~a; ~a may not be a Loci endpoint"
                  method (endpoint)))
          (let ((error (gethash "error" reply)))
            (when (and error (not (eq error :null)))
              (fail "Loci refused ~a: ~a" method
                    (or (nlk:json-value error :string "message") (one-line body 200)))))
          (values (gethash "result" reply) session-id))))))

(defun open-session ()
  "Handshake with the app: => the session id, NIL when the server is stateless."
  (multiple-value-bind (result session)
      (send 1 "initialize"
            (nlk:json-object "protocolVersion" +protocol-version+
                              "capabilities" (nlk:json-object)
                              "clientInfo" (nlk:json-object "name" "nodecode"
                                                             "version" +client-version+))
            nil)
    (declare (ignore result))
    (ignore-errors
     (post (endpoint)
           (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0"
                                                     "method" "notifications/initialized"))
           (request-headers session)
           (setting :request-timeout)))
    session))

(defun tools (&optional session)
  "The tools the app advertises now, cached as *TOOLS*: (values LIST SESSION)."
  (multiple-value-bind (result reopened)
      (send 2 "tools/list" (nlk:json-object) session)
    (setf *tools*
          (loop for tool across (nlk:json-array result "tools")
                collect (list :name (nlk:json-value tool :string "name")
                              :description (nlk:json-value tool :string "description"))))
    (values *tools* reopened)))

(defun content-text (result)
  "RESULT's text content, one block a line."
  (let ((pieces (loop for block across (nlk:json-array result "content")
                      for text = (nlk:json-value block :string "text")
                      when text collect text)))
    (format nil "~{~a~^~%~}" pieces)))

(defun failed-p (result)
  "Whether the app marked RESULT a failed call."
  (and (nlk:json-value result :boolean "isError") t))

(defun call-with (session id name arguments)
  "tools/call NAME on SESSION: => (values TEXT ERROR-P)."
  (multiple-value-bind (result session)
      (send id "tools/call"
            (nlk:json-object "name" name "arguments" (or arguments (nlk:json-object)))
            session)
    (declare (ignore session))
    (values (content-text result) (failed-p result))))

(defun call-tool (name arguments)
  "Call one tool the app advertises, NAME, with ARGUMENTS: => (values TEXT ERROR-P)."
  (call-with (open-session) 3 name arguments))

(defun status-text ()
  "One paragraph: connected where, what the app says about itself, which tools it has."
  (let* ((session (open-session))
         (found (tools session))
         (names (mapcar (lambda (tool) (getf tool :name)) found))
         (note (when (member "loci_status" names :test #'string=)
                 (multiple-value-bind (text error) (call-with session 3 "loci_status" nil)
                   (declare (ignore error))
                   (nonblank text)))))
    (format nil "Loci is connected at ~a~@[~%~a~]~%Tools this app advertises (~a): ~{~a~^, ~}.~%~
Call one with loci_call, or put a single prompt to loci_generate."
            (endpoint) (and note (one-line note 600)) (length names) names)))
