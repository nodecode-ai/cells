;;;; mcp.lisp --- BotSee's hosted MCP server, spoken over streamable HTTP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original declared the server in mcp.json and let its harness's
;;;; native MCP client do the talking: one POST a JSON-RPC message to
;;;; https://botsee.io/mcp, the key in an Authorization bearer header, the
;;;; Mcp-Session-Id the initialize answer issues echoed on every message
;;;; after. This is that client, written small -- initialize,
;;;; notifications/initialized, tools/list paging until the server stops
;;;; offering a cursor (at most four pages), tools/call -- with the SSE body
;;;; read, because a streamable-HTTP server may answer a request either as
;;;; one JSON body or as an event stream carrying it.
;;;;
;;;; Nothing here chooses a tool: the server's own list is the catalog, as
;;;; the original refused to invent one. Nothing here spends on its own
;;;; either: tools/call is what the model asked for, and the buyer's
;;;; approval is the model's business (the primer says which tools cost).

(in-package #:nodecode-botsee)

(defparameter +client-name+ "nodecode-botsee")
(defparameter +client-version+ "0.1.0")

(defparameter +max-tool-pages+ 4
  "How many tools/list pages one refresh follows before it reports the cursor
it stopped on.")

(defparameter +detail-chars+ 600
  "How much of a failed answer's body a refusal quotes.")

;;; --- words ---------------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun clipped (text &optional (limit +detail-chars+))
  "TEXT at most LIMIT characters, with a note when it was cut."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a [... ~d more characters]" (subseq text 0 limit)
                (- (length text) limit)))))

(defun one-line (text)
  "TEXT with every run of whitespace as one space and both ends trimmed."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (with-output-to-string (out)
                 (let ((space nil))
                   (loop for character across (or text "")
                         do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                                (setf space t)
                                (progn (when space (write-char #\Space out))
                                       (setf space nil)
                                       (write-char character out))))))))

;;; --- the session the settings make -----------------------------------------

(defstruct (session (:copier nil) (:constructor %make-session))
  url key protocol-version timeout
  id
  (next-id 0 :type integer)
  (initialized nil)
  (tools nil)
  (cursor nil))

(defvar *session* nil
  "The one session this cell holds open: built on the first call, dropped on
stop, re-initialized when the server says it expired.")

(defun session ()
  "The running cell's session, made from its settings the first time."
  (or *session*
      (setf *session*
            (%make-session :url (setting :url)
                            :key (setting :api-key)
                            :protocol-version (setting :protocol-version)
                            :timeout (setting :timeout-seconds)))))

;;; --- the one seam -----------------------------------------------------------

(defun request (method url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS HEADERS)."
  (nlk:http method url :headers headers :content content :timeout (or timeout 60)))

(defun header-value (headers name)
  "NAME's value from the headers a response carried -- a hash table as the
HTTP call answers, an alist as a test writes it -- or NIL."
  (cond ((hash-table-p headers)
         (or (gethash name headers)
             (gethash (string-downcase name) headers)
             (loop for key being the hash-keys of headers using (hash-value value)
                   when (string-equal key name) return value)))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun body-text (body)
  "BODY as text: what the HTTP call answers, or the empty string."
  (cond ((stringp body) body)
        ((null body) "")
        ((vectorp body)
         (or (ignore-errors (sb-ext:octets-to-string body :external-format :utf-8)) ""))
        (t (princ-to-string body))))

;;; --- what a server answers --------------------------------------------------

(defun decode-object (text)
  "TEXT as one decoded JSON object, or NIL when it is not one."
  (let ((value (ignore-errors (nlk:decode-json (or text "")))))
    (and (hash-table-p value) value)))

(defun sse-messages (text)
  "TEXT's `data:' payloads in the order the server sent them; a body that is
not server-sent events answers none."
  (let ((events '())
        (lines '()))
    (flet ((flush ()
             (when lines
               (push (format nil "~{~a~^~%~}" (nreverse lines)) events)
               (setf lines '()))))
      (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
            for trimmed = (string-right-trim '(#\Return) line)
            do (cond ((uiop:string-prefix-p "data:" trimmed)
                      (push (string-left-trim '(#\Space #\Tab) (subseq trimmed 5)) lines))
                     ((zerop (length trimmed)) (flush))))
      (flush))
    (nreverse events)))

(defun answer-message (body id)
  "The JSON object in BODY that answers ID: the body whole when it is one
object, else the event-stream payload that answers ID, else the last payload
that is an object at all."
  (let ((whole (decode-object body)))
    (if (and whole (equal id (gethash "id" whole)))
        whole
        (loop with fallback = nil
              for payload in (sse-messages body)
              for message = (decode-object payload)
              when message
                do (if (equal id (gethash "id" message))
                       (return message)
                       (setf fallback message))
              finally (return fallback)))))

(defun problem-message (body)
  "BODY's own sentence when it is JSON, else its text clipped."
  (let ((value (ignore-errors (nlk:decode-json (or body "")))))
    (or (and (hash-table-p value)
             (or (nlk:json-value value :string "message")
                 (nlk:json-value value :string "error" "message")
                 (nlk:json-value value :string "detail")
                 (nlk:json-value value :string "error")))
        (nonblank (clipped body)))))

;;; --- the exchange -----------------------------------------------------------

(defun rpc-headers (state)
  "The headers every message carries: the type, both answer types accepted,
the section's key as a bearer token, the protocol version, and the session
the server issued once it has."
  (append (list (cons "content-type" "application/json")
                (cons "accept" "application/json, text/event-stream")
                (cons "authorization" (format nil "Bearer ~a" (session-key state)))
                (cons "mcp-protocol-version" (session-protocol-version state)))
          (let ((id (session-id state)))
            (and id (list (cons "mcp-session-id" id))))))

(defun note-session (state headers)
  "Keep the session id an answer issued."
  (let ((id (header-value headers "mcp-session-id")))
    (when (and (stringp id) (plusp (length id)))
      (setf (session-id state) id))))

(defun rpc-once (state method &optional params)
  "One JSON-RPC request. => (values RESULT EXPIRED-P); EXPIRED-P is true
when the server answered 404 to a session it once issued."
  (let* ((id (incf (session-next-id state)))
         (frame (nlk:encode-json-object
                 (nlk:json-object "jsonrpc" "2.0"
                                  "id" id
                                  "method" method
                                  :opt "params" params))))
    (multiple-value-bind (body status headers)
        (request :post (session-url state) :headers (rpc-headers state)
                 :content frame :timeout (session-timeout state))
      (note-session state headers)
      (cond
        ((and (eql status 404) (session-id state) (not (string= method "initialize")))
         (values nil t))
        ((member status '(401 403))
         (fail "BotSee's MCP server refused the key: HTTP ~d~@[ - ~a~]. Check botsee.api_key."
               status (problem-message body)))
        ((>= status 400)
         (fail "BotSee's MCP server answered HTTP ~d~@[ - ~a~]" status (problem-message body)))
        (t
         (let ((message (answer-message body id)))
           (unless message
             (fail "BotSee's MCP server answered HTTP ~d with nothing this cell could read: ~a"
                   status (clipped body)))
           (let ((error (nlk:json-value message :object "error")))
             (when error
               (fail "BotSee's MCP server refused ~a: ~a" method
                     (or (nonblank (nlk:json-value error :string "message"))
                         (clipped (nlk:encode-json-object error) 300)))))
           (values (nlk:json-value message :object "result") nil)))))))

(defun rpc (state method &optional params)
  "RPC-ONCE, re-initializing one expired session and asking once more."
  (multiple-value-bind (result expired) (rpc-once state method params)
    (if expired
        (progn
          (setf (session-id state) nil
                (session-initialized state) nil
                (session-tools state) nil
                (session-cursor state) nil)
          (unless (string= method "initialize") (ensure-session state))
          (rpc-once state method params))
        result)))

(defun notify (state method)
  "A JSON-RPC notification: no id, an answer of 202 and no body."
  (multiple-value-bind (body status headers)
      (request :post (session-url state) :headers (rpc-headers state)
               :content (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "method" method))
               :timeout (session-timeout state))
    (note-session state headers)
    (when (>= status 400)
      (fail "BotSee's MCP server refused ~a: HTTP ~d~@[ - ~a~]"
            method status (problem-message body)))))

(defun ensure-session (&aux (state (session)))
  "STATE, initialized once: the initialize exchange and the notification that
follows it, which is what makes the server answer tools/list."
  (unless (session-initialized state)
    (let ((result (rpc state "initialize"
                       (nlk:json-object
                        "protocolVersion" (session-protocol-version state)
                        "capabilities" (nlk:json-object)
                        "clientInfo" (nlk:json-object
                                       "name" +client-name+
                                       "version" +client-version+)))))
      (let ((version (nonblank (nlk:json-value result :string "protocolVersion"))))
        (when version (setf (session-protocol-version state) version)))
      (notify state "notifications/initialized")
      (setf (session-initialized state) t)))
  state)

;;; --- the catalog the server advertises ---------------------------------------

(defun tool-entry (tool)
  "One advertised tool as this cell reads it: its name, its description and
the arguments its schema names, each flagging whether it is required."
  (let* ((schema (nlk:json-value tool :object "inputSchema"))
         (properties (nlk:json-value schema :object "properties"))
         (required (loop for name across (nlk:json-array schema "required") collect name)))
    (list :name (or (nonblank (nlk:json-value tool :string "name")) "?")
          :description (or (nlk:json-value tool :string "description") "")
          :arguments (when properties
                       (loop for name being the hash-keys of properties using (hash-value spec)
                             collect (list :name name
                                           :required (and (member name required :test #'string=) t)
                                           :description (or (nlk:json-value spec :string "description") "")))))))

(defun read-tools (state)
  "Every page of the server's tool list, up to +MAX-TOOL-PAGES+. => (values
TOOLS CURSOR-LEFT-P); CURSOR-LEFT-P is the cursor the last page offered
when the ceiling was reached."
  (let ((tools '())
        (cursor nil)
        (more nil))
    (loop for page from 1
          do (let* ((params (nlk:json-object :opt "cursor" cursor))
                    (result (rpc state "tools/list" params)))
               (loop for tool across (nlk:json-array result "tools")
                     do (push (tool-entry tool) tools))
               (setf cursor (nonblank (nlk:json-value result :string "nextCursor"))
                     more cursor))
             (when (or (null cursor) (>= page +max-tool-pages+))
               (return)))
    (values (nreverse tools) more)))

(defun catalog (state &key refresh)
  "The advertised tools, read once and kept until REFRESH or a stop. =>
(values TOOLS CURSOR-LEFT-P)."
  (when (or refresh (null (session-tools state)))
    (multiple-value-bind (tools cursor) (read-tools state)
      (setf (session-tools state) tools
            (session-cursor state) cursor)))
  (values (session-tools state) (session-cursor state)))

;;; --- what a model reads ------------------------------------------------------

(defun tool-block (entry index)
  "One advertised tool as the model reads it: a numbered name and what it
does, then the arguments it takes."
  (let ((arguments (getf entry :arguments))
        (description (one-line (getf entry :description))))
    (with-output-to-string (out)
      (format out "[~d] ~a~@[ - ~a~]" index (getf entry :name)
              (and (plusp (length description)) description))
      (format out "~%    arguments: ~a"
              (if arguments
                  (format nil "~{~a~^, ~}"
                          (loop for argument in arguments
                                collect (if (getf argument :required)
                                            (format nil "~a (required)" (getf argument :name))
                                            (getf argument :name))))
                  "none")))))

(defun tool-list-text (tools cursor)
  "What the list of advertised tools answers."
  (if (null tools)
      "botsee: the server advertised no tools for this key. Check the account's enabled capabilities, then (botsee:tools :refresh t)."
      (format nil "botsee: ~d tool~:p advertised by the BotSee MCP server~@[ (more follow, cursor ~s)~]~2%~{~a~^~2%~}~2%Call one with botsee_call: {\"tool\": \"<name>\", \"arguments\": {...}}, naming only the arguments its line lists."
              (length tools) cursor
              (loop for tool in tools
                    for index from 1
                    collect (tool-block tool index)))))

(defun result-text (result)
  "RESULT's text blocks joined, or its structured content encoded, or the
empty string."
  (let ((blocks (loop for block across (nlk:json-array result "content")
                      for text = (and (equal "text" (nlk:json-value block :string "type"))
                                      (nlk:json-value block :string "text"))
                      when text collect text))
        (structured (nlk:json-value result :object "structuredContent")))
    (cond (blocks (format nil "~{~a~^~2%~}" blocks))
          (structured (nlk:encode-json-object structured))
          (t ""))))

(defun render-result (name result)
  "What one tool call answers: the server's own text, or its own words when it
called the call a failure."
  (let ((text (result-text result))
        (failed (nlk:json-value result :boolean "isError")))
    (cond (failed
           (fail "~a: ~a" name
                 (if (plusp (length text))
                     text
                     "the server called the call a failure and said nothing more")))
          ((zerop (length text))
           (format nil "~a: the server answered with no text" name))
          (t text))))

;;; --- the two calls, made plain ----------------------------------------------

(defun call-tool (name arguments)
  "One tools/call on the running session, its answer as text."
  (let* ((state (ensure-session))
         (result (rpc state "tools/call"
                      (nlk:json-object "name" name
                                       "arguments" (or arguments (nlk:json-object))))))
    (render-result name result)))

(defun arguments-object (arguments)
  "ARGUMENTS as one JSON object: a hash table as it stands, a JSON string
decoded, NIL the empty object."
  (cond ((null arguments) (nlk:json-object))
        ((hash-table-p arguments) arguments)
        ((stringp arguments)
         (let ((text (nonblank arguments)))
           (if (null text)
               (nlk:json-object)
               (let ((value (ignore-errors (nlk:decode-json text))))
                 (if (hash-table-p value)
                     value
                     (fail "arguments must be one JSON object, e.g. {\"domain\": \"example.com\"}, not ~s"
                           (clipped text 200)))))))
        (t (fail "arguments must be a JSON object, not ~s" arguments))))
