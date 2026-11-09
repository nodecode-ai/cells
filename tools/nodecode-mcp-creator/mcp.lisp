;;;; mcp.lisp --- one short-lived MCP client: initialize, list, disconnect.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original introspects a server with the MCP SDK, over stdio or
;;;; streamable HTTP, and lists only: tools, prompts, resources. This is the
;;;; same client, small enough to read: newline-delimited JSON-RPC over a
;;;; child's two pipes, or one POST per message to a URL, JSON or SSE back.
;;;; It never calls a tool -- introspection is read-only by construction --
;;;; and it closes what it opened, including the child.
;;;;
;;;; *CONNECT* is the seam: a function of a server plist answering a
;;;; connection whose ASK answers (values RESULT ERROR). Every test binds it,
;;;; so no test spawns anything.

(in-package #:nodecode-mcp-creator)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks; a server may answer another.")

(defparameter +frame-chars+ 4000000
  "One message's ceiling; past it the connection is dropped.")

(defstruct (conn (:constructor make-conn ()))
  kind          ; :stdio or :http, or :fake
  ask           ; (method params) -> (values result error-or-nil)
  close         ; thunk, or NIL
  server-info
  protocol
  session-id
  notes)        ; a plist of what the transport saw, for the answer

(defparameter *connect* nil
  "The seam: (lambda (server) -> CONN). Set to CONNECT-CONN at load.")

(defun ask (conn method &optional params)
  "METHOD on CONN => (values RESULT NIL) or (values NIL ERROR-STRING)."
  (funcall (conn-ask conn) method params))

(defun ask! (conn method &optional params)
  "METHOD on CONN, its result, refusing when the server answered an error."
  (multiple-value-bind (result error) (ask conn method params)
    (if error (fail "~a: ~a" method error) result)))

(defun ask-or-nil (conn method &optional params)
  "METHOD's result, or NIL when the server does not implement it: a server
with no prompts answers -32601 to prompts/list, and that is not a failure."
  (handler-case (nth-value 0 (ask conn method params)) (error () nil)))

;;; --- stdio --------------------------------------------------------------------

(defparameter +stderr-log-chars+ 4096
  "How much of a child's stderr is kept for a failure message.")

(defun launch (argv env cwd)
  "Spawn ARGV with both pipes ours; (values OUTPUT INPUT PROCESS). A child's
stderr goes to the same pipe as its stdout, where a non-JSON line is skipped."
  (multiple-value-bind (pid output error process input)
      (nlk:spawn-program argv :input :stream :error-output :output
                              :environment env
                              :directory (and cwd (plusp (length cwd)) cwd))
    (declare (ignore pid error))
    (values output input process)))

(defun write-frame (stream text)
  (write-string text stream)
  (write-char #\Newline stream)
  (finish-output stream))

(defun read-frame (stream fd timeout)
  "One line from STREAM, or :TIMEOUT, or :CLOSED at end of file."
  (let ((buffer (make-string-output-stream))
        (count 0)
        (deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop
      (let ((char (read-char-no-hang stream nil :eof)))
        (cond
          ((eq char :eof) (return :closed))
          ((null char)
           (let ((remaining (/ (- deadline (get-internal-real-time))
                               (float internal-time-units-per-second))))
             (when (or (<= remaining 0)
                       (not (sb-sys:wait-until-fd-usable fd :input remaining)))
               (return :timeout))))
          ((char= char #\Newline) (return (get-output-stream-string buffer)))
          (t (write-char char buffer)
             (when (> (incf count) +frame-chars+) (return :closed))))))))

(defun stdio-conn (server timeout)
  (let* ((command (member-text server "command"))
         (args (let ((args (field server "args"))) (if (vectorp args) (coerce args 'list) '())))
         (env (let ((env (field server "env")))
                (when (hash-table-p env)
                  (loop for key being the hash-keys of env
                        collect (format nil "~a=~a" key (member-text env key))))))
         (cwd (member-text server "cwd")))
    (when (zerop (length command))
      (fail "the server has no command to start"))
    (multiple-value-bind (output input process)
        (launch (cons command (mapcar (lambda (arg) (princ-to-string arg)) args)) env cwd)
      (let* ((fd (sb-sys:fd-stream-fd output))
             (next-id 0)
             (conn (make-conn)))
        (setf (conn-kind conn) :stdio
              (conn-ask conn)
              (lambda (method &optional params)
                (let ((id (incf next-id)))
                  (handler-case
                      (progn
                        (write-frame input
                                     (nlk:encode-json-object
                                      (object "jsonrpc" "2.0" "id" id "method" method
                                              "params" (or params (nlk:make-json-object)))))
                        (loop repeat 200
                              for frame = (read-frame output fd timeout)
                              do (cond
                                   ((eq frame :closed)
                                    (return (values nil "the server closed the connection")))
                                   ((eq frame :timeout)
                                    (return (values nil (format nil "no answer to ~a within ~ss"
                                                                method timeout))))
                                   (t
                                    (let ((message (handler-case (nlk:decode-json frame)
                                                     (error () nil))))
                                      (when (hash-table-p message)
                                        (let ((reply (field message "id"))
                                              (error (field message "error")))
                                          (cond
                                            ((and (numberp reply) (= reply id))
                                             (return (if error
                                                         (values nil (member-text error "message"))
                                                         (values (field message "result") nil))))
                                            ((field message "method") nil)))))))))
                    (error (condition)
                      (values nil (format nil "~a" condition))))))
              (conn-close conn)
              (lambda ()
                (ignore-errors (close input))
                (ignore-errors (close output))
                (ignore-errors (nlk:kill-tree process)))
              (conn-notes conn) (list :pid (ignore-errors (sb-ext:process-pid process))))
        conn))))

;;; --- streamable HTTP -----------------------------------------------------------

(defun sse-payload (body)
  "The JSON of the last `data:' line in BODY when BODY is an event stream,
else BODY itself."
  (if (and (search "data:" body) (not (char= (char (string-left-trim '(#\Space #\Newline) body) 0) #\{)))
      (or (loop for line in (uiop:split-string body :separator '(#\Newline))
                for trimmed = (string-trim '(#\Space #\Return) line)
                when (uiop:string-prefix-p "data:" trimmed)
                  do (return (string-trim '(#\Space) (subseq trimmed 5)))
                finally (return nil))
          body)
      body))

(defun http-conn (server timeout)
  (let* ((url (member-text server "url"))
         (headers (let ((headers (field server "headers")))
                    (when (hash-table-p headers)
                      (loop for key being the hash-keys of headers
                            collect (cons key (member-text headers key))))))
         (next-id 0)
         (session nil)
         (conn (make-conn)))
    (when (zerop (length url)) (fail "the server has no url to connect to"))
    (setf (conn-kind conn) :http
          (conn-ask conn)
          (lambda (method &optional params)
            (let* ((id (incf next-id))
                   (text (nlk:encode-json-object
                          (object "jsonrpc" "2.0" "id" id "method" method
                                  "params" (or params (nlk:make-json-object)))))
                   (all (append (list (cons "content-type" "application/json")
                                      (cons "accept" "application/json, text/event-stream"))
                                (when session (list (cons "mcp-session-id" session)))
                                headers)))
              (handler-case
                  (multiple-value-bind (body status reply-headers)
                      (nlk:http :post url :headers all :content text :timeout timeout)
                    (declare (ignore reply-headers))
                    (let ((new-session (cdr (assoc "mcp-session-id" reply-headers :test #'string-equal))))
                      (when new-session (setf session new-session)))
                    (if (>= status 400)
                        (values nil (format nil "HTTP ~d from ~a" status url))
                        (let ((message (handler-case (nlk:decode-json (sse-payload (or body "")))
                                         (error () nil))))
                          (if (not (hash-table-p message))
                              (values nil "the server answered no JSON-RPC message")
                              (let ((error (field message "error")))
                                (if error
                                    (values nil (member-text error "message"))
                                    (values (field message "result") nil)))))))
                (error (condition) (values nil (format nil "~a" condition))))))
          (conn-close conn) nil)
    conn))

;;; --- connect and list ------------------------------------------------------------

(defun server-timeout (server)
  (let ((timeout (field server "timeout")))
    (cond ((numberp timeout) timeout)
          ((and (stringp timeout) (plusp (length timeout))) (or (ignore-errors (parse-integer timeout)) 60))
          (t (ignore-errors (setting :timeout-seconds)) 60))))

(defun connect-conn (server)
  "Open a connection to SERVER (a plist): stdio when it names a command, HTTP
when it names a url. The caller closes it."
  (if (plusp (length (member-text server "command")))
      (stdio-conn server (server-timeout server))
      (http-conn server (server-timeout server))))

(unless *connect* (setf *connect* #'connect-conn))

(defun handshake (conn)
  "initialize, then notifications/initialized: the client's one hello."
  (let ((result (ask! conn "initialize"
                      (nlk:json-object
                       "protocolVersion" +protocol-version+
                       "capabilities" (nlk:make-json-object)
                       "clientInfo" (nlk:json-object "name" "nodecode" "version" "0.1.0")))))
    (setf (conn-server-info conn) (field result "serverInfo")
          (conn-protocol conn) (member-text result "protocolVersion"))
    ;; A notification has no id and no answer; a refusal here is not fatal.
    (ignore-errors (funcall (conn-ask conn) "notifications/initialized" nil))
    result))

(defun list-tools (conn &key (max-desc 220) (limit 200))
  "The server's tools as plists (:name :description :schema), following
nextCursor up to LIMIT entries, each description cut at MAX-DESC characters."
  (let ((tools '()))
    (loop for pages from 1 to 20
          for cursor = nil then next
          for result = (ask! conn "tools/list" (and cursor (nlk:json-object "cursor" cursor)))
          for next = (member-text result "nextCursor")
          do (loop for tool across (or (field result "tools") #())
                   for name = (member-text tool "name")
                   for description = (member-text tool "description")
                   for schema = (or (field tool "inputSchema") (field tool "input_schema"))
                   when (plusp (length name))
                     do (push (list :name name
                                    :description (nlk:one-line description :cap max-desc)
                                    :schema schema
                                    :annotations (field tool "annotations"))
                              tools)
                        (when (>= (length tools) limit) (return))
                   finally (when (>= (length tools) limit) (loop-finish)))
          while (and next (plusp (length next)) (< (length tools) limit)))
    (coerce (nreverse tools) 'vector)))

(defun list-simple (conn method key)
  "The `key' entries of METHOD's answer as a vector of plists, or #()."
  (let ((result (ask-or-nil conn method)))
    (coerce (loop for entry across (or (field result key) #())
                  collect (list :name (member-text entry "name")
                                :description (nlk:one-line (member-text entry "description") :cap 200)))
            'vector)))

(defun schema-hash (schema)
  "A tool's input schema as one stable hash: what drift compares."
  (nlk:sha256-text
   (handler-case (nlk:encode-json-object (or schema (nlk:make-json-object)))
     (error () ""))))

(defun fingerprint (tools)
  "The catalog's fingerprint: names and schema hashes, sorted, hashed."
  (nlk:sha256-text
   (format nil "~{~a~^~%~}"
           (sort (loop for tool across tools
                       collect (format nil "~a ~a" (getf tool :name) (getf tool :schema-hash)))
                 #'string<))))

(defun tool-json (tool)
  (let* ((schema (getf tool :schema))
         (properties (field schema "properties"))
         (required (let ((required (field schema "required")))
                     (if (vectorp required) (coerce required 'list) '()))))
    (object "name" (getf tool :name)
            "description" (getf tool :description)
            "required" (coerce required 'vector)
            "params" (coerce (or (members properties) '()) 'vector)
            "schema_hash" (getf tool :schema-hash))))

(defun snapshot-json (snapshot)
  (object "server" (getf snapshot :server)
          "transport" (getf snapshot :transport)
          "at" (getf snapshot :at)
          "protocol" (getf snapshot :protocol)
          "server_info" (getf snapshot :server-info)
          "instructions" (getf snapshot :instructions)
          "fingerprint" (getf snapshot :fingerprint)
          "tools" (coerce (map 'list #'tool-json (getf snapshot :tools)) 'vector)
          "prompts" (getf snapshot :prompts)
          "resources" (getf snapshot :resources)
          "notes" (getf snapshot :notes)))

(defun snapshot-server (server &key (max-desc 220))
  "Connect once to SERVER, list what it offers, and answer the snapshot plist.
Read-only: initialize, tools/list, prompts/list, resources/list, close."
  (let ((conn (funcall *connect* server)))
    (unwind-protect
         (progn
           (let ((init (handshake conn)))
             (let* ((tools (map 'vector (lambda (tool)
                                        (setf (getf tool :schema-hash) (schema-hash (getf tool :schema)))
                                        tool)
                                (list-tools conn :max-desc max-desc)))
                    (snapshot (list :server (getf server :name)
                                    :transport (string-downcase (conn-kind conn))
                                    :at (now-iso)
                                    :protocol (conn-protocol conn)
                                    :server-info (conn-server-info conn)
                                    :instructions (nlk:one-line (member-text init "instructions") :cap 400)
                                    :tools tools
                                    :prompts (list-simple conn "prompts/list" "prompts")
                                    :resources (list-simple conn "resources/list" "resources")
                                    :notes (conn-notes conn))))
               (setf (getf snapshot :fingerprint) (fingerprint tools))
               snapshot)))
      (when (conn-close conn) (ignore-errors (funcall (conn-close conn)))))))

;;; --- snapshots on disk -------------------------------------------------------------

(defun snapshot-path (server-name)
  (data-path "snapshots" (format nil "~a.json" (ensure-name server-name :what "server name"))))

(defun save-snapshot (snapshot)
  "SNAPSHOT as JSON under snapshots/<server>.json; answers the path."
  (let ((text (json-string (snapshot-json snapshot))))
    (when (> (length text) +snapshot-chars+)
      (setf text (subseq text 0 +snapshot-chars+)))
    (write-text (snapshot-path (getf snapshot :server)) text)))

(defun snapshot-from-json (table)
  "A saved snapshot TABLE as the plist the builders read."
  (list :server (member-text table "server")
        :transport (member-text table "transport")
        :at (member-text table "at")
        :protocol (member-text table "protocol")
        :server-info (field table "server_info")
        :instructions (member-text table "instructions")
        :fingerprint (member-text table "fingerprint")
        :prompts (or (field table "prompts") #())
        :resources (or (field table "resources") #())
        :tools (coerce (loop for tool across (or (field table "tools") #())
                             collect (list :name (member-text tool "name")
                                           :description (member-text tool "description")
                                           :schema (field tool "schema")
                                           :schema-hash (member-text tool "schema_hash")))
                       'vector)))

(defun saved-snapshot (server-name)
  "The last snapshot of SERVER-NAME as a hash, or NIL."
  (let ((path (snapshot-path server-name)))
    (when (file-exists-p path) (decode (read-text path)))))
