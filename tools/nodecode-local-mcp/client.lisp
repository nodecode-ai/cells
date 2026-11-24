;;;; client.lisp --- the LMCP server as a child process, spoken to in MCP's JSON-RPC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin is a launcher: it starts the server bundled inside the
;;;; LMCP app the user installed (scripts/start-lmcp.sh in the original) with
;;;; LMCP_DISABLE_TELEMETRY=1, and leaves the tools to the harness's MCP
;;;; client. Nodecode's own MCP client reads its servers from the config; this
;;;; cell carries the client instead, so one section is the whole setup: find
;;;; the binary the same two places the original's launcher looks, start it,
;;;; shake hands, read tools/list, and answer one tools/call at a time.
;;;;
;;;; One request in flight from this side, read on the calling thread under
;;;; that call's own deadline: no reader thread to leave behind when the cell
;;;; stops. CONNECT is the one place a process starts, so a test stubs it.

(in-package #:nodecode-local-mcp)

(defparameter *protocol-version* "2025-06-18"
  "The MCP version announced; the server answers with the one it will speak.")
(defconstant +frame-limit+ (* 1024 1024)
  "Largest frame read or sent, in characters.")
(defconstant +max-pages+ 8
  "tools/list pages followed before the catalog is taken as complete.")
(defconstant +default-timeout-seconds+ 30
  "How long one exchange waits when the section names no timeout.")
(defconstant +close-grace-seconds+ 1
  "How long the server is given to end on its own after its stdin closes.")

(defparameter *app-names* '("LMCP.app" "LocalMCPTray.app")
  "The app bundle names the original's launcher looks for, in order.")

(defvar *connection* nil
  "The connection to the LMCP server this cell started, or NIL while stopped.")

(defstruct (connection (:copier nil) (:constructor make-connection))
  "One started LMCP server: its process, the two pipes, and what it listed."
  (pid nil)
  (process nil)
  (input nil)
  (output nil)
  (log-path nil)
  (partial nil)                         ; a line a deadline cut short
  (next-id 0 :type integer)
  (catalog '())                         ; ((:name :description :schema) ...)
  (server-info nil)
  (closed-p nil)
  (responder nil)                       ; a test's (lambda (method params) result)
  (label "LMCP" :type string))

;;; --- where the app is -----------------------------------------------------------

(defun app-candidates ()
  "The paths the LMCP app installs its server binary at, in the order the
original's launcher looks: the user's Applications, then the system one, for
either bundle name."
  (loop for directory in (list (merge-pathnames "Applications/" (user-homedir-pathname))
                               #p"/Applications/")
        nconc (loop for app in *app-names*
                    collect (merge-pathnames (format nil "~a/Contents/MacOS/local-mcp-server" app)
                                             directory))))

(defun existing-file (path)
  "PATH's namestring when it names a file that is there, else NIL."
  (let ((truename (and path (ignore-errors (probe-file path)))))
    (and truename (uiop:file-exists-p truename) (namestring truename))))

(defun server-path ()
  "The server binary the section names, or the first the LMCP app installed.
A named path answers whether or not it is there, so a typo is reported as
the typo it is."
  (let ((named (and (setting :server-path) (nlk:trimmed (setting :server-path)))))
    (if (and named (plusp (length named)))
        named
        (some #'existing-file (app-candidates)))))

(defun server-args ()
  "The arguments the server is started with; --auto when the section names none."
  (or (remove-if-not #'stringp (setting :server-args)) (list "--auto")))

(defun log-file ()
  "Where the server's stderr is appended, inside this home: the first
question about a server that dies at startup is what it printed."
  (nlk:home "local-mcp/server.log"))

(defun child-environment ()
  "This process's environment with LMCP_DISABLE_TELEMETRY=1, when the
section asks for it -- the one setting the original's mcp.json carries."
  (when (setting :disable-telemetry)
    (cons "LMCP_DISABLE_TELEMETRY=1"
          (remove-if (lambda (entry) (uiop:string-prefix-p "LMCP_DISABLE_TELEMETRY=" entry))
                     (coerce (sb-ext:posix-environ) 'list)))))

;;; --- the pipe -------------------------------------------------------------------

(defun now-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun deadline-after (seconds)
  "An absolute deadline SECONDS from now."
  (+ (now-seconds) seconds))

(defun timeout-seconds ()
  "The section's ceiling on one exchange, in seconds."
  (or (ignore-errors (setting :timeout-seconds)) +default-timeout-seconds+))

(defun send-message (connection message)
  "Write MESSAGE as one JSON line on the server's stdin."
  (let* ((text (nlk:encode-json-object message))
         (stream (connection-input connection)))
    (when (> (length text) +frame-limit+)
      (fail "the frame for ~a is over the 1 MiB limit" (gethash "method" message)))
    (unless (streamp stream)
      (fail "the LMCP server is not connected"))
    (handler-case
        (progn (write-string text stream)
               (write-char #\Newline stream)
               (finish-output stream))
      (error (condition)
        (fail "writing to the LMCP server failed (~a)" condition)))))

(defun read-line-before (connection deadline)
  "One line from the server's stdout, :TIMEOUT past DEADLINE, or :CLOSED at
end of file. A line a deadline cut short is kept on the connection and
resumed by the next read."
  (let ((stream (connection-output connection))
        (buffer (make-string-output-stream))
        (count 0))
    (when (connection-partial connection)
      (write-string (connection-partial connection) buffer)
      (setf count (length (connection-partial connection))
            (connection-partial connection) nil))
    (unless (streamp stream)
      (return-from read-line-before :closed))
    (loop
      (let ((character (handler-case (read-char-no-hang stream nil :eof)
                         (error () :eof))))
        (cond
          ((eq character :eof) (return :closed))
          ((null character)
           (let ((remaining (- deadline (now-seconds))))
             (when (<= remaining 0)
               (setf (connection-partial connection) (get-output-stream-string buffer))
               (return :timeout))
             (ignore-errors
               (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream) :input
                                             (min remaining 0.25)))))
          ((char= character #\Newline) (return (get-output-stream-string buffer)))
          (t
           (write-char character buffer)
           (when (> (incf count) +frame-limit+)
             (setf (connection-closed-p connection) t)
             (fail "the LMCP server wrote over 1 MiB with no line end; the connection is dropped"))))))))

(defun read-message (connection deadline)
  "The next JSON object from the server, :TIMEOUT, or :CLOSED. A line that is
not a JSON object is skipped, as a server's stray banner is."
  (loop
    (let ((line (read-line-before connection deadline)))
      (cond
        ((keywordp line) (return line))
        ((zerop (length (string-trim '(#\Space #\Tab #\Return) line))) nil)
        (t (let ((message (handler-case (nlk:decode-json line) (error () nil))))
             (when (hash-table-p message) (return message))))))))

(defun child-exited (connection)
  "How the server ended, once it has, else NIL."
  (and (connection-pid connection)
       (ignore-errors (nlk:child-exit (connection-pid connection) (connection-process connection)))))

(defun diagnosis (connection)
  "One clause saying why the server is not answering."
  (let ((exit (child-exited connection)))
    (if exit
        (format nil "the server exited (~a); its stderr is in ~a" exit (namestring (log-file)))
        "the server is still running")))

(defun exchange (connection method &optional params)
  "METHOD's result object, from the server, or from a test's responder."
  (let ((responder (connection-responder connection)))
    (if responder
        (funcall responder method params)
        (request connection method params))))

(defun request (connection method &optional params)
  "One JSON-RPC request and the result it answers, or a refusal naming METHOD."
  (let ((id (incf (connection-next-id connection)))
        (deadline (deadline-after (timeout-seconds))))
    (when (connection-closed-p connection)
      (fail "the LMCP server is not connected"))
    (send-message connection
                  (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                   "params" (or params (nlk:json-object))))
    (loop
      (let ((message (read-message connection deadline)))
        (cond
          ((eq message :timeout)
           (fail "LMCP did not answer ~a within ~a seconds; the app may be waiting on a window"
                 method (timeout-seconds)))
          ((eq message :closed)
           (setf (connection-closed-p connection) t)
           (fail "LMCP stopped while ~a waited: ~a" method (diagnosis connection)))
          (t
           (let ((answer (gethash "id" message)))
             ;; No id is a notification, and an earlier id is a late answer to
             ;; a request that already ended: both are skipped.
             (when (and (numberp answer) (= answer id))
               (let ((condition (gethash "error" message)))
                 (if (hash-table-p condition)
                     (fail "LMCP refused ~a: ~a (code ~a)" method
                           (or (nlk:json-value condition :string "message") "no message")
                           (or (nlk:json-value condition :any "code") "none"))
                     (return (gethash "result" message))))))))))))

(defun notify (connection method &optional params)
  "One JSON-RPC notification: no id, so the server answers nothing. A test's
responder stands in for the pipe, and takes no notification."
  (unless (connection-responder connection)
    (send-message connection (nlk:json-object "jsonrpc" "2.0" "method" method
                                               "params" (or params (nlk:json-object))))))

;;; --- MCP ----------------------------------------------------------------------------

(defun tool-entry (tool)
  "One tools/list row as (:name :description :schema)."
  (list :name (or (nlk:json-value tool :string "name") "")
        :description (or (nlk:json-value tool :string "description") "")
        :schema (gethash "inputSchema" tool)))

(defun list-tools (connection)
  "Every tool the server lists, following its cursors."
  (let ((tools '()) (cursor nil) (pages 0))
    (loop
      (let* ((params (if cursor (nlk:json-object "cursor" cursor) (nlk:json-object)))
             (result (exchange connection "tools/list" params))
             (page (nlk:json-array result "tools")))
        (loop for tool across page do (push (tool-entry tool) tools))
        (incf pages)
        (setf cursor (and (hash-table-p result) (nlk:json-value result :string "nextCursor")))
        (when (or (null cursor) (>= pages +max-pages+)) (return))))
    (nreverse tools)))

(defun initialize (connection)
  "The handshake, then the notification that says this client is ready."
  (let ((result (exchange connection "initialize"
                           (nlk:json-object
                            "protocolVersion" *protocol-version*
                            "capabilities" (nlk:json-object)
                            "clientInfo" (nlk:json-object "name" "nodecode" "version" "0.1.0")))))
    (setf (connection-server-info connection) result)
    (notify connection "notifications/initialized")
    result))

(defun connect (argv &key environment error-log label)
  "Start ARGV, shake hands and answer the connection with its catalog. The
one place a process starts, so a test stubs this name."
  (ensure-directories-exist (log-file))
  (multiple-value-bind (pid output error process input)
      (nlk:spawn-program argv :input :stream :output :stream
                              :error-output (or error-log (log-file))
                              :environment environment)
    (declare (ignore error))
    (let ((connection (make-connection :pid pid :process process :input input :output output
                                       :log-path (or error-log (log-file))
                                       :label (or label (first argv) "LMCP"))))
      (handler-case
          (progn (initialize connection)
                 (setf (connection-catalog connection) (list-tools connection))
                 connection)
        (error (condition)
          (close-connection connection)
          (error condition))))))

(defun close-connection (connection)
  "Let the server end on its own after its stdin closes, then end it."
  (when (and connection (not (connection-closed-p connection)))
    (setf (connection-closed-p connection) t)
    (let ((pid (connection-pid connection)))
      (ignore-errors (close (connection-input connection)))
      (when (and pid (null (connection-responder connection)))
        (loop repeat (round (* 10 +close-grace-seconds+))
              until (child-exited connection)
              do (sleep 0.1))
        (unless (child-exited connection)
          (ignore-errors (nlk:kill-tree pid))))
      (ignore-errors (close (connection-output connection)))
      (setf (connection-catalog connection) nil)))
  t)

(defun result-text (result)
  "RESULT -- a tools/call result -- as the text the model reads."
  (let ((content (nlk:json-array result "content")))
    (if (plusp (length content))
        (format nil "~{~a~^~%~}"
                (loop for part across content
                      collect (or (nlk:json-value part :string "text")
                                  (format nil "[~a] ~a"
                                          (or (nlk:json-value part :string "type") "content")
                                          (or (ignore-errors (nlk:encode-json-object part)) "")))))
        (or (ignore-errors (nlk:encode-json-object result)) "LMCP answered nothing"))))

(defun call-tool (connection name arguments)
  "Call NAME with ARGUMENTS (a hash, or NIL) => (values TEXT FAILED-P)."
  (let* ((result (exchange connection "tools/call"
                            (nlk:json-object "name" name
                                             "arguments" (or arguments (nlk:json-object)))))
         (failed (and (hash-table-p result) (nlk:json-value result :boolean "isError"))))
    (values (result-text result) (and failed t))))

(defun connection ()
  "The running cell's connection, or the refusal RUNNING-SETTINGS says."
  (running-settings)
  (or *connection*
      (fail "LMCP is not connected: (restart-cells \"nodecode-local-mcp\") starts the app's server again")))

(defun offered-tools (connection)
  "CONNECTION's catalog cut down by the section's include and exclude lists."
  (let ((include (remove-if-not #'stringp (setting :include)))
        (exclude (remove-if-not #'stringp (setting :exclude))))
    (loop for tool in (connection-catalog connection)
          for name = (getf tool :name)
          when (and (or (null include) (member name include :test #'string-equal))
                    (not (member name exclude :test #'string-equal)))
            collect tool)))

(defun find-offered-tool (connection name)
  (find name (offered-tools connection) :key (lambda (tool) (getf tool :name)) :test #'string-equal))
