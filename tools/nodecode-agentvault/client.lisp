;;;; client.lisp --- the Agent Vault app's MCP server, over its stdin and stdout.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The app ships its own MCP server: a Node runtime and a server.js inside
;;;; the installed bundle, started with VAULT_MCP_TOKEN (the binding token the
;;;; app issued for this client), VAULT_MCP_HARNESS (which client binding the
;;;; token is) and, when the archive is not in its default place, VAULT_HOME.
;;;; That is what the original's launch script does, and this is the same thing
;;;; without a shell: NLK:SPAWN-PROGRAM starts the pair, and one frame per line
;;;; carries JSON-RPC down and back.
;;;;
;;;; One connection is kept and opened again when the app has restarted; a call
;;;; that finds none makes one. Every request goes through the connection's
;;;; lock, so the turn thread and an eval worker cannot interleave frames on
;;;; the read side. A read is bounded by vault.timeout_seconds without a reader
;;;; thread: LISTEN drains what the stream already holds, and when it holds
;;;; nothing the thread sleeps a hundredth and looks again. A stderr joined to
;;;; stdout is chatter, kept and quoted when the stream closes, never a frame.

(in-package #:nodecode-agentvault)

(defparameter +app-default+ "/Applications/Agent Vault.app"
  "Where the app is when vault.app_path says nothing: the default the
original's launch script carries.")

(defparameter +runtime+ "Contents/Resources/runtime/node"
  "The Node runtime inside the app bundle, relative to its top.")

(defparameter +server-js+ "Contents/Resources/app/src/mcp/server.js"
  "The app's MCP server, relative to the bundle's top.")

(defun launch-app (argv &key environment)
  "Start the app's own runtime and MCP server, once they are there to start.
The contract is NLK:SPAWN-PROGRAM's: (values PID OUTPUT ERROR PROCESS INPUT);
the server's stderr rides its stdout, so a child that fails says why."
  (unless (probe-file (first argv))
    (error 'vault-offline
           :detail (format nil "the Agent Vault app is not at ~a: install it, or set vault.app_path to where it is"
                           (app-path))))
  (unless (probe-file (second argv))
    (error 'vault-offline
           :detail (format nil "the app at ~a carries no Vault MCP server: update Agent Vault"
                           (app-path))))
  (nlk:spawn-program argv :environment environment :input :stream
                           :output :stream :error-output :output))

(defparameter *launch* #'launch-app
  "The one spawn in this cell, and the one name a test stubs: it takes ARGV and
:ENVIRONMENT and answers (values PID OUTPUT ERROR PROCESS INPUT).")

(defstruct (server (:copier nil))
  "One conversation with the app's server: its pipes, its id counter, its
catalog, and what the child wrote that was not a frame."
  (lock (bt2:make-lock :name "agentvault mcp"))
  pid
  process
  input
  output
  (next-id 0)
  (tools nil)
  (chatter nil))

(defvar *options* nil
  "The section read once at start, as a plist: :token :client :app-path
:vault-home :timeout-seconds. Nothing below SETTING reads the image's own
settings, so a verb or a tool answering on another thread finds the values it
was started with.")

(defun read-options ()
  "The section as a plist, read once: the values the cell was started on."
  (or *options*
      (handler-case (setf *options* (list :token (setting :token)
                                          :client (setting :client)
                                          :app-path (setting :app-path)
                                          :vault-home (setting :vault-home)
                                          :timeout-seconds (setting :timeout-seconds)))
        (error () nil))))

(defun option (key)
  "The running section's value for KEY, with the defaults the declaration has."
  (read-options)
  (or (getf *options* key)
      (case key
        (:client "hermes")
        (:timeout-seconds 30)
        (:app-path "")
        (t nil))))

(defvar *server* nil
  "The open connection, or NIL. Opened by the first call and closed by
STOP-CELL; the app may restart between two calls, and the next opens it again.")

;;; --- time -------------------------------------------------------------------------

(defun deadline-after (seconds)
  "The internal real time SECONDS from now."
  (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))

(defun seconds-left (deadline)
  "Seconds until DEADLINE, never below zero."
  (max 0 (float (/ (- deadline (get-internal-real-time)) internal-time-units-per-second))))

;;; --- the streams -------------------------------------------------------------------

(defun read-line-by (stream deadline)
  "One line from STREAM: the text, :EOF at end of file, or :TIMEOUT once
DEADLINE passes with nothing arriving. Only characters the stream already
holds are read, so a half-written line never blocks the wait."
  (loop with out = (make-string-output-stream)
        with count = 0
        do (cond ((listen stream)
                  (let ((char (read-char stream nil :eof)))
                    (cond ((eq char :eof)
                           (return (if (zerop count) :eof (get-output-stream-string out))))
                          ((char= char #\Newline)
                           (return (get-output-stream-string out)))
                          (t (write-char char out) (incf count)))))
                 ((<= (seconds-left deadline) 0) (return :timeout))
                 (t (sleep 0.01)))))

(defun send-message (server object)
  "Write OBJECT to SERVER as one frame."
  (write-string (nlk:encode-json-object object) (server-input server))
  (terpri (server-input server))
  (finish-output (server-input server))
  object)

(defun decode-frame (line)
  "LINE as a frame, or NIL when it is not JSON: the child's stderr, joined to
its stdout, is chatter."
  (handler-case (let ((value (nlk:decode-json line)))
                  (and (hash-table-p value) value))
    (error () nil)))

(defun receive-message (server deadline)
  "The next frame SERVER writes, before DEADLINE."
  (loop for line = (read-line-by (server-output server) deadline)
        do (cond ((eq line :timeout)
                  (error 'vault-timeout
                         :detail (format nil "Agent Vault did not answer within ~a seconds; a large archive or a first index read can pass that, so raise vault.timeout_seconds"
                                         (option :timeout-seconds))))
                 ((eq line :eof)
                  (error 'vault-offline
                         :detail (format nil "the Agent Vault server closed its stream~@[; it said: ~{~a~^ / ~}~]"
                                         (reverse (server-chatter server)))))
                 (t (let ((frame (decode-frame line)))
                      (if frame
                          (return frame)
                          (let ((trimmed (nlk:trimmed line)))
                            (when (plusp (length trimmed))
                              (push trimmed (server-chatter server))))))))))

(defun request (server method &optional params (timeout (option :timeout-seconds)))
  "METHOD with PARAMS on SERVER => its result object. An error the server
answers is a VAULT-REFUSED carrying its own words."
  (bt2:with-lock-held ((server-lock server))
    (let ((id (incf (server-next-id server))))
      (send-message server (nlk:make-json-object "jsonrpc" "2.0" "id" id "method" method
                                                 "params" (or params (nlk:make-json-object))))
      (let ((deadline (deadline-after timeout)))
        (loop for frame = (receive-message server deadline)
              when (equal id (gethash "id" frame))
                do (let ((error (gethash "error" frame)))
                     (when error
                       (error 'vault-refused
                              :detail (format nil "Agent Vault refused ~a: ~a"
                                              method
                                              (or (gethash "message" error)
                                                  (nlk:encode-json-object error)))))
                     (return (gethash "result" frame))))))))

(defun notify (server method)
  "One notification, a frame with no id: an answer the server does not send."
  (bt2:with-lock-held ((server-lock server))
    (send-message server (nlk:make-json-object "jsonrpc" "2.0" "method" method
                                               "params" (nlk:make-json-object)))
    t))

;;; --- where the app is ---------------------------------------------------------------

(defun app-path ()
  "The app's top: vault.app_path, else the usual place."
  (let ((path (nlk:trimmed (or (option :app-path) ""))))
    (if (plusp (length path)) (string-right-trim "/" path) +app-default+)))

(defun runtime-path ()
  "The app's bundled Node runtime."
  (concatenate 'string (app-path) "/" +runtime+))

(defun server-path ()
  "The app's MCP server."
  (concatenate 'string (app-path) "/" +server-js+))

(defun binding-token ()
  "The binding token, checked the way the app's launcher checks it: 64 hex
characters, this client's own and no other."
  (let ((token (nlk:trimmed (or (option :token) ""))))
    (when (zerop (length token))
      (fail "no binding token: connect this client in Agent Vault, paste the token it issues into the token member of the vault section, then (restart-cells \"nodecode-agentvault\")"))
    (unless (and (= 64 (length token))
                 (every (lambda (char) (digit-char-p char 16)) token))
      (fail "the binding token is not 64 hexadecimal characters: paste the one Agent Vault issued for this client's binding"))
    token))

(defun environment ()
  "This image's environment with the app's variables set: the token, the
client binding it belongs to, and the archive's home when it is not the
default."
  (let ((pairs (list (cons "VAULT_MCP_TOKEN" (binding-token))
                      (cons "VAULT_MCP_HARNESS" (option :client))))
        (home (nlk:trimmed (or (option :vault-home) ""))))
    (when (plusp (length home))
      (push (cons "VAULT_HOME" home) pairs))
    (with-environment pairs)))

(defun with-environment (pairs)
  "The environment block: PAIRS first, then this image's own minus the names
PAIRS names, so no variable is set twice."
  (let ((names (mapcar #'car pairs)))
    (append (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair))) pairs)
            (remove-if (lambda (entry)
                         (member (subseq entry 0 (or (position #\= entry) (length entry)))
                                 names :test #'string=))
                       (sb-ext:posix-environ)))))

;;; --- the conversation ---------------------------------------------------------------

(defun handshake (server)
  "Initialize SERVER and tell it this side is initialized."
  (request server "initialize"
           (nlk:make-json-object
            "protocolVersion" "2025-06-18"
            "capabilities" (nlk:make-json-object)
            "clientInfo" (nlk:make-json-object "name" "nodecode-agentvault" "version" "0.1.0")))
  (notify server "notifications/initialized")
  server)

(defun catalog (server)
  "The tools SERVER lists, by name."
  (let ((result (request server "tools/list" (nlk:make-json-object))))
    (loop for tool across (or (gethash "tools" result) #())
          collect (or (gethash "name" tool) ""))))

(defun connect ()
  "The open connection, opening one when there is none. => the SERVER."
  (or *server*
      (let ((argv (list (runtime-path) (server-path))))
        (multiple-value-bind (pid output error process input)
            (funcall *launch* argv :environment (environment))
          (declare (ignore error))
          (let ((server (make-server :pid pid :process process :output output :input input)))
            (setf *server* server)
            (handler-case (progn (handshake server)
                                 (setf (server-tools server) (catalog server))
                                 server)
              (vault-error (condition) (disconnect) (signal condition))
              (error (condition)
                (disconnect)
                (error 'vault-offline
                       :detail (format nil "Agent Vault's server did not start: ~a" condition)))))))))

(defun disconnect ()
  "Close the connection: stdin first, so the server leaves on its own, then a
SIGTERM for one that stays. => T."
  (let ((server *server*))
    (setf *server* nil)
    (when server
      (ignore-errors (close (server-input server)))
      (when (server-process server)
        (ignore-errors (sb-ext:process-kill (server-process server) 15)))
      (ignore-errors (close (server-output server)))
      t)))

;;; --- one call -----------------------------------------------------------------------

(defun text-of (result)
  "The text parts of a tools/call RESULT, joined."
  (with-output-to-string (out)
    (loop for part across (or (gethash "content" result) #())
          when (equal "text" (gethash "type" part))
            do (write-string (gethash "text" part) out))))

(defun server-call (name &optional arguments)
  "NAME with ARGUMENTS answered by the app's server => its text. An isError
answer is a VAULT-REFUSED carrying the server's own words."
  (let* ((server (connect))
         (result (request server "tools/call"
                          (nlk:make-json-object "name" name
                                                 "arguments" (or arguments (nlk:make-json-object))))))
    (let ((text (text-of result)))
      (if (gethash "isError" result)
          (error 'vault-refused
                 :detail (if (plusp (length (nlk:trimmed text))) text
                             (format nil "Agent Vault answered ~a with an error" name)))
          text))))

(defun arguments (&rest pairs)
  "A tools/call arguments object from PAIRS: a key whose value is NIL or an
empty string is left out, so an unfiltered search sends no filter at all."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          when (and value (or (not (stringp value)) (plusp (length (nlk:trimmed value)))))
            do (setf (gethash key object) value))
    object))
