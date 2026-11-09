;;;; transport.lisp --- one MCP server over stdio, newline-delimited JSON-RPC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's mcp.json was a command, not code: `npx -y rea-agents@<pin>
;;;; mcp', two forwarded environment variables and a stdio transport. That is
;;;; what this file is, spoken by the cell itself so the tools need no
;;;; hand-edited `mcp.servers' entry: one child process, one JSON-RPC object
;;;; per line, an id matched on the way back.
;;;;
;;;; Everything the tests need to fake sits behind TRANSPORT: a class of four
;;;; generic functions. The real one is a process; a test's is a hash table of
;;;; canned answers. Nothing above this file knows which it has.
;;;;
;;;; One reader, on the calling thread, under the call's deadline, the way the
;;;; shipped mcp cell reads: a call is cancelled by its deadline, and a
;;;; notification that arrives mid-call (a progress line, a log) is skipped
;;;; rather than parsed as an answer.

(in-package #:nodecode-rea)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision the handshake asks for. A server answers the one it speaks,
and the difference is not this cell's to police: REA answers 2024-11-05.")

(defparameter +open-timeout+ 120
  "Seconds the handshake may take. npx unpacks the pinned package on the
first start of a machine, which is slow but not a Ghidra import.")

(defclass transport ()
  ()
  (:documentation "A connection to one MCP server. REQUEST is one call;
NOTIFY is one call that wants no answer; CLOSE ends it; DESCRIBE names it."))

(defgeneric transport-request (transport method params &key timeout)
  (:documentation "Send one JSON-RPC request and answer its result, decoded:
an equal hash table. Signals the server's own error as REA-ERROR."))

(defgeneric transport-notify (transport method params)
  (:documentation "Send one JSON-RPC notification: no id, no answer."))

(defgeneric transport-close (transport)
  (:documentation "End the connection. Idempotent."))

(defgeneric transport-describe (transport)
  (:documentation "The command line, for the operator: never a secret."))

(defvar *transport-factory* nil
  "A function of (COMMAND ENV) answering a TRANSPORT, or NIL for the stdio one.
The test suite binds a stub here: the run has no network and no npx.")

;;; --- the stdio transport -----------------------------------------------------

(defclass stdio-transport (transport)
  ((command :initarg :command :reader stdio-command)
   (process :initarg :process :reader stdio-process)
   (input :initarg :input :reader stdio-input)
   (output :initarg :output :reader stdio-output)
   (next-id :initform 0 :accessor stdio-next-id)
   (error :initform nil :accessor stdio-error)))

(defmethod print-object ((transport stdio-transport) stream)
  (print-unreadable-object (transport stream :type t)
    (format stream "~a" (stdio-command transport))))

(defmethod transport-describe ((transport stdio-transport))
  (format nil "~{~a~^ ~}" (stdio-command transport)))

(defun message-line (object)
  "OBJECT as one JSON-RPC line, without its newline."
  (shasht:write-json object nil))

(defun send-line (transport object)
  (let ((stream (stdio-input transport)))
    (unless (open-stream-p stream)
      (fail "the REA server's input is closed~@[: ~a~]" (stdio-error transport)))
    (write-string (message-line object) stream)
    (terpri stream)
    (finish-output stream)))

(defun parse-line (line)
  "LINE decoded, or NIL for a line that is not JSON (the server's own stderr
rides the same pipe, and npx prints there)."
  (handler-case (let ((value (shasht:read-json line)))
                  (if (hash-table-p value) value nil))
    (error () nil)))

(defun request-id (object)
  "The id OBJECT carries, or NIL for a notification. NIL is also id 0's
absence, so ids start at 1."
  (let ((id (and (hash-table-p object) (gethash "id" object))))
    id))

(defmethod transport-request ((transport stdio-transport) method params &key timeout)
  (let* ((id (incf (stdio-next-id transport)))
         (message (if params
                      (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)
                      (nlk:json-object "jsonrpc" "2.0" "id" id "method" method)))
         (stream (stdio-output transport))
         (deadline (or timeout +open-timeout+)))
    (send-line transport message)
    (loop
      (let ((line (handler-case
                      (sb-sys:with-deadline (:seconds deadline)
                        (read-line stream nil :eof))
                    (sb-sys:deadline-timeout ()
                      (fail "REA did not answer ~a within ~as: a first Ghidra import can need longer, so raise call_timeout_seconds (or ghidra_startup_timeout_ms for the import itself)"
                            method deadline))
                    (error (condition)
                      (setf (stdio-error transport) (princ-to-string condition))
                      (fail "reading REA's answer to ~a failed: ~a" method condition)))))
        (cond
          ((eq line :eof)
           (fail "the REA server closed its stream while answering ~a~@[: ~a~]"
                 method (stdio-error transport)))
          ((string= line "") nil)
          (t
           (let ((object (parse-line line)))
             (cond
               ((null object) nil)
               ((equal (request-id object) id)
                (let ((error (gethash "error" object)))
                  (when error
                    (fail "REA refused ~a: ~a"
                          method
                          (or (and (hash-table-p error) (gethash "message" error))
                              error))))
                (return (gethash "result" object)))
               (t nil)))))))))

(defmethod transport-notify ((transport stdio-transport) method params)
  (send-line transport
              (if params
                  (nlk:json-object "jsonrpc" "2.0" "method" method "params" params)
                  (nlk:json-object "jsonrpc" "2.0" "method" method)))
  nil)

(defmethod transport-close ((transport stdio-transport))
  ;; Closing the write side is the polite stop: the server sees EOF and exits.
  ;; A server that does not is terminated, and reaped either way.
  (ignore-errors (close (stdio-input transport)))
  (let ((process (stdio-process transport)))
    (when process
      (when (uiop:process-alive-p process)
        (ignore-errors (uiop:terminate-process process :urgent t)))
      (ignore-errors (uiop:wait-process process))))
  (ignore-errors (close (stdio-output transport)))
  nil)

(defun merged-environment (extra)
  "The process's own environment with EXTRA, an alist, laid over it. A server
inherits PATH, HOME and the rest, so the engines it finds on PATH are found."
  #+sbcl
  (let ((pairs (loop for (name . value) in extra
                      when (and value (not (string= value "")))
                        collect (format nil "~a=~a" name value))))
    (append pairs
            (remove-if (lambda (entry)
                         (some (lambda (pair) (uiop:string-prefix-p (car pair) entry)) pairs))
                       (sb-ext:posix-environ))))
  #-sbcl
  (loop for (name . value) in extra
        when (and value (not (string= value "")))
          collect (format nil "~a=~a" name value)))

(defun open-stdio (command env)
  "Launch COMMAND (a list, its argv) and answer the transport on its stdio."
  (let (process)
    (handler-case
        (setf process (uiop:launch-program command
                                            :input :stream
                                            :output :stream
                                            :error-output :output
                                            :element-type 'character
                                            :environment (merged-environment env)))
      (error (condition)
        (fail "cannot start the REA server (~{~a~^ ~}): ~a. It needs Node.js 22.19+, 24.11+ or 26+ with npx on PATH; name another with the npx setting"
              command condition)))
    (make-instance 'stdio-transport
                   :command command
                   :process process
                   :input (uiop:process-info-input process)
                   :output (uiop:process-info-output process))))

(defun open-transport (command env)
  "A transport for COMMAND and ENV: the stub when the suite bound one."
  (if *transport-factory*
      (funcall *transport-factory* command env)
      (open-stdio command env)))

;;; --- the handshake -----------------------------------------------------------

(defun handshake (transport)
  "Introduce this client and say the opening notification has been sent."
  (transport-request transport "initialize"
                      (nlk:json-object "protocolVersion" +protocol-version+
                                        "capabilities" (nlk:json-object)
                                        "clientInfo" (nlk:json-object "name" "nodecode-rea"
                                                                       "version" "0.1.0"))
                      :timeout +open-timeout+)
  (transport-notify transport "notifications/initialized" nil)
  transport)
