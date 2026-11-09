;;;; acp.lisp --- the grok CLI, spoken to in JSON-RPC over stdio.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Agent Client Protocol: one JSON-RPC 2.0 message a line on the
;;;; child's stdin and stdout. A round is four requests in order --
;;;; initialize, session/new, session/set_config_option (the model and the
;;;; reasoning effort, when the turn names them) and session/prompt -- with
;;;; the CLI's own traffic read in between: session/update notifications
;;;; carrying the answer's words, fs/read_text_file and fs/write_text_file
;;;; calls it makes of this image, and a permission ask, which a headless
;;;; round answers by taking the first allow the CLI offers.
;;;;
;;;; The round is synchronous -- one request outstanding at a time -- so a
;;;; session is two streams, no reader thread: what the CLI writes while a
;;;; request waits is served in the loop that waits for the reply. The
;;;; streams are the whole seam: a test builds a session over strings.

(in-package #:nodecode-grok-acp)

(define-condition acp-error (error)
  ((detail :initarg :detail :reader acp-error-detail)
   (code :initarg :code :initform nil :reader acp-error-code))
  (:report (lambda (condition stream) (write-string (acp-error-detail condition) stream)))
  (:documentation "The CLI could not start, closed its output, answered an
error, or opened no session."))

(defstruct (acp-session (:constructor make-acp-session (to-agent from-agent
                                                         &key process pid directory)))
  "One running CLI: the stream this image writes its requests down, the one
its messages come up on, and the child behind them."
  (to-agent nil) (from-agent nil) (process nil) (pid nil) (directory nil)
  (next-id 0 :type integer))

;;; --- writing and reading one line ---------------------------------------------

(defun acp-send (session object)
  "Write OBJECT to the CLI as one line."
  (let ((stream (acp-session-to-agent session)))
    (write-string (nlk:encode-json-object object) stream)
    (write-char #\Newline stream)
    (finish-output stream))
  object)

(defun acp-read (session)
  "One message the CLI wrote, decoded, or NIL at the end of its output."
  (let ((stream (acp-session-from-agent session)))
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          for text = (string-trim '(#\Space #\Tab #\Return) line)
          for object = (and (plusp (length text)) (ignore-errors (nlk:decode-json text)))
          when (hash-table-p object) return object
          finally (return nil))))

(defun acp-respond (session id result)
  "Answer the CLI's own request ID with RESULT."
  (ignore-errors (acp-send session (nlk:json-object "jsonrpc" "2.0" "id" id "result" result))))

(defun acp-respond-error (session id code text)
  "Refuse the CLI's own request ID."
  (ignore-errors
   (acp-send session (nlk:json-object "jsonrpc" "2.0" "id" id
                                       "error" (nlk:json-object "code" code "message" text)))))

;;; --- what the CLI may ask a client --------------------------------------------

(defun acp-permission-answer (params)
  "The outcome a headless round gives the CLI's permission ask: the first
option it offers that allows, else the first option at all, else cancelled."
  (let* ((offered (and (hash-table-p params) (gethash "options" params)))
         (options (cond ((vectorp offered) (coerce offered 'list))
                        ((listp offered) offered)))
         (allow (find-if (lambda (option)
                           (and (hash-table-p option)
                                (search "allow" (or (nlk:json-value option :string "kind") ""))))
                         options))
         (choice (or allow (first options))))
    (if (and (hash-table-p choice) (nlk:json-value choice :string "optionId"))
        (nlk:json-object "outcome" (nlk:json-object "outcome" "selected"
                                                     "optionId" (nlk:json-value choice :string "optionId")))
        (nlk:json-object "outcome" (nlk:json-object "outcome" "cancelled")))))

(defun acp-read-file (params)
  "The CLI's fs/read_text_file: the whole file, as RESULT carries it."
  (nlk:json-object "content" (uiop:read-file-string (nlk:json-value params :string "path"))))

(defun acp-write-file (params)
  "The CLI's fs/write_text_file: the content written, and a null result."
  (let ((path (nlk:json-value params :string "path"))
        (content (or (nlk:json-value params :string "content") "")))
    (when (uiop:pathname-directory-pathname path)
      (ensure-directories-exist path))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    :null))

(defun acp-serve (session id method params)
  "Answer the CLI's own request ID: its permission ask, a file read or write,
or the refusal ACP names for a method a client does not offer."
  (handler-case
      (cond ((equal method "session/request_permission")
             (acp-respond session id (acp-permission-answer params)))
            ((equal method "fs/read_text_file")
             (acp-respond session id (acp-read-file params)))
            ((equal method "fs/write_text_file")
             (acp-respond session id (acp-write-file params)))
            (t (acp-respond-error session id -32601
                                   (format nil "Nodecode does not offer ~a" method))))
    (error (condition)
      (acp-respond-error session id -32603 (princ-to-string condition)))))

;;; --- requests and replies ------------------------------------------------------

(defun acp-dispatch (session message on-update)
  "Serve one line the CLI wrote. => T when it answers a request of ours, which
is whichever reply comes while one waits."
  (multiple-value-bind (id id-p) (gethash "id" message)
    (multiple-value-bind (method method-p) (gethash "method" message)
      (cond ((and id-p method-p)
             (acp-serve session id method (gethash "params" message))
             nil)
            (method-p
             (when (and on-update (equal method "session/update"))
               (ignore-errors (funcall on-update (gethash "params" message))))
             nil)
            (id-p (not (eq id :null)))
            (t nil)))))

(defun acp-result (method message)
  "The result MESSAGE carries, or ACP-ERROR saying why there is none."
  (let ((error (gethash "error" message)))
    (if (hash-table-p error)
        (let* ((text (or (nlk:json-value error :string "message") "an error")))
          (error 'acp-error
                 :code (let ((code (gethash "code" error)))
                         (and (integerp code) code))
                 :detail (if (search "uthentication" text)
                             (format nil "the grok CLI is not logged in (~a): run `grok' once and sign in" text)
                             (format nil "the grok CLI refused ~a: ~a" method text))))
        (gethash "result" message))))

(defun acp-request (session method params &key on-update)
  "Send a request and answer its result, serving what the CLI asks meanwhile."
  (let ((id (incf (acp-session-next-id session))))
    (acp-send session (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params))
    (loop for message = (acp-read session)
          do (unless message
               (error 'acp-error
                      :detail (format nil "the grok CLI closed its output while ~a waited" method)))
             (when (acp-dispatch session message on-update)
               (return (acp-result method message))))))

;;; --- the child ------------------------------------------------------------------

(defun open-acp-session (argv directory)
  "Start ARGV in DIRECTORY with both pipes ours and answer the session over
them. The one place a child starts, and the seam every test stubs."
  (handler-case
      (multiple-value-bind (pid output errors process input)
          (nlk:spawn-program argv :input :stream :output :stream
                                  :error-output nil :directory directory)
        (declare (ignore errors))
        (make-acp-session input output :process process :pid pid :directory directory))
    (error (condition)
      (error 'acp-error :detail (format nil "could not start ~{~a~^ ~}: ~a" argv condition)))))

(defun close-acp-session (session)
  "Close SESSION's pipes and stop the CLI, so no child outlives the round."
  (ignore-errors (close (acp-session-to-agent session)))
  (let ((pid (acp-session-pid session))
        (process (acp-session-process session)))
    (when pid (ignore-errors (nlk:kill-tree pid)))
    (when (and pid process) (ignore-errors (nlk:wait-for-child pid process))))
  (ignore-errors (close (acp-session-from-agent session)))
  t)

;;; --- a round -------------------------------------------------------------------

(defun update-text (update)
  "The text one session/update carries, when it carries one."
  (let ((content (and (hash-table-p update) (gethash "content" update))))
    (and (hash-table-p content) (nlk:json-value content :string "text"))))

(defun dispatch-update (params on-text on-thought)
  "Route one session/update: the answer's words, or the agent's thinking."
  (let* ((update (and (hash-table-p params) (gethash "update" params)))
         (kind (and (hash-table-p update) (nlk:json-value update :string "sessionUpdate")))
         (text (update-text update)))
    (when (and (stringp text) (plusp (length text)))
      (cond ((equal kind "agent_message_chunk") (funcall on-text text))
            ((equal kind "agent_thought_chunk") (when on-thought (funcall on-thought text)))))))

(defun set-session-option (session session-id config-id value)
  "Say one session config option: the model, the reasoning effort. A CLI
that does not offer the option keeps its own default, so a refused option is
not the round's failure."
  (when (and (stringp value) (plusp (length value)))
    (ignore-errors
     (acp-request session "session/set_config_option"
                   (nlk:json-object "sessionId" session-id
                                     "configId" config-id
                                     "value" value)))))

(defun directory-name (directory)
  "DIRECTORY as the string session/new is handed, this process's own when NIL."
  (namestring (uiop:ensure-directory-pathname (or directory *default-pathname-defaults*))))

(defun run-acp-round (command argv directory prompt &key model effort on-text on-thought)
  "One round in the CLI: initialize, session/new, the model and the effort,
then one session/prompt whose streamed text comes back through ON-TEXT.
=> the prompt's stopReason."
  (let* ((session (open-acp-session (cons command (coerce argv 'list)) directory))
         (answer (or on-text (lambda (text) (declare (ignore text))))))
    (unwind-protect
         (progn
           (acp-request session "initialize"
                         (nlk:json-object
                          "protocolVersion" +protocol-version+
                          "clientCapabilities"
                          (nlk:json-object "fs" (nlk:json-object "readTextFile" t
                                                                  "writeTextFile" t))
                          "clientInfo" (nlk:json-object "name" "nodecode" "title" "Nodecode")))
           (let* ((new (acp-request session "session/new"
                                     (nlk:json-object "cwd" (directory-name directory)
                                                      "mcpServers" (vector))))
                  (session-id (nlk:json-value new :string "sessionId")))
             (unless (stringp session-id)
               (error 'acp-error :detail "the grok CLI opened no session"))
             (set-session-option session session-id "model" model)
             (set-session-option session session-id "reasoning_effort" effort)
             (let ((result (acp-request session "session/prompt"
                                         (nlk:json-object
                                          "sessionId" session-id
                                          "prompt" (vector (nlk:json-object "type" "text"
                                                                            "text" prompt)))
                                         :on-update (lambda (params)
                                                      (dispatch-update params answer on-thought)))))
               (or (nlk:json-value result :string "stopReason") "end_turn"))))
      (close-acp-session session))))
