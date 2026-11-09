;;;; acp.lisp --- a small, fail-closed ACP (Agent Client Protocol) client for
;;;; one `kimi acp' child.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One call, one process, one session, then the process goes: nothing here
;;;; holds state between calls. The child is started with this process's
;;;; environment narrowed to the names the CLI needs, so the CLI's own login
;;;; under HOME is the credential and nothing else is handed over.
;;;;
;;;; The client is read-only by construction, not by asking nicely: a
;;;; filesystem or terminal request is answered with a JSON-RPC error, and a
;;;; permission request with `reject_once', before the agent can act. The
;;;; instruction the prompt carries says the same in words; the denials hold.
;;;;
;;;; *OPEN-AGENT* is the seam: a function of the settings struct answering an
;;;; AGENT. Production opens a child; a test substitutes a scripted peer, so
;;;; the dialogue and the replies are asserted offline.

(in-package #:nodecode-kimi-code-delegate)

;;; --- the peer, as verbs ------------------------------------------------

(defstruct (agent (:constructor make-agent (&key send recv close stderr)))
  "One ACP peer: SEND takes a message object, RECV takes a timeout in seconds
and answers a message, :TIMEOUT or :EOF, CLOSE ends it."
  (send nil :read-only t)
  (recv nil :read-only t)
  (close nil :read-only t)
  (stderr nil :read-only t))

(defvar *open-agent* nil
  "NIL opens a child of the CLI the section names; a function of the settings
struct answers an AGENT instead. A test binds it to a scripted peer.")

(defstruct (session (:constructor make-session (settings agent directory)))
  "One ACP conversation: the peer, the folder it was opened in, the ids this
side has spent, and the text the agent has said so far."
  settings agent directory (next-id 0) (chunks '()) options)

;;; --- time ---------------------------------------------------------------

(defun seconds->ticks (seconds)
  (round (* (max 0 seconds) internal-time-units-per-second)))

(defun ticks-left (deadline)
  "Seconds from now until DEADLINE, never negative."
  (max 0.0 (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))

;;; --- what the child inherits ---------------------------------------------

(defun entry-name (entry)
  "The NAME of one \"NAME=value\" environment ENTRY."
  (let ((equals (position #\= entry)))
    (if equals (subseq entry 0 equals) entry)))

(defun child-environment (&optional (source (sb-ext:posix-environ)))
  "SOURCE — \"NAME=value\" strings, this process's own when NIL — narrowed to
the names a Kimi CLI needs, HOME always among them."
  ;; What this keeps out is the point: every other key and token of this
  ;; process stays here, so the child can only use its own login.
  (let ((kept '())
        (home nil))
    (dolist (entry source)
      (let ((name (entry-name entry)))
        (cond ((string= name "HOME") (setf home entry))
              ((member name +inherited-environment+ :test #'string=) (push entry kept)))))
    (unless home
      (let ((value (uiop:getenv "HOME")))
        (when (and value (plusp (length value)))
          (setf home (format nil "HOME=~a" value)))))
    (if home (cons home kept) kept)))

;;; --- diagnostics ---------------------------------------------------------

(defun secret-ish-p (token)
  "Whether TOKEN reads as a credential rather than as prose."
  (let ((lower (string-downcase token)))
    (or (some (lambda (needle) (search needle lower))
              '("ghp_" "gho_" "github_pat_" "sk-" "bearer" "access_token" "refresh_token"))
        (and (or (position #\= token) (position #\: token))
             (some (lambda (needle) (search needle lower))
                   '("token" "secret" "password" "api_key" "apikey"))))))

(defun redacted-token (token)
  "TOKEN with its value, when it carries one, taken out."
  (let* ((equals (position #\= token))
         (colon (position #\: token))
         (at (cond ((and equals colon) (min equals colon)) (equals) (colon))))
    (cond ((not (secret-ish-p token)) token)
          ((and at (< (1+ at) (length token)))
           (concatenate 'string (subseq token 0 (1+ at)) "[REDACTED]"))
          (t "[REDACTED]"))))

(defun redact (text)
  "TEXT with credential-shaped tokens replaced, as child output is shown."
  (if (or (null text) (zerop (length text)))
      ""
      (with-output-to-string (out)
        (let ((start 0)
              (length (length text)))
          (loop for i from 0 to length
                do (when (or (= i length)
                              (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
                     (when (> i start)
                       (write-string (redacted-token (subseq text start i)) out))
                     (when (< i length) (write-char (char text i) out))
                     (setf start (1+ i))))))))

(defun bounded (text &optional (limit *max-diagnostic-chars*))
  "TEXT, at most LIMIT characters, with the cut said out loud."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (let ((half (floor limit 2)))
          (format nil "~a~%...[truncated]...~%~a" (subseq text 0 half)
                  (subseq text (- (length text) half)))))))

(defun stderr-text (agent)
  "What the child has said on stderr, redacted and bounded; \"\" for none."
  (let ((stream (agent-stderr agent)))
    (if (and stream (streamp stream) (listen stream))
        (let ((lines '()))
          (loop for line = (read-line stream nil :eof)
                while (and line (not (eq line :eof)))
                do (push line lines)
                while (< (length lines) 40))
          (bounded (redact (format nil "~{~a~^~%~}" (nreverse lines)))))
        "")))

;;; --- naming the CLI ------------------------------------------------------

(defun separator-char-p (char)
  (or (char= char #\/) (char= char #\\)))

(defun executable-file-p (path)
  "Whether PATH names a file this process may execute."
  (let ((file (and path (probe-file path))))
    (and file
         (not (uiop:directory-pathname-p file))
         #+win32 t
         #-win32 (handler-case
                     (progn (sb-posix:access (uiop:native-namestring file) sb-posix:x-ok) t)
                   (error () nil)))))

(defun default-install ()
  "The CLI's own install location under HOME, when it is there."
  (let ((home (uiop:getenv "HOME")))
    (when (and home (plusp (length home)))
      (let ((candidate (merge-pathnames ".kimi-code/bin/kimi"
                                        (uiop:ensure-directory-pathname home))))
        (and (executable-file-p candidate) (uiop:native-namestring candidate))))))

(defun search-path (name)
  "NAME, looked for in every PATH directory; its full name or NIL."
  (let ((path (uiop:getenv "PATH")))
    (loop for directory in (uiop:split-string (or path "")
                                              :separator (list #+win32 #\; #-win32 #\:))
          for candidate = (and (plusp (length directory))
                               (merge-pathnames name
                                                (uiop:ensure-directory-pathname directory)))
          when (executable-file-p candidate)
            do (return (uiop:native-namestring (probe-file candidate))))))

(defun resolve-command (command)
  "The Kimi Code executable COMMAND names: a path, or a name searched for on
PATH, with the CLI's own install location tried for the default."
  (let ((name (nlk:trimmed (or command ""))))
    (when (zerop (length name)) (setf name +default-command+))
    (if (some #'separator-char-p name)
        (let ((file (probe-file name)))
          (unless (executable-file-p file)
            (fail "the Kimi Code executable ~a~:[ does not exist~; is not executable~]; ~
set kimi-code-delegate.command in /settings" name (and file t)))
          (uiop:native-namestring file))
        (or (and (string= name +default-command+) (default-install))
            (search-path name)
            (fail "the Kimi Code CLI ~a was not found on PATH: install it, run `kimi login' ~
once in a terminal, or set kimi-code-delegate.command in /settings"
                  name)))))

(defun resolved-working-directory ()
  "The folder a delegated call runs in: the section's, else this process's."
  (let ((named (nlk:trimmed (or (setting :working-directory) ""))))
    (if (plusp (length named))
        (let ((path (uiop:ensure-directory-pathname named)))
          (unless (uiop:directory-exists-p path)
            (fail "the Kimi working directory ~a is not a directory" named))
          (uiop:native-namestring path))
        (uiop:native-namestring (uiop:getcwd)))))

;;; --- the child as an agent ----------------------------------------------

(defun decode-wire (line)
  "LINE as one JSON-RPC message object, or a refusal naming what arrived."
  (let ((value (handler-case (nlk:decode-json line :whole t)
                 (error (condition)
                   (fail "Kimi returned malformed ACP JSON: ~a"
                         (bounded (princ-to-string condition)))))))
    (unless (hash-table-p value)
      (fail "Kimi returned a non-object ACP frame"))
    (unless (equal "2.0" (gethash "jsonrpc" value))
      (fail "Kimi returned an invalid JSON-RPC version"))
    value))

(defun receive-message (stream timeout)
  "One decoded line from STREAM within TIMEOUT seconds, or :TIMEOUT / :EOF."
  (let ((deadline (+ (get-internal-real-time) (seconds->ticks timeout))))
    (loop
      (cond
        ((listen stream)
         (let ((line (read-line stream nil :eof)))
           (cond ((eq line :eof) (return :eof))
                 ((> (length line) *max-wire-line-chars*)
                  (fail "an ACP response line exceeded the size limit"))
                 (t (return (decode-wire line))))))
        ((>= (get-internal-real-time) deadline) (return :timeout))
        (t (sleep 0.01))))))

(defun open-process-agent (settings)
  "Start one `kimi acp' child for SETTINGS and answer an AGENT over its pipes."
  (declare (ignore settings))
  (let ((command (resolve-command (setting :command))))
    (multiple-value-bind (pid out err process in)
        (nlk:spawn-program (list command "acp")
                           :directory (resolved-working-directory)
                           :environment (child-environment)
                           :output :stream :error-output :stream :input :stream)
      (declare (ignore pid process))
      (unless (and out in)
        (ignore-errors (close out))
        (ignore-errors (close err))
        (ignore-errors (close in))
        (fail "starting `~a acp' gave no stdio pipes" command))
      (make-agent
       :stderr err
       :send (lambda (message)
               (let ((line (nlk:encode-json-object message)))
                 (when (> (length line) *max-wire-line-chars*)
                   (fail "the ACP request exceeded the size limit"))
                 (handler-case
                     (progn (write-string line in)
                            (write-char #\Newline in)
                            (finish-output in))
                   (error ()
                     (fail "Kimi ACP closed its input unexpectedly")))))
       :recv (lambda (timeout) (receive-message out timeout))
       :close (lambda ()
                (ignore-errors (close in))
                (ignore-errors (close out))
                (ignore-errors (close err)))))))

;;; --- the dialogue --------------------------------------------------------

(defun send-message (session object)
  (funcall (agent-send (session-agent session)) object)
  t)

(defun jsonrpc-error (id code message)
  (nlk:json-object "jsonrpc" "2.0" "id" id
                    "error" (nlk:json-object "code" code "message" message)))

(defun permission-denied (id params)
  "The reply to a permission request: the agent's own `reject_once' option
when it offered one, a plain cancellation otherwise."
  (let ((options (gethash "options" params))
        (reject nil))
    (when (vectorp options)
      (loop for option across options
            when (and (hash-table-p option)
                      (equal "reject_once" (gethash "kind" option))
                      (let ((option-id (gethash "optionId" option)))
                        (and (stringp option-id) (plusp (length option-id)))))
              do (setf reject (gethash "optionId" option))))
    (nlk:json-object
     "jsonrpc" "2.0" "id" id
     "result" (nlk:json-object
               "outcome" (if reject
                             (nlk:json-object "outcome" "selected" "optionId" reject)
                             (nlk:json-object "outcome" "cancelled"))))))

(defun collect-update (session params)
  "Keep the agent's words from one session/update; anything else is ignored."
  (let* ((update (gethash "update" params))
         (kind (and (hash-table-p update) (gethash "sessionUpdate" update))))
    (when (and (stringp kind) (equal kind "agent_message_chunk"))
      (let* ((content (gethash "content" update))
             (text (and (hash-table-p content) (gethash "text" content))))
        (when (and (stringp text) (plusp (length text)))
          (push text (session-chunks session)))))
    t))

(defun handle-server-message (session message)
  "Answer MESSAGE when the agent sent it; NIL when it is the response waited for."
  (let ((method (gethash "method" message)))
    (unless (stringp method)
      (return-from handle-server-message nil))
    (let ((params (let ((value (gethash "params" message)))
                    (if (hash-table-p value) value (nlk:make-json-object))))
          (id-p (nth-value 1 (gethash "id" message))))
      (cond
        ;; a notification: the words of the answer, or nothing to say back
        ((equal method "session/update") (collect-update session params))
        ((not id-p) t)
        (t
         (let ((id (gethash "id" message)))
           (cond
             ((equal method "session/request_permission")
              (send-message session (permission-denied id params)))
             ((or (uiop:string-prefix-p "fs/" method)
                  (uiop:string-prefix-p "terminal/" method))
              (send-message session
                             (jsonrpc-error id -32000
                                            "Direct filesystem and terminal access is disabled by the delegate-only client")))
             (t (send-message session
                               (jsonrpc-error id -32601
                                              (format nil "Unsupported ACP method: ~a" method)))))
           t))))))

(defun cancel (session session-id)
  "Tell the agent to stop the prompt in flight; best effort, never fatal."
  (when session-id
    (ignore-errors
      (send-message session
                     (nlk:json-object "jsonrpc" "2.0" "method" "session/cancel"
                                      "params" (nlk:json-object "sessionId" session-id))))))

(defun request (session method params &key timeout session-id)
  "Send one request and wait for its answer, answering the agent's own
requests as they arrive. => the result object."
  (let* ((id (incf (session-next-id session)))
         (seconds (or timeout (setting :startup-timeout-seconds)))
         (deadline (+ (get-internal-real-time) (seconds->ticks seconds))))
    (send-message session (nlk:json-object "jsonrpc" "2.0" "id" id
                                            "method" method "params" params))
    (loop
      (let ((message (funcall (agent-recv (session-agent session)) (ticks-left deadline))))
        (cond
          ((eq message :timeout)
           (when (equal method "session/prompt") (cancel session session-id))
           (let ((said (stderr-text (session-agent session))))
             (fail "timed out after ~a s waiting for Kimi ACP ~a~@[~%~a~]"
                   (round seconds) method (and (plusp (length said)) said))))
          ((eq message :eof)
           (let ((said (stderr-text (session-agent session))))
             (fail "Kimi Code ACP exited before replying~@[~%~a~]"
                   (and (plusp (length said)) said))))
          ((handle-server-message session message))
          (t
           (unless (equal id (gethash "id" message))
             (fail "Kimi returned a response for an unexpected request id"))
           (multiple-value-bind (error error-p) (gethash "error" message)
             (multiple-value-bind (result result-p) (gethash "result" message)
               (cond
                 ((eq error-p result-p)
                  (fail "Kimi returned an invalid JSON-RPC response"))
                 (error-p
                  (let ((detail (if (hash-table-p error) (gethash "message" error) error)))
                    (fail "Kimi ACP ~a failed: ~a" method
                          (or (and (stringp detail) (bounded (redact detail)))
                              "unspecified error"))))
                 (t (return result)))))))))))

(defun offered-values (option key)
  "The string VALUEs OPTION offers under KEY."
  (let ((items (and (hash-table-p option) (gethash key option))))
    (loop for item across (if (vectorp items) items #())
          when (and (hash-table-p item) (stringp (gethash "value" item)))
            collect (gethash "value" item))))

(defun config-option (options id)
  (when (vectorp options)
    (loop for option across options
          when (and (hash-table-p option) (equal id (gethash "id" option)))
            return option)))

(defun initialize-and-authenticate (session)
  "The handshake: protocol version, the agent's identity, its CLI-owned login."
  (let ((result (request session "initialize"
                         (nlk:json-object
                          "protocolVersion" +protocol-version+
                          "clientCapabilities"
                          (nlk:json-object "fs" (nlk:json-object "readTextFile" nil
                                                                  "writeTextFile" nil))
                          "clientInfo"
                          (nlk:json-object "name" "nodecode-kimi-code-delegate"
                                           "title" "Kimi Code Delegate"
                                           "version" +version+)))))
    (unless (hash-table-p result)
      (fail "Kimi ACP initialize returned a non-object result"))
    (let ((version (gethash "protocolVersion" result)))
      (unless (and (numberp version) (= version +protocol-version+))
        (fail "Kimi ACP speaks protocol ~a, not ~a" version +protocol-version+)))
    (let ((info (gethash "agentInfo" result)))
      (unless (and (hash-table-p info)
                   (let ((name (gethash "name" info)))
                     (and (stringp name) (search "kimi" (string-downcase name)))))
        (fail "the ACP executable did not identify itself as Kimi Code")))
    (let ((methods (gethash "authMethods" result)))
      (unless (and (vectorp methods)
                   (loop for method across methods
                         thereis (and (hash-table-p method)
                                      (equal "login" (gethash "id" method)))))
        (fail "Kimi ACP did not advertise its CLI-owned login method: upgrade Kimi Code, ~
then run `kimi login' once in a terminal")))
    (request session "authenticate" (nlk:json-object "methodId" "login"))
    result))

(defun open-conversation (session)
  "One ACP session in the configured folder, with no MCP servers forwarded."
  (let ((result (request session "session/new"
                         (nlk:json-object "cwd" (session-directory session)
                                          "mcpServers" (vector)))))
    (let ((id (and (hash-table-p result) (gethash "sessionId" result))))
      (unless (and (stringp id) (plusp (length (nlk:trimmed id))))
        (fail "Kimi ACP did not return a session id"))
      (setf (session-options session) (gethash "configOptions" result))
      id)))

(defun configure (session session-id model thinking)
  "Set the model, then the reasoning effort, on the session. => model, thinking."
  (let* ((options (session-options session))
         (value (cdr (assoc model +models+ :test #'equal)))
         (model-option (config-option options "model")))
    (unless value (fail "model must be k3 or k3-256k"))
    (unless (member value (offered-values model-option "options") :test #'equal)
      (fail "Kimi ACP does not offer model ~a" model))
    (request session "session/set_config_option"
             (nlk:json-object "sessionId" session-id "configId" "model" "value" value))
    (let* ((thinking-option (config-option options "thinking"))
           (choices (offered-values thinking-option "options"))
           (current (let ((value (and (hash-table-p thinking-option)
                                      (gethash "currentValue" thinking-option))))
                      (and (stringp value) value)))
           (selected (if (equal thinking "auto")
                         (if (member current choices :test #'equal)
                             current
                             (fail "Kimi ACP did not provide a usable current thinking value"))
                         thinking)))
      (unless (member selected choices :test #'equal)
        (fail "Kimi ACP does not offer thinking ~a" thinking))
      (request session "session/set_config_option"
               (nlk:json-object "sessionId" session-id "configId" "thinking"
                                 "value" selected))
      (values value selected))))

(defun ask (session session-id prompt)
  "One prompt, its text collected as the agent streams it. => the answer."
  (setf (session-chunks session) '())
  (let* ((advisory
           (format nil "READ-ONLY DELEGATION BOUNDARY: Return analysis or suggested text ~
only. Do not execute tools, terminal commands, filesystem operations, network mutations, or ~
other side effects. The ACP client will deny such requests.~%~%Delegated task:~%~a"
                   prompt))
         (result (request session "session/prompt"
                          (nlk:json-object "sessionId" session-id
                                           "prompt" (vector (nlk:json-object "type" "text"
                                                                              "text" advisory)))
                          :timeout (setting :timeout-seconds)
                          :session-id session-id))
         (response (nlk:trimmed (format nil "~{~a~}" (reverse (session-chunks session))))))
    (when (zerop (length response))
      (fail "Kimi ACP ended with '~a' but returned no text: the provider status is unknown, ~
so check the login and the account's quota in a terminal, then retry"
            (or (and (hash-table-p result) (gethash "stopReason" result)) "unknown")))
    response))

;;; --- what a caller asks for ---------------------------------------------

(defun check-prompt (prompt)
  "PROMPT as one delegated task, refused when absent or past the limit."
  (let ((text (or prompt "")))
    (when (zerop (length (nlk:trimmed text)))
      (fail "prompt must not be empty"))
    (when (> (length text) *max-prompt-chars*)
      (fail "prompt exceeds the ~:d character limit" *max-prompt-chars*))
    text))

(defun check-thinking (thinking)
  "THINKING as the CLI's own option id: auto, or a name shaped like one."
  (let ((text (string-downcase (nlk:trimmed (or thinking "")))))
    (when (zerop (length text)) (setf text "auto"))
    (unless (and (<= 1 (length text) 64)
                 (or (alphanumericp (char text 0))
                     (find (char text 0) "abcdefghijklmnopqrstuvwxyz0123456789" :test #'char=))
                 (every (lambda (char) (or (alphanumericp char)
                                           (find char "_-" :test #'char=)))
                        text))
      (fail "thinking must be auto or an option id the CLI offers, not ~s" thinking))
    text))

(defun check-model (model)
  (let ((text (nlk:trimmed (or model ""))))
    (when (zerop (length text)) (setf text "k3"))
    (unless (assoc text +models+ :test #'equal)
      (fail "model must be k3 or k3-256k, not ~s" model))
    text))

;;; --- one call, and one probe --------------------------------------------

(defun delegate (prompt model thinking)
  "One delegated call: a fresh child, one session, the agent's words.
=> a plist of :RESPONSE, :MODEL and :THINKING."
  (let* ((settings (running-settings))
         (peer (funcall (or *open-agent* #'open-process-agent) settings))
         (session (make-session settings peer (resolved-working-directory))))
    (unwind-protect
         (progn
           (initialize-and-authenticate session)
           (let ((session-id (open-conversation session)))
             (multiple-value-bind (selected-model selected-thinking)
                 (configure session session-id model thinking)
               (list :response (ask session session-id prompt)
                     :model selected-model
                     :thinking selected-thinking))))
      (when (agent-close peer)
        (ignore-errors (funcall (agent-close peer)))))))

(defun probe ()
  "The handshake only, as the doctor command reports it: who answered, the
models it offers, and its current effort."
  (let* ((settings (running-settings))
         (peer (funcall (or *open-agent* #'open-process-agent) settings))
         (session (make-session settings peer (resolved-working-directory))))
    (unwind-protect
         (let ((initialize (initialize-and-authenticate session)))
           (let ((session-id (open-conversation session)))
             (declare (ignore session-id))
             (let ((info (gethash "agentInfo" initialize)))
               (list :agent (or (and (hash-table-p info) (gethash "name" info)) "Kimi Code")
                     :version (or (and (hash-table-p info) (gethash "version" info)) "unknown")
                     :models (offered-values (config-option (session-options session) "model")
                                              "options")
                     :thinking (let ((value (gethash "currentValue"
                                                     (or (config-option (session-options session)
                                                                        "thinking")
                                                         (nlk:make-json-object)))))
                                 (and (stringp value) value))))))
      (when (agent-close peer)
        (ignore-errors (funcall (agent-close peer)))))))
