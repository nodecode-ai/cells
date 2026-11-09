;;;; wire.lisp --- Coder's REST API, and the workspace PTY one command rides.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two seams, each a special a test rebinds: *HTTP* is one REST exchange
;;;; (nlk:http by default) and *PTY-READ* is the whole of one PTY run, from
;;;; the socket opening to the exit marker or the deadline, answering the text
;;;; the terminal produced (websocket-driver by default). Everywhere else the
;;;; HTTP paths, the build polling and the marker parsing are the work.
;;;;
;;;; The flow is the original's: find the workspace by name, start it when its
;;;; last build stopped, wait for its agent to be connected and ready, then ask
;;;; that agent's PTY to run bash -c a script that ends by printing the exit
;;;; marker, and read terminal output until the marker comes back.

(in-package #:nodecode-coder)

;;; --- the seams ---------------------------------------------------------------

(defun dial (method url headers content timeout)
  "One REST exchange: (values BODY STATUS), as NLK:HTTP answers it."
  (nlk:http method url :headers headers
                    :content content
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))

(defvar *http* #'dial
  "One REST exchange => (values BODY STATUS). A test binds its own.")

(defparameter +exit-prefix+ "__NODECODE_EXIT_"
  "The marker's head; the reconnect id and the exit code follow it, and its
tail closes the marker, so nothing the command prints can be mistaken for an
exit code unless it prints this whole shape with the run's own id.")

(defparameter +marker-chars+ 500000
  "How much terminal output one command may answer with, head and tail kept
and the middle counted rather than carried.")

(defun marker-text (token)
  "The exit marker one PTY run's TOKEN closes the command with."
  (concatenate 'string +exit-prefix+ token "__"))

(defvar *new-token* (lambda ()
                      (format nil "~36r~36r" (random most-positive-fixnum)
                              (get-internal-real-time)))
  "The reconnect id one PTY run carries. A test binds a fixed one.")

(defvar *getenv* #'uiop:getenv
  "The environment read for forward_env. A test binds its own.")

(defun frame-text (message)
  "One PTY frame as text: websocket-driver hands text frames as a string and
Coder's terminal output as octets."
  (etypecase message
    (string message)
    ((vector (unsigned-byte 8))
     (sb-ext:octets-to-string message :external-format :utf-8))
    ((simple-array (unsigned-byte 8) (*))
     (sb-ext:octets-to-string (coerce message '(vector (unsigned-byte 8)))
                              :external-format :utf-8))))

(defun read-pty (url headers marker seconds)
  "The terminal text URL produces, read until MARKER appears, the peer closes
or SECONDS pass: every frame into chunks, the marker checked on what came so
far, so a marker split across frames is still found."
  (let* ((socket (wsd:make-client url :additional-headers headers))
         (mailbox (sb-concurrency:make-mailbox :name "nodecode-coder pty"))
         (deadline (+ (get-internal-real-time)
                      (round (* seconds internal-time-units-per-second))))
         (chunks '()))
    (wsd:on :message socket
            (lambda (message)
              (sb-concurrency:send-message mailbox (list :message (frame-text message)))))
    (wsd:on :close socket
            (lambda (&key code reason)
              (declare (ignore code reason))
              (sb-concurrency:send-message mailbox (list :close))))
    (wsd:on :error socket
            (lambda (error)
              (declare (ignore error))
              (sb-concurrency:send-message mailbox (list :close))))
    (unwind-protect
         (progn
           (handler-case (sb-sys:with-deadline (:seconds (max 1 (min 30 seconds)))
                           (wsd:start-connection socket))
             (sb-sys:deadline-timeout ()
               (fail "the closed websocket to Coder did not open within 30 seconds")))
           (labels ((text () (apply #'concatenate 'string (reverse chunks))))
             (loop
               (let ((message (sb-concurrency:receive-message mailbox :timeout 1)))
                 (cond (message
                        (if (eq (first message) :message)
                            (progn (push (second message) chunks)
                                   (when (search marker (text))
                                     (return (text))))
                            (return (text))))
                       ((> (get-internal-real-time) deadline)
                        (return (text))))))))
      (ignore-errors (nlk:sever-websocket socket)))))

(defvar *pty-read* #'read-pty
  "One PTY run => the text it produced. A test binds a canned transcript.")

;;; --- words every verb shares --------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, or NIL when it is not a string with something in it."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun refuse (format-control &rest args)
  "The call's failure, in words that name what to change."
  (nle:failure (format nil "ERROR: Coder: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal or an error becomes that text, never an unhandled
error."
  `(handler-case (progn ,@body)
     (coder-error (condition)
       (nle:failure (format nil "ERROR: Coder: ~a" (coder-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Coder: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun shell-quote (text)
  "TEXT as one POSIX shell word, single quotes and all."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for character across text
          do (if (char= character #\')
                 (write-string "'\\''" out)
                 (write-char character out)))
    (write-char #\' out)))

(defun query-string (pairs)
  "PAIRS as a URL query string, leading ? and all, or an empty string."
  (if (null pairs)
      ""
      (with-output-to-string (out)
        (write-char #\? out)
        (loop for (key . value) in pairs
              for first = t then nil
              do (unless first (write-char #\& out))
                 (write-string (quri:url-encode key) out)
                 (write-char #\= out)
                 (write-string (quri:url-encode value :encoding :utf-8) out)))))

(defun clipped (text)
  "TEXT under +MARKER-CHARS+, head and tail kept and the middle counted."
  (if (<= (length text) +marker-chars+)
      text
      (let* ((head (floor +marker-chars+ 2))
             (tail (- +marker-chars+ head)))
        (format nil "~a~%... [~:d characters omitted] ...~%~a"
                (subseq text 0 head) (- (length text) head tail)
                (subseq text (- (length text) tail))))))

(defun tidy (text)
  "TEXT as a terminal answer reads: CRLF folded to one LF, the tail trimmed."
  (string-right-trim
   '(#\Return #\Newline #\Space)
   (with-output-to-string (out)
     (loop with length = (length text)
           for index from 0 below length
           for character = (char text index)
           do (cond ((and (char= character #\Return)
                          (< (1+ index) length)
                          (char= (char text (1+ index)) #\Newline))
                     nil)
                    ((char= character #\Return) (write-char #\Newline out))
                    (t (write-char character out)))))))

(defun find-exit (output marker)
  "=> (values START END CODE) for the last complete exit MARKER in OUTPUT, or
(vals nil nil nil) when the command's shell never printed one."
  (let ((position 0) (best nil) (length (length output)))
    (loop
      (let ((start (search marker output :start2 position)))
        (unless start (return (values-list best)))
        (let* ((digits-start (+ start (length marker)))
               (digits-end (loop for index from digits-start below length
                                 while (< (- index digits-start) 3)
                                 while (digit-char-p (char output index))
                                 finally (return index)))
               (close-end (+ digits-end (length marker))))
          (when (and (> digits-end digits-start)
                     (<= close-end length)
                     (string= marker output :start2 digits-end :end2 close-end))
            (setf best (list start close-end
                             (parse-integer output :start digits-start :end digits-end))))
          (setf position (1+ start)))))))

;;; --- the settings -------------------------------------------------------------

(defun https-origin (url)
  "URL as Coder's own origin: https, no credentials, path, query or fragment."
  (let ((url (nonblank url)))
    (unless url
      (nlk:config-error "no url: set it to your Coder deployment's origin, e.g. \"https://coder.example.com\""))
    (unless (uiop:string-prefix-p "https://" url)
      (nlk:config-error "url ~s is not https://; a Coder deployment is reached over HTTPS, e.g. \"https://coder.example.com\"" url))
    (when (or (find #\? url) (find #\# url) (find #\@ url))
      (nlk:config-error "url ~s carries a query, a fragment or credentials; give the origin only, e.g. \"https://coder.example.com\"" url))
    (let ((rest (subseq url (length "https://"))))
      (when (or (zerop (length rest)) (find #\/ rest))
        (nlk:config-error "url ~s has a path; give the origin only, e.g. \"https://coder.example.com\"" url)))
    (string-right-trim "/" url)))

(defun read-settings (values table)
  "The declared VALUES, plus the session token the section names: read once,
here, so an unset one refuses at start and never at a call."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :token-env)))
         (file (nonblank (getf values :token-file)))
         (workspace (nonblank (getf values :workspace)))
         (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (setf (getf plist :url) (https-origin (getf values :url))
          (getf plist :workspace) (or workspace "")
          (getf plist :token)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "token_env names ~a, which is unset or empty" env)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "token_file ~a cannot be read, or is empty" file)))
                (t (nlk:config-error "no token: set token_env to the environment variable holding a Coder session token, or token_file to a file holding one"))))
    plist))

;;; --- one REST exchange --------------------------------------------------------

(defun call (method path &key query payload (timeout nil))
  "METHOD PATH on the deployment => (values BODY STATUS), BODY the text Coder
answered. A dial that fails is a refusal, never an unhandled error."
  (let ((url (concatenate 'string (setting :url) path (query-string query)))
        (seconds (or timeout (setting :timeout))))
    (handler-case
        (funcall *http* method url
                 (list (cons "Coder-Session-Token" (setting :token))
                       (cons "accept" "application/json")
                       (cons "content-type" "application/json"))
                 (and payload (nlk:encode-json-object payload))
                 seconds)
      (error (condition)
        (fail "~a ~a did not answer: ~a" method path
              (nlk:one-line (princ-to-string condition)))))))

(defun detail (body)
  "Coder's own words for a failed call, clipped."
  (let ((text (nlk:trimmed (or (and (stringp body) body) ""))))
    (and (plusp (length text)) (nlk:clip text 300))))

(defun api (method path &key query payload timeout)
  "PATH's decoded JSON answer, or a refusal carrying Coder's own words."
  (multiple-value-bind (body status) (call method path :query query :payload payload
                                                       :timeout timeout)
    (if (and (integerp status) (< status 400))
        (or (ignore-errors (nlk:decode-json body))
            (fail "~a ~a answered something that is not JSON" method path))
        (fail "~a ~a answered HTTP ~a~@[: ~a~]" method path status (detail body)))))

;;; --- workspaces ---------------------------------------------------------------

(defun workspace-summaries (&key name)
  "The caller's own workspaces, NAME narrowing the search on Coder's side."
  (let* ((q (if (nonblank name) (format nil "owner:me name:~a" name) "owner:me"))
         (payload (api :get "/api/v2/workspaces"
                       :query (list (cons "q" q) (cons "limit" "100")))))
    (coerce (nlk:json-array payload "workspaces") 'list)))

(defun named (summaries name)
  "The summary in SUMMARIES whose name is NAME, else NIL."
  (find-if (lambda (summary)
             (let ((found (nlk:json-value summary :string "name")))
               (and found (string= found name))))
           summaries))

(defun resolve-workspace (name)
  "NAME's summary, or a refusal that says what the deployment does have."
  (let* ((summaries (workspace-summaries :name name))
         (summary (named summaries name)))
    (or summary
        (fail "no workspace named ~s that you own~@[; you have ~{~a~^, ~}~]"
              name
              (let ((others (remove-if-not #'identity
                                           (mapcar (lambda (each)
                                                     (nlk:json-value each :string "name"))
                                                   summaries))))
                (and others (subseq others 0 (min 8 (length others)))))))))

(defun workspace-payload (id)
  "One workspace's own payload, the thing latest_build and its agents are in."
  (api :get (format nil "/api/v2/workspaces/~a" (quri:url-encode id))))

(defun agents-of (payload)
  "Every agent the workspace's latest build reports."
  (let ((agents '()))
    (loop for resource across (nlk:json-array (nlk:json-value payload :object "latest_build")
                                              "resources")
          do (loop for agent across (nlk:json-array resource "agents")
                   do (push agent agents)))
    (nreverse agents)))

(defun ready-agent (payload)
  "The first agent of PAYLOAD that is connected and ready to run a command."
  (find-if (lambda (agent)
             (and (let ((status (nlk:json-value agent :string "status")))
                    (and status (string-equal status "connected")))
                  (let ((state (nlk:json-value agent :string "lifecycle_state")))
                    (and state (string-equal state "ready")))))
           (agents-of payload)))

(defun build-state (payload)
  "=> (values TRANSITION STATUS) of PAYLOAD's latest build, both downcased."
  (let ((build (nlk:json-value payload :object "latest_build")))
    (values (string-downcase (or (nlk:json-value build :string "transition") ""))
            (string-downcase (or (nlk:json-value build :string "status") "")))))

(defun start-build (id name deadline)
  "Ask Coder to start workspace ID, and wait for the build to succeed."
  (let* ((build (api :post (format nil "/api/v2/workspaces/~a/builds" (quri:url-encode id))
                     :payload (nlk:json-object "transition" "start")))
         (build-id (nlk:json-value build :string "id")))
    (unless build-id
      (fail "Coder answered no build id when starting ~a" name))
    (loop
      (let* ((state (api :get (format nil "/api/v2/workspacebuilds/~a"
                                       (quri:url-encode build-id))))
             (job (nlk:json-value state :object "job"))
             (status (string-downcase (or (nlk:json-value job :string "status") ""))))
        (when (nlk:json-value job :string "completed_at")
          (unless (string= status "succeeded")
            (fail "the start of ~a finished as ~a" name (or (nonblank status) "unknown")))
          (return build-id))
        (when (> (get-internal-real-time) deadline)
          (fail "~a did not finish starting within ~a seconds" name (setting :startup-timeout)))
        (sleep 1)))))

(defun await-agent (id name deadline)
  "The workspace's ready agent, starting or waiting for it until DEADLINE."
  (let ((payload (workspace-payload id))
        (asked nil))
    (multiple-value-bind (transition status) (build-state payload)
      (cond ((string= transition "delete")
             (fail "workspace ~a is deleted" name))
            ((and (not (string= transition "start")) (string= status "stopped"))
             (setf asked t)
             (start-build id name deadline)
             (setf payload (workspace-payload id)))))
    (loop
      (let ((agent (ready-agent payload)))
        (when agent (return (values agent asked))))
      (when (> (get-internal-real-time) deadline)
        (fail "~a's agent did not connect and turn ready within ~a seconds~@[ (it was started for this call)~]"
              name (setting :startup-timeout) asked))
      (sleep 1)
      (setf payload (workspace-payload id)))))

;;; --- one command over the PTY ---------------------------------------------------

(defun forwarded-exports ()
  "The forward_env names that hold a value here, as export lines; the session
token itself is never forwarded."
  (let ((lines '()))
    (dolist (name (setting :forward-env))
      (let ((name (and (stringp name) (nlk:trimmed name))))
        (when (and name (plusp (length name)) (not (string= name "CODER_API_KEY")))
          (let ((value (funcall *getenv* name)))
            (when (and value (plusp (length value)))
              (push (format nil "export ~a=~a" name (shell-quote value)) lines))))))
    (nreverse lines)))

(defun pty-script (command cwd marker)
  "The one shell word the PTY runs: the command in a login shell, its status
kept, printed between two exit markers, and the same status exited with."
  (let ((body (concatenate 'string
                           (format nil "~@[cd ~a && ~]" (and (nonblank cwd) (shell-quote cwd)))
                           (format nil "bash -lc ~a; __nc_status=$?; " (shell-quote command))
                           (format nil "printf '\\n~a%s~a\\n' \"$__nc_status\"; " marker marker)
                           "exit \"$__nc_status\"")))
    (format nil "bash -c ~a" (shell-quote body))))

(defun pty-url (agent-id reconnect script)
  "The workspace agent's PTY socket, the command riding the query string."
  (let ((origin (setting :url)))
    (format nil "~a/api/v2/workspaceagents/~a/pty?reconnect=~a&command=~a&height=80&width=80"
            (if (uiop:string-prefix-p "https://" origin)
                (concatenate 'string "wss://" (subseq origin (length "https://")))
                (concatenate 'string "ws://" (subseq origin (length "http://"))))
            (quri:url-encode (princ-to-string agent-id))
            (quri:url-encode reconnect)
            (quri:url-encode script :encoding :utf-8))))

(defun run-remote (agent name command cwd timeout)
  "Run COMMAND in workspace NAME through AGENT's PTY => (values OUTPUT CODE)."
  (let* ((token (funcall *new-token*))
         (marker (marker-text token))
         (script (format nil "~{~a~%~}~a" (forwarded-exports)
                         (pty-script command cwd marker)))
         (url (pty-url (nlk:json-value agent :string "id") token script)))
    (when (> (length url) 8000)
      (refuse "the command is too long for the PTY's query string (~:d bytes encoded, 8000 allowed): write it to a file inside the workspace and run that"
              (length url)))
    (let ((text (funcall *pty-read* url
                         (list (cons "Coder-Session-Token" (setting :token)))
                         marker timeout)))
      (multiple-value-bind (start end code) (find-exit text marker)
        (if start
            (values (tidy (concatenate 'string (subseq text 0 start) (subseq text end)))
                    code)
            (values (tidy text) 1))))))

(in-package #:nodecode-coder)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Coder is available through the nodecode-coder cell: three tools run commands and read
workspaces on your own Coder deployment. A person can call the same functions from eval
in the coder package.
  coder_workspaces  {query, max}
      The workspaces you own, each with its id, its last build's transition and status and
      its template. query narrows by name on Coder's side; use it rather than listing
      everything on a deployment with many workspaces.
  coder_workspace  {workspace}
      One workspace in full: its last build, and each agent's status and lifecycle state.
      An agent that is not connected and ready cannot run a command; this is how to see why.
  coder_exec  {command, workspace, cwd, timeout}
      Run one shell command (bash -lc) inside an existing workspace and answer its output and
      its exit code. workspace defaults to the cell's configured workspace; cwd defaults to
      the workspace's home. A stopped workspace is started first, which can take minutes.
      The command rides the PTY's query string, so keep it under a few thousand characters:
      for anything longer, write a script inside the workspace and run the script.
      Only the environment variable names the operator listed in forward_env are copied in;
      the Coder session token never is. Output is capped, with the middle counted rather
      than carried.
  A command's exit code is the workspace shell's own: 0 is success. A missing exit marker
  means the terminal closed before the command finished, and the answer says so by its
  exit code of 1."
  "What (help :coder) answers while the cell runs.")

;;; --- the three tools' arguments ---------------------------------------------------

(nlk:define-contract workspace-list
  (:doc "List the Coder workspaces you own, with each one's id, last build and template. Use it to find the exact workspace name coder_exec should run in.")
  ("query" :string :default ""
   :doc "a name fragment to narrow the list on Coder's side (my-app); omit to list every workspace you own")
  ("max" :integer :slot max-workspaces
   :doc "the most workspaces to answer; default 40"))

(nlk:define-contract workspace-read
  (:doc "Read one Coder workspace in full: its last build's transition and status, and each agent's status and lifecycle state. Use it when coder_exec says a workspace is not ready, to see whether its agent has connected.")
  ("workspace" :text :required t
   :doc "the exact workspace name, as coder_workspaces answered it"))

(nlk:define-contract command-run
  (:doc "Run one shell command inside an existing Coder workspace and answer its terminal output and exit code. The command runs as bash -lc in the workspace, not on this machine, so it sees the workspace's files, tools and network. A stopped workspace is started first, which can take minutes. Keep the command under a few thousand characters; for anything longer, write a script inside the workspace and run the script.")
  ("command" :text :required t
   :doc "the shell command to run in the workspace (ls -la, git status, pytest -q)")
  ("workspace" :string :default ""
   :doc "the workspace to run in; empty, the one the cell is configured with")
  ("cwd" :string :default ""
   :doc "the directory to run in, inside the workspace; empty, the workspace's home directory")
  ("timeout" :integer
   :doc "how many seconds this command may run; overrides the cell's timeout for this one call"))

;;; --- coder_workspaces ------------------------------------------------------------

(defun workspace-entry (summary)
  "One workspace as the list answers it: what a caller needs to pick one."
  (nlk:json-object
   "name" (or (nlk:json-value summary :string "name") "")
   "id" (or (nlk:json-value summary :string "id") "")
   "status" (string-downcase (or (nlk:json-value summary :string "latest_build" "status") ""))
   "transition" (string-downcase (or (nlk:json-value summary :string "latest_build" "transition") ""))
   "template" (or (nlk:json-value summary :string "template_name") "")))

(defun list-workspaces (call)
  "The coder_workspaces tool: the workspaces the caller owns."
  (answering
    (let* ((query (nonblank (workspace-list-query call)))
           (cap (or (workspace-list-max-workspaces call) 40))
           (summaries (workspace-summaries :name query))
           (matching (if query
                          (remove-if-not
                           (lambda (summary)
                             (search (string-downcase query)
                                     (string-downcase (or (nlk:json-value summary :string "name") ""))))
                           summaries)
                          summaries))
           (shown (if (> (length matching) cap) (subseq matching 0 cap) matching))
           (answer (nlk:json-object
                    "count" (length shown)
                    "workspaces" (map 'vector #'workspace-entry shown))))
      (when (> (length matching) cap)
        (setf (gethash "truncated" answer) t
              (gethash "truncated_note" answer)
              (format nil "Showing the first ~a of ~a workspaces; narrow with query to see the rest."
                      cap (length matching))))
      (nlk:encode-json-object answer))))

;;; --- coder_workspace -------------------------------------------------------------

(defun agent-entry (agent)
  (nlk:json-object
   "id" (or (nlk:json-value agent :string "id") "")
   "name" (or (nlk:json-value agent :string "name") "")
   "status" (string-downcase (or (nlk:json-value agent :string "status") ""))
   "lifecycle_state" (string-downcase (or (nlk:json-value agent :string "lifecycle_state") ""))
   "operating_system" (or (nlk:json-value agent :string "operating_system") "")
   "architecture" (or (nlk:json-value agent :string "architecture") "")))

(defun read-workspace (call)
  "The coder_workspace tool: one workspace's build and agents."
  (answering
    (let* ((name (nlk:trimmed (or (workspace-read-workspace call) "")))
           (summary (resolve-workspace name))
           (id (or (nlk:json-value summary :string "id")
                   (fail "Coder answered no id for workspace ~a" name)))
           (payload (workspace-payload id))
           (agents (agents-of payload)))
      (multiple-value-bind (transition status) (build-state payload)
        (nlk:encode-json-object
         (nlk:json-object
          "name" (or (nlk:json-value payload :string "name") name)
          "id" id
          "transition" transition
          "status" status
          "template" (or (nlk:json-value payload :string "template_name")
                         (nlk:json-value summary :string "template_name") "")
          "ready" (if (ready-agent payload) t :false)
          "agents" (map 'vector #'agent-entry agents)
          "note" (cond ((ready-agent payload) "")
                        ((string= transition "delete") "this workspace is deleted")
                        ((and (not (string= transition "start")) (string= status "stopped"))
                         "the workspace is stopped: coder_exec starts it, which can take minutes")
                        (t "no agent is connected and ready yet: the workspace may still be starting"))))))))

;;; --- coder_exec -------------------------------------------------------------------

(defun workspace-of (call)
  "The workspace a command runs in: the call's own, else the configured one."
  (let ((named (nonblank (command-run-workspace call)))
        (configured (nonblank (setting :workspace))))
    (or named configured
        (fail "no workspace: name one in the call, or set the cell's workspace to the workspace you work in"))))

(defun run-command (call)
  "The coder_exec tool: one command, in the workspace, through its agent's PTY."
  (answering
    (let* ((command (nlk:trimmed (command-run-command call)))
           (name (workspace-of (if (plusp (length command))
                                   call
                                   (return-from run-command (refuse "the command is empty")))))
           (summary (resolve-workspace name))
           (id (or (nlk:json-value summary :string "id")
                   (fail "Coder answered no id for workspace ~a" name)))
           (seconds (or (command-run-timeout call) (setting :timeout)))
           (deadline (+ (get-internal-real-time)
                        (round (* (setting :startup-timeout) internal-time-units-per-second)))))
      (multiple-value-bind (agent started) (await-agent id name deadline)
        (let ((agent-id (nlk:json-value agent :string "id")))
          (unless agent-id
            (fail "workspace ~a reported an agent with no id" name))
          (multiple-value-bind (output code)
              (run-remote agent name command (command-run-cwd call) seconds)
            (nlk:encode-json-object
             (nlk:json-object "workspace" name
                              "exit_code" code
                              "output" (clipped output)
                              "started" (if started t :false)))))))))


;;; --- the cell --------------------------------------------------------------------

(nle:define-cell coder
  (:section ("coder")
    (:guide "the deployment's HTTPS url; a session token named in token_env or kept in token_file; workspace is the one a command runs in by default; next step: ask for a command to run in the workspace, or for the list of your workspaces")
    (:one-of "token_env" "token_file")
    ("url" :string :required t
     :doc "the Coder deployment's origin, https:// and no path, e.g. https://coder.example.com")
    ("token_env" :env
     :doc "the environment variable holding a Coder session token (CODER_API_KEY)")
    ("token_file" :path
     :doc "a file holding a Coder session token, guarded by the file's own permissions")
    ("workspace" :string :default ""
     :doc "the workspace coder_exec runs in when a call names none")
    ("forward_env" :list
     :doc "environment variable names copied into the remote shell for one command; empty, none, and the session token never")
    ("startup_timeout" :integer :default 180 :min 5
     :doc "how long a stopped workspace may take to start and its agent to connect, in seconds")
    ("timeout" :integer :default 120 :min 1
     :doc "how long one command may run, in seconds"))
  (:settings #'read-settings)
  (:help :coder
         "coder: coder_workspaces, coder_workspace and coder_exec read workspaces on your Coder deployment and run commands in one"
         *manual*)
  (:tool "coder_workspaces" (nlk:contract 'workspace-list) #'list-workspaces)
  (:tool "coder_workspace" (nlk:contract 'workspace-read) #'read-workspace)
  (:tool "coder_exec" (nlk:contract 'command-run) #'run-command))
