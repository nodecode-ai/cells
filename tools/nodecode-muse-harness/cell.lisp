;;;; cell.lisp --- one Muse Code turn over MSP, from the local muse executable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of SnoobieJunes/hermes-muse-harness. The Hermes
;;;; plugin started `muse serve' and spoke Muse Session Protocol
;;;; (newline-delimited JSON-RPC 2.0) on its stdin and stdout, one turn per
;;;; call; this cell does the same from Lisp, through NLK:SPAWN-PROGRAM, and
;;;; answers the turn's text.
;;;;
;;;; The rules the original set are kept, because each was a decision:
;;;;  * the subscription is tried first: no key goes into the child's
;;;;    environment, so Muse answers on the browser login it already holds;
;;;;  * a typed quota failure (`error.quota' on turn/completed) retries the one
;;;;    prompt on pay-as-you-go, with META_API_KEY set from the variable this
;;;;    folder's api_key_env names -- never from the rest of the environment,
;;;;    and never MUSE_CODE_SUB_TOKEN, which another tool minted;
;;;;  * the child gets an allowlist of the environment, not a copy of it;
;;;;  * an MCP server the model names must be loopback HTTP with an explicit
;;;;    port: a stdio command would be arbitrary execution once approved;
;;;;  * the workspace must stay inside the session directory;
;;;;  * contributor mode is always off.

(defpackage #:nodecode-muse-harness
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:send-frame #:receive-frame #:close-transport
           #:*transport-maker*))

(in-package #:nodecode-muse-harness)

(nlk:define-peripheral muse-harness :not-running t)

;;; --- what the model is told ---------------------------------------------------------

(defparameter +primer+
  "One prompt inside the local Muse Code session, through the `muse' you
already installed and logged in (`muse login'). Muse's own shell and file
tools stay inside the session directory, and approval_mode decides whether
they ask first.

  muse_turn      the tool: prompt, and optionally model, workspace,
                 mcp_servers, timeout_seconds
  /muse PROMPT   the same turn from the keyboard; /muse alone shows the mode

The subscription is used first: the first attempt passes no key, so Muse
answers on the browser login. When Muse reports the allowance exhausted, the
same prompt is retried once on pay-as-you-go with META_API_KEY set from the
environment variable api_key_env names, and the answer says so.

mcp_servers is a JSON object of loopback HTTP servers, for example
{\"unreal\":{\"transport\":\"streamableHttp\",\"url\":\"http://127.0.0.1:8000/mcp\"}}.
A stdio command is refused, because a model-supplied command is execution.
Contributor mode is always off. Nothing here mints a key, calls api.meta.ai
itself, or reads Muse's login files."
  "What (help :muse) answers while the cell runs.")

;;; --- limits ---------------------------------------------------------------------------

(defparameter *default-timeout* 600
  "Seconds one turn waits when neither the call nor the section says.")

(defparameter *max-timeout* 1200
  "The longest one turn waits.")

(defparameter *frame-limit* 8000000
  "The longest frame MSP may send: one line of JSON on the child's stdout.")

(defparameter *prompt-limit* 1000000
  "The longest prompt one turn carries, in characters.")

(defparameter *tail-chars* 2000
  "How much of what the child wrote to stderr an error repeats.")

;;; --- conditions ------------------------------------------------------------------------

(define-condition quota-exhausted (muse-harness-error)
  ((detail :initarg :detail :reader quota-detail :initform nil))
  (:report (lambda (condition stream)
             (format stream "Muse reports the subscription allowance is exhausted~@[ (~a)~]"
                     (quota-detail condition))))
  (:documentation "The typed quota failure: MSP's `error.quota' on turn/completed."))

;;; --- the environment a child gets --------------------------------------------------------

(defparameter *allowed-names* '("PATH" "HOME" "USER" "TMPDIR" "LANG")
  "The whole environment names the child inherits; LC_* and XDG_* ride along.")

(defparameter *blocked-names* '("META_API_KEY" "MODEL_API_KEY" "META_MODEL_API_KEY" "MUSE_CODE_SUB_TOKEN")
  "Keys never forwarded: the subscription attempt must find none of them, and
MUSE_CODE_SUB_TOKEN was minted by another tool.")

(defun environment-name (entry)
  "The NAME of ENTRY, a \"NAME=value\" string: up to its first `='."
  (subseq entry 0 (or (position #\= entry :start (min 1 (length entry))) (length entry))))

(defun this-environment ()
  "This process's environment, as \"NAME=value\" strings."
  (coerce (sb-ext:posix-environ) 'list))

(defun child-environment (&key api-key)
  "The child's environment, from an allowlist: no provider key, no shell, no
terminal. API-KEY, when given, is the one thing added, as META_API_KEY."
  (let ((out '()))
    (dolist (entry (this-environment))
      (let ((name (environment-name entry)))
        (when (and (not (member name *blocked-names* :test #'string=))
                   (or (member name *allowed-names* :test #'string=)
                       (uiop:string-prefix-p "LC_" name)
                       (uiop:string-prefix-p "XDG_" name)))
          (push entry out))))
    (when (and (stringp api-key) (plusp (length (nlk:trimmed api-key))))
      (push (format nil "META_API_KEY=~a" (nlk:trimmed api-key)) out))
    (nreverse out)))

;;; --- a UUIDv7, which MSP makes a command id -----------------------------------------------

(defun uuid7 ()
  "A UUIDv7: MSP rejects a command id that is not one."
  (let* ((millis (logand (round (* 1000 (nlk:unix-now))) (1- (ash 1 48))))
         (random (random (ash 1 74)))
         (value (logior (ash millis 80) (ash 7 76) random))
         (value (logior (logandc2 value (ash 3 62)) (ash 2 62)))
         (hex (format nil "~32,'0x" value)))
    (format nil "~a-~a-~a-~a-~a"
            (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
            (subseq hex 16 20) (subseq hex 20 32))))

;;; --- the frames a transport carries ------------------------------------------------------

(defstruct (mailbox (:copier nil))
  "Frames the reader thread has taken off the child's stdout, in order."
  (lock (sb-thread:make-mutex :name "muse serve mailbox"))
  (ready (sb-thread:make-waitqueue :name "muse serve frame"))
  (items '())
  (closed nil))

(defun mailbox-post (mailbox item)
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (setf (mailbox-items mailbox) (nconc (mailbox-items mailbox) (list item)))
    (sb-thread:condition-broadcast (mailbox-ready mailbox))))

(defun mailbox-close (mailbox)
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (setf (mailbox-closed mailbox) t)
    (sb-thread:condition-broadcast (mailbox-ready mailbox))))

(defun mailbox-take (mailbox timeout)
  "The oldest frame, or :CLOSED, or :TIMEOUT once TIMEOUT seconds pass."
  (let ((deadline (+ (get-internal-real-time)
                      (round (* (max 0 timeout) internal-time-units-per-second)))))
    (sb-thread:with-mutex ((mailbox-lock mailbox))
      (loop
        (when (mailbox-items mailbox)
          (return (pop (mailbox-items mailbox))))
        (when (mailbox-closed mailbox)
          (return :closed))
        (let ((left (/ (- deadline (get-internal-real-time))
                       internal-time-units-per-second)))
          (when (<= left 0)
            (return :timeout))
          ;; a spurious wake re-reads the queue; the timeout is the bound
          (unless (sb-thread:condition-wait (mailbox-ready mailbox)
                                             (mailbox-lock mailbox)
                                             :timeout left)
            (return :timeout)))))))

(defstruct (transport (:copier nil))
  "One child speaking MSP on its stdin and stdout."
  pid
  process
  input
  output
  (mailbox nil)
  (thread nil)
  (log-path nil)
  (label "muse serve")
  (next-id 0)
  (closed-p nil)
  (script nil))

(defun send-frame (transport frame)
  "Write FRAME, one line of JSON, to TRANSPORT's child -- or to the script
standing in for it."
  (let ((script (transport-script transport)))
    (if script
        (funcall (getf script :send) frame)
        (process-send transport frame))))

(defun process-send (transport frame)
  (when (transport-closed-p transport)
    (fail "~a is gone~a" (transport-label transport) (serve-tail transport)))
  (handler-case
      (let ((stream (transport-input transport)))
        (write-string (nlk:encode-json-object frame) stream)
        (terpri stream)
        (force-output stream))
    (error (condition)
      (fail "writing to ~a failed (~a)~a"
            (transport-label transport) condition (serve-tail transport)))))

(defun receive-frame (transport timeout)
  "The next frame from TRANSPORT, or a failure after TIMEOUT seconds -- the
child gone, or silent. A script standing in for the child answers the same
:CLOSED and :TIMEOUT a pipe does."
  (let ((script (transport-script transport)))
    (loop
      (let ((item (if script
                      (funcall (getf script :receive) timeout)
                      (mailbox-take (transport-mailbox transport) timeout))))
        (cond
          ((eq item :closed)
           (fail "~a exited before the turn finished~a"
                 (transport-label transport) (serve-tail transport)))
          ((eq item :timeout)
           (fail "~a did not answer within ~,1f s~a"
                 (transport-label transport) timeout (serve-tail transport)))
          ((eq item :skip) (setf timeout 0.5))
          (t (return item)))))))

(defun close-transport (transport)
  "Stop TRANSPORT's child and let go of its pipes."
  (let ((script (transport-script transport)))
    (if script
        (funcall (getf script :close))
        (process-close transport))))

(defun process-close (transport)
  (unless (transport-closed-p transport)
    (setf (transport-closed-p transport) t)
    (let ((process (transport-process transport)))
      (when (and process (eq :running (sb-ext:process-status process)))
        (ignore-errors (sb-ext:process-kill process 15))
        (unless (ignore-errors (sb-ext:process-wait process 2))
          (ignore-errors (sb-ext:process-kill process 9))
          (ignore-errors (sb-ext:process-wait process 2)))))
    (ignore-errors (close (transport-input transport)))
    (ignore-errors (close (transport-output transport))))
  t)

(defun serve-tail (transport)
  "What the child wrote to stderr, as a suffix for an error, or NIL."
  (let ((path (transport-log-path transport)))
    (when (and path (probe-file path))
      (ignore-errors
        (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (uiop:read-file-string path))))
          (when (plusp (length text))
            (format nil ": ~a" (nlk:clip text *tail-chars*))))))))

;;; --- starting the child ------------------------------------------------------------------

(defun serve-log-path ()
  "This run's stderr file, emptied first, so an error repeats this run's tail."
  (let ((path (nlk:home "muse/serve.log")))
    (ensure-directories-exist path)
    (when (probe-file path)
      (ignore-errors (delete-file path)))
    path))

(defun serve-argv (binary model)
  "The argv that starts MSP: the user's own executable, contributor mode off,
no session log, and MODEL as one token."
  (let ((model (nlk:trimmed (or model ""))))
    (when (or (zerop (length model))
              (find-if (lambda (character)
                         (member character '(#\Space #\Tab #\Newline #\; #\| #\&)))
                       model))
      (fail "~s is no Muse model id: one token, with no spaces or shell punctuation" model))
    (list binary "serve" "--provider" "meta" "--contributor-mode" "off"
          "--no-session-log" "--model" model)))

(defun start-serve (plan)
  "Spawn PLAN's argv. => a TRANSPORT, or a failure naming what could not start."
  (let ((argv (serve-argv (getf plan :binary) (getf plan :model))))
    (handler-case
        (nlk:bind (((pid output _ process input)
                    (nlk:spawn-program argv
                                       :input :stream
                                       :output :stream
                                       :error-output (getf plan :log-path)
                                       :environment (getf plan :environment))))
          (declare (ignore _))
          (let ((transport (make-transport :pid pid :process process
                                           :input input :output output
                                           :log-path (getf plan :log-path)
                                           :label "muse serve"
                                           :mailbox (make-mailbox))))
            (start-reader transport)
            transport))
      (error (condition)
        (fail "could not start ~s (~a): is Muse Code installed, and `muse' on PATH?"
              (first argv) condition)))))

(defun start-reader (transport)
  "One thread takes the child's stdout, a line at a time, into its mailbox."
  (setf (transport-thread transport)
        (sb-thread:make-thread
         (lambda ()
           (unwind-protect
                (handler-case
                    (loop for line = (read-line (transport-output transport) nil nil)
                          while line
                          do (mailbox-post (transport-mailbox transport) (parse-frame line)))
                  (error () nil))
             (mailbox-close (transport-mailbox transport))))
         :name "muse serve stdout"))
  transport)

(defun parse-frame (line)
  "LINE decoded: a frame hash table, :SKIP for a blank line, or an error
frame carrying what could not be read."
  (let ((text (string-trim '(#\Space #\Tab #\Return) line)))
    (cond
      ((zerop (length text)) :skip)
      ((> (length text) *frame-limit*)
       (nlk:json-object "jsonrpc" "2.0"
                        "error" (nlk:json-object "message" "muse frame exceeded the size cap")))
      (t
       (let ((frame (handler-case (nlk:decode-json text) (error () nil))))
         (if (hash-table-p frame)
             frame
             (nlk:json-object
              "jsonrpc" "2.0"
              "error" (nlk:json-object "message"
                                        (format nil "non-json frame: ~a"
                                                (nlk:clip text 200))))))))))

(defvar *transport-maker* nil
  "When bound, (FUNCALL *TRANSPORT-MAKER* PLAN) answers the transport
instead of starting `muse serve' -- what a test scripts MSP through. PLAN is
(:binary B :model M :environment ENV :log-path PATH).")

(defun open-transport (binary model api-key)
  (let ((plan (list :binary binary :model model
                    :environment (child-environment :api-key api-key)
                    :log-path (serve-log-path))))
    (or (and *transport-maker* (funcall *transport-maker* plan))
        (start-serve plan))))

;;; --- reading a frame ---------------------------------------------------------------------

(defun frame-id (frame) (nlk:json-value frame :any "id"))
(defun frame-method (frame) (nlk:json-value frame :string "method"))

(defun present-p (object &rest path)
  "Whether the final key of PATH exists on OBJECT, whatever its value."
  (nth-value 1 (apply #'nlk:json-value object :any path)))

(defun server-request-p (frame)
  "Whether FRAME is MSP asking this client something: a method and an id, and
no result or error of its own."
  (and (present-p frame "method")
       (present-p frame "id")
       (not (present-p frame "result"))
       (not (present-p frame "error"))))

(defun seconds-left (deadline)
  (max 0 (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))

;;; --- one turn --------------------------------------------------------------------------

(defstruct (turn-state (:copier nil))
  (texts '())
  (items (make-hash-table :test #'equal))
  (decisions '())
  (session nil)
  (allow nil)
  (terminal nil)
  (error nil)
  (billing "subscription"))

(defun send-request (transport method params)
  "Send METHOD with PARAMS. => the id its reply carries."
  (let ((id (princ-to-string (incf (transport-next-id transport)))))
    (send-frame transport (nlk:json-object "jsonrpc" "2.0" "id" id
                                            "method" method "params" params))
    id))

(defun pick-choice (choices allow)
  "The choice to answer an approval with: the approved one under allowAll,
else the first denial MSP offers."
  (let ((wanted '("denied" "deniedPolicyAmendment" "abort")))
    (find-if (lambda (choice)
               (let ((decision (nlk:json-value choice :string "decision")))
                 (if allow
                     (equal decision "approved")
                     (member decision wanted :test #'string=))))
             choices)))

(defun answer-server-request (transport frame state)
  "Answer what MSP asked: the empty result it expects, then, for an approval,
the decision the mode gives. The model cannot set the mode; only the section can."
  (send-frame transport (nlk:json-object "jsonrpc" "2.0" "id" (frame-id frame)
                                          "result" (nlk:json-object)))
  (when (equal "approval/request" (frame-method frame))
    (let* ((params (or (nlk:json-value frame :object "params") (nlk:json-object)))
           (choices (coerce (or (nlk:json-value params :array "availableChoices") #()) 'list))
           (choice (pick-choice choices (turn-state-allow state)))
           (decision (if choice (nlk:json-value choice :string "decision") "unanswered")))
      (push (list :approval (nlk:json-value params :string "approvalId") :decision decision)
            (turn-state-decisions state))
      (when choice
        (send-request transport "approval/decide"
                       (nlk:json-object
                        "commandId" (uuid7)
                        "sessionId" (or (nlk:json-value params :string "sessionId")
                                        (turn-state-session state))
                        "approvalId" (nlk:json-value params :string "approvalId")
                        "requirementId" (nlk:json-value params :string "currentRequirementId")
                        "choiceId" (nlk:json-value choice :string "choiceId")))))))

(defun note-frame (frame state)
  "Take from FRAME what the turn keeps: the agent's text, the terminal, its error."
  (let ((method (frame-method frame))
        (params (or (nlk:json-value frame :object "params") (nlk:json-object))))
    (cond
      ((equal method "item/started")
       (let ((item (or (nlk:json-value params :object "item") params)))
         (when (and (equal "agentMessage" (nlk:json-value item :string "kind"))
                    (nlk:json-value item :string "itemId"))
           (setf (gethash (nlk:json-value item :string "itemId") (turn-state-items state)) t))))
      ((equal method "item/delta")
       (let ((item-id (nlk:json-value params :string "itemId"))
             (field (nlk:json-value params :string "field")))
         (when (and item-id
                    (gethash item-id (turn-state-items state))
                    (or (null field) (equal field "text")))
           (push (or (nlk:json-value params :string "delta") "")
                 (turn-state-texts state)))))
      ((equal method "turn/completed")
       (setf (turn-state-terminal state) (nlk:json-value params :string "terminal")
             (turn-state-error state) (nlk:json-value params :object "error"))))))

(defun quota-failure-p (frame)
  "Whether FRAME is MSP's typed quota failure: `turn/completed' whose error
carries the quota member. Never decided by reading a message."
  (let ((error (nlk:json-value frame :object "params" "error")))
    (and (equal "turn/completed" (frame-method frame))
         error
         (present-p error "quota"))))

(defun await (transport state deadline &key match until-completed)
  "Read frames until MATCH's reply, or -- with UNTIL-COMPLETED -- the turn's
completion, answering MSP's requests on the way. => the frame that ended it."
  (loop
    (let ((frame (receive-frame transport (seconds-left deadline))))
      (cond
        ((server-request-p frame)
         (answer-server-request transport frame state))
        ((and match (equal match (princ-to-string (or (frame-id frame) ""))))
         (let ((error (nlk:json-value frame :object "error")))
           (when error
             (fail "muse answered with an error: ~a"
                   (or (nlk:json-value error :string "message") "no message"))))
         (return frame))
        (t
         (note-frame frame state)
         (when (and until-completed (equal "turn/completed" (frame-method frame)))
           (when (quota-failure-p frame)
             (error 'quota-exhausted
                    :detail (nlk:json-value frame :object "params" "error")))
           (return frame)))))))

(defun one-turn (prompt model workspace approval-mode mcp-servers timeout billing binary api-key)
  "One session, one turn. => (values TEXT STATE); the child is never left behind."
  (let* ((transport (open-transport binary model api-key))
         (state (make-turn-state :allow (equal approval-mode "allowAll")
                                 :billing billing))
         (deadline (+ (get-internal-real-time)
                      (round (* timeout internal-time-units-per-second)))))
    (unwind-protect
         (progn
           (await transport state deadline
                  :match (send-request
                          transport "initialize"
                          (nlk:json-object
                           "clientInfo" (nlk:json-object "name" "nodecode_muse_harness"
                                                          "version" "0.1.0")
                           "capabilities" (nlk:json-object
                                           "userInputDialogs" nil
                                           "requestedCapabilities" (vector "sessionMcp")))))
           (send-frame transport (nlk:json-object "jsonrpc" "2.0" "method" "initialized"))
           (let ((start (nlk:json-object "commandId" (uuid7)
                                          "approvalMode" approval-mode
                                          "workspaceRoot" workspace
                                          "modelId" model)))
             (when (hash-table-p mcp-servers)
               (setf (gethash "config" start)
                     (nlk:json-object "mcpServers" mcp-servers)))
             (let* ((answer (await transport state deadline
                                     :match (send-request transport "session/start" start)))
                    (session-id (nlk:json-value answer :string "result" "session" "sessionId")))
               (unless session-id
                 (fail "muse session/start answered no session"))
               (setf (turn-state-session state) session-id)
               (await transport state deadline
                      :match (send-request
                              transport "turn/start"
                              (nlk:json-object
                               "commandId" (uuid7)
                               "sessionId" session-id
                               "input" (vector (nlk:json-object "type" "text" "text" prompt)))))
               (await transport state deadline :until-completed t)
               (values (format nil "~{~a~}" (nreverse (turn-state-texts state))) state))))
      (close-transport transport))))

;;; --- the workspace and the MCP servers a call may name -------------------------------------

(defun directory-truename (path)
  (ignore-errors (uiop:truename* (uiop:ensure-directory-pathname path))))

(defun workspace-within (workspace root)
  "WORKSPACE resolved, or a failure when it is not a directory inside ROOT."
  (let* ((base (or (directory-truename root) (fail "~a is no directory" root)))
         (given (nlk:trimmed (or workspace "")))
         (target (and (plusp (length given)) (directory-truename given))))
    (unless target
      (fail "~a is no directory to run Muse in" given))
    (unless (uiop:subpathp target base)
      (fail "the workspace must stay inside the session directory ~a"
            (uiop:native-namestring base)))
    (uiop:native-namestring target)))

(defun loopback-url-p (url)
  "Whether URL is an http address on 127.0.0.1, localhost or ::1 with an
explicit port -- the only MCP server a model may name."
  (let ((start (cond ((uiop:string-prefix-p "http://" url) 7)
                     ((uiop:string-prefix-p "https://" url) 8)
                     (t nil))))
    (when start
      (let* ((rest (subseq url start))
             (end (or (position-if (lambda (character)
                                     (member character '(#\/ #\? #\#)))
                                   rest)
                      (length rest)))
             (authority (subseq rest 0 end)))
        (when (and (plusp (length authority))
                   (not (find #\@ authority)))
          (let* ((colon (position #\: authority :from-end t))
                 (host (if colon (subseq authority 0 colon) authority))
                 (port (and colon (ignore-errors
                                    (parse-integer authority :start (1+ colon))))))
            (and port
                 (<= 1 port 65535)
                 (member (string-downcase
                          (if (and (> (length host) 1) (char= #\[ (char host 0)))
                              (string-trim "[]" host)
                              host))
                         '("127.0.0.1" "localhost" "::1")
                         :test #'string=))))))))

(defun normalize-mcp-servers (raw)
  "RAW, a JSON object of name to server, kept to streamableHttp on loopback.
A stdio command is refused: a model-supplied command is arbitrary execution."
  (let ((object (cond ((null raw) nil)
                      ((hash-table-p raw) raw)
                      ((and (stringp raw) (plusp (length (nlk:trimmed raw))))
                       (let ((decoded (handler-case (nlk:decode-json raw)
                                        (error () (fail "mcp_servers is not valid JSON")))))
                         (unless (hash-table-p decoded)
                           (fail "mcp_servers must be a JSON object of name to server"))
                         decoded))
                      ((stringp raw) nil)
                      (t (fail "mcp_servers must be a JSON object of name to server")))))
    (when object
      (let ((cleaned (make-hash-table :test #'equal)))
        (maphash (lambda (name spec)
                   (unless (hash-table-p spec)
                     (fail "MCP server ~a is no object" name))
                   (unless (equal "streamableHttp" (nlk:json-value spec :string "transport"))
                     (fail "MCP server ~a must be streamableHttp on loopback: a stdio command would be execution"
                           name))
                   (let ((url (nlk:json-value spec :string "url")))
                     (unless (and url (loopback-url-p url))
                       (fail "MCP server ~a needs an http url on 127.0.0.1, localhost or ::1 with an explicit port"
                             name))
                     (setf (gethash name cleaned)
                           (nlk:json-object "transport" "streamableHttp" "url" url))))
                 object)
        cleaned))))

;;; --- the turn the tool and the command share ------------------------------------------------

(defun named-or (value fallback)
  (let ((value (and (stringp value) (nlk:trimmed value))))
    (if (and value (plusp (length value))) value fallback)))

(defun api-key ()
  "The Meta key for the one pay-as-you-go retry: the variable the section
names, never the rest of the environment."
  (let ((name (or (setting :api-key-env) "")))
    (when (and (stringp name) (plusp (length (nlk:trimmed name))))
      (let ((value (uiop:getenv (nlk:trimmed name))))
        (and value (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))))

(defun run-turn (&key prompt model workspace approval-mode mcp-servers timeout)
  "One Muse turn: the subscription first, and the same prompt once on
pay-as-you-go after a typed quota failure. => (values TEXT STATE)."
  (let* ((prompt (or prompt ""))
         (binary (named-or (setting :binary) "muse"))
         (model (named-or model (setting :model)))
         (approval-mode (named-or approval-mode (setting :approval-mode)))
         (timeout (or timeout (setting :timeout-seconds) *default-timeout*))
         (root (uiop:native-namestring (uiop:getcwd))))
    (unless (plusp (length (nlk:trimmed prompt)))
      (fail "a Muse turn needs a prompt"))
    (when (> (length prompt) *prompt-limit*)
      (fail "the prompt is longer than ~a characters" *prompt-limit*))
    (unless (and (realp timeout) (<= 1 timeout *max-timeout*))
      (fail "timeout_seconds must be between 1 and ~a" *max-timeout*))
    (let ((workspace (workspace-within (named-or workspace root) root))
          (mcp (normalize-mcp-servers mcp-servers))
          (key (api-key)))
    (handler-case
        (one-turn prompt model workspace approval-mode mcp timeout
                  "subscription" binary nil)
      (quota-exhausted ()
        (if key
            (one-turn prompt model workspace approval-mode mcp timeout
                      "api_key_fallback" binary key)
            (fail "the Muse subscription allowance is exhausted and ~a names no key: set it, or wait for the allowance"
                  (named-or (setting :api-key-env) "api_key_env"))))))))

(defun error-text (object)
  (or (and (hash-table-p object) (nlk:json-value object :string "message"))
      (and (hash-table-p object) (nlk:json-value object :string "kind"))
      "no message"))

(defun tool-turn (call)
  "The muse_turn tool: one turn, its text as the answer."
  (handler-case
      (multiple-value-bind (text state)
          (run-turn :prompt (muse-turn-prompt call)
                    :model (muse-turn-model call)
                    :workspace (muse-turn-workspace call)
                    :mcp-servers (muse-turn-mcp-servers call)
                    :timeout (let ((seconds (muse-turn-timeout-seconds call)))
                               (and seconds (plusp seconds) seconds)))
        (let ((terminal (turn-state-terminal state)))
          (if (and terminal (not (equal terminal "completed")))
              (nle:failure (format nil "ERROR: Muse ended the turn ~a: ~a~%~a"
                                   terminal (error-text (turn-state-error state)) text))
              (format nil "~a~@[~%[pay-as-you-go: the subscription allowance was exhausted, ~
so Muse billed this turn with META_API_KEY]~]"
                      text
                      (equal (turn-state-billing state) "api_key_fallback")))))
    (muse-harness-error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))
    (error (condition)
      (nle:failure (format nil "ERROR: the Muse turn failed: ~a" condition)))))

(defun run-muse (args session-id)
  "One /muse turn; typed alone, the mode and the model it would use."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (format nil "muse: one turn inside the local Muse Code session~%~
approval: ~a~%model: ~a~%usage: /muse PROMPT"
                (setting :approval-mode) (setting :model))
        (tool-turn (make-muse-turn :prompt text)))))

;;; --- what a call may say ----------------------------------------------------------------

(nlk:define-contract muse-turn
  (:doc "Run one prompt inside the local Muse Code session -- the `muse' executable the user installed and logged in -- and answer the model's reply. The subscription is used first; when Muse reports the allowance exhausted, the same prompt is retried once on pay-as-you-go. Muse's own shell and file tools stay inside the session directory. mcp_servers may only be loopback HTTP; a stdio command is refused. Use it to hand a task to Muse, or to read what Muse answers, when the user asks for Muse by name.")
  ("prompt" :text :required t
            :doc "the prompt Muse should run")
  ("model" :string :default ""
           :doc "the Muse model id; empty uses the folder's model setting")
  ("workspace" :string :default ""
              :doc "a directory inside the session directory to run the turn in; empty uses the session directory")
  ("mcp_servers" :string :default ""
                 :doc "a JSON object of loopback MCP servers to attach, e.g. {\"unreal\":{\"transport\":\"streamableHttp\",\"url\":\"http://127.0.0.1:8000/mcp\"}}")
  ("timeout_seconds" :integer :default 0
                     :doc "how long to wait, in seconds, at most 1200; 0 uses the folder's setting"))

;;; --- the declaration --------------------------------------------------------------------

(nle:define-cell muse-harness
  (:section ("muse-harness")
    (:guide "binary is the muse executable and needs no setting when it is on PATH; approval_mode is denyUnmatched or allowAll; api_key_env names the variable holding a Meta key, used only for the one pay-as-you-go retry after the subscription allowance is exhausted")
    ("binary" :string :default "muse"
              :doc "the muse executable: a name on PATH, or a path")
    ("model" :string :default "muse-wonder-full-1.0"
             :doc "the Muse model id one turn runs on when the call names none")
    ("approval_mode" :choice :options '("denyUnmatched" "allowAll") :default "denyUnmatched"
                     :doc "Muse's own tool approval: denyUnmatched asks nothing and takes no tool it was not given; allowAll lets the Muse session run its shell and file tools inside the session directory without asking")
    ("timeout_seconds" :integer :default 600 :min 1
                       :doc "how long one turn waits, in seconds")
    ("api_key_env" :env
                   :doc "the environment variable holding a Meta Model API key, used only for the one pay-as-you-go retry; unset stays on the Muse login"))
  (:tool "muse_turn" (nlk:contract 'muse-turn) 'tool-turn)
  (:help :muse "muse_turn: one prompt inside the local Muse Code session, its answer back; /muse" +primer+)
  (:command "muse" 'run-muse
            :description "One prompt inside the local Muse Code session"
            :argument-hint "PROMPT"))
