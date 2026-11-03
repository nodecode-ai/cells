;;;; registry.lisp --- the server table: connect, call, reconnect, stop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One SERVER per configured entry, living for the cell's whole run.
;;;; Two locks per server, taken in this order and never the reverse: the
;;;; CALL-LOCK serializes everything that uses the transport (connect, a
;;;; call, a catalog refresh) and is held for seconds; the STATE-LOCK guards
;;;; the published fields a status line reads and is held for microseconds.
;;;;
;;;; Who connects: one short-lived thread per server at start and on
;;;; restart, holding the call lock for the handshake and the first
;;;; tools/list — START-CELL returns at once, the gateway never waits on a
;;;; server. A call that arrives meanwhile waits for the lock against its
;;;; own deadline and reports "still connecting" if that passes: one
;;;; deadline end to end, never lock-timeout plus request-timeout.
;;;;
;;;; Recovery is the retired Zig client's: a server in :ERROR gets ONE
;;;; reconnect attempt per call, on the caller's thread, inside the call's
;;;; deadline; a timeout keeps the connection (the late reply is skipped as
;;;; stale by the next call); a closed transport drops it. No backoff, no
;;;; supervisor thread — a dead command fails in milliseconds and the model
;;;; reads MCP-OFFLINE with the log path; /mcp restart is the operator's
;;;; lever.
;;;;
;;;; Cancellation: nothing interrupts the evaluating thread except
;;;; (eval-interrupt), which arrives as a SERIOUS-CONDITION the handlers
;;;; here let through. An unwind that leaves mid-frame poisons the stream,
;;;; so the cleanup drops the connection when the write phase was cut.

(in-package #:nodecode-mcp)

(declaim (ftype function sync-tool-functions refresh-primer))

(defparameter *stop-lock-seconds* 2
  "How long STOP waits for a server's call lock before closing from outside.")

(nlk:define-record (server (:copier nil))
  spec
  (state :disconnected)                 ; :disconnected :connecting :ready :error :disabled :refused :stopped
  (error-text nil)
  client
  (tools '() :type list)                ; tool plists (:name :description :schema)
  (symbols '() :type list)              ; generated function symbols
  (call-lock (bt2:make-lock :name "mcp-call"))
  (state-lock (bt2:make-lock :name "mcp-state"))
  (connected-at nil)
  (timing nil)                          ; (CONNECT-MS LIST-MS) of the last connect
  (sign-in nil)                         ; (:url :redirect :held :id :until) while its command waits on a browser
  (settled-p t))                        ; NIL while a connect is publishing

(defparameter *sign-in-grace-seconds* 2
  "How long a call waits for a command held on a browser to answer before it
fails, saying the sign-in still waits.")

(nlk:define-record (registry (:copier nil))
  (servers '() :type list)              ; name order
  (allowlist '() :type list)
  (stopping-p nil))

;;; DEFVAR: a
;;; reload of this file mid-serve must not forget running children.
(defvar *registry* nil
  "The live registry, or NIL when the cell is not running.")

(defun running-registry ()
  "The live registry, or a refusal while the cell is not running."
  (or *registry* (fail "the MCP cell is not running")))

;;; --- state ----------------------------------------------------------------

(defun server-name (server)
  (server-spec-name server.spec))

(defun set-state (server state &optional error-text)
  (bt2:with-lock-held ((server-state-lock server))
    (setf server.state state
          server.error-text error-text))
  state)

(defun server-snapshot (server)
  "The published fields as a plist, read under the state lock."
  (bt2:with-lock-held ((server-state-lock server))
    (let ((spec server.spec))
      (list :name spec.name
            :state server.state
            :error server.error-text
            :tool-count (length server.tools)
            :tools server.tools
            :transport (transport-text spec)
            :log-path (when (eq spec.transport :stdio) (namestring (stderr-log-path spec.name)))
            :timing server.timing
            :sign-in (getf server.sign-in :url)
            :server-info (let ((client server.client)) (and client client.server-info))))))

(defun make-registry-from-section (section)
  (multiple-value-bind (specs allowlist) (parse-mcp-section section)
    (make-registry
     :allowlist allowlist
     :servers (mapcar (lambda (spec)
                        (make-server
                         :spec spec
                         :state (cond ((server-spec-refusal spec) :refused)
                                      ((not (server-spec-enabled-p spec)) :disabled)
                                      (t :disconnected))
                         :error-text spec.refusal))
                      specs))))

(defun registry-server-names (&optional (registry *registry*))
  (and registry (mapcar #'server-name registry.servers)))

(defun find-server (name)
  "The server called NAME, or MCP-UNKNOWN-SERVER naming the configured ones."
  (or (find name (registry-servers (running-registry)) :key #'server-name :test #'string=)
      (error 'mcp-unknown-server
             :detail (format nil "no MCP server named ~s; configured: ~{~a~^, ~}"
                             name (or (registry-server-names) '("none"))))))

;;; --- connect --------------------------------------------------------------

(defun open-client (spec allowlist)
  "A client over a fresh transport to SPEC -- its process started, or its URL
named -- with nothing said on it yet."
  (make-client :transport (ecase spec.transport
                            (:stdio (start-stdio spec allowlist))
                            (:http (make-http-transport
                                    :url spec.url
                                    :headers (resolved-alist spec.headers "headers")
                                    :oauth (spec-binding spec))))))

(defun elapsed-ms (from to)
  "The whole milliseconds between two NOW-SECONDS readings."
  (round (* 1000 (- to from))))

(defun greet (client deadline &aux (start (now-seconds)))
  "The handshake on CLIENT, then its catalog, both before DEADLINE. =>
(values TOOLS CONNECT-MS LIST-MS): the tool plists, how long the server took
to answer the handshake (a process's start-up inside it) and how long to list."
  (handshake client deadline)
  (let ((transport client.transport))
    (when (typep transport 'http-transport)
      (setf (http-transport-protocol-version transport) client.protocol-version)))
  (let* ((shaken (now-seconds))
         (tools (list-tools client deadline)))
    (values tools (elapsed-ms start shaken) (elapsed-ms shaken (now-seconds)))))

(defun close-client (server)
  "Close the server's transport, if any; never signals."
  (setf server.sign-in nil)
  (nlk:when-let (client server.client)
    (setf server.client nil)
    (ignore-errors (transport-close client.transport))))

(defun failure-text (server condition &aux (spec server.spec))
  "CONDITION's text, with the stderr log named for a process."
  ;; A reference that names nothing failed before any process started: no log to read.
  (if (and (eq spec.transport :stdio) (not (typep condition 'reference-unresolved)))
      (let ((detail (princ-to-string condition)) (log (namestring (stderr-log-path spec.name))))
        (if (search log detail) detail (format nil "~a; see ~a" detail log)))
      (princ-to-string condition)))

(defun connect-locked (server &key deadline)
  "Connect SERVER with its call lock held: transport, handshake, catalog."
  ;; T when ready; NIL with the state set to :ERROR otherwise. A handshake
  ;; that fails or times out closes the transport (a child that cannot
  ;; initialize is not kept alive).
  (let* ((spec server.spec)
         (allowlist (if *registry* (registry-allowlist *registry*) +default-inherit-env+))
         (deadline (or deadline (deadline-after spec.timeout-ms))))
    (close-client server)
    (bt2:with-lock-held ((server-state-lock server))
      (setf server.settled-p nil))
    (set-state server :connecting)
    (nlk:with-cleanup ((bt2:with-lock-held ((server-state-lock server))
                         (setf (server-settled-p server) t)))
      (nlk:with-handlers (;; Its command waits on a browser: kept, the one the browser answers.
                          (awaits-sign-in (condition) (note-sign-in server condition) nil)
                          (error (condition)
                            (let ((text (failure-text server condition)))
                              (close-client server)
                              (set-state server :error text)
                              (refresh-primer)
                              nil)))
        ;; The client is published before a word is said on it, so a stop
        ;; from outside closes a handshake that hangs.
        (let ((client (open-client spec allowlist)))
          (setf server.client client)
          (multiple-value-call #'publish-ready server (greet client deadline))
          t)))))

(defun publish-ready (server tools connect-ms list-ms)
  "SERVER connected with TOOLS: its catalog, timing and functions, then
:READY, then the primer -- a reader that sees :ready finds the functions defined."
  (bt2:with-lock-held ((server-state-lock server))
    (setf server.tools tools
          server.connected-at (get-universal-time)
          server.timing (list connect-ms list-ms)
          server.sign-in nil))
  (sync-tool-functions server)
  (set-state server :ready)
  (refresh-primer))

;;; --- a command that signs in on its own -----------------------------------
;;; mcp-remote and its kind hold every request until a browser comes back to
;;; their loopback listener (stdio.lisp sees the address they print). The
;;; process is kept, so the sign-in it started can finish: the server reads
;;; :ERROR with the sign-in named, a call fails at once with the address, and
;;; the request it held is taken up again once its command answers -- the
;;; initialize of a connect, then the catalog. *SIGN-IN-WAIT-SECONDS* on, a
;;; call starts it over on a fresh process.

(defun note-sign-in (server condition &aux (name (server-name server))
                                           (method (awaits-sign-in-method condition)))
  "SERVER's command waits on a browser (CONDITION): publish :ERROR naming the
sign-in and keep what a call takes up again."
  (bt2:with-lock-held ((server-state-lock server))
    (setf server.sign-in (list :url (awaits-sign-in-url condition)
                               :redirect (awaits-sign-in-redirect condition)
                               ;; What the next answer finishes: the handshake, the catalog or a call.
                               :held (cond ((equal method "initialize") :initialize)
                                           ((equal method "tools/list") :catalog)
                                           (t :call))
                               :id (awaits-sign-in-request-id condition)
                               :until (+ (now-seconds) *sign-in-wait-seconds*))
          server.state :error
          server.error-text (format nil "~a needs a sign-in in a browser: /mcp login ~a" name name)))
  (refresh-primer))

(defun pending-sign-in-p (server &aux (pending server.sign-in) (client server.client))
  "True while SERVER's command still waits on the browser it asked for."
  (and pending client
       (< (now-seconds) (getf pending :until))
       (let ((transport client.transport))
         (and (typep transport 'stdio-transport)
              (not (stdio-transport-closed-p transport))
              (not (child-exited transport))))))

(defun sign-in-text (server &aux (name (server-name server)) (pending server.sign-in))
  "What a call held on SERVER's sign-in says: the address to open, whole."
  (format nil "~a needs a sign-in in a browser: open ~a and approve it. Its command waits for the answer at ~a~
~:[~;, and the call it held goes through once it is signed in~]. From a browser on another machine, paste the ~
address it lands on: /mcp login ~a ADDRESS"
          name (getf pending :url) (or (getf pending :redirect) "its own listener")
          (eq (getf pending :held) :call) name))

(defun resume-sign-in (server deadline &key (wait *sign-in-grace-seconds*)
                       &aux (client server.client) (pending server.sign-in) (start (now-seconds)))
  "Take up what SERVER's command held for its sign-in, waiting at most WAIT
seconds for it to answer: T once SERVER is ready, NIL while it still waits.
Call with SERVER's call lock held."
  (handler-case
      (let ((answer-by (min deadline (+ start wait))))
        (ecase (getf pending :held)
          (:initialize
           (handshake client deadline (rpc-await-reply client (getf pending :id) "initialize" answer-by))
           (let ((shaken (now-seconds)))
             (publish-ready server (list-tools client deadline)
                            (elapsed-ms start shaken) (elapsed-ms shaken (now-seconds)))))
          (:catalog
           (publish-ready server (list-tools client deadline) (first server.timing) (elapsed-ms start (now-seconds))))
          ;; A call held: its late answer is skipped; the next call finds out.
          (:call
           (bt2:with-lock-held ((server-state-lock server)) (setf server.sign-in nil))
           (set-state server :ready)
           (refresh-primer)))
        t)
    (rpc-timeout () nil)
    (awaits-sign-in (condition) (note-sign-in server condition) nil)))

(defun spawn-connect (server)
  "Connect SERVER on its own short-lived thread."
  ;; Returns the thread, or NIL when the registry is stopping or the server
  ;; cannot connect at all.
  (when (and *registry*
             (not (registry-stopping-p *registry*))
             (not (member server.state '(:disabled :refused :stopped))))
    (let ((name (server-name server))
          (lock server.call-lock)
          (timeout (/ (server-spec-timeout-ms server.spec) 1000)))
      (nlk:spawn (format nil "mcp-connect-~a" name)
        ;; WITH-LOCK-HELD runs nothing and answers NIL when the lock does not come in time.
        (unless (bt2:with-lock-held (lock :timeout timeout) (connect-locked server) t)
          (warn "mcp ~a: busy, connect skipped" name))))))

(defun drop-connection (server detail)
  "The transport is unusable: close it, publish :ERROR, keep the catalog
the model last saw (its functions answer MCP-OFFLINE until a reconnect)."
  (close-client server)
  (set-state server :error (failure-text server detail))
  (refresh-primer))

;;; --- the call -------------------------------------------------------------

(defun call-tool (server tool-name arguments &key timeout-seconds)
  "tools/call on SERVER: the raw result object."
  ;; The whole sequence runs under the call lock against one deadline; every
  ;; failure maps to the MCP-* family the model reads.
  (let* ((name (server-name server))
         (seconds (or timeout-seconds (/ (server-spec-timeout-ms server.spec) 1000)))
         (deadline (deadline-after (* 1000 seconds)))
         (lock server.call-lock)
         (phase :idle))
    (unless (bt2:acquire-lock lock :timeout (seconds-remaining deadline))
      (error 'mcp-timeout :server name
                          :detail "still connecting (or busy with another call); retry"))
    (nlk:with-cleanup ((when (eq phase :writing)
                         (ignore-errors (drop-connection server "interrupted while writing a frame")))
                       (bt2:release-lock lock))
      (prog1
          (handler-case
              (let* (;; A ready server's client, after one reconnect for one that fell over.
                     (client (flet ((offline (detail)
                                      (error 'mcp-offline :server name :detail detail)))
                               (case server.state
                                 (:ready server.client)
                                 ((:error :disconnected :connecting)
                                  (cond ((pending-sign-in-p server)
                                         (if (resume-sign-in server deadline)
                                             server.client
                                             (offline (sign-in-text server))))
                                        ((connect-locked server :deadline deadline) server.client)
                                        ((pending-sign-in-p server) (offline (sign-in-text server)))
                                        (t (offline (or server.error-text "connect failed")))))
                                 (:disabled (offline "disabled in config (enabled: false)"))
                                 (:refused (offline (or server.error-text "refused config")))
                                 (:stopped (offline "the cell is stopped"))
                                 (t (offline (format nil "state ~a" server.state))))))
                     (params (nlk:json-object "name" tool-name
                                              "arguments" (or arguments (nlk:make-json-object)))))
                (setf phase :writing)
                (let ((id (rpc-send-request client "tools/call" params deadline)))
                  (setf phase :reading)
                  (rpc-await-reply client id "tools/call" deadline)))
            (mcp-error (condition) (error condition))
            ;; Its command holds the call for a browser: kept, failed now with the address.
            (awaits-sign-in (condition)
              (setf phase :idle)
              (note-sign-in server condition)
              (error 'mcp-offline :server name :detail (sign-in-text server)))
            (rpc-timeout (condition)
              (setf phase :idle)
              (error 'mcp-timeout
                     :server name
                     :detail (format nil "~a (~a s); the connection is kept and the late reply will be skipped"
                                     condition seconds)))
            (rpc-error (condition)
              (setf phase :idle)
              (error 'mcp-tool-error :server name :tool tool-name
                                     :detail (princ-to-string condition)))
            (transport-closed (condition)
              (setf phase :idle)
              (drop-connection server condition)
              (error 'mcp-offline :server name
                                  :detail server.error-text))
            (error (condition)
              (setf phase :idle)
              (fail "~a/~a: ~a" name tool-name condition)))
        (setf phase :idle)
        (let ((client server.client))
          ;; A relist failure keeps the old catalog; the connection's fate is the failure's.
          (when (and client client.list-changed-p)
            (nlk:with-handlers ((transport-closed (condition) (drop-connection server condition))
                                (error (condition)
                                  (warn "mcp ~a: tools/list after list_changed failed: ~a"
                                        (server-name server) condition)))
              (relist server client (deadline-after (server-spec-timeout-ms server.spec))))))))))

(defun relist (server client deadline &aux (start (now-seconds)))
  "List SERVER's catalog again on CLIENT, its live connection, before
DEADLINE, and publish it: the tools, their functions, the primer. => (values
TOOLS LIST-MS), the listing's own milliseconds."
  (let* ((tools (list-tools client deadline))
         (list-ms (elapsed-ms start (now-seconds))))
    (bt2:with-lock-held ((server-state-lock server)) (setf server.tools tools))
    (sync-tool-functions server)
    (refresh-primer)
    (values tools list-ms)))

;;; --- a test ---------------------------------------------------------------
;;; What the page's Test answers: whether a server works, what it offers, how
;;; long it took to answer. A configured server is tested on its live
;;; connection -- a ready one lists its catalog again on the connection the
;;; model's calls use, one that is not ready gets the reconnect a call would
;;; make -- under its call lock, against its own deadline. A server not yet
;;; added is PROBEd on a connection of its own, closed before the answer
;;; whatever happened, so a test never leaves a process behind. Either answers
;;; a plist: :OK, :REUSED, :CONNECT-MS, :LIST-MS, :TOOLS and :SERVER-INFO; or
;;; :ERROR, the failure as the client met it, and :SAID, what the process
;;; wrote to its stderr meanwhile -- a server's own words for why it did not
;;; start.

(defun log-mark (spec)
  "Where SPEC's stderr log ends now, in octets, which LOG-SINCE reads after;
NIL for a server that is not a process."
  (when (eq spec.transport :stdio)
    (or (ignore-errors (with-open-file (in (stderr-log-path spec.name) :element-type '(unsigned-byte 8))
                         (file-length in)))
        0)))

(defun log-since (spec mark &key (lines 12))
  "The last LINES lines SPEC's process wrote to its stderr log after MARK --
the launch stamps this cell writes left out -- or NIL when it wrote none."
  (let* ((text (and mark
                    (ignore-errors
                     (with-open-file (in (stderr-log-path spec.name) :element-type '(unsigned-byte 8))
                       (file-position in mark)
                       (let ((octets (make-array (- (file-length in) mark) :element-type '(unsigned-byte 8))))
                         (read-sequence octets in)
                         (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement #\?)))))))
         (said (remove-if (lambda (line)
                            (or (zerop (length (string-trim " " line)))
                                (uiop:string-prefix-p ";; mcp " line)))
                          (nlk:lines text))))
    (and said (nlk:clip (format nil "~{~a~^~%~}" (last said lines)) 2000))))

(defun server-allowlist ()
  "The environment a child inherits, as START-CELL reads it: the running
registry's, else the section's as config.jsonc says it now."
  (if *registry*
      (registry-allowlist *registry*)
      (nth-value 1 (parse-mcp-section (nlk:json-value (nle:read-shared-config) :object "mcp")))))

(defun probe (spec &aux (deadline (deadline-after spec.timeout-ms)) (mark (log-mark spec)) (client nil))
  "Try SPEC, a server entry that need not be configured, on a connection of
its own: connect, list, close. => the test plist."
  (let ((outcome (nlk:with-cleanup ((when client (ignore-errors (transport-close client.transport))))
                   (handler-case
                       (progn
                         (setf client (open-client spec (server-allowlist)))
                         (multiple-value-bind (tools connect-ms list-ms) (greet client deadline)
                           (list :ok t :connect-ms connect-ms :list-ms list-ms :tools tools
                                 :server-info client.server-info)))
                     (error (condition) (list :error (princ-to-string condition)))))))
    ;; Read after the close, so a process that was still writing has finished.
    (if (getf outcome :ok)
        outcome
        (list* :said (log-since spec mark) outcome))))

(defun test-server (server &aux (spec server.spec) (deadline (deadline-after spec.timeout-ms))
                                (mark (log-mark spec)) (lock server.call-lock))
  "Test SERVER, a configured one, on its live connection; one turned off in
config.jsonc is PROBEd instead, and stays off. => the test plist."
  (case server.state
    (:refused (list :error (or server.error-text "refused by its entry in config.jsonc")))
    (:disabled (probe spec))
    (t
     (if (not (bt2:acquire-lock lock :timeout (seconds-remaining deadline)))
         (list :error "busy past its deadline with a call or a connect; test it again in a moment")
         (let ((outcome
                 (nlk:with-cleanup ((bt2:release-lock lock))
                   (handler-case
                       (cond
                         ((eq server.state :ready)
                          (multiple-value-bind (tools list-ms) (relist server server.client deadline)
                            (list :ok t :reused t :connect-ms (first server.timing)
                                  :list-ms list-ms :tools tools
                                  :server-info (client-server-info server.client))))
                         ((if (pending-sign-in-p server)
                              (resume-sign-in server deadline)
                              (connect-locked server :deadline deadline))
                          (list :ok t :connect-ms (first server.timing) :list-ms (second server.timing)
                                :tools server.tools :server-info (client-server-info server.client)))
                         ((pending-sign-in-p server) (list :error (sign-in-text server)))
                         (t (list :error server.error-text)))
                     (awaits-sign-in (condition)
                       (note-sign-in server condition)
                       (list :error (sign-in-text server)))
                     ;; A deadline keeps the connection, as a call's does; a closed one is dropped.
                     (transport-closed (condition)
                       (drop-connection server condition)
                       (list :error server.error-text))
                     (error (condition) (list :error (princ-to-string condition)))))))
           (if (getf outcome :ok)
               outcome
               (list* :said (log-since spec mark) outcome)))))))
