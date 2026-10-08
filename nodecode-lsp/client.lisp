;;;; client.lisp --- one running language server, from spawn to stop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A server is keyed by its name and the root it runs in, so a monorepo
;;;; gets one per package root and every session working there shares it:
;;;; the gateway is one long-lived process, which is what omp's mux daemon
;;;; existed to give it. A server starts on its own thread the first time a
;;;; file of its is written or asked about, so no caller waits longer than
;;;; its own budget for a slow initialize; one that failed is answered failed
;;;; for +RETRY-SECONDS+, then tried again. A crash is noticed by the reader
;;;; at end of file and the next use respawns it.
;;;;
;;;; The reader thread keeps what the server publishes -- diagnostics by
;;;; path, with a counter every publish advances, and the $/progress tokens
;;;; that say it is still loading -- and answers what it asks. A document is
;;;; synced from disk, whole, under a lock of its own that the reader never
;;;; takes: a writer blocked on a full pipe must never hold what the reader
;;;; needs to drain the other one.
;;;;
;;;; Stop asks politely (shutdown, exit), then kills the process group, closes
;;;; the write side, joins the reader and reaps the child: posix_spawn reaps
;;;; nothing on its own. Every server stops in parallel under one bound,
;;;; because the gateway stops cells one after another. Should the organism
;;;; die by SIGKILL, each server reads end of file on stdin -- the organism
;;;; held the only copy -- and its processId names a process that is gone.

(in-package #:nodecode-lsp)

(defparameter +start-seconds+ 30
  "How long initialize may take before the server is failed.")

(defparameter +retry-seconds+ 30
  "How long a failed server is answered failed before it is started again.")

(defparameter +settle-seconds+ 0.25
  "How long an unversioned publish must stand before it is the answer.")

(defparameter +cold-seconds+ 12
  "How long after start an empty unversioned publish is taken for a
placeholder rather than an answer (omp's deferred wait).")

(defparameter +loaded-seconds+ 15
  "How long after start a server counts as loaded whatever its progress says.")

(defparameter +quiet-seconds+ 2
  "How long a server that reports no progress at all must have run to count as loaded.")

(defparameter +quiescent-seconds+ 120
  "How long after start a server that reports its status counts as loaded
even while it says it is busy.")

(defstruct (server (:conc-name srv-) (:copier nil))
  "One language server process in one root, and what it has told us."
  (name "" :type string)
  (root "" :type string)
  spec argv
  (state :starting)                     ; :starting :ready :failed :stopped
  failure failed-at reported
  conn capabilities
  (encoding :utf-16)
  (lock (bt2:make-lock :name "lsp server"))
  (cv (bt2:make-condition-variable))
  (doc-lock (bt2:make-lock :name "lsp documents"))
  (published (make-hash-table :test 'equal)) ; path -> (COUNTER VERSION ITEMS TIME)
  (counter 0)
  (documents (make-hash-table :test 'equal)) ; path -> (VERSION . TEXT)
  (progress (make-hash-table :test 'equal))  ; active $/progress tokens
  progress-seen progress-ended
  status-seen quiescent                       ; experimental/serverStatus
  (registrations (make-hash-table :test 'equal)) ; id -> method
  (started (get-internal-real-time))
  (used (get-internal-real-time))
  log exit)

(defvar *servers* '()
  "Every server the cell holds, running, starting or recently failed.")

(defvar *servers-lock* (bt2:make-lock :name "lsp servers"))

;;; --- state ------------------------------------------------------------------

(defun note-state (server state &optional failure)
  "Move SERVER to STATE, keeping the first FAILURE said; a stop is final."
  (bt2:with-lock-held ((srv-lock server))
    (unless (eq :stopped (srv-state server))
      (setf (srv-state server) state)
      (when (and failure (null (srv-failure server)))
        (setf (srv-failure server) failure
              (srv-failed-at server) (get-internal-real-time))))
    (bt2:condition-broadcast (srv-cv server))))

(defun await-state (server deadline)
  "SERVER's state once it is no longer starting, or :STARTING at DEADLINE."
  (bt2:with-lock-held ((srv-lock server))
    (loop (let ((left (seconds-left deadline)))
            (unless (and (eq :starting (srv-state server)) (plusp left))
              (return (srv-state server)))
            (bt2:condition-wait (srv-cv server) (srv-lock server) :timeout (min 1 left))))))

(defun loaded-p (server)
  "Whether SERVER has loaded its project. One that reports its status
(rust-analyzer's experimental/serverStatus) is loaded once it says it is
quiescent; any other once no progress runs and one has ended, or none ever
began and +QUIET-SECONDS+ have gone, or +LOADED-SECONDS+ have gone at all."
  ;; omp polls rust-analyzer/analyzerStatus for the same answer; the status
  ;; notification is the server saying it, and any server may send it.
  (let ((age (seconds-since (srv-started server))))
    (if (srv-status-seen server)
        (or (srv-quiescent server) (> age +quiescent-seconds+))
        (or (> age +loaded-seconds+)
            (and (zerop (hash-table-count (srv-progress server)))
                 (or (srv-progress-ended server)
                     (and (not (srv-progress-seen server)) (> age +quiet-seconds+))))))))

(defun await-loaded (server deadline)
  "Whether SERVER is loaded by DEADLINE."
  (bt2:with-lock-held ((srv-lock server))
    (loop (when (loaded-p server) (return t))
          (let ((left (seconds-left deadline)))
            (unless (plusp left) (return nil))
            (bt2:condition-wait (srv-cv server) (srv-lock server) :timeout (min 0.25 left))))))

(defun touch (server)
  (setf (srv-used server) (get-internal-real-time))
  server)

;;; --- start -------------------------------------------------------------------

(defun log-path (name)
  "Where server NAME's stderr is kept."
  (nlk:home (format nil "lsp/~a.log" name)))

(defun log-tail (path)
  "The last lines PATH holds, at most three and 300 characters, as one line."
  (let ((text (ignore-errors
               (with-open-file (in path :element-type '(unsigned-byte 8))
                 (let* ((size (file-length in))
                        (bytes (make-array (min size 2048) :element-type '(unsigned-byte 8))))
                   (file-position in (- size (length bytes)))
                   (read-sequence bytes in)
                   (sb-ext:octets-to-string bytes :external-format (list :utf-8 :replacement +replacement+)))))))
    (when text
      (let ((lines (remove-if (lambda (line) (or (zerop (length (string-trim " " line)))
                                                 (uiop:string-prefix-p ";; lsp " line)))
                              (uiop:split-string text :separator '(#\Newline)))))
        (nlk:one-line (format nil "~{~a~^ | ~}" (last lines 3)) :cap 300)))))

(defun workspace-folder (server)
  (let ((root (string-right-trim "/" (srv-root server))))
    (nlk:json-object "uri" (path-uri root) "name" (file-name root))))

(defun client-capabilities ()
  "What this client tells a server it can take: omp's set, less the code
actions, formatting and file operations v1 does not ask for, plus both
position encodings."
  (let ((kinds (coerce (loop for kind from 1 to 26 collect kind) 'vector)))
    (nlk:json-object
     "general" (nlk:json-object "positionEncodings" (vector "utf-8" "utf-16"))
     "textDocument"
     (nlk:json-object
      "synchronization" (nlk:json-object "didSave" t "dynamicRegistration" nil
                                         "willSave" nil "willSaveWaitUntil" nil)
      "hover" (nlk:json-object "contentFormat" (vector "markdown" "plaintext") "dynamicRegistration" nil)
      "definition" (nlk:json-object "dynamicRegistration" nil "linkSupport" t)
      "references" (nlk:json-object "dynamicRegistration" nil)
      "documentSymbol" (nlk:json-object "dynamicRegistration" nil
                                        "hierarchicalDocumentSymbolSupport" t
                                        "symbolKind" (nlk:json-object "valueSet" kinds))
      "rename" (nlk:json-object "dynamicRegistration" nil "prepareSupport" nil)
      "publishDiagnostics" (nlk:json-object "relatedInformation" t "versionSupport" t
                                            "tagSupport" (nlk:json-object "valueSet" (vector 1 2))
                                            "codeDescriptionSupport" t "dataSupport" t)
      "diagnostic" (nlk:json-object "dynamicRegistration" t))
     "window" (nlk:json-object "workDoneProgress" t)
     "experimental" (nlk:json-object "serverStatusNotification" t)
     "workspace" (nlk:json-object
                  "applyEdit" t
                  "workspaceEdit" (nlk:json-object "documentChanges" t "failureHandling" "abort")
                  "configuration" t
                  "workspaceFolders" t
                  "symbol" (nlk:json-object "dynamicRegistration" nil
                                            "symbolKind" (nlk:json-object "valueSet" kinds))))))

(defun initialize-params (server)
  (let ((spec (srv-spec server)))
    (nlk:json-object
     "processId" (sb-posix:getpid)
     "clientInfo" (nlk:json-object "name" "nodecode")
     "rootUri" (path-uri (string-right-trim "/" (srv-root server)))
     "rootPath" (string-right-trim "/" (srv-root server))
     "capabilities" (client-capabilities)
     "initializationOptions" (or (spec-init-options spec) (nlk:make-json-object))
     "workspaceFolders" (vector (workspace-folder server)))))

(defun server-for (spec root argv)
  "The server SPEC runs in ROOT with ARGV: the one running or starting, a
recent failure, or a new one starting now on a thread of its own."
  (bt2:with-lock-held (*servers-lock*)
    (let ((found (find-if (lambda (server)
                            (and (string= (srv-name server) (spec-name spec))
                                 (string= (srv-root server) root)))
                          *servers*)))
      (cond ((and found (member (srv-state found) '(:starting :ready)))
             (touch found))
            ((and found (eq :failed (srv-state found))
                  (< (seconds-since (srv-failed-at found)) +retry-seconds+))
             found)
            (t (when found
                 (setf *servers* (remove found *servers*)))
               (let ((server (make-server :name (spec-name spec) :root root :spec spec :argv argv)))
                 (push server *servers*)
                 (nlk:spawn (format nil "lsp start ~a" (spec-name spec)) (start-server server))
                 server))))))

(defun start-server (server)
  "Spawn SERVER's process and shake hands; any failure is the server's FAILURE."
  (handler-case
      (let ((log (log-path (srv-name server))))
        (ensure-directories-exist log)
        (with-open-file (out log :direction :output :if-exists :append :if-does-not-exist :create
                                 :external-format :utf-8)
          (format out "~&;; lsp ~a: ~{~a~^ ~} in ~a, ~a~%" (srv-name server) (srv-argv server)
                  (srv-root server) (nlk:iso-time (nlk:unix-now) :millis nil)))
        (setf (srv-log server) log)
        (let ((conn (open-connection (srv-argv server)
                                     :directory (srv-root server) :error-log log
                                     :label (srv-name server)
                                     :on-request (lambda (method params) (server-request server method params))
                                     :on-notification (lambda (method params)
                                                        (server-notification server method params))
                                     :on-close (lambda () (server-closed server))))
              (stopped nil))
          (bt2:with-lock-held ((srv-lock server))
            (setf (srv-conn server) conn
                  stopped (eq :stopped (srv-state server))))
          (when stopped
            (return-from start-server (stop-process server)))
          (multiple-value-bind (result state detail)
              (call conn "initialize" (initialize-params server) :seconds +start-seconds+)
            (unless (eq state :ok)
              (error "initialize ~a~@[: ~a~]"
                     (case state (:timeout "timed out") (:closed "failed: the server exited") (t "was refused"))
                     detail))
            (let* ((capabilities (nlk:json-value result :object "capabilities"))
                   (encoding (nlk:json-value capabilities :string "positionEncoding")))
              (setf (srv-capabilities server) capabilities
                    (srv-encoding server) (cond ((equal encoding "utf-8") :utf-8)
                                                ((equal encoding "utf-32") :utf-32)
                                                (t :utf-16)))))
          (notify conn "initialized" (nlk:make-json-object))
          (notify conn "workspace/didChangeConfiguration"
                  (nlk:json-object "settings" (or (spec-settings (srv-spec server)) (nlk:make-json-object))))
          (note-state server :ready)))
    (error (condition)
      (note-state server :failed (failure-text server condition))
      (stop-process server))))

(defun failure-text (server condition)
  "Why SERVER failed: CONDITION, and what it last printed."
  (let ((tail (and (srv-log server) (log-tail (srv-log server)))))
    (format nil "~a~@[; it printed: ~a~]" (nlk:one-line (princ-to-string condition) :cap 300) tail)))

;;; --- stop ------------------------------------------------------------------------

(defun reap (server)
  "SERVER's exit status once its child has exited, reaping it then; else NIL."
  (bt2:with-lock-held ((srv-lock server))
    (or (srv-exit server)
        (let ((conn (srv-conn server)))
          (and conn (conn-pid conn)
               (setf (srv-exit server) (nlk:child-exit (conn-pid conn) (conn-process conn))))))))

(defun await-exit (server seconds)
  (loop with deadline = (deadline-after seconds)
        until (or (reap server) (zerop (seconds-left deadline)))
        do (sleep 0.05))
  (srv-exit server))

(defun stop-process (server &key graceful)
  "End SERVER's process: GRACEFUL asks first, then the group is killed, the
write side closed, the reader joined and the child reaped."
  (let ((conn (bt2:with-lock-held ((srv-lock server)) (srv-conn server))))
    (when conn
      (when graceful
        (ignore-errors (call conn "shutdown" nil :seconds 1))
        (ignore-errors (notify conn "exit"))
        (await-exit server 0.5))
      (unless (srv-exit server)
        (nlk:kill-tree (conn-pid conn)))
      (close-connection conn)
      (ignore-errors
       (sb-thread:join-thread (bt2:thread-native-thread (conn-reader conn)) :default nil :timeout 1))
      (await-exit server 1))
    t))

(defun stop-server (server)
  "Stop SERVER for good."
  (let ((was (bt2:with-lock-held ((srv-lock server))
               (prog1 (srv-state server)
                 (setf (srv-state server) :stopped)
                 (bt2:condition-broadcast (srv-cv server))))))
    (stop-process server :graceful (eq was :ready))))

(defun stop-servers (servers &key (seconds 3))
  "Stop SERVERS, all at once, waiting at most SECONDS for the lot."
  (let ((threads (mapcar (lambda (server)
                           (nlk:spawn (format nil "lsp stop ~a" (srv-name server)) (stop-server server)))
                         servers))
        (deadline (deadline-after seconds)))
    (dolist (thread threads)
      (ignore-errors
       (sb-thread:join-thread (bt2:thread-native-thread thread)
                              :default nil :timeout (max 0.01 (seconds-left deadline)))))
    (length servers)))

(defun stop-all-servers ()
  "Stop every server the cell holds."
  (stop-servers (bt2:with-lock-held (*servers-lock*) (shiftf *servers* '()))))

(defun reap-idle ()
  "Stop every server unused for idle_minutes."
  (let* ((limit (* 60 (setting :idle-minutes)))
         (idle (bt2:with-lock-held (*servers-lock*)
                 (let ((idle (remove-if-not (lambda (server)
                                              (and (eq :ready (srv-state server))
                                                   (> (seconds-since (srv-used server)) limit)))
                                            *servers*)))
                   (setf *servers* (set-difference *servers* idle))
                   idle))))
    (when idle
      (stop-servers idle))))

(defun server-closed (server)
  "The reader reached end of file: unless SERVER is being stopped, it died."
  (unless (eq :stopped (srv-state server))
    (unless (await-exit server 1)
      (nlk:kill-tree (conn-pid (srv-conn server)))
      (await-exit server 1))
    (note-state server :failed
                (format nil "exited~@[ with status ~a~]~@[; it printed: ~a~]"
                        (srv-exit server) (and (srv-log server) (log-tail (srv-log server)))))))

;;; --- what the server says and asks -----------------------------------------------

(defun server-notification (server method params)
  (cond ((string= method "textDocument/publishDiagnostics")
         (let ((path (uri-path (nlk:json-value params :string "uri"))))
           (when path
             (bt2:with-lock-held ((srv-lock server))
               (setf (gethash path (srv-published server))
                     (list (incf (srv-counter server))
                           (nlk:json-value params :integer "version")
                           (nlk:json-array params "diagnostics")
                           (get-internal-real-time)))
               (bt2:condition-broadcast (srv-cv server))))))
        ((string= method "experimental/serverStatus")
         (bt2:with-lock-held ((srv-lock server))
           (setf (srv-status-seen server) t
                 (srv-quiescent server) (nlk:json-value params :boolean "quiescent"))
           (bt2:condition-broadcast (srv-cv server))))
        ((string= method "$/progress")
         (let ((token (princ-to-string (gethash "token" params)))
               (kind (nlk:json-value params :string "value" "kind")))
           (bt2:with-lock-held ((srv-lock server))
             (cond ((equal kind "begin")
                    (setf (gethash token (srv-progress server)) t
                          (srv-progress-seen server) t))
                   ((equal kind "end")
                    (remhash token (srv-progress server))
                    (when (zerop (hash-table-count (srv-progress server)))
                      (setf (srv-progress-ended server) t))))
             (bt2:condition-broadcast (srv-cv server)))))))

(defun setting-section (settings section)
  "What SETTINGS holds for SECTION: the whole of it for none, a member named
SECTION, else the dotted path SECTION walks."
  (cond ((null settings) nil)
        ((or (null section) (string= section "")) settings)
        (t (multiple-value-bind (value present) (gethash section settings)
             (if present
                 value
                 (apply #'nlk:json-value settings :any (uiop:split-string section :separator ".")))))))

(defparameter +null-answered+
  '("window/workDoneProgress/create" "window/showMessageRequest"
    "workspace/semanticTokens/refresh" "workspace/inlayHint/refresh" "workspace/inlineValue/refresh"
    "workspace/codeLens/refresh" "workspace/diagnostic/refresh" "workspace/foldingRange/refresh")
  "Server requests answered with null: acknowledged, nothing to do.")

(defun server-request (server method params)
  "Answer the server's request METHOD; signals RPC-REFUSAL for one not served."
  (cond ((string= method "workspace/configuration")
         (map 'vector (lambda (item)
                        (or (setting-section (spec-settings (srv-spec server))
                                             (nlk:json-value item :string "section"))
                            :null))
              (nlk:json-array params "items")))
        ((string= method "workspace/workspaceFolders")
         (vector (workspace-folder server)))
        ((member method +null-answered+ :test #'string=)
         nil)
        ((string= method "client/registerCapability")
         (bt2:with-lock-held ((srv-lock server))
           (loop for registration across (nlk:json-array params "registrations")
                 for id = (nlk:json-value registration :string "id")
                 when id
                   do (setf (gethash id (srv-registrations server))
                            (nlk:json-value registration :string "method"))))
         nil)
        ((string= method "client/unregisterCapability")
         (bt2:with-lock-held ((srv-lock server))
           ;; The spec's own spelling, and the corrected one.
           (loop for registration across (or (nlk:json-value params :array "unregisterations")
                                             (nlk:json-array params "unregistrations"))
                 for id = (nlk:json-value registration :string "id")
                 when id do (remhash id (srv-registrations server))))
         nil)
        ((string= method "workspace/applyEdit")
         (handler-case (progn (apply-workspace-edit (nlk:json-value params :object "edit")
                                                    (srv-encoding server))
                              (nlk:json-object "applied" t))
           (error (condition)
             (nlk:json-object "applied" nil "failureReason" (princ-to-string condition)))))
        ((string= method "window/showDocument")
         (nlk:json-object "success" nil))
        (t (error 'rpc-refusal :code -32601 :message (format nil "~a is not supported" method)))))

(defun pulls-diagnostics-p (server)
  "Whether SERVER answers textDocument/diagnostic, declared or registered."
  (or (nlk:json-value (srv-capabilities server) :any "diagnosticProvider")
      (bt2:with-lock-held ((srv-lock server))
        (loop for method being the hash-values of (srv-registrations server)
              thereis (equal method "textDocument/diagnostic")))))

;;; --- documents -------------------------------------------------------------------

(defun text-document (path)
  (nlk:json-object "uri" (path-uri path)))

(defun send-save (server path text)
  "didSave, when SERVER asked for saves and has loaded its project, with the
text when it asked for that."
  ;; Never before the project loaded: rust-analyzer 1.90 panics on a save of
  ;; a file its workspace does not hold yet ("Unable to get
  ;; FileSourceRootInput ... this is a bug"), and exits.
  (let ((save (nlk:json-value (srv-capabilities server) :any "textDocumentSync" "save")))
    (when (and save (loaded-p server))
      (notify (srv-conn server) "textDocument/didSave"
              (nlk:json-object "textDocument" (text-document path)
                               :when (nlk:json-value save :boolean "includeText") "text" text)))))

(defun sync-document (server path)
  "Bring SERVER's copy of PATH to what is on disk, whole.
=> (values VERSION CHANGED): the version it now has, NIL once the file is
gone (closed if it was open); CHANGED when something was sent."
  (let ((text (read-file path))
        (conn (srv-conn server)))
    (bt2:with-lock-held ((srv-doc-lock server))
      (let ((open (gethash path (srv-documents server))))
        (cond ((null text)
               (when open
                 (remhash path (srv-documents server))
                 (notify conn "textDocument/didClose" (nlk:json-object "textDocument" (text-document path))))
               (values nil (and open t)))
              ((null open)
               (notify conn "textDocument/didOpen"
                       (nlk:json-object "textDocument"
                                        (nlk:json-object "uri" (path-uri path)
                                                         "languageId" (language-id (srv-spec server) path)
                                                         "version" 1 "text" text)))
               ;; No save after an open: the text the server was handed is the file's.
               (setf (gethash path (srv-documents server)) (cons 1 text))
               (values 1 t))
              ((string= text (cdr open))
               (values (car open) nil))
              (t (let ((version (1+ (car open))))
                   (notify conn "textDocument/didChange"
                           (nlk:json-object "textDocument" (nlk:json-object "uri" (path-uri path)
                                                                            "version" version)
                                            "contentChanges" (vector (nlk:json-object "text" text))))
                   (setf (gethash path (srv-documents server)) (cons version text))
                   (send-save server path text)
                   (values version t))))))))

(defun refresh-documents (server &optional except)
  "Sync every document SERVER holds open but EXCEPT from disk, so what it
answers is about the files as they are."
  (dolist (path (bt2:with-lock-held ((srv-doc-lock server))
                  (alexandria:hash-table-keys (srv-documents server))))
    (unless (equal path except)
      (sync-document server path))))

;;; --- diagnostics, waited for ---------------------------------------------------------
;;; omp's waitForDiagnostics, as one polling loop over every (server, file)
;;; pair a call asks about, so servers answer in parallel without a thread
;;; each. A pull (textDocument/diagnostic) is asked once when the server
;;; serves it; otherwise the answer is the first publish after our sync that
;;; names the version we sent, or an unversioned one that stood unchanged
;;; +SETTLE-SECONDS+.
;;; An empty publish before the project loaded is not trusted, nor is an empty
;;; unversioned one from a server younger than +COLD-SECONDS+: both are often
;;; a placeholder before analysis.

(defstruct (job (:copier nil))
  "One file's diagnostics from one server, being waited for."
  server path (phase :start) since version changed pull items)

(defun cold-placeholder-p (server version items)
  (and (null version) (zerop (length items))
       (< (seconds-since (srv-started server)) +cold-seconds+)))

(defun published-answer (job)
  "What SERVER has published for the job's file that answers it, or NIL."
  (let* ((server (job-server job))
         (entry (bt2:with-lock-held ((srv-lock server))
                  (gethash (job-path job) (srv-published server)))))
    (when entry
      (destructuring-bind (counter version items time) entry
        (let ((trusted (or (plusp (length items))
                           (and (loaded-p server) (not (cold-placeholder-p server version items))))))
          (cond ((not (job-changed job))
                 ;; Nothing was sent: what stands is about the text as it is.
                 (and (or (null version) (eql version (job-version job))) items))
                ((<= counter (job-since job)) nil)
                ((and version (eql version (job-version job)) trusted) items)
                ;; A versioned publish is about the version it names, never ours.
                ((and (null version) (>= (seconds-since time) +settle-seconds+) trusted) items)))))))

(defun send-pull (job)
  "Ask the job's server for the file's diagnostics, the answer coming to the job."
  (setf (job-pull job)
        (request-async (srv-conn (job-server job)) "textDocument/diagnostic"
                       (nlk:json-object "textDocument" (text-document (job-path job))))))

(defun pulled-answer (job)
  "The pull's items once it answered in full, NIL while it has not. An empty
answer before the project loaded is asked again once it has (rust-analyzer
answers at once with nothing while it loads); a pull that failed is dropped,
and the publish answers instead."
  (let ((reply (job-pull job))
        (server (job-server job)))
    (cond ((eq reply :again)
           (when (loaded-p server) (send-pull job))
           nil)
          ((null reply) nil)
          (t (case (reply-state reply)
               (:ok (let* ((result (reply-result reply))
                           (items (nlk:json-array result "items")))
                      (cond ((not (equal "full" (nlk:json-value result :string "kind")))
                             (setf (job-pull job) nil) nil)
                            ((or (plusp (length items)) (loaded-p server)) items)
                            (t (setf (job-pull job) :again) nil))))
               (:waiting nil)
               (t (setf (job-pull job) nil) nil))))))

(defun job-step (job)
  "Advance JOB once. => T when it has its answer or never will."
  (let ((server (job-server job)))
    (handler-case
        (ecase (job-phase job)
          ((:done :failed :gone) t)
          (:start
           (case (srv-state server)
             (:starting nil)
             (:ready
              (refresh-documents server (job-path job))
              (setf (job-since job) (bt2:with-lock-held ((srv-lock server)) (srv-counter server)))
              (multiple-value-bind (version changed) (sync-document server (job-path job))
                (setf (job-version job) version (job-changed job) changed)
                (cond ((null version) (setf (job-phase job) :gone) t)
                      (t (when (pulls-diagnostics-p server)
                           (send-pull job))
                         (setf (job-phase job) :wait)
                         (job-step job)))))
             (t (setf (job-phase job) :failed) t)))
          (:wait
           (if (not (eq :ready (srv-state server)))
               (progn (setf (job-phase job) :failed) t)
               (let ((items (or (pulled-answer job) (published-answer job))))
                 (when items
                   (setf (job-items job) items (job-phase job) :done)
                   t)))))
      (lsp-error ()
        (setf (job-phase job) :failed)
        t))))

(defun collect-diagnostics (pairs deadline)
  "Wait, until DEADLINE, for the diagnostics of every (SERVER . PATH) in
PAIRS. => the JOBS, each :DONE with its items, :GONE, :FAILED, or still
waiting."
  (let ((jobs (mapcar (lambda (pair) (make-job :server (touch (car pair)) :path (cdr pair))) pairs)))
    (unwind-protect
         (loop (let ((open (remove-if #'job-step jobs)))
                 (when (or (null open) (zerop (seconds-left deadline)))
                   (return jobs))
                 (sleep 0.05)))
      (dolist (job jobs)
        (let ((reply (job-pull job)))
          (when (and (reply-p reply) (eq :waiting (reply-state reply)))
            (abandon (srv-conn (job-server job)) reply)))))))
