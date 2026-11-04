;;;; bridge.lisp --- the HTTP bridge the companion extension polls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The extension is an HTTP CLIENT of this process: it long-polls GET /next
;;;; (held up to *LONG-POLL-SECONDS*, answered with one command or "none"),
;;;; POSTs each command's outcome to /result, and preflights that POST with
;;;; OPTIONS. The port is hard-coded in the extension's manifest, so this
;;;; bridge is its own clack/hunchentoot acceptor on 127.0.0.1:17318 — never
;;;; a route on the gateway's listener. The wire shapes, the CORS rules and
;;;; the timeout classification are pi-chrome's (index.ts), ported verbatim:
;;;; browser-extension/ is pi-chrome's extension with the changes its NOTICE
;;;; lists; the one on the wire is the bridge secret (below).
;;;;
;;;; One command in flight per send: BRIDGE-SEND enqueues a PENDING, wakes a
;;;; parked poller, and waits on the pending's own semaphore. The extension
;;;; serializes commands itself (it awaits each before polling again), so the
;;;; queue is normally at most one deep. Threads: pollers are hunchentoot
;;;; worker threads parked on the READY semaphore; senders are EVAL's
;;;; eval-N threads parked on DONE. Neither holds the bridge lock across a
;;;; wait.
;;;;
;;;; Retention: everything here is live-only. A gateway restart forgets the
;;;; queue; the extension reconnects on its own 2 s backoff and a result it
;;;; posts for a command from a previous life 404s by id.
;;;;
;;;; The secret (cells#14): every request that moves a command -- GET /next,
;;;; POST /result, POST /command -- carries `Authorization: Bearer <secret>'.
;;;; The cell mints one per home (BRIDGE-TOKEN, cell.lisp) into the
;;;; extension's loadable copy, 0600, so only this user and the extension
;;;; loaded from there read it. pi-chrome has none, and without it any local
;;;; process or other user on the machine could take commands off the queue,
;;;; forge their results, or drive Chrome through /command. A /next without it
;;;; takes nothing and hears "none" at once with the version header, so an
;;;; extension built before the secret reloads itself onto the build that
;;;; sends it. The first authenticated request carrying an Origin pins that
;;;; Origin as the extension's: any other is refused for the bridge's life.
;;;;
;;;; A second process (nodecode#169). The port is the extension's, so one
;;;; process on the machine serves it. Any other that starts the cell -- a
;;;; headless `nodecode rpc', a bench, a second gateway -- is pi-chrome's
;;;; "client mode", a RELAY: each command goes to the owner as a POST
;;;; /command carrying this home's secret, and the owner queues it beside its
;;;; own. A relay tries the port every *RECLAIM-SECONDS*, and the first try
;;;; that binds it (the owner exited) makes it the bridge, so the extension
;;;; has a process to poll while any of the home's runs. An owner that refuses
;;;; the secret -- another home's nodecode, a pi-chrome host -- leaves this
;;;; process without Chrome: the command that tries says so, never the boot.
;;;; POST /command is also what a real pi session that lost the port to us
;;;; sends, and curl's probe.

(in-package #:nodecode-chrome)

;;; Tests SETF an ephemeral port; this is deliberately not a config key — the
;;; extension would not follow it.
(defparameter *bridge-port* 17318
  "The extension's hard-coded bridge port.")

;;; Must stay at or under the extension's own 25 s command budget and under
;;; the MV3 idle kill.
(defparameter *long-poll-seconds* 25
  "How long GET /next parks before answering \"none\".")

(defparameter *send-timeout-seconds* 30
  "Default wait for one command's result (pi-chrome DEFAULT_TIMEOUT_MS).")

;;; A full-page screenshot arrives as tiles of base64 PNG inside one JSON
;;; document — multi-MB is ordinary.
(defconstant +max-body-bytes+ (* 16 1024 1024)
  "Ceiling on a /result or /command body.")

;;; DEFVAR: a reload of this file mid-serve must
;;; not forget a running acceptor.
(defvar *bridge* nil
  "The one live bridge, or NIL.")

;;; --- state ----------------------------------------------------------------

(nlk:define-record (pending (:constructor %make-pending))
  "One command from enqueue to result."
  ;; DELIVERED-AT is set when a poller hands it to the extension;
  ;; RESULT/ERROR-TEXT by /result; DONE is signalled exactly once by /result
  ;; or by STOP-BRIDGE.
  (id "" :type string :read-only t)
  (action "" :type string :read-only t)
  (params nil :read-only t)
  (done (bt2:make-semaphore :name "chrome-pending") :read-only t)
  (result nil)
  (error-text nil)
  (delivered-at nil))

(nlk:define-record (bridge (:constructor %make-bridge))
  (port 0 :type integer)
  (version "" :type string)
  ;; The extension's manifest name: only a poll whose ?name= starts with it
  ;; gets commands.
  (name "" :type string)
  (handler nil)
  (lock (bt2:make-lock :name "chrome-bridge") :read-only t)
  ;; FIFO of PENDING awaiting a poller, oldest first.
  (queue '() :type list)
  ;; One count per enqueue (and a burst on stop); pollers park here.
  (ready (bt2:make-semaphore :name "chrome-ready") :read-only t)
  ;; id -> PENDING for every command not yet resolved (queued or delivered).
  (pending (make-hash-table :test #'equal) :read-only t)
  ;; Universal time of the last /next or /result, and the ?name= it carried.
  (last-seen nil)
  (client-name nil)
  ;; The same for a poll from any other extension on this port: an old build
  ;; still loaded from a release folder, or pi's own.
  (foreign-seen nil)
  (foreign-name nil)
  ;; The per-home secret every command-moving request carries as a Bearer.
  (secret "" :type string :read-only t)
  ;; The Origin the first authenticated request carried; any other is refused.
  (extension-origin nil)
  ;; Universal time of the last /next refused for want of the secret.
  (unauthorized-seen nil)
  (stopping-p nil))

(nlk:define-record (relay (:constructor %make-relay))
  "A process that found the port served: its commands go through the owner."
  (port 0 :type integer :read-only t)
  (secret "" :type string :read-only t)
  ;; START-BRIDGE's arguments: the takeover starts the bridge with them.
  (start-arguments '() :type list :read-only t)
  ;; Signalled to end the watcher's wait early, on stop.
  (wake (bt2:make-semaphore :name "chrome-relay") :read-only t)
  (stopping-p nil))

(define-condition bridge-port-taken (error)
  ((port :initarg :port :reader bridge-port-taken-port))
  (:report (lambda (condition stream)
             (format stream "chrome bridge: 127.0.0.1:~d is already bound"
                     (bridge-port-taken-port condition))))
  (:documentation "START-BRIDGE found its port served by another process."))

;;; DEFVAR, as *BRIDGE*'s own: a reload must not forget a takeover in flight.
(defvar *bridge-lock* (bt2:make-lock :name "chrome-bridge-hands")
  "Held while *BRIDGE* changes hands: a takeover against a close.")

(defun bridge-url (bridge)
  (format nil "http://127.0.0.1:~d" (if (relay-p bridge) (relay-port bridge) bridge.port)))

(defun advertised-version (bridge)
  "The extension version BRIDGE advertises, or a relay would on takeover."
  (if (relay-p bridge)
      (getf (relay-start-arguments bridge) :version "0.0.0")
      (bridge-version bridge)))

(defun poll-age-seconds (bridge &aux (seen bridge.last-seen))
  "Seconds since the extension last spoke, or NIL when it never has."
  (and seen (- (get-universal-time) seen)))

(defun bridge-connected-p (bridge &aux (age (poll-age-seconds bridge)))
  (and age (< age 300)))

(defun foreign-polling-p (bridge &aux (seen bridge.foreign-seen))
  "Whether another extension polled this port in the last minute."
  (and seen (< (- (get-universal-time) seen) 60)))

(defun unauthorized-polling-p (bridge &aux (seen bridge.unauthorized-seen))
  "Whether a poll without the bridge secret came in the last minute."
  (and seen (< (- (get-universal-time) seen) 60)))

;;; --- sending --------------------------------------------------------------

(defun bridge-send (bridge action params &key (timeout *send-timeout-seconds*))
  "Hand ACTION/PARAMS to the extension and return its decoded result."
  (when (relay-p bridge)
    (return-from bridge-send (relay-send bridge action params :timeout timeout)))
  ;; PARAMS is a JSON object (EQUAL hash table). Signals CHROME-OFFLINE with
  ;; no bridge, CHROME-COMMAND-FAILED on an ok:false reply, CHROME-TIMEOUT
  ;; when no result lands within TIMEOUT seconds. The UNWIND-PROTECT is what
  ;; makes the wait safe to interrupt: an EVAL-INTERRUPT or a stop leaves no
  ;; orphan in the queue or the pending table.
  (when (or (null bridge) bridge.stopping-p)
    (error 'chrome-offline))
  (check-type action string)
  (let ((pending (%make-pending :id (nlk:make-durable-id "chrome")
                                :action action
                                :params (or params (nlk:make-json-object)))))
    (nlk:with-cleanup ((bt2:with-lock-held ((bridge-lock bridge))
                         (remhash (pending-id pending) (bridge-pending bridge))
                         (setf (bridge-queue bridge)
                               (delete pending (bridge-queue bridge)))))
      (bt2:with-lock-held ((bridge-lock bridge))
        (setf (gethash pending.id bridge.pending) pending
              bridge.queue
              (append bridge.queue (list pending))))
      (bt2:signal-semaphore bridge.ready)
      (cond
        ((bt2:wait-on-semaphore (pending-done pending) :timeout timeout)
         (if pending.error-text
             (error 'chrome-command-failed
                    :action action :detail pending.error-text)
             pending.result))
        (t
         ;; pi-chrome's three timeout classes: delivered, not polling, not picked up.
         (let ((age (poll-age-seconds bridge)))
           (error 'chrome-timeout
                  :detail
                  (cond
                    ((pending-delivered-at pending)
                     (format nil "Timed out after ~as: the Chrome extension received the ~
                                       command but never returned a result. The action may be ~
                                       long-running, or the result post failed. Run /chrome ~
                                       doctor; if it persists, reload 'Nodecode Chrome Connector' at ~
                                       chrome://extensions."
                             timeout))
                    ((and (or (null age) (> age 60)) (foreign-polling-p bridge))
                     (format nil "Timed out after ~as: Chrome has '~a' polling instead of ~
                                       '~a'. Remove it at chrome://extensions, then install ~
                                       this one from /chrome."
                             timeout bridge.foreign-name bridge.name))
                    ((and (or (null age) (> age 60)) (unauthorized-polling-p bridge))
                     (format nil "Timed out after ~as: an extension is polling without this ~
                                       bridge's secret, most likely a build from before it. Reload ~
                                       'Nodecode Chrome Connector' at chrome://extensions, or install ~
                                       it from /chrome and load the folder it names."
                             timeout))
                    ((or (null age) (> age 60))
                     (format nil "Timed out after ~as: the Chrome extension is not polling ~
                                       (last seen ~a). Install it from /chrome, then load the folder it ~
                                       names in your normal Chrome profile and keep that Chrome ~
                                       window open."
                             timeout (if age (format nil "~ds ago" age) "never")))
                    (t
                     (format nil "Timed out after ~as: the Chrome extension is polling ~
                                       (last seen ~ds ago) but did not pick up this command in ~
                                       time. Retry; if it persists, reload 'Nodecode Chrome Connector' ~
                                       at chrome://extensions."
                             timeout age))))))))))

;;; --- request plumbing -----------------------------------------------------

(defun request-header (env name &aux (headers (getf env :headers)))
  (and (hash-table-p headers) (gethash name headers)))

(defun json-response (status object &rest extra-headers)
  (list status
        (list* :content-type "application/json; charset=utf-8"
               :cache-control "no-store"
               extra-headers)
        (list (nlk:encode-json-object object))))

(defun refusal (status text &optional cors)
  (apply #'json-response status
         (nlk:json-object "ok" :false "error" text)
         cors))

(defun make-bridge-secret ()
  "A fresh bridge secret: 32 bytes from the operating system, as hex."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 32) 'list)))

(defun secret= (a b)
  "Whether strings A and B are equal, in a time that does not depend on where
they first differ."
  (and (= (length a) (length b))
       (zerop (loop with diff = 0
                    for x across a
                    for y across b
                    do (setf diff (logior diff (logxor (char-code x) (char-code y))))
                    finally (return diff)))))

(defun authorized-p (bridge env &aux (header (or (request-header env "authorization") ""))
                                     (scheme "Bearer "))
  "Whether ENV carries BRIDGE's secret as its Bearer credential."
  (and (plusp (length bridge.secret))
       (> (length header) (length scheme))
       (string-equal scheme header :end2 (length scheme))
       (secret= bridge.secret (string-trim " " (subseq header (length scheme))))))

(defun foreign-origin-p (bridge origin)
  "Pin ORIGIN as BRIDGE's extension when none is pinned yet; => whether ORIGIN
is another one. Asked only of an authenticated request."
  (and (plusp (length origin))
       (bt2:with-lock-held ((bridge-lock bridge))
         (string/= origin (or bridge.extension-origin
                              (setf bridge.extension-origin origin))))))

(defun read-body-json (env &aux (length (or (getf env :content-length) 0))
                                (stream (getf env :raw-body)))
  "The request body decoded from UTF-8 octets, or (values NIL STATUS TEXT)
naming the refusal."
  ;; Octets, not a character read: clack's raw body is a latin-1 flexi-stream,
  ;; and a string read would mojibake every non-ASCII character of page text.
  (cond
    ((> length +max-body-bytes+)
     (values nil 413 "request body too large"))
    ((or (zerop length) (null stream))
     (values nil 400 "request body required"))
    (t
     (let ((octets (make-array length :element-type '(unsigned-byte 8))))
       (if (/= length (read-sequence octets stream))
           (values nil 400 "request body ended early")
           (nlk:with-handlers ((error (condition)
                                 (values nil 400 (format nil "invalid JSON: ~a" condition))))
             (values (nlk:decode-json
                      (sb-ext:octets-to-string octets :external-format :utf-8)))))))))

;;; --- routes ---------------------------------------------------------------

(defun note-seen (bridge env)
  "Record the request's client; => whether it is this bridge's extension."
  ;; The poll's ?name=, percent-decoded; a malformed value stays whole. A
  ;; request without one (POST /result) is ours.
  (let ((name (nlk:when-let (raw (cdr (assoc "name" (ignore-errors (quri:url-decode-params
                                                                     (getf env :query-string)
                                                                     :lenient t :percent-decode nil))
                                             :test #'string=)))
                (handler-case (quri:url-decode raw :lenient t)
                  (error () raw)))))
    (bt2:with-lock-held ((bridge-lock bridge))
      (if (or (null name) (uiop:string-prefix-p bridge.name name))
          (progn (setf bridge.last-seen (get-universal-time))
                 (when name (setf bridge.client-name name))
                 t)
          (progn (setf bridge.foreign-seen (get-universal-time)
                       bridge.foreign-name name)
                 nil)))))

(defun client-gone-p (env)
  "Whether the client behind ENV's connection has hung up."
  ;; A long-poll client sends nothing after its request, so a zero-byte peek
  ;; is its EOF and a reset signals. The socket is the one clack's hunchentoot
  ;; handler keeps on the :CLACK.IO client (unexported); without it the answer
  ;; is NIL, as before the check. Windows has no MSG_DONTWAIT: there a dead
  ;; poll still takes one command.
  #+win32 (declare (ignore env))
  #-win32
  (nlk:when-let (socket (ignore-errors
                         (usocket:socket (clack.handler.hunchentoot::client-socket
                                          (getf env :clack.io)))))
    (handler-case
        (eql 0 (nth-value 1 (sb-bsd-sockets:socket-receive
                             socket (make-array 1 :element-type '(unsigned-byte 8)) 1
                             :peek t :dontwait t)))
      (sb-bsd-sockets:socket-error () t))))

(defun make-bridge-app (bridge)
  "The clack application: a closed table over method and path."
  (lambda (env)
    (let* ((method (getf env :request-method))
           (uri (or (getf env :request-uri) ""))
           (path (subseq uri 0 (or (position #\? uri) (length uri))))
           (origin (or (request-header env "origin") ""))
           ;; CORS only for an extension Origin; without expose-headers the version handshake no-ops.
           (cors (when (uiop:string-prefix-p "chrome-extension://" origin)
                   (list :access-control-allow-origin origin
                         :access-control-allow-methods "GET,POST,OPTIONS"
                         :access-control-allow-headers "content-type, authorization"
                         :access-control-expose-headers "x-pi-chrome-version"
                         :vary "origin")))
           (version bridge.version))
      (cond
        ;; pi-chrome isBrowserOriginAllowed: an extension's Origin, else sec-fetch-site none/same-origin.
        ((and (or (eq method :options)
                  (and (eq method :get) (string= path "/next"))
                  (and (eq method :post) (string= path "/result")))
              (if (plusp (length origin))
                  (null cors)
                  (not (member (or (request-header env "sec-fetch-site") "") '("" "none" "same-origin")
                               :test #'string=))))
         (refusal 403 "browser origin not allowed"))
        ((eq method :options)
         (apply #'json-response 200 (nlk:json-object "ok" t) cors))
        ((and (eq method :get) (string= path "/status"))
         (json-response 200 (bt2:with-lock-held ((bridge-lock bridge))
                              (nlk:json-object
                               "url" (bridge-url bridge)
                               "version" version
                               "mode" "server"
                               "connected" (if (bridge-connected-p bridge) t :false)
                               "lastSeenAt" (or bridge.last-seen :null)
                               "clientName" (or bridge.client-name :null)
                               "foreignClient" (or bridge.foreign-name :null)
                               "unauthorizedPollAt" (or bridge.unauthorized-seen :null)
                               "queuedCommands" (length bridge.queue)
                               "pendingCommands" (hash-table-count bridge.pending)))))
        ;; A poll without the secret takes nothing. It hears "none" at once,
        ;; with the version, so an extension older than the secret reloads onto
        ;; the build that sends it (it reads the header only on a 2xx).
        ((and (eq method :get) (string= path "/next") (not (authorized-p bridge env)))
         (bt2:with-lock-held ((bridge-lock bridge))
           (setf bridge.unauthorized-seen (get-universal-time)))
         (apply #'json-response 200 (nlk:json-object "type" "none"
                                                     "reason" "unauthorized"
                                                     "expectedExtensionVersion" version)
                :x-pi-chrome-version version cors))
        ((and (eq method :post) (string= path "/result") (not (authorized-p bridge env)))
         (refusal 401 "the bridge secret is missing or wrong" cors))
        ((and (or (and (eq method :get) (string= path "/next"))
                  (and (eq method :post) (string= path "/result")))
              (foreign-origin-p bridge origin))
         (refusal 403 "another extension holds this bridge" cors))
        ((and (eq method :get) (string= path "/next"))
         ;; The long poll: the oldest queued command, else park on READY; "none" at the deadline.
         (let ((deadline (nlk:deadline-after *long-poll-seconds*))
               (ours (note-seen bridge env))
               (woken nil))
           (labels ((answer (object)
                      (apply #'json-response 200 object
                             :x-pi-chrome-version version cors))
                    (none ()
                      (answer (nlk:json-object "type" "none"
                                               "expectedExtensionVersion" version)))
                    (remaining ()
                      (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
             (if (not ours)
                 ;; Another extension's poll waits out its deadline without a
                 ;; command: answered at once, it would poll again at once.
                 (progn
                   (loop until (or bridge.stopping-p (client-gone-p env) (<= (remaining) 0))
                         do (sleep (min 0.5 (max 0 (remaining)))))
                   (none))
                 (loop
                   ;; A poll whose Chrome was killed or restarted stays parked
                   ;; here up to the deadline; it must not take a command nobody
                   ;; will run (the send would wait out its whole timeout). It
                   ;; passes on the wake it took, so a live poll gets the command.
                   (when (client-gone-p env)
                     (when woken (bt2:signal-semaphore bridge.ready))
                     (return (none)))
                   (nlk:when-let (pending (bt2:with-lock-held ((bridge-lock bridge))
                                            (pop bridge.queue)))
                     (setf pending.delivered-at (get-universal-time))
                     (return (answer (nlk:json-object
                                      "type" "command"
                                      "command" (nlk:json-object
                                                 "id" pending.id
                                                 "action" pending.action
                                                 "params" pending.params)
                                      "expectedExtensionVersion" version))))
                   (let ((remaining (remaining)))
                     (when (or bridge.stopping-p
                               (<= remaining 0)
                               (not (setf woken (bt2:wait-on-semaphore
                                                 bridge.ready :timeout (float remaining 1d0)))))
                       (return (none)))))))))
        ((and (eq method :post) (string= path "/result"))
         ;; Settle the PENDING the body names.
         (nlk:bind (((body status text) (read-body-json env))
                    (id (nlk:json-value body :string "id"))
                    (pending (and id (bt2:with-lock-held ((bridge-lock bridge))
                                       (gethash id bridge.pending)))))
           (when body (note-seen bridge env))
           (cond
             ((null body) (refusal status text cors))
             ((null pending) (refusal 404 "unknown command id" cors))
             (t
              (if (nlk:json-value body :boolean "ok")
                  (setf pending.result (nlk:json-value body :any "result"))
                  (setf pending.error-text
                        (or (nlk:json-value body :text "error")
                            "Chrome extension command failed")))
              (bt2:signal-semaphore pending.done)
              (apply #'json-response 200 (nlk:json-object "ok" t) cors)))))
        ;; Host-to-host, pi-chrome isLocalProcessRequest: no Origin, no sec-fetch-site.
        ((and (eq method :post) (string= path "/command")
              (or (request-header env "origin") (request-header env "sec-fetch-site")))
         (refusal 403 "Chrome commands are accepted only from local processes"))
        ((and (eq method :post) (string= path "/command") (not (authorized-p bridge env)))
         (refusal 401 "the bridge secret is missing or wrong"))
        ((and (eq method :post) (string= path "/command"))
         (multiple-value-bind (body status text) (read-body-json env)
           (let ((action (nlk:json-value body :text "action"))
                 (params (nlk:json-value body :object "params"))
                 (timeout (nlk:if-let (ms (nlk:json-value body :number "timeoutMs"))
                            (/ ms 1000) *send-timeout-seconds*)))
             (cond
               ((null body) (refusal status text))
               ((null action) (refusal 400 "Missing command action"))
               (t
                (nlk:with-handlers ((chrome-error (condition)
                                      ;; KIND and DETAIL: a relay signals what this send did.
                                      (json-response 504 (nlk:json-object
                                                          "ok" :false
                                                          "error" (princ-to-string condition)
                                                          "kind" (error-kind condition)
                                                          "detail" (or (chrome-error-detail condition)
                                                                       :null)))))
                  (json-response
                   200 (nlk:json-object
                        "ok" t
                        "result" (or (bridge-send bridge action params :timeout timeout)
                                     :null)))))))))
        (t (json-response 404 (nlk:json-object "error" "not found")))))))

;;; --- lifecycle ------------------------------------------------------------

(defun start-bridge (&key (port *bridge-port*) (version "0.0.0")
                          (name "Nodecode Chrome Connector")
                          (secret (make-bridge-secret))
                          extension-origin)
  "Start the acceptor on 127.0.0.1:PORT advertising VERSION (the vendored
manifest's — advertising a newer one makes the extension reload itself in a
loop), handing commands only to a poll whose client name starts with NAME
(the manifest's), and moving a command only for a request that carries
SECRET as its Bearer (the cell passes BRIDGE-TOKEN; the default is a fresh
one nobody else knows). EXTENSION-ORIGIN, when given, is the only Origin
such a request may carry; without it the first one to authenticate is pinned."
  ;; Returns the live BRIDGE. NLE:SERVE-LOCAL signals rather than answering a
  ;; bridge nobody serves: a bound port, an acceptor that never answers. A
  ;; Ctrl-C during START-CELLS lands in its acceptance latch, before the
  ;; cell is recorded as started, and takes the acceptor with it.
  (let ((bridge (%make-bridge :port port :version version :name name
                               :secret secret :extension-origin extension-origin)))
    (setf bridge.handler
          (handler-case (nle:serve-local (make-bridge-app bridge) :port port)
            (usocket:address-in-use-error ()
              (error 'bridge-port-taken :port port))))
    bridge))

(defun stop-bridge (bridge)
  "Fail every pending send, wake parked pollers so they answer \"none\" now
instead of at their deadline, stop the acceptor, and wait for the port to
close. A relay stops relaying and watching. Idempotent."
  (when (relay-p bridge)
    (return-from stop-bridge (stop-relay bridge)))
  (bt2:with-lock-held ((bridge-lock bridge))
    (setf bridge.stopping-p t)
    (loop for pending being the hash-values of bridge.pending
          do (setf pending.error-text "Chrome bridge stopped")
             (bt2:signal-semaphore pending.done))
    (clrhash bridge.pending)
    (setf bridge.queue '()))
  (bt2:signal-semaphore bridge.ready :count 16)
  (nlk:when-let (handler (shiftf bridge.handler nil))
    (nle:stop-clack-handler handler))
  t)

;;; --- a second process: the relay -----------------------------------------

(defparameter *reclaim-seconds* 5
  "How often a relay tries to take the port over. Tests shorten it.")

(defun open-bridge (&rest arguments &key (port *bridge-port*) (secret (make-bridge-secret))
                    &allow-other-keys)
  "Serve PORT with START-BRIDGE's ARGUMENTS => the BRIDGE; when another
process serves it, => a RELAY through that one, watching the port."
  (let ((arguments (list* :port port :secret secret arguments)))
    (handler-case (apply #'start-bridge arguments)
      (bridge-port-taken ()
        (let ((relay (%make-relay :port port :secret secret :start-arguments arguments)))
          (nlk:spawn "chrome-bridge-reclaim" (watch-port relay))
          relay)))))

(defun watch-port (relay)
  "Until RELAY stops or stands down, try its port every *RECLAIM-SECONDS*."
  ;; An error starting the bridge leaves the relay as it was, for the next try.
  (loop until (or relay.stopping-p
                  (bt2:wait-on-semaphore relay.wake :timeout *reclaim-seconds*)
                  relay.stopping-p)
        do (nlk:with-handlers ((error () nil))
             (take-over relay))))

(defun take-over (relay)
  "When RELAY's port binds now, the bridge started on it replaces RELAY as
*BRIDGE* and RELAY stands down. => what *BRIDGE* holds after."
  (bt2:with-lock-held (*bridge-lock*)
    (when (and (eq *bridge* relay) (not relay.stopping-p))
      ;; Any refusal leaves the relay relaying, for the next try.
      (handler-case (setf *bridge* (apply #'start-bridge relay.start-arguments)
                          relay.stopping-p t)
        (error () nil)))
    *bridge*))

(defun live-bridge (&aux (bridge *bridge*))
  "*BRIDGE*, once a relay whose port is free now has taken it over."
  (if (relay-p bridge) (take-over bridge) bridge))

(defun stop-relay (relay)
  (setf relay.stopping-p t)
  (bt2:signal-semaphore relay.wake)
  t)

(defun close-bridge ()
  "Stop the one live bridge or relay, if there is one."
  (nlk:when-let (bridge (bt2:with-lock-held (*bridge-lock*) (shiftf *bridge* nil)))
    (stop-bridge bridge)))

(defun relay-url (relay path)
  (format nil "http://127.0.0.1:~d~a" relay.port path))

(defun error-kind (condition)
  "CONDITION's class as POST /command names it, for a relay to signal again."
  (typecase condition
    (chrome-timeout "timeout")
    (chrome-command-failed "command-failed")
    (chrome-offline "offline")
    (t "error")))

(defun refused-secret-text (relay)
  (format nil "127.0.0.1:~d is served by a Chrome bridge in another process that refuses ~
               this home's secret: a nodecode of another home, or a pi-chrome host. Chrome ~
               works from the process that serves it."
          relay.port))

(defun relay-send (relay action params &key (timeout *send-timeout-seconds*))
  "Send ACTION/PARAMS through the owner's POST /command => the decoded result,
signalling what the owner's own send signalled. An owner gone, the port is
taken over and the command sent from here."
  (when relay.stopping-p
    (error 'chrome-offline))
  (multiple-value-bind (text status)
      (handler-case
          (nlk:http :post (relay-url relay "/command")
                    :headers (list (cons "authorization" (format nil "Bearer ~a" relay.secret))
                                   (cons "content-type" "application/json"))
                    :content (nlk:encode-json-object
                              (nlk:json-object "action" action
                                               "params" (or params (nlk:make-json-object))
                                               "timeoutMs" (round (* timeout 1000))))
                    ;; The owner holds the exchange for the whole send.
                    :timeout (+ timeout 10) :connect-timeout 5)
        (usocket:connection-refused-error ()
          (let ((now (take-over relay)))
            (return-from relay-send
              (if (and now (not (relay-p now)))
                  (bridge-send now action params :timeout timeout)
                  (error 'chrome-offline
                         :detail (format nil "The process serving 127.0.0.1:~d is gone and ~
                                              another took the port; retry."
                                         relay.port))))))
        (sb-sys:deadline-timeout ()
          (error 'chrome-timeout
                 :detail (format nil "Timed out after ~as: the process serving 127.0.0.1:~d ~
                                      did not answer." timeout relay.port))))
    (let ((body (ignore-errors (nlk:decode-json text))))
      (flet ((field (key) (and (hash-table-p body) (nlk:json-value body :text key))))
        (cond
          ((and (eql status 200) (hash-table-p body))
           (nlk:json-value body :any "result"))
          ((eql status 401) (error 'chrome-offline :detail (refused-secret-text relay)))
          (t
           (let ((detail (or (field "detail") (field "error")
                             (format nil "the bridge on 127.0.0.1:~d answered HTTP ~d"
                                     relay.port status))))
             (nlk:dispatch (field "kind") equal
               ("timeout" (error 'chrome-timeout :detail detail))
               ("offline" (error 'chrome-offline :detail detail))
               (t (error 'chrome-command-failed :action action :detail detail))))))))))

(defun relay-owner (relay)
  "What the process serving RELAY's port says => (values STATUS ACCEPTED):
its /status object or NIL when it answers none, and whether it takes this
home's secret."
  ;; A POST /command without an action is refused after the secret is
  ;; checked: 400 says the secret holds, 401 that it does not.
  (flet ((ask (method path &rest keys)
           (ignore-errors
            (multiple-value-list (apply #'nlk:http method (relay-url relay path)
                                        :timeout 3 :connect-timeout 2 keys)))))
    (let ((status (ask :get "/status"))
          (probe (ask :post "/command"
                      :headers (list (cons "authorization" (format nil "Bearer ~a" relay.secret))
                                     (cons "content-type" "application/json"))
                      :content "{}")))
      (values (and (eql (second status) 200)
                   (let ((object (ignore-errors (nlk:decode-json (first status)))))
                     (and (hash-table-p object) object)))
              (eql (second probe) 400)))))
