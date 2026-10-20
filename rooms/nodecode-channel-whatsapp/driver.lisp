;;;; driver.lisp --- the WhatsApp driver: built once, started, spoken to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WhatsApp has no bot API a personal account can use; a linked device
;;;; speaks the WhatsApp Web protocol, end-to-end encrypted, and that protocol
;;;; is whatsmeow's (go.mau.fi/whatsmeow, MPL-2.0). driver/main.go is a small
;;;; Go program over it, and this file is everything the cell knows about it:
;;;;
;;;;   - BUILD. The driver is compiled from driver/ on first start, into the
;;;;     home under a name that is the digest of its sources, so an edited
;;;;     driver is a new binary and an unchanged one is built once.
;;;;     channels.whatsapp.driver_path names a binary built elsewhere instead.
;;;;   - THE WIRE. One JSON object a line each way. A request carries an id the
;;;;     reply repeats; anything else the driver writes is an event. Requests
;;;;     run concurrently on the driver's side, so replies come back in any
;;;;     order and each caller waits on its own box.
;;;;   - THE EXECUTOR. The kit's plans are HTTP-shaped: a path and a body. Here
;;;;     the path is the driver's op (/send, /edit, /react ...) and the body its
;;;;     args, so every outbound call the kit makes, and every one a test
;;;;     records, is one request-plan, as on every other platform.
;;;;
;;;; A DRIVER is the wire with nothing behind it: SEND is how a line leaves.
;;;; START-DRIVER-PROCESS puts a process behind it; a test puts a fake.

(in-package #:nodecode-channel-whatsapp)

(nlk:access (plan request-plan) (chunk text-chunk))

(defstruct (driver (:constructor %make-driver) (:copier nil))
  ;; (line) => writes one request line; signals when it cannot.
  (send nil :type (or null function))
  (lock (bt2:make-lock :name "whatsapp-driver"))
  (next-id 0 :type integer)
  ;; request id -> its reply box, (LOCK CV . REPLY)
  (pending (make-hash-table :test #'equal :synchronized t))
  ;; events the driver wrote, oldest first, and their wakeup
  (events-lock (bt2:make-lock :name "whatsapp-driver-events"))
  (events-cv (bt2:make-condition-variable :name "whatsapp-driver-events"))
  (events '() :type list)
  (events-tail '() :type list)
  (live t)
  ;; the process behind it, when there is one
  (pid nil) (process nil) (input nil) (output nil) (reader nil) (log nil))

(nlk:access (driver driver))

(defun make-driver (&key send)
  "A driver whose request lines leave through SEND."
  (%make-driver :send send))

;;; --- the wire ------------------------------------------------------------------

(defun enqueue-event (driver event)
  (bt2:with-lock-held (driver.events-lock)
    (let ((cell (list event)))
      (if driver.events
          (setf (cdr driver.events-tail) cell)
          (setf driver.events cell))
      (setf driver.events-tail cell))
    (bt2:condition-notify driver.events-cv)))

(defun driver-next-event (driver &optional (timeout 1))
  "The oldest event DRIVER has written and nobody took, waiting up to
TIMEOUT seconds for one; NIL when none came."
  (bt2:with-lock-held (driver.events-lock)
    (unless driver.events
      (bt2:condition-wait driver.events-cv driver.events-lock :timeout timeout))
    (when driver.events
      (pop driver.events))))

(defun deliver-reply (driver id reply)
  (nlk:when-let (box (gethash id driver.pending))
    (remhash id driver.pending)
    (bt2:with-lock-held ((first box))
      (setf (cddr box) (list reply))
      (bt2:condition-notify (second box)))))

(defun driver-accept-line (driver line)
  "One LINE the driver wrote: a reply goes to the call waiting on its id,
anything else is an event, queued for the lap."
  (let ((message (and (plusp (length (nlk:trimmed line)))
                      (handler-case (nlk:decode-json line)
                        (error ()
                          (warn "whatsapp: the driver wrote a line that is not JSON: ~a"
                                (nlk:clip line 200))
                          nil)))))
    ;; An event says so first: a message event carries its message's id too.
    (cond ((not (hash-table-p message)) nil)
          ((stringp (gethash "event" message)) (enqueue-event driver message))
          ((stringp (gethash "id" message)) (deliver-reply driver (gethash "id" message) message))
          ;; A refusal of a line the driver could not read names no id.
          (t (warn "whatsapp: the driver said ~a" (nlk:clip line 300))))))

(defun driver-fail-pending (driver reason)
  "Answer every call still waiting with REASON: the driver is gone."
  (loop for id in (loop for key being the hash-keys of driver.pending collect key)
        do (deliver-reply driver id (nlk:json-object "ok" nil "status" 0 "error" reason))))

(defun driver-alive-p (driver)
  (and driver driver.live t))

(defun driver-call (driver op args &key (timeout 30))
  "Ask DRIVER for OP with ARGS (a JSON object), waiting up to TIMEOUT
seconds. => (values STATUS RESULT ERROR): 200 and the driver's result, or
the status it refused with and why; status 0 when no answer came."
  (unless (driver-alive-p driver)
    (return-from driver-call (values 0 nil "the WhatsApp driver is not running")))
  (let* ((id (bt2:with-lock-held (driver.lock) (format nil "~d" (incf driver.next-id))))
         (box (list* (bt2:make-lock :name "whatsapp-reply") (bt2:make-condition-variable) nil))
         (line (nlk:encode-json-object
                (nlk:json-object "id" id "op" op "args" (or args (nlk:json-object))))))
    (setf (gethash id driver.pending) box)
    (handler-case (bt2:with-lock-held (driver.lock) (funcall driver.send line))
      (error (condition)
        (remhash id driver.pending)
        (return-from driver-call
          (values 0 nil (format nil "the WhatsApp driver could not be written to: ~a" condition)))))
    (let ((reply (bt2:with-lock-held ((first box))
                   (loop with deadline = (+ (get-internal-real-time)
                                            (* timeout internal-time-units-per-second))
                         until (cddr box)
                         for left = (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)
                         while (plusp left)
                         do (bt2:condition-wait (second box) (first box) :timeout left))
                   (first (cddr box)))))
      (remhash id driver.pending)
      (cond ((null reply)
             (values 0 nil (format nil "the WhatsApp driver did not answer ~a within ~ds" op timeout)))
            ((gethash "ok" reply) (values 200 (gethash "result" reply) nil))
            (t (values (or (nlk:json-value reply :integer "status") 502) nil
                       (or (nlk:json-value reply :string "error") "refused")))))))

;;; --- the process ----------------------------------------------------------------

(defun start-driver-process (binary &key db media log (online t) (device-name "Nodecode")
                                     (log-level "info"))
  "BINARY started over the linked device kept in DB, downloads landing in
MEDIA, its own log appended to LOG: a DRIVER whose reader thread queues
what it writes."
  (ensure-directories-exist db)
  (ensure-directories-exist (uiop:ensure-directory-pathname media))
  (when log (ensure-directories-exist log))
  (multiple-value-bind (pid output errors process input)
      (nlk:spawn-program (list (uiop:native-namestring binary)
                               "--db" (uiop:native-namestring db)
                               "--media" (uiop:native-namestring media)
                               "--log-level" log-level
                               "--device-name" device-name
                               (format nil "--online=~:[false~;true~]" online))
                         :input :stream :output :stream
                         :error-output (or log nil))
    (declare (ignore errors))
    (let ((driver (%make-driver :pid pid :process process :input input :output output :log log)))
      (setf driver.send (lambda (line)
                          (write-line line input)
                          (finish-output input)))
      (setf driver.reader
            (nlk:spawn "channel-whatsapp-driver-read"
              (handler-case
                  (loop for line = (read-line output nil)
                        while line
                        do (driver-accept-line driver line))
                (error (condition)
                  (warn "whatsapp: reading the driver failed: ~a" condition)))
              (setf driver.live nil)
              (driver-fail-pending driver "the WhatsApp driver exited")
              (enqueue-event driver (nlk:json-object "event" "exited"))))
      driver)))

(defun stop-driver (driver &key (grace 5))
  "End DRIVER's process: its stdin closed, which disconnects it cleanly,
then killed if it is still there after GRACE seconds."
  (when driver
    (setf driver.send nil)
    (when driver.input (ignore-errors (close driver.input)))
    (when driver.reader
      (loop repeat (* 10 grace)
            while (bt2:thread-alive-p driver.reader)
            do (sleep 0.1)))
    (when (and driver.pid (or (null driver.reader) (bt2:thread-alive-p driver.reader)))
      (nlk:kill-tree driver.pid))
    (when driver.pid
      (ignore-errors (nlk:wait-for-child driver.pid driver.process)))
    (when driver.output (ignore-errors (close driver.output)))
    (setf driver.live nil)
    (driver-fail-pending driver "the WhatsApp driver was stopped")
    t))

;;; --- the build ------------------------------------------------------------------

(defun on-path (program)
  "The file PROGRAM names in one of PATH's directories, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                            :separator (list (uiop:inter-directory-separator)))
        for candidate = (and (plusp (length directory))
                             (probe-file (merge-pathnames program (uiop:ensure-directory-pathname directory))))
        when candidate return candidate))

(defparameter +driver-sources+ '("main.go" "go.mod" "go.sum")
  "The files whose contents name a build: one changed, the binary is new.")

(defun driver-source-directory ()
  (merge-pathnames "driver/" (asdf:system-source-directory "nodecode-channel-whatsapp")))

(defun driver-digest (&optional (directory (driver-source-directory)))
  (nlk:short-digest (format nil "~{~a~^~%~}"
                            (loop for name in +driver-sources+
                                  for path = (merge-pathnames name directory)
                                  collect (if (probe-file path) (uiop:read-file-string path) "")))
                    12))

(defun driver-binary-path (&optional (digest (driver-digest)))
  (nlk:home (format nil "channels/whatsapp/bin/whatsapp-driver-~a~@[~a~]" digest
                    #+win32 ".exe" #-win32 nil)))

(defparameter +no-go+
  "WhatsApp's driver is built once from this cell's driver/ folder, and building it needs Go 1.24 or newer on PATH (https://go.dev/dl/). Install Go and restart the channel, or build the driver yourself (cd driver && go build) and set channels.whatsapp.driver_path to it.")

(defun ensure-driver-binary (&key path)
  "The driver to run: PATH when the section names one, else this cell's
driver/ built into the home -- once per version of its sources, the older
builds removed."
  (when (and path (plusp (length path)))
    (return-from ensure-driver-binary
      (or (probe-file path)
          (config-error "channels.whatsapp.driver_path ~a does not exist" path))))
  (let ((target (driver-binary-path)))
    (or (probe-file target)
        (let ((go (on-path "go"))
              (temp (format nil "~a.~d.tmp" (namestring target) (sb-posix:getpid))))
          (unless go (config-error "~a" +no-go+))
          (ensure-directories-exist target)
          (set-channel-status "whatsapp" :detail "building the WhatsApp driver (the first start only; a minute or two)")
          (multiple-value-bind (output errors code)
              (uiop:run-program (list "env" "CGO_ENABLED=0" (uiop:native-namestring go)
                                      "build" "-trimpath" "-o" temp ".")
                                :directory (driver-source-directory)
                                :output :string :error-output :string
                                :ignore-error-status t)
            (declare (ignore output))
            (unless (and (zerop code) (probe-file temp))
              (ignore-errors (delete-file temp))
              (error "building the WhatsApp driver failed (exit ~a): ~a" code
                     (nlk:clip (nlk:trimmed errors) 1500))))
          (rename-file temp target)
          ;; Older builds go: only this version of the sources runs.
          (dolist (old (directory (merge-pathnames "whatsapp-driver-*" target)))
            (unless (equal (file-namestring old) (file-namestring target))
              (ignore-errors (delete-file old))))
          (probe-file target)))))

;;; --- the executor ---------------------------------------------------------------

(defparameter +retry-delays+ '(0.5 1.5)
  "Seconds between a retried plan's attempts.")

(defun run-op (current plan &optional (args plan.body))
  "PLAN's op through the driver CURRENT answers, ARGS its body: an EXECUTION.
Retried, when the plan opts in, through a driver that is reconnecting (503)
or failed on WhatsApp's side (5xx); never through a refusal or a dead driver."
  (let ((op (string-left-trim "/" plan.path)))
    (loop for attempt from 1
          do (multiple-value-bind (status result error)
                 (driver-call (funcall current) op (request-body args)
                              :timeout plan.timeout-seconds)
               (let ((retry (and plan.retry-server-errors (>= status 500)
                                 (<= attempt (length +retry-delays+)))))
                 (cond ((= status 200)
                        (return (make-execution :ok-p t :status 200 :attempts attempt :body result)))
                       (retry (sleep (nth (1- attempt) +retry-delays+)))
                       (t (return (make-execution :ok-p nil :status status :attempts attempt
                                                  :error (format nil "whatsapp ~a failed~:[~; (status ~:*~a)~]: ~a"
                                                                 plan.audit-label
                                                                 (and (plusp status) status)
                                                                 error))))))))))

(defun request-body (args)
  "ARGS as the JSON object a request carries: a keyword plist (the model's
door) converted, a hash table as it is."
  (cond ((hash-table-p args) args)
        ((and (consp args) (keywordp (first args))) (nlk:plist-json args))
        ((null args) (nlk:json-object))
        (t args)))

(defun composite-p (plan)
  (and (hash-table-p plan.body)
       (or (gethash "__files" plan.body) (gethash "__poll" plan.body))))

(defun run-composite (current plan &optional on-sent)
  "A post that carries files or a poll beside its words: each file sent as
its own message, the first quoting what the post quotes; then the words;
then the poll under them. => the words' execution -- the message the post
is -- or, with no words, the last file's."
  (let* ((body (alexandria:copy-hash-table plan.body))
         (files (coerce (or (gethash "__files" body) #()) 'list))
         (poll (gethash "__poll" body))
         (quoted (gethash "quote" body))
         (chat (gethash "chat" body))
         (last nil)
         (words nil))
    (remhash "__files" body)
    (remhash "__poll" body)
    (dolist (file files)
      (let ((args (alexandria:copy-hash-table file)))
        (setf (gethash "chat" args) chat)
        (when quoted (setf (gethash "quote" args) quoted))
        (setf last (run-op current (make-request-plan :path "/send_file" :body args
                                                      :timeout-seconds (max 60 plan.timeout-seconds)
                                                      :retry-server-errors t :audit-label "send_file")
                           args))
        (unless (execution-ok-p last) (return-from run-composite last))
        (when on-sent (funcall on-sent "send_file" args last))))
    (when (plusp (length (nlk:trimmed (or (gethash "text" body) ""))))
      (setf words (run-op current plan body))
      (unless (execution-ok-p words) (return-from run-composite words))
      (when on-sent (funcall on-sent (string-left-trim "/" plan.path) body words)))
    (when poll
      (let* ((args (alexandria:copy-hash-table poll))
             (sent (progn (setf (gethash "chat" args) chat)
                          (run-op current (make-request-plan :path "/poll" :body args
                                                             :timeout-seconds plan.timeout-seconds
                                                             :retry-server-errors t :audit-label "send_poll")
                                  args))))
        (if (execution-ok-p sent)
            (when on-sent (funcall on-sent "poll" args sent))
            (warn "whatsapp: the choices under a post did not send: ~a" (execution-error sent)))))
    (or words last (make-execution :ok-p nil :status 400 :error "whatsapp: a post with nothing in it"))))

(defun make-driver-executor (current &key on-sent)
  "The executor every plan runs through: CURRENT answers the driver running
now (it is replaced when it restarts), a NIL plan -- a beat WhatsApp cannot
show -- is done, and a post carrying files or a poll runs as its messages.
ON-SENT, (args execution), hears every message that landed."
  (make-plan-executor
   :run (lambda (plan)
          (cond ((null plan) (make-execution :ok-p t :status 200 :attempts 0))
                ((composite-p plan) (run-composite current plan on-sent))
                (t (let ((result (run-op current plan)))
                     (when (and on-sent (execution-ok-p result) (hash-table-p plan.body))
                       (funcall on-sent (string-left-trim "/" plan.path) plan.body result))
                     result))))))
