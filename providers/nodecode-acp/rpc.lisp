;;;; rpc.lisp --- one agent process, spoken to in JSON-RPC over stdio.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An ACP agent is a child process: one JSON-RPC 2.0 message per line on its
;;;; stdin and stdout, stderr appended to a log the refusals quote. A reader
;;;; thread takes every line the agent writes: a reply goes to the BOX its
;;;; request waits on, a notification or a request of the agent's own
;;;; (session/update, session/request_permission) to the SINK the agent's
;;;; session id names -- the running turn's queue. Nothing here blocks the
;;;; reader: a request a sink cannot take at once is answered later, by the
;;;; RESPOND function it is handed.

(in-package #:nodecode-acp)

(define-condition acp-error (error)
  ((detail :initarg :detail :reader acp-error-detail)
   (code :initarg :code :initform nil :reader acp-error-code))
  (:report (lambda (condition stream) (write-string (acp-error-detail condition) stream))))

(defstruct (box (:constructor make-box ()))
  "Where one request's reply lands: the response object, or (:CLOSED . WHY)."
  (semaphore (bt2:make-semaphore :name "acp reply"))
  (reply nil))

(defstruct (connection (:conc-name conn-))
  "One running agent."
  (name "" :type string)
  (argv '() :type list)
  (pid nil)
  (process nil)
  (input nil)
  (output nil)
  (log nil)
  (write-lock (bt2:make-lock :name "acp write"))
  (lock (bt2:make-lock :name "acp state"))
  (next-id 0 :type integer)
  (pending (make-hash-table))
  (sinks (make-hash-table :test 'equal))
  (reader nil)
  (agent nil)
  (closed nil)
  (used (get-universal-time)))

(defun launch (argv &key directory environment error-log)
  "Spawn ARGV in DIRECTORY with both pipes ours and stderr appended to
ERROR-LOG. => (values PID OUTPUT INPUT PROCESS)."
  ;; The one process spawn in the cell, so a test stubs this name. Through
  ;; the kernel's spawn: the child leads its own group, so KILL-TREE reaches
  ;; the node or rust process an npx shim starts under it.
  (nlk:bind (((pid output _ process input)
              (nlk:spawn-program argv :input :stream :error-output error-log
                                      :directory directory :environment environment)))
    (values pid output input process)))

(defun log-tail (conn &optional (lines 3))
  "The last LINES lines the agent wrote on stderr, one line, or NIL."
  (let ((path (conn-log conn)))
    (when (and path (probe-file path))
      (let* ((text (ignore-errors (uiop:read-file-string path)))
             (all (remove "" (uiop:split-string (or text "") :separator '(#\Newline))
                          :test #'string= :key (lambda (line) (string-trim " 	" line)))))
        ;; The log is the session's, across launches: each opens with its
        ;; `-- command' line, and only what the running one wrote counts.
        (let ((since (let ((at (position-if (lambda (line) (uiop:string-prefix-p "-- " line)) all :from-end t)))
                       (if at (nthcdr (1+ at) all) all))))
          (when since
            (format nil "~{~a~^ / ~}" (last since lines))))))))

(defun send (conn object)
  "Write OBJECT to the agent as one line."
  (let ((line (nlk:encode-json-object object)))
    (bt2:with-lock-held ((conn-write-lock conn))
      (when (conn-closed conn)
        (error 'acp-error :detail (conn-closed conn)))
      (handler-case
          (let ((stream (conn-input conn)))
            (write-string line stream)
            (write-char #\Newline stream)
            (finish-output stream))
        (error (condition)
          (error 'acp-error :detail (format nil "~a stopped reading: ~a" (conn-name conn) condition)))))))

(defun notify (conn method params)
  "Send the notification METHOD with PARAMS."
  (send conn (nle::json-object "jsonrpc" "2.0" "method" method "params" params)))

(defun request (conn method params)
  "Send the request METHOD with PARAMS. => the BOX its reply lands in."
  (let ((box (make-box)) (id 0))
    (bt2:with-lock-held ((conn-lock conn))
      (setf id (incf (conn-next-id conn))
            (gethash id (conn-pending conn)) box))
    (handler-case (send conn (nle::json-object "jsonrpc" "2.0" "id" id "method" method "params" params))
      (acp-error (condition)
        (bt2:with-lock-held ((conn-lock conn)) (remhash id (conn-pending conn)))
        (error condition)))
    box))

(defun box-done-p (box)
  "Whether BOX's reply came."
  (not (null (box-reply box))))

(defun reply-result (conn reply)
  "The result REPLY carries, or ACP-ERROR saying why there is none."
  (cond ((and (consp reply) (eq (car reply) :closed))
         (error 'acp-error :detail (cdr reply)))
        ((hash-table-p (gethash "error" reply))
         (let* ((error (gethash "error" reply))
                (message (gethash "message" error))
                (data (gethash "data" error)))
           (error 'acp-error
                  :code (gethash "code" error)
                  :detail (format nil "~a answered: ~a~@[ (~a)~]" (conn-name conn)
                                  (if (stringp message) message "an error")
                                  (cond ((stringp data) data)
                                        ((hash-table-p data)
                                         (let ((said (gethash "message" data)))
                                           (if (stringp said) said (nlk:encode-json-object data)))))))))
        (t (gethash "result" reply))))

(defun await (conn box &key (seconds 60) poll)
  "Wait for BOX's reply, calling POLL every 0.2 s, and answer its result.
ACP-ERROR when it carries an error, the agent closed, or SECONDS pass."
  (loop with deadline = (and seconds (+ (get-internal-real-time)
                                        (* seconds internal-time-units-per-second)))
        until (or (box-done-p box) (bt2:wait-on-semaphore (box-semaphore box) :timeout 0.2))
        do (when poll (funcall poll))
           (when (and deadline (> (get-internal-real-time) deadline))
             (error 'acp-error :detail (format nil "~a did not answer in ~a s" (conn-name conn) seconds))))
  (reply-result conn (box-reply box)))

(defun call (conn method params &key (seconds 60) poll)
  "Request METHOD and wait for its result (AWAIT)."
  (await conn (request conn method params) :seconds seconds :poll poll))

(defun respond (conn id result)
  "Answer the agent's request ID with RESULT."
  (ignore-errors (send conn (nle::json-object "jsonrpc" "2.0" "id" id "result" result))))

(defun respond-error (conn id code text)
  "Refuse the agent's request ID."
  (ignore-errors
   (send conn (nle::json-object "jsonrpc" "2.0" "id" id
                                "error" (nle::json-object "code" code "message" text)))))

(defun sink-for (conn params)
  "The sink of the agent session PARAMS names, or NIL."
  (let ((session (and (hash-table-p params) (gethash "sessionId" params))))
    (and (stringp session)
         (bt2:with-lock-held ((conn-lock conn)) (gethash session (conn-sinks conn))))))

(defun dispatch (conn message)
  "Route one line the agent wrote."
  (let ((id (gethash "id" message))
        (method (gethash "method" message))
        (params (gethash "params" message)))
    (cond
      ((and (stringp method) id (not (eq id :null)))
       (let ((sink (sink-for conn params)))
         (cond
           ((and sink (string= method "session/request_permission"))
            (funcall sink (list :permission params
                                (lambda (result) (respond conn id result)))))
           ((string= method "session/request_permission")
            ;; Nobody is running a turn on that session: nobody can say yes.
            (respond conn id (nle::json-object "outcome" (nle::json-object "outcome" "cancelled"))))
           (t (respond-error conn id -32601 (format nil "Nodecode does not offer ~a" method))))))
      ((stringp method)
       (when (string= method "session/update")
         (let ((sink (sink-for conn params))
               (update (gethash "update" params)))
           (when (and sink (hash-table-p update))
             (funcall sink (list :update update))))))
      ((and id (not (eq id :null)))
       (let ((box (bt2:with-lock-held ((conn-lock conn))
                    (prog1 (gethash id (conn-pending conn))
                      (remhash id (conn-pending conn))))))
         (when box
           (setf (box-reply box) message)
           (bt2:signal-semaphore (box-semaphore box))))))))

(defun read-loop (conn)
  "The reader thread: every line the agent writes, until it closes."
  (let ((why nil))
    (handler-case
        (loop for line = (read-line (conn-output conn) nil nil)
              while line
              do (let ((message (ignore-errors (nlk:decode-json line))))
                   (when (hash-table-p message)
                     (with-simple-restart (skip "Skip this message")
                       (handler-bind ((error (lambda (condition)
                                               (declare (ignore condition))
                                               (invoke-restart 'skip))))
                         (dispatch conn message))))))
      (error (condition) (setf why (princ-to-string condition))))
    (close-connection conn (format nil "~a exited~@[: ~a~]" (conn-name conn)
                                   (or (log-tail conn) why)))))

(defun close-connection (conn why)
  "Close CONN once: every waiting request and sink is told WHY, and the
agent's process tree is stopped."
  (let (boxes sinks)
    (bt2:with-lock-held ((conn-lock conn))
      (when (conn-closed conn) (return-from close-connection nil))
      (setf (conn-closed conn) why)
      (maphash (lambda (id box) (declare (ignore id)) (push box boxes)) (conn-pending conn))
      (clrhash (conn-pending conn))
      (maphash (lambda (id sink) (declare (ignore id)) (push sink sinks)) (conn-sinks conn)))
    (dolist (box boxes)
      (setf (box-reply box) (cons :closed why))
      (bt2:signal-semaphore (box-semaphore box)))
    (dolist (sink sinks) (ignore-errors (funcall sink (list :closed why))))
    (ignore-errors (close (conn-input conn)))
    (when (conn-pid conn) (ignore-errors (nlk:kill-tree (conn-pid conn))))
    (when (conn-pid conn)
      (let ((pid (conn-pid conn)) (process (conn-process conn)))
        (bt2:make-thread (lambda () (ignore-errors (nlk:wait-for-child pid process)))
                         :name "acp reap")))
    t))

(defun open-connection (name argv &key directory environment log)
  "Start the agent ARGV under NAME in DIRECTORY and initialize it.
=> the CONNECTION, its CONN-AGENT the initialize result."
  (when log
    (ensure-directories-exist log)
    (with-open-file (out log :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out "~&-- ~{~a~^ ~}~%" argv)))
  (multiple-value-bind (pid output input process)
      (handler-case (launch argv :directory directory :environment environment :error-log log)
        (error (condition)
          (error 'acp-error :detail (format nil "~a could not start (~{~a~^ ~}): ~a" name argv condition))))
    (let ((conn (make-connection :name name :argv argv :pid pid :process process
                                 :input input :output output :log log)))
      (setf (conn-reader conn)
            (bt2:make-thread (lambda () (read-loop conn)) :name (format nil "acp ~a" name)))
      (handler-case
          (setf (conn-agent conn)
                (call conn "initialize"
                      (nle::json-object
                       "protocolVersion" +protocol-version+
                       ;; The agent keeps its own tools: it reads and writes the
                       ;; session's folder and runs its commands itself, asking
                       ;; first where its own rules say so.
                       "clientCapabilities" (nle::json-object
                                             "fs" (nle::json-object "readTextFile" :false
                                                                    "writeTextFile" :false)
                                             "terminal" :false)
                       "clientInfo" (nle::json-object "name" "nodecode" "title" "Nodecode"))
                      :seconds (setting :start-seconds)))
        (acp-error (condition)
          (close-connection conn (acp-error-detail condition))
          (error condition)))
      conn)))
