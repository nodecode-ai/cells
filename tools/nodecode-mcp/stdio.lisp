;;;; stdio.lisp --- a child process speaking newline-delimited JSON-RPC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The stdio transport: one frame per line on the child's stdin and
;;;; stdout. The child's stderr goes to an append-only file under
;;;; *LOG-DIRECTORY* — never to this image's fd 2 (severed to a log while a
;;;; TUI owns the terminal) and never to /dev/null (the first question about
;;;; a server that dies at startup is "what did it print").
;;;;
;;;; Reads are bounded without a reader thread: READ-CHAR-NO-HANG drains
;;;; what the stream holds, and when it holds nothing the thread parks in
;;;; SB-SYS:WAIT-UNTIL-FD-USABLE for what remains of the deadline. A plain
;;;; READ-LINE after the wait would still block on a half-written line; the
;;;; partial line a deadline interrupts is kept on the transport and resumed
;;;; by the next read, so a late reply never turns into garbage.
;;;;
;;;; The single spawn goes through LAUNCH, the one name a test stubs.
;;;;
;;;; A command that signs in on its own -- mcp-remote and its kind, a remote
;;;; server behind a local process -- prints the address to open on its stderr
;;;; and holds every request until a browser comes back to its loopback
;;;; listener: a wait that would only end at its deadline. So a read that waits
;;;; looks at what the child wrote to its stderr log every
;;;; *SIGN-IN-POLL-SECONDS*, and once an OAuth authorization address has stood
;;;; there *SIGN-IN-SETTLE-SECONDS* without an answer, the read ends with
;;;; AWAITS-SIGN-IN. The child is kept: it is the one the browser answers.

(in-package #:nodecode-mcp)

(defparameter *close-grace-seconds* 1
  "How long CLOSE waits for the child to exit on its own after stdin closes,
and again after SIGTERM, before SIGKILL.")

(defstruct (stdio-transport (:copier nil))
  pid                                   ; the child, leading a process group
  process                               ; its SB-EXT:PROCESS on the fork path
  (exit nil)                            ; its exit status, once reaped
  input
  output
  log-path
  (label "stdio" :type string)
  (partial nil)                         ; a line a deadline cut short
  (log-mark nil)                        ; octets of the stderr log already looked at
  (asked nil)                           ; (URL REDIRECT SEEN) an address printed, not yet answered
  (closed-p nil))

(defparameter *sign-in-poll-seconds* 0.5
  "How often a read waiting on a process looks at what it wrote to its stderr.")

(defparameter *sign-in-settle-seconds* 2
  "How long an authorization address stands on a process's stderr with no
answer before the read takes the process for one waiting on a browser.")

(defun log-end (path)
  "PATH's length in octets, 0 when it cannot be read."
  (or (ignore-errors (sb-posix:stat-size (sb-posix:stat (namestring path)))) 0))

(defun authorization-request (line)
  "The OAuth authorization address on LINE -- an http(s) URL whose query
asks response_type=code of a client_id -- as (values URL REDIRECT-URI), or NIL."
  (loop for start = (search "http" line) then (search "http" line :start2 (1+ start))
        while start
        do (let* ((end (or (position-if (lambda (char) (member char '(#\Space #\Tab #\" #\' #\< #\>)))
                                        line :start start)
                           (length line)))
                  (url (string-right-trim ".,;)" (subseq line start end)))
                  (params (and (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url))
                               (ignore-errors (quri:uri-query-params (quri:uri url))))))
             (flet ((param (key) (cdr (assoc key params :test #'string=))))
               (when (and (equal (param "response_type") "code") (param "client_id"))
                 (return (values url (param "redirect_uri"))))))))

(defun sign-in-asked (transport &aux (path (stdio-transport-log-path transport))
                                     (mark (stdio-transport-log-mark transport)))
  "Note the last authorization address TRANSPORT's child printed on its stderr
since the last look; whole lines only, so an address half written waits."
  (let ((end (if mark (log-end path) 0)))
    (when (and mark (> end mark))
      ;; A server that floods its log is read from its last 64 KiB.
      (let* ((from (max mark (- end 65536)))
             (octets (ignore-errors
                      (with-open-file (in path :element-type '(unsigned-byte 8))
                        (file-position in from)
                        (let ((octets (make-array (- end from) :element-type '(unsigned-byte 8))))
                          (subseq octets 0 (read-sequence octets in))))))
             (cut (and octets (position 10 octets :from-end t))))
        (when cut
          (setf (stdio-transport-log-mark transport) (+ from cut 1))
          (dolist (line (nlk:lines (sb-ext:octets-to-string octets :end cut
                                                                   :external-format '(:utf-8 :replacement #\?))))
            (multiple-value-bind (url redirect) (authorization-request line)
              (when url
                (setf (stdio-transport-asked transport) (list url redirect (now-seconds)))))))))))

(defun check-sign-in (transport)
  "Signal AWAITS-SIGN-IN once an address TRANSPORT's child printed has stood
*SIGN-IN-SETTLE-SECONDS* with no answer."
  (sign-in-asked transport)
  (nlk:when-let (asked (stdio-transport-asked transport))
    (destructuring-bind (url redirect seen) asked
      (when (>= (- (now-seconds) seen) *sign-in-settle-seconds*)
        (setf (stdio-transport-asked transport) nil)
        (error 'awaits-sign-in :url url :redirect redirect)))))

(defun launch (argv &key environment error-log)
  "Spawn ARGV with both pipes ours and stderr appended to ERROR-LOG.
=> (values PID OUTPUT INPUT PROCESS)."
  ;; The one process spawn in the cell, so a test stubs this name. Through
  ;; the kernel's spawn, not a fork: the cell starts every server at once,
  ;; and a fork caught by a collection while it held the allocator's locks
  ;; would freeze the whole image — its MCP-connecting neighbour waiting on
  ;; those locks, the collection waiting on the neighbour.
  (nlk:bind (((pid output _ process input)
              (nlk:spawn-program argv :input :stream :error-output error-log
                                      :environment environment)))
    (values pid output input process)))

(defun start-stdio (spec allowlist)
  "Launch SPEC's command; a STDIO-TRANSPORT, or TRANSPORT-CLOSED naming why
(REFERENCE-UNRESOLVED, before any launch, for an env reference that names nothing)."
  (let ((log-path (stderr-log-path spec.name)) (argv (cons spec.command spec.args))
        (environment (child-environment spec allowlist)) (mark 0))
    (handler-case
        (progn
          ;; Create the log (:APPEND opens without O_CREAT) and stamp the launch.
          (ensure-directories-exist log-path)
          (with-open-file (out log-path :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :create)
            (format out "~&;; mcp ~a: launched ~a ~a~{ ~a~}~%"
                    spec.name (nlk:iso-time (nlk:unix-now) :millis nil) spec.command spec.args))
          ;; What the child writes is read from here on, for an address it asks a browser to open.
          (setf mark (log-end log-path))
          (multiple-value-bind (pid output input process)
              (launch argv
                      :environment environment
                      :error-log log-path)
            (make-stdio-transport
             :pid pid
             :process process
             :input input
             :output output
             :log-path log-path
             :log-mark mark
             :label (format nil "stdio ~a" spec.command))))
      (error (condition)
        ;; The spawn's own words name the command again ("spawning X: ..."): it is said once.
        (let* ((said (princ-to-string condition))
               (again (format nil "spawning ~a" spec.command))
               (cut (and (eql 0 (search again said)) (search ": " said :start2 (length again)))))
          (error 'transport-closed :detail (format nil "cannot launch ~a: ~a" spec.command
                                                   (if cut (subseq said (+ cut 2)) said))))))))

(defmethod transport-send ((transport stdio-transport) text deadline)
  (declare (ignore deadline))
  (when (stdio-transport-closed-p transport)
    (error 'transport-closed :detail "process closed"))
  (check-frame text)
  (handler-case
      (let ((stream (stdio-transport-input transport)))
        (write-string text stream)
        (write-char #\Newline stream)
        (finish-output stream))
    (error (condition)
      (error 'transport-closed
             :detail (format nil "write to the process failed (~a); ~a"
                             condition (exit-text transport))))))

(defun read-frame (transport deadline &aux (stream (stdio-transport-output transport))
                                           (fd (sb-sys:fd-stream-fd stream))
                                           (buffer (make-string-output-stream))
                                           (count 0))
  "One line from the child, or :TIMEOUT, or :CLOSED at EOF."
  (when (stdio-transport-partial transport)
    (write-string (stdio-transport-partial transport) buffer)
    (setf count (length (stdio-transport-partial transport))
          (stdio-transport-partial transport) nil))
  (loop
    (let ((char (read-char-no-hang stream nil :eof)))
      (cond
        ((eq char :eof) (return :closed))
        ((null char)
         (let ((remaining (seconds-remaining deadline)))
           (flet ((keep-partial ()
                    (setf (stdio-transport-partial transport) (get-output-stream-string buffer))))
             (cond ((zerop remaining) (keep-partial) (return :timeout))
                   ((sb-sys:wait-until-fd-usable fd :input (min remaining *sign-in-poll-seconds*)))
                   (t (handler-bind ((awaits-sign-in (lambda (condition)
                                                       (declare (ignore condition))
                                                       (keep-partial))))
                        (check-sign-in transport)))))))
        ((char= char #\Newline)
         ;; An answer: an address printed before it was not a wait on a browser.
         (setf (stdio-transport-asked transport) nil)
         (when (stdio-transport-log-mark transport)
           (setf (stdio-transport-log-mark transport) (log-end (stdio-transport-log-path transport))))
         (return (get-output-stream-string buffer)))
        (t
         (write-char char buffer)
         (when (> (incf count) +frame-limit+)
           (error 'transport-closed
                  :detail "frame over the 1 MiB limit; connection dropped")))))))

(defmethod transport-receive ((transport stdio-transport) deadline)
  (when (stdio-transport-closed-p transport)
    (return-from transport-receive :closed))
  (loop
    (let ((frame (handler-case (read-frame transport deadline)
                   (transport-closed (condition) (error condition))
                   (awaits-sign-in (condition) (error condition))
                   (error (condition)
                     (error 'transport-closed
                            :detail (format nil "read from the process failed (~a); ~a"
                                            condition (exit-text transport)))))))
      (cond
        ((keywordp frame) (return frame))
        ((zerop (length (string-trim '(#\Space #\Tab #\Return) frame))))
        (t
         (let ((message (handler-case (nlk:decode-json frame) (error () :unreadable))))
           (if (hash-table-p message)
               (return message)
               (warn "mcp ~a: skipped a line that is not a JSON object: ~a"
                     (stdio-transport-label transport)
                     (if (> (length frame) 80) (subseq frame 0 80) frame)))))))))

(defun child-exited (transport)
  "TRANSPORT's child's exit status once it has exited, else NIL."
  (or (stdio-transport-exit transport)
      (and (stdio-transport-pid transport)
           (setf (stdio-transport-exit transport)
                 (nlk:child-exit (stdio-transport-pid transport)
                                 (stdio-transport-process transport))))))

(defun exit-text (transport)
  "\"still running\", or how the child ended, for an error line."
  (cond ((null (stdio-transport-pid transport)) "no process")
        ((null (child-exited transport)) "process still running")
        (t (format nil "process exited with ~a; see ~a"
                   (stdio-transport-exit transport)
                   (namestring (stdio-transport-log-path transport))))))

(defun await-exit (transport seconds)
  (loop repeat (ceiling (* seconds 10))
        until (child-exited transport)
        do (sleep 0.1)))

(defmethod transport-close ((transport stdio-transport))
  (unless (stdio-transport-closed-p transport)
    (setf (stdio-transport-closed-p transport) t)
    (let ((pid (stdio-transport-pid transport)))
      (ignore-errors (close (stdio-transport-input transport)))
      (when pid
        ;; Stdin closed, then SIGTERM to the child's group, then the whole
        ;; group killed: a server npx or uvx started is a tree, not a process.
        (await-exit transport *close-grace-seconds*)
        (unless (child-exited transport)
          #-win32 (ignore-errors (sb-posix:kill (- pid) sb-posix:sigterm))
          (await-exit transport *close-grace-seconds*))
        (unless (child-exited transport)
          (nlk:kill-tree pid)
          (await-exit transport *close-grace-seconds*))
        (ignore-errors (close (stdio-transport-output transport)))))
    t))
