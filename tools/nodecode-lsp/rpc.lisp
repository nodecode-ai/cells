;;;; rpc.lisp --- JSON-RPC over a child's stdin and stdout, framed by length.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A language server speaks JSON-RPC with a Content-Length header in front
;;;; of every message, and the length counts bytes. The kernel's spawn hands
;;;; back UTF-8 character streams that signal on a byte that does not decode,
;;;; so LAUNCH, the one spawn in the cell and the one name a test stubs,
;;;; trades each for a byte stream over a dup of its descriptor (closing the
;;;; character stream alone, whose finalizer would otherwise close the
;;;; descriptor under us). A body is read as exactly its length in bytes and
;;;; decoded with U+FFFD for whatever is not UTF-8, so one bad byte costs one
;;;; character, never the connection.
;;;;
;;;; One reader thread per connection routes what arrives. A message with a
;;;; `method' is the server's -- a request when it carries an id, which is
;;;; answered on the reader thread, else a notification -- and only a message
;;;; without one is a reply to us: the server's request ids live in their own
;;;; space and collide with ours (omp #3001, a basedpyright configuration
;;;; pull swallowed as the reply to a pending request). Three kinds of thread
;;;; write -- the turn thread, eval workers running verbs, the reader
;;;; answering the server -- so every write holds the connection's write
;;;; lock, and a wait that gives up tells the server with $/cancelRequest.
;;;;
;;;; Nothing here closes a stream another thread reads. CLOSE-CONNECTION
;;;; closes the write side; the reader leaves at end of file, which killing
;;;; the child gives it, and closes its own.

(in-package #:nodecode-lsp)

(defparameter +frame-limit+ (* 64 1024 1024)
  "The largest body a server may send, in bytes; a larger length drops the
connection rather than allocating it.")

(defparameter +replacement+ (code-char #xfffd)
  "What a body byte that is not UTF-8 decodes to.")

(define-condition rpc-refusal (error)
  ((code :initarg :code :reader rpc-refusal-code)
   (message :initarg :message :reader rpc-refusal-message))
  (:report (lambda (condition stream)
             (format stream "~a (~d)" (rpc-refusal-message condition) (rpc-refusal-code condition))))
  (:documentation "How a server request handler declines: the JSON-RPC error answered."))

(defstruct (connection (:conc-name conn-) (:copier nil))
  "One child speaking JSON-RPC: the pipes, the requests in flight, the reader."
  (label "lsp" :type string)
  pid process
  input                                 ; byte stream down the child's stdin
  output                                ; byte stream up from its stdout
  (write-lock (bt2:make-lock :name "lsp write"))
  (lock (bt2:make-lock :name "lsp replies"))
  (cv (bt2:make-condition-variable))
  (next-id 0)
  (pending (make-hash-table))           ; id -> REPLY
  (closed nil)
  reader
  on-request                            ; (lambda (method params)) => result
  on-notification                       ; (lambda (method params))
  on-close)                             ; (lambda ()), once the reader leaves

(defstruct (reply (:copier nil))
  "One request of ours: its id and, once answered, how."
  id method (state :waiting) result error)

;;; --- time ------------------------------------------------------------------

(defun deadline-after (seconds)
  "The internal real time SECONDS from now."
  (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))

(defun seconds-left (deadline)
  "Seconds until DEADLINE, never below zero."
  (max 0 (float (/ (- deadline (get-internal-real-time)) internal-time-units-per-second))))

(defun seconds-since (time)
  "Seconds since the internal real TIME."
  (float (/ (- (get-internal-real-time) time) internal-time-units-per-second)))

;;; --- the child ---------------------------------------------------------------

(defun byte-stream (stream direction label)
  "A byte stream over a dup of STREAM's descriptor, STREAM closed."
  (let ((fd (sb-posix:dup (sb-sys:fd-stream-fd stream))))
    (close stream)
    (sb-sys:make-fd-stream fd direction t :element-type '(unsigned-byte 8)
                                          :buffering :full :auto-close t :name label)))

(defun launch (argv &key directory error-log)
  "Spawn ARGV in DIRECTORY with both pipes ours, as byte streams, and stderr
appended to ERROR-LOG. => (values PID OUTPUT INPUT PROCESS)."
  ;; Through the kernel's posix_spawn, not a fork: a fork caught by a
  ;; collection while it held the allocator's locks froze the whole image
  ;; (the MCP cell, 2026-09-22). The child leads its own process group, so
  ;; KILL-TREE reaches what it starts.
  (nlk:bind (((pid output _ process input)
              (nlk:spawn-program argv :directory directory :input :stream :error-output error-log)))
    (values pid
            (byte-stream output :input "lsp stdout")
            (byte-stream input :output "lsp stdin")
            process)))

(defun open-connection (argv &key directory error-log (label (first argv))
                                  on-request on-notification on-close)
  "Launch ARGV and start reading it. => the CONNECTION."
  (multiple-value-bind (pid output input process)
      (launch argv :directory directory :error-log error-log)
    (let ((conn (make-connection :label label :pid pid :process process
                                 :input input :output output
                                 :on-request on-request :on-notification on-notification
                                 :on-close on-close)))
      (setf (conn-reader conn) (nlk:spawn (format nil "lsp ~a" label) (reader-loop conn)))
      conn)))

(defun close-connection (conn)
  "Close CONN's write side; the reader closes its own at end of file."
  (bt2:with-lock-held ((conn-write-lock conn))
    (setf (conn-closed conn) t)
    (ignore-errors (close (conn-input conn))))
  t)

;;; --- framing -----------------------------------------------------------------

(defun header-length (line)
  "The length a header LINE gives, or NIL when it is another header."
  (let ((colon (position #\: line)))
    (and colon
         (string-equal "content-length" (string-trim " " (subseq line 0 colon)))
         (parse-integer line :start (1+ colon) :junk-allowed t))))

(defun read-header (stream)
  "The Content-Length of the next header block on STREAM, :EOF, or NIL for a
block that carried none (junk a wrapper printed, skipped)."
  ;; Lines end at LF, a CR before it dropped. Blank lines before a block are
  ;; skipped; the blank line after one ends it.
  (let ((line (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (length nil)
        (lines 0)
        (count 0))
    (loop
      (let ((byte (read-byte stream nil :eof)))
        (cond ((eq byte :eof) (return :eof))
              ((> (incf count) 65536) (return nil))
              ((/= byte 10) (vector-push-extend byte line))
              (t (let ((text (string-right-trim '(#\Return)
                                                (sb-ext:octets-to-string line :external-format :latin-1))))
                   (setf (fill-pointer line) 0)
                   (cond ((plusp (length text))
                          (incf lines)
                          (let ((given (header-length text)))
                            (when given (setf length given))))
                         ((plusp lines) (return length))))))))))

(defun read-message (stream)
  "The next message on STREAM, a decoded JSON object; :EOF at the end; :JUNK
for a block that is not one."
  (let ((length (read-header stream)))
    (cond ((eq length :eof) :eof)
          ((null length) :junk)
          ((or (minusp length) (> length +frame-limit+)) :eof)
          (t (let ((body (make-array length :element-type '(unsigned-byte 8))))
               (if (< (read-sequence body stream) length)
                   :eof
                   (let ((value (ignore-errors
                                 (nlk:decode-json
                                  (sb-ext:octets-to-string
                                   body :external-format (list :utf-8 :replacement +replacement+))))))
                     (if (hash-table-p value) value :junk))))))))

(defun write-message (conn object)
  "Send OBJECT down CONN, framed. Refuses on a closed connection."
  (let* ((body (nlk:encode-json-octets object))
         (header (sb-ext:string-to-octets
                  (format nil "Content-Length: ~d~c~c~c~c" (length body)
                          #\Return #\Newline #\Return #\Newline)
                  :external-format :latin-1)))
    (bt2:with-lock-held ((conn-write-lock conn))
      (when (conn-closed conn)
        (fail "~a is not running" (conn-label conn)))
      (handler-case
          (let ((out (conn-input conn)))
            (write-sequence header out)
            (write-sequence body out)
            (finish-output out))
        (error (condition)
          (setf (conn-closed conn) t)
          (fail "writing to ~a failed: ~a" (conn-label conn) condition))))))

;;; --- reading -----------------------------------------------------------------

(defun reader-loop (conn &aux (stream (conn-output conn)))
  "Route every message CONN's child sends until end of file."
  ;; LISTEN first: a read takes the pipe's whole buffer, so a second message
  ;; can sit in the stream while the descriptor reads idle. The wait wakes
  ;; once a second to notice a connection closed under a child that keeps
  ;; its stdout open.
  (unwind-protect
       (ignore-errors
        (loop with fd = (sb-sys:fd-stream-fd stream)
              until (conn-closed conn)
              do (when (or (listen stream) (sb-sys:wait-until-fd-usable fd :input 1))
                   (let ((message (read-message stream)))
                     (case message
                       (:eof (return))
                       (:junk)
                       (t (dispatch conn message)))))))
    (finish-connection conn)))

(defun finish-connection (conn)
  "The reader's way out: every pending request answered closed, its stream
closed, the owner told."
  (bt2:with-lock-held ((conn-lock conn))
    (setf (conn-closed conn) t)
    (loop for reply being the hash-values of (conn-pending conn)
          do (setf (reply-state reply) :closed))
    (clrhash (conn-pending conn))
    (bt2:condition-broadcast (conn-cv conn)))
  (ignore-errors (close (conn-output conn)))
  (when (conn-on-close conn)
    (ignore-errors (funcall (conn-on-close conn)))))

(defun dispatch (conn message)
  "Route one MESSAGE: the server's request, its notification, or our reply."
  (multiple-value-bind (id has-id) (gethash "id" message)
    (let ((method (nlk:json-value message :string "method"))
          (params (gethash "params" message)))
      (cond ((and method has-id (not (eq id :null)))
             (answer conn id method params))
            (method
             (when (conn-on-notification conn)
               (ignore-errors (funcall (conn-on-notification conn) method params))))
            ((integerp id)
             (bt2:with-lock-held ((conn-lock conn))
               (let ((reply (gethash id (conn-pending conn))))
                 (when reply
                   (remhash id (conn-pending conn))
                   (let ((error (gethash "error" message)))
                     (if (hash-table-p error)
                         (setf (reply-error reply) error (reply-state reply) :error)
                         (setf (reply-result reply) (gethash "result" message)
                               (reply-state reply) :ok)))
                   (bt2:condition-broadcast (conn-cv conn))))))))))

(defun answer (conn id method params)
  "Answer the server's request ID: the handler's result, or the error it declined with."
  (let ((response
          (handler-case
              (let ((result (if (conn-on-request conn)
                                (funcall (conn-on-request conn) method params)
                                (error 'rpc-refusal :code -32601 :message "method not found"))))
                (nlk:json-object "jsonrpc" "2.0" "id" id "result" (or result :null)))
            (rpc-refusal (condition)
              (nlk:json-object "jsonrpc" "2.0" "id" id
                               "error" (nlk:json-object "code" (rpc-refusal-code condition)
                                                        "message" (rpc-refusal-message condition))))
            (error (condition)
              (nlk:json-object "jsonrpc" "2.0" "id" id
                               "error" (nlk:json-object "code" -32603
                                                        "message" (princ-to-string condition)))))))
    (ignore-errors (write-message conn response))))

;;; --- our requests --------------------------------------------------------------

(defun notify (conn method &optional params)
  "Send the notification METHOD with PARAMS."
  (write-message conn (nlk:json-object "jsonrpc" "2.0" "method" method :opt "params" params)))

(defun request-async (conn method &optional params)
  "Send the request METHOD and answer its REPLY at once, still :WAITING."
  (let ((reply (make-reply :method method)))
    (bt2:with-lock-held ((conn-lock conn))
      (setf (reply-id reply) (incf (conn-next-id conn))
            (gethash (reply-id reply) (conn-pending conn)) reply))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (bt2:with-lock-held ((conn-lock conn))
                              (remhash (reply-id reply) (conn-pending conn))))))
      (write-message conn (nlk:json-object "jsonrpc" "2.0" "id" (reply-id reply)
                                           "method" method :opt "params" params)))
    reply))

(defun await-reply (conn reply deadline)
  "Wait until REPLY is answered or DEADLINE passes; => its state."
  (bt2:with-lock-held ((conn-lock conn))
    (loop (let ((left (seconds-left deadline)))
            (unless (and (eq :waiting (reply-state reply)) (plusp left))
              (return))
            (bt2:condition-wait (conn-cv conn) (conn-lock conn) :timeout (min 1 left)))))
  (reply-state reply))

(defun abandon (conn reply)
  "Stop waiting for REPLY and tell the server so."
  (when (eq :waiting (reply-state reply))
    (bt2:with-lock-held ((conn-lock conn))
      (remhash (reply-id reply) (conn-pending conn))
      (setf (reply-state reply) :abandoned))
    (ignore-errors (notify conn "$/cancelRequest" (nlk:json-object "id" (reply-id reply))))))

(defun reply-detail (reply)
  "REPLY's error as one line: the message and its code."
  (let ((error (reply-error reply)))
    (format nil "~a~@[ (~a)~]"
            (or (nlk:json-value error :string "message") "error")
            (nlk:json-value error :integer "code"))))

(defun call (conn method params &key (seconds 8))
  "Ask CONN METHOD with PARAMS and wait at most SECONDS.
=> (values RESULT STATE DETAIL): STATE :OK, :ERROR (DETAIL the server's
message), :CLOSED or :TIMEOUT."
  ;; A wait cut short -- a timeout, Esc's unwind -- cancels the request.
  (let ((reply (request-async conn method params))
        (finished nil))
    (unwind-protect
         (let ((state (await-reply conn reply (deadline-after seconds))))
           (setf finished t)
           (case state
             (:ok (values (reply-result reply) :ok nil))
             (:error (values nil :error (reply-detail reply)))
             (:closed (values nil :closed nil))
             (t (abandon conn reply) (values nil :timeout nil))))
      (unless finished
        (abandon conn reply)))))
