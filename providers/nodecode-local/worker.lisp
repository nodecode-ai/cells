;;;; worker.lisp --- one tiny-model worker: find it on its socket, start it, ask it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi's coding-agent/src/tiny/title-client.ts
;;;; (probeTinyWorker, connectTinyWorker, onnxLaunch, mlxLaunch, chat) and
;;;; jsonl-socket.ts. The protocol (title-protocol.ts), one JSON object per
;;;; line over the worker's Unix socket:
;;;;
;;;;   {"type": "ping", "id"}                          -> {"type": "pong", "id", "tag"}
;;;;   {"type": "chat", "id", "messages": [{role, content}], "maxNewTokens"}
;;;;                                                   -> {"type": "progress", ...}* then
;;;;                                                      {"type": "text", "id", "text"}
;;;;                                                      | {"type": "error", "id", "error"}
;;;;
;;;; A worker renders the chat template with the model's own tokenizer and
;;;; answers the generated text whole, greedily, with thinking turned off.
;;;; omp replaces a worker whose launch tag is not its own; this cell takes any
;;;; worker that answers a ping, since every one speaks the same protocol, and
;;;; omp in turn replaces one this cell started the next time it looks.

(in-package #:nodecode-local)

(defparameter +probe-seconds+ 3
  "How long a ping may wait for its pong (PROBE_TIMEOUT_MS).")

(defparameter +ready-seconds+ 120
  "How long a started worker may take to bind its socket (READY_TIMEOUT_MS).")

(defparameter +launch-tag+ "nodecode-local"
  "The tag a worker this cell starts answers a ping with.")

;;; --- the socket ------------------------------------------------------------------------

(defun connect (path)
  "A character stream on the Unix socket at PATH, or NIL when nothing listens there."
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (handler-case
        (progn (sb-bsd-sockets:socket-connect socket (uiop:native-namestring path))
               (sb-bsd-sockets:socket-make-stream socket :input t :output t :element-type 'character
                                                         :external-format :utf-8 :buffering :full))
      (error ()
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        nil))))

(defun send-line (stream object)
  "Write OBJECT to STREAM as one JSON line."
  (write-string (nlk:encode-json-object object) stream)
  (terpri stream)
  (finish-output stream))

(defun read-reply (stream seconds)
  "The next JSON object STREAM sends within SECONDS, or NIL at its end; a
blank or unreadable line is passed over."
  (loop
    (let ((line (nlk:with-cancellable-wait (nle::*current-durable-turn*)
                  (sb-sys:with-deadline (:seconds seconds) (read-line stream nil nil)))))
      (unless line (return nil))
      (let ((object (ignore-errors (nlk:decode-json line))))
        (when (hash-table-p object) (return object))))))

(defun probe (path)
  "A live worker's stream at PATH, its ping answered, or NIL."
  (alexandria:when-let (stream (connect path))
    (or (handler-case
            (progn (send-line stream (nlk:json-object "type" "ping" "id" "probe"))
                   (let ((reply (read-reply stream +probe-seconds+)))
                     (and (equal "pong" (nlk:json-value reply :string "type")) stream)))
          ((or error sb-sys:deadline-timeout) () nil))
        (progn (ignore-errors (close stream :abort t)) nil))))

;;; --- starting one ------------------------------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun runtime-dir ()
  "Where the workers' sockets are: the section's runtime_dir, else omp's."
  (alexandria:if-let (dir (nonblank (setting :runtime-dir)))
    (uiop:ensure-directory-pathname dir)
    (default-runtime-dir)))

(defun backend ()
  "The worker backend a round asks for: the section's, else omp's own choice,
MLX when PI_TINY_DEVICE says mlx (or metal), else ONNX."
  (let ((chosen (setting :backend)))
    (cond ((equal chosen "onnx") :onnx)
          ((equal chosen "mlx") :mlx)
          ((member (nle::credential-env "PI_TINY_DEVICE") '("mlx" "metal") :test #'string-equal) :mlx)
          (t :onnx))))

(defun spawn-p (backend)
  "Whether this cell starts a BACKEND worker that is not running: the
section allows it, and for ONNX it names the omp binary to start."
  (and (setting :spawn)
       (or (eq backend :mlx) (nonblank (setting :omp-command)))))

(defun launch (model-id backend endpoint)
  "(values ARGV ENVIRONMENT) that start MODEL-ID's BACKEND worker on ENDPOINT
(onnxLaunch, mlxLaunch): the omp binary re-entered for ONNX, mlx-server.py
under the mlx-lm venv for MLX. ENVIRONMENT is what goes over this process's."
  (if (eq backend :mlx)
      (values (list (or (nonblank (setting :python)) (mlx-python) "python3")
                    "-u" (uiop:native-namestring
                          (asdf:system-relative-pathname "nodecode-local" "worker/mlx-server.py"))
                    "--socket" endpoint
                    "--tag" (format nil "~a|mlx|~a" +launch-tag+ +mlx-lm-version+)
                    "--model-key" model-id
                    "--repo" (mlx-repo model-id)
                    "--dir" (uiop:native-namestring (mlx-model-dir model-id))
                    "--idle-seconds" (princ-to-string +idle-seconds+))
              '(("PYTHONUNBUFFERED" . "1") ("PYTHONIOENCODING" . "utf-8") ("TRANSFORMERS_VERBOSITY" . "error")
                ("HF_HUB_DISABLE_PROGRESS_BARS" . "1") ("TOKENIZERS_PARALLELISM" . "false")))
      (values (list (nonblank (setting :omp-command)) +worker-arg+)
              `(("OMP_TINY_WORKER_SOCKET" . ,endpoint)
                ("OMP_TINY_WORKER_MODEL" . ,model-id)
                ("OMP_TINY_WORKER_TAG" . ,(format nil "~a|onnx" +launch-tag+))))))

(defun spawn-worker (argv environment log)
  "Start ARGV with ENVIRONMENT over this process's, its output to LOG, not
waited for: it outlives this process and exits on its own once idle.
=> the process."
  (sb-ext:run-program (first argv) (rest argv)
                      :search t :wait nil :input nil
                      :output (uiop:native-namestring log) :if-output-exists :supersede :error :output
                      :environment (append (loop for (name . value) in environment
                                                 collect (format nil "~a=~a" name value))
                                           (remove-if (lambda (entry)
                                                        (some (lambda (pair)
                                                                (uiop:string-prefix-p (format nil "~a=" (car pair)) entry))
                                                              environment))
                                                      (sb-ext:posix-environ)))))

(defun process-alive-p (process)
  "Whether PROCESS, a child or a stand-in, still runs."
  (if (sb-ext:process-p process) (sb-ext:process-alive-p process) t))

(defun log-tail (log)
  "The last 500 characters of a worker's LOG, its banner left out, or NIL."
  (let ((text (ignore-errors (uiop:read-file-string log))))
    (when text
      (let ((kept (string-trim '(#\Space #\Newline)
                               (format nil "~{~a~^~%~}"
                                       (remove-if (lambda (line) (uiop:string-prefix-p "omp tiny worker listening on " line))
                                                  (uiop:split-string text :separator '(#\Newline)))))))
        (and (plusp (length kept)) (subseq kept (max 0 (- (length kept) 500))))))))

(defun worker-stream (model-id backend)
  "A live stream to MODEL-ID's BACKEND worker: the one already on its socket,
else one this cell starts (connectTinyWorker), else a refusal saying why."
  (let* ((dir (runtime-dir))
         (name (worker-name model-id backend))
         (endpoint (uiop:native-namestring (merge-pathnames (format nil "~a.sock" name) dir)))
         (log (merge-pathnames (format nil "~a.log" name) dir)))
    (or (probe endpoint)
        (progn
          (unless (spawn-p backend)
            (error 'nle::provider-error
                   :status 503 :scope :request
                   :detail (format nil "no ~(~a~) worker for ~a listens at ~a, and this cell starts none: ~
                                        set local.spawn to true and local.omp_command to the omp binary, ~
                                        or let omp start one" backend model-id endpoint)))
          (ensure-directories-exist dir)
          (multiple-value-bind (argv environment) (launch model-id backend endpoint)
            (let ((process (handler-case (spawn-worker argv environment log)
                             (error (condition)
                               (error 'nle::provider-error
                                      :status 503 :scope :request
                                      :detail (format nil "the ~(~a~) worker for ~a could not start (~a): ~a"
                                                      backend model-id (first argv) condition)))))
                  (deadline (+ (get-internal-real-time) (* +ready-seconds+ internal-time-units-per-second))))
              (loop
                (alexandria:when-let (stream (probe endpoint)) (return stream))
                (unless (process-alive-p process)
                  (error 'nle::provider-error
                         :status 503 :scope :request
                         :detail (format nil "the ~(~a~) worker for ~a exited~@[: ~a~]" backend model-id (log-tail log))))
                (when (> (get-internal-real-time) deadline)
                  (error 'nle::provider-error
                         :status 503 :scope :request
                         :detail (format nil "the ~(~a~) worker for ~a did not bind ~a within ~d s"
                                         backend model-id endpoint +ready-seconds+)))
                (when nle::*current-durable-turn*
                  (nlk:ensure-turn-not-cancelled nle::*current-durable-turn*))
                (sleep 0.2))))))))

;;; --- asking it -------------------------------------------------------------------------

(defvar *next-id* 0
  "The last request id this process sent a worker.")

(defun chat (stream messages max-new-tokens seconds)
  "Send one chat request on STREAM; => (values TEXT REQUEST): the text the
worker generated, and the request as it went. A worker's error is a provider
error carrying its words."
  (let ((request (nlk:json-object "type" "chat"
                                  "id" (princ-to-string (incf *next-id*))
                                  "messages" messages
                                  "maxNewTokens" max-new-tokens)))
    (send-line stream request)
    (loop
      (let ((reply (handler-case (read-reply stream seconds)
                     (sb-sys:deadline-timeout ()
                       (error 'nle::provider-stream-incomplete
                              :detail (format nil "the worker said nothing for ~d s" seconds))))))
        (unless reply
          (error 'nle::provider-stream-incomplete :detail "the worker closed its socket before answering"))
        (when (equal (gethash "id" reply) (gethash "id" request))
          (let ((type (nlk:json-value reply :string "type")))
            (cond ((equal type "text") (return (values (or (nlk:json-value reply :string "text") "") request)))
                  ((equal type "error")
                   (error 'nle::provider-error
                          :status 500 :scope :contract
                          :detail (format nil "local inference failed: ~a"
                                          (nlk:clip (or (nlk:json-value reply :string "error") "") 400 :ellipsis "…")))))))))))
