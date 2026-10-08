;;;; cell-test.lisp --- the local cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The worker is a stand-in on a real Unix socket in a scratch directory,
;;;; answering the protocol from a thread of this process; a worker the cell
;;;; would start is a stubbed SPAWN-WORKER that records the command and puts
;;;; the stand-in up instead. Nothing touches the network, starts a process,
;;;; downloads a weight, or reads the operator's omp directories.

(in-package #:nodecode.test)

(define-test-slice "local" "LOCAL-CELL-" :start nodecode-local:start-cell)

(define-cell-lifecycle-tests "local"
  (:hooks 'nle::models-catalog-table :credential)
  (:running (is (nle::find-lane-by-name "local" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "local" nil)) "and taken back out"))
  (:refused ("backend" "cuda") ("python" 5)))

;;; --- fixtures -------------------------------------------------------------------

(defun local-scratch-dir ()
  "A fresh, short scratch directory: a socket path must fit in 108 bytes."
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "/tmp/nc-local-~36r/" (random (expt 36 8) (make-random-state t))))))
    (ensure-directories-exist dir)
    dir))

(defstruct local-worker server thread (requests '()) path stopping)

(defun local-worker-up (path replies)
  "A stand-in worker on the Unix socket at PATH: it answers a ping with a
pong and a chat with REPLIES, each a JSON text whose ~a is the request's id,
then closes; it serves connections until stopped."
  (let* ((server (make-instance 'sb-bsd-sockets:local-socket :type :stream))
         (worker (make-local-worker :server server :path path)))
    (sb-bsd-sockets:socket-bind server path)
    (sb-bsd-sockets:socket-listen server 4)
    (setf (local-worker-thread worker)
          (sb-thread:make-thread
           (lambda ()
             (loop
               (let ((connection (handler-case (sb-bsd-sockets:socket-accept server) (error () (return)))))
                 (when (local-worker-stopping worker)
                   (ignore-errors (sb-bsd-sockets:socket-close connection))
                   (return))
                 (let ((stream (sb-bsd-sockets:socket-make-stream connection :input t :output t
                                                                             :element-type 'character
                                                                             :external-format :utf-8
                                                                             :buffering :full)))
                   (ignore-errors
                    (loop for line = (read-line stream nil nil)
                          while line
                          do (let ((request (nlk:decode-json line)))
                               (push request (local-worker-requests worker))
                               (if (equal "ping" (gethash "type" request))
                                   (format stream "{\"type\":\"pong\",\"id\":\"~a\",\"tag\":\"omp|onnx||\"}~%"
                                           (gethash "id" request))
                                   (dolist (reply replies)
                                     (format stream reply (gethash "id" request))
                                     (terpri stream)))
                               (finish-output stream))))
                   (ignore-errors (close stream))))))
           :name "local stand-in worker"))
    worker))

(defun local-worker-down (worker)
  "Stop WORKER: a blocked accept is woken by one last connection, which it
takes as the word to stop, since closing its socket from here wakes nothing."
  (setf (local-worker-stopping worker) t)
  (let ((stream (nodecode-local::connect (local-worker-path worker)))
        (thread (local-worker-thread worker)))
    (if stream
        (progn (ignore-errors (close stream :abort t))
               (sb-thread:join-thread thread :timeout 5 :default nil))
        ;; its socket file is gone: nothing can reach the accept now
        (ignore-errors (sb-thread:terminate-thread thread))))
  (ignore-errors (sb-bsd-sockets:socket-close (local-worker-server worker)))
  (ignore-errors (delete-file (local-worker-path worker))))

(defun local-chat-request (worker)
  "The chat request WORKER was sent."
  (find "chat" (local-worker-requests worker) :key (lambda (request) (gethash "type" request)) :test #'equal))

(defparameter +local-answer+
  '("{\"type\":\"progress\",\"id\":\"~a\",\"event\":{\"modelKey\":\"lfm2.5-230m\",\"status\":\"ready\"}}"
    "{\"type\":\"text\",\"id\":\"~a\",\"text\":\"Fixing the parser\"}")
  "A worker's answer: a progress event, then the text.")

(defun local-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-local-round ((values worker) (&key (model "lfm2.5-230m") (replies '+local-answer+) (up t)
                                                  (context '(user-context "name this session")) config max-tokens)
                            &body forms)
  "FORMS with the cell started on CONFIG (runtime_dir a scratch directory),
a stand-in worker for MODEL's ONNX socket when UP, and one round run: VALUES
the lane's answer, or the provider error it ended in."
  `(let* ((dir (local-scratch-dir))
          (path (uiop:native-namestring (merge-pathnames (format nil "~a.sock" (nodecode-local::worker-name ,model :onnx)) dir)))
          (,worker (and ,up (local-worker-up path ,replies)))
          (,values nil))
     (declare (ignorable ,worker ,values))
     (unwind-protect
          (with-cell-stop ((local-start "runtime_dir" (uiop:native-namestring dir) "backend" "onnx" ,@config))
            (let ((nle::*provider* "local") (nle::*model* ,model) (nle::*api-key* nil) (nle::*endpoint* nil)
                  (nle::*max-completion-tokens* ,max-tokens))
              (setf ,values (handler-case (multiple-value-list (local-lane-round ,context))
                              (nle::provider-error (condition) condition)))
              ,@forms))
       (when ,worker (local-worker-down ,worker))
       (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

;;; --- the catalog -----------------------------------------------------------------------

(deftest local-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((local-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "local"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Local models" (nlk:json-value row :string "name")))
      (is (equal "local://inference" (nlk:json-value row :string "api")))
      (is (= 8 (hash-table-count models)) "the eight text models")
      (is (gethash "lfm2.5-230m" models) "omp's default")
      (is (null (gethash "whisper-base" models)) "transcription is not a text model")
      (is (null (gethash "kokoro" models)) "nor is speech")
      (is (not (nle::catalog-model-tool-call-p (gethash "lfm2.5-230m" models))) "no tool calls, as omp seeds them")
      (is (equal "local" (nle::configured-provider-lane "local")))
      (is (equal "local://inference" (nle::provider-endpoint "local"))
          "so /model-aux local lfm2.5-230m resolves"))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (is (eq :public (nle:credential-source (nle::resolve-provider-credential "local" :auth-path auth :probe t)))
            "no key, and never another family's")))))

;;; --- the request ------------------------------------------------------------------------

(deftest local-cell-flattens-the-conversation-for-the-worker ()
  (with-local-round (values worker)
      (:context (compiled-context
                 (list (nle::message "user" "fix the parser")
                       (nlk:json-object "role" "assistant" "content" "Reading it."
                                        "tool_calls" (vector (nle::chat-tool-call-object "c1" "eval" "{}")))
                       (nle::message "tool" "(defun parse ...)" :name "eval" :tool-call-id "c1")
                       (nle::message "assistant" "Done."))))
    (let* ((request (local-chat-request worker))
           (messages (coerce (nlk:json-array request "messages") 'list)))
      (is (equal "chat" (gethash "type" request)))
      (is (= 256 (gethash "maxNewTokens" request)) "omp's default completion length")
      (is (equal '("system" "user" "user" "user") (mapcar (lambda (m) (gethash "role" m)) messages))
          "system then user turns only: the worker's templates alternate nothing else")
      (is (equal "fix the parser" (gethash "content" (second messages))))
      (is (search "Reading it." (gethash "content" (third messages))) "the assistant's text joins the next turn")
      (is (search "(defun parse" (gethash "content" (third messages))) "beside the tool result")
      (is (equal "Done." (gethash "content" (fourth messages))) "a trailing assistant turn goes as the user's"))))

(deftest local-cell-answers-the-workers-text ()
  (with-local-round (values worker) (:max-tokens 5000)
    (destructuring-bind (message usage finish request-json) values
      (is (equal "Fixing the parser" (nlk:json-value message :string "content")))
      (is (null usage))
      (is (equal "stop" finish))
      (is (equal "chat" (nlk:json-value (nlk:decode-json request-json) :string "type"))))
    (is (= 1024 (gethash "maxNewTokens" (local-chat-request worker))) "a ceiling past 1024 is held to it")
    (is (find "ping" (local-worker-requests worker) :key (lambda (r) (gethash "type" r)) :test #'equal)
        "the worker was pinged before it was asked")))

(deftest local-cell-says-what-a-worker-refused ()
  (with-local-round (values worker) (:replies '("{\"type\":\"error\",\"id\":\"~a\",\"error\":\"model download failed\"}"))
    (is (typep values 'nle::provider-error))
    (is (search "model download failed" (nle::provider-error-detail values))))
  (with-local-round (values worker) (:replies '("{\"type\":\"text\",\"id\":\"~a\",\"text\":\"\"}"))
    (is (search "returned no output" (nle::provider-error-detail values))))
  (with-local-round (values worker) (:model "gpt-4o" :up nil)
    (is (typep values 'nle::provider-config-error))
    (is (search "cannot run model local/gpt-4o" (nle::provider-error-detail values))))
  (with-local-round (values worker) (:model "qwen3-1.7b" :up nil)
    (is (search "MLX backend only" (nle::provider-error-detail values)) "omp's ONNX refusal, before any worker")))

;;; --- starting a worker ------------------------------------------------------------------

(deftest local-cell-starts-the-onnx-worker-omp-runs ()
  (let ((started nil) (stand-in nil))
    (unwind-protect
         (with-stubbed-fdefinition (nodecode-local::spawn-worker (argv environment log)
                                    (setf started (list argv environment log)
                                          stand-in (local-worker-up (cdr (assoc "OMP_TINY_WORKER_SOCKET" environment :test #'equal))
                                                                    +local-answer+))
                                    :stand-in)
           (with-local-round (values worker) (:up nil :config ("omp_command" "/opt/omp/bin/omp"))
             (is (equal "Fixing the parser" (nlk:json-value (first values) :string "content")))
             (destructuring-bind (argv environment log) started
               (is (equal '("/opt/omp/bin/omp" "__omp_worker_tiny_inference") argv) "the omp binary, re-entered")
               (is (equal "lfm2.5-230m" (cdr (assoc "OMP_TINY_WORKER_MODEL" environment :test #'equal))))
               (is (search "lfm2.5-230m-onnx.sock" (cdr (assoc "OMP_TINY_WORKER_SOCKET" environment :test #'equal))))
               (is (search "lfm2.5-230m-onnx.log" (namestring log))))))
      (when stand-in (local-worker-down stand-in)))))

(deftest local-cell-starts-omps-mlx-worker ()
  (with-cell-stop ((local-start "backend" "mlx" "python" "/opt/mlx/bin/python"))
    (multiple-value-bind (argv environment)
        (nodecode-local::launch "llama3.2:3b" :mlx "/tmp/x/llama3.2_3b-mlx.sock")
      (is (equal "/opt/mlx/bin/python" (first argv)))
      (is (search "worker/mlx-server.py" (third argv)) "the cell's copy of omp's script")
      (is (member "mlx-community/Llama-3.2-3B-Instruct-4bit" argv :test #'equal) "the model's MLX weights")
      (is (search "mlx/mlx-community--Llama-3.2-3B-Instruct-4bit/"
                  (second (member "--dir" argv :test #'equal))))
      (is (equal "900" (second (member "--idle-seconds" argv :test #'equal))))
      (is (equal "1" (cdr (assoc "PYTHONUNBUFFERED" environment :test #'equal)))))
    (is (equal "llama3.2_3b-mlx" (nodecode-local::worker-name "llama3.2:3b" :mlx)) "the socket name omp gives it")))

(deftest local-cell-starts-nothing-when-told-not-to ()
  (let ((started nil))
    (with-stubbed-fdefinition (nodecode-local::spawn-worker (argv environment log) (setf started t) :stand-in)
      (with-local-round (values worker) (:up nil :config ("spawn" nil))
        (is (typep values 'nle::provider-error))
        (is (search "starts none" (nle::provider-error-detail values)))
        (is (null started))))))

(deftest local-cell-finds-omps-runtime-directory ()
  (with-cell-stop ((local-start))
    (let ((home (local-scratch-dir)))
      (unwind-protect
           (with-stubbed-fdefinition (nodecode-local::home () home)
             (with-stubbed-fdefinition (nle::credential-env (name) nil)
               (is (equal (merge-pathnames ".omp/run/tiny/" home) (nodecode-local::default-runtime-dir))))
             (ensure-directories-exist (merge-pathnames "state/omp/" home))
             (with-stubbed-fdefinition (nle::credential-env (name)
                                        (and (equal name "XDG_STATE_HOME")
                                             (uiop:native-namestring (merge-pathnames "state/" home))))
               (is (equal (merge-pathnames "state/omp/run/tiny/" home) (nodecode-local::default-runtime-dir))
                   "an existing $XDG_STATE_HOME/omp wins, as omp's dirs.ts reads it")))
        (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)))))
