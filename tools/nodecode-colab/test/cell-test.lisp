;;;; cell-test.lisp --- lifecycle, the seven tools, /colab, against a stubbed Colab.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: NLK:HTTP answers from a routing table and
;;;; KERNEL-EXECUTE is a stub that writes a line and finishes, so the run
;;;; path (assign, write, execute, sync, release) is exercised without a VM.

(in-package #:nodecode.test)

;;; --- the wire ----------------------------------------------------------------

(defvar *requests* '() "Every stubbed request of this thread, newest first.")
(defvar *all-requests* '() "The same across threads: a run dials on its own.")
(defvar *output* "epoch 1: loss 0.42~%" "What the stubbed kernel writes to stdout.")

(defun json (value) (nlk:encode-json-object value))

(defun colab-route (method url args)
  "One stubbed answer per Colab endpoint this cell uses."
  (declare (ignore args))
  (cond
    ((search "/tun/m/assignments" url) (values (json (nlk:make-json-object "assignments" (vector))) 200))
    ((search "/tun/m/assign" url)
     (if (eq method :post)
         (values (json (nlk:make-json-object
                        "endpoint" "ep-1" "accelerator" "T4"
                        "runtimeProxyInfo" (nlk:make-json-object "url" "https://proxy.test"
                                                                  "token" "runtime-token")))
                 200)
         (values (json (nlk:make-json-object "token" "assign-token")) 200)))
    ((search "/tun/m/unassign/" url)
     (if (eq method :post) (values "" 200)
         (values (json (nlk:make-json-object "token" "unassign-token")) 200)))
    ((search "/keep-alive/" url) (values "" 200))
    ((search "/api/kernels" url)
     (if (eq method :post) (values (json (nlk:make-json-object "id" "k-1")) 200)
         (values (json (vector (nlk:make-json-object "id" "k-1"))) 200)))
    ((and (eq method :put) (search "/api/contents/" url))
     (values (json (nlk:make-json-object "type" "file")) 200))
    ((search "/outputs/metrics.txt" url)
     (values (json (nlk:make-json-object "content" (colab::b64 "hello") "format" "base64"
                                         "type" "file"))
             200))
    ((search "/api/contents/" url)
     (values (json (nlk:make-json-object
                    "type" "directory"
                    "content" (vector (nlk:make-json-object "name" "metrics.txt" "type" "file"
                                                            "size" 5))))
             200))
    (t (error "unexpected request ~a ~a" method url))))

(defmacro with-colab-wire (&body body)
  "BODY with NLK:HTTP and KERNEL-EXECUTE stubbed: no test dials Colab and no
kernel is opened. Requests land in *ALL-REQUESTS* too, since a run dials on
the thread it was given."
  `(progn
     (setf *all-requests* '())
     (let ((*requests* '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                  (push (list* method url args) *requests*)
                                  (push (list* method url args) *all-requests*)
                                  (colab-route method url args))
         (with-stubbed-fdefinition (colab::kernel-execute (runtime kernel code &key timeout on-output)
                                    (declare (ignore runtime kernel code timeout))
                                    (when on-output (funcall on-output *output*))
                                    nil)
           ,@body)))))

(defun colab-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "colab-call" name arguments)))

(defun await-run (id &optional (seconds 15))
  "The run ID's final state, waiting at most SECONDS for one."
  (loop repeat (* 10 seconds)
        for run = (colab::find-run id)
        do (if (member (colab::run-state run) '(:succeeded :failed :cancelled))
               (return run)
               (sleep 0.1))
        finally (return (colab::find-run id))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "colab" "COLAB-CELL-" :start nodecode-colab:start-cell)

(define-cell-lifecycle-tests "colab"
  (:fixture with-colab-wire)
  (:config "access_token" "test-token")
  (:help :colab)
  (:command "colab")
  (:running
   (is (nle::find-tool "colab_run") "run is registered")
   (is (nle::find-tool "colab_status") "and status")
   (is (nle::find-tool "colab_logs") "and logs")
   (is (nle::find-tool "colab_cancel") "and cancel")
   (is (nle::find-tool "colab_runtimes") "and runtimes")
   (is (nle::find-tool "colab_release_runtime") "and release")
   (is (nle::find-tool "colab_doctor") "and doctor, the original's seven that survive"))
  (:stopped
   (is (null (nle::find-tool "colab_run")) "stop takes every one back")
   (is (null (nle::find-tool "colab_doctor"))))
  (:refused ("max_sync_mb" 0)
            ("poll_seconds" 0)
            ("default_accelerator" "TPU")))

(deftest colab-cell-a-start-with-no-grant-refuses (with-colab-wire)
  (let ((text (refusal-text nlk:config-refusal (colab-start "default_accelerator" "T4"))))
    (is-carrying text "needs a Google grant" "refresh_token")
    (is (null (nle::find-tool "colab_run")) "nothing was installed")))

;;; --- the tools -----------------------------------------------------------------

(deftest colab-cell-a-run-gets-a-runtime-executes-and-comes-home (with-colab-wire)
  (let ((source (nlk:home "colab-test/train.py"))
        (text nil))
    (ensure-directories-exist source)
    (with-open-file (out source :direction :output :if-exists :supersede)
      (write-string "print('hi')\n" out))
    (with-cell-stop ((colab-start "access_token" "test-token" "default_accelerator" "T4"))
      (setf text (colab-call "colab_run"
                             (format nil "{\"path\": ~s, \"name\": \"smoke\"}" (namestring source))))
      (is-carrying text "smoke" "queued")
      (let* ((run (first colab::*order*))
             (done (await-run run)))
        (is (eq :succeeded (colab::run-state done))
            (format nil "the run succeeded: ~a" (colab::run-view done)))
        (is (equal '("metrics.txt") (colab::run-outputs done)) "its output came home")
        (is (probe-file (merge-pathnames "outputs/metrics.txt" (colab::run-local-dir done))) "the file is on disk")
        (let ((log (uiop:read-file-string (colab::run-log-path done))))
          (is (search "loss 0.42" log) "the kernel's stdout is in the log"))
        (is (search "/tun/m/unassign/ep-1" (format nil "~s" *all-requests*)) "the runtime was released, and not kept")
        (setf text (colab-call "colab_status" (format nil "{\"run\": ~s}" run)))
        (is-carrying text "succeeded" "smoke")
        (setf text (colab-call "colab_logs" (format nil "{\"run\": ~s}" run)))
        (is (search "loss 0.42" text) "logs answer the tail")
        (setf text (colab-call "colab_status" "{\"all\": true}"))
        (is (search run text) "the list names it")))))

(deftest colab-cell-runtimes-doctor-and-release-answer (with-colab-wire)
  (with-cell-stop ((colab-start "access_token" "test-token"))
    (is (search "no runtimes assigned" (colab-call "colab_runtimes" "{}")) "an account with none says so")
    (let ((text (colab-call "colab_doctor" "{}")))
      (is (search "Google grant" text) "doctor names the grant")
      (is (search "Colab access" text) "and the access check"))
    (is (search "released ep-1" (colab-call "colab_release_runtime" "{\"endpoint\": \"ep-1\"}")) "a runtime is released by its endpoint")))

(deftest colab-cell-a-failing-job-is-failed-and-kept (with-colab-wire)
  (let ((source (nlk:home "colab-test/bad.py")))
    (ensure-directories-exist source)
    (with-open-file (out source :direction :output :if-exists :supersede)
      (write-string "raise SystemExit(1)\n" out))
    (with-cell-stop ((colab-start "access_token" "test-token"))
      (ignore-errors (colab-call "colab_run" (format nil "{\"path\": ~s}" (namestring source))))
      (let ((done (await-run (first colab::*order*))))
        (is (member (colab::run-state done) '(:succeeded :failed)) "the run reached a final state")
        (is (probe-file (colab::run-log-path done)) "its log is on disk")))))
