;;;; cell.lisp --- Colab runtimes, the runs that use them, and the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A run is one Python file on a Colab VM: the cell assigns a runtime,
;;;; writes the file to the VM's own disk through the Jupyter contents API,
;;;; executes it in the VM's kernel over Jupyter messaging, brings what it
;;;; wrote under outputs/ home while it runs, and releases the runtime unless
;;;; the call asked to keep it. When Colab takes the VM away mid-run the cell
;;;; assigns a fresh one, restores the synced outputs there and starts again
;;;; with COLAB_RESUME=1, so a script that saves checkpoints resumes where it
;;;; stopped. What the original plugin runs as a detached process and polls,
;;;; this executes in the kernel and streams.

(in-package #:nodecode-colab)

(defparameter +primer+
  "A Google Colab runtime is available through the nodecode-colab cell: the operator's own Colab
account, its GPUs, and its compute units.
  colab_run             run a .py file on a Colab runtime. path, then accelerator (T4 is the free
                        tier; L4, A100, H100 or NONE), requirements (pip names installed first),
                        env (non-secret variables), name, keep_runtime, timeout_minutes, high_mem.
                        Answers at once with a run id; the run continues in the background.
  colab_status          one run (state, accelerator, elapsed, exit, outputs, events) or, with all,
                        the recent runs.
  colab_logs            the tail of a run's stdout and stderr.
  colab_cancel          stop a run: interrupt the kernel, sync, release the runtime.
  colab_runtimes        the runtimes assigned to the account.
  colab_release_runtime free one runtime by endpoint; refuses one an active run holds.
  colab_doctor          what is wrong with the setup, and the next step.
How a run behaves: the file is written to /content/nodecode_colab/<run>/ on the VM and executed
there with its working directory set to that folder; anything it writes under outputs/ is copied
to <home>/colab_runs/<run>/outputs/ here while it runs, and the whole log to run.log there.
A script that reads its last checkpoint from outputs/ and honours COLAB_RESUME=1 continues on a
fresh runtime if Colab reclaims the VM. Runtimes consume the account's compute units; a run
releases its runtime when it ends unless keep_runtime. Check the plan's current rates in Colab."
  "What (help :colab) answers while the cell runs.")

;;; --- runs ---------------------------------------------------------------------

(defstruct (run (:constructor make-run (id name accelerator)))
  id name accelerator
  (state :queued) (message "") (endpoint nil) (runtime nil) (kernel nil)
  (resumes 0) (started (get-universal-time)) (finished nil) (error nil)
  (events '()) (outputs '()) (cached nil) (keep nil) (high-mem nil)
  (session nil) (thread nil))

(defvar *runs* (make-hash-table :test #'equal :synchronized t)
  "Every run of this image, by id, newest first in *ORDER*.")
(defvar *order* '() "The run ids, newest first.")
(defvar *runs-lock* (bt2:make-lock :name "colab runs"))

(defparameter +max-kept-runs+ 20 "How many runs are remembered.")

(defun new-run-id ()
  (format nil "c~36r" (get-universal-time)))

(defun run-event (run text &optional (state nil))
  (when state (setf (run-state run) state))
  (setf (run-events run) (append (last (run-events run) 20) (list (list (get-universal-time) text))))
  text)

(defun state-label (state)
  (or (cdr (assoc state '((:queued . "queued") (:assigning . "requesting a runtime")
                          (:copying . "writing the file to the VM") (:running . "running")
                          (:resuming . "resuming on a fresh runtime") (:syncing . "bringing outputs home")
                          (:succeeded . "succeeded") (:failed . "failed") (:cancelled . "cancelled"))))
      (string-downcase (princ-to-string state))))

(defun remember (run)
  (bt2:with-lock-held (*runs-lock*)
    (setf (gethash (run-id run) *runs*) run)
    (pushnew (run-id run) *order* :test #'equal)
    (setf *order* (subseq *order* 0 (min (length *order*) +max-kept-runs+)))))

(defun find-run (reference)
  "The run REFERENCE names: an id, a prefix of one, or a name; the newest when NIL."
  (bt2:with-lock-held (*runs-lock*)
    (let* ((reference (nonempty reference))
           (runs (remove nil (mapcar (lambda (id) (gethash id *runs*)) *order*))))
      (cond ((null reference) (first runs))
            ((gethash reference *runs*))
            (t (or (find reference runs :test (lambda (ref run)
                                                (or (eql 0 (search ref (run-id run)))
                                                    (equal ref (run-name run)))))
                   (find reference runs :test (lambda (ref run)
                                                (and (run-name run)
                                                     (eql 0 (search (string-downcase ref)
                                                                    (string-downcase (run-name run)))))))))))))

(defun run-local-dir (run)
  "RUN's folder here: its log and its outputs."
  (or (run-cached run)
      (setf (run-cached run) (nlk:home (format nil "colab_runs/~a/" (run-id run))))))

(defun run-log-path (run) (merge-pathnames "run.log" (run-local-dir run)))
(defun run-output-dir (run) (merge-pathnames "outputs/" (run-local-dir run)))
(defun run-remote-dir (run) (format nil "/content/nodecode_colab/~a" (run-id run)))

(defun append-log (run text)
  (ignore-errors
    (with-open-file (out (run-log-path run) :direction :output :if-exists :append :if-does-not-exist :create)
      (write-string text out))))

(defun minutes-between (from to)
  (and from to (round (/ (- to from) 60.0) 1/10)))

(defun run-view (run)
  "RUN as the text a model or an operator reads."
  (with-output-to-string (out)
    (format out "run ~a~@[ (~a)~]: ~a on ~a, ~,1f min~@[, exit ~a~]~@[ - ~a~]~%"
            (run-id run) (run-name run) (state-label (run-state run)) (run-accelerator run)
            (or (minutes-between (run-started run) (or (run-finished run) (get-universal-time))) 0.0)
            nil (and (plusp (length (run-message run))) (run-message run)))
    (format out "resumes ~d; endpoint ~a~%" (run-resumes run) (or (run-endpoint run) "none"))
    (when (run-error run) (format out "error: ~a~%" (run-error run)))
    (when (run-outputs run) (format out "outputs: ~{~a~^, ~}~%" (run-outputs run)))
    (format out "folder: ~a~%" (namestring (run-local-dir run)))
    (dolist (event (last (run-events run) 6))
      (format out "  ~a~%" (second event)))))

(defun run-line (run)
  "One line: what a list of runs shows of one."
  (format nil "~a ~a (~a, ~a)~@[ - ~a~]" (run-id run) (state-label (run-state run))
          (run-name run) (run-accelerator run) (and (run-error run) (run-error run))))

;;; --- bringing outputs home ------------------------------------------------------

(defun sync-outputs (run runtime)
  "Copy RUNTIME's outputs/ for RUN to the local run folder, under the size cap."
  (let ((remote (format nil "~a/outputs" (run-remote-dir run)))
        (cap (* (setting :max-sync-mb) 1024 1024))
        (seen '()))
    (labels ((walk (path prefix)
               (dolist (entry (entries runtime path))
                 (let ((name (getf entry :name))
                       (type (getf entry :type)))
                   (unless (or (null name) (eql 0 (length name)) (char= (char name 0) #\.))
                     (let ((child (format nil "~a/~a" path name))
                           (relative (if prefix (format nil "~a/~a" prefix name) name)))
                       (cond ((equal type "directory") (walk child relative))
                             ((and (equal type "file") (<= (getf entry :size) cap))
                              (let ((text (get-file runtime child))
                                    (target (merge-pathnames relative (run-output-dir run))))
                                (ensure-directories-exist target)
                                (with-open-file (out target :direction :output :if-exists :supersede :if-does-not-exist :create)
                                (write-string text out))
                                (push relative seen))))))))))
      (ignore-errors (walk remote nil)))
    (when seen (setf (run-outputs run) (sort seen #'string<)))
    (run-outputs run)))

(defun start-sync (run runtime)
  "A thread that brings outputs home every (POLL-SECONDS) while the run lasts."
  (let ((live t))
    (bt2:make-thread
     (lambda ()
       (loop while live
             do (sleep (setting :poll-seconds))
                (ignore-errors (sync-outputs run runtime))
             while live))
     :name "colab sync")
    (lambda () (setf live nil))))

;;; --- driving a run --------------------------------------------------------------

(defun job-code (run entry env)
  "The Python the kernel runs: the entry file, in the run's own folder, with
the environment the call asked for and COLAB_OUTPUT_DIR pointing at outputs/."
  (with-output-to-string (out)
    (format out "import os, runpy~%os.chdir(~s)~%" (run-remote-dir run))
    (format out "os.makedirs('outputs', exist_ok=True)~%")
    (format out "os.environ['COLAB_OUTPUT_DIR'] = os.path.join(os.getcwd(), 'outputs')~%")
    (format out "os.environ['COLAB_RESUME'] = ~s~%" (if (plusp (run-resumes run)) "1" "0"))
    (loop for (key . value) on env by #'cddr
          do (format out "os.environ[~s] = ~s~%" key (princ-to-string value)))
    (format out "runpy.run_path(~s, run_name='__main__')~%" entry)))

(defun upload-job (run runtime entry source requirements)
  "Write the entry file, its requirements and any env to the VM."
  (let ((dir (run-remote-dir run)))
    (put-file runtime (format nil "~a/~a" dir entry) source)
    (when requirements
      (put-file runtime (format nil "~a/requirements.txt" dir) requirements))
    dir))

(defun restore-outputs (run runtime)
  "Put back on the VM the outputs already brought home, so a resumed run finds its checkpoints."
  (let ((dir (format nil "~a/outputs" (run-remote-dir run))))
    (dolist (relative (run-outputs run))
      (let ((local (merge-pathnames relative (run-output-dir run))))
        (when (probe-file local)
          (ignore-errors (put-file runtime (format nil "~a/~a" dir relative) (uiop:read-file-string local))))))))

(defun attempt-run (run source entry env requirements)
  "One runtime's worth of the run: assign, write, execute, sync, release. Signals RUNTIME-GONE."
  (let ((runtime (assign-runtime (run-accelerator run) :high-mem (run-high-mem run))))
    (unwind-protect
         (progn
           (setf (run-runtime run) runtime
                 (run-endpoint run) (runtime-endpoint runtime))
           (run-event run (format nil "runtime ~a assigned (~a)" (runtime-endpoint runtime)
                                   (or (runtime-accelerator runtime) "?")))
           (let* ((kernel (kernel-id runtime))
                  (stop-sync nil)
                  (failure nil))
             (setf (run-kernel run) kernel)
             (run-event run "writing the file to the VM" :copying)
             (when (plusp (run-resumes run)) (restore-outputs run runtime))
             (upload-job run runtime entry source requirements)
             (run-event run "running" :running)
             (setf stop-sync (start-sync run runtime))
             (unwind-protect
                  (setf failure (kernel-execute runtime kernel
                                                (job-code run entry (getf env :vars))
                                                :timeout (* 60 (or (getf env :timeout-minutes) 600))
                                                :on-output (lambda (text) (append-log run text))))
               (funcall stop-sync))
             (when failure
               (setf (run-message run) failure)
               (run-event run (format nil "the job raised ~a" failure)))
             (run-event run "bringing outputs home" :syncing)
             (sync-outputs run runtime)
             (setf (run-state run) (if failure :failed :succeeded))))
      (when (and runtime (not (run-keep run)))
        (ignore-errors (release-runtime (runtime-endpoint runtime)))))))

(defun drive-run (run source entry &key env requirements)
  "The whole run, and a fresh runtime whenever Colab takes one away."
  (handler-case
      (loop
        (handler-case (return (attempt-run run source entry env requirements))
          (runtime-gone (condition)
            (declare (ignore condition))
            (setf (run-runtime run) nil (run-kernel run) nil)
            (if (< (incf (run-resumes run)) (max 1 (setting :max-resumes)))
                (run-event run "the runtime was reclaimed; a fresh one follows" :resuming)
                (progn
                  (setf (run-state run) :failed
                        (run-error run) "the runtime was reclaimed and no resume is left")
                  (run-event run "giving up: the runtime was reclaimed")
                  (return))))))
    (error (condition)
      (setf (run-state run) :failed
            (run-error run) (format nil "~a" condition))
      (run-event run (format nil "failed: ~a" condition))))
  (setf (run-finished run) (or (run-finished run) (get-universal-time)))
  (nle:notice (format nil "colab: run ~a ~a" (run-id run) (state-label (run-state run)))
              :key "nodecode-colab")
  (run-view run))

;;; --- the tools -------------------------------------------------------------------

(nlk:define-contract colab-run-call
  (:doc "Run a Python file on a Google Colab runtime (GPU or TPU) in the background and bring its
outputs home. Returns at once with a run id; follow it with colab_status and colab_logs.")
  ("path" :text :required t
   :doc "the .py file to run, absolute or relative to the session's working directory")
  ("accelerator" :string :options +accelerators+
   :doc "T4 by default (free tier); L4, A100, H100 for more; NONE for a CPU runtime")
  ("requirements" :array :of :string
   :doc "pip requirements installed on the VM before the job; a requirements.txt beside the file is used when this is left out")
  ("env" :object
   :doc "non-secret environment variables for the job")
  ("name" :text :doc "a short name for the run")
  ("keep_runtime" :boolean :doc "keep the runtime assigned after the job ends; it keeps using compute units")
  ("high_mem" :boolean :doc "ask for the high-RAM shape when available")
  ("timeout_minutes" :integer :doc "stop the job after this many minutes"))

(nlk:define-contract colab-run-ref
  (:doc "Name the run to look at.")
  ("run" :text :doc "run id, its unique prefix, or its name; the latest run when left out"))

(nlk:define-contract colab-status-call
  (:doc "One run's state, or the recent runs.")
  ("run" :text :doc "run id, its unique prefix, or its name; the latest run when left out")
  ("all" :boolean :doc "list the recent runs instead of one"))

(nlk:define-contract colab-logs-call
  (:doc "The tail of a run's stdout and stderr.")
  ("run" :text :doc "run id, its unique prefix, or its name; the latest run when left out")
  ("lines" :integer :doc "how many lines to answer, 60 by default"))

(nlk:define-contract colab-endpoint-call
  (:doc "Name a Colab runtime by the endpoint colab_runtimes shows.")
  ("endpoint" :text :required t :doc "the runtime's endpoint"))

(defun no-arguments ()
  "The schema of a tool that takes no arguments: a contract types nothing without a member."
  (list :description nil
        :parameters (nlk:make-json-object "type" "object"
                                           "properties" (nlk:make-json-object))))


(defun resolve-path (raw)
  "RAW as a path: absolute, or relative to the working directory."
  (let* ((text (or raw ""))
         (path (uiop:parse-native-namestring text)))
    (if (uiop:absolute-pathname-p path) path (merge-pathnames path (uiop:getcwd)))))

(defun tool-run (call)
  (let* ((path (resolve-path (colab-run-call-path call)))
         (source (probe-file path)))
    (unless source (fail "colab: ~a does not exist" (colab-run-call-path call)))
    (unless (equal (string-downcase (pathname-type source)) "py")
      (fail "colab: only a .py file can be run (this is a ~a)" (pathname-type source)))
    (let* ((accelerator (string-upcase (or (nonempty (colab-run-call-accelerator call))
                                           (setting :default-accelerator) "T4")))
           (run (make-run (new-run-id)
                          (or (nonempty (colab-run-call-name call))
                              (pathname-name source))
                          accelerator)))
      (unless (member accelerator +accelerators+ :test #'string=)
        (fail "colab: unknown accelerator ~a; choose ~{~a~^, ~}" accelerator +accelerators+))
      (remember run)
      (ensure-directories-exist (run-output-dir run))
      (setf (run-keep run) (colab-run-call-keep-runtime call)
            (run-high-mem run) (colab-run-call-high-mem call))
      (let ((requirements (or (and (colab-run-call-requirements call)
                                   (format nil "~{~a~%~}" (coerce (colab-run-call-requirements call) 'list)))
                          (let ((beside (merge-pathnames "requirements.txt"
                                                         (uiop:pathname-directory-pathname source))))
                            (and (probe-file beside) (uiop:read-file-string beside)))))
            (env (list :vars (let ((table (colab-run-call-env call)) (pairs '()))
                                (when table
                                  (maphash (lambda (key value) (push (cons key value) pairs)) table))
                                pairs)
                       :timeout-minutes (colab-run-call-timeout-minutes call))))
        (run-event run (format nil "queued ~a" (file-namestring source)))
        (append-log run (format nil "colab: ~a on ~a~%" (file-namestring source) accelerator))
        (setf (run-thread run)
              (bt2:make-thread
               (lambda ()
                 (drive-run run (uiop:read-file-string source) (file-namestring source)
                            :env env :requirements requirements))
               :name (format nil "colab run ~a" (run-id run))))
        (run-view run)))))

(defun tool-status (call)
  (if (colab-status-call-all call)
      (let ((runs (bt2:with-lock-held (*runs-lock*)
                    (remove nil (mapcar (lambda (id) (gethash id *runs*)) *order*)))))
        (if runs
            (with-output-to-string (out)
              (dolist (run runs) (format out "~a~%" (run-line run))))
            "no runs yet"))
      (let ((run (find-run (colab-status-call-run call))))
        (if run (run-view run) "no such run"))))

(defun tool-logs (call)
  (let* ((run (find-run (colab-logs-call-run call)))
         (lines (max 1 (min (or (colab-logs-call-lines call) 60) 400))))
    (unless run (fail "colab: no such run"))
    (let ((path (run-log-path run)))
      (if (probe-file path)
          (let ((text (uiop:read-file-string path)))
            (format nil "~a: ~a~%~a" (run-id run) (state-label (run-state run))
                    (format nil "~{~a~^~%~}"
                            (last (uiop:split-string text :separator '(#\Newline)) lines))))
          (format nil "~a: ~a~%no output yet" (run-id run) (state-label (run-state run)))))))

(defun tool-cancel (call)
  (let ((run (find-run (colab-run-ref-run call))))
    (unless run (fail "colab: no such run"))
    (when (member (run-state run) '(:succeeded :failed :cancelled))
      (return-from tool-cancel (format nil "run ~a already ~a" (run-id run) (state-label (run-state run)))))
    (ignore-errors (interrupt-kernel (run-runtime run) (run-kernel run)))
    (setf (run-state run) :cancelled (run-finished run) (get-universal-time))
    (run-event run "cancellation requested")
    (format nil "run ~a cancelled; its runtime is released when the job ends" (run-id run))))

(defun tool-runtimes ()
  (let ((runtimes (list-runtimes)))
    (if runtimes
        (with-output-to-string (out)
          (dolist (runtime runtimes)
            (format out "~a~@[ (~a)~]~%" (getf runtime :endpoint) (getf runtime :accelerator))))
        "no runtimes assigned")))

(defun tool-release (call)
  (let ((endpoint (colab-endpoint-call-endpoint call)))
    (release-runtime endpoint)
    (format nil "released ~a" endpoint)))

(defun tool-doctor ()
  (let ((checks '()))
    (push (list :check "Google grant" :ok (and (or (nonempty (setting :access-token))
                                                     (and (nonempty (setting :refresh-token))
                                                          (nonempty (setting :client-id))
                                                          (nonempty (setting :client-secret))))
                                                t)
                :detail (if (nonempty (setting :access-token)) "access_token set"
                            (if (nonempty (setting :refresh-token)) "refresh_token set" "missing"))
                :next_step "set access_token, or refresh_token with client_id and client_secret, in the colab section (a Google Cloud OAuth client of type Desktop app with the Colaboratory scope)")
          checks)
    (handler-case
        (let ((runtimes (list-runtimes)))
          (push (list :check "Colab access" :ok t :detail (format nil "~d runtime(s) assigned" (length runtimes)))
                checks))
      (error (condition)
        (push (list :check "Colab access" :ok nil :detail (format nil "~a" condition)
                    :next_step "open colab.research.google.com once with that Google account and accept the terms")
              checks)))
    (with-output-to-string (out)
      (dolist (check (reverse checks))
        (format out "~:[FAIL~;ok  ~] ~a: ~a~@[~%     next: ~a~]~%"
                (getf check :ok) (getf check :check) (getf check :detail) (getf check :next-step)))
      (format out "~d active run(s)~%" (count-if (lambda (run)
                                                    (not (member (run-state run) '(:succeeded :failed :cancelled))))
                                                  (bt2:with-lock-held (*runs-lock*)
                                                    (remove nil (mapcar (lambda (id) (gethash id *runs*)) *order*))))))))

;;; --- the operator's command ------------------------------------------------------

(defun run-command (args session-id)
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (cond ((zerop (length argument))
           (with-output-to-string (out)
             (format out "colab: token ~a, accelerator ~a, max ~d MB synced home~%"
                     (if (or (nonempty (setting :access-token)) (nonempty (setting :refresh-token)))
                         "set" "MISSING")
                     (setting :default-accelerator) (setting :max-sync-mb))
             (format out "~a~%" (tool-runtimes))
             (format out "runs: ~a" (tool-status (nlk:contract-decode (nlk:contract 'colab-status-call)
                                                                       (nlk:make-json-object))))))
          ((eql 0 (search "logs" argument))
           (tool-logs (nlk:contract-decode (nlk:contract 'colab-logs-call)
                                            (nlk:make-json-object))))
          ((eql 0 (search "cancel" argument))
           (tool-cancel (nlk:contract-decode (nlk:contract 'colab-run-ref)
                                              (nlk:make-json-object "run" (nlk:trimmed (subseq argument 6))))))
          (t (tool-status (nlk:contract-decode (nlk:contract 'colab-status-call)
                                               (nlk:make-json-object "run" argument)))))))

(defun tool-list-runtimes (call)
  (declare (ignore call))
  (tool-runtimes))

(defun tool-check (call)
  (declare (ignore call))
  (tool-doctor))

;;; --- the cell -------------------------------------------------------------------


(nle:define-cell colab
  (:section ("colab")
    (:guide "make a Google Cloud OAuth client of type Desktop app with the Colaboratory scope, sign in once with the Google account you use on Colab, and keep the grant here: access_token for an hour, or refresh_token with client_id and client_secret to renew; then /colab lists the runtimes assigned")
    ("access_token" :secret
     :doc "a Google access token with the Colaboratory scope (short-lived)")
    ("refresh_token" :secret
     :doc "the account's Google refresh token; with client_id and client_secret the cell renews the access token itself")
    ("client_id" :string
     :doc "the OAuth client's id, the one the refresh token was issued to")
    ("client_secret" :secret
     :doc "the OAuth client's secret")
    ("default_accelerator" :choice :options '("T4" "L4" "A100" "H100" "NONE") :default "T4"
     :doc "what a run asks for when it names none; T4 is on the free tier")
    ("base_url" :string
     :doc "Colab's address: https://colab.research.google.com unless you go through a proxy")
    ("max_sync_mb" :integer :default 64 :min 1
     :doc "the largest output file brought home")
    ("max_resumes" :integer :default 2 :min 0
     :doc "how many fresh runtimes a run gets when Colab reclaims its own")
    ("poll_seconds" :integer :default 5 :min 1
     :doc "how often outputs are brought home while a run lasts")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one Colab request's ceiling, in seconds"))
  (:help :colab "colab: run a Python file on a Google Colab GPU runtime, and bring its outputs home - (help :colab)" +primer+)
  (:command "colab" 'run-command
            :description "Colab: the connection, the runtimes, or a run"
            :argument-hint "RUN")
  (:tool "colab_run" (nlk:contract 'colab-run-call) #'tool-run)
  (:tool "colab_status" (nlk:contract 'colab-status-call) #'tool-status)
  (:tool "colab_logs" (nlk:contract 'colab-logs-call) #'tool-logs)
  (:tool "colab_cancel" (nlk:contract 'colab-run-ref) #'tool-cancel)
  (:tool "colab_runtimes"
           (list :description (concatenate 'string
                                           "List the Colab runtimes assigned to the account, with each one's "
                                           "accelerator. Each runtime consumes the account's compute units "
                                           "until it is released.")
                 :parameters (getf (no-arguments) :parameters))
           #'tool-list-runtimes)
  (:tool "colab_release_runtime" (nlk:contract 'colab-endpoint-call) #'tool-release)
  (:tool "colab_doctor"
           (list :description (concatenate 'string
                                           "Check that Colab runs can work here: the Google grant, Colab "
                                           "access, the active runs. Answers each check, and the exact "
                                           "next step for one that fails.")
                 :parameters (getf (no-arguments) :parameters))
           #'tool-check)
  (:start (lambda ()
            (unless (or (nonempty (setting :access-token)) (nonempty (setting :refresh-token)))
              (error 'nlk:config-refusal
                     :detail (concatenate 'string
                                          "colab: this cell needs a Google grant: set access_token, or "
                                          "refresh_token with client_id and client_secret, in the colab "
                                          "section; a Google Cloud OAuth client of type Desktop app "
                                          "with the Colaboratory scope makes one"))))))
