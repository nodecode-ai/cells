;;;; cell.lisp --- Remesher's four tools, over the CLI the plugin wrapped.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :remesher) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path: the
;;;; work is a command line, and NLK:RUN-BOUNDED is the one seam that starts
;;;; it, bounded, in a process group of its own.
;;;;
;;;; Config, a sibling top-level key of the shared config:
;;;;   "remesher": {"cli": "/opt/remesher/.venv/bin/comfy-prompt-cli",
;;;;                "backend": "comfy3d",
;;;;                "config": "/opt/remesher/config.json",
;;;;                "workflow": "/opt/remesher/workflows/comfy3d.json"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from remesher/remesher's Hermes plugin (see NOTICE): the four
;;;; tools and their arguments, the job receipt that outlives the call, the
;;;; one-active-job rule, the deadline, the artifact manifest with its
;;;; SHA-256, the GLB-header check, and the environment scrub that keeps
;;;; ambient tokens out of the child.

(in-package #:nodecode-remesher)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Remesher is available through the nodecode-remesher cell: four tools, which a person can
also call from eval in the remesher package. It turns one image into a GLB through a Remesher
CLI that is installed and configured separately; the cell installs nothing and downloads
nothing.
  remesher_health  {}
      With the comfy3d or native backend, runs the CLI's own `health' against the configured
      server: connectivity only, never GPU readiness. With native-local it answers
      `configured' without starting anything. `gpu_generation_verified' is always false.
  remesher_generate  {image, timeout_seconds, target_face_num, native_preset}
      Submit one bounded image-to-GLB job and answer its job_id at once. image is an
      absolute local PNG/JPEG/WebP path, max 20 MiB, already foreground-masked and
      square-composited on black: the cell removes no background. With comfy3d the image is
      uploaded to the ComfyUI server the CLI's config names; native-local starts a local
      server of its own and may take the GPU. One job at a time: a second submission answers
      `busy'. timeout_seconds is 1 to 1800.
  remesher_job  {job_id}
      The persisted receipt: queued, running, succeeded, failed, timed_out or interrupted,
      with the CLI's log path and, on success, the artifact manifest. Poll it every few
      seconds; never replay a job whose outcome is ambiguous, the remote run may still be
      going. A receipt whose saved files are gone or changed answers `integrity_error'.
  remesher_assets  {job_id}
      On success, each GLB's path, size and SHA-256. The check is the GLB header and its
      length, not geometry or render: verify the hash again where the artifact is used.
Artifacts and logs live under the profile's home in remesher/jobs/<job_id>/."
  "What (help :remesher) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (remesher-error (condition)
       (nle:failure (format nil "ERROR: ~a" (remesher-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Remesher: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer (object)
  "OBJECT as the JSON text a tool answers with."
  (nlk:encode-json-object object))

;;; --- the section ---------------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defparameter +timeout-limit+ 1800
  "The longest one job may run, in seconds: the original's per-call maximum.")

(defun loopback-origin-p (url)
  "Whether URL is a numeric-loopback HTTP origin: the only server a native
backend may talk to."
  (or (uiop:string-prefix-p "http://127.0.0.1" url)
      (uiop:string-prefix-p "http://[::1]" url)))

(defun require-file (path setting-name)
  "PATH as a namestring, or the refusal naming SETTING-NAME, which must be a
file that exists."
  (unless (and path (probe-file path) (not (uiop:directory-pathname-p path)))
    (nlk:config-error
     "remesher.~a is ~s: set it to an existing absolute file path (this cell installs nothing)"
     setting-name (or path "")))
  (uiop:native-namestring (truename path)))

(defun read-settings (values table)
  "The declared VALUES, checked against the backend they name: read once, here."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (backend (or (nonblank (getf values :backend)) "comfy3d"))
         (cli (require-file (nonblank (getf values :cli)) "cli")))
    (setf (getf plist :backend) backend
          (getf plist :cli) cli)
    (cond
      ((string= backend "comfy3d")
       (setf (getf plist :config) (require-file (nonblank (getf values :config)) "config")
             (getf plist :workflow) (require-file (nonblank (getf values :workflow)) "workflow")))
      ((string= backend "native")
       (let ((server (or (nonblank (getf values :native-server)) "http://127.0.0.1:8188")))
         (unless (loopback-origin-p server)
           (nlk:config-error
            "remesher.native_server is ~s: a native backend talks to a numeric loopback origin only, e.g. \"http://127.0.0.1:8188\""
            server))
         (setf (getf plist :native-server) server)))
      (t
       (let ((python (require-file (nonblank (getf values :native-python)) "native_python"))
             (checkout (nonblank (getf values :native-checkout)))
             (server (or (nonblank (getf values :native-server)) "http://127.0.0.1:8188")))
         (unless (and checkout (uiop:directory-exists-p checkout)
                      (probe-file (merge-pathnames "main.py" (uiop:ensure-directory-pathname checkout))))
           (nlk:config-error
            "remesher.native_checkout is ~s: set it to the absolute path of the CLI's checkout, which holds main.py"
            (or checkout "")))
         (setf (getf plist :native-python) python
               (getf plist :native-checkout) (uiop:native-namestring
                                              (uiop:ensure-directory-pathname checkout))
               (getf plist :native-server) server))))
    plist))

;;; --- starting the CLI ------------------------------------------------------------

(defparameter +environment-kept+
  '("PATH" "HOME" "LANG" "LC_ALL" "TMPDIR" "SSL_CERT_FILE" "SSL_CERT_DIR")
  "The only ambient variables a child inherits: activation scripts, provider
keys and CUDA/HF/Torch variables stay in the image. What the backend needs is
set in the section, not the ambient environment.")

(defun environment-entry-name (entry)
  "ENTRY's variable name: \"NAME=value\" or a bare \"NAME\"."
  (subseq entry 0 (or (position #\= entry) (length entry))))

(defun child-environment ()
  "This process's environment cut down to +ENVIRONMENT-KEPT+: the kept entries
whole, and a bare name for every other, which removes it."
  (loop for entry in (sb-ext:posix-environ)
        for name = (environment-entry-name entry)
        if (member name +environment-kept+ :test #'string=)
          collect entry
        else
          collect name))

(defun run-cli (argv directory seconds log)
  "Start ARGV in DIRECTORY, bounded by SECONDS, its output appended to LOG
(or dropped when LOG is NIL). => its exit status, or :TIMEOUT, the whole
process group killed."
  (nlk:run-bounded argv
                   :seconds seconds
                   :directory directory
                   :environment (child-environment)
                   :output (or log nil)
                   :error-output (if log :output nil)))

(defvar *run-cli* #'run-cli
  "SEAM: the function that starts the CLI, for a test to stub. =>
(ARGV DIRECTORY SECONDS LOG) answers the exit status or :TIMEOUT.")

;;; --- where a job lives -----------------------------------------------------------

(defun jobs-root ()
  "The directory every job of this profile is kept in, made on first use."
  (let ((path (nlk:home "remesher/jobs/")))
    (ensure-directories-exist path)
    path))

(defun job-folder (id)
  "The directory of the job ID."
  (merge-pathnames (format nil "~a/" id) (jobs-root)))

(defun job-id ()
  "A fresh job id: 32 lowercase hex characters."
  (subseq (nlk:sha256-text (format nil "~a/~a/~a" (nlk:iso-now) (random most-positive-fixnum)
                                   (nlk:random-bytes 16)))
          7 39))

(defun job-id-p (text)
  "Whether TEXT is a job id: 32 lowercase hex characters, and nothing else."
  (and (stringp text) (= 32 (length text))
       (every (lambda (char) (or (digit-char-p char 16) (find char "abcdef"))) text)))

;;; --- the receipt -----------------------------------------------------------------

(defun state-file (folder)
  (merge-pathnames "state.json" folder))

(defun asset-object (asset)
  "One artifact's manifest entry, as JSON."
  (nlk:json-object "path" (getf asset :path)
                    "size_bytes" (getf asset :size)
                    "sha256" (getf asset :sha256)
                    "media_type" "model/gltf-binary"
                    "validation" "glb_header_only"))

(defun save-state (folder state)
  "Write STATE to the folder's receipt, atomically: a reader sees the old one
whole or the new one whole."
  (nlk:write-file-atomically
   (state-file folder)
   (nlk:encode-json-object
    (nlk:json-object "schema_version" 1
                     "job_id" (getf state :job-id)
                     "status" (getf state :status)
                     "backend" (getf state :backend)
                     "created_at" (getf state :created-at)
                     "deadline" (getf state :deadline)
                     "finished_at" (or (getf state :finished-at) 0)
                     "image" (getf state :image)
                     "argv" (coerce (getf state :argv) 'vector)
                     "assets" (map 'vector #'asset-object (getf state :assets))
                     "log_path" (getf state :log)
                     "error" (or (getf state :error) "")))
   :mode #o600 :directory-mode #o700))

(defun read-assets (object)
  "The manifest entries of a decoded receipt, as plists."
  (loop for asset across (or (nlk:json-array object "assets") #())
        collect (list :path (nlk:json-value asset :string "path")
                      :size (nlk:json-value asset :integer "size_bytes")
                      :sha256 (nlk:json-value asset :string "sha256"))))

(defun read-state (folder)
  "The job's receipt as a plist, or NIL when there is none."
  (let ((file (state-file folder)))
    (when (probe-file file)
      (let ((object (nlk:decode-json (uiop:read-file-string file))))
        (list :job-id (nlk:json-value object :string "job_id")
              :status (nlk:json-value object :string "status")
              :backend (nlk:json-value object :string "backend")
              :created-at (nlk:json-value object :integer "created_at")
              :deadline (nlk:json-value object :integer "deadline")
              :image (nlk:json-value object :string "image")
              :argv (coerce (or (nlk:json-array object "argv") #()) 'list)
              :assets (read-assets object)
              :log (nlk:json-value object :string "log_path")
              :error (let ((text (nlk:json-value object :string "error")))
                       (and (stringp text) (plusp (length text)) text)))))))

(defun state-object (state)
  "STATE as the JSON text a job or assets call answers with."
  (answer (nlk:json-object "job_id" (getf state :job-id)
                            "status" (getf state :status)
                            "backend" (or (getf state :backend) "")
                            "created_at" (or (getf state :created-at) 0)
                            "deadline" (or (getf state :deadline) 0)
                            "assets" (map 'vector #'asset-object (getf state :assets))
                            "log_path" (or (getf state :log) "")
                            "error" (or (getf state :error) ""))))

;;; --- artifacts -------------------------------------------------------------------

(defun glb-length (path)
  "PATH's length when it is a GLB — the magic, version 2, and a total length
that matches the file — else NIL."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (when in
          (let ((header (make-array 12 :element-type '(unsigned-byte 8)))
                (size (file-length in)))
            (when (= 12 (read-sequence header in))
              (flet ((word (at)
                       (+ (aref header at) (ash (aref header (+ at 1)) 8)
                          (ash (aref header (+ at 2)) 16) (ash (aref header (+ at 3)) 24))))
                (when (and (equalp (subseq header 0 4) #(103 108 84 70))
                           (= 2 (word 4))
                           (= size (word 8)))
                  size))))))
    (error () nil)))

(defun collect-assets (output)
  "The valid GLB files in OUTPUT, at most 16, each with its size and SHA-256.
A file whose header is not a GLB is not an artifact."
  (loop for path in (sort (directory (merge-pathnames "*.glb" output)) #'string<
                           :key (lambda (path) (namestring path)))
        for kept = 0 then (1+ kept)
        for size = (and (< kept 16) (glb-length path))
        when size
          collect (list :path (namestring path)
                        :size size
                        :sha256 (nlk:sha256-text (nodecode.term:file-octets path)))))

(defun assets-intact-p (assets)
  "Whether every manifest entry still names the file it did, at the size and
hash it had."
  (and (plusp (length assets))
       (every (lambda (asset)
                (let ((path (getf asset :path)))
                  (and (probe-file path)
                       (= (or (glb-length path) -1) (getf asset :size))
                       (equal (getf asset :sha256)
                              (nlk:sha256-text (nodecode.term:file-octets path))))))
              assets)))

;;; --- the worker -------------------------------------------------------------------

(defun job-progress (folder state)
  "Run STATE's argv in FOLDER, bounded by the receipt's deadline, and record
what happened. Called on a thread of its own; never replayed."
  (let* ((argv (getf state :argv))
         (left (max 1 (round (- (getf state :deadline) (nlk:unix-now))))))
    (setf (getf state :status) "running")
    (save-state folder state)
    (let ((status (funcall *run-cli* argv folder left (getf state :log))))
      (cond
        ((eq status :timeout)
         (setf (getf state :status) "timed_out"
               (getf state :error) "The local deadline expired; the remote run may still be active"))
        ((and (integerp status) (zerop status))
         (let ((artifacts (collect-assets (merge-pathnames "output/" (job-folder (getf state :job-id))))))
           (setf (getf state :assets) artifacts)
           (if artifacts
               (setf (getf state :status) "succeeded")
               (setf (getf state :status) "failed"
                     (getf state :error) "The CLI returned no valid GLB header"))))
        (t (setf (getf state :status) "failed"
                 (getf state :error) (format nil "The CLI exited with status ~a; its log is ~a"
                                              status (getf state :log)))))
      (setf (getf state :finished-at) (nlk:unix-now))
      (save-state folder state))))

(defun start-job (image timeout faces preset)
  "Queue one job over IMAGE and start its worker. => its receipt's text."
  (let* ((id (job-id))
         (folder (job-folder id))
         (output (merge-pathnames "output/" folder))
         (source (merge-pathnames (format nil "input.~a" (string-downcase (pathname-type (pathname image))))
                                   folder)))
    (ensure-directories-exist output)
    (uiop:copy-file image source)
    (let* ((now (nlk:unix-now))
           (state (list :job-id id :status "queued" :backend (setting :backend)
                        :created-at now :deadline (+ now timeout) :finished-at 0
                        :image (namestring source) :assets '() :error nil
                        :log (namestring (merge-pathnames "cli.log" folder))
                        :argv (cli-argv source output id timeout faces preset))))
      (save-state folder state)
      (let ((runner *run-cli*))
        ;; SPAWN takes body forms, not a lambda: the body is the thread.
        (nlk:spawn "remesher-job"
          (let ((*run-cli* runner))
            (handler-case (job-progress folder (read-state folder))
              (error (condition)
                (let ((failed (read-state folder)))
                  (when failed
                    (setf (getf failed :status) "failed"
                          (getf failed :error) (format nil "The worker failed: ~a"
                                                       (nlk:one-line (princ-to-string condition)))
                          (getf failed :finished-at) (nlk:unix-now))
                    (ignore-errors (save-state folder failed)))))))))
      (state-object state))))

;;; --- the command line --------------------------------------------------------------

(defun cli-argv (source output id timeout faces preset)
  "The CLI's argv for one job: the backend's own options, then the image, the
output directory, the prefix, the face target or preset, and the deadline."
  (let ((backend (setting :backend)))
    (append
     (list (setting :cli) "image-to-glb")
     (cond
       ((string= backend "comfy3d")
        (list "--config" (setting :config) "--workflow-file" (setting :workflow)))
       ((string= backend "native")
        (list "--backend" "native" "--native-model" (setting :native-model)
              "--native-server" (setting :native-server)))
       (t (list "--backend" "native-local" "--native-model" (setting :native-model)
                "--native-python" (setting :native-python)
                "--native-checkout" (setting :native-checkout)
                "--native-device" (setting :native-device))))
     (list "--image" (namestring source)
           "--out-dir" (namestring output)
           "--filename-prefix" (format nil "nodecode_~a" id))
     (if preset
         (list "--native-preset" preset)
         (list "--target-face-num" (princ-to-string faces)))
     (list "--timeout" (princ-to-string timeout)))))

(defun one-active-job ()
  "The id of the job already queued or running, or NIL: the original's
one-job-at-a-time rule, so two submissions never race one GPU."
  (loop for file in (directory (merge-pathnames "*/state.json" (jobs-root)))
        for state = (ignore-errors (read-state (uiop:pathname-directory-pathname file)))
        when (and state (member (getf state :status) '("queued" "running") :test #'string=))
          return (getf state :job-id)))

;;; --- remesher_generate ------------------------------------------------------------

(defun generate (call)
  "The remesher_generate tool: one bounded image-to-GLB job, queued."
  (answering
    (let* ((image (nlk:trimmed (or (remesher-generate-image call) "")))
           (timeout (or (remesher-generate-timeout-seconds call) 900))
           (asked-faces (remesher-generate-target-face-num call))
           (preset (nonblank (remesher-generate-native-preset call)))
           (backend (setting :backend)))
      (when (zerop (length image))
        (return-from generate (refuse "image must be the absolute path of a local PNG, JPEG or WebP")))
      (unless (uiop:absolute-pathname-p (pathname image))
        (return-from generate (refuse "image ~s is relative: give an absolute path" image)))
      (unless (and (probe-file image) (not (uiop:directory-pathname-p (pathname image))))
        (return-from generate (refuse "image ~s is not an existing file" image)))
      (let ((type (string-downcase (or (pathname-type (pathname image)) ""))))
        (unless (member type '("png" "jpg" "jpeg" "webp") :test #'string=)
          (return-from generate (refuse "image ~s is a .~a: only PNG, JPEG and WebP are accepted" image type))))
      (when (> (with-open-file (in image :element-type '(unsigned-byte 8)) (file-length in))
               (* 20 1024 1024))
        (return-from generate (refuse "image ~s is larger than 20 MiB" image)))
      (unless (and (integerp timeout) (<= 1 timeout +timeout-limit+))
        (return-from generate (refuse "timeout_seconds must be an integer from 1 to ~a" +timeout-limit+)))
      (when (and asked-faces (not (and (integerp asked-faces) (<= 1000 asked-faces 200000))))
        (return-from generate (refuse "target_face_num must be an integer from 1000 to 200000")))
      (when (and preset (string= backend "comfy3d"))
        (return-from generate
          (refuse "native_preset needs a native backend; this cell is set to comfy3d")))
      (when (and preset asked-faces)
        (return-from generate
          (refuse "native_preset and target_face_num conflict: the preset owns the face target. Omit one")))
      (nlk:when-let (busy (one-active-job))
        (return-from generate
          (refuse "job ~a is still active: poll it with remesher_job, one job at a time" busy)))
      (start-job image timeout (or asked-faces 80000) preset))))

;;; --- remesher_job and remesher_assets ---------------------------------------------

(defun job-state (job-id &aux (folder (job-folder job-id)))
  "The receipt of JOB-ID, an integrity_error state when its artifacts no
longer match, or NIL when there is none."
  (let ((state (read-state folder)))
    (when (and state (string= (getf state :status) "succeeded")
               (not (assets-intact-p (getf state :assets))))
      (setf (getf state :status) "integrity_error"
            (getf state :assets) '()
            (getf state :error) "Saved artifacts are missing, changed, or unreadable"))
    state))

(defun read-one (call reader-name)
  "The job a read tool names, through CALL, or NIL when the id is not one."
  (let ((id (nlk:trimmed (or (funcall reader-name call) ""))))
    (unless (job-id-p id)
      (return-from read-one (cons :invalid (refuse "job_id must be 32 lowercase hex characters"))))
    (let ((state (job-state id)))
      (if state
          (cons :job state)
          (cons :missing (answer (nlk:json-object "status" "not_found" "job_id" id)))))))

(defun job (call)
  "The remesher_job tool: one job's persisted state."
  (answering
    (let ((found (read-one call #'remesher-job-read-job-id)))
      (if (eq (car found) :job)
          (state-object (cdr found))
          (cdr found)))))

(defun assets (call)
  "The remesher_assets tool: the artifacts of a successful job."
  (answering
    (let ((found (read-one call #'remesher-assets-read-job-id)))
      (if (eq (car found) :job)
          (let ((state (cdr found)))
            (answer (nlk:json-object "job_id" (getf state :job-id)
                                      "status" (getf state :status)
                                      "assets" (if (string= (getf state :status) "succeeded")
                                                   (map 'vector #'asset-object (getf state :assets))
                                                   #()))))
          (cdr found)))))

;;; --- remesher_health ----------------------------------------------------------------

(defun native-config-file ()
  "A config file naming the loopback server, for the CLI's own health check."
  (let* ((folder (nlk:home (format nil "remesher/tmp/~a/" (job-id))))
         (file (merge-pathnames "config.json" folder)))
    (ensure-directories-exist file)
    (nlk:write-file-atomically
     file (nlk:encode-json-object (nlk:json-object "server_url" (setting :native-server)))
     :mode #o600 :directory-mode #o700)
    file))

(defun health (call)
  "The remesher_health tool: connectivity through the CLI, or a native-local
configuration without starting anything."
  (declare (ignore call))
  (answering
    (let ((backend (setting :backend)))
      (if (string= backend "native-local")
          (answer (nlk:json-object "status" "configured"
                                    "backend" backend
                                    "scope" "configuration_only"
                                    "gpu_generation_verified" nil
                                    "native_model" (setting :native-model)))
          (let* ((config (if (string= backend "comfy3d") (setting :config) (native-config-file)))
                 (status (funcall *run-cli* (list (setting :cli) "health" "--config" config)
                                  (uiop:temporary-directory) 25 nil)))
            (answer (nlk:json-object "backend" backend
                                      "scope" "connectivity_only"
                                      "status" (cond ((eq status :timeout) "timed_out")
                                                     ((and (integerp status) (zerop status)) "healthy")
                                                     (t "unhealthy"))
                                      "returncode" (if (integerp status) status -1)
                                      "gpu_generation_verified" nil)))))))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell remesher
  (:section ("remesher")
    (:guide "point cli at the Remesher CLI you already installed (comfy-prompt-cli); comfy3d also needs its config and workflow files, native-local the CLI's python and checkout; next step: ask for a GLB from an image")
    ("cli" :path :required t
     :doc "the absolute path of the installed Remesher CLI (comfy-prompt-cli); the cell installs nothing")
    ("backend" :choice :options (quote ("comfy3d" "native" "native-local")) :default "comfy3d"
     :doc "comfy3d: an existing ComfyUI/Comfy3D server through the CLI's config; native: an existing numeric-loopback native server; native-local: the CLI starts and owns a local server")
    ("config" :path
     :doc "comfy3d: the CLI's config.json naming your ComfyUI server")
    ("workflow" :path
     :doc "comfy3d: the Comfy3D workflow JSON the CLI submits")
    ("native_server" :string :default "http://127.0.0.1:8188"
     :doc "native: the numeric-loopback HTTP origin of the native ComfyUI server")
    ("native_model" :choice :options (quote ("trellis2" "pixal3d")) :default "trellis2"
     :doc "native and native-local: which native model the CLI drives")
    ("native_python" :path
     :doc "native-local: the checkout's own python, symlinks left unresolved so the venv keeps its identity")
    ("native_checkout" :path
     :doc "native-local: the CLI's checkout, the directory that holds main.py")
    ("native_device" :choice :options (quote ("cuda" "cpu")) :default "cuda"
     :doc "native-local: the device the owned local server runs on"))
  (:settings #'read-settings)
  (:help :remesher
         "remesher: remesher_generate turns an image into a GLB through your installed Remesher CLI; remesher_job and remesher_assets read the job"
         +primer+)
  (:tool "remesher_generate" (nlk:contract 'remesher-generate) #'generate)
  (:tool "remesher_job" (nlk:contract 'remesher-job-read) #'job)
  (:tool "remesher_assets" (nlk:contract 'remesher-assets-read) #'assets)
  (:tool "remesher_health" (no-arguments
                             "Check the configured backend: the CLI's own health against the server (connectivity only), or a native-local configuration without starting it. Never GPU readiness.")
  #'health))
