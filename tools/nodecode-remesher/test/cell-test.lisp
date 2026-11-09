;;;; cell-test.lisp --- the remesher cell against a stubbed CLI.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The CLI is a stubbed NODECODE-REMESHER::*RUN-CLI* and every path is a
;;;; file in the scratch home: nothing starts a real CLI, reaches a ComfyUI
;;;; server or touches the operator's files. Each tool is called the way a
;;;; model's call reaches it — through the real dispatch, contract and all —
;;;; so the schema the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "remesher" "REMESHER-CELL-")

;;; --- fixtures --------------------------------------------------------------------

(defun remesher-scratch (name &optional (content "test"))
  "An absolute file named NAME under the scratch home, holding CONTENT."
  (let ((path (nlk:home (format nil "remesher-tests/~a" name))))
    (ensure-directories-exist path)
    (unless (probe-file path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
        (write-string content out)))
    (namestring (truename path))))

(defun remesher-checkout ()
  "A directory that looks like a native-local checkout: it holds main.py."
  (let* ((dir (nlk:home "remesher-tests/checkout/"))
         (main (merge-pathnames "main.py" dir)))
    (ensure-directories-exist main)
    (unless (probe-file main)
      (with-open-file (out main :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
        (write-string "# a stub checkout" out)))
    (namestring (truename dir))))

(defun remesher-config (&rest pairs)
  "The whole shared config the `remesher' section PAIRS make, the cli and the
comfy3d files defaulted to scratch files."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "cli" section (remesher-scratch "cli"))
    (alexandria:ensure-gethash "backend" section "comfy3d")
    (when (equal "comfy3d" (gethash "backend" section))
      (alexandria:ensure-gethash "config" section (remesher-scratch "config.json" "{\"server_url\":\"http://127.0.0.1:8188/\"}"))
      (alexandria:ensure-gethash "workflow" section (remesher-scratch "workflow.json" "{}")))
    (nlk:json-object "remesher" section)))

(defun remesher-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-remesher:start-cell (apply #'remesher-config pairs)))

;;; --- a GLB, and the stub that writes one ------------------------------------------

(defparameter +glb+
  (make-array 12 :element-type '(unsigned-byte 8)
              :initial-contents '(103 108 84 70 2 0 0 0 12 0 0 0))
  "A twelve-byte GLB: the magic, version 2, and a total length of twelve.")

(defun argv-value (argv flag)
  "ARGV's word after FLAG, or NIL."
  (second (member flag argv :test #'string=)))

(defun stub-writes-glb (argv &optional (kind "glb"))
  "The stub CLI's answer to one image-to-glb call: a GLB, named as the
--filename-prefix asked, in the job's output directory. => 0."
  (let* ((out (uiop:ensure-directory-pathname (argv-value argv "--out-dir")))
         (file (merge-pathnames (format nil "~a.~a" (argv-value argv "--filename-prefix") kind)
                                 out)))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede :if-does-not-exist :create)
      (write-sequence +glb+ stream))
    0))

(defun remesher-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun remesher-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (remesher-answer id tool arguments)))

(defun remesher-image (&optional (name "masked.png"))
  "An absolute path to a scratch image, as a generate call names it."
  (remesher-scratch name "not really a png"))

(defun await-job (job-id &optional (limit 300))
  "JOB-ID's state once it is terminal, polling as a model would."
  (loop repeat limit
        for object = (remesher-object "await" "remesher_job" (format nil "{\"job_id\":\"~a\"}" job-id))
        for status = (nlk:json-value object :string "status")
        unless (member status '("queued" "running") :test #'string=)
          return object
        do (sleep 0.02)
        finally (return nil)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "remesher"
  (:config "cli" (remesher-scratch "cli")
           "config" (remesher-scratch "config.json" "{\"server_url\":\"http://127.0.0.1:8188/\"}")
           "workflow" (remesher-scratch "workflow.json" "{}"))
  (:help :remesher)
  (:running (is (nle::find-tool "remesher_generate") "the generate tool is registered")
            (is (nle::find-tool "remesher_job") "the job tool is registered")
            (is (nle::find-tool "remesher_assets") "the assets tool is registered")
            (is (nle::find-tool "remesher_health") "the health tool is registered"))
  (:refused ("backend" "somewhere-else")
            ("native_model" "sdxl")))

(deftest remesher-cell-without-a-cli-refuses ()
  ;; The one setting no verb can work without: no cli, no job.
  (let ((condition (signals-error nlk:config-refusal (remesher-start "cli" ""))))
    (is (search "cli" (princ-to-string condition)) "the refusal names the missing member")
    (is (null (nle::find-tool "remesher_generate")) "and installs nothing"))
  (let ((condition (signals-error nlk:config-refusal
                      (remesher-start "cli" "/nonexistent/remesher-cli"))))
    (is (search "cli" (princ-to-string condition)) "a cli that is not there is refused too")))

;;; --- remesher_generate --------------------------------------------------------------

(deftest remesher-cell-generates-a-job-and-hashes-its-artifact ()
  (with-cell-stop ((remesher-start))
    (let ((nodecode-remesher::*run-cli*
            (lambda (argv directory seconds log)
              (declare (ignore directory seconds log))
              (stub-writes-glb argv))))
      (let* ((answer (remesher-object "gen-1" "remesher_generate"
                                       (format nil "{\"image\":\"~a\"}" (remesher-image))))
             (job-id (nlk:json-value answer :string "job_id")))
        (is (= 32 (length job-id)) "generate answers a 32-hex job id at once")
        (let ((state (await-job job-id)))
          (is (equal "succeeded" (nlk:json-value state :string "status")) "the worker finished the job")
          (let* ((artifacts (remesher-object "assets-1" "remesher_assets"
                                              (format nil "{\"job_id\":\"~a\"}" job-id)))
                 (assets (nlk:json-array artifacts "assets")))
            (is (= 1 (length assets)) "one GLB was collected")
            (is (equal "model/gltf-binary" (nlk:json-value (aref assets 0) :string "media_type")))
            (is (equal (nlk:sha256-text (nodecode.term:file-octets
                                          (nlk:json-value (aref assets 0) :string "path")))
                       (nlk:json-value (aref assets 0) :string "sha256"))
                "the manifest's hash is the file's own")))))))

(deftest remesher-cell-a-missing-artifact-is-an-integrity-error ()
  (with-cell-stop ((remesher-start))
    (let ((nodecode-remesher::*run-cli*
            (lambda (argv directory seconds log)
              (declare (ignore directory seconds log))
              (stub-writes-glb argv))))
      (let* ((answer (remesher-object "gen-2" "remesher_generate"
                                       (format nil "{\"image\":\"~a\"}" (remesher-image "second.png"))))
             (job-id (nlk:json-value answer :string "job_id"))
             (state (await-job job-id))
             (path (nlk:json-value (aref (nlk:json-array state "assets") 0) :string "path")))
        (is (equal "succeeded" (nlk:json-value state :string "status")))
        (delete-file path)
        (let ((after (remesher-object "assets-2" "remesher_assets"
                                       (format nil "{\"job_id\":\"~a\"}" job-id))))
          (is (equal "integrity_error" (nlk:json-value after :string "status"))
              "a saved artifact that is gone is not delivered")
          (is (zerop (length (nlk:json-array after "assets"))) "and no assets are answered"))))))

(deftest remesher-cell-refuses-a-bad-image-and-a-second-active-job ()
  (with-cell-stop ((remesher-start))
    (let ((nodecode-remesher::*run-cli* (lambda (argv directory seconds log)
                                          (declare (ignore directory seconds log))
                                          (sleep 0.3)
                                          (stub-writes-glb argv))))
      (let ((relative (remesher-answer "bad-1" "remesher_generate" "{\"image\":\"local.png\"}"))
            (missing (remesher-answer "bad-2" "remesher_generate"
                                       "{\"image\":\"/nonexistent/none.png\"}")))
        (is (search "absolute" relative) "a relative image path is refused")
        (is (search "not an existing file" missing) "a missing image is refused"))
      (let* ((first (remesher-object "gen-3" "remesher_generate"
                                      (format nil "{\"image\":\"~a\"}" (remesher-image "third.png"))))
             (second (remesher-answer "gen-4" "remesher_generate"
                                       (format nil "{\"image\":\"~a\"}" (remesher-image "fourth.png")))))
        (is (= 32 (length (nlk:json-value first :string "job_id"))) "the first job is queued")
        (is (search "still active" second)
            "a second submission while one runs is refused as busy")
        (let ((state (await-job (nlk:json-value first :string "job_id"))))
          (is (equal "succeeded" (nlk:json-value state :string "status")) "and the first one finishes"))))))

(deftest remesher-cell-an-unknown-job-is-not-found ()
  (with-cell-stop ((remesher-start))
    (let ((answer (remesher-object "job-1" "remesher_job"
                                    "{\"job_id\":\"0123456789abcdef0123456789abcdef\"}")))
      (is (equal "not_found" (nlk:json-value answer :string "status"))))
    (let ((answer (remesher-answer "job-2" "remesher_job" "{\"job_id\":\"not-a-job\"}")))
      (is (search "32 lowercase hex" answer) "a malformed id is refused before the disk"))))

;;; --- remesher_health ----------------------------------------------------------------

(deftest remesher-cell-health-asks-the-cli-and-native-local-does-not ()
  (with-cell-stop ((remesher-start))
    (let* ((asked '())
          (nodecode-remesher::*run-cli*
            (lambda (argv directory seconds log)
              (declare (ignore directory seconds log))
              (push argv asked)
              0)))
      (let ((answer (remesher-object "health-1" "remesher_health" "{}")))
        (is (equal "healthy" (nlk:json-value answer :string "status")) "a zero exit is healthy")
        (is (equal "connectivity_only" (nlk:json-value answer :string "scope")))
        (is (eq nil (nlk:json-value answer :boolean "gpu_generation_verified")))
        (is (equal "health" (second (first asked))) "the CLI's own health ran")
        (is (equal (remesher-scratch "config.json" "{\"server_url\":\"http://127.0.0.1:8188/\"}")
                   (fourth (first asked)))
            "against the configured config file"))))
  (with-cell-stop ((remesher-start "backend" "native-local"
                                   "native_python" (remesher-scratch "python")
                                   "native_checkout" (remesher-checkout)))
    (let ((nodecode-remesher::*run-cli*
            (lambda (argv directory seconds log)
              (declare (ignore argv directory seconds log))
              (error "native-local health must not start anything"))))
      (let ((answer (remesher-object "health-2" "remesher_health" "{}")))
        (is (equal "configured" (nlk:json-value answer :string "status")))
        (is (equal "configuration_only" (nlk:json-value answer :string "scope")))))))
