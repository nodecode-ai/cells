;;;; provider.lisp --- what the apple provider is: the Mac it runs on, the bridge, the one model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/apple.kdl and providers/apple.kdl, coding-agent/src/config/
;;;; model-discovery.ts (discoverAppleFoundationModels) and model-registry.ts
;;;; (offered on darwin/arm64 only), and crates/pi-natives/src/applefm/ (the
;;;; bridge and how it is built).
;;;;
;;;; Apple Foundation Models is the system language model of macOS 27 on Apple
;;;; silicon, reached only through Apple's FoundationModels framework. omp
;;;; compiles a Swift bridge (bridge.swift) into a dylib its native addon
;;;; loads, behind a small C ABI. The cell builds the same bridge, beside a
;;;; main.swift of its own, into an executable it runs per request: the
;;;; bridge's events arrive on Swift's own threads, which a Lisp image cannot
;;;; take as callbacks, and a child's stdout carries them as lines instead.
;;;; Nothing is bundled: the one model, `on-device', is offered when the
;;;; bridge says the model is available, sized as the bridge reports it.

(in-package #:nodecode-apple)

(defparameter +base+ "local://apple-foundation-models"
  "The base omp's discovered row names: no address, the lane's own marker.")

(defparameter +model+ "on-device"
  "The one model's id (discoverAppleFoundationModels).")

(defparameter +default-context+ 128000
  "The window when the bridge names none (DISCOVERY_DEFAULT_CONTEXT_WINDOW).")

(defparameter +default-output+ 32768
  "The output ceiling under the window (DISCOVERY_DEFAULT_MAX_TOKENS).")

(defun mac-p ()
  "Whether this is a Mac with Apple silicon, where omp offers the provider."
  (and (eq (uiop:operating-system) :macosx)
       (member (uiop:architecture) '(:arm64 :aarch64))
       t))

;;; --- the bridge helper ----------------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun bridge-file (name)
  "The file NAME of this cell's bridge/ folder."
  (asdf:system-relative-pathname "nodecode-apple" (format nil "bridge/~a" name)))

(defun built-helper ()
  "Where the helper this cell builds lives: the user cache, named by the
sources it was built from, so a changed bridge builds again."
  (let ((digest (nlk:sha256-text (concatenate 'string
                                              (uiop:read-file-string (bridge-file "bridge.swift"))
                                              (uiop:read-file-string (bridge-file "main.swift"))))))
    (merge-pathnames (format nil "nodecode-apple-bridge-~a" (subseq digest 7 19))
                     (uiop:xdg-cache-home "nodecode-apple/"))))

(defun build-helper (out)
  "Compile the helper into OUT with bridge/build.sh: (values OK-P WORDS)."
  (multiple-value-bind (output errors status)
      (uiop:run-program (list "/bin/sh" (uiop:native-namestring (bridge-file "build.sh"))
                              (uiop:native-namestring out))
                        :output :string :error-output :string :ignore-error-status t)
    (values (eql status 0) (nlk:trimmed (format nil "~a~%~a" (or errors "") (or output ""))))))

(defvar *helper-lock* (bt2:make-lock :name "nodecode-apple helper")
  "Held across a build, so two rounds never build at once.")

(defun helper ()
  "The helper a round runs: the section's, else the one this cell built,
built now when it is not there yet; a refusal says why there is none."
  (or (nonblank (setting :helper))
      (bt2:with-lock-held (*helper-lock*)
        (let ((out (built-helper)))
          (if (probe-file out)
              (uiop:native-namestring out)
              (multiple-value-bind (ok words) (build-helper out)
                (if ok
                    (uiop:native-namestring out)
                    (error 'nle::provider-config-error
                           :status 503
                           :detail (format nil "the Apple Foundation Models bridge could not be built: ~a"
                                           (nlk:clip (nlk:one-line words) 300 :ellipsis "…"))))))))))

;;; --- availability ----------------------------------------------------------------

(defvar *availability* nil
  "The bridge's last availability event (an object), or NIL before it was asked.")

(defun ask-availability (helper)
  "The availability event HELPER prints, or an object saying why there is none."
  (handler-case
      (multiple-value-bind (output errors status)
          (uiop:run-program (list helper "availability") :output :string :error-output :string
                                                         :ignore-error-status t)
        (or (and (eql status 0)
                 (let ((event (ignore-errors (nlk:decode-json (nlk:trimmed output)))))
                   (and (hash-table-p event) event)))
            (nlk:json-object "type" "availability" "available" nil "reason" "runtime"
                             "message" (nlk:clip (nlk:one-line (format nil "~a ~a" errors output)) 300 :ellipsis "…"))))
    (error (condition)
      (nlk:json-object "type" "availability" "available" nil "reason" "runtime"
                       "message" (princ-to-string condition)))))

(defun available-p ()
  "Whether the last availability said the model can generate."
  (nlk:json-value *availability* :boolean "available"))

(defun model-reasoning-p ()
  "Whether the model reasons in a channel of its own, as the bridge says."
  (nlk:json-value *availability* :boolean "reasoningCapable"))

(defun model-vision-p ()
  "Whether the model takes images, as the bridge says."
  (nlk:json-value *availability* :boolean "vision"))

(defun catalog-model ()
  "The one model as the catalog keeps it (discoverAppleFoundationModels)."
  (let* ((context (or (nlk:json-value *availability* :integer "contextSize") +default-context+))
         (variant (nlk:json-value *availability* :text "variant"))
         (tools (multiple-value-bind (value present) (gethash "toolCalling" *availability*)
                  (if present (eq value t) t))))
    (nle::make-catalog-model
     (if variant (format nil "Apple ~a" variant) "Apple Foundation Model")
     context
     (min context +default-output+)
     (if (model-vision-p) #("text" "image") #("text"))
     #("text")
     ;; no rungs are declared: the bridge's reasoning levels ride an effort
     ;; the turn already carries
     nil
     (model-reasoning-p)
     nil
     tools
     nil)))

(defun catalog-row ()
  "The apple provider as a models.dev provider: this cell's lane package,
omp's marker base, no key, and the one model while the bridge says it can
generate (none otherwise, as omp offers none)."
  (let ((models (make-hash-table :test 'equal)))
    (when (available-p)
      (setf (gethash +model+ models) (catalog-model)))
    (nlk:json-object "name" "Apple Foundation Models (on-device)"
                     "npm" +npm+
                     "api" +base+
                     "env" #()
                     "models" models)))
