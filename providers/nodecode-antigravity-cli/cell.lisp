;;;; cell.lisp --- the cell: the Antigravity CLI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The provider rides a lane of its own name, antigravity-cli, whose stream
;;;; function is this cell's (CALL-AGY-STREAMING, provider.lisp): a round is
;;;; one `agy' child fed the prompt, its stream-json events folded into the
;;;; organism's parts. The other hooks say the rest of the truth to the host:
;;;;
;;;;   :CREDENTIAL                    the login is the CLI's own, which this
;;;;                                   image never holds
;;;;   CONFIGURED-PROVIDER-INVENTORY  /models lists antigravity-cli/<model>
;;;;   RESOLVE-MODEL-CAPABILITY       a model's window is Google's row of it
;;;;
;;;; Config, a sibling top-level key:
;;;;   "antigravity-cli": {"command": "agy", "models": [...], "effort": "high"}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-antigravity-cli)

(defun credential (op next)
  "The :CREDENTIAL answer for antigravity-cli: a placeholder, since the login
is the CLI's and no request leaves this image."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential +provider+ :oauth)
      (funcall next op)))

(defun model-rows ()
  "The /models rows this provider offers: the section's models, each with
Google's declared window for it."
  (loop for id in (coerce (setting :models) 'list)
        collect (list :provider-id +provider+
                      :provider-display "Antigravity CLI"
                      :model-id id
                      :context-window (ignore-errors
                                       (nle::model-capability-context-tokens
                                        (nle::resolve-model-capability id +provider+))))))

(defun inventory (next &rest arguments)
  "CONFIGURED-PROVIDER-INVENTORY advice: antigravity-cli among the
credentialed providers, its models among the configured ones."
  (multiple-value-bind (ids models) (apply next arguments)
    (values (sort (adjoin +provider+ ids :test #'string-equal) #'string<)
            (append models (model-rows)))))

(defun capability (next model-id &optional (provider nle::*provider*))
  "RESOLVE-MODEL-CAPABILITY advice: an antigravity-cli model is the Google
row of the same id."
  (funcall next model-id (if (equal provider +provider+) "google" provider)))

(defun register-lane ()
  "The antigravity-cli lane: this cell's own stream function under it."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                             :stream-symbol 'call-agy-streaming
                             :family :google
                             :reasoning-carry :text
                             :default-endpoint "agy://local")))

(defun unregister-lane ()
  "Take the lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes*
                :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "Register the lane, and say so when there is no CLI to drive."
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (unless (cli-present-p (agy-command))
    (nle:notice (format nil "antigravity-cli: no ~a on PATH, so antigravity-cli/ models cannot run; install the Antigravity CLI, sign it in, then (restart-cells \"nodecode-antigravity-cli\")"
                        (agy-command))
                :level :warning :key "nodecode-antigravity-cli")
    (nle:on-stop (lambda () (nle:notice nil :key "nodecode-antigravity-cli")))))

(nle:define-cell antigravity-cli
  (:section ("antigravity-cli")
    (:guide "command is the agy CLI, signed in with `agy' on this machine; models are what /models lists as antigravity-cli/<model>; effort is passed to agy as --effort")
    ("command" :string :default "agy"
     :doc "the Antigravity CLI: a name on PATH or a path")
    ("models" :list :default +default-models+
     :doc "the model ids /models offers under antigravity-cli/")
    ("effort" :string :default ""
     :doc "the thinking effort handed to agy as --effort (low, medium, high); empty names none"))
  (:start #'start)
  (:hook :credential #'credential)
  (:hook 'nle::configured-provider-inventory #'inventory)
  (:hook 'nle::resolve-model-capability #'capability))
