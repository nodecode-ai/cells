;;;; cell.lisp --- the cell: the Kiro CLI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The provider rides a lane of its own name, kiro-acp, whose stream
;;;; function is this cell's (CALL-KIRO-STREAMING, provider.lisp): a round
;;;; spawns `kiro-cli acp' (acp.lisp), speaks the Agent Client Protocol down
;;;; its pipes, and folds the answer back. The other hooks say the rest of
;;;; the truth to the host:
;;;;
;;;;   :CREDENTIAL                    the login is the Kiro CLI's own, which
;;;;                                   this image never holds
;;;;   CONFIGURED-PROVIDER-INVENTORY  /models lists kiro-acp/<model>
;;;;
;;;; Config, a top-level key:
;;;;   "kiro-acp": {"command": "kiro-cli", "args": ["acp"],
;;;;                "models": ["default"]}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-kiro-acp)

(defun credential (op next)
  "The :CREDENTIAL answer for kiro-acp: a placeholder, since the login is
the CLI's and no key is stored here."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential +provider+ :oauth)
      (funcall next op)))

(defun model-rows ()
  "The /models rows kiro-acp offers: the section's models, each a model the
CLI selects after session/new; `default' is the agent's own session model."
  (loop for id in (coerce (setting :models) 'list)
        collect (list :provider-id +provider+
                      :provider-display "Kiro (AWS)"
                      :model-id id
                      :context-window nil)))

(defun inventory (next &rest arguments)
  "CONFIGURED-PROVIDER-INVENTORY advice: kiro-acp among the credentialed
providers, its models among the configured ones."
  (multiple-value-bind (ids models) (apply next arguments)
    (values (sort (adjoin +provider+ ids :test #'string-equal) #'string<)
            (append models (model-rows)))))

(defun command-present-p (command)
  "Whether COMMAND names a file: a path as it stands, or a name on PATH."
  (if (find #\/ command)
      (and (probe-file command) t)
      (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                 :separator (list #\:))
            for found = (and (plusp (length directory))
                             (probe-file (merge-pathnames
                                          command
                                          (uiop:ensure-directory-pathname directory))))
            when found do (return found))))

(defun register-lane ()
  "The kiro-acp lane: this cell's own stream function under it."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                             :stream-symbol 'call-kiro-streaming
                             :family :openai-completions
                             :reasoning-carry :text
                             :default-endpoint "kiro-acp://stdio")))

(defun unregister-lane ()
  "Take the lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes*
                :key #'nle::provider-lane-name :test #'equal)))

(defun refuse (control &rest arguments)
  "Refuse the section: a config refusal saying CONTROL with ARGUMENTS."
  (error 'nlk:config-refusal :detail (apply #'format nil control arguments)))

(defun read-settings (values table)
  "The section's declared VALUES, refusing a section that names no CLI: a
file is handed to nothing."
  (when (and table (let ((command (getf values :command)))
                     (or (not (stringp command)) (zerop (length command)))))
    (refuse "kiro-acp: command names the Kiro CLI, a name on PATH or a path"))
  values)

(defun start ()
  "Register the lane, and say so when the section names no CLI or no such
CLI is installed."
  (let ((command (setting :command)))
    (cond
      ((not (and (stringp command) (plusp (length command))))
       (nle:notice "kiro-acp: set \"command\" to the Kiro CLI in the kiro-acp section -- {\"kiro-acp\": {\"command\": \"kiro-cli\"}} -- then (restart-cells \"nodecode-kiro-acp\")"
                   :level :warning :key "nodecode-kiro-acp")
       (nle:on-stop (lambda () (nle:notice nil :key "nodecode-kiro-acp"))))
      (t
       (register-lane)
       (nle:on-stop #'unregister-lane)
       (unless (command-present-p command)
         (nle:notice (format nil "kiro-acp: no ~a on PATH, so kiro-acp/ models cannot run; install the Kiro CLI, run `kiro-cli` once and sign in, then (restart-cells \"nodecode-kiro-acp\")"
                             command)
                     :level :warning :key "nodecode-kiro-acp")
         (nle:on-stop (lambda () (nle:notice nil :key "nodecode-kiro-acp"))))))))

(nle:define-cell kiro-acp
  (:section ("kiro-acp")
    (:guide "command is the Kiro CLI, logged in with `kiro-cli` on this machine; args is its ACP argv (extra flags such as --agent or --effort ride there too); models are what /models lists as kiro-acp/<model>, `default` keeping the CLI's own session model")
    ("command" :string :required t
     :doc "the Kiro CLI: a name on PATH or a path, usually kiro-cli")
    ("args" :list :default (vector "acp")
     :doc "the argv handed to the CLI after its name: `acp' is the ACP transport; flags such as --agent NAME or -a ride here")
    ("models" :list :default +default-models+
     :doc "the model ids /models offers under kiro-acp/, each selected after session/new; `default` sends no model and keeps the CLI's session default")
    ("effort" :string :default ""
     :doc "the reasoning effort of a turn that names none; empty leaves the CLI's own default"))
  (:settings #'read-settings)
  (:start #'start)
  (:hook :credential #'credential)
  (:hook 'nle::configured-provider-inventory #'inventory))
