;;;; cell.lisp --- the cell: the grok CLI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The provider rides a lane of its own name, grok-acp, whose stream
;;;; function is this cell's (CALL-GROK-STREAMING, provider.lisp): a round
;;;; spawns `grok agent --no-leader stdio' (acp.lisp), speaks the Agent
;;;; Client Protocol down its pipes, and folds the answer back. The other
;;;; hooks say the rest of the truth to the host:
;;;;
;;;;   :CREDENTIAL                    the login is the grok CLI's own, which
;;;;                                   this image never holds
;;;;   CONFIGURED-PROVIDER-INVENTORY  /models lists grok-acp/<model>
;;;;
;;;; Config, a top-level key:
;;;;   "grok-acp": {"command": "grok", "args": ["agent", "--no-leader", "stdio"],
;;;;                "models": ["grok-4.7", "grok-4.6"]}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-grok-acp)

(defun credential (op next)
  "The :CREDENTIAL answer for grok-acp: a placeholder, since the login is
the CLI's and no key is stored here."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential +provider+ :oauth)
      (funcall next op)))

(defun model-rows ()
  "The /models rows grok-acp offers: the section's models, each a model the
CLI selects after session/new."
  (loop for id in (coerce (setting :models) 'list)
        collect (list :provider-id +provider+
                      :provider-display "Grok (grok CLI)"
                      :model-id id
                      :context-window nil)))

(defun inventory (next &rest arguments)
  "CONFIGURED-PROVIDER-INVENTORY advice: grok-acp among the credentialed
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
  "The grok-acp lane: this cell's own stream function under it."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                             :stream-symbol 'call-grok-streaming
                             :family :openai-completions
                             :reasoning-carry :text
                             :default-endpoint "grok-acp://stdio")))

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
    (refuse "grok-acp: command names the grok CLI, a name on PATH or a path"))
  values)

(defun start ()
  "Register the lane, and say so when the section names no CLI or no such
CLI is installed."
  (let ((command (setting :command)))
    (cond
      ((not (and (stringp command) (plusp (length command))))
       (nle:notice "grok-acp: set \"command\" to the grok CLI in the grok-acp section -- {\"grok-acp\": {\"command\": \"grok\"}} -- then (restart-cells \"nodecode-grok-acp\")"
                   :level :warning :key "nodecode-grok-acp")
       (nle:on-stop (lambda () (nle:notice nil :key "nodecode-grok-acp"))))
      (t
       (register-lane)
       (nle:on-stop #'unregister-lane)
       (unless (command-present-p command)
         (nle:notice (format nil "grok-acp: no ~a on PATH, so grok-acp/ models cannot run; install the grok CLI, run `grok' once and sign in, then (restart-cells \"nodecode-grok-acp\")"
                             command)
                     :level :warning :key "nodecode-grok-acp")
         (nle:on-stop (lambda () (nle:notice nil :key "nodecode-grok-acp"))))))))

(nle:define-cell grok-acp
  (:section ("grok-acp")
    (:guide "command is the grok CLI, logged in with `grok' on this machine; args is its ACP argv; models are what /models lists as grok-acp/<model>")
    ("command" :string :required t
     :doc "the grok CLI: a name on PATH or a path")
    ("args" :list :default (vector "agent" "--no-leader" "stdio")
     :doc "the argv handed to the CLI after its name: `stdio' is the ACP transport, `--no-leader' keeps the request off a shared leader")
    ("models" :list :default +default-models+
     :doc "the model ids /models offers under grok-acp/, each selected after session/new")
    ("effort" :string :default ""
     :doc "the reasoning effort of a turn that names none: xhigh, high, medium or low; empty leaves the CLI's own default"))
  (:settings #'read-settings)
  (:start #'start)
  (:hook :credential #'credential)
  (:hook 'nle::configured-provider-inventory #'inventory))
