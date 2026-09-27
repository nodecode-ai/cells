;;;; addon.lisp --- the add-on: one lane, the request path under it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The claude-code provider rides a lane of its own name whose fold is the
;;;; Anthropic lane's (CALL-ANTHROPIC-STREAMING): Nodecode builds the round's
;;;; Messages body as for any Anthropic model. Five hooks make that body the
;;;; CLI's request and keep the rest of the organism honest about it:
;;;;
;;;;   WALK-PROVIDER-STREAM            a claude-code round hands its body to the
;;;;                                   CLI (cli.lisp) and sends back what it
;;;;                                   wrote, folding the stream with the tool
;;;;                                   names turned back (wire.lisp)
;;;;   :CREDENTIAL                     the provider's credential is the CLI's
;;;;                                   login, which this image never holds
;;;;   CONFIGURED-PROVIDER-INVENTORY   /models lists claude-code/<model>
;;;;   RESOLVE-MODEL-CAPABILITY        a model's window, output ceiling and
;;;;                                   effort ladder are Anthropic's own row
;;;;   MODEL-PRICE                     none: a subscription round is not
;;;;                                   billed per token, so no round is priced
;;;;
;;;; Config, a sibling top-level key next to `websearch' and `cron':
;;;;   "claude-code": {"command": "claude",
;;;;                   "models": ["claude-sonnet-5", "claude-haiku-4-5"],
;;;;                   "timeout_seconds": 120}
;;;; A vetoed section ("enabled": false) installs nothing: no lane, no hook.

(in-package #:nodecode-claude-code)

(defun claude-code-config-p (config)
  "Whether the frozen provider CONFIG rides the claude-code lane."
  (and config (equal (nle::effective-provider-config-lane config) +provider+)))

(defun round-directory ()
  "Where the CLI runs for this round: the session's own directory, so the
environment the CLI describes to the model is the one Nodecode's prompt
names; the home directory outside a turn."
  (let* ((turn (nle:turn))
         (cwd (and turn (nlk:find-session-cwd (getf turn :session-id)))))
    (or (and cwd (ignore-errors (probe-file (uiop:ensure-directory-pathname cwd))))
        (user-homedir-pathname))))

(defun walk (next fold &rest keys &key config request-json &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a claude-code round sends the request the
CLI wrote for its body, and folds the answer with Nodecode's tool names."
  (if (claude-code-config-p config)
      (multiple-value-bind (headers endpoint octets)
          (author-request (nlk:decode-json request-json)
                          :model (nle::effective-provider-config-model config)
                          :window (nle::effective-provider-config-context-tokens config)
                          :directory (round-directory))
        (apply next (unprefixed-fold fold)
               :request-json octets :headers headers :endpoint endpoint
               (alexandria:remove-from-plist keys :request-json :headers :endpoint)))
      (apply next fold keys)))

(defun credential (op next)
  "The :CREDENTIAL answer for claude-code: a placeholder key, since the login
is the CLI's and every header the round sends is the CLI's own."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential +provider+ :oauth)
      (funcall next op)))

(defun model-rows ()
  "The /models rows the claude-code provider offers: the section's models,
each with Anthropic's window for it."
  (loop for id in (coerce (setting :models) 'list)
        collect (list :provider-id +provider+
                      :provider-display "Claude Code"
                      :model-id id
                      :context-window (ignore-errors
                                       (nle::model-capability-context-tokens
                                        (nle::resolve-model-capability id +provider+))))))

(defun inventory (next &rest arguments)
  "CONFIGURED-PROVIDER-INVENTORY advice: claude-code among the credentialed
providers, its models among the configured ones."
  (multiple-value-bind (ids models) (apply next arguments)
    (values (sort (adjoin +provider+ ids :test #'string-equal) #'string<)
            (append models (model-rows)))))

(defun capability (next model-id &optional (provider nle::*provider*))
  "RESOLVE-MODEL-CAPABILITY advice: a claude-code model is the Anthropic
row of the same id."
  (funcall next model-id (if (equal provider +provider+) "anthropic" provider)))

(defun price (next model-id provider)
  "MODEL-PRICE advice: no price for a claude-code model."
  (unless (equal provider +provider+)
    (funcall next model-id provider)))

(defun register-lane ()
  "The claude-code lane: the Anthropic fold under this provider's name."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'nle::call-anthropic-streaming
                            :family :anthropic
                            :reasoning-carry :text
                            :default-endpoint (concatenate 'string +upstream+ "/v1/messages")
                            :path "/messages")))

(defun unregister-lane ()
  "Take the claude-code lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "Register the lane, and say so when there is no CLI to drive."
  (register-lane)
  (nle:on-stop #'unregister-lane)
  (unless (cli-program (setting :command))
    (nle:notice (format nil "claude-code: no ~a on PATH, so claude-code/ models cannot run; install Claude Code, run `claude' once and /login, then (restart-addons \"nodecode-claude-code\")"
                        (setting :command))
                :level :warning :key "nodecode-claude-code")
    (nle:on-stop (lambda () (nle:notice nil :key "nodecode-claude-code")))))

(nle:define-addon claude-code
  (:section ("claude-code")
    (:guide "command is the claude CLI, logged in with `claude' then /login; models are what /models lists as claude-code/<model>")
    ("command" :string :default "claude"
     :doc "the Claude Code CLI: a name on PATH or a path")
    ("models" :list :default (vector "claude-opus-5-5" "claude-sonnet-5" "claude-haiku-4-5" "claude-fable-5-1")
     :doc "the Anthropic model ids /models offers under claude-code/")
    ("timeout_seconds" :integer :default 120 :min 1
     :doc "how long the CLI may take to write one round's request, its history replay included"))
  (:start #'start)
  (:hook :credential #'credential)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::configured-provider-inventory #'inventory)
  (:hook 'nle::resolve-model-capability #'capability)
  (:hook 'nle::model-price #'price))
