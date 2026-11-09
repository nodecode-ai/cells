;;;; cell.lisp --- one tool and one diagnostic command over the Kimi Code CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model asks for a second opinion, the cell starts the operator's own
;;;; `kimi acp', hands it one self-contained prompt, and answers what the
;;;; agent said. Kimi is never a model route here: the cell is delegate-only,
;;;; the call is read-only, and the agent's own login is the credential.
;;;; `kimi-code' is the doctor: who answered, which models it offers, and how
;;;; the login handshake went, before a real call is spent.
;;;;
;;;; A rewrite of s-a-s-k-i-a/hermes-kimi-code. See NOTICE.

(in-package #:nodecode-kimi-code-delegate)

;;; --- the manual ----------------------------------------------------------

(defparameter +primer+
  "`kimi_code_delegate' asks the operator's own Kimi Code CLI for read-only
advisory analysis: a prompt, an optional model (k3 or k3-256k) and an optional
thinking effort, and the answer's text comes back as JSON (ok, response,
model, thinking). Every call is one fresh `kimi acp' process and one session,
started in the configured working directory with an environment narrowed to the
names the CLI needs, so the CLI's own `kimi login' is the only credential and
nothing else of this process reaches it. Kimi is not a model route and cannot
change this session's model: a delegated call that fails leaves the session on
the model it had. It cannot touch the machine either - the client answers a
permission request with the agent's own reject option and a filesystem or
terminal request with an error - and an empty answer is a failure, not a turn.
/kimi-code doctor runs the handshake alone: who answered, the version, the
models offered and the current thinking effort. The section is
kimi-code-delegate in ~/.nodecode/config.jsonc: command (the CLI on PATH or a
path), working_directory, model, thinking, timeout_seconds and
startup_timeout_seconds. There is no key: the CLI is signed in once, in a
terminal, with `kimi login'.")

;;; --- the tool's arguments, declared once --------------------------------

(nlk:define-contract kimi-delegate-arguments
  (:doc "Ask the locally signed-in Kimi Code CLI for read-only advisory analysis. Use it only when an explicit Kimi opinion is worth a second call: it is not this session's model, cannot change it, and cannot touch files or run anything. Pass a self-contained task, since the agent sees nothing of this conversation.")
  ("prompt" :text :required t
            :doc "the task or question, whole: context, what was tried, what an answer should decide")
  ("model" :string :options '("k3" "k3-256k")
           :doc "k3, or k3-256k for a longer context; absent uses the section's model")
  ("thinking" :string
              :doc "the CLI's reasoning effort: auto keeps its current one; else an option id it offers"))

(defun argument-or-setting (value name)
  "VALUE when it was given, else the section's NAME."
  (let ((text (nlk:trimmed (or value ""))))
    (if (plusp (length text)) text (setting name))))

;;; --- the tool ------------------------------------------------------------

(defun delegate-tool (arguments)
  "The kimi_code_delegate tool: one delegated call, its JSON answer as text."
  (handler-case
      (let* ((prompt (check-prompt (kimi-delegate-arguments-prompt arguments)))
             (model (check-model (argument-or-setting (kimi-delegate-arguments-model arguments)
                                                      :model)))
             (thinking (check-thinking (argument-or-setting
                                        (kimi-delegate-arguments-thinking arguments)
                                        :thinking)))
             (result (delegate prompt model thinking)))
        (nlk:encode-json-object
         (nlk:json-object "ok" t
                          "response" (getf result :response)
                          "model" (getf result :model)
                          "thinking" (getf result :thinking)
                          "delegate_only" t
                          "read_only" t)))
    (kimi-code-delegate-error (condition)
      (nle:failure
       (nlk:encode-json-object
        (nlk:json-object "ok" nil
                         "error" (kimi-code-delegate-error-detail condition)
                         "delegate_only" t
                         "read_only" t))))))

;;; --- the doctor ----------------------------------------------------------

(defun run-doctor (args session-id)
  "The /kimi-code command: the handshake alone, before a real call is spent."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((and (plusp (length word)) (not (equal word "doctor")))
       (format nil "usage: /kimi-code doctor~%Runs the ACP handshake alone: the CLI, its login, ~
its models and its current thinking effort."))
      (t
       (handler-case
           (let ((result (probe)))
             (format nil "Kimi Code ACP: OK (~a ~a)~%ACP login handshake: accepted~%~
OAuth: not verified until a real delegated prompt succeeds~%Models offered: ~{~a~^, ~}~%~
Current thinking: ~a~%Routing: delegate-only - read-only, never this session's model~%"
                     (getf result :agent) (getf result :version)
                     (or (getf result :models) (list "none"))
                     (or (getf result :thinking) "unknown")))
         (kimi-code-delegate-error (condition)
           (format nil "Kimi Code ACP: FAILED~%~a" (kimi-code-delegate-error-detail condition))))))))

;;; --- the cell ------------------------------------------------------------

(nle:define-cell kimi-code-delegate
  (:section ("kimi-code-delegate")
    (:guide "install the Kimi Code CLI and run `kimi login' once in a terminal; command names it when it is not `kimi' on PATH, model picks k3 or k3-256k, working_directory is the folder a call runs in")
    ("command" :string :default "kimi"
               :doc "the Kimi Code CLI: a name on PATH, or a path to the executable")
    ("working_directory" :path
                         :doc "the folder a delegated call runs in; empty uses Nodecode's own")
    ("model" :choice :options '("k3" "k3-256k") :default "k3"
             :doc "k3, or k3-256k for a longer context")
    ("thinking" :string :default "auto"
                :doc "auto keeps the CLI's current effort; else an option id it offers")
    ("timeout_seconds" :integer :default 300 :min 1
                       :doc "one delegated call's ceiling, in seconds")
    ("startup_timeout_seconds" :integer :default 10 :min 1
                                :doc "the handshake's ceiling, in seconds"))
  (:tool "kimi_code_delegate" (nlk:contract 'kimi-delegate-arguments) 'delegate-tool)
  (:help :kimi-code "a read-only delegate to the locally signed-in Kimi Code CLI; /kimi-code doctor checks it" +primer+)
  (:command "kimi-code" #'run-doctor
            :description "Check the Kimi Code CLI, its login and the models it offers"
            :argument-hint "doctor"))
