;;;; cell.lisp --- the tool, the command and the manual: one microVM, one answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original plugin changed for Hermes -- where its terminal backend
;;;; ran -- is here a verb the model or the operator calls: one command in a
;;;; smol machine, one answer with its exit code and its output. The machines
;;;; are the engine's, so the CLI's own words (`smolvm machine run|create|exec|
;;;; ls|stop|delete') are the whole protocol and no SDK is bundled.

(in-package #:nodecode-smol)

;;; --- the tool -----------------------------------------------------------------------

(defun inventory (call)
  "The smol_machines answer: the machines this computer holds, CALL's name narrowing
it. => the JSON string a model reads."
  (handler-case (machines-answer (smol-inventory-name call))
    (smol-error (condition)
      (nlk:encode-json-object
       (nlk:make-json-object "ok" nil "error" (smol-error-detail condition))))))

;;; --- the same two questions from the operator -----------------------------------------

(defparameter +primer+
  "Commands in a microVM: smol_run runs one shell command in a smol machine, a Linux VM on
this computer with its own kernel and its own files, so a build, an install or a
destructive command never touches this computer's files.
  (smol:run \"uname -a\")                 one command in a machine removed when it exits
  (smol:run \"pip install pandas\" :image \"python:3.12-slim\")
  (smol:run \"ls /root\" :machine \"dev\")  a named machine, made on first use
  (smol:run \"make\" :machine \"dev\" :timeout-seconds 600)
  (smol:machines)                      the machines this computer holds
/smol CMD, /smol (the list), /smol stop NAME and /smol rm NAME do the same from the
composer. The answer is JSON: ok, exit_code, output, clipped, machine, image, network,
seconds, notes. The machine needs bash, so an image without it (alpine) takes
`apk add bash' first. The smol section names the CLI, the image and the ceiling; the
engine is smolvm, from smolmachines.com."
  "What (help :smol) answers while the cell runs.")

(defun machines-text ()
  "The machines this computer holds, as the CLI lists them in its own columns."
  (demand-binary)
  (let* ((result (run-argv (list-argv) (setting :timeout-seconds)))
         (output (string-trim '(#\Space #\Tab #\Newline #\Return) (or (getf result :output) ""))))
    (cond ((not (zerop (getf result :exit-code)))
           (format nil "smolvm machine ls exited ~d: ~a" (getf result :exit-code) (first-line output)))
          ((zerop (length output)) "no machines yet: /smol uname -a boots one, removed when its command exits")
          (t output))))

(defun run-line (command)
  "COMMAND in a machine of its own, as the lines /smol prints."
  (demand-binary)
  (let* ((result (run-argv (one-off-argv command (setting :image) (setting :network))
                            (setting :timeout-seconds)))
         (output (string-trim '(#\Space #\Tab #\Newline #\Return) (or (getf result :output) ""))))
    (if (getf result :timed-out-p)
        (format nil "killed after ~d second~:p~%~a" (setting :timeout-seconds) output)
        (format nil "exit ~d~%~a" (getf result :exit-code) output))))

(defun free-machine (argv name verb)
  "Free NAME by running ARGV, as the line /smol prints; VERB is what happened to it."
  (demand-binary)
  (let ((name (nlk:trimmed name)))
    (cond ((zerop (length name)) "which machine? /smol lists them")
          (t (let ((result (run-argv argv (setting :timeout-seconds))))
               (if (zerop (getf result :exit-code))
                   (format nil "~a ~a" verb name)
                   (format nil "~a: smolvm exited ~d: ~a" name (getf result :exit-code)
                           (first-line (getf result :output)))))))))

(defun slash-smol (args session-id)
  "One /smol invocation: the machines this computer holds, the command ARGS names run
in a machine of its own, or a machine stopped or deleted."
  (declare (ignore session-id))
  (let* ((argument (nlk:trimmed (or args "")))
         (words (uiop:split-string argument :separator '(#\Space #\Tab)))
         (head (string-downcase (or (first words) "")))
         (rest (nlk:trimmed (format nil "~{~a~^ ~}" (rest words)))))
    (handler-case
        (cond ((or (zerop (length argument)) (member head '("ls" "list" "machines") :test #'string=))
               (machines-text))
              ((member head '("rm" "delete" "del") :test #'string=)
               (free-machine (delete-argv rest) rest "deleted"))
              ((string= head "stop")
               (free-machine (stop-argv rest) rest "stopped"))
              (t (run-line argument)))
      (smol-error (condition) (smol-error-detail condition)))))

(defun run (command &rest pairs)
  "COMMAND in a smol machine -- (smol:run \"uname -a\"), or with the pairs
(smol:run \"make\" :machine \"dev\" :timeout-seconds 600) -- as the JSON string the
tool answers."
  (running-settings)
  (answer (nlk:contract-decode
           (nlk:contract 'smol-run)
           (apply #'nlk:make-json-object
                  (append (list "command" command)
                          (loop for (key value) on pairs by #'cddr
                                append (list (string-downcase (string key)) value)))))))

(defun machines (&optional name)
  "The smol machines on this computer, or the one NAME names, as the JSON string the
tool answers."
  (running-settings)
  (inventory (nlk:contract-decode (nlk:contract 'smol-inventory)
                                   (if name (nlk:make-json-object "name" name) (nlk:make-json-object)))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell smol
  (:section ("smol")
    (:guide "the smolvm CLI is what runs the machines: binary is where it is when it is not on PATH; image is what a one-off machine boots from; timeout_seconds bounds one command; next step: ask for /smol uname -a")
    ("binary" :string :default "smolvm"
              :doc "the engine's CLI: a name on PATH, or a path to it")
    ("image" :string :default "python:3.12-slim"
             :doc "the OCI image a one-off machine boots from; a named machine keeps the image it was made with")
    ("network" :boolean :default t
               :doc "whether a machine this cell boots gets outbound network; one call may turn it off")
    ("timeout_seconds" :integer :default 120 :min 1
                       :doc "one command's ceiling, in seconds; a call may ask for another")
    ("max_output" :integer :default 20000 :min 200
                  :doc "the characters of output one answer carries; the rest is clipped and the answer says so")
    ("extra_args" :list
                  :doc "arguments this cell adds to every machine run and machine create, for a flag it does not name"))
  (:tool "smol_run" (nlk:contract 'smol-run) #'answer)
  (:tool "smol_machines" (nlk:contract 'smol-inventory) #'inventory)
  (:help :smol "smol: a shell command in a local microVM - (smol:run \"uname -a\"); /smol CMD|ls|stop NAME|rm NAME" +primer+)
  (:command "smol" 'slash-smol
            :catches 'smol-error
            :description "Run a command in a smol machine: a microVM apart from your files"
            :argument-hint "CMD|ls|stop NAME|rm NAME")
  (:start (lambda ()
            (nle:notice (format nil "smol: commands run in a microVM through ~a" (setting :binary))
                        :key "nodecode-smol")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-smol"))))))
