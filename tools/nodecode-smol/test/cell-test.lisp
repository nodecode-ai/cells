;;;; cell-test.lisp --- the lifecycle; the argv the cell builds; the fields of
;;;; its answer; the refusals; /smol; the eval function.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own work: that a command reaches an
;;;; engine as `machine run -- bash -c', that a named machine is made once and
;;;; then executed in, that a session's kept machine keeps its name, that the
;;;; exit code, the output, a ceiling and a clipped answer all reach the caller
;;;; as the fields the original plugin's users read, and that a missing CLI, a
;;;; config outside its bounds and a call outside the contract each refuse.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "smol"
  (:help :smol)
  (:command "smol")
  (:running (is (equal "smolvm" (nodecode-smol::setting :binary)) "the CLI is smolvm, on PATH")
            (is (equal "python:3.12-slim" (nodecode-smol::setting :image)) "a one-off machine boots the image the plugin named")
            (is (= 120 (nodecode-smol::setting :timeout-seconds)) "a command gets two minutes")
            (is (= 20000 (nodecode-smol::setting :max-output)) "an answer carries twenty thousand characters")
            (is (eq t (nodecode-smol::setting :network)) "a machine gets outbound network"))
  (:refused ("timeout_seconds" 0) ("max_output" 10))
  (:idle smol:smol-error (smol:run "uname -a")))

;;; --- one command in one machine ----------------------------------------------------

(deftest smol-cell-a-one-off-machine-answers-output-and-code ()
  (with-smol (:calls calls
              :answer (lambda (argv)
                        (declare (ignore argv))
                        (smol-exit "Linux node 6.1.0 #1 SMP x86_64 GNU/Linux")))
    (let ((answer (smol-tool "smol_run" "{\"command\": \"uname -a\"}")))
      (is (eq t (nlk:json-value answer :boolean "ok")) "ok is true when the command exited zero")
      (is (= 0 (nlk:json-value answer :integer "exit_code")) "the exit code is the command's")
      (is (search "Linux node 6.1.0" (nlk:json-value answer :string "output")) "the output is the command's")
      (is (equal '("smolvm" "machine" "run" "--net" "--image" "python:3.12-slim" "--" "bash" "-c" "uname -a")
                 (first calls)) "one-off asks for machine run, the section's image, and the command's bash -c")
      (is (= 1 (length calls)) "a one-off is one call to the engine")
      (is (null (nlk:json-value answer :string "machine")) "and names no machine, so nothing is left behind"))))

(deftest smol-cell-a-failing-command-still-answers ()
  (with-smol (:answer (lambda (argv)
                        (declare (ignore argv))
                        (smol-exit "ls: cannot access 'nope'" :code 2)))
    (let ((answer (smol-tool "smol_run" "{\"command\": \"ls nope\"}")))
      (is (null (nlk:json-value answer :boolean "ok")) "ok is false")
      (is (= 2 (nlk:json-value answer :integer "exit_code")) "the code is the command's own")
      (is (search "cannot access" (nlk:json-value answer :string "output")) "and its complaint is the output")
      (is (search "the command exited 2" (first (answer-notes answer))) "a note says the command failed"))))

(deftest smol-cell-a-named-machine-is-made-once-and-then-executed-in ()
  (with-smol (:calls calls
              :answer (smol-by-subcommand (list (cons "create" (smol-exit "machine dev already exists" :code 1))
                                                (cons "exec" (smol-exit "/root/a.txt")))))
    (let ((answer (smol-tool "smol_run" "{\"command\": \"ls /root\", \"machine\": \"dev\"}")))
      (is (equal '("create" "exec") (mapcar (lambda (argv) (third argv)) (reverse calls))) "the machine is made, then the command runs in it")
      (is (search "--name dev" (format nil "~{~a~^ ~}" (second calls))) "the machine is named in the create")
      (is (equal "dev" (nlk:json-value answer :string "machine")) "the answer names the machine")
      (is (search "already exists" (first (answer-notes answer))) "a create that found the machine is a note, not a failure")
      (is (equal "/root/a.txt" (nlk:json-value answer :string "output")) "and the command's output is the answer"))))

(deftest smol-cell-keep-uses-one-machine-per-session ()
  (with-smol (:calls calls
              :answer (smol-by-subcommand (list (cons "exec" (smol-exit "done")))))
    (let ((first-run (smol-tool "smol_run" "{\"command\": \"pwd\", \"keep\": true}"))
          (second-run (smol-tool "smol_run" "{\"command\": \"pwd\", \"keep\": true}")))
      (let ((name (nlk:json-value first-run :string "machine")))
        (is (search "nodecode-smol-" name) "a kept machine is named for this cell")
        (is (equal name (nlk:json-value second-run :string "machine")) "and the next call finds the same one"))
      (is (equal '("create" "exec" "create" "exec") (mapcar (lambda (argv) (third argv)) (reverse calls))) "each kept call makes sure of the machine before it runs"))))

(deftest smol-cell-a-command-past-its-ceiling-is-killed-and-says-so ()
  (with-smol (:answer (lambda (argv)
                        (declare (ignore argv))
                        (smol-exit "partial" :code 124 :timed-out-p t)))
    (let ((answer (smol-tool "smol_run" "{\"command\": \"sleep 600\", \"timeout_seconds\": 5}")))
      (is (= 124 (nlk:json-value answer :integer "exit_code")) "the code timeout(1) uses says it was killed")
      (is (null (nlk:json-value answer :boolean "ok")) "and the command did not succeed")
      (is (search "5 seconds" (first (answer-notes answer))) "the note names the ceiling the call asked for"))))

(deftest smol-cell-long-output-is-clipped-and-says-so ()
  (with-smol (:answer (lambda (argv)
                        (declare (ignore argv))
                        (smol-exit (make-string 400 :initial-element #\x))))
    (let ((answer (smol-tool "smol_run" "{\"command\": \"yes\"}" "max_output" 200)))
      (is (= 200 (length (nlk:json-value answer :string "output"))) "the answer carries max_output characters")
      (is (eq t (nlk:json-value answer :boolean "clipped")) "and says it clipped")
      (is (search "raise max_output" (first (answer-notes answer))) "and how to get the rest"))))

;;; --- the machines on this computer ---------------------------------------------------

(deftest smol-cell-the-machine-list-is-the-clis-own-columns ()
  (with-smol (:answer (lambda (argv)
                        (declare (ignore argv))
                        (smol-exit (format nil "NAME   IMAGE             STATE~%dev    python:3.12-slim  running~%build  alpine            stopped~%"))))
    (let ((answer (smol-tool "smol_machines" "{}")))
      (is (equal '("dev" "build") (answer-machines answer)) "every machine is a name, the header left out")
      (is (search "STATE" (nlk:json-value answer :string "raw")) "the CLI's own text rides along"))
    (let ((one (smol-tool "smol_machines" "{\"name\": \"build\"}")))
      (is (equal '("build") (answer-machines one)) "a name narrows the list"))
    (let ((none (smol-tool "smol_machines" "{\"name\": \"nope\"}")))
      (is (null (answer-machines none)) "a name nothing holds shows nothing")
      (is (search "no machine named nope" (first (answer-notes none))) "and the note says so"))))

;;; --- the refusals -----------------------------------------------------------------

(deftest smol-cell-a-missing-cli-refuses-and-names-its-setting ()
  (with-smol (:which nil)
    (let ((answer (smol-tool "smol_run" "{\"command\": \"uname -a\"}")))
      (is (null (nlk:json-value answer :boolean "ok")) "ok is false")
      (is (search "binary" (nlk:json-value answer :string "error")) "the refusal names the setting that fixes it")
      (is (search "smolmachines.com" (nlk:json-value answer :string "error")) "and where the CLI comes from"))
    (let ((listed (smol-tool "smol_machines" "{}")))
      (is (search "binary" (nlk:json-value listed :string "error")) "the list refuses the same way"))))

(deftest smol-cell-a-call-outside-the-contract-refuses ()
  (is (handler-case (progn (nlk:contract-decode (nlk:contract 'nodecode-smol::smol-run)
                                                (nlk:make-json-object "command" "uname -a"
                                                                       "timeout_seconds" "60"))
                           nil)
        (nlk:contract-refusal () t)) "a timeout as a string is refused, never coerced"))

;;; --- /smol, and the eval function ----------------------------------------------------

(deftest smol-cell-the-command-runs-lists-and-frees ()
  (with-smol (:answer (smol-by-subcommand (list (cons "run" (smol-exit "Linux"))
                                                (cons "ls" (smol-exit "NAME~%dev"))
                                                (cons "delete" (smol-exit "")))))
    (with-cell-stop ((smol-start))
      (let ((ran (cell-entry "nodecode-smol" "smol" "uname -a")))
        (is (search "exit 0" ran) "a command prints its exit code")
        (is (search "Linux" ran) "and its output"))
      (is (search "dev" (cell-entry "nodecode-smol" "smol" "")) "bare /smol lists the machines")
      (is (equal "deleted dev" (cell-entry "nodecode-smol" "smol" "rm dev")) "rm frees a machine")
      (is (search "which machine?" (cell-entry "nodecode-smol" "smol" "rm")) "rm with no name says what it needs"))))

(deftest smol-cell-the-eval-function-answers-the-tools-string ()
  (with-smol (:answer (lambda (argv)
                        (declare (ignore argv))
                        (smol-exit "hello")))
    (with-cell-stop ((smol-start))
      (let ((answer (nlk:decode-json (smol:run "echo hello"))))
        (is (eq t (nlk:json-value answer :boolean "ok")) "the eval call answers ok")
        (is (equal "hello" (nlk:json-value answer :string "output")) "with the command's output"))
      (is (eq t (nlk:json-value (nlk:decode-json (smol:machines)) :boolean "ok")) "and (smol:machines) answers the list"))))
