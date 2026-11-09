;;;; cell-test.lisp --- the cell against a stubbed wslc: what it asks, what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every process goes through NODECODE-WSLC::*CLI*, so the whole runtime is
;;;; one stub here: nothing is spawned, no container is started, and the
;;;; assertions are on the argv the cell built and on the answer it made of
;;;; the output the stub gave.

(in-package #:nodecode.test)

(define-test-slice "wslc" "WSLC-CELL-" :start nodecode-wslc:start-cell)

;;; --- the runtime these tests answer as -------------------------------------------------

(defparameter +image+ "alpine:3.20")

(defvar *calls* nil
  "Every argv the stub answered this test, newest first.")

(defvar *container* nil
  "The container the stub says exists, or NIL.")

(defvar *running* t
  "Whether the stub's container runs.")

(defvar *help-flags* "--cpus --memory --env --volume --workdir --label --network --name"
  "What `wslc run --help' prints: the core flags alone, so every hardening flag is missing.")

(defun argv-text (argv)
  "ARGV as the one line a test searches."
  (format nil "~{~a~^ ~}" argv))

(defun stub-wslc (argv &key seconds)
  "The runtime as these tests answer for it: one container, named by the argv
that created it."
  (declare (ignore seconds))
  (push argv *calls*)
  (let ((line (argv-text argv)))
    (cond
      ((search "--version" line) (values "wslc 0.9.0" "" 0))
      ((search "run --help" line) (values *help-flags* "" 0))
      ((and (member "run" argv :test #'equal) (member "-d" argv :test #'equal))
       (setf *container* (second (member "--name" argv :test #'equal)) *running* t)
       (values "created" "" 0))
      ((search " start " line)
       (setf *running* t)
       (values "started" "" 0))
      ((search " exec " line)
       (values "hello from the sandbox" "" 0))
      ((search " inspect " line)
       (if *container*
           (values (format nil "[{\"Name\":\"/~a\",\"State\":{\"Running\":~a}}]"
                           *container* (if *running* "true" "false"))
                   "" 0)
           (values "" "no such container" 1)))
      ((search " ps " line)
       (values (if *container*
                   (format nil "{\"Names\":\"~a\",\"State\":\"running\",\"Image\":\"~a\",\"Labels\":\"nodecode-wslc=1\"}~%"
                           *container* +image+)
                   "")
               "" 0))
      (t (values "" "" 0)))))

(defmacro with-wslc (&body body)
  "BODY with the CLI stubbed and the stub's container forgotten."
  `(let ((*calls* nil) (*container* nil) (*running* t)
         (nodecode-wslc::*cli* #'stub-wslc))
     ,@body))

(defun started-pairs (&rest extra)
  "A healthy section's pairs, EXTRA pairs on top."
  (append (list "image" +image+ "binary" "/bin/sh" "sandbox" "test") extra))

(defmacro with-sandbox (&body body)
  "BODY with the cell started on a healthy section, and wslc stubbed."
  `(with-wslc
     (with-cell-stop ((apply #'wslc-start (started-pairs)))
       ,@body)))

(defun tool-call (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun call-for (fragment)
  "The newest argv holding FRAGMENT."
  (find-if (lambda (argv) (search fragment (argv-text argv))) *calls*))

(defun calls-for (fragment)
  "Every argv holding FRAGMENT."
  (remove-if-not (lambda (argv) (search fragment (argv-text argv))) *calls*))

;;; --- lifecycle, one assertion per thing NLE:DEFINE-CELL owns ----------------------------

(define-cell-lifecycle-tests "wslc"
  (:config "image" +image+ "binary" "/bin/sh" "sandbox" "test")
  (:help :wslc)
  (:command "wslc")
  (:running (is (equal +image+ (nodecode-wslc::setting :image)) "image is the section's")
            (is (equal "/root" (nodecode-wslc::setting :cwd)) "cwd defaults to /root")
            (is (ppcre:scan "^nodecode-wslc-test$" (nodecode-wslc:sandbox-name)) "a persistent sandbox's name is reproducible across sessions"))
  (:refused ("sandbox" "test")
            ("image" +image+ "timeout_seconds" 0))
  (:idle nodecode-wslc:wslc-error
         (nodecode-wslc:exec "true")))

;;; --- what the cell asks the runtime ------------------------------------------------------

(deftest wslc-cell-the-tool-runs-one-command-in-the-sandbox ()
  (with-sandbox
    (let ((answer (tool-call "wslc_exec" "command" "uname -a")))
      (is (search "hello from the sandbox" answer) "the command's output is the answer")
      (is (search "exit 0" answer) "and its exit status with it"))
    (let ((run (call-for " run " )))
      (is run "the sandbox is created on the first command")
      (is (member +image+ run :test #'equal) "from the section's image")
      (is (member "nodecode-wslc=1" run :test #'equal) "labelled as this cell's own")
      (is (member "sleep" run :test #'equal) "kept alive between commands"))
    (let ((exec (call-for " exec ")))
      (is exec "one command was run")
      (is (member "bash" exec :test #'equal) "through bash")
      (is (member "-lc" exec :test #'equal) "as a command string")
      (is (search "uname -a" (argv-text exec)) "and it is the one the model asked for"))))

(deftest wslc-cell-a-second-command-reuses-the-running-sandbox ()
  (with-sandbox
    (tool-call "wslc_exec" "command" "true")
    (tool-call "wslc_exec" "command" "true")
    (is (= 1 (length (calls-for " run "))) "one container for the run, not one per command")
    (is (= 2 (length (calls-for " exec "))) "and every command went to it")))

(deftest wslc-cell-a-stopped-sandbox-is-started-not-recreated ()
  (with-sandbox
    (tool-call "wslc_exec" "command" "true")
    (setf *running* nil)
    (tool-call "wslc_exec" "command" "true")
    (is (call-for " start ") "a stopped sandbox is started again")
    (is (= 1 (length (calls-for " run "))) "and not made a second time")))

(deftest wslc-cell-a-nonzero-exit-is-an-answer ()
  (with-wslc
    (with-cell-stop ((apply #'wslc-start (started-pairs)))
      (let ((nodecode-wslc::*cli*
             (lambda (argv &key seconds)
               (declare (ignore seconds))
               (if (search " exec " (argv-text argv))
                   (values "boom" "a warning" 7)
                   (stub-wslc argv)))))
        (let ((answer (tool-call "wslc_exec" "command" "false")))
          (is (search "exit 7" answer) "the exit status is reported")
          (is (search "boom" answer) "the command's output with it")
          (is (search "a warning" answer) "and what it said on stderr"))))))

(deftest wslc-cell-a-cwd-is-the-directory-the-command-runs-in ()
  (with-sandbox
    (tool-call "wslc_exec" "command" "ls" "cwd" "/work")
    (let ((exec (call-for " exec " )))
      (is (search "cd '/work' && ls" (argv-text exec)) "the command is given its own directory"))))

(deftest wslc-cell-a-non-persistent-sandbox-is-one-run-only ()
  (with-wslc
    (with-cell-stop ((apply #'wslc-start (started-pairs "persistent" nil)))
      (is (ppcre:scan "^nodecode-wslc-test-[0-9a-f]{8}$" (nodecode-wslc:sandbox-name)) "its name carries this run's own suffix")
      (tool-call "wslc_exec" "command" "true")
      (funcall stop)
      (setf stop nil)
      (is (call-for " rm -f ") "and the container is removed when this run ends"))))

;;; --- the other two tools, and the operator's command ----------------------------------------

(deftest wslc-cell-the-other-tools-answer-through-the-same-cli ()
  (with-sandbox
    (let ((rows (tool-call "wslc_containers")))
      (is (search "no sandbox of this cell" rows) "with no sandbox, the tool says so"))
    (tool-call "wslc_exec" "command" "true")
    (let ((rows (tool-call "wslc_containers")))
      (is (search "nodecode-wslc-test" rows) "the sandbox is listed by name")
      (is (search "running" rows) "with its state"))
    (is (search "is gone" (tool-call "wslc_remove" "name" "nodecode-wslc-test")) "a sandbox this cell started is removed")
    (let ((words (refusal-text nodecode-wslc:wslc-error
                    (tool-call "wslc_remove" "name" "someone-elses-container"))))
      (is (search "not a sandbox this cell started" words) "and one it did not start is not its to remove"))))

(deftest wslc-cell-the-command-reports-state-and-takes-a-removal ()
  (with-sandbox
    (let ((status (cell-entry "nodecode-wslc" "wslc" "")))
      (is (search "not created yet" status) "it says the sandbox is not there yet")
      (is (search "0.9.0" status) "and answers its version")
      (is (search +image+ status) "and the image the sandbox runs")
      (is (search "not hardened like Docker" status) "and what this wslc build cannot harden")
      (is (search "--cap-drop" status) "naming the flags that are not there"))
    (tool-call "wslc_exec" "command" "true")
    (let ((listed (cell-entry "nodecode-wslc" "wslc" "ps")))
      (is (search "nodecode-wslc-test" listed) "ps lists the runtime's containers"))
    (let ((removed (cell-entry "nodecode-wslc" "wslc" "rm nodecode-wslc-test")))
      (is (search "is gone" removed) "rm removes one it started"))
    (let ((unknown (cell-entry "nodecode-wslc" "wslc" "wobble")))
      (is (search "not a word it knows" unknown) "a word it does not know is said so"))))

;;; --- a machine without the CLI --------------------------------------------------------------

(deftest wslc-cell-a-missing-cli-is-a-refusal-the-model-can-act-on ()
  (with-wslc
    (with-cell-stop ((wslc-start "image" +image+ "sandbox" "test"
                                "binary" "/nonexistent/wslc"))
      (let ((words (refusal-text nodecode-wslc:wslc-error
                      (tool-call "wslc_exec" "command" "true"))))
        (is (search "no such file" words) "the refusal names what is wrong")
        (is (search "/nonexistent/wslc" words) "and the path the section named")))))

(deftest wslc-cell-a-cli-that-will-not-create-the-sandbox-says-why ()
  (with-wslc
    (with-cell-stop ((apply #'wslc-start (started-pairs)))
      (let ((nodecode-wslc::*cli*
             (lambda (argv &key seconds)
               (declare (ignore seconds))
               (if (and (member "run" argv :test #'equal) (member "-d" argv :test #'equal))
                   (values "" "image not found" 1)
                   (stub-wslc argv)))))
        (let ((words (refusal-text nodecode-wslc:wslc-error
                        (tool-call "wslc_exec" "command" "true"))))
          (is (search "could not start a sandbox" words))
          (is (search "image not found" words) "the runtime's own words reach the model"))))))
