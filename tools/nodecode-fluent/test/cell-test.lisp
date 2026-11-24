;;;; cell-test.lisp --- lifecycle; the help parser; the checker; /fluent; the hook; the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs a program or dials anything: *RUNNER* answers DEMO's own
;;;; help, a small cobra-shaped page, and the store is a folder of its own.

(in-package #:nodecode.test)

(define-test-slice "fluent" "FLUENT-CELL-" :start nodecode-fluent:start-cell)

(defparameter *fluent-demo-root*
  "Usage:
  demo [OPTIONS] COMMAND

Commands:
  run         Run a container
  list        List containers

Options:
  -h, --help      help for demo
      --debug     turn on debugging
")

(defparameter *fluent-demo-run*
  "Usage:
  demo run [OPTIONS] IMAGE

Options:
      --rm            remove the container when it exits
      --name string   name the container
  -d, --detach        run in the background
  -h, --help          help for run
")

(defun fluent-demo-runner (argv)
  "THE stub: DEMO's own help and version, and nothing else."
  (cond ((equal argv '("demo" "--help")) (values *fluent-demo-root* 0))
        ((equal argv '("demo" "run" "--help")) (values *fluent-demo-run* 0))
        ((equal argv '("demo" "list" "--help"))
         (values "Usage:
  demo list [OPTIONS]

Options:
  -h, --help   help for list
" 0))
        ((equal argv '("demo" "--version")) (values "demo version 1.2.3
" 0))
        (t (values "" 127))))

(defmacro with-fluent-scratch (&body body)
  "BODY with a store of its own, the stub as the program runner, the cell started."
  `(let ((nodecode-fluent::*store-dir*
           (merge-pathnames (format nil "fluent-test-~a/" (gensym)) (uiop:temporary-directory)))
         (nodecode-fluent::*runner* #'fluent-demo-runner))
     (with-cell-stop ((fluent-start))
       ,@body)))

(defmacro with-fluent ((&rest pairs) &body body)
  "BODY with the cell started under PAIRS as its `fluent' section."
  `(let ((nodecode-fluent::*store-dir*
           (merge-pathnames (format nil "fluent-test-~a/" (gensym)) (uiop:temporary-directory)))
         (nodecode-fluent::*runner* #'fluent-demo-runner))
     (with-cell-stop ((fluent-start ,@pairs))
       ,@body)))

(define-cell-lifecycle-tests "fluent"
  (:fixture with-fluent-scratch)
  (:config "mode" "block")
  (:hooks :tool)
  (:help :fluent)
  (:command "fluent")
  (:running (is (hash-table-p nodecode-fluent::*cards*) "the card table is read at start"))
  (:stopped (is (zerop (hash-table-count nodecode-fluent::*cards*)) "and emptied"))
  (:refused ("max_subcommands" 0)))

;;; --- the parser ---------------------------------------------------------------

(deftest fluent-cell-parses-a-help-page ()
  (let ((parsed (nodecode-fluent:parse-help *fluent-demo-root*)))
    (is (getf parsed :certain) "a Usage line and options make the page one to judge by")
    (is (equal '("run" "list") (mapcar (lambda (s) (getf s :name)) (getf parsed :subcommands))))
    (is (nodecode-fluent::flag-named (getf parsed :flags) "--debug") "a long option is read")
    (is (nodecode-fluent::flag-named (getf parsed :flags) "-h") "and its short spelling beside it"))
  (let ((parsed (nodecode-fluent:parse-help "just some prose, with --nothing in it")))
    (is (not (getf parsed :certain)) "a page with no usage line is never judged by")))

;;; --- the checker --------------------------------------------------------------

(deftest fluent-cell-catches-an-invented-option (with-fluent ()
  (multiple-value-bind (card problem) (nodecode-fluent:adopt "demo")
    (is (null problem) "demo's help is read")
    (is (search "1.2.3" (getf card :version)) "its own version line is kept")
    (is (= 2 (length (getf card :subcommands))) "and its two subcommands"))
  (let ((findings (nodecode-fluent:check-command "demo run --rmm alpine")))
    (is (= 1 (length findings)) "one invented option")
    (is (equal "--rmm" (getf (first findings) :token)))
    (is (equal "--rm" (getf (first findings) :suggestion)) "with the option it meant")
    (is (equal "demo run" (getf (first findings) :command))))
  (is (null (nodecode-fluent:check-command "demo run --rm alpine")) "a real option passes")
  (is (null (nodecode-fluent:check-command "demo run -- --rmm alpine")) "after -- nothing is judged")
  (is (null (nodecode-fluent:check-command "demo run --det alpine")) "a unique prefix passes")
  (is (null (nodecode-fluent:check-command "demo frobnicate --whatever")) "an unread subcommand is left alone")
  (is (null (nodecode-fluent:check-command "other-thing --whatever")) "and so is an unread program")))

;;; --- /fluent ------------------------------------------------------------------

(deftest fluent-cell-adopts-shows-and-forgets (with-fluent ()
  (is (search "fluent is now fluent in demo" (nodecode-fluent:run-slash "adopt demo" nil)))
  (is (probe-file (merge-pathnames "demo.card" nodecode-fluent::*store-dir*)) "the card is on disk")
  (is (search "demo" (nodecode-fluent:run-slash "status" nil)))
  (is (search "--rm" (nodecode-fluent:run-slash "show demo run" nil)) "/fluent show prints the options")
  (is (search "demo run" (nodecode-fluent:run-slash "check demo run --rmm alpine" nil)) "and /fluent check says what would refuse")
  (is (search "has not read" (nodecode-fluent:run-slash "check other --rmm" nil)) "an unread program is named")
  (is (search "forgotten demo" (nodecode-fluent:run-slash "forget demo" nil)))
  (is (null (gethash "demo" nodecode-fluent::*cards*)) "and the checker forgets it too")
  (is (not (probe-file (merge-pathnames "demo.card" nodecode-fluent::*store-dir*))) "file and all")))

;;; --- the pre-call check -------------------------------------------------------

(defun fluent-verdict (form &rest strings)
  "The installed :tool hook's answer for FORM with the strings object STRINGS (name text ...):
:ALLOWED, or the refusal string."
  (funcall (cell-hook "nodecode-fluent" :tool)
           (list :name "eval"
                 :arguments (if strings
                                (nlk:json-object
                                 "form" form
                                 "strings" (let ((object (nlk:json-object)))
                                             (loop for (name text) on strings by #'cddr
                                                   do (setf (gethash name object) text))
                                             object))
                                (nlk:json-object "form" form))
                 :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(deftest fluent-cell-refuses-an-invented-option-before-it-runs (with-fluent ()
  (nodecode-fluent:adopt "demo")
  (let ((answer (fluent-verdict "(sh \"demo run --rmm alpine\")")))
    (is (stringp answer) "the call does not run")
    (is (search "refused by nodecode-fluent" answer))
    (is (search "did you mean --rm" answer) "the refusal names the option meant"))
  (is (eq :allowed (fluent-verdict "(sh \"demo run --rm alpine\")")) "a real option runs")
  (is (eq :allowed (fluent-verdict "(+ 1 2)")) "a call that spawns nothing runs")
  (is (stringp (fluent-verdict "(uiop:run-program (list \"demo\" \"run\" \"--rmm\" \"alpine\"))")) "argv spelled as a list is read too")
  (is (stringp (fluent-verdict "(sh ($ \"cmd\"))" "cmd" "demo run --bogus")) "and a command the strings object carries")))

(deftest fluent-cell-warn-mode-lets-a-flagged-command-run (with-fluent ("mode" "warn")
  (nodecode-fluent:adopt "demo")
  (is (eq :allowed (fluent-verdict "(sh \"demo run --rmm alpine\")")) "in warn mode the call runs and the note is on the board")))

;;; --- the tool -----------------------------------------------------------------

(defun fluent-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(deftest fluent-cell-the-tool-checks-one-command (with-fluent ()
  (let ((text (fluent-call-tool "fluent_check"
                                 "{\"command\":\"demo run --rmm alpine\",\"adopt\":true}")))
    (is (search "--rmm" text) "the invented option is named")
    (is (search "did you mean --rm" text) "with the option it meant"))
  (is (search "every long option" (fluent-call-tool "fluent_check"
                                                     "{\"command\":\"demo run --rm alpine\"}")) "and a clean command says so")))
