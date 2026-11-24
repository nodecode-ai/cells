;;;; cell-test.lisp --- the ledger, the observers, the reports, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network or a real shell: the two functions the
;;;; cell advises are driven through their advice with a stubbed NEXT, the
;;;; ledger is a file in the scratch home, and the clock is the one thing the
;;;; tests move (a stubbed NOW the rows carry).

(in-package #:nodecode.test)

(define-test-slice "command-ledger" "COMMAND-LEDGER-CELL-"
  :start nodecode-command-ledger:start-cell)

(define-cell-lifecycle-tests "command-ledger"
  (:help :command-ledger)
  (:command "command-log")
  (:hooks 'nodecode.evolved::slash 'nodecode.evolved::execute-slash-command)
  (:running (is (= 5000 (nodecode-command-ledger::setting :rows)) "rows defaults to five thousand"))
  (:refused ("rows" 0) ("rows" 4) ("rows" "many"))
  (:idle nodecode-command-ledger:command-ledger-error
         (cmdlog:summary)
         (cmdlog:recent)
         (cmdlog:clear)))

;;; --- the fixture ---------------------------------------------------------------------

(defun cl-ledger-file ()
  "A ledger file of this test's own, in the scratch home, empty."
  (let ((path (nlk:home "command-ledger-test/run.jsonl")))
    (ensure-directories-exist path)
    (when (probe-file path) (delete-file path))
    path))

(defmacro with-ledger ((&key (now 1700000000)) &body body)
  "BODY with the cell started, its ledger a fresh file, and the clock at NOW,
a unix second, for every row written."
  (let ((path (gensym "PATH")))
    `(let ((,path (cl-ledger-file)))
       (with-stubbed-fdefinitions
           ((nodecode-command-ledger::ledger-path () ,path)
            (nodecode-command-ledger::now () ,now))
         (with-cell-stop ((command-ledger-start))
           (unwind-protect (progn ,@body)
             (when (probe-file ,path) (delete-file ,path))))))))

(defun cl-answer (&optional (answer "ran"))
  "A stubbed NEXT for a function hook: it records nothing and answers ANSWER."
  (lambda (&rest arguments) (declare (ignore arguments)) answer))

(defun cl-run (line &key (session "session-one"))
  "LINE as one call of SLASH would run it through the cell's advice."
  (apply #'nodecode-command-ledger::observe-slash (cl-answer) line (list :session-id session)))

(defun cl-run-typed (name)
  "NAME as one command typed in the terminal, through the cell's advice."
  (funcall #'nodecode-command-ledger::observe-typed (cl-answer) nil name ""))

(defun cl-rows ()
  "The ledger's rows, oldest first."
  (nodecode-command-ledger::rows))

(defun cl-handler ()
  "/command-log's RUN, as the catalog's own headless arm answers it."
  (nodecode.evolved::slash-command-headless (nle::find-registered-command "command-log")))

;;; --- one row a command --------------------------------------------------------------

(deftest command-ledger-cell-every-line-becomes-one-row ()
  (with-ledger ()
    (is (equal "ran" (cl-run "/model grok-4")) "the advice answers what the command answered")
    (cl-run "/usage")
    (let ((rows (cl-rows)))
      (is (= 2 (length rows)) "two commands, two rows")
      (let ((commands (mapcar #'nodecode-command-ledger::row-command rows)))
        (is (equal '("model" "usage") commands) "the name, without its slash or arguments"))
      (let ((surfaces (mapcar #'nodecode-command-ledger::row-surface rows)))
        (is (equal '("gateway" "gateway") surfaces) "a line no shell typed is the gateway surface")))))

(deftest command-ledger-cell-a-session-is-kept-as-a-hash ()
  (with-ledger ()
    (cl-run "/model grok-4" :session "session-one")
    (let* ((row (first (cl-rows)))
           (tag (nodecode-command-ledger::row-session row)))
      (is (= 12 (length tag)) "twelve hex digits stand in for the session")
      (is (every (lambda (char) (digit-char-p char 16)) tag) "and they are hex")
      (is (not (search "session-one" tag)) "the session id itself is never written")
      (is (not (search "grok-4" (nlk:read-text (nodecode-command-ledger::ledger-path)))) "nor are the arguments typed after the command"))))

(deftest command-ledger-cell-typing-here-is-the-cli-surface ()
  (with-ledger ()
    (let ((nlk:*scribe-session-id* "session-two"))
      (is (equal "ran" (cl-run-typed "model")) "the advice answers what the command answered"))
    (let ((row (first (cl-rows))))
      (is (equal "model" (nodecode-command-ledger::row-command row)))
      (is (string= "cli" (nodecode-command-ledger::row-surface row)) "a command typed in the terminal is the cli surface")
      (let ((running (nlk:short-digest "session-two" 12))
            (kept (nodecode-command-ledger::row-session row)))
        (is (string= running kept) "the running session's tag, not its id")))))

(deftest command-ledger-cell-its-own-command-is-not-counted ()
  (with-ledger ()
    (cl-run "/command-log recent")
    (cl-run "/command-log")
    (cl-run-typed "command-log")
    (is (null (cl-rows)) "reading the ledger is not using it")))

;;; --- the reports --------------------------------------------------------------------

(deftest command-ledger-cell-summary-counts-uses-and-sessions ()
  (with-ledger ()
    (cl-run "/model")
    (cl-run "/model" :session "session-two")
    (cl-run "/usage")
    (is-carrying (text (cmdlog:summary))
      ("Slash commands, last 24 hours:") ("/model" "a row for the command run twice")
      ("uses") ("share") ("sessions") ("total") ("100%")
      (:absent "/compress" "a command never run is not listed"))
    (let ((text (cmdlog:summary :by "surface")))
      (is (search "surface" text) "a surface grouping names that column")
      (is (search "gateway" text) "and the surface the rows were run on"))
    (is-carrying (text (cmdlog:summary :window "session"))
      ("/model" "the latest session's own rows") ("total" "one row each, so a total line"))))

(deftest command-ledger-cell-the-command-line-choices ()
  (with-ledger ()
    (cl-run "/model")
    (cl-run "/usage")
    (let ((handler (cl-handler)))
      (is-carrying (text (funcall handler "7d" "s1"))
        ("Slash commands, last 7 days:") ("/model" "the command ran") ("/usage"))
      (is-carrying (text (funcall handler "surfaces" "s1")) ("surface") ("gateway"))
      (is-carrying (text (funcall handler "days" "s1")) ("day") ("uses"))
      (is-carrying (text (funcall handler "recent" "s1"))
        ("newest 2 of 2") ("/usage" "the newest first") ("surface" "a surface column"))
      (is-carrying (text (funcall handler "recent 1" "s1")) ("newest 1 of 2") ("when"))
      (is-carrying (text (funcall handler "nonsense" "s1")) ("Unknown option: nonsense"))
      (is-carrying (text (funcall handler "all 12h" "s1")) ("all recorded"))
      (is-carrying (text (funcall handler "clear" "s1")) ("deleted 2 recorded command(s)"))
      (is (null (cl-rows)) "and the ledger is gone"))))

(deftest command-ledger-cell-a-window-leaves-older-rows-behind ()
  (with-ledger ()
    (cl-run "/model")
    (nlk:append-json-line (nodecode-command-ledger::ledger-path)
                           (nlk:json-object "ts" (- 1700000000 (* 3 86400))
                                            "command" "compress"
                                            "session" (nlk:short-digest "old" 12)
                                            "surface" "gateway"))
    (is (not (search "/compress" (cmdlog:summary))) "three days ago is outside a day")
    (is (search "/compress" (cmdlog:summary :window "all")) "and inside everything")
    (is (search "last 4 days" (funcall (cl-handler) "4d" "s1")) "a window word titles the report")
    (is (search "last 24 hours" (funcall (cl-handler) "" "s1")))))

;;; --- the ledger keeps its shape ---------------------------------------------------

(deftest command-ledger-cell-a-full-ledger-keeps-the-newest-rows ()
  (with-ledger ()
    (let ((path (nodecode-command-ledger::ledger-path)))
      (dotimes (n 8)
        (nlk:append-json-line path (nlk:json-object "ts" (+ 1700000000 n)
                                                     "command" (format nil "c~d" n)
                                                     "session" ""
                                                     "surface" "cli")))
      (nodecode-command-ledger::trim path 5)
      (let ((commands (mapcar #'nodecode-command-ledger::row-command (cl-rows))))
        (is (= 5 (length commands)) "the file holds the bound")
        (is (equal '("c3" "c4" "c5" "c6" "c7") commands) "and the newest of them")))))

(deftest command-ledger-cell-a-broken-write-never-breaks-the-command ()
  (with-ledger ()
    (with-stubbed-fdefinitions
        ((nodecode-command-ledger::append-row (&rest arguments)
           (error "the disk is full after ~d argument(s)" (length arguments))))
      (is (equal "ran" (cl-run "/model")) "the command answers anyway")
      (is (equal "ran" (cl-run-typed "model")) "on either surface"))))
