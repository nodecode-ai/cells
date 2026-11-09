;;;; cell-test.lisp --- the lifecycle, the digest, the capture, the tools,
;;;; /debug-desk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the cell starts and stops cleanly and a bad
;;;; section refuses; that the digest counts today's commits in a real
;;;; throwaway repository, names its branch, files and subjects, and says so
;;;; when a folder is in no repository; that a shell answer that failed is
;;;; kept with its exit code, that a token in it is redacted away, that the
;;;; same failure twice is one failure counted, and that a call which
;;;; succeeded leaves the desk alone; that both tools answer through their
;;;; contracts; that the written summary asks once and is then kept; and that
;;;; /debug-desk today|last answers. No test needs a network: git runs on a
;;;; local repository, and the model the summary asks is a Lisp function.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "debug-desk"
  (:hooks :tool)
  (:help :debug-desk)
  (:command "debug-desk")
  (:running (is (= 120 (nodecode-debug-desk::setting :debounce-seconds))
                "a repeat of the same failure inside two minutes is one failure"))
  (:refused ("debounce_seconds" -1))
  (:idle nodecode-debug-desk:debug-desk-error
         (nodecode-debug-desk:today)
         (nodecode-debug-desk:last-failure)))

;;; --- the digest --------------------------------------------------------------------

(deftest debug-desk-cell-a-digest-counts-todays-commits ()
  (let ((repo (desk-repo :commits 2)))
    (with-desk
      (let ((text (nodecode-debug-desk:today (namestring repo))))
        (is (search "commits: 2" text) (format nil "both commits landed since midnight; the digest was: ~a" text))
        (is (search "branch main" text) "the branch is named")
        (is (search "feat 1" text) (format nil "the type counts are read off the subjects: ~a" text))
        (is (search "fix 1" text) (format nil "both types are counted: ~a" text))
        (is (search "README" text) "the touched file is listed")
        (is (search "fix: the desk reads git" text) "and the subjects are listed")))))

(deftest debug-desk-cell-a-digest-names-a-folder-in-no-repository ()
  (let ((folder (merge-pathnames (format nil "desk-norepo-~a-~a-~a/" (get-universal-time) (random 1000000) (gensym "N"))
                                  (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "keep" folder))
    (with-desk
      (let ((text (nodecode-debug-desk:today (namestring folder))))
        (is (search "is in no git repository" text) "and says what to do about it")))))

;;; --- what a failed shell call leaves behind -------------------------------------------

(deftest debug-desk-cell-a-failed-shell-call-is-kept-with-its-exit-code ()
  (with-desk
    (is (search "no failed shell command captured yet"
                (nodecode-debug-desk:last-failure))
        "nothing is captured before the agent fails")
    (desk-answer "make: *** [test] Error 2
[exit 2]"
                 :command "make test" :cwd "project")
    (let ((text (nodecode-debug-desk:last-failure)))
      (is (search "command: make test" text) "the command is named")
      (is (search "exit: 2" text) "and its status")
      (is (search "Error 2" text) "with the answer's own tail"))))

(deftest debug-desk-cell-a-token-in-a-failure-is-redacted ()
  (with-desk
    (desk-answer (format nil "fatal: bad credentials for ghp_~a~%[exit 128]"
                          "abcdefghij0123456789")
                 :command "git push")
    (let ((text (nodecode-debug-desk:last-failure)))
      (is (search "[REDACTED]" text) "the token is replaced")
      (is (not (search "ghp_abcdefghij0123456789" text)) "and never kept"))))

(deftest debug-desk-cell-the-same-failure-twice-is-one-counted ()
  (with-desk
    (desk-answer "boom
[exit 2]" :command "make test")
    (desk-answer "boom
[exit 2]" :command "make test")
    (is (search "seen 2 times" (nodecode-debug-desk:last-failure))
        "the repeat is counted, not a second failure")))

(deftest debug-desk-cell-a-call-that-succeeded-leaves-the-desk-alone ()
  (with-desk
    (desk-answer "ValueError: x is not a number
[exit 1]" :command "python t.py")
    (let ((kept (nodecode-debug-desk:last-failure)))
      (desk-answer "42" :command "python t.py")
      (is (equal kept (nodecode-debug-desk:last-failure)) "a success is not a failure"))))

(deftest debug-desk-cell-a-call-that-signals-is-a-failure-too ()
  (with-desk
    (is (handler-case
            (progn
              (funcall (cell-hook "nodecode-debug-desk" :tool)
                       (list :name "snippet"
                             :arguments (nlk:make-json-object "form" "(car nil)")
                             :call-id "call-2")
                       (lambda (op) (declare (ignore op)) (error "the form signalled")))
              nil)
          (error () t))
        "the call's own error still reaches the engine")
    (is (search "(car nil)" (nodecode-debug-desk:last-failure))
        "a body that signalled is kept as the last failure")))

;;; --- the tools, and the summary -------------------------------------------------------

(deftest debug-desk-cell-the-tools-answer-through-their-contracts ()
  (let ((repo (desk-repo :commits 1)))
    (with-desk
      (is (second (desk-tool-entry "debug_daily_report")) "the digest tool is registered")
      (is (second (desk-tool-entry "debug_last_failure")) "and so is the failure tool")
      (is (search "commits: 1" (desk-call "debug_daily_report" "cwd" (namestring repo)))
          "the digest tool answers for a folder")
      (desk-answer "npm ERR! test failed
[exit 1]" :command "npm test")
      (is (search "command: npm test" (desk-call "debug_last_failure"))
          "the failure tool answers with what was kept")
      (is (search "command: npm test" (desk-call "debug_last_failure" "summarize" nil))
          "and a false summarize still answers the failure"))))

(deftest debug-desk-cell-a-summary-asks-the-model-once ()
  (with-desk
    (desk-answer "Traceback (most recent call last):
ValueError: x
[exit 1]" :command "python t.py")
    (let ((asked 0))
      (let ((nodecode-debug-desk::*ask-model*
              (lambda (system user)
                (declare (ignore system user))
                (incf asked)
                "the test died on an undefined name")))
        (is (search "summary: the test died on an undefined name"
                    (nodecode-debug-desk:last-failure :summarize t))
            "the summary rides the answer")
        (nodecode-debug-desk:last-failure :summarize t)
        (is (= 1 asked) "and the kept summary answers the second read")))
    (desk-answer "TypeError: y is not callable
[exit 3]" :command "make build")
    (let ((nodecode-debug-desk::*ask-model*
            (lambda (system user)
              (declare (ignore system user))
              (error "no model here"))))
      (is (search "summary: none"
                  (nodecode-debug-desk:last-failure :summarize t))
          "a model that does not answer costs the failure nothing"))))

;;; --- /debug-desk -----------------------------------------------------------------------

(deftest debug-desk-cell-the-slash-command-answers-today-and-last ()
  (let ((repo (desk-repo :commits 1)))
    (with-temp-store ()
      (with-desk
      (nlk:create-session :id "desk-cell-slash" :cwd (namestring repo))
      (multiple-value-bind (text dialog)
          (cell-entry "nodecode-debug-desk" "debug-desk" "today" "desk-cell-slash")
        (declare (ignore dialog))
        (is (search "commits: 1" text) "/debug-desk today reads the session's folder"))
      (desk-answer "go: build failed
[exit 1]" :command "make build")
      (multiple-value-bind (text dialog)
          (cell-entry "nodecode-debug-desk" "debug-desk" "last" "desk-cell-slash")
        (declare (ignore dialog))
        (is (search "command: make build" text) "/debug-desk last answers the desk"))
      (multiple-value-bind (text dialog)
          (cell-entry "nodecode-debug-desk" "debug-desk" "" "desk-cell-slash")
        (declare (ignore dialog))
        (is (search "/debug-desk today" text) "and no verb is the usage"))))))
