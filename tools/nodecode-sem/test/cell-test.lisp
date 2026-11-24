;;;; cell-test.lisp --- the sem cell against its own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Sem and Weave call is stubbed: RUN-CLI answers a scripted text, NLK:RUN-BOUNDED
;;;; answers one call's environment, and nothing spawns a process or reads a repository.

(in-package #:nodecode.test)

(define-test-slice "sem" "SEM-CELL-" :start nodecode-sem:start-cell)

(defvar *calls* '()
  "Every (ARGV DIRECTORY SECONDS) a stubbed RUN-CLI was handed this test, newest first.")

(defmacro with-cli ((answer) &body body)
  "BODY with RUN-CLI stubbed: ANSWER, a form with ARGV bound, answers
 (values OUT ERR STATUS) and every call is recorded in *CALLS*."
  `(let ((*calls* '()))
     (with-stubbed-fdefinition (nodecode-sem::run-cli (argv &key directory seconds)
                                  (push (list argv directory seconds) *calls*)
                                  ,answer)
       ,@body)))

(defmacro with-sem-cli (&body body)
  "BODY with the two version probes and every other call answered: the
 fixture that lets a cell start without a CLI on the machine."
  `(with-cli ((if (member "--version" argv :test #'equal)
                  (values (format nil "~a 1.0" (first argv)) "" 0)
                  (values "{\"entities\":[]}" "" 0)))
     ,@body))

(defun find-op (&rest pairs)
  "A SEM-FIND-QUERY from PAIRS."
  (nodecode-sem:decode-sem-find-query (apply #'nlk:make-json-object pairs)))

(defun diff-op (&rest pairs)
  "A SEM-DIFF-QUERY from PAIRS."
  (nodecode-sem:decode-sem-diff-query (apply #'nlk:make-json-object pairs)))

;;; --- the cell's own life ---------------------------------------------------

(define-cell-lifecycle-tests "sem"
  (:fixture with-sem-cli)
  (:help :sem)
  (:command "sem")
  (:running
   (is (equal "sem" (nodecode-sem::setting :sem-path)) "the default program")
   (is (equal 120 (nodecode-sem::setting :timeout)) "the default bound")
   (is (nodecode-sem::setting :offline) "offline by default"))
  (:refused ("timeout" 1) ("max_chars" 10))
  (:idle nodecode-sem:sem-error (nodecode-sem:sem-find (find-op "names" (vector "parse_config")))))

;;; --- what each tool answers ------------------------------------------------

(deftest sem-cell-find-answers-what-the-cli-printed ()
  (with-cli ((values "{\"definition\":\"parse_config\"}" "" 0))
    (with-cell-stop ((sem-start))
      (let ((answer (nodecode-sem:sem-find (find-op "names" (vector "parse_config" "load_config")))))
        (is (search "parse_config" answer) "the CLI's own answer, passed through")
        (is (equal '("sem" "find" "parse_config" "load_config" "--json") (first (first *calls*))) "one argv, no shell, --json asked for by the cell")))))

(deftest sem-cell-find-passes-callers-context-and-narrowing ()
  (with-cli ((values "{}" "" 0))
    (with-cell-stop ((sem-start))
      (nodecode-sem:sem-find (find-op "names" (vector "parse_config")
                                      "callers" t "context" t "budget" 900 "in" "src/"))
      (is (equal '("sem" "find" "parse_config" "--callers" "--context" "--budget" "900"
                   "--in" "src/" "--json")
                 (first (first *calls*))) "the flags the call asked for, in one argv"))))

(deftest sem-cell-diff-is-entities-not-lines ()
  (with-cli ((values "{\"changed\":[\"parse_config\"]}" "" 0))
    (with-cell-stop ((sem-start))
      (is (search "parse_config" (nodecode-sem:sem-diff (diff-op))) "no range is the working tree")
      (is (equal '("sem" "diff" "--json") (first (first *calls*))) "a bare diff")
      (nodecode-sem:sem-diff (diff-op "range" "main..HEAD"))
      (is (equal '("sem" "diff" "main..HEAD" "--json") (first (first *calls*))) "a named range is one argument"))))

(deftest sem-cell-impact-asks-for-tests-and-a-file ()
  (with-cli ((values "{\"callers\":[]}" "" 0))
    (with-cell-stop ((sem-start))
      (nodecode-sem:sem-impact
       (nodecode-sem:decode-sem-impact-query
        (nlk:make-json-object "name" "parse_config" "tests" t "file" "src/config.py")))
      (is (equal '("sem" "impact" "parse_config" "--tests" "--file" "src/config.py" "--json")
                 (first (first *calls*)))))))

(deftest sem-cell-weave-preview-never-merges ()
  (with-cli ((values "config.py: independent entity changes" "" 0))
    (with-cell-stop ((sem-start))
      (let ((answer (nodecode-sem:weave-preview
                     (nodecode-sem:decode-weave-preview-query
                      (nlk:make-json-object "branch" "feature-a" "file" "config.py")))))
        (is (search "config.py" answer) "what the preview showed")
        (is (equal '("weave" "preview" "feature-a" "--file" "config.py") (first (first *calls*))) "preview only: no setup, no land, no merge")))))

(deftest sem-cell-a-missing-argument-refuses ()
  (is (signals-error nlk:contract-refusal (nodecode-sem:decode-sem-find-query (nlk:make-json-object))) "sem_find without a name")
  (is (signals-error nlk:contract-refusal (nodecode-sem:decode-weave-preview-query (nlk:make-json-object))) "weave_preview without a branch"))

(deftest sem-cell-an-absent-cli-is-the-prerequisite ()
  (with-cli ((error "run-program: sem: No such file or directory"))
    (with-cell-stop ((sem-start))
      (let ((answer (nodecode-sem:sem-find (find-op "names" (vector "parse_config")))))
        (is (search "could not be started" answer) "the tool says so instead of signalling")
        (is (search "github.com/Ataraxy-Labs/sem" answer) "and names where Sem comes from")))))

(deftest sem-cell-a-failing-cli-keeps-its-status ()
  (with-cli ((values "" "not a git repository" 128))
    (with-cell-stop ((sem-start))
      (let ((answer (nodecode-sem:sem-diff (diff-op))))
        (is (search "exited 128" answer) "the status is the verdict")
        (is (search "not a git repository" answer) "and stderr is not swallowed")))))

(deftest sem-cell-a-loud-answer-is-cut ()
  (with-cli ((values (make-string 4000 :initial-element #\x) "" 0))
    (with-cell-stop ((sem-start "max_chars" 1000))
      (let ((answer (nodecode-sem:sem-diff (diff-op))))
        (is (search "[cut at 1000 of 4000 characters" answer) "the cut says where it was cut")))))

;;; --- the prefix, the directory and the bound -------------------------------

(deftest sem-cell-every-call-is-offline-and-bounded ()
  ;; The plugin's own habit -- SEM_NO_NETWORK=1 SEM_NO_TELEMETRY=1 on every
  ;; command -- is laid on by the cell instead of typed by the model.
  (let ((seen nil))
    (with-stubbed-fdefinition (nlk:run-bounded (argv &key directory environment seconds
                                                      input output error-output)
                                (setf seen (list argv directory environment seconds))
                                (values "  out  " "" 0))
      (with-cell-stop ((sem-start "directory" "/tmp/a-repo"))
        (let ((answer (nodecode-sem:call "sem" '("sem" "--version"))))
          (is (equal "out" answer) "stdout, trimmed")
          (is (equal '("SEM_NO_NETWORK=1" "SEM_NO_TELEMETRY=1") (third seen)) "the caution the skill typed by hand, on every call")
          (is (equal "/tmp/a-repo" (second seen)) "in the repository the section names")
          (is (eql 120 (fourth seen)) "bounded by the timeout setting"))))))

;;; --- /sem ------------------------------------------------------------------

(deftest sem-cell-command-probes-both-and-runs-one ()
  (with-cli ((if (member "--version" argv :test #'equal)
                  (values (format nil "~a 9.9" (first argv)) "" 0)
                  (values "entities" "" 0)))
    (with-cell-stop ((sem-start))
      (let ((status (nodecode-sem::run-command "" "s1")))
        (is (search "sem: sem 9.9" status) "Sem's version")
        (is (search "weave: weave 9.9" status) "and Weave's"))
      (let ((answer (nodecode-sem::run-command "find parse_config --json" "s1")))
        (is (search "entities" answer) "typed arguments go to sem")
        (is (equal '("sem" "find" "parse_config" "--json") (first (first *calls*))) "split on whitespace, never through a shell")))))
