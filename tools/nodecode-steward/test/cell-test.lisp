;;;; cell-test.lisp --- the steward cell over a scratch registry and a stubbed shell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here: the lifecycle every cell owns (start installs the
;;;; five tools, the manual topic, /steward and the round hook; stop takes
;;;; them back; a section of the wrong shape refuses); that a project opens
;;;; and its line reports the autonomy it was given; that a cycle runs the
;;;; declared checks through the one exec seam, in the project's own
;;;; directory, and grades them healthy, watch or degraded, and unknown when
;;;; a check could not run at all; that the allowlist is fail-closed; that an
;;;; initiative needs planner autonomy and a measured health, and that a
;;;; person approves it; and that a bound session reads its project's line at
;;;; its next round. Nothing here runs a real command.

(in-package #:nodecode.test)

(define-test-slice "steward" "STEWARD-CELL-" :start nodecode-steward:start-cell)

;;; --- a scratch registry, a stubbed shell, a stubbed round ---------------------------

(defvar *steward-exits* nil
  "((PROGRAM . STATUS) ...) the stubbed exec answers; a program not here passes.")

(defvar *steward-directories* nil
  "Every directory the stubbed exec was handed, newest first.")

(defvar *steward-delivered* nil
  "Every text the round hook recorded, newest first.")

(defun steward-scratch-file ()
  "A registry path of this test's own, under the temporary directory."
  (merge-pathnames (format nil "steward-~d-~d.json" (random 100000) (random 100000))
                   (uiop:temporary-directory)))

(defmacro with-steward-registry (&body body)
  "BODY over a scratch registry file, removed on the way out."
  (let ((path (gensym "REGISTRY")))
    `(let* ((,path (steward-scratch-file))
            (nodecode-steward::*data-file* ,path)
            (*steward-exits* '())
            (*steward-directories* '())
            (*steward-delivered* '()))
       (unwind-protect (progn ,@body)
         (when (probe-file ,path) (ignore-errors (delete-file ,path)))))))

(defmacro with-steward-running (&body body)
  "BODY with the cell started, and stopped however it leaves."
  `(with-cell-stop ((steward-start)) ,@body))

(defmacro with-steward-shell (&body body)
  "BODY with the cell's one exec seam answered from *STEWARD-EXITS*."
  `(with-stubbed-fdefinition (nodecode-steward::exec (argv directory seconds)
                               (let ((program (first argv))
                                     (entry (assoc (first argv) *steward-exits* :test #'string=)))
                                 (push directory *steward-directories*)
                                 (values (format nil "~a ran" program) ""
                                         (if entry (cdr entry) 0))))
     ,@body))

(defmacro with-steward-delivery (&body body)
  "BODY with the round hook's recording seam captured instead of written."
  `(with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &key text)
                               (push text *steward-delivered*))
     ,@body))

(defun steward-tool (name &rest pairs)
  "The registered tool NAME called as the dispatch seam calls it."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry) (apply #'nlk:make-json-object pairs))))

(defun steward-cwd ()
  "A directory that exists, to stand in for a project's own."
  (namestring (uiop:ensure-directory-pathname (uiop:temporary-directory))))

;;; --- lifecycle ------------------------------------------------------------------------

(define-cell-lifecycle-tests "steward"
  (:hooks :round)
  (:help :steward)
  (:command "steward")
  (:refused ("timeout_seconds" 0))
  (:running (is (nle::find-tool "steward_open") "steward_open is registered")
            (is (nle::find-tool "steward_status") "steward_status is registered")
            (is (nle::find-tool "steward_cycle") "steward_cycle is registered")
            (is (nle::find-tool "steward_propose") "steward_propose is registered")
            (is (nle::find-tool "steward_bind") "steward_bind is registered")
            (is (= 60 (nodecode-steward::setting :timeout-seconds)) "a check's ceiling defaults to a minute")))

;;; --- opening, and the board ----------------------------------------------------------

(deftest steward-cell-opens-a-project-and-shows-its-board ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-running
        (is-carrying (opened (steward-tool "steward_open" "project" "alpha" "title" "Alpha"
                                            "path" (steward-cwd) "autonomy" "builder"))
                     ("opened alpha (Alpha)")
                     ("autonomy: builder"))
        (is-carrying (answer (steward-tool "steward_status" "project" "alpha"))
                     ("alpha (Alpha)")
                     ("health not measured"))
        (is-carrying (board (steward-tool "steward_status")) ("1 project" "alpha"))
        (signals-error nodecode-steward:steward-error
          (steward-tool "steward_status" "project" "nowhere"))))))

(deftest steward-cell-refuses-an-unknown-autonomy-level ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-running
        (signals-error nodecode-steward:steward-error
          (steward-tool "steward_open" "project" "beta" "autonomy" "benevolent-dictator"))
        (is (null (nodecode-steward::find-project (nodecode-steward::registry) "beta"))
            "a refused open leaves no half a project")))))

;;; --- a cycle -------------------------------------------------------------------------

(deftest steward-cell-a-cycle-runs-the-declared-checks-and-grades-them ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-running
        (let ((directory (steward-cwd)))
          (steward-tool "steward_open" "project" "alpha" "path" directory "autonomy" "builder"
                         "checks" (vector "tests: make test" "lint: flake")
                         "allow" (vector "make" "flake"))
          (setf *steward-exits* '(("make" . 0) ("flake" . 0)))
          (is-carrying (healthy (steward-tool "steward_cycle" "project" "alpha"))
                       ("alpha: healthy"))
          (is (and *steward-directories*
                   (every (lambda (one) (string= (namestring one) directory))
                          *steward-directories*))
              "each check ran in the project's own directory")
          (setf *steward-exits* '(("make" . 0) ("flake" . 1)))
          (is-carrying (watch (steward-tool "steward_cycle" "project" "alpha"))
                       ("alpha: watch"))
          (setf *steward-exits* '(("make" . 2) ("flake" . 1)))
          (is-carrying (degraded (steward-tool "steward_cycle" "project" "alpha"))
                       ("alpha: degraded"))
          (is-carrying (status (steward-tool "steward_status" "project" "alpha"))
                       ("health degraded")))))))

(deftest steward-cell-an-unrunnable-check-keeps-health-unknown ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-running
        (steward-tool "steward_open" "project" "alpha" "path" (steward-cwd)
                       "autonomy" "builder"
                       "checks" (vector "tests: make test" "docs: flake")
                       "allow" (vector "make"))
        (setf *steward-exits* '(("make" . 0)))
        (is-carrying (answer (steward-tool "steward_cycle" "project" "alpha"))
                     ("alpha: unknown")
                     ("docs: unmeasured")
                     ("not on this project's allowlist"))))))

(deftest steward-cell-a-cycle-needs-builder-autonomy-and-some-checks ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-running
        (steward-tool "steward_open" "project" "alpha" "path" (steward-cwd)
                       "autonomy" "planner"
                       "checks" (vector "tests: make test") "allow" (vector "make"))
        (signals-error nodecode-steward:steward-error
          (steward-tool "steward_cycle" "project" "alpha"))
        (steward-tool "steward_open" "project" "bare" "path" (steward-cwd)
                       "autonomy" "builder")
        (signals-error nodecode-steward:steward-error
          (steward-tool "steward_cycle" "project" "bare"))))))

;;; --- initiatives, and a person's approval ---------------------------------------------

(deftest steward-cell-a-proposal-is-gated-on-level-and-on-health ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-running
        (steward-tool "steward_open" "project" "alpha" "path" (steward-cwd)
                       "autonomy" "planner"
                       "checks" (vector "tests: make test") "allow" (vector "make"))
        (signals-error nodecode-steward:steward-error
          (steward-tool "steward_propose" "project" "alpha" "title" "tidy the ci"
                         "rationale" "the check takes nine minutes"))
        (steward-tool "steward_open" "project" "alpha" "autonomy" "builder")
        (setf *steward-exits* '(("make" . 0)))
        (steward-tool "steward_cycle" "project" "alpha")
        (is-carrying (proposed (steward-tool "steward_propose" "project" "alpha"
                                             "title" "tidy the ci"
                                             "rationale" "the check takes nine minutes"
                                             "risk" "low"))
                     ("proposed i1" "tidy the ci"))
        (is-carrying (before (steward-tool "steward_status" "project" "alpha"))
                     ("i1  tidy the ci" "proposed"))
        (is-carrying (approved (nodecode-steward:steer "approve alpha i1" "s1"))
                     ("i1 is now approved"))
        (is-carrying (after (steward-tool "steward_status" "project" "alpha"))
                     ("approved"))))))

(deftest steward-cell-an-unmeasured-project-takes-no-proposal ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-running
        (steward-tool "steward_open" "project" "alpha" "path" (steward-cwd)
                       "autonomy" "steward"
                       "checks" (vector "tests: make test" "lint: flake")
                       "allow" (vector "make"))
        (setf *steward-exits* '(("make" . 0)))
        (steward-tool "steward_cycle" "project" "alpha")
        (signals-error nodecode-steward:steward-error
          (steward-tool "steward_propose" "project" "alpha" "title" "x" "rationale" "y"))))))

;;; --- a session that follows a project --------------------------------------------------

(deftest steward-cell-a-bound-session-reads-its-project-at-the-round ()
  (with-steward-registry
    (with-steward-shell
      (with-steward-delivery
        (with-steward-running
          (steward-tool "steward_open" "project" "alpha" "title" "Alpha" "path" (steward-cwd)
                         "autonomy" "planner" "briefing" "the release branch is frozen")
          (is-carrying (bound (steward-tool "steward_bind" "project" "alpha" "session" "s1"))
                       ("this session follows alpha"))
          (nodecode-steward::deliver (list :session-id "s1" :turn "t1"))
          (is (= 1 (length *steward-delivered*)) "the bound session is handed one line")
          (is-carrying (line (first *steward-delivered*))
                       ("steward: alpha is at autonomy planner")
                       ("health not measured")
                       ("the release branch is frozen"))
          (nodecode-steward::deliver (list :session-id "s2" :turn "t2"))
          (is (= 1 (length *steward-delivered*)) "an unbound session is handed nothing")
          (steward-tool "steward_bind" "session" "s1")
          (nodecode-steward::deliver (list :session-id "s1" :turn "t3"))
          (is (= 1 (length *steward-delivered*)) "unbinding stops the line"))))))
