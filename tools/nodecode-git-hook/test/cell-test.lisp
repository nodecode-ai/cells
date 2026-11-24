;;;; cell-test.lisp --- git-hook against a stubbed git.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the lifecycle every cell owns; that a clean worktree
;;;; is pulled once per session and a dirty one never; that the paths a turn
;;;; changed are the only ones committed, an edit to an already-dirty file
;;;; included; that a secret-looking name is never staged; that a push refused
;;;; on policy drops the hook's own commit and closes the branch, saying so
;;;; once; that /git-hook reports and moves its switches; and that a git that
;;;; cannot run at all never fails the tool call. Nothing here needs git
;;;; installed, and no repository is created: the process seam answers.

(in-package #:nodecode.test)

;;; --- a stubbed git ---------------------------------------------------------

(defvar *root* nil "What `git rev-parse --show-toplevel' answers.")
(defvar *status* "" "What `git status --porcelain' answers.")
(defvar *upstream* "origin/main" "What the upstream lookup answers; NIL means there is none.")
(defvar *branch* "main" "What the checked-out branch is.")
(defvar *tracked* "" "What `git ls-files' answers: NUL-separated paths.")
(defvar *push-answer* '("" "" 0) "What `git push' answers: (OUTPUT ERROR STATUS).")
(defvar *commit-answer* '("" "" 0) "What `git commit' answers.")
(defvar *calls* '() "Every git command the cell ran, newest first: one line a call.")

(defun fake-git (dir args seconds)
  "The stub: DIR and SECONDS are ignored, ARGS decide the answer."
  (declare (ignore dir seconds))
  (let ((line (format nil "~{~a~^ ~}" args)))
    (push line *calls*)
    (cond
      ((search "status --porcelain" line) (values *status* "" 0))
      ((search "rev-parse --show-toplevel" line)
       (if *root* (values *root* "" 0) (values "" "not a git repository" 128)))
      ((search "--symbolic-full-name" line)
       (if *upstream* (values *upstream* "" 0) (values "" "no upstream" 128)))
      ((search "rev-parse --abbrev-ref" line) (values *branch* "" 0))
      ((search "rev-parse HEAD" line) (values "0123456789abcdef0123456789abcdef01234567" "" 0))
      ((search "pull --ff-only" line) (values "" "" 0))
      ((search "ls-files" line) (values *tracked* "" 0))
      ((search "push" line) (values-list *push-answer*))
      ((search "reset --soft" line) (values "" "" 0))
      ((search "add " line) (values "" "" 0))
      ((search "commit" line) (values-list *commit-answer*))
      (t (values "" "" 0)))))

(defmacro with-git-fake ((root) &body body)
  "Run BODY against the stub, every answer at its default and ROOT as the
worktree git reports."
  `(let ((*root* ,root) (*status* "") (*upstream* "origin/main") (*branch* "main")
         (*tracked* "") (*push-answer* (list "" "" 0)) (*commit-answer* (list "" "" 0))
         (*calls* '())
         (nodecode-git-hook::*run* #'fake-git))
     ,@body))

(defmacro gh-with-turn ((session) &body body)
  "Run BODY as a turn of SESSION: the cell reads the running turn for it."
  `(let ((nle::*live-turn* (nle::%make-live-turn :session-id ,session :turn-id "t1")))
     ,@body))

(defun some-call (needle)
  "The first command the cell ran that contains NEEDLE, or NIL."
  (some (lambda (line) (and (search needle line) t)) *calls*))

(defun call-count (needle)
  "How many commands the cell ran that contain NEEDLE."
  (count-if (lambda (line) (and (search needle line) t)) *calls*))

(defun scratch-dir (&rest files)
  "A fresh directory with FILES written into it: (NAME . CONTENT) ..."
  (let ((dir (merge-pathnames (format nil "git-hook-~a/" (random 10000000))
                               (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    (dolist (file files)
      (let ((path (merge-pathnames (car file) dir)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
          (write-string (cdr file) out))))
    (string-right-trim "/" (namestring dir))))

(defun status-of (&rest rels)
  "A `git status --porcelain -z' answer naming RELS, each (REL . STATUS)."
  (with-output-to-string (out)
    (dolist (rel rels) (format out "~a ~a~c" (cdr rel) (car rel) #\Null))))

(defun call-op (name &rest pairs)
  "A :TOOL op naming NAME with a JSON arguments object from PAIRS."
  (let ((object (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key object) value))
    (list :name name :arguments object :call-id "c1")))

(defun git-hook-call (op)
  "Run OP through the installed :tool point with a NEXT that answers :RAN.
=> (values ANSWER RAN-P)."
  (let ((ran nil))
    (values (funcall (cell-hook "nodecode-git-hook" :tool) op
                      (lambda (op) (declare (ignore op)) (setf ran t) :ran))
            ran)))

(defun git-hook-round (session)
  "Run the :round point as the core does, for SESSION."
  (funcall (cell-hook "nodecode-git-hook" :round)
           (list :session-id session :turn "t1" :round 1)
           (lambda (op) (declare (ignore op)) nil)))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "git-hook"
  (:config "roots" (vector (scratch-dir)))
  (:hooks :tool :round)
  (:command "git-hook")
  (:running (is (eq t nodecode-git-hook::*commit-on*) "commits are on, as the section's default says")
            (is (eq t nodecode-git-hook::*push-on*) "and so are pushes")
            (is (= 12 (nodecode-git-hook::setting :pull-timeout-seconds)) "a pull's ceiling is 12 seconds"))
  (:refused ()
            ("roots" (vector (scratch-dir)) "budget_seconds" 0)
            ("roots" (vector (scratch-dir)) "pull_timeout_seconds" 0))
  (:idle nodecode-git-hook:git-hook-error
         (nodecode-git-hook::run-command "" "s1")))

;;; --- what a call does to the worktree --------------------------------------

(deftest git-hook-cell-pulls-a-clean-worktree-once ()
  (let ((root (scratch-dir '("a.txt" . "one"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (gh-with-turn ("s1")
          (git-hook-call (call-op "read-file" "path" (format nil "~a/a.txt" root)))
          (is (= 1 (call-count "pull --ff-only")) "a clean worktree with an upstream is pulled before a read")
          (git-hook-call (call-op "read-file" "path" (format nil "~a/a.txt" root)))
          (is (= 1 (call-count "pull --ff-only")) "and only once per session"))))))

(deftest git-hook-cell-leaves-a-dirty-worktree-alone ()
  (let ((root (scratch-dir '("a.txt" . "one"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (setf *status* (status-of '("a.txt" . " M")))
        (gh-with-turn ("s1")
          (git-hook-call (call-op "read-file" "path" (format nil "~a/a.txt" root)))
          (is (zerop (call-count "pull --ff-only")) "a dirty worktree is never pulled"))))))

(deftest git-hook-cell-does-not-pull-beside-a-write ()
  (let ((root (scratch-dir '("a.txt" . "one"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (gh-with-turn ("s1")
          (git-hook-call (call-op "write-file" "path" (format nil "~a/a.txt" root)))
          (is (zerop (call-count "pull --ff-only")) "a call that lands content gets no pull beside it"))))))

(deftest git-hook-cell-commits-only-what-a-turn-changed ()
  (let ((root (scratch-dir '("a.txt" . "one") '("b.txt" . "two"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (setf *status* (status-of '("b.txt" . " M")))
        (gh-with-turn ("s1")
          (git-hook-call (call-op "write-file" "path" (format nil "~a/a.txt" root)))
          (setf *status* (status-of '("b.txt" . " M") '("a.txt" . " M")))
          (git-hook-call (call-op "write-file" "path" (format nil "~a/a.txt" root)))
          (git-hook-round "s1")
          (is (some-call "add -- :(literal)a.txt") "the path this turn changed is staged")
          (is (not (some-call ":(literal)b.txt")) "and dirt nobody touched is left alone")
          (is (some-call "commit -m update a.txt") "with the turn's own subject")
          (is (some-call "push") "and pushed"))))))

(deftest git-hook-cell-sees-an-edit-to-an-already-dirty-file ()
  (let ((root (scratch-dir '("a.txt" . "one"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (setf *status* (status-of '("a.txt" . " M")))
        (gh-with-turn ("s1")
          (git-hook-call (call-op "edit" "path" (format nil "~a/a.txt" root)))
          (with-open-file (out (merge-pathnames "a.txt" (uiop:ensure-directory-pathname root))
                               :direction :output :if-exists :append)
            (write-string "and more" out))
          (git-hook-call (call-op "edit" "path" (format nil "~a/a.txt" root)))
          (git-hook-round "s1")
          (is (some-call ":(literal)a.txt") "a rewrite of an already-dirty file is a delta too"))))))

(deftest git-hook-cell-never-stages-a-secret ()
  (let ((root (scratch-dir '("notes.md" . "n") '(".env" . "TOKEN=1"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (gh-with-turn ("s1")
          (git-hook-call (call-op "write-file" "path" (format nil "~a/notes.md" root)))
          (setf *status* (status-of '("notes.md" . " M") '(".env" . "??")))
          (git-hook-call (call-op "write-file" "path" (format nil "~a/notes.md" root)))
          (git-hook-round "s1")
          (is (some-call ":(literal)notes.md") "a real file is staged")
          (is (not (some-call ".env")) "a secret-looking name is not"))))))

(deftest git-hook-cell-a-policy-refusal-closes-the-branch ()
  (let ((root (scratch-dir '("a.txt" . "one"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (setf *push-answer* (list "" "remote: error: GH013: Repository rule violations found" 1))
        (gh-with-turn ("s1")
          (git-hook-call (call-op "write-file" "path" (format nil "~a/a.txt" root)))
          (setf *status* (status-of '("a.txt" . " M")))
          (git-hook-call (call-op "write-file" "path" (format nil "~a/a.txt" root)))
          (git-hook-round "s1")
          (is (some-call "reset --soft") "its own unpushable commit is dropped again")
          (let ((notice (second (cell-notice "nodecode-git-hook"))))
            (is (and notice (search "rejects direct pushes" notice))
                (format nil "the operator is told which branch, and why: ~s" notice))
            (is (and notice (search "a pull request is required" notice))
                (format nil "and what to do instead: ~s" notice)))
          (let ((committed (call-count "commit -m")))
            (git-hook-call (call-op "write-file" "path" (format nil "~a/a.txt" root)))
            (git-hook-round "s1")
            (is (= committed (call-count "commit -m")) "and nothing is committed there again this session")))))))

(deftest git-hook-cell-a-broken-git-never-fails-the-call ()
  (let ((root (scratch-dir '("a.txt" . "one"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (let ((nodecode-git-hook::*run*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error "git is gone"))))
        (gh-with-turn ("s1")
          (multiple-value-bind (answer ran)
              (git-hook-call (call-op "read-file" "path" (format nil "~a/a.txt" root)))
            (is (eq :ran answer) "the call's own answer comes back")
            (is ran "and the tool really ran")))))))

;;; --- the rules the hook reads ----------------------------------------------

(deftest git-hook-cell-reads-a-porcelain-answer ()
  (let ((records (nodecode-git-hook::parse-porcelain
                  (format nil " M a.txt~c?? b.txt~cR  c.txt~cd.txt~c"
                          #\Null #\Null #\Null #\Null))))
    (is (equal '("a.txt" " M") (first records)) "a change is (REL STATUS)")
    (is (equal '("b.txt" "??") (second records)) "an untracked file is one too")
    (is (equal '("c.txt" "R ") (third records)) "a rename names its new path")
    (is (equal '("d.txt" "R ") (fourth records)) "and its source")))

(deftest git-hook-cell-skips-scratch-and-secret-names ()
  (with-cell-stop ((git-hook-start "roots" (vector (scratch-dir)) "secret_names" (vector "prod-passwords")))
    (is (nodecode-git-hook::secret-path-p ".env.local") "a .env file is a secret")
    (is (nodecode-git-hook::secret-path-p "keys/auth.json") "so is auth.json, wherever it sits")
    (is (nodecode-git-hook::secret-path-p "keys/id_ed25519") "so is an ssh key")
    (is (nodecode-git-hook::secret-path-p "deploy/prod-passwords.txt") "and a name the section adds")
    (is (not (nodecode-git-hook::secret-path-p "src/app.lisp")) "a source file is not")
    (is (nodecode-git-hook::transient-p "src/.write-tmp.4821") "an atomic-write temp is scratch")
    (is (nodecode-git-hook::transient-p "#app.lisp#") "and so is an editor's lock file")
    (is (not (nodecode-git-hook::transient-p "src/template.lisp")) "a real file is neither")))

(deftest git-hook-cell-knows-a-policy-refusal ()
  (is (nodecode-git-hook::policy-rejection-p
       "remote: error: GH013: Repository rule violations found for refs/heads/main") "a ruleset that requires a pull request")
  (is (nodecode-git-hook::policy-rejection-p
       "! [remote rejected] main -> main (protected branch hook declined)") "and a protected branch")
  (is (not (nodecode-git-hook::policy-rejection-p
            "fatal: unable to access 'https://example.invalid/r.git': Could not resolve host")) "a network failure is not policy"))

;;; --- /git-hook -------------------------------------------------------------

(deftest git-hook-cell-command-reports-and-moves-its-switches ()
  (let ((root (scratch-dir '("a.txt" . "one"))))
    (with-cell-stop ((git-hook-start "roots" (vector root)))
      (with-git-fake (root)
        (let ((status (nodecode-git-hook::run-command "" "s1")))
          (is (search "commits on" status) "status names the switches")
          (is (search root status) "and the allowlist it acts on"))
        (let ((flushed (nodecode-git-hook::run-command "flush" "s1")))
          (is (search "nothing was waiting" flushed) "a flush with nothing queued says so"))
        (nodecode-git-hook::run-command "off" "s1")
        (is (not nodecode-git-hook::*commit-on*) "off turns commits off at once")
        (nodecode-git-hook::run-command "push off" "s1")
        (is (not nodecode-git-hook::*push-on*) "push off keeps them local")
        (is (search "/git-hook [status" (nodecode-git-hook::run-command "sometimes" "s1")) "an unknown word is usage")))))
