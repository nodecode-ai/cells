;;;; cell-test.lisp --- the chronos cell against a stubbed git.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs git, a repository or the network: every command line goes
;;;; through NODECODE-CHRONOS::*RUN*, a stub that records the argv it was
;;;; handed and answers what a shadow repository would. What is under test is
;;;; the part this cell owns -- the command lines it builds, the metadata it
;;;; reads out of a commit, the answers the model reads, and the hook that
;;;; takes a checkpoint before a call and only before one that can change the
;;;; workspace.

(in-package #:nodecode.test)

(define-test-slice "chronos" "CHRONOS-CELL-" :start nodecode-chronos:start-cell)

(define-cell-lifecycle-tests "chronos"
  (:help :chronos)
  (:command "chronos")
  (:hooks :tool)
  (:running
   (is (nle::find-tool "chronos_checkpoint") "the checkpoint tool is registered")
   (is (nle::find-tool "chronos_diff") "so is the diff tool")
   (is (null (nodecode-chronos::setting :workspace)) "with no workspace configured for the run"))
  (:stopped
   (is (null (nle::find-tool "chronos_checkpoint")) "stop takes the tools back")
   (is (null (nle::find-tool "chronos_rollback")) "every one of them"))
  (:refused ("max_checkpoints" 0)
            ("max_checkpoints" "many")
            ("timeout_seconds" "soon")))

;;; --- the stub --------------------------------------------------------------------

(defvar *calls* nil
  "The git command lines the stub was handed, newest first.")

(defparameter +sha+ "1111111111111111111111111111111111111111")
(defparameter +older+ "2222222222222222222222222222222222222222")

(defun stub-git (&key (dirty t) (head +sha+) (log "") (diff "") (untracked ""))
  "A git stub: it records every command line in *CALLS* and answers the way a
shadow repository with HEAD at +SHA+ would."
  (lambda (argv directory)
    (declare (ignore directory))
    (push argv *calls*)
    (let ((arguments (cdr argv)))
      (cond
        ((member "init" arguments :test #'equal) (values "" "" 0))
        ((member "rev-parse" arguments :test #'equal)
         (if head (values (format nil "~a~%" head) "" 0) (values "" "" 128)))
        ((member "add" arguments :test #'equal) (values "" "" 0))
        ((member "diff-index" arguments :test #'equal) (values "" "" (if dirty 1 0)))
        ((member "commit" arguments :test #'equal) (values "" "" 0))
        ((member "log" arguments :test #'equal) (values log "" (if head 0 128)))
        ((member "ls-files" arguments :test #'equal) (values untracked "" 0))
        ((member "ls-tree" arguments :test #'equal) (values "notes.txt" "" 0))
        ((member "diff" arguments :test #'equal) (values diff "" 0))
        (t (values "" "" 0))))))

(defmacro with-chronos ((&rest options) &body body)
  "BODY with the cell started, every git run stubbed, and a scratch directory
as the workspace the cell checkpoints."
  (let ((directory (gensym "DIRECTORY")))
    `(let* ((,directory (merge-pathnames (format nil "chronos-~d/" (random 1000000))
                                          (uiop:temporary-directory)))
            (*calls* nil)
            (nodecode-chronos::*run* (stub-git ,@options))
            (nodecode-chronos::*workspace-root*
              (string-right-trim "/" (uiop:native-namestring ,directory))))
       (ensure-directories-exist ,directory)
       (unwind-protect
            (with-cell-stop ((chronos-start)) ,@body)
         (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore)))))

(defun call-tool (name arguments)
  "NAME called the way the model calls it, as the text it reads back."
  (nle::message-content (execute-wire-call "chronos-call" name arguments)))

(defun call-answer (name arguments)
  "NAME's answer for ARGUMENTS, decoded."
  (nlk:decode-json (call-tool name arguments)))

(defun ran-p (verb)
  "Did the stub see a git command whose arguments include VERB?"
  (some (lambda (argv) (member verb (cdr argv) :test #'equal)) *calls*))

(defun git-call-line (verb)
  "The argv of the last git run that carried VERB, or NIL."
  (find-if (lambda (argv) (member verb (cdr argv) :test #'equal)) *calls*))

(defun log-line (hash time description tool)
  "One line of `git log --pretty=format:%H%x09%ct%x09%s', as the cell reads it."
  (format nil "~a~c~d~c~a~a"
          hash #\Tab time #\Tab nodecode-chronos::+message-prefix+
          (nlk:encode-json-object (nlk:json-object "desc" description "tool" tool "time" time))))

;;; --- the four tools ---------------------------------------------------------------

(deftest chronos-cell-the-checkpoint-tool-records-one ()
  (with-chronos (:dirty t)
    (let ((result (call-answer "chronos_checkpoint" "{\"description\": \"before the migration\"}")))
      (is (equal "ok" (gethash "status" result)) "the checkpoint was taken")
      (is (eq t (gethash "created" result)) "and says it created one")
      (is (= 8 (length (gethash "id" result))) "under the short id the listing shows")
      (is (equal "before the migration" (gethash "description" result)) "with the description it was given")
      (let ((commit (find-if (lambda (argv) (member "commit" (cdr argv) :test #'equal)) *calls*)))
        (is (not (null commit)) "a commit was run")
        (is (search nodecode-chronos::+message-prefix+ (format nil "~{~a~^ ~}" commit)) "under the metadata message")
        (is (member "--no-verify" commit :test #'equal) "with the operator's git hooks kept out of it")))
    (is (ran-p "diff-index") "the change was looked for before a commit was written")))

(deftest chronos-cell-a-call-that-changed-nothing-records-none ()
  (with-chronos (:dirty nil)
    (let ((result (call-answer "chronos_checkpoint" "{\"description\": \"nothing to do\"}")))
      (is (equal "ok" (gethash "status" result)) "the call is fine")
      (is (null (gethash "created" result)) "but no checkpoint was created")
      (is (null (ran-p "commit")) "and no commit ran"))))

(deftest chronos-cell-the-list-reads-the-metadata-out-of-the-commit ()
  (with-chronos (:log (format nil "~a~%~a~%"
                               (log-line +sha+ 1760000000 "before the edit" "edit")
                               (log-line +older+ 1759990000 "baseline" "system")))
    (let* ((result (call-answer "chronos_list_checkpoints" "{\"limit\": 5}"))
           (first (aref (gethash "checkpoints" result) 0)))
      (is (equal "ok" (gethash "status" result)) "the listing answers")
      (is (= 2 (gethash "count" result)) "both checkpoints")
      (is (equal "before the edit" (gethash "description" first)) "the description comes out of the message")
      (is (equal "edit" (gethash "tool" first)) "with the tool that took it")
      (is (equal "11111111" (gethash "id" first)) "and the short id")
      (is (= 1760000000 (gethash "time" first)) "and when"))))

(deftest chronos-cell-diff-of-one-checkpoint-builds-the-run ()
  (with-chronos (:diff "--- a/notes.txt~%+++ b/notes.txt~%@@ -1 +1 @@~%-old~%+new~%")
    (let ((result (call-answer "chronos_diff" "{\"checkpoint_id\": \"11111111\"}")))
      (is (equal "ok" (gethash "status" result)) "the diff answers")
      (is (search "+new" (gethash "diff" result)) "with the change in it")
      (let ((diff (find-if (lambda (argv) (member "diff" (cdr argv) :test #'equal)) *calls*)))
        (is (member "--no-ext-diff" diff :test #'equal) "the operator's diff config cannot change the output")
        (is (ran-p "11111111^{commit}") "the id was resolved to a commit before the diff ran")
        (is (member (git-call-line "diff") *calls*) "the diff ran against the parent")))))

(deftest chronos-cell-a-worktree-diff-lists-what-it-did-not-cover ()
  (with-chronos (:diff "" :untracked "notes.txt")
    (let ((result (call-answer "chronos_diff" "{\"from_checkpoint\": \"latest\"}")))
      (is (equal "ok" (gethash "status" result)) "a diff from the latest checkpoint answers")
      (is (equal "WORKTREE" (gethash "to" result)) "against the files on disk")
      (is (equal "notes.txt" (aref (gethash "untracked" result) 0)) "and names the untracked file"))))

(deftest chronos-cell-a-flag-shaped-id-never-reaches-git ()
  (with-chronos ()
    (let ((result (call-answer "chronos_diff" "{\"from_checkpoint\": \"-x\"}")))
      (is (equal "error" (gethash "status" result)) "an option is no checkpoint")
      (is (search "no checkpoint" (gethash "error" result)) "and the answer says so"))))

(deftest chronos-cell-a-missing-question-is-refused-before-anything-runs ()
  (with-chronos ()
    (let ((text (call-tool "chronos_rollback" "{}")))
      (is (search "checkpoint_id" text) "the refusal names the member it wanted")
      (is (null *calls*) "and nothing ran"))))

(deftest chronos-cell-a-rollback-takes-a-safety-checkpoint-first ()
  (with-chronos (:untracked "notes.txt")
    (let ((result (call-answer "chronos_rollback" "{\"checkpoint_id\": \"latest\", \"clean\": true}")))
      (is (equal "ok" (gethash "status" result)) "the restore answers")
      (is (equal "11111111" (gethash "restored" result)) "with what it restored")
      (is (not (null (gethash "safety" result))) "and the safety checkpoint it took first")
      (is (ran-p "read-tree") "which is how the tree was written back")
      (is (ran-p "commit") "the safety checkpoint is a commit of its own"))))

;;; --- the hook and the command -----------------------------------------------------

(deftest chronos-cell-the-hook-checkpoints-a-modifying-call-only ()
  (with-chronos (:dirty t)
    (let* ((op (list :name "eval" :arguments (nlk:json-object "form" "(write-file \"a\" \"b\")")))
           (result (nodecode-chronos::around-tool op (lambda (call) (declare (ignore call)) "done"))))
      (is (equal "done" result) "the call ran and its answer came back")
      (let ((commit (find-if (lambda (argv) (member "commit" (cdr argv) :test #'equal)) *calls*)))
        (is (not (null commit)) "with a checkpoint taken before it")
        (is (search "eval" (format nil "~{~a~^ ~}" commit)) "described by the tool it precedes")))
    (setf *calls* nil)
    (let ((result (nodecode-chronos::around-tool (list :name "help") (lambda (call) (declare (ignore call)) "text"))))
      (is (equal "text" result) "a call that cannot change a file passes through")
      (is (null *calls*) "and git is not started for it"))))

(deftest chronos-cell-the-command-lists-what-is-recorded ()
  (with-chronos (:log (log-line +sha+ 1760000000 "before the edit" "edit"))
    (let ((text (nodecode-chronos::run-command "" nil)))
      (is (search "1 checkpoint" text) "the command counts them")
      (is (search "before the edit" text) "and names each one")
      (is (search "/chronos rollback" text) "with the verb that puts one back")))
  (with-chronos (:log "")
    (is (search "no checkpoint yet" (nodecode-chronos::run-command "list" nil)) "an empty repository says so")))

(deftest chronos-cell-the-manual-carries-the-four-tools ()
  (with-cell-stop ((chronos-start))
    (let ((text (cddr (assoc :chronos nle::*help-topics*))))
      (is (search "chronos_checkpoint" text) "the checkpoint tool is named")
      (is (search "chronos_rollback" text) "the rollback tool too")
      (is (search "rollback can itself be" text) "and the reason a rollback can be undone")
      (is (search "nothing leaves this machine" text) "and that it stays local"))))
