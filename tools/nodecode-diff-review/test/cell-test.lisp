;;;; cell-test.lisp --- the diff-review cell against a stubbed git.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a repository, a git install or the network: every run goes
;;;; through NODECODE-DIFF-REVIEW::*RUN*, a stub that records the command line
;;;; it was handed and answers what a repository would. What is under test is
;;;; the part this cell owns -- the command line it builds, the reading of the
;;;; diff that comes back, and the JSON the model reads.

(in-package #:nodecode.test)

(define-test-slice "diff-review" "DIFF-REVIEW-CELL-" :start nodecode-diff-review:start-cell)

(define-cell-lifecycle-tests "diff-review"
  (:help :diff-review)
  (:command "diff-review")
  (:running
   (is (nle::find-tool "diff_review") "the review tool is registered")
   (is (equal 30 (nodecode-diff-review::setting :timeout-seconds)) "and the section's defaults are the values it runs on"))
  (:stopped
   (is (null (nle::find-tool "diff_review")) "stop takes the tool back"))
  (:refused ("timeout_seconds" 0)
            ("timeout_seconds" "soon")
            ("max_warnings" 0)
            ("max_files" 0)))

;;; --- the stub --------------------------------------------------------------------

(defvar *calls* nil
  "The git command lines the stub was handed, newest first.")

(defun stub-runner (&key (toplevel "/work/project") (untracked "")
                         (sha "1111111111111111111111111111111111111111")
                         (version "git version 2.43.0") (diff "") (stderr "")
                         (status 0) (quiet-status 0))
  "A git stub: it records every command line in *CALLS* and answers the way a
repository at TOPLEVEL would, with DIFF as git's own diff output."
  (lambda (argv directory)
    (declare (ignore directory))
    (push argv *calls*)
    (let ((arguments (rest argv)))
      (cond
        ((member "--version" arguments :test #'equal) (values version "" 0))
        ((member "rev-parse" arguments :test #'equal)
         (if (member "--show-toplevel" arguments :test #'equal)
             (values (format nil "~a~%" toplevel) "" 0)
             (values (format nil "~a~%" sha) "" 0)))
        ((member "symbolic-ref" arguments :test #'equal) (values "refs/heads/main" "" 0))
        ((member "ls-files" arguments :test #'equal) (values untracked "" 0))
        ((member "hash-object" arguments :test #'equal) (values (format nil "~a~%" sha) "" 0))
        ((member "merge-base" arguments :test #'equal) (values (format nil "~a~%" sha) "" 0))
        ((member "diff" arguments :test #'equal)
         (if (find "--quiet" arguments :test #'equal)
             (values "" "" quiet-status)
             (values diff stderr status)))
        (t (values "" "" 0))))))

(defmacro with-git (options &body body)
  "BODY with the cell started and every git run answered by a stub."
  `(let ((*calls* nil)
         (nodecode-diff-review::*run* (stub-runner ,@options)))
     (with-cell-stop ((diff-review-start))
       ,@body)))

(defun review-call (arguments)
  "The tool called the way the model calls it, as the text it reads back."
  (nle::message-content (execute-wire-call "diff-review-call" "diff_review" arguments)))

(defun review-answer (arguments)
  "The tool's answer for ARGUMENTS, decoded."
  (nlk:decode-json (review-call arguments)))

(defparameter +sample-diff+
  (format nil "~{~a~%~}"
          (list "diff --git a/app.py b/app.py"
                "--- a/app.py"
                "+++ b/app.py"
                "@@ -1,3 +1,7 @@"
                " import os"
                "+print(\"debug\")"
                "+api_key = \"abcdef123456\""
                "+except:"
                "-print(\"gone\")"
                "+TODO: tidy this"
                "+console.log(x)"
                " done"))
  "A diff with one line of each thing the rules read, and one removed line that
must not count.")

;;; --- the rules, on a diff handed in ------------------------------------------

(deftest diff-review-cell-a-diff-text-is-read-for-what-it-adds ()

  (with-git ()

    (let* ((answer (review-call (concatenate 'string "{\"diff_text\": "

                                              (nlk:encode-json-object +sample-diff+)

                                              "}")))

           (result (nlk:decode-json answer)))

      (is (equal "ok" (gethash "status" result)) "a diff handed in is reviewed")

      (is (= 1 (gethash "files_changed" result)) "one file")

      (is (= 5 (gethash "total_added" result)) "five added lines")

      (is (= 1 (gethash "total_removed" result)) "one removed line, which no rule reads")

      (is (= 5 (gethash "warning_count" result)) "five findings, none of them the removed print")

      (let ((issues (map 'list (lambda (warning) (gethash "issue" warning))

                         (gethash "warnings" result))))

        (is (member "debug print statement" issues :test #'equal) "the print( is named")

        (is (member "possible hardcoded secret" issues :test #'equal) "so is the assigned key")

        (is (member "bare except clause" issues :test #'equal) "the bare except too")

        (is (member "unresolved TODO/FIXME" issues :test #'equal) "and the marker")

        (is (member "console.log statement" issues :test #'equal) "and the JavaScript call"))

      (is (equal "diff_text" (gethash "mode" (gethash "comparison" result))) "and the answer says which review it was"))))

(deftest diff-review-cell-text-that-is-no-diff-is-refused ()
  (with-git ()
    (let ((result (review-answer "{\"diff_text\": \"just some words\"}")))
      (is (equal "error" (gethash "status" result)) "not a diff, not a review")
      (is (equal "invalid_argument" (gethash "error_type" result)) "and said so")
      (is (search "unified diff" (gethash "message" result)) "with what was wanted"))))

(deftest diff-review-cell-a-flag-shaped-ref-is-refused-before-git-runs ()
  (with-git ()
    (let ((result (review-answer "{\"base\": \"-x\"}")))
      (is (equal "invalid_argument" (gethash "error_type" result)) "an option is no revision")
      (is (null *calls*) "and git was never started"))))

;;; --- the working tree -------------------------------------------------------------

(deftest diff-review-cell-the-working-tree-run-fixes-its-flags ()
  (with-git (:diff +sample-diff+)
    (let ((result (review-answer "{}")))
      (is (equal "ok" (gethash "status" result)) "the stub's diff is the review")
      (let ((call (find-if (lambda (argv) (member "diff" (rest argv) :test #'equal)) *calls*)))
        (is (equal "git" (first call)) "git was the program")
        (is (member "--no-ext-diff" call :test #'equal) "the flags the parser needs are passed")
        (is (member "--no-textconv" call :test #'equal) "so the operator's config cannot change the output")
        (is (member "core.quotePath=false" call :test #'equal) "paths arrive unquoted where they can")
        (is (member "1111111111111111111111111111111111111111" call :test #'equal) "and the base ref was resolved to a commit first")
        (is (equal "--" (car (last call))) "with the pathspec terminator last"))
      (is (= 1 (gethash "files_changed" result)) "one file changed")
      (is (equal "app.py" (gethash "path" (aref (gethash "files" result) 0))) "the a/ and b/ prefixes are off the path"))))

(deftest diff-review-cell-untracked-files-are-read-too ()
  (let* ((directory (merge-pathnames (format nil "diff-review-~d/" (random 1000000))
                                     (uiop:temporary-directory)))
         (file (merge-pathnames "notes.py" directory)))
    (ensure-directories-exist directory)
    (uiop:with-output-file (out file :if-exists :supersede)
      (write-line "print(\"uncommitted\")" out))
    (unwind-protect
         (with-git (:toplevel (uiop:native-namestring directory) :untracked "notes.py")
           (let* ((result (review-answer "{}"))
                  (files (gethash "files" result)))
             (is (equal "ok" (gethash "status" result)) "an untracked file is enough to review")
             (is (= 1 (gethash "files_changed" result)) "one file")
             (is (equal "untracked" (gethash "status" (aref files 0))) "marked untracked")
             (is (= 1 (gethash "warning_count" result)) "and its print( was read")
             (is (equal "notes.py" (gethash "file" (aref (gethash "warnings" result) 0))) "under its own name")))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

;;; --- the branch's own commits ----------------------------------------------------

(deftest diff-review-cell-a-target-compares-against-the-merge-base ()
  (with-git (:diff +sample-diff+)
    (let ((result (review-answer "{\"target\": \"origin/main\"}")))
      (is (equal "ok" (gethash "status" result)) "the branch is reviewed")
      (let ((comparison (gethash "comparison" result)))
        (is (equal "merge_target" (gethash "mode" comparison)) "in the merge-target mode")
        (is (equal "origin/main" (gethash "target" comparison)) "against the target the call named")
        (is (stringp (gethash "merge_base" comparison)) "and from the common ancestor"))
      (let ((call (find-if (lambda (argv) (member "diff" (rest argv) :test #'equal)) *calls*)))
        (is (member "--quiet" call :test #'equal) "with the dirty check run beside it")
        (is (= 2 (count-if (lambda (argv) (member "diff" (rest argv) :test #'equal)) *calls*)) "one dirty check and one review")))))

;;; --- a git that complains --------------------------------------------------------

(deftest diff-review-cell-a-directory-outside-a-work-tree-says-so ()
  (let ((*calls* nil)
        (nodecode-diff-review::*run*
          (lambda (argv directory)
            (declare (ignore directory))
            (push argv *calls*)
            (if (member "--version" (rest argv) :test #'equal)
                (values "git version 2.43.0" "" 0)
                (values "" "fatal: not a git repository (or any of the parent directories): .git" 128)))))
    (with-cell-stop ((diff-review-start))
      (let ((result (review-answer "{}")))
        (is (equal "error" (gethash "status" result)) "no repository, no review")
        (is (equal "not_a_repository" (gethash "error_type" result)) "and the type says which")
        (is (search "not inside a git work tree" (gethash "message" result)) "and says which directory")))))

(deftest diff-review-cell-a-git-too-old-is-refused ()
  (with-git (:version "git version 2.20.1")
    (let ((result (review-answer "{}")))
      (is (equal "error" (gethash "status" result)) "2.20 is no 2.28")
      (is (search "2.28" (gethash "message" result)) "and the answer says what it needs"))))

;;; --- the operator's command, and what a message may carry ------------------------

(deftest diff-review-cell-the-command-summarizes-the-same-review ()
  (with-git (:diff +sample-diff+)
    (let ((text (nodecode-diff-review::run-command "" nil)))
      (is (search "1 file changed, +5 -1, 5 warnings" text) "the counts, in one line")
      (is (search "debug print statement" text) "then what was found")
      (is (search "app.py:2" text) "by file and line"))))

(deftest diff-review-cell-a-credential-in-a-message-is-masked ()
  (let ((text (nodecode-diff-review::shown "https://ghp_abcdefgh1234@example.invalid/x")))
    (is (search "***@example.invalid" text) "the userinfo is masked")
    (is (null (search "ghp_" text)) "and the token with it"))
  (let ((text (nodecode-diff-review::shown "https://user:secret@example.invalid/x")))
    (is (search "https://***@example.invalid" text) "a password in a URL goes too")))

(deftest diff-review-cell-the-manual-carries-the-review ()
  (with-cell-stop ((diff-review-start))
    (let ((text (cddr (assoc :diff-review nle::*help-topics*))))
      (is (search "diff_review" text) "the tool is named")
      (is (search "origin/main" text) "with the merge-target call spelled out")
      (is (search "diff_text" text) "and the way round a terminal this process cannot reach")
      (is (search "git 2.28" text) "and the git it needs"))))