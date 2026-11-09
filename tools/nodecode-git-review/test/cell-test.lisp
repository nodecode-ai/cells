;;;; cell-test.lisp --- the git review cell against a stubbed git.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the lifecycle every cell owns; that one review call
;;;; answers the branch, the per-file churn and the patch, with an untracked
;;;; file rendered as its own hunk; that a long patch is cut with a note; that
;;;; a path outside the repository, an unknown scope and a commit with no
;;;; message are refused; that a commit stages, commits with hooks off and
;;;; reports each step; that read_only refuses before git runs at all; and
;;;; that /review answers the same text. Nothing here needs git installed.

(in-package #:nodecode.test)

(define-test-slice "git-review" "GIT-REVIEW-CELL-" :start nodecode-git-review:start-cell)

;;; --- the stubbed process seam --------------------------------------------------------

(defvar *answers* nil
  "Canned git answers, in order of the search: (SUBSTRING OUTPUT EXIT-CODE).")
(defvar *calls* nil
  "Every command the cell ran, newest first: one line a call.")

(defun stub-answer (dir program args)
  (declare (ignore dir))
  (let ((line (format nil "~a ~{~a~^ ~}" program args)))
    (push line *calls*)
    (dolist (entry *answers* (values "" 0))
      (when (search (first entry) line)
        (return-from stub-answer (values (second entry) (third entry)))))))

(defmacro with-git-stub ((&rest answers) &body body)
  `(let ((*answers* (list ,@(mapcar (lambda (answer) `(list ,@answer)) answers)))
         (*calls* '())
         (nodecode-git-review::*run*
           (lambda (dir program args timeout)
             (declare (ignore timeout))
             (stub-answer dir program args))))
     ,@body))

(defun scratch-repo (&optional files)
  "A fresh directory, with FILES written into it: (NAME . CONTENT) ..."
  (let ((dir (merge-pathnames (format nil "git-review-~a/" (random 10000000))
                               (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    (dolist (file files)
      (let ((path (merge-pathnames (car file) dir)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
          (write-string (cdr file) out))))
    (namestring dir)))

(defun some-call (needle)
  (some (lambda (line) (and (search needle line) t)) *calls*))

(defparameter *sample-patch*
  "diff --git a/src/app.lisp b/src/app.lisp
--- a/src/app.lisp
+++ b/src/app.lisp
@@ -1,3 +1,4 @@
 line one
-line two
+line two, changed
+line three
"
  "One changed file, plus two lines and minus one.")

(defun object-of (pairs)
  "A JSON object from alternating key/value PAIRS."
  (let ((object (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun review-request (&rest pairs)
  (nlk:contract-decode (nlk:contract 'nodecode-git-review::git-review-request)
                        (object-of pairs)))

(defun commit-request (&rest pairs)
  (nlk:contract-decode (nlk:contract 'nodecode-git-review::git-commit-request)
                        (object-of pairs)))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "git-review"
  (:running (is (= 60000 (nodecode-git-review::setting :max-chars))
                "the patch ceiling defaults to 60000 characters")
            (is (eq t (nodecode-git-review::setting :new-files)) "new files are shown")
            (is (string= "" (nodecode-git-review::setting :base)) "and no base is named"))
  (:refused ("max_chars" 10))
  (:idle nodecode-git-review::git-review-error
         (nodecode-git-review:review :repo "somewhere")))

;;; --- reading a change ----------------------------------------------------------------

(deftest git-review-cell-reads-a-change ()
  (with-cell-stop ((git-review-start))
    (let* ((repo (scratch-repo '(("new.txt" . "hello
new
"))))
           (text (with-git-stub (("rev-parse --show-toplevel" repo 0)
                                 ("rev-parse --verify" "abc1234" 0)
                                 ("status --porcelain" "## feature/one...origin/main
 M src/app.lisp
?? new.txt
" 0)
                                 ("ls-files --others" "new.txt
" 0)
                                 ("diff" *sample-patch* 0))
                   (nodecode-git-review::review-tool (review-request "repo" repo)))))
      (is (search "scope uncommitted: the working tree, against HEAD" text))
      (is (search "branch feature/one" text) "the checked-out branch is in the header")
      (is (search "+2 -1  src/app.lisp" text) "churn is counted from the patch itself")
      (is (search "M" text) "the file's status is carried")
      (is (search "patch:" text) "the patch follows the summary")
      (is (search "+line two, changed" text) "the changed line is in the text")
      (is (search "+hello" text) "an untracked file is shown as its own hunk"))))

(deftest git-review-cell-a-long-patch-is-cut ()
  (with-cell-stop ((git-review-start "max_chars" 2000))
    (let* ((repo (scratch-repo))
           (long (with-output-to-string (out)
                   (format out "diff --git a/big.txt b/big.txt~%--- a/big.txt~%+++ b/big.txt~%")
                   (dotimes (i 200) (format out "+line ~d of a long patch~%" i))))
           (text (with-git-stub (("rev-parse --show-toplevel" repo 0)
                                 ("rev-parse --verify" "abc1234" 0)
                                 ("status --porcelain" "## main
 M big.txt
" 0)
                                 ("ls-files --others" "" 0)
                                 ("diff" long 0))
                   (nodecode-git-review::review-tool (review-request "repo" repo)))))
      (is (search "[patch cut here:" text) "a patch past the ceiling is cut, and says so")
      (is (< (length text) (+ 2000 400)) "and the answer stays near the ceiling"))))

(deftest git-review-cell-files-only-skips-the-patch ()
  (with-cell-stop ((git-review-start))
    (let* ((repo (scratch-repo))
           (text (with-git-stub (("rev-parse --show-toplevel" repo 0)
                                 ("rev-parse --verify" "abc1234" 0)
                                 ("status --porcelain" "## main
 M src/app.lisp
" 0)
                                 ("ls-files --others" "" 0)
                                 ("diff" *sample-patch* 0))
                   (nodecode-git-review::review-tool
                    (review-request "repo" repo "files_only" t)))))
      (is (search "src/app.lisp" text) "the file list is there")
      (is (search "files_only" text) "and the patch says it is not shown")
      (is (not (search "+line two, changed" text)) "no diff body"))))

;;; --- what is refused ----------------------------------------------------------------

(deftest git-review-cell-refuses-a-path-outside-the-repository ()
  (with-cell-stop ((git-review-start))
    (let* ((repo (scratch-repo))
           (text (with-git-stub (("rev-parse --show-toplevel" repo 0))
                   (nodecode-git-review::review-tool
                    (review-request "repo" repo "path" "../outside.txt")))))
      (is (search "ERROR:" text) "the refusal is a failed result")
      (is (search "is not a repository-relative path" text)))))

(deftest git-review-cell-refuses-an-unknown-scope ()
  (is (signals-error nlk:contract-refusal
        (review-request "scope" "everything"))))

(deftest git-review-cell-a-commit-needs-a-message ()
  (is (signals-error nlk:contract-refusal
        (commit-request "repo" "/tmp"))))

;;; --- the write path -----------------------------------------------------------------

(deftest git-review-cell-commits-and-reports-each-step ()
  (with-cell-stop ((git-review-start))
    (let ((repo (scratch-repo)))
      (with-git-stub (("rev-parse --show-toplevel" repo 0)
                      ("add -A" "" 0)
                      ("commit --no-verify" "[main 1a2b3c4] fix the totals" 0))
        (let ((report (nodecode-git-review::commit-tool
                       (commit-request "repo" repo "message" "fix the totals"))))
          (is (search "staged everything" report) "the first step is reported")
          (is (search "committed: [main 1a2b3c4] fix the totals" report) "and the commit")
          (is (some-call "commit --no-verify -m fix the totals")
              "hooks are off on the commit the cell makes"))))))

(deftest git-review-cell-read-only-refuses-before-git-runs ()
  (with-cell-stop ((git-review-start "read_only" t))
    (let* ((repo (scratch-repo))
           (report (with-git-stub ()
                     (nodecode-git-review::commit-tool
                      (commit-request "repo" repo "message" "fix the totals")))))
      (is (search "ERROR: read_only is on" report) "the write is refused")
      (is (null *calls*) "and git never ran"))))

;;; --- the command --------------------------------------------------------------------

(deftest git-review-cell-the-command-reads-a-scope ()
  (with-cell-stop ((git-review-start))
    (let* ((repo (scratch-repo))
           (text (with-git-stub (("rev-parse --show-toplevel" repo 0)
                                 ("status --porcelain" "## main
 M src/app.lisp
" 0)
                                 ("--cached" *sample-patch* 0))
                   (nodecode-git-review::review-command "staged" "s1"))))
      (is (search "scope staged: the index, against HEAD" text))
      (is (search "src/app.lisp" text) "the staged file is named"))))
