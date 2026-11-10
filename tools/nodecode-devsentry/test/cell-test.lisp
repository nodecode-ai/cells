;;;; cell-test.lisp --- the devsentry cell against a scratch folder and a stubbed git.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The inventory and the trace are read off a folder this file makes and
;;;; removes; every git run goes through NODECODE-DEVSENTRY::*RUN*, a stub that
;;;; records the command line and answers what a repository would. No test needs
;;;; a repository, a git install or the network.

(in-package #:nodecode.test)

(define-test-slice "devsentry" "DEVSENTRY-CELL-" :start nodecode-devsentry:start-cell)

(define-cell-lifecycle-tests "devsentry"
  (:help :devsentry)
  (:command "devsentry")
  (:running
   (is (nle::find-tool "inspect_project") "the inventory tool is registered")
   (is (nle::find-tool "diagnose_error") "the trace tool is registered")
   (is (nle::find-tool "review_changes") "the diff tool is registered")
   (is (equal 20 (nodecode-devsentry::setting :git-timeout-seconds)) "the section's defaults are the values it runs on"))
  (:stopped
   (is (null (nle::find-tool "inspect_project")) "stop takes the tools back")
   (is (null (nle::find-tool "review_changes")) "all of them"))
  (:refused ("max_depth" 0) ("git_timeout_seconds" 0) ("context_lines" -1) ("max_patch_bytes" 1)))

;;; --- the scratch folder, and calling a tool the way the model does ---------------

(defmacro with-scratch ((variable) &body body)
  "BODY with VARIABLE a fresh empty directory, removed afterwards."
  `(let ((,variable (merge-pathnames (format nil "devsentry-~a-~a/" (get-universal-time) (random most-positive-fixnum))
                                     (uiop:temporary-directory))))
     (ensure-directories-exist ,variable)
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,variable :validate t)))))

(defun put-file (directory name text)
  "TEXT written at NAME under DIRECTORY, its parent folders made."
  (let ((path (merge-pathnames name directory)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))
    path))

(defun call-tool (name arguments)
  "NAME called the way the model calls it => the text it reads back."
  (nle::message-content (execute-wire-call (format nil "devsentry-call-~a" name) name (nlk:encode-json-object arguments))))

(defun answer-of (name &rest members)
  "NAME called with MEMBERS as key/value pairs, its JSON answer decoded."
  (nlk:decode-json (call-tool name (apply #'nlk:make-json-object members))))

(defun trimmed-namestring (pathname)
  "PATHNAME's native name, its trailing separator taken off."
  (string-right-trim "/" (uiop:native-namestring (truename pathname))))

;;; --- the boundary ------------------------------------------------------------------

(deftest devsentry-cell-a-call-must-name-its-directory ()
  (is (signals-error nlk:contract-refusal (nlk:contract-decode (nlk:contract 'nodecode-devsentry::inspect-project) (nlk:make-json-object "max_depth" 3))) "inspect_project refuses a call without a path")
  (is (signals-error nlk:contract-refusal (nlk:contract-decode (nlk:contract 'nodecode-devsentry::review-changes) (nlk:make-json-object "context_lines" 5))) "and so does review_changes")
  (is (signals-error nlk:contract-refusal (nlk:contract-decode (nlk:contract 'nodecode-devsentry::diagnose-error) (nlk:make-json-object "project_path" "/tmp"))) "a diagnosis without the trace"))

;;; --- the inventory -----------------------------------------------------------------

(deftest devsentry-cell-an-inventory-reads-names-not-contents ()
  (with-cell-stop ((devsentry-start))
    (with-scratch (root)
      (put-file root "src/main.py" "print('hi')\n")
      (put-file root "README.md" "# notes\n")
      (put-file root ".env" "TOKEN=secret\n")
      (put-file root "node_modules/left-pad/index.js" "module.exports = 1\n")
      (let* ((answer (answer-of "inspect_project" "path" (uiop:native-namestring root)))
             (summary (gethash "summary" answer))
             (paths (loop for entry across (gethash "entries" answer) collect (gethash "path" entry)))
             (languages (loop for row across (gethash "languages" answer) collect (cons (gethash "language" row) (gethash "files" row))))
             (dotenv (find ".env" (gethash "entries" answer) :test #'equal :key (lambda (entry) (gethash "path" entry)))))
        (is (gethash "ok" answer) "the inventory answers")
        (is (= 3 (gethash "files" summary)) "three files outside the dependency folder: main.py, README.md and .env")
        (is (not (member "node_modules" paths :test #'equal)) "a dependency folder is not entered")
        (is (find "node_modules" (gethash "skipped" answer) :test #'equal :key (lambda (folder) (gethash "path" folder))) "and it is named as skipped")
        (is (member (cons "Python" 1) languages :test #'equal) "the .py is counted as Python")
        (is (gethash "sensitive" dotenv) "a dotenv file is marked")
        (is (equal "not_read" (gethash "content" dotenv)) "and its contents were not read")
        (is (equal (trimmed-namestring root) (gethash "resolved" (gethash "path" answer))) "the answer says which directory it read")))))

(deftest devsentry-cell-an-inventory-refuses-a-missing-folder ()
  (with-cell-stop ((devsentry-start))
    (with-scratch (root)
      (let ((missing (answer-of "inspect_project" "path" (namestring (merge-pathnames "nowhere/" root))))
            (file (answer-of "inspect_project" "path" (namestring (put-file root "one.txt" "x")))))
        (is (not (gethash "ok" missing)) "a folder that is not there is refused")
        (is (equal "not_found" (gethash "code" (gethash "error" missing))) "with the code that says which")
        (is (equal "not_a_directory" (gethash "code" (gethash "error" file))) "and a file where a directory was expected")))))

;;; --- the trace ----------------------------------------------------------------------

(defparameter +trace+
  (format nil "~{~a~%~}" (list "Traceback (most recent call last):"
                                "  File \"src/main.py\", line 2, in <module>"
                                "    run()"
                                "  File \"src/main.py\", line 7, in run"
                                "    1 / 0"
                                "ZeroDivisionError: division by zero"))
  "A Python traceback citing one file inside the project, twice.")

(deftest devsentry-cell-a-trace-becomes-frames-and-leads ()
  (with-cell-stop ((devsentry-start))
    (with-scratch (root)
      (put-file root "src/main.py" (format nil "def run():~%    return 1 / 0~%~%run()~%"))
      (let* ((answer (answer-of "diagnose_error" "trace" +trace+ "project_path" (uiop:native-namestring root)))
             (frames (gethash "frames" answer))
             (exceptions (gethash "exceptions" answer))
             (context (gethash "context" (first (coerce frames 'list))))
             (cited (gethash "snippet" context)))
        (is (gethash "ok" answer) "the trace is read")
        (is (equal "ZeroDivisionError" (gethash "name" (first (coerce exceptions 'list)))) "the exception name is found")
        (is (= 2 (length frames)) "both frames are found")
        (is (equal "python" (gethash "style" (first (coerce frames 'list)))) "as Python frames")
        (is (equal 2 (gethash "line" (first (coerce frames 'list)))) "with their line numbers")
        (is (search "divisor" (first (coerce (gethash "leads" answer) 'list))) "and a lead a person can check")
        (is (equal "leads_only" (gethash "certainty" answer)) "said to be leads, not a cause")
        (is (find 2 cited :test #'= :key (lambda (row) (gethash "line" row))) "the cited line is quoted from the project")))))

(deftest devsentry-cell-a-trace-says-which-file-it-could-not-read ()
  (with-cell-stop ((devsentry-start))
    (with-scratch (root)
      (let* ((answer (answer-of "diagnose_error" "trace" +trace+ "project_path" (uiop:native-namestring root)))
             (context (gethash "context" (first (coerce (gethash "frames" answer) 'list)))))
        (is (gethash "omitted" context) "a cited file that is not there is not quoted")
        (is (equal "not_in_project" (gethash "reason" context)) "and the answer says why")))))

;;; --- the diff, over a stubbed git ----------------------------------------------------

(defvar *git-calls* nil "The git command lines the stub was handed, newest first.")

(defun nul-list (&rest pieces)
  "PIECES joined with a NUL between them, as git's -z output is."
  (with-output-to-string (out)
    (dolist (piece pieces)
      (write-string piece out)
      (write-char #\Null out))))

(defun stub-git (toplevel &key (sha "1111111111111111111111111111111111111111"))
  "A git stub answering what a repository at TOPLEVEL would, recording each call."
  (lambda (argv directory)
    (declare (ignore directory))
    (push argv *git-calls*)
    (let ((arguments (rest argv)))
      (cond
        ((member "--is-inside-work-tree" arguments :test #'equal) (values (format nil "true~%") "" 0))
        ((member "--show-toplevel" arguments :test #'equal) (values (format nil "~a~%" toplevel) "" 0))
        ((member "--absolute-git-dir" arguments :test #'equal) (values (format nil "~a/.git~%" toplevel) "" 0))
        ((member "HEAD" arguments :test #'equal) (values (format nil "~a~%" sha) "" 0))
        ((member "ls-files" arguments :test #'equal) (values (nul-list "notes.txt") "" 0))
        ((and (member "diff" arguments :test #'equal) (member "--name-status" arguments :test #'equal))
         (if (member "--cached" arguments :test #'equal)
             (values (nul-list "M" "src/app.py" "A" ".env") "" 0)
             (values (nul-list "M" "src/app.py" "M" "logo.png") "" 0)))
        ((member "diff" arguments :test #'equal)
         (if (equal (car (last arguments)) "logo.png")
             (values (format nil "Binary files a/logo.png and b/logo.png differ~%") "" 0)
             (values (format nil "@@ -1 +1 @@~%+print(1)~%") "" 0)))
        (t (values "" "" 0))))))

(deftest devsentry-cell-a-review-names-both-scopes-and-omits-secrets ()
  (with-scratch (root)
    (let* ((toplevel (trimmed-namestring root))
           (*git-calls* nil)
           (nodecode-devsentry::*run* (stub-git toplevel)))
      (with-cell-stop ((devsentry-start))
        (let* ((answer (answer-of "review_changes" "path" (uiop:native-namestring root)))
               (staged (gethash "staged" answer))
               (unstaged (gethash "unstaged" answer))
               (staged-files (coerce (gethash "files" staged) 'list))
               (unstaged-files (coerce (gethash "files" unstaged) 'list))
               (dotenv (find ".env" staged-files :test #'equal :key (lambda (entry) (gethash "path" entry))))
               (png (find "logo.png" unstaged-files :test #'equal :key (lambda (entry) (gethash "path" entry)))))
          (is (gethash "ok" answer) "the review answers")
          (is (= 2 (gethash "file_count" staged)) "the index holds two tracked changes")
          (is (equal "sensitive_path" (gethash "reason" dotenv)) "a dotenv file carries no patch")
          (is (null (gethash "patch" dotenv)) "not one line of it")
          (is (equal "binary" (gethash "reason" png)) "a binary file says so")
          (is (search "+print(1)" (gethash "patch" (first staged-files))) "a text patch is carried")
          (is (= 1 (gethash "untracked_count" answer)) "untracked files are counted")
          (is (gethash "index_sha256_unchanged" (gethash "mutation" answer)) "and the index did not move")
          (is (member "--no-optional-locks" (first *git-calls*) :test #'equal) "git runs with optional locks off")
          (is (some (lambda (argv) (member "core.fsmonitor=" argv :test #'equal)) *git-calls*) "and with fsmonitor overridden"))))))

(deftest devsentry-cell-a-review-refuses-a-directory-that-is-not-a-repository ()
  (with-scratch (root)
    (let ((*git-calls* nil)
          (nodecode-devsentry::*run* (lambda (argv directory)
                                        (declare (ignore argv directory))
                                        (values "" "fatal: not a git repository" 128))))
      (with-cell-stop ((devsentry-start))
        (let ((error (gethash "error" (answer-of "review_changes" "path" (uiop:native-namestring root)))))
          (is (equal "not_a_git_repository" (gethash "code" error)) "a plain folder is refused, with the reason git gave"))))))

;;; --- the operator's own look ---------------------------------------------------------

(deftest devsentry-cell-the-command-reads-a-folder ()
  (with-cell-stop ((devsentry-start))
    (with-scratch (root)
      (put-file root "src/main.py" "print(1)")
      (let ((text (nodecode-devsentry::run-command (uiop:native-namestring root) nil)))
        (is (search "devsentry:" text) "the command answers a line about the folder")
        (is (search (trimmed-namestring root) text) "naming the directory it read")
        (is (search "Python" text) "and the languages it found"))
      (is (search "does not exist" (nodecode-devsentry::run-command "/no/such/place" nil)) "a folder that is not there is answered in words"))))
