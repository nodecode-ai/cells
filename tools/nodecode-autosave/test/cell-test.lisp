;;;; cell-test.lisp --- the lifecycle, the snapshot before an edit, restore, the shadow ref.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: that it starts and stops and
;;;; refuses a keep of zero; that the hook keeps the bytes a mutating form is
;;;; about to overwrite and nothing for a form that changes no file; that
;;;; restore puts a version back and a second one follows the first; that in a
;;;; work tree the snapshot is one shadow ref update per call, skipped when the
;;;; tree is unchanged, and never the real index; and that /autosave answers.

(in-package #:nodecode.test)

;;; --- lifecycle --------------------------------------------------------------

(define-cell-lifecycle-tests "autosave"
  (:hooks :tool)
  (:help :autosave)
  (:command "autosave")
  (:running (is (= 20 (nodecode-autosave::setting :keep)) "keep defaults to twenty")
            (is (eq t (nodecode-autosave::setting :git)) "git snapshots are on by default"))
  (:refused ("keep" 0) ("max_file_mb" 0)))

;;; --- what the hook keeps -------------------------------------------------------

(deftest autosave-cell-keeps-the-bytes-a-mutating-form-overwrites ()
  (with-autosave ()
    (let ((file (autosave-file "first")))
      (let ((nodecode-autosave::*git-run* (autosave-no-git)))
        (is (eq :ran (autosave-hook (format nil "(edit ~s \"first\" \"second\")" (namestring file)))) "the call ran")
        (is (= 1 (length (nodecode-autosave:versions (namestring file)))) "one version kept")
        (autosave-rewrite file "second")
        (is (search "restored" (nodecode-autosave:restore (namestring file))) "restore answers")
        (is (equal "first" (uiop:read-file-string file)) "the earlier bytes are back")))))

(deftest autosave-cell-keeps-nothing-for-a-form-that-changes-no-file ()
  (with-autosave ()
    (let ((file (autosave-file "kept")))
      (let ((nodecode-autosave::*git-run* (autosave-no-git)))
        (autosave-hook "(* 6 7)")
        (autosave-hook (format nil "(sh \"ls ~a\")" (namestring file)))
        (is (null (nodecode-autosave:versions (namestring file))) "no version for a read")
        (is (search "nothing held" (nodecode-autosave::list-text)) "the listing says so")))))

(deftest autosave-cell-keeps-a-version-per-edit ()
  (with-autosave ()
    (let ((file (autosave-file "one")))
      (let ((nodecode-autosave::*git-run* (autosave-no-git)))
        (autosave-hook (format nil "(write-file ~s \"two\")" (namestring file)))
        (autosave-rewrite file "two")
        (autosave-hook (format nil "(write-file ~s \"three\")" (namestring file)))
        (is (= 2 (length (nodecode-autosave:versions (namestring file)))) "two versions")
        (autosave-rewrite file "three")
        (nodecode-autosave:restore (namestring file) 2)
        (is (equal "one" (uiop:read-file-string file)) "version two is the first edit")))))

(deftest autosave-cell-a-credentials-file-is-not-copied ()
  (with-autosave ()
    (let ((file (autosave-file "SECRET=1")))
      (setf file (uiop:merge-pathnames* ".env" (uiop:pathname-directory-pathname file)))
      (autosave-rewrite file "SECRET=1")
      (let ((nodecode-autosave::*git-run* (autosave-no-git)))
        (autosave-hook (format nil "(write-file ~s \"SECRET=2\")" (namestring file)))
        (is (null (nodecode-autosave:versions (namestring file))) "a .env is left alone")))))

;;; --- the git shadow ref ----------------------------------------------------------

;;; --- the git shadow ref ----------------------------------------------------------

(defun autosave-git-answers (record &key (tree "tree-1"))
  "A git seam over a clean small repository holding TREE, with no shadow ref yet:
every command it answers is passed to RECORD."
  (autosave-fake-git
   (list (cons "rev-parse" (lambda (repo args output)
                              (declare (ignore repo output))
                              (cond ((member "--show-toplevel" args :test #'string=)
                                     (values "/autosave-work-tree/" 0))
                                    ((member "--verify" args :test #'string=)
                                     (values "" 1))
                                    (t (values "head-1" 0)))))
         (cons "add" (lambda (repo args output)
                       (declare (ignore repo args output))
                       (funcall record (list "add"))
                       (values "" 0)))
         (cons "write-tree" (lambda (repo args output)
                              (declare (ignore repo args output))
                              (funcall record (list "write-tree"))
                              (values tree 0)))
         (cons "commit-tree" (lambda (repo args output)
                               (declare (ignore repo args output))
                               (funcall record (list "commit-tree"))
                               (values "commit-1" 0)))
         (cons "update-ref" (lambda (repo args output)
                              (declare (ignore repo output))
                              (funcall record args)
                              (values "" 0))))))

(deftest autosave-cell-a-work-tree-gets-one-shadow-ref-update-a-call ()
  (with-autosave ("git" t)
    (let* ((file (autosave-file "work tree content"))
           (calls '())
           (nodecode-autosave::*git-run*
             (autosave-git-answers (lambda (call) (push call calls)))))
      (is (eq :ran (autosave-hook
                    (format nil "(edit ~s \"a\" \"b\") (edit ~s \"b\" \"c\")"
                            (namestring file) (namestring file)))) "the call ran")
      (is (= 1 (count-if (lambda (call) (equal (first call) "commit-tree")) calls)) "one snapshot for two edits of one repository")
      (is (find-if (lambda (call) (member "--create-reflog" call :test #'string=)) calls) "the ref is written with a reflog")
      (is (equal "" (ledger-text)) (ledger-text)))))

(deftest autosave-cell-an-unchanged-tree-is-not-snapshotted-again ()
  (with-autosave ("git" t)
    (let* ((file (autosave-file "same"))
           (calls '())
           ;; the shadow ref already holds the tree the work tree has
           (nodecode-autosave::*git-run*
             (autosave-fake-git
              (list (cons "rev-parse" (lambda (repo args output)
                                         (declare (ignore repo output))
                                         (if (member "--show-toplevel" args :test #'string=)
                                             (values "/autosave-work-tree/" 0)
                                             (values "tree-1" 0))))
                    (cons "add" (lambda (repo args output)
                                  (declare (ignore repo args output))
                                  (funcall (lambda (call) (push call calls)) (list "add"))
                                  (values "" 0)))
                    (cons "write-tree" (lambda (repo args output)
                                          (declare (ignore repo args output))
                                          (values "tree-1" 0)))
                    (cons "commit-tree" (lambda (repo args output)
                                           (declare (ignore repo args output))
                                           (push (list "commit-tree") calls)
                                           (values "commit-2" 0)))))))
      (autosave-hook (format nil "(write-file ~s \"x\")" (namestring file)))
      (is (null (find-if (lambda (call) (equal (first call) "commit-tree")) calls)) "nothing new to store, so no commit")
      (is (equal "" (ledger-text)) (ledger-text)))))

(deftest autosave-cell-slash-command-answers ()
  (with-autosave ()
    (let ((file (autosave-file "before")))
      (let ((nodecode-autosave::*git-run* (autosave-no-git)))
        (autosave-hook (format nil "(write-file ~s \"after\")" (namestring file)))
        (is (search "1 version" (nodecode-autosave::run-slash
                                 (format nil "find ~a" (namestring file)) "s1")) "find names the version")
        (is (search (uiop:native-namestring file) (nodecode-autosave::run-slash "list" "s1")) "list names the path")
        (is (search "restored" (nodecode-autosave::run-slash
                                (format nil "restore ~a" (namestring file)) "s1")) "restore from the operator's side")
        (is (equal "before" (uiop:read-file-string file)) "and it wrote the old bytes")
        (is (search "1 path" (nodecode-autosave::run-slash "" "s1")) "status counts it")))))
