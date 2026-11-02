;;;; inventory-test.lisp --- the person's own projects are found, checked, and named.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun sp-folder-names (folders root)
  (sort (mapcar (lambda (f) (enough-namestring f root)) folders) #'string<))

(defun sp-walk (root &rest options)
  (sp-folder-names (apply #'nodecode-cyber-security::walk-projects root options) root))

(defun sp-short (folder) (nodecode-cyber-security::short-folder folder))

(deftest cyber-security-projects-are-found-by-their-files ()
  (let ((root (sp-scratch-dir "walk")))
    (unwind-protect
         (progn
           (sp-write root "shop/package-lock.json" "{}")
           (sp-write root "api/composer.lock" "{}")
           (sp-write root "blog/wp-includes/version.php" "<?php")
           (sp-write root "repo/.git/HEAD" "ref: refs/heads/main")
           (sp-write root "shop/node_modules/left-pad/package.json" "{}")
           (sp-write root ".hidden/app/package.json" "{}")
           (sp-write root "notes/readme.txt" "no project here")
           (sp-write root "a/b/c/d/e/package.json" "{}")
           (uiop:run-program (list "ln" "-s" (namestring (merge-pathnames "shop/" root))
                                   (namestring (merge-pathnames "notes/link" root))))
           ;; Lockfiles, a WordPress core and a git repository mark a project;
           ;; dependency trees, hidden folders, links and folders past the depth do not.
           (is (equal '("api/" "blog/" "repo/" "shop/") (sp-walk root :depth 4)))
           (is (equal '("a/b/c/d/e/" "api/" "blog/" "repo/" "shop/") (sp-walk root :depth 5)) "the depth is a setting")
           ;; One folder read, the root, which is no project.
           (is (null (sp-walk root :depth 4 :budget (list 1))) "the folder budget stops the walk"))
      (uiop:delete-directory-tree root :validate t))))

(deftest cyber-security-a-venv-package-is-read-from-its-metadata ()
  (let ((root (sp-scratch-dir "venv"))
        (site ".venv/lib/python3.12/site-packages/"))
    (unwind-protect
         (progn
           (sp-write root (format nil "~amlflow-3.12.0.dist-info/METADATA" site)
                     (format nil "Metadata-Version: 2.1~%Name: mlflow~%Version: 3.12.0~%"))
           (sp-write root (format nil "~atyping_extensions-4.12.2.dist-info/METADATA" site)
                     (format nil "Name: typing_extensions~%Version: 4.12.2~%"))
           (is (equal "3.12.0" (nodecode-cyber-security::venv-package-version root "mlflow")))
           (let ((v (nodecode-cyber-security::venv-package-version root "Typing.Extensions")))
             (is (equal "4.12.2" v) "names compare the way pip compares them"))
           (is (null (nodecode-cyber-security::venv-package-version root "pillow")) "not installed there"))
      (uiop:delete-directory-tree root :validate t))))

(deftest cyber-security-the-report-checks-every-project-and-names-it (with-stubbed-security)
  (let ((root (sp-scratch-dir "stack")))
    (unwind-protect
         (let ((shop (merge-pathnames "shop/" root))
               (site (merge-pathnames "site/" root))
               (ml (merge-pathnames "ml/" root)))
           (sp-write shop "package-lock.json"
                     (concatenate 'string "{\"packages\":{\"node_modules/next\":{\"version\":\"15.5.20\"},"
                                  "\"node_modules/undici\":{\"version\":\"7.30.5\"}}}"))
           (sp-write site "package-lock.json" "{\"packages\":{\"node_modules/next\":{\"version\":\"15.5.28\"}}}")
           (sp-write ml ".venv/lib/python3.12/site-packages/mlflow-3.12.0.dist-info/METADATA"
                     (format nil "Name: mlflow~%Version: 3.12.0~%"))
           (let ((folders (mapcar #'namestring (list shop site ml))))
             (setf nodecode-cyber-security::*find-projects* (lambda () folders)))
           (let* ((rows (nodecode-cyber-security:check))
                  (next (find "nextjs-2026-10" rows :test #'equal :key (lambda (r) (getf r :id))))
                  (where (mapcar (lambda (c) (getf c :where)) (getf next :results)))
                  (text (nodecode-cyber-security:report)))
             (is (eq :vulnerable (getf next :status)) "one project on a vulnerable build is enough")
             (is (equal (list (namestring shop) (namestring site)) where) "each result names its project")
             (is (search (format nil "next in ~a: 15.5.20 < 15.5.28" (sp-short shop)) text))
             (is (search (format nil "already fixed in ~a (15.5.28)" (sp-short site)) text))
             ;; A Python package in the project's own virtualenv.
             (is (search (format nil "mlflow in ~a: 3.12.0 < 3.17.0" (sp-short ml)) text))
             (is (search "ok    HIGH undici-2026-10" text) "present at a fixed build, and shown as such")
             (is (search "and 3 projects:" text) "the second line says what was checked")
             (is (not (search "php2shell" text)) "no php here: not listed")
             (is (not (search "railway-2026-10-21" text)) "no sign of Railway here: not listed")
             (is (search "Not shown:" text))))
      (uiop:delete-directory-tree root :validate t))))

(deftest cyber-security-a-check-kind-from-a-newer-feed-is-skipped (with-stubbed-security)
  (let ((r (nodecode-cyber-security::run-check (sp-check "{\"kind\":\"cargo\",\"package\":\"zip\"}"))))
    (is (eq :skipped (getf r :status)) "an older copy skips what it cannot read")
    (is (search "newer copy" (getf r :why)))))
