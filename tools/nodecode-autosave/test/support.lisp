;;;; support.lisp --- autosave test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Autosave tests register into the shared nodecode.test registry under an
;;;; AUTOSAVE-CELL- name prefix; RUN-AUTOSAVE-TESTS runs exactly that slice.
;;;; No test needs a git binary or a repository: the cell's one git seam,
;;;; *GIT-RUN*, is bound to a fake that answers, and everything else is a
;;;; scratch directory under the temporary one.

(in-package #:nodecode.test)

(define-test-slice "autosave" "AUTOSAVE-CELL-" :start nodecode-autosave:start-cell)

(defvar *autosave-root* nil
  "The scratch data directory the running test's cell writes to.")

(defmacro with-autosave ((&rest pairs) &body body)
  "BODY with the cell started on a fresh data directory of its own and the
config PAIRS it is given, and stopped on unwind."
  `(let ((*autosave-root* (merge-pathnames
                            (format nil "autosave-~d-~d/" (get-universal-time) (random 1000000))
                            (uiop:temporary-directory))))
     (ensure-directories-exist *autosave-root*)
     (unwind-protect
          (with-cell-stop ((autosave-start "storage_dir" (uiop:native-namestring *autosave-root*)
                                           ,@pairs))
            ,@body)
       (ignore-errors (uiop:delete-directory-tree *autosave-root* :validate t
                                                  :if-does-not-exist :ignore)))))

(defun autosave-file (content)
  "A scratch file under the running test's directory, holding CONTENT."
  (let ((file (merge-pathnames (format nil "file-~d-~d.txt" (get-universal-time) (random 1000000))
                                *autosave-root*)))
    (autosave-rewrite file content)))

(defun autosave-rewrite (file content)
  "Put CONTENT in FILE and answer FILE."
  (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string content out))
  file)

(defun autosave-hook (form &rest strings)
  "The installed :tool hook run on an eval call of FORM; => what NEXT answered."
  (let ((args (nlk:json-object "form" form)))
    (when strings
      (let ((object (nlk:json-object)))
        (loop for (name text) on strings by #'cddr do (setf (gethash name object) text))
        (setf (gethash "strings" args) object)))
    (funcall (cell-hook "nodecode-autosave" :tool)
             (list :name "eval" :arguments args :call-id "c1")
             (lambda (op) (declare (ignore op)) :ran))))

(defun autosave-no-git ()
  "A stand-in for the cell's git seam that answers every question with failure,
so a path is never in a work tree."
  (lambda (repo args &key environment output)
    (declare (ignore repo args environment output))
    (values "" 1)))

(defun autosave-fake-git (answers)
  "A stand-in for the cell's git seam: ANSWERS maps a subcommand name to a
function of (REPO ARGS OUTPUT) answering (values TEXT CODE)."
  (lambda (repo args &key environment output)
    (declare (ignore environment))
    (let ((answer (cdr (assoc (first args) answers :test #'string=))))
      (if answer
          (funcall answer repo args output)
          (values "" 1)))))

(defun ledger-text ()
  "What the running test's cell wrote to its ledger, or an empty string."
  (let ((file (merge-pathnames "ledger.txt" *autosave-root*)))
    (if (uiop:file-exists-p file) (uiop:read-file-string file) "")))

