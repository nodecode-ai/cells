;;;; support.lisp --- the sluice test slice and its scratch workspace.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(define-test-slice "sluice" "SLUICE-" :start nodecode-sluice:start-cell)

(defvar *calls* nil
  "The (COMMAND DIRECTORY) pairs the run stub was handed, newest first.")

(defun stub-run (output exit)
  "A run stub: records the pair and answers OUTPUT with EXIT."
  (lambda (command directory)
    (push (list command directory) *calls*)
    (values output exit)))

(defun make-workspace (&rest files)
  "A fresh folder holding FILES, each (NAME TEXT). => its namestring."
  (let ((dir (format nil "~asluice-~d/"
                      (uiop:native-namestring (uiop:temporary-directory))
                      (random 1000000))))
    (ensure-directories-exist dir)
    (dolist (file files)
      (let ((path (format nil "~a~a" dir (car file))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create :external-format :utf-8)
          (write-string (second file) out))))
    dir))

(defmacro with-workspace ((variable &rest files) &body body)
  "BODY with VARIABLE bound to a fresh workspace holding FILES, and the cell
started on it; the folder and the stage go away with the body."
  `(let* ((,variable (make-workspace ,@(mapcar (lambda (file) `(list ,@file)) files)))
          (workspace ,variable))
     (with-cell-stop ((sluice-start "workspace" workspace))
       (unwind-protect (progn ,@body)
         (ignore-errors (uiop:delete-directory-tree
                         (uiop:ensure-directory-pathname workspace) :validate t))
         (ignore-errors (nodecode-sluice::discard-stage (nodecode-sluice::ensure-stage*)))))))

(defun tool-answer (tool arguments)
  "TOOL called the way the model calls it => the text it reads back."
  (nle::message-content (execute-wire-call "sluice-test" tool arguments)))
