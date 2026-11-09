;;;; support.lisp --- a scratch store per test, and a started cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Magi reads and writes files and nothing else, so what a test needs is a
;;;; store of its own: a fresh folder under the system temp directory, one
;;;; per config, so no two tests share MEMORY.md, and no test writes the
;;;; operator's own store. The lifecycle test starts the whole cell.

(in-package #:nodecode.test)

(defvar *magi-counter* 0
  "Bumped so every config answers a store of its own.")

(defun magi-scratch-store ()
  "A fresh, empty store folder, as a namestring."
  (let ((dir (merge-pathnames (format nil "nl-magi-~a-~a/" (sb-posix:getpid) (incf *magi-counter*))
                               #p"/tmp/")))
    (ensure-directories-exist dir)
    (namestring dir)))

(define-test-slice "magi" "MAGI-CELL-" :start nodecode-magi:start-cell
  :defaults (("store" (magi-scratch-store))))

(defun magi-store-file (store name)
  (merge-pathnames name (uiop:ensure-directory-pathname store)))

(defun magi-seed-store (store target &rest entries)
  "Write ENTRIES as TARGET's whole store under STORE."
  (let ((path (magi-store-file store (if (string-equal target "user") "USER.md" "MEMORY.md"))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                              :external-format :utf-8)
      (loop for (entry . rest) on entries
            do (write-string entry out)
               (when rest (write-string nodecode-magi::+delimiter+ out))))
    path))

(defun magi-store-entries (store &optional (target "memory"))
  "TARGET's entries as the file holds them."
  (let ((path (magi-store-file store (if (string-equal target "user") "USER.md" "MEMORY.md"))))
    (if (probe-file path) (nodecode-magi::split-entries (nlk:read-text path)) '())))

(defun magi-pending-files (store)
  (let ((dir (merge-pathnames "pending/memory/" (uiop:ensure-directory-pathname store))))
    (if (uiop:directory-exists-p dir) (directory (merge-pathnames "*.json" dir)) '())))

(defmacro with-magi ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (store from the slice's
default when PAIRS leave it out) and every store it wrote left behind."
  `(with-saved-globals (nle:*tools*)
     (with-cell-stop ((magi-start ,@pairs))
       ,@body)))
