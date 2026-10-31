;;;; support.lisp --- the slice, and the two seams the tests stub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials or reads the machine: *FETCH* answers what a test
;;;; names instead of GitHub, and *RUN* answers a canned output per command
;;;; instead of running it.

(in-package #:nodecode.test)

(define-test-slice "cyber-security" "CYBER-SECURITY-"
  :start nodecode-cyber-security:start-cell)

(defvar *sp-fetch-answer* '(nil 599)
  "(BODY STATUS) the stubbed feed fetch answers.")

(defvar *sp-outputs* '()
  "(PROGRAM-NAME OUTPUT EXIT) rows the stubbed runner answers, by argv's first word
or, for sh -c, by a substring of the script.")

(defun sp-run (argv)
  (let ((row (find-if (lambda (row)
                        (if (equal (first argv) "sh")
                            (and (uiop:string-prefix-p "sh:" (first row))
                                 (search (subseq (first row) 3) (or (third argv) "")))
                            (equal (first row) (first argv))))
                      *sp-outputs*)))
    (if row (values (second row) (third row)) (values "" 127))))

(defmacro with-stubbed-security (&body body)
  "BODY with the feed fetch and the command runner stubbed, and the feed state fresh."
  `(with-saved-globals (nodecode-cyber-security::*fetch* nodecode-cyber-security::*run*
                        nodecode-cyber-security::*feed* nodecode-cyber-security::*feed-state*
                        nodecode-cyber-security::*feed-url* nodecode-cyber-security::*pinned*)
     (setf *sp-fetch-answer* '(nil 599)
           *sp-outputs* '()
           *sp-ran* '()
           nodecode-cyber-security::*feed* nil
           nodecode-cyber-security::*feed-state* (list :source :none)
           nodecode-cyber-security::*fetch* (lambda (url) (declare (ignore url))
                                                (values-list *sp-fetch-answer*))
           nodecode-cyber-security::*run* 'sp-run)
     ,@body))

(defvar *sp-ran* '() "Every argv the stubbed runner was handed, newest first.")

(defun sp-run-logged (argv)
  (push argv *sp-ran*)
  (sp-run argv))

(defun sp-pin (&rest checks)
  "This copy vouches for CHECKS' commands too, as if a release had pinned
them beside the cell: the fixtures stand in for the shipped feed, which
changes every six hours."
  (let ((set (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k set) v)) (nodecode-cyber-security::pinned-commands))
    (dolist (c checks)
      (dolist (key '("command" "backport"))
        (let ((words (nodecode-cyber-security:field c key)))
          (when words (setf (gethash words set) t)))))
    (setf nodecode-cyber-security::*pinned* (lambda () set))))

(defun sp-check (json)
  "One check object from its JSON text."
  (shasht:read-json json))

(defun sp-scratch-dir (name)
  "A fresh empty directory under the temp folder."
  (let ((dir (uiop:ensure-directory-pathname
              ;; A fresh random state: the default one is the same in every
              ;; fresh image, so every run named the same folder, and a run
              ;; under another account found it already there and not its own.
              (merge-pathnames (format nil "sp-~a-~36r/" name
                                       (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun sp-write (dir relative text)
  (let ((path (merge-pathnames relative dir)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))
