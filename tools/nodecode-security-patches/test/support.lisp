;;;; support.lisp --- the slice, and the two seams the tests stub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials or reads the machine: *FETCH* answers what a test
;;;; names instead of GitHub, and *RUN* answers a canned output per command
;;;; instead of running it.

(in-package #:nodecode.test)

(define-test-slice "security-patches" "SECURITY-PATCHES-"
  :start nodecode-security-patches:start-cell)

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
  `(with-saved-globals (nodecode-security-patches::*fetch* nodecode-security-patches::*run*
                        nodecode-security-patches::*feed* nodecode-security-patches::*feed-state*
                        nodecode-security-patches::*feed-url*)
     (setf *sp-fetch-answer* '(nil 599)
           *sp-outputs* '()
           nodecode-security-patches::*feed* nil
           nodecode-security-patches::*feed-state* (list :source :none)
           nodecode-security-patches::*fetch* (lambda (url) (declare (ignore url))
                                                (values-list *sp-fetch-answer*))
           nodecode-security-patches::*run* 'sp-run)
     ,@body))

(defun sp-check (json)
  "One check object from its JSON text."
  (shasht:read-json json))

(defun sp-scratch-dir (name)
  "A fresh empty directory under the temp folder."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "sp-~a-~d/" name (random 1000000))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun sp-write (dir relative text)
  (let ((path (merge-pathnames relative dir)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))
