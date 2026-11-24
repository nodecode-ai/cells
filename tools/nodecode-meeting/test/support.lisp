;;;; support.lisp --- the test slice: a scratch transcript directory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; MEETING-CELL- name prefix; RUN-MEETING-TESTS runs exactly that slice.
;;;;
;;;; The only seam that reaches outside the image is the file system, and a
;;;; test never touches the operator's: WITH-MEETING starts the cell on a
;;;; directory of the run's own, and removes it on unwind. Nothing here
;;;; dials anything.

(in-package #:nodecode.test)

(define-test-slice "meeting" "MEETING-CELL-" :start nodecode-meeting:start-cell)

;;; --- a transcript directory of this test's own --------------------------------------

(defun meeting-scratch-dir ()
  "A fresh directory under the run's temporary directory."
  (let ((dir (merge-pathnames
              (format nil "meeting-~a-~a/" (get-universal-time) (random 100000))
              (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-meeting ((&rest pairs) &body body)
  "Run BODY with the cell started on a scratch directory, stopped and removed on unwind."
  `(let ((dir (meeting-scratch-dir)))
     (unwind-protect
          (with-cell-stop ((meeting-start "dir" (namestring dir) ,@pairs))
            ,@body)
       (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

;;; --- one call through the real dispatch ---------------------------------------------

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(defun meeting-backdate (path seconds)
  "Make PATH look SECONDS older, so a staleness rule is tested without waiting."
  ;; The POSIX call counts seconds from 1970; FILE-WRITE-DATE counts them
  ;; from 1900. A test's staleness rule is worth the one subtraction.
  #+sbcl (let ((then (- (get-universal-time) 2208988800 seconds)))
           (sb-posix:utime (namestring path) then then))
  #-sbcl (declare (ignore path seconds))
  path)

;;; --- one call through the real dispatch ---------------------------------------------

(defun meeting-call-tool (name arguments)
  "NAME called as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))
