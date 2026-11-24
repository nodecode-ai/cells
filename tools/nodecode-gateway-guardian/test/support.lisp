;;;; support.lisp --- the cell's test runner and its scratch log files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test here runs offline over real files: WITH-LOG-FILES makes a
;;;; scratch folder of its own, WRITE-LOG puts a log in it, and the cell is
;;;; started on the section that names them. No HTTP is involved and nothing
;;;; stubs the cell itself, so what the verbs read is what the tests wrote.

(in-package #:nodecode.test)

(define-test-slice "gateway-guardian" "GATEWAY-GUARDIAN-CELL-"
  :start nodecode-gateway-guardian:start-cell
  ;; The process scan reads /proc, and this run is not a gateway: every test
  ;; carries the switch off unless it says otherwise.
  :defaults (("watch_processes" nil)))

(defvar *log-dir* nil
  "This test's scratch folder of log files; WITH-LOG-FILES binds it.")

(defun test-log-dir ()
  "The folder this test writes its logs in, made once."
  (or *log-dir*
      (setf *log-dir* (uiop:ensure-directory-pathname
                        (merge-pathnames
                         (format nil "gateway-guardian-~a/" (string-downcase (symbol-name (gensym))))
                         (uiop:temporary-directory))))))

(defun write-log (name lines)
  "Write LINES to a fresh file called NAME in this test's folder; answer its path."
  (let ((path (merge-pathnames name (test-log-dir))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (dolist (line lines) (write-line line out)))
    (namestring path)))

(defmacro with-log-files (&body body)
  "BODY over a scratch folder of log files of its own, removed after."
  `(let ((*log-dir* (uiop:ensure-directory-pathname
                      (merge-pathnames
                       (format nil "gateway-guardian-~a/" (string-downcase (symbol-name (gensym))))
                       (uiop:temporary-directory)))))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree *log-dir* :validate t)))))

(defun call-tool (name &rest pairs)
  "NAME's registered body, called as the dispatcher calls it: one decoded
arguments object."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry) (apply #'nlk:make-json-object pairs))))

(defun answer (text)
  "TEXT decoded as a wire body is: a hash table with string keys."
  (nlk:decode-json text))
