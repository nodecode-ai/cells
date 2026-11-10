;;;; support.lisp --- the recued test slice, and the config it stubs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Recued tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a RECUED-CELL-
;;;; name prefix; RUN-RECUED-TESTS runs exactly that slice.
;;;;
;;;; The one thing stubbed is the shared config and this computer's PATH: a
;;;; test run must not write the operator's config.jsonc, and no test here
;;;; starts a process. Everything else is the cell's own code: the section it
;;;; reads, the entry it builds, the lines /recued answers.

(in-package #:nodecode.test)

(define-test-slice "recued" "RECUED-CELL-" :start nodecode-recued:start-cell)

(defvar *recued-servers* nil
  "The mcp.servers table the stubs answer from, as (NAME . ENTRY) pairs.")

(defvar *recued-on-path-p* t
  "Whether the stub's PATH probe finds the Recued command.")

(defun recued-read (path)
  "The config reader the fixture installs: what NLE:CONFIG-GET answers at PATH."
  (cond ((equal path '("mcp" "servers"))
         (loop for (name . entry) in *recued-servers*
               append (list (intern name :keyword) entry)))
        ((and (consp path) (= 3 (length path)) (equal (subseq path 0 2) '("mcp" "servers")))
         (cdr (assoc (third path) *recued-servers* :test #'equal)))
        (t nil)))

(defun recued-write (path value)
  "The config writer the fixture installs: what NLE:CONFIG-SET does at PATH, as one line."
  (let* ((name (third path))
         (old (cdr (assoc name *recued-servers* :test #'equal))))
    (if (eq value :remove)
        (progn
          (setf *recued-servers* (remove name *recued-servers* :key #'first :test #'equal))
          (format nil "mcp.servers.~a: removed (1 removed, 0 replaced)" name))
        (let ((pairs (loop for (key member) on value by #'cddr collect key collect member)))
          (setf *recued-servers*
                (acons name pairs (remove name *recued-servers* :key #'first :test #'equal)))
          (format nil "mcp.servers.~a: set ~{~(~a~)~^, ~} (~d added, ~d replaced)" name
                  (loop for (key member) on value by #'cddr collect key)
                  (loop for (key member) on value by #'cddr
                        unless (and old (member key old)) count key)
                  (loop for (key member) on value by #'cddr
                        count (and old (member key old))))))))

(defun recued-locate (name)
  "The stub's PATH probe: a path for NAME when *RECUED-ON-PATH-P*, else nothing."
  (and *recued-on-path-p* (stringp name) (plusp (length name))
       (format nil "/usr/local/bin/~a" name)))

(defmacro with-recued-seams (&body body)
  "BODY over an empty mcp.servers table, a PATH that has the Recued command, and
no operator config written: the three seams the cell reads through, stubbed."
  `(let ((*recued-servers* nil)
         (*recued-on-path-p* t)
         (nodecode-recued::*config-reader* #'recued-read)
         (nodecode-recued::*config-writer* #'recued-write)
         (nodecode-recued::*command-locator* #'recued-locate))
     ,@body))

(defun recued-entry (&optional (name "recued"))
  "The entry the config holds for NAME, as the stub table has it."
  (cdr (assoc name *recued-servers* :test #'equal)))
