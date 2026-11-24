;;;; support.lisp --- cell-generator test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CELL-GENERATOR-
;;;; name prefix; RUN-CELL-GENERATOR-TESTS runs exactly that slice.
;;;;
;;;; Every test writes into a folder of its own: WITH-CELLS starts the cell
;;;; with its section's DIRECTORY pointing there, so nothing the generator
;;;; writes lands in a home. The one thing a test loads is what it just
;;;; generated -- an .asd and its system, read by this image's ASDF, exactly
;;;; as a boot would read it.

(in-package #:nodecode.test)

(define-test-slice "cell-generator" "CELL-GENERATOR-"
  :start nodecode-cell-generator:start-cell)

(defmacro with-cells ((&rest pairs) &body body)
  "BODY with the cell started, its cells folder a fresh temp directory that
the generated folders land in -- the rest of the section from PAIRS -- and
the whole thing stopped on unwind."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir "cells")
       (with-cell-stop ((cell-generator-start "directory" (namestring ,dir) ,@pairs))
         ,@body))))

(defun generated-folder (name)
  "The folder cell NAME was scaffolded into, as the cell's own section says."
  (merge-pathnames (format nil "nodecode-~a/" name)
                    (nodecode-cell-generator::cell-directory)))

(defun generated-file (name file)
  "The FILE of the cell NAME."
  (let ((path (merge-pathnames file (generated-folder name))))
    (unless (probe-file path)
      (error "~a was not written" (namestring path)))
    path))

(defun answer-member (answer key)
  "ANSWER, a tool's JSON string, read back for KEY."
  (nlk:json-value (nlk:decode-json answer) :string key))

(defun check-reads (path)
  "Every toplevel form of PATH through the reader, the way a load reads it;
=> how many it read, or a refusal carrying the file's tail."
  (with-open-file (in path :external-format :utf-8)
    (loop for form = (handler-case (read in nil :eof)
                       (error (e)
                         (let ((text (uiop:read-file-string path)))
                           (error "~a does not read (~a); it is~%~a"
                                  (file-namestring path) e text))))
          until (eq form :eof)
          count 1)))

(defun load-generated-cell (name &key test-system)
  "Load the .asd and the system of the cell named NAME from its folder, the
way a boot would; with TEST-SYSTEM, its /test system too. => NAME's package."
  (let* ((folder (generated-folder name))
         (asd (merge-pathnames (format nil "nodecode-~a.asd" name) folder))
         (system (format nil "nodecode-~a" name)))
    (check-reads (merge-pathnames "cell.lisp" folder))
    (check-reads asd)
    (asdf:load-asd (truename asd))
    (unless (asdf:find-system system nil)
      (error "~a registered nothing: ~a" system (uiop:read-file-string asd)))
    (asdf:load-system system)
    (when test-system (asdf:load-system (format nil "~a/test" system)))
    (or (find-package (string-upcase system))
        (error "~a defined no package" system))))

(defun start-generated (name)
  "Start the generated cell NAME the way the gateway does => its stop thunk,
or a test failure when it answers none."
  (let* ((package (load-generated-cell name))
         (start (find-symbol "START-CELL" package)))
    (unless (and start (fboundp start))
      (error "~a exports no START-CELL" name))
    ;; the config the gateway hands a cell: one section, under the folder's own
    ;; name, empty because every field this cell declares is optional
    (let* ((section (make-hash-table :test #'equal))
           (config (make-hash-table :test #'equal))
           (stop (progn (setf (gethash name config) section)
                        (funcall start config))))
      (unless (functionp stop)
        (error "~a's START-CELL answered no stop thunk" name))
      stop)))
