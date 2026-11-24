;;;; support.lisp --- fabric test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; fabric tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a FABRIC-CELL-
;;;; name prefix; RUN-FABRIC-TESTS runs exactly that slice.
;;;;
;;;; The cell's only outside world is a folder of markdown files, so that is
;;;; what the tests own: every start builds a fresh directory under the
;;;; temporary directory, and nothing here touches ~/fabric or a network.

(in-package #:nodecode.test)

(define-test-slice "fabric" "FABRIC-CELL-" :start nodecode-fabric:start-cell
  :defaults (("agent" "tester") ("dir" (fabric-test-dir))))

(defun fabric-test-dir ()
  "A fresh, empty fabric folder for one start."
  (let ((dir (merge-pathnames "fabric-cell-test/" (uiop:temporary-directory))))
    (ignore-errors (uiop:delete-directory-tree dir :validate t))
    (ensure-directories-exist (merge-pathnames "probe" dir))
    (namestring dir)))

(defmacro with-fabric ((&rest pairs) &body body)
  "BODY with the cell started over a fresh folder and PAIRS in its section."
  `(with-cell-stop ((fabric-start ,@pairs)) ,@body))

(defun fabric-entries ()
  "The entry files the running cell sees."
  (sort (copy-list (directory (merge-pathnames "*.md" (nodecode-fabric::fabric-dir))))
        #'string< :key #'file-namestring))

(defun fabric-entry-field (path key)
  "KEY's value in the entry file PATH, as the cell reads it."
  (nodecode-fabric::field (nodecode-fabric::read-entry path) key))

(defun fabric-standing (session)
  "The fabric section SESSION's requests carry, or NIL."
  (cdr (assoc "fabric" (nle::read-harness-sections session) :test #'equal)))
