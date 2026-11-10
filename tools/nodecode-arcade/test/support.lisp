;;;; support.lisp --- the arcade test runner and the helpers every test reads.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under an ARCADE name prefix;
;;;; RUN-ARCADE-TESTS runs exactly that slice. Nothing here dials anything: the
;;;; cabinet reads no network and no environment, and the one file it writes is
;;;; a store the test binds to a path of its own.

(in-package #:nodecode.test)

(define-test-slice "arcade" "ARCADE" :start nodecode-arcade:start-cell)

(defun arcade-tool (id name arguments)
  "NAME called as the provider's call calls it: the answer's text."
  (nle::message-content (execute-wire-call id name arguments)))

(defmacro with-cabinet ((&rest start-args) &body body)
  "BODY inside a started cabinet, its board a file of this test's own, the
cabinet empty of runs."
  `(let ((nodecode-arcade:*store-file* (nlk:home "arcade/test-scores.txt")))
     (ignore-errors (delete-file nodecode-arcade:*store-file*))
     (nodecode-arcade:reset-cabinet)
     (unwind-protect
          (with-cell-stop ((arcade-start ,@start-args))
            ,@body)
       (nodecode-arcade:reset-cabinet))))

(defmacro with-board-file ((path) &body body)
  "BODY with the board written to PATH, emptied first."
  `(let ((nodecode-arcade:*store-file* ,path))
     (ignore-errors (delete-file nodecode-arcade:*store-file*))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file nodecode-arcade:*store-file*)))))
