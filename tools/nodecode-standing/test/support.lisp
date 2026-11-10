;;;; support.lisp --- standing test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Standing tests register into the shared nodecode.test registry under a
;;;; STANDING- name prefix and run as that slice, so this system's test-op
;;;; never re-runs the core suite.

(in-package #:nodecode.test)

(define-test-slice "standing" "STANDING-CELL" :start nodecode-standing:start-cell)
