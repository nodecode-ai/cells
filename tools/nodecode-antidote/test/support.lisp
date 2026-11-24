;;;; support.lisp --- antidote test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Antidote tests register into the shared nodecode.test registry under an
;;;; ANTIDOTE- name prefix and run as that slice, so this system's test-op
;;;; never re-runs the core suite.

(in-package #:nodecode.test)

(define-test-slice "antidote" "ANTIDOTE" :start nodecode-antidote:start-cell)
