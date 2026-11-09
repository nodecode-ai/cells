;;;; support.lisp --- process-table test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Process-table tests register into the shared nodecode.test registry under
;;;; a PROCESS-TABLE- name prefix and run as that slice, so this system's
;;;; test-op never re-runs the core suite.

(in-package #:nodecode.test)

(define-test-slice "process-table" "PROCESS-TABLE" :start nodecode-process-table:start-cell)
