;;;; support.lisp --- tenuo test runner.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; TENUO- name prefix and RUN-TENUO-TESTS runs exactly that slice, so this
;;;; system's test-op never re-runs the core suite. The hermetic posture
;;;; LET-binds NLE:*HOOKS* around every test body, so the hook a start
;;;; installs unwinds with the test.

(in-package #:nodecode.test)

(define-test-slice "tenuo" "TENUO" :start nodecode-tenuo:start-cell)
