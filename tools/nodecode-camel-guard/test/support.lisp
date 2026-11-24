;;;; support.lisp --- camel-guard test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; camel-guard tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CAMEL-GUARD- name
;;;; prefix; RUN-CAMEL-GUARD-TESTS runs exactly that slice, so this system's
;;;; test-op never re-runs the core suite. The hermetic posture LET-binds
;;;; NLE:*HOOKS* to '() around every test body, so the hook START-CELL installs
;;;; unwinds with the test.

(in-package #:nodecode.test)

(define-test-slice "camel-guard" "CAMEL-GUARD" :start nodecode-camel-guard:start-cell)
