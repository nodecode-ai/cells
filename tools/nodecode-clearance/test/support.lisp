;;;; support.lisp --- clearance test runner and what every test reaches for.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Clearance tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CLEARANCE- name
;;;; prefix; RUN-CLEARANCE-TESTS runs exactly that slice, so this system's
;;;; test-op never re-runs the core suite and `just test` never runs these.
;;;;
;;;; The hermetic posture LET-binds NLE:*HOOKS* to '() around every test body,
;;;; so a hook installed by START-CELL unwinds with the test and never guards a
;;;; later one. The home the audit log is written under is the run's own.

(in-package #:nodecode.test)

(define-test-slice "clearance" "CLEARANCE-" :start nodecode-clearance:start-cell)
