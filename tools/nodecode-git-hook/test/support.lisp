;;;; support.lisp --- git-hook test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; git-hook tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a GIT-HOOK- name
;;;; prefix; RUN-GIT-HOOK-TESTS runs exactly that slice, so this system's
;;;; test-op never re-runs the core suite. The hermetic posture LET-binds
;;;; NLE:*HOOKS* to '() around every test body, so the hooks START-CELL
;;;; installs unwind with the test.

(in-package #:nodecode.test)

(define-test-slice "git-hook" "GIT-HOOK" :start nodecode-git-hook:start-cell)
