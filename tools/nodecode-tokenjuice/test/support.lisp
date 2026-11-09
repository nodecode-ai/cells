;;;; support.lisp --- the tokenjuice test slice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tokenjuice's tests register into the SHARED nodecode.test registry under
;;;; a TOKENJUICE name prefix; RUN-TOKENJUICE-TESTS runs exactly that slice,
;;;; so this system's test-op never re-runs the core suite. The hermetic
;;;; posture LET-binds NLE:*HOOKS* to '() around every test body, so a hook
;;;; installed by START-CELL unwinds with the test and never folds a later
;;;; test's output.

(in-package #:nodecode.test)

(define-test-slice "tokenjuice" "TOKENJUICE" :start nodecode-tokenjuice:start-cell)
