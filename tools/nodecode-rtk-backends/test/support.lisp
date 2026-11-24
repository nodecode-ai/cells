;;;; support.lisp --- the rtk test slice.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; RTK's tests register into the SHARED nodecode.test registry under an
;;;; RTK-CELL name prefix; RUN-RTK-TESTS runs exactly that slice, so this
;;;; system's test-op never re-runs the core suite. The hermetic posture
;;;; LET-binds NLE:*HOOKS* to '() around every test body, so a hook installed
;;;; by START-CELL unwinds with the test and never rewrites a later test's
;;;; call.

(in-package #:nodecode.test)

(define-test-slice "rtk-backends" "RTK-BACKENDS-CELL" :start nodecode-rtk-backends:start-cell)
