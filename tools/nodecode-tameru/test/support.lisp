;;;; support.lisp --- the tameru test slice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tameru's tests register into the SHARED nodecode.test registry under a
;;;; TAMERU name prefix; RUN-TAMERU-TESTS runs exactly that slice, so this
;;;; system's test-op never re-runs the core suite. The hermetic posture
;;;; LET-binds NLE:*HOOKS* to '() around every test body, so the hook a start
;;;; installs unwinds with the test and never shortens a later test's output.

(in-package #:nodecode.test)

(define-test-slice "tameru" "TAMERU" :start nodecode-tameru:start-cell)
