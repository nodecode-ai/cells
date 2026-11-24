;;;; support.lisp --- the sentinel's test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under the
;;;; SENTINEL name prefix, and RUN-SENTINEL-TESTS runs exactly that slice, so
;;;; this system's test-op never re-runs the core suite. The hermetic posture
;;;; LET-binds NLE:*HOOKS* around every test body, so a hook a start installs
;;;; never reaches a later test.

(in-package #:nodecode.test)

(define-test-slice "sentinel" "SENTINEL" :start nodecode-sentinel:start-cell)
