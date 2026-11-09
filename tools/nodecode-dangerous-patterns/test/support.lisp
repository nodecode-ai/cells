;;;; support.lisp --- the dangerous-patterns test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tests register into the SAME nodecode.test registry as every other cell
;;;; (the core DEFTEST, with its hermetic posture), under a PATTERNS- name
;;;; prefix, so RUN-DANGEROUS-PATTERNS-TESTS runs exactly this slice.

(in-package #:nodecode.test)

(define-test-slice "dangerous-patterns" "DANGEROUS-PATTERNS" :start nodecode-dangerous-patterns:start-cell)
