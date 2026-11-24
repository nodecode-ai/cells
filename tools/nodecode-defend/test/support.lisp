;;;; support.lisp --- defend test runner.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Defend tests register into the shared nodecode.test registry under a
;;;; DEFEND- name prefix; RUN-DEFEND-TESTS runs exactly that slice, so this
;;;; system's test-op never re-runs the core suite.

(in-package #:nodecode.test)

(define-test-slice "defend" "DEFEND" :start nodecode-defend:start-cell)
