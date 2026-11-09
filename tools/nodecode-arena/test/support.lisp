;;;; support.lisp --- the arena test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under an ARENA-CELL-
;;;; name prefix; RUN-ARENA-TESTS runs exactly that slice. Nothing here dials
;;;; anything: a run is files in a temporary folder and one JSON state file,
;;;; and the sub-agents are the test writing their outputs by hand.

(in-package #:nodecode.test)

(define-test-slice "arena" "ARENA-CELL-" :start nodecode-arena:start-cell)
