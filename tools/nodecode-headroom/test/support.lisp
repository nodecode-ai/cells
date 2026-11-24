;;;; support.lisp --- the headroom test slice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tests register into the shared NODECODE.TEST registry under a HEADROOM-
;;;; name prefix; RUN-HEADROOM-TESTS runs exactly that slice, so the core
;;;; suite is never re-run here.

(in-package #:nodecode.test)

(define-test-slice "headroom" "HEADROOM-" :start nodecode-headroom:start-cell)
