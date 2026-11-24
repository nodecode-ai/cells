;;;; support.lisp --- memory-shield test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; These tests register into the shared nodecode.test registry under a
;;;; MEMORY-SHIELD- name prefix, so this system's test-op runs only this
;;;; folder's slice. The knowledge folder is a scratch one, made and removed
;;;; by the helper below: no test reads the home it runs in.

(in-package #:nodecode.test)

(define-test-slice "memory-shield" "MEMORY-SHIELD" :start nodecode-memory-shield:start-cell)
