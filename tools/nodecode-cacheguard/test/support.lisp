;;;; support.lisp --- cacheguard test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Cacheguard tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CACHEGUARD- name
;;;; prefix; RUN-CACHEGUARD-TESTS runs exactly that slice.

(in-package #:nodecode.test)

(defvar *cacheguard-notices* '()
  "What the cell said while a test had NLE:NOTICE stubbed, newest first.")

(defun cacheguard-hook ()
  "The cell's installed advice on the provider call, or NIL when it is not running."
  (cell-hook "nodecode-cacheguard" 'nodecode.evolved::call-provider))

(defun cacheguard-seed (session &key (pause-minutes 12) (tokens 48000))
  "Put SESSION in the state a real chat reaches after TOKENS and a pause."
  (setf (gethash session nodecode-cacheguard::*history*)
        (list :last-at (- (nodecode-cacheguard::now) (* 60 (round pause-minutes)))
              :prompt-tokens tokens)))

(defun cacheguard-usage (&key (input 0) (cached 0) (written 0) (output 100))
  (nodecode.evolved::make-provider-usage :input-tokens input :cached-input-tokens cached
                                          :cache-write-tokens written :output-tokens output))

(define-test-slice "cacheguard" "CACHEGUARD" :start nodecode-cacheguard:start-cell)
