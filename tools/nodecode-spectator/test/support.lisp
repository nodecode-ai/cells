;;;; support.lisp --- spectator test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Spectator tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a SPECTATOR- name
;;;; prefix; RUN-SPECTATOR-TESTS runs exactly that slice.
;;;;
;;;; Nothing here reaches the network: a fact is a fact in a real store, an
;;;; invite is a token in the running cell, and the one seam that would leave
;;;; the image -- the ingress an approved suggestion travels -- is a variable
;;;; the test rebinds.

(in-package #:nodecode.test)

(define-test-slice "spectator" "SPECTATOR" :start nodecode-spectator:start-cell)

(defun spectator-token (text)
  "The token in the address SHARE answered: the segment after /SPECTATOR/."
  (let* ((start (search "/spectator/" text))
         (rest (and start (subseq text (+ start (length "/spectator/"))))))
    (and rest (subseq rest 0 (or (position #\/ rest) (length rest))))))

(defun spectator-env (token &key (op "") (method :get) query)
  "The clack env the gateway hands GUEST-ROUTE for TOKEN, its OP and QUERY."
  (list :request-method method
        :path-info (format nil "/spectator/~a/~a" token op)
        :query-string query
        :headers nil))
