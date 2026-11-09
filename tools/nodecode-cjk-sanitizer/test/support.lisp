;;;; support.lisp --- cjk-sanitizer test runner and the one helper.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CJK-SANITIZER
;;;; name prefix; RUN-CJK-SANITIZER-TESTS runs exactly that slice. Nothing
;;;; here dials anything: the sanitizer reads no file, no environment and no
;;;; network, and the one seam the tests reach for is the cell's own hook.

(in-package #:nodecode.test)

(define-test-slice "cjk-sanitizer" "CJK-SANITIZER" :start nodecode-cjk-sanitizer:start-cell)

(defun cjk-parts (kind text)
  "The stream parts NLE::EMIT-STREAM-PART hands an observer for one KIND part
carrying TEXT, with the cell running: what a lane's one fragment becomes."
  (let ((parts '()))
    (nle::emit-stream-part (lambda (part) (push part parts)) kind :id "s1" :text text)
    (nreverse parts)))
