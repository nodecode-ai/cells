;;;; support.lisp --- the heartbeat slice's runner and the two seams it stubs.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; HEARTBEAT-CELL- prefix, and RUN-HEARTBEAT-TESTS runs exactly that slice.
;;;; The ticker thread is never relied on: WITH-HEARTBEAT-RUNTIME starts the
;;;; cell with *TICK-P* NIL and every test drives TICK by hand with the
;;;; instant it means, so nothing races the assertions.

(in-package #:nodecode.test)

(define-test-slice "heartbeat" "HEARTBEAT-CELL-" :start nodecode-heartbeat:start-cell)

(defvar *heartbeat-submits* '()
  "(:session :prompt :command-id :watch-id) plists the stub received, newest first.")

(defun heartbeat-submits ()
  "Every captured wake, oldest first."
  (reverse *heartbeat-submits*))

(defmacro with-captured-wakes (&body body)
  "Run BODY with the cell's one ingress seam recording instead of starting a turn."
  `(let ((*heartbeat-submits* '()))
     (with-stubbed-fdefinitions
         ((nodecode-heartbeat::submit-wake (session prompt command-id watch-id)
            (push (list :session session :prompt prompt
                        :command-id command-id :watch-id watch-id)
                  *heartbeat-submits*)
            :started))
       ,@body)))

(defmacro with-heartbeat-runtime ((&key (tick-seconds 60) (max-watches 5)) &body body)
  "Run BODY with the cell started, no ticker thread and an empty registry, so a
test sees exactly the watches it armed. The registry is never written."
  `(let ((nodecode-heartbeat::*tick-p* nil)
         (nodecode-heartbeat::*save-p* nil)
         (nodecode-heartbeat::*watches* '())
         (nlk:*scribe-session-id* "s1"))
     (with-cell-stop ((heartbeat-start "tick_seconds" ,tick-seconds "max_watches" ,max-watches))
       (setf nodecode-heartbeat::*watches* '())
       ,@body)))

(defun heartbeat-arm (&rest arguments)
  "ARM against the test's clock and session, as a verb or the tool would call it."
  (apply #'nodecode-heartbeat::arm arguments))
