;;;; support.lisp --- hyperactive's test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under the
;;;; HYPERACTIVE-CELL- name prefix; RUN-HYPERACTIVE-TESTS runs exactly that
;;;; slice. Nothing of the cell is stubbed: a start reads a real section,
;;;; the verbs write a real file in the run's scratch home.

(in-package #:nodecode.test)

(define-test-slice "hyperactive" "HYPERACTIVE-CELL-" :start nodecode-hyperactive:start-cell)

(defmacro with-hyperactive-home (&body body)
  "BODY over a temp store and a scratch home, so the state file is the run's own."
  `(with-temp-store ()
     (ensure-durable-session "s1")
     ,@body))

(defmacro with-hyperactive ((&rest config) &body body)
  "BODY with the cell started on CONFIG pairs (string keys, as the section reads them)
-- or on its defaults -- and stopped on unwind."
  `(with-hyperactive-home
     (with-cell-stop ((hyperactive-start ,@config))
       ,@body)))

(defun ha-args (&rest pairs)
  "A tool call's arguments: a JSON object of PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun ha-tool (name arguments)
  "The answer a call to tool NAME with ARGUMENTS gets, through the registered function."
  (let ((tool (nle::find-tool name)))
    (unless tool (error "no tool ~a registered" name))
    (funcall (third tool) arguments)))

(defun ha-now ()
  (nodecode-hyperactive::now))
