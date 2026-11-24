;;;; support.lisp --- the zeromem test runner and its fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Zeromem tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a ZEROMEM-CELL-
;;;; name prefix; RUN-ZEROMEM-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed. The store is a real folder of real
;;;; files under the suite's scratch home, the turns are real facts in a real
;;;; event log, and a round is the cell's own :ROUND hook funcalled by hand.
;;;; No test dials anything: the store has no network path at all.

(in-package #:nodecode.test)

(define-test-slice "zeromem" "ZEROMEM-CELL-" :start nodecode-zeromem:start-cell)

(defmacro with-zeromem ((&rest config) &body body)
  "BODY with the cell started -- on its defaults, or on the (KEY VALUE)
pairs CONFIG gives -- over a store of the scratch home's own, and stopped on
unwind."
  `(with-temp-store ()
     (with-cell-stop ((zeromem-start ,@config))
       ,@body)))

(defun zeromem-store ()
  "The store the running cell opened."
  (nodecode-zeromem::running-store))

(defun zeromem-kept ()
  "The turns the running cell keeps, in memory."
  (nodecode-zeromem::store-turns (zeromem-store)))

(defun zeromem-admit (session input &optional (source "gateway"))
  "Admit one turn on SESSION with INPUT, as the gateway's own start does."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command input
                    :invocation (nlk:default-invocation "turn.start" command :source source))))

(defun zeromem-say (turn text)
  "Record TEXT as TURN's answer and complete the turn."
  (nlk:complete-turn turn (nle::message "assistant" text)))

(defun zeromem-ingest (session)
  "Keep what SESSION said since the store last read it, as the round hook does."
  (nodecode-zeromem::ingest-session (zeromem-store) session))

(defun zeromem-told (session)
  "Every reminder text SESSION's turns were told, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt"
                                    :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when text collect text))

(defun zeromem-round (session turn round)
  "Run the cell's own :ROUND hook for ROUND of TURN, as the turn worker does."
  (funcall (cell-hook "nodecode-zeromem" :round)
           (list :session-id session :turn turn :round round)
           (lambda (op) (declare (ignore op)) nil)))
