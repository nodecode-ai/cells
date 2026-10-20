;;;; support.lisp --- fork test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Fork tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a FORK-CELL-
;;;; name prefix; RUN-FORK-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed. Sessions are real, a fork is a real
;;;; anchored child in a real store, its turn is admitted through NLE:SUBMIT
;;;; and runs on a real worker; the one stub is the provider
;;;; (WITH-PROVIDER-STUB).

(in-package #:nodecode.test)

(define-test-slice "fork" "FORK-CELL-" :start nodecode-fork:start-cell)

(defun fork-calling-turn (session)
  "A turn admitted on SESSION, as the operator's would be, whose round called
eval and has no result yet: the turn a call of fork:run runs in."
  (let* ((command (nlk:make-durable-id "command"))
         (turn (nlk:admit-turn session command "split this up"
                               :invocation (nlk:default-invocation "turn.start" command :source "gateway")))
         (arguments "{\"form\":\"(fork:run \\\"the task\\\")\"}"))
    (nlk:record-assistant-message
     turn (nlk:json-object "role" "assistant" "content" :null
                           "tool_calls" (vector (nlk:json-object
                                                 "id" "call-fork" "type" "function"
                                                 "function" (nlk:json-object "name" "eval"
                                                                             "arguments" arguments)))))
    (nlk:record-tool-call turn "call-fork" "eval" arguments)
    turn))

(defmacro with-fork-call ((session turn) &body body)
  "BODY over a temp store holding SESSION, the cell started on its defaults,
as SESSION's eval inside TURN: an earlier exchange answered, then TURN, whose
eval call is still open, as one calling fork:run is."
  `(with-temp-store ()
     (with-cell-stop ((fork-start))
       (seed-completed-turn ,session "the parent's own question" "the parent's own answer")
       (let* ((,turn (fork-calling-turn ,session))
              (nlk:*scribe-session-id* ,session)
              (nle::*current-durable-turn* ,turn))
         ,@body))))

(defun fork-request-text (context needle)
  "The first message text CONTEXT, one compiled request, carries that holds
NEEDLE, or NIL."
  (loop for message across (coerce (nle::compiled-turn-context-messages context) 'vector)
        for text = (nle::content-text (gethash "content" message))
          thereis (and (stringp text) (search needle text) text)))

(defun fork-field (results key)
  "KEY of each of fork:run's RESULTS, in order."
  (mapcar (lambda (result) (getf result key)) results))
