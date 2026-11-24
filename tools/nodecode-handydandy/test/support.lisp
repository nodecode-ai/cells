;;;; support.lisp --- handydandy test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; HandyDandy tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a HANDYDDANDY-
;;;; CELL- name prefix; RUN-HANDYDDANDY-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed. Sessions are real, a handoff is a real
;;;; file under the scratch home's handoffs folder, a turn runs through
;;;; NLE:RUN; the one stub is the provider (WITH-PROVIDER-STUB), which
;;;; answers the model's messages.

(in-package #:nodecode.test)

(define-test-slice "handydandy" "HANDYDANDY-CELL-" :start nodecode-handydandy:start-cell)

(defvar *turns* 0
  "How many turns a test admitted, so each gets a command id of its own.")

(defmacro with-handydandy ((&rest sessions) &body body)
  "BODY over a temp store holding SESSIONS, the cell started on its defaults
-- or on the pairs a leading (:config PAIR...) form gives -- and stopped on
unwind."
  ;; Each pair is ("key" value); the start takes them flat, key then value.
  (let ((config (and (consp (first body)) (eq :config (first (first body)))
                     (loop for pair in (rest (pop body)) append pair))))
    `(with-temp-store ()
       (setf *turns* 0)
       ,@(mapcar (lambda (session) `(ensure-durable-session ,session)) sessions)
       (with-cell-stop ((handydandy-start ,@config))
         ,@body))))

(defun handydandy-as (session thunk)
  "THUNK's values, called as SESSION's evaluation would call it."
  (let ((nlk:*scribe-session-id* session))
    (funcall thunk)))

(defmacro on-session (session &body body)
  "BODY as SESSION's evaluation: (on-session \"a\" (handydandy:status))."
  `(handydandy-as ,session (lambda () ,@body)))

(defun handydandy-turn (session &optional (text "go"))
  "One turn of SESSION through a stubbed provider, as one prompt from the
operator would take it. => the provider stub's answer."
  (with-provider-stub (nil (wire-message "assistant" :content "done"))
    (nle:run text :session (nle:intern-session session)
                 :command-id (format nil "handydandy-~a-~d" session (incf *turns*)))))

(defun handydandy-told (session)
  "Every line this cell put in SESSION's turns' prompts, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt" :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when (and text (uiop:string-prefix-p "[handydandy]" text))
          collect text))

(defun handydandy-brief (&key (workstreams "the cell is written; its tests are next")
                               (next-action "run the cell's tests"))
  "A brief as the model would pass it to the handoff tool."
  (nodecode-handydandy::make-handoff-brief :workstreams workstreams :next-action next-action))
