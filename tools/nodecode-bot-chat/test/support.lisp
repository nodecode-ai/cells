;;;; support.lisp --- bot chat's test runner and the helpers its tests share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Bot chat's tests register into the shared nodecode.test registry under a
;;;; BOT-CHAT- name prefix, so RUN-BOT-CHAT-TESTS runs exactly that slice and
;;;; never the core suite. Every test here is offline: the rules read a call's
;;;; own text, and nothing is asked of the network or of an operator.

(in-package #:nodecode.test)

(define-test-slice "bot-chat" "BOT-CHAT" :start nodecode-bot-chat:start-cell)

(defun bot-chat-here (&optional (session "s1") (turn "t1"))
  "Bind the call context the cell reads when no turn names one."
  (setf nodecode-bot-chat::*here* (list :session-id session :turn-id turn)))

(defun bot-chat-verdict (op)
  "The installed :TOOL hook's answer for OP: :ALLOWED, or the refusal text it
answered instead."
  (funcall (cell-hook "nodecode-bot-chat" :tool) op
           (lambda (op) (declare (ignore op)) :allowed)))

(defun bot-chat-eval-verdict (form &optional (session "s1"))
  "The verdict for an EVAL call of FORM in SESSION."
  (bot-chat-here session)
  (bot-chat-verdict (list :name "eval"
                           :arguments (nlk:json-object "form" form)
                           :call-id "bot-chat-call")))

(defun bot-chat-tool-verdict (name &rest pairs)
  "The verdict for a call of the tool NAME carrying PAIRS."
  (bot-chat-here)
  (bot-chat-verdict (list :name name
                           :arguments (apply #'nlk:make-json-object pairs)
                           :call-id "bot-chat-call")))

(defun bot-chat-command (args &optional (session "s1"))
  "Run /bot-chat ARGS in SESSION: what the registered command's function answers."
  (cell-entry "nodecode-bot-chat" "bot-chat" args session))

(defun bot-chat-section (session)
  "The standing harness sections the cell supplies for SESSION."
  (funcall (cell-hook "nodecode-bot-chat" 'nle::read-harness-sections)
           (lambda (id) (declare (ignore id)) (list (cons "index" "nothing here")))
           session))

(defun bot-chat-ask (title &optional (session "s1") (turn "t1"))
  "One ask of the advised ASK-OPERATOR: its first value, and whether the
cell let the question through (:ASKED) or answered it unanswered."
  (bot-chat-here session turn)
  (funcall (cell-hook "nodecode-bot-chat" 'nle:ask-operator)
           (lambda (&rest args) (declare (ignore args)) :asked)
           title))
