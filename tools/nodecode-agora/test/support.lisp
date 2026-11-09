;;;; support.lisp --- the agora test slice and its two helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the same nodecode.test registry every cell's do
;;;; (the core DEFTEST, with its hermetic machine-state posture) under an
;;;; AGORA-CELL- name prefix; RUN-AGORA-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed: the ledger is a real file under the
;;;; scratch home, the verbs are the cell's own, and a tool is called the way
;;;; the engine calls it -- its contract's decoder, then the registered
;;;; function -- so the schema the model reads and the body are proved on one
;;;; path. The only thing that reaches outside a test is the clock.

(in-package #:nodecode.test)

(define-test-slice "agora" "AGORA-CELL-" :start nodecode-agora:start-cell)

(defparameter +roles+ #("architect" "developer" "reviewer")
  "The roles a healthy section names.")

(defparameter +decoders+
  '(("agora_raise_motion" . nodecode-agora::decode-raise-call)
    ("agora_speak"        . nodecode-agora::decode-speak-call)
    ("agora_vote"         . nodecode-agora::decode-vote-call)
    ("agora_close_motion" . nodecode-agora::decode-close-call)
    ("agora_get_motion"   . nodecode-agora::decode-motion-call)
    ("agora_list_motions" . nodecode-agora::decode-motions-call)
    ("agora_create_task"  . nodecode-agora::decode-task-call)
    ("agora_list_tasks"   . nodecode-agora::decode-tasks-call)
    ("agora_close_task"   . nodecode-agora::decode-done-call))
  "Each tool and the decoder its contract derives.")

(defun agora-tool (name json)
  "The tool NAME called the way the engine calls it, on the JSON arguments a
model sent: refused before the body when the declaration is not satisfied."
  (let ((entry (nle::find-tool name))
        (decoder (cdr (assoc name +decoders+ :test #'string=))))
    (unless entry (error "the ~a tool is not registered" name))
    (unless decoder (error "~a has no decoder" name))
    (funcall (third entry) (funcall decoder (nlk:decode-json json)))))

(defun refusal (thunk)
  "The refusal THUNK signals, as its text, or NIL when it did not."
  (handler-case (progn (funcall thunk) nil)
    (agora:agora-error (condition) (princ-to-string condition))))

(defmacro with-agora ((&rest pairs) &body body)
  "BODY inside a started cell on PAIRS (the `roles` of +ROLES+ unless given),
under a scratch home and a temp store, stopped on unwind."
  `(with-temp-store ()
     (with-cell-stop ((agora-start "roles" ,+roles+ ,@pairs))
       ,@body)))
