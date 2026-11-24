;;;; package.lisp --- the DEFERRED-QUESTIONS package: the ledger's verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DQ so an eval form reads
;;;; (dq:request :question "Which date?") and (dq:answer 3 "Friday").
;;;; Every exported verb answers a STRING and every refusal is
;;;; DEFERRED-QUESTIONS-ERROR, rendered by the eval snippet as
;;;; "ERROR: DEFERRED-QUESTIONS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `deferred_questions'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :MAX-OPTIONS).

(defpackage #:nodecode-deferred-questions
  (:use #:cl)
  (:nicknames #:dq)
  (:export
   ;; the ledger, every verb a string
   #:request #:answer #:show #:board #:cancel
   ;; what one model request carries, and the numbers behind it
   #:continuation #:pending-items
   ;; data, so a layer can reword it
   #:+actions+ #:*framing* #:+primer+
   ;; the one condition
   #:deferred-questions-error))

(in-package #:nodecode-deferred-questions)

(nlk:define-peripheral deferred-questions :not-running t :lock "deferred-questions"
  :recursive t)
