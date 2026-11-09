;;;; package.lisp --- the TELEGRAM-BUSINESS package: three tools, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TELEGRAM-BUSINESS so an eval form reads
;;;; (telegram-business:poll-updates ...). The three verbs are tools the model
;;;; calls; a person can call the same functions from eval.

(defpackage #:nodecode-telegram-business
  (:use #:cl)
  (:nicknames #:telegram-business)
  (:export #:start-cell
           #:stop-cell
           #:poll-updates #:draft-reply #:send-reply #:run-command
           #:telegram-business-error #:telegram-business-error-detail))

(in-package #:nodecode-telegram-business)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and TELEGRAM-BUSINESS-ERROR, which a call that cannot go
;;; on signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral telegram-business :not-running t :lock "telegram-business" :recursive t)
