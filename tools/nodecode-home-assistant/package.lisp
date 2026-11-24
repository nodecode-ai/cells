;;;; package.lisp --- the HOME-ASSISTANT package: four tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed HOME-ASSISTANT so an eval form reads
;;;; (home-assistant:list-entities ...). The four verbs are tools the model
;;;; calls; a person can call the same functions from eval.

(defpackage #:nodecode-home-assistant
  (:use #:cl)
  (:nicknames #:home-assistant)
  (:export #:start-cell
           #:stop-cell
           #:list-entities #:read-state #:list-services #:call-service
           #:home-assistant-error #:home-assistant-error-detail))

(in-package #:nodecode-home-assistant)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and HOME-ASSISTANT-ERROR, which a request that cannot go
;;; on signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral home-assistant :not-running t)
