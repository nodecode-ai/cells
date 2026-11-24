;;;; package.lisp --- the HEARTBEAT package: the registry, the verbs, the one condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, which is how the loader finds
;;;; START-CELL. It is nicknamed HEARTBEAT so an eval form reads
;;;; (heartbeat:watches). Nothing here shadows a COMMON-LISP name: the
;;;; operator's verb for taking a watch away is FORGET, beside CL:REMOVE.

(defpackage #:nodecode-heartbeat
  (:use #:cl)
  (:nicknames #:heartbeat)
  (:export
   ;; the operator's and the model's verbs, callable from eval
   #:add #:watches #:show #:pause #:resume #:forget #:check
   ;; the one condition every refusal is
   #:heartbeat-error))

(in-package #:nodecode-heartbeat)

;;; Defines HEARTBEAT-ERROR and FAIL, *HEARTBEAT* and RUNNING-SETTINGS, and
;;; WITH-HEARTBEAT-LOCK, the lock the registry and the ticker share.
(nlk:define-peripheral heartbeat :not-running t :lock "heartbeat")
