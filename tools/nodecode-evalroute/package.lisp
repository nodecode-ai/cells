;;;; package.lisp --- the NODECODE-EVALROUTE package: the verbs and the one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed EVALROUTE so an eval form reads
;;;; (evalroute:route "task"). Every refusal the model sees is
;;;; EVALROUTE-ERROR, which the eval snippet renders as
;;;; "ERROR: EVALROUTE-ERROR: <detail>".

(defpackage #:nodecode-evalroute
  (:use #:cl)
  (:nicknames #:evalroute)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:route #:rate #:report #:lanes #:status
   ;; the boundary payload of the evalroute_route tool
   #:route-request #:route-request-task #:route-request-lane
   ;; the one condition
   #:evalroute-error #:evalroute-error-detail))

(in-package #:nodecode-evalroute)

;;; The settings special (*EVALROUTE*), FAIL and the condition come from the
;;; declarations that load here; CELL.LISP reads the section through SETTING
;;; and refuses as "not running" while the cell is stopped.
(nlk:define-peripheral evalroute :not-running t :lock "evalroute")
