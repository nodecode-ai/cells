;;;; package.lisp --- the NODECODE-ADAPTIVE-EFFORT package: the three verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed AEFFORT so an eval form reads
;;;; (aeffort:choose ...). Every failure the model sees is
;;;; ADAPTIVE-EFFORT-ERROR, which the eval snippet renders as
;;;; "ERROR: ADAPTIVE-EFFORT-ERROR: <detail>"; the detail never carries a key.

(defpackage #:nodecode-adaptive-effort
  (:use #:cl)
  (:nicknames #:aeffort)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:choose #:probe #:history #:status
   ;; the one condition
   #:adaptive-effort-error))

(in-package #:nodecode-adaptive-effort)

;;; The settings special (*ADAPTIVE-EFFORT*) and the condition come from the
;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; score.lisp
;;; reads the section through SETTING, and refuses as "not running" while the
;;; cell is stopped.
(nlk:define-peripheral adaptive-effort :not-running t)
