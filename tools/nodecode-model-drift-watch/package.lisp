;;;; package.lisp --- the NODECODE-MODEL-DRIFT-WATCH package: the verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed MODEL-DRIFT-WATCH so an eval form
;;;; reads (model-drift-watch:status). Every refusal the model sees is
;;;; MODEL-DRIFT-WATCH-ERROR, which the eval snippet renders as
;;;; "ERROR: MODEL-DRIFT-WATCH-ERROR: <detail>".

(defpackage #:nodecode-model-drift-watch
  (:use #:cl)
  (:nicknames #:model-drift-watch)
  (:export
   ;; the verbs, each answering the report as a string
   #:status #:check #:estimate #:reset #:questions
   ;; the suite, so a layer can reword an item
   #:*suite*
   ;; the one condition
   #:model-drift-watch-error))

(in-package #:nodecode-model-drift-watch)

;;; The condition, FAIL, the settings variable and RUNNING-SETTINGS come from
;;; here; the section itself is declared in cell.lisp, which loads last.
(nlk:define-peripheral model-drift-watch :not-running t :lock "model-drift-watch")
