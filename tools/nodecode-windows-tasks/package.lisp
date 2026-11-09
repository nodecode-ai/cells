;;;; package.lisp --- the NODECODE-WINDOWS-TASKS package: two verbs and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, nicknamed WINDOWS-TASKS so an eval
;;;; form reads (windows-tasks:tasks): the same word the command and the help
;;;; topic use. It is meant to be used qualified only.

(defpackage #:nodecode-windows-tasks
  (:use #:cl)
  (:nicknames #:windows-tasks)
  (:export
   #:tasks #:task-detail
   #:windows-tasks-error))

(in-package #:nodecode-windows-tasks)

;;; WINDOWS-TASKS-ERROR is the one condition every refusal signals, FAIL signals
;;; it with a detail; NOT-RUNNING gives this package *WINDOWS-TASKS*,
;;; RUNNING-SETTINGS -- the refusal a verb opens with while the cell is not
;;; started -- and DEFINE-VERB. SETTING comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last.
(nlk:define-peripheral windows-tasks :not-running t)
