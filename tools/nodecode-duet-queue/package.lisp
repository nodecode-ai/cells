;;;; package.lisp --- the DUET-QUEUE package: the cell's verbs and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the folder: the loader finds START-CELL as this
;;;; package's exported symbol, and every contribution is filed under
;;;; "nodecode-duet-queue". DEFINE-PERIPHERAL gives the package
;;;; DUET-QUEUE-ERROR, FAIL, *DUET-QUEUE* (the settings while it runs) and
;;;; WITH-DUET-QUEUE-LOCK.

(defpackage #:nodecode-duet-queue
  (:use #:cl)
  (:export
   ;; the two tools, so a person can call one from eval
   #:queue-tool #:swarm-tool
   ;; the store's verbs
   #:add-task #:list-tasks #:task-line #:queue-summary #:run-one
   ;; the condition, and the runner a test replaces
   #:duet-queue-error #:*run-command*))

(in-package #:nodecode-duet-queue)

(nlk:define-peripheral duet-queue :not-running t :lock "duet-queue")
