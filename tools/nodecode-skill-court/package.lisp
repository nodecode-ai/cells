;;;; package.lisp --- the NODECODE-SKILL-COURT package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. The court's pieces are internal: the model and
;;;; the operator reach this cell through its four tools, its /skill-court
;;;; command and (help :skill-court), never through a verb of its own.

(defpackage #:nodecode-skill-court
  (:use #:cl)
  (:export #:score #:make-pair #:pair #:pair-task #:pair-with #:pair-without
           #:pair-with-tokens #:pair-without-tokens))

(in-package #:nodecode-skill-court)

(nlk:define-peripheral skill-court :not-running t :lock "skill-court")
