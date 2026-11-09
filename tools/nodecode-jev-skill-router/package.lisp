;;;; package.lisp --- the NODECODE-JEV-SKILL-ROUTER package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. The routing pieces are internal: the model
;;;; reaches this cell through the skill_select tool, the operator through
;;;; /jev-skill-router and (help :jev-skill-router).

(defpackage #:nodecode-jev-skill-router
  (:use #:cl)
  (:nicknames #:jev-skill-router))

(in-package #:nodecode-jev-skill-router)

(nlk:define-peripheral jev-skill-router :not-running t)
