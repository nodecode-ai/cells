;;;; package.lisp --- the NODECODE-SKILL-ROUTER package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. The router's pieces are internal: the model and
;;;; the operator reach this cell through (help :skill-router) and the
;;;; /skill-router command, never through a verb of its own.

(defpackage #:nodecode-skill-router
  (:use #:cl)
  (:export #:route-request))

(in-package #:nodecode-skill-router)

(nlk:define-peripheral skill-router :not-running t)
