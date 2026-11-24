;;;; package.lisp --- the NODECODE-YOU package: the model's verbs and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed YOU so an eval form reads
;;;; (you:search "..."). It is meant to be used qualified only: SEARCH
;;;; shadows its CL namesake (the model's verb). Inside this package, string
;;;; searching spells CL:SEARCH.

(defpackage #:nodecode-you
  (:use #:cl)
  (:nicknames #:you)
  (:shadow #:search)
  (:export #:search #:contents #:research #:finance #:you-error))

(in-package #:nodecode-you)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; mcp.lisp and tools.lisp read the section through it. FAIL and
;;; YOU-ERROR, which a call that cannot go on signals, come from here too.
(nlk:define-peripheral you :not-running t)
