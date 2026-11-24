;;;; package.lisp --- the RAM-FIRST-CONTEXT package: one recall verb, its refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed RAM-FIRST-CONTEXT so an eval form reads
;;;; (ram-first-context:recall "gate").
;;;;
;;;; A Nodecode rewrite of ether-btc/ram-first-context, see NOTICE.

(defpackage #:nodecode-ram-first-context
  (:use #:cl)
  (:nicknames #:ram-first-context)
  (:export #:recall #:matches #:available-p #:matches-text #:+manual+
           #:ram-first-context-error #:ram-first-context-error-detail))

(in-package #:nodecode-ram-first-context)

;;; RAM-FIRST-CONTEXT-ERROR, its detail reader, FAIL, *RAM-FIRST-CONTEXT*,
;;; RUNNING-SETTINGS and DEFINE-VERB: the same four every folder declares.
(nlk:define-peripheral ram-first-context :not-running t)
