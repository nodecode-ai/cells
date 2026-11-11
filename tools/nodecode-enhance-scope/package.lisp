;;;; package.lisp --- the NODECODE-ENHANCE-SCOPE package: three verbs and the settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ENHANCE-SCOPE so an eval form reads
;;;; (enhance-scope:check-text). The condition, FAIL, the settings variable and
;;;; SETTING come from the NLK:DEFINE-PERIPHERAL and NLE:DEFINE-CELL
;;;; declarations in cell.lisp.

(defpackage #:nodecode-enhance-scope
  (:use #:cl)
  (:nicknames #:enhance-scope)
  (:export
   ;; what /enhance-scope answers, and the transform the tests read directly
   #:status-text #:check-text #:content-text #:identity-text #:rewrite-identity
   #:+default-content+ #:+content-limit+
   ;; the one condition
   #:enhance-scope-error))

(in-package #:nodecode-enhance-scope)

(nlk:define-peripheral enhance-scope :not-running t)
