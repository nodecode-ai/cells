;;;; package.lisp --- the FLEET-POLICY package: the watch verb and its refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loads first, so CELL.LISP's NLE:DEFINE-CELL finds the package, the
;;;; condition its verbs signal, and the settings variable its reader fills.
;;;; The package is named after the system, nicknamed FLEET-POLICY for the
;;;; slash command, the manual topic and an eval form typed by hand.

(defpackage #:nodecode-fleet-policy
  (:use #:cl)
  (:nicknames #:fleet-policy)
  (:export #:fleet-news #:watch-new-items #:gate-line #:state-directory
           #:fleet-policy-error))

(in-package #:nodecode-fleet-policy)

;;; FLEET-POLICY-ERROR is what every refusal is, FAIL signals one, and
;;; *FLEET-POLICY* is the settings the cell runs on -- NIL until it starts,
;;; which is why a verb called before that refuses in words.
(nlk:define-peripheral fleet-policy :not-running t)
