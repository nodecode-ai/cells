;;;; package.lisp --- the POCKET package: three verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed POCKET so an eval form reads (pocket:notify ...).
;;;; Every verb answers a STRING and every refusal is POCKET-ERROR, rendered
;;;; by the eval snippet as "ERROR: POCKET-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `pocket' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :PROJECT).

(defpackage #:nodecode-pocket
  (:use #:cl)
  (:nicknames #:pocket)
  (:export
   ;; verbs, every one a string
   #:notify #:ask #:answer
   ;; what a test binds instead of dialling
   #:*request*
   ;; the one condition
   #:pocket-error))

(in-package #:nodecode-pocket)

(nlk:define-peripheral pocket :not-running t)
