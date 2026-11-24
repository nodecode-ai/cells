;;;; package.lisp --- the AFTERFORGE package: six verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed AFTERFORGE so an eval form reads
;;;; (afterforge:report "case-001"). Every exported verb answers a STRING and
;;;; every refusal is AFTERFORGE-ERROR, rendered by the eval snippet as
;;;; "ERROR: AFTERFORGE-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `afterforge' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :PROJECT).

(defpackage #:nodecode-afterforge
  (:use #:cl)
  (:nicknames #:afterforge)
  (:export
   ;; verbs, every one a string
   #:record #:cases #:review #:recipe #:verify #:report
   ;; what /afterforge runs
   #:run-slash
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:afterforge-error))

(in-package #:nodecode-afterforge)

(nlk:define-peripheral afterforge :not-running t)
