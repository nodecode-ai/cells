;;;; package.lisp --- the GBRAIN package: one verb, the primer, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed GBRAIN so an eval form reads (gbrain:recall ...).
;;;; RECALL answers a STRING and every refusal is GBRAIN-ERROR, rendered by
;;;; the eval snippet as "ERROR: GBRAIN-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `gbrain' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :url).

(defpackage #:nodecode-gbrain
  (:use #:cl)
  (:nicknames #:gbrain)
  (:export
   ;; the verb, a string
   #:recall
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:gbrain-error))

(in-package #:nodecode-gbrain)

(nlk:define-peripheral gbrain :not-running t)
