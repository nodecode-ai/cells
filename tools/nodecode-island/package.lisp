;;;; package.lisp --- the ISLAND package: the two verbs and the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed ISLAND so a kept definition reads (island:call ...).
;;;; Every exported verb answers a STRING and every refusal is ISLAND-ERROR.
;;;;
;;;; The settings this cell runs on derive from the one `island' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :world-url).

(defpackage #:nodecode-island
  (:use #:cl)
  (:nicknames #:island)
  (:export
   ;; verbs, every one a string
   #:call #:tools
   ;; the playbook, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:island-error))

(in-package #:nodecode-island)

(nlk:define-peripheral island :not-running t)
