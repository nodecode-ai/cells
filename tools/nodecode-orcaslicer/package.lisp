;;;; package.lisp --- the ORCASLICER package: the tools, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed ORCA so an eval form reads (orca:diagnose). Every
;;;; exported verb answers the text a model reads; every refusal is
;;;; ORCASLICER-ERROR, whose detail the tool answer carries.
;;;;
;;;; The settings this cell runs on derive from the one `orcaslicer' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :slicer).

(defpackage #:nodecode-orcaslicer
  (:use #:cl)
  (:nicknames #:orca)
  (:export
   ;; verbs, every one the text a tool answers
   #:diagnose #:search-presets #:slice #:job #:export-artifact
   #:printer-status #:upload
   ;; data, so a layer can reword it
   #:*manual* #:*changes-bounds*
   ;; the one condition
   #:orcaslicer-error))
