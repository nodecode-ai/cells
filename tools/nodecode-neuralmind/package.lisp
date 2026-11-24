;;;; package.lisp --- the NEURALMIND package: the verbs, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed NEURALMIND so an eval form reads
;;;; (neuralmind:recall "what does the parser do") and (neuralmind:recap).
;;;; Every verb answers a STRING; every refusal is NEURALMIND-ERROR, rendered
;;;; by the eval snippet as "ERROR: NEURALMIND-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `neuralmind' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :COMMAND).

(defpackage #:nodecode-neuralmind
  (:use #:cl)
  (:nicknames #:neuralmind)
  (:export
   ;; verbs, every one a string
   #:recall #:recap
   ;; data, so a layer can reword it
   #:*primer* #:*no-project* #:*nothing*
   ;; the one condition
   #:neuralmind-error))

(in-package #:nodecode-neuralmind)

(nlk:define-peripheral neuralmind :not-running t)
