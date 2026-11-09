;;;; package.lisp --- the DEBUG-DESK package: two verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DEBUG-DESK so an eval form reads
;;;; (debug-desk:today), (debug-desk:last-failure). Every exported verb
;;;; answers a STRING; every refusal is DEBUG-DESK-ERROR, which the eval
;;;; snippet renders as "ERROR: DEBUG-DESK-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `debug-desk' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :debounce-seconds).

(defpackage #:nodecode-debug-desk
  (:use #:cl)
  (:nicknames #:debug-desk)
  (:export
   ;; verbs, every one a string
   #:today #:last-failure
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:debug-desk-error))

(in-package #:nodecode-debug-desk)

(nlk:define-peripheral debug-desk :not-running t)
