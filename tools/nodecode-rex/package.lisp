;;;; package.lisp --- the REX package: four verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed REX so an eval form reads (rex:panes). Every
;;;; exported verb answers a STRING and every refusal is REX-ERROR, rendered
;;;; by the eval snippet as "ERROR: REX-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `rex' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :binary)
;;;; and its siblings. NLK:DEFINE-PERIPHERAL also gives this package its
;;;; condition, FAIL and the not-running refusal.

(defpackage #:nodecode-rex
  (:use #:cl)
  (:nicknames #:rex)
  (:export
   ;; verbs, every one a string
   #:run #:capture #:panes #:label
   ;; data, so a layer can reword it
   #:*primer* #:*marks*
   ;; the one condition
   #:rex-error))

(in-package #:nodecode-rex)

(nlk:define-peripheral rex :not-running t)
