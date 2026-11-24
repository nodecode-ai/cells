;;;; package.lisp --- the DEJA package: three verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DEJA so an eval form reads (deja:recall "..."),
;;;; (deja:fix "..."), (deja:blame "src/parser.lisp"). Every exported verb
;;;; answers a STRING and every refusal is DEJA-ERROR, rendered by the eval
;;;; snippet as "ERROR: DEJA-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `deja' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :deja).

(defpackage #:nodecode-deja
  (:use #:cl)
  (:nicknames #:deja)
  (:export
   ;; verbs, every one a string
   #:recall #:fix #:blame
   ;; data, so a layer can reword it
   #:*primer* #:*standing* #:*framing*
   ;; the one condition
   #:deja-error))

(in-package #:nodecode-deja)

(nlk:define-peripheral deja :not-running t)
