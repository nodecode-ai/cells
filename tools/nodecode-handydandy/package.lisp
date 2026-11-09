;;;; package.lisp --- the HANDYDDANDY package: the condition, the verbs, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed HANDYDDANDY so an eval form reads
;;;; (handydandy:status). Every exported verb answers a STRING and every
;;;; refusal is HANDYDDANDY-ERROR, rendered by the eval snippet as
;;;; "ERROR: HANDYDDANDY-ERROR: <detail>".
;;;;
;;;; The settings the cell runs on derive from the one `handydandy' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING
;;;; :MAX-INPUT-TOKENS).

(defpackage #:nodecode-handydandy
  (:use #:cl)
  (:nicknames #:handydandy)
  (:export
   ;; the one verb
   #:status
   ;; data, so a layer can reword what a long session is told
   #:*notice* #:*instruction* #:*saved*
   ;; the one condition
   #:handydandy-error))

(in-package #:nodecode-handydandy)

(nlk:define-peripheral handydandy :not-running t :lock "handydandy" :recursive t)
