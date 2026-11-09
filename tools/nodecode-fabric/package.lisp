;;;; package.lisp --- the FABRIC package: six verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FABRIC so an eval form reads (fabric:recall "make
;;;; check"), (fabric:write ...), (fabric:pending). Every exported verb answers
;;;; a STRING; a refusal is FABRIC-ERROR, rendered by the eval snippet as
;;;; "ERROR: FABRIC-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `fabric' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :fabric).

(defpackage #:nodecode-fabric
  (:use #:cl)
  (:nicknames #:fabric)
  (:export
   ;; verbs, every one a string
   #:recall #:remember #:lookup #:pending #:brief #:curate
   ;; data, so a layer can reword it
   #:*primer* #:*standing* #:*framing* #:*nothing*
   ;; the one condition
   #:fabric-error))

(in-package #:nodecode-fabric)

(nlk:define-peripheral fabric :not-running t)
