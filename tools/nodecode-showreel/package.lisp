;;;; package.lisp --- the SHOWREEL package: the verb, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed SHOWREEL so an eval form reads
;;;; (showreel:render "..."). Every verb answers a string and refuses with
;;;; SHOWREEL-ERROR, which the eval snippet renders as
;;;; "ERROR: SHOWREEL-ERROR: <detail>".

(defpackage #:nodecode-showreel
  (:use #:cl)
  (:nicknames #:showreel)
  (:export
   ;; the verb
   #:render
   ;; data, so a layer can reword it
   #:*aspects* #:*hard-cap* #:*primer*
   ;; the one condition
   #:showreel-error))

(in-package #:nodecode-showreel)

;;; FAIL and SHOWREEL-ERROR, which a render that cannot go on signals, and
;;; DEFINE-VERB, whose functions refuse while the cell is not running, come
;;; from here. SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp,
;;; which loads last.
(nlk:define-peripheral showreel :not-running t)
