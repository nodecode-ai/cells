;;;; package.lisp --- the POLYMARKET package: the verbs, the condition, the lock.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed POLYMARKET so an eval form reads
;;;; (polymarket:movers). Every verb answers a string; every refusal is
;;;; POLYMARKET-ERROR, rendered by the eval snippet as "ERROR: POLYMARKET-ERROR:
;;;; <detail>". The settings the cell runs on derive from the one `polymarket'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :...).

(defpackage #:nodecode-polymarket
  (:use #:cl)
  (:nicknames #:polymarket)
  (:shadow #:search)
  (:export
   ;; verbs, every one a string
   #:scan #:search #:market #:signals #:watch #:brief
   ;; data, so a layer can reword it
   #:*disclaimer* #:*primer*
   ;; the one condition
   #:polymarket-error))

(in-package #:nodecode-polymarket)

(nlk:define-peripheral polymarket :not-running t :lock "polymarket")
