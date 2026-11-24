;;;; package.lisp --- the KAME-ROTATE package: the verbs and the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed KAME so an eval form reads (kame:status). Every
;;;; refusal is KAME-ROTATE-ERROR, which the dispatch seam renders as
;;;; "ERROR: ...". The settings it runs on derive from the one `kame'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :base-url).

(defpackage #:nodecode-kame-rotate
  (:use #:cl)
  (:nicknames #:kame)
  (:export
   ;; verbs, every one a string
   #:status #:reset-pool
   ;; data, so a layer can reword it
   #:+primer+
   ;; the one condition
   #:kame-rotate-error))

(in-package #:nodecode-kame-rotate)

(nlk:define-peripheral kame-rotate :not-running t)
