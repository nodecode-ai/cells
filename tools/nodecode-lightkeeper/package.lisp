;;;; package.lisp --- the LIGHTKEEPER package: three verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed KEEPER so an eval form reads (keeper:find-notes "books").
;;;; Every verb answers a STRING; every refusal is LIGHTKEEPER-ERROR, which the
;;;; tool wrapper shows the model as "ERROR: LIGHTKEEPER-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `lightkeeper' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :vault).

(defpackage #:nodecode-lightkeeper
  (:use #:cl)
  (:nicknames #:keeper)
  (:export
   ;; verbs, every one a string
   #:find-notes #:read-note #:prime
   ;; data, so a layer can reword it
   #:+primer+
   ;; the one condition
   #:lightkeeper-error))

(in-package #:nodecode-lightkeeper)

(nlk:define-peripheral lightkeeper :not-running t)
