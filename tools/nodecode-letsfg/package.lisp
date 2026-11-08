;;;; package.lisp --- the cell's package: model-facing vocabulary, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed LETSFG so an EVAL form reads (letsfg:call-tool ...).
;;;; The name is the system's, which is how the folder loader finds START-CELL.
;;;; Every verb answers a STRING -- the eval snippet prints the value with ~S,
;;;; so a string reads as text -- and every refusal is LETSFG-ERROR, which the
;;;; eval snippet renders as "ERROR: LETSFG-ERROR: <detail>".

(defpackage #:nodecode-letsfg
  (:use #:cl)
  (:nicknames #:letsfg)
  (:export #:letsfg-error #:call-tool #:tools #:search-flights #:search-hotels))
