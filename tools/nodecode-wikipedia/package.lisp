;;;; package.lisp --- the cell's package, its condition, and its settings variable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-WIKIPEDIA, and every
;;;; contribution is filed under that name downcased, which is this folder's
;;;; name. DEFINE-PERIPHERAL gives the package its condition
;;;; WIKIPEDIA-ERROR, FAIL, the settings variable and DEFINE-VERB, so the
;;;; files between this one and the declaration can refuse in one voice.

(defpackage #:nodecode-wikipedia
  (:use #:cl)
  (:export #:article #:wikipedia-error))

(in-package #:nodecode-wikipedia)

(nlk:define-peripheral wikipedia :not-running t)
