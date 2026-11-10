;;;; package.lisp --- the cell's package, its condition, and its settings variable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-ESTAT, and every contribution
;;;; is filed under that name downcased, which is this folder's name.
;;;; DEFINE-PERIPHERAL gives the package its condition ESTAT-ERROR and
;;;; FAIL, the settings variable, and DEFINE-VERB, so the files between
;;;; this one and the declaration refuse in one voice.

(defpackage #:nodecode-estat
  (:use #:cl)
  (:export #:search-tables #:table-info #:get-data #:estat-error))

(in-package #:nodecode-estat)

(nlk:define-peripheral estat :not-running t)
