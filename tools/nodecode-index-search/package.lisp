;;;; package.lisp --- the cell's package, its condition, and its settings variable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-INDEX-SEARCH, and every
;;;; contribution is filed under that name downcased, which is this folder's
;;;; name. DEFINE-PERIPHERAL gives the package its condition
;;;; INDEX-SEARCH-ERROR and FAIL, and the settings variable *INDEX-SEARCH*,
;;;; so the files between this one and the declaration refuse in one voice.

(defpackage #:nodecode-index-search
  (:use #:cl)
  (:export #:index-search-error))

(in-package #:nodecode-index-search)

(nlk:define-peripheral index-search :not-running t)
