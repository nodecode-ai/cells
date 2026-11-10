;;;; package.lisp --- the cell's package, its condition, and its settings variable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as the
;;;; exported symbol of package NODECODE-JP-ADDRESS, and every contribution is
;;;; filed under that name downcased, which is this folder's name.
;;;; DEFINE-PERIPHERAL gives the package its condition JP-ADDRESS-ERROR, FAIL,
;;;; the settings variable and DEFINE-VERB, so the files between this one and
;;;; the declaration can refuse in one voice.

(defpackage #:nodecode-jp-address
  (:use #:cl)
  (:export #:address-split-json #:jp-address-error))

(in-package #:nodecode-jp-address)

(nlk:define-peripheral jp-address :not-running t)
