;;;; package.lisp --- the cell's package, its condition, and its settings variable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-KOKKAI, and every contribution
;;;; is filed under that name downcased, which is this folder's name.
;;;; DEFINE-PERIPHERAL gives the package its condition KOKKAI-ERROR, FAIL,
;;;; the settings variable *KOKKAI* and RUNNING-SETTINGS, so the files
;;;; between this one and the declaration can refuse in one voice.

(defpackage #:nodecode-kokkai
  (:use #:cl)
  (:export #:meetings #:speeches #:watch #:kokkai-error))

(in-package #:nodecode-kokkai)

(nlk:define-peripheral kokkai :not-running t)
