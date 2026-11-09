;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package <SYSTEM-NAME>, and every contribution is
;;;; filed under that name downcased, which is this folder's name.

(defpackage #:nodecode-priors
  (:use #:cl)
  (:export #:priors-error #:priors-error-detail
           #:start-cell
           #:*session*))
