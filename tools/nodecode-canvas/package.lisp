;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, so this name and the folder's are the same.

(defpackage #:nodecode-canvas
  (:use #:cl)
  (:export #:canvas-error #:canvas-error-detail))
