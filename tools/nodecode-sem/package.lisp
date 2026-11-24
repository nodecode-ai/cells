;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, and every contribution is filed under the same name
;;;; downcased, which is this folder's name.

(defpackage #:nodecode-sem
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:sem-error
           #:sem-error-detail
           #:sem-find
           #:sem-diff
           #:sem-impact
           #:weave-preview
           #:call
           #:run-cli
           #:words))

(in-package #:nodecode-sem)
