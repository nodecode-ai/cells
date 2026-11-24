;;;; package.lisp --- the cell's package, named after its system.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-verifier
  (:use #:cl)
  (:export #:start-cell #:stop-cell))
