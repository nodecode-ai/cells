;;;; package.lisp --- the cell's package, named after its system.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-advisor
  (:use #:cl)
  (:export #:start-cell #:stop-cell))
