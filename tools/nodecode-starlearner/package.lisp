;;;; package.lisp --- the cell's package, its one condition exported.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-starlearner
  (:use #:cl)
  (:export #:starlearner-error #:starlearner-error-detail))
