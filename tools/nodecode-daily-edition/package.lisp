;;;; package.lisp --- the package, its one condition, its settings variable.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-daily-edition
  (:use #:cl)
  (:nicknames #:daily-edition)
  (:export #:start-cell #:daily-edition-error))

(in-package #:nodecode-daily-edition)

(nlk:define-peripheral daily-edition :not-running t)
