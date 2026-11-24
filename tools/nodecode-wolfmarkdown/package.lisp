;;;; package.lisp --- the package the pure half lives in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package: the checks and the formatter, every one a function over
;;;; strings, so the tests read them without a cell running; then the tools,
;;;; the command and the declaration in cell.lisp. A refusal in this folder
;;;; is WOLFMARKDOWN-ERROR, from NLK:DEFINE-PERIPHERAL.

(defpackage #:nodecode-wolfmarkdown
  (:use #:cl)
  (:export
   ;; the pure half: text in, text out
   #:verify-text #:clean-text #:inventory-text
   #:token-index #:compare-tokens
   ;; the tools and the command
   #:run-verify #:run-clean #:run-outline #:run-slash
   #:start-cell #:stop-cell))

(in-package #:nodecode-wolfmarkdown)

(nlk:define-peripheral wolfmarkdown :not-running t)
