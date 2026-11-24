;;;; package.lisp --- this cell's one package, and the short name the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-flow
  (:nicknames #:flow)
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:sessions-text #:tree-text #:baseline #:compare-baseline #:report))

(in-package #:nodecode-flow)

(nlk:define-peripheral flow :not-running t)
