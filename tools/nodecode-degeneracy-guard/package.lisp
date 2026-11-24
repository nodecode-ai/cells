;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
(defpackage #:nodecode-degeneracy-guard
  (:use #:cl)
  (:export #:start-cell
           #:check-text
           #:check-lines
           #:judge-argument
           #:detect-structure
           #:session-notice))
