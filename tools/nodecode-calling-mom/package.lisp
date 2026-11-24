;;;; package.lisp --- the cell's package, named after its system.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-calling-mom
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           ;; the risk rules, which are data a caller may read on its own
           #:classify #:critical-file
           #:+critical-commands+ #:+watch-commands+ #:+critical-files+))
