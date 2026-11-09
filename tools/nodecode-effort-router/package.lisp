;;;; package.lisp --- the NODECODE-EFFORT-ROUTER package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the folder: the loader finds START-CELL in it,
;;;; and every contribution is filed under "nodecode-effort-router".
;;;;
;;;; NLE:DEFINE-CELL in cell.lisp declares the section, the tools, the
;;;; command, the manual topic, the advice and the start; the state the
;;;; advice and the tools both read is declared here, ahead of every use.

(defpackage #:nodecode-effort-router
  (:use #:cl)
  (:export
   ;; what a reader of the cell's own record asks it
   #:decide
   #:route-answer
   #:status-text))
