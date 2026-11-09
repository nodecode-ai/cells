;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system: the loader finds START-CELL as the exported
;;;; symbol of package NODECODE-HARDLIGHT, which NLE:DEFINE-CELL defines.

(defpackage #:nodecode-hardlight
  (:use #:cl))
