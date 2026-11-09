;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system: the loader finds START-CELL as the exported
;;;; symbol of package <SYSTEM-NAME>, and every contribution is filed under
;;;; the name downcased, which is this folder's name.

(defpackage #:nodecode-refusal
  (:use #:cl))
