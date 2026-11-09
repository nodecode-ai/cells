;;;; package.lisp --- the cell's package and what the tests read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package <SYSTEM-NAME>, and every contribution is
;;;; filed under that name downcased, which is this folder's name.

(defpackage #:nodecode-lcm-x
  (:use #:cl)
  (:documentation
   "LCM-X: a lossless store of every turn, searched by lcm_search and read
back verbatim by lcm_expand. A Nodecode rewrite of electricsheephq/lcm-x.")
  (:export #:lcm-x-error
           ;; the store, as the tests read it back
           #:store-directory #:session-label #:read-blocks))
