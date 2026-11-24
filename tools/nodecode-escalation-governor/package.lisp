;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
(defpackage #:nodecode-escalation-governor
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:packet-text
           #:trip-reason))
