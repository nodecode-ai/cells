;;;; package.lisp --- the package, its one condition, (magi:...) from eval.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-magi
  (:use #:cl)
  (:export #:start-cell
           #:list-pending
           #:show-record
           #:stage
           #:decide
           #:entries
           #:add-entry
           #:store-path
           #:summary-line))
