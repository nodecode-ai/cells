;;;; package.lisp --- the cell's package and what a caller outside it may name.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-morning-briefing
  (:use #:cl)
  (:export #:*morning-briefing*
           #:morning-briefing-error
           #:build-briefing
           #:brief
           #:run-command
           #:page-html
           #:atom-entries))
