;;;; package.lisp --- the fluent package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-fluent
  (:nicknames #:fluent)
  (:use #:cl)
  (:export #:adopt #:cards #:check-command #:check-segment #:forget-program
           #:parse-help #:read-cards #:run-slash #:split-commands
           #:start-cell #:stop-cell #:forget-all))
