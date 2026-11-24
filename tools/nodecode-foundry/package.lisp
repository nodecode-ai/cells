;;;; package.lisp --- the cell's package, named by its folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is nodecode-foundry; FOUNDRY is its one nickname, and the
;;;; slash command and the manual topic carry the same word, so a second
;;;; cell with the same name cannot load beside it.

(defpackage #:nodecode-foundry
  (:use #:cl)
  (:nicknames #:foundry)
  (:export #:start-cell #:stop-cell
           #:spec-for #:build #:verify #:listing #:catalog-text
           #:run-command #:root
           #:foundry-error))
