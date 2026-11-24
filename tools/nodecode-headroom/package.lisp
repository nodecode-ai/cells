;;;; package.lisp --- the cell's package, named by its folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is nodecode-headroom; HEADROOM is its one nickname, and the
;;;; slash command and the manual topic carry the same word, so a second cell
;;;; with the same name cannot load beside it.

(defpackage #:nodecode-headroom
  (:use #:cl)
  (:nicknames #:headroom)
  (:export #:start-cell #:stop-cell
           #:classify-command #:lighten #:snapshot #:status-text #:explain-text
           #:memory #:read-memory #:headroom-error))
