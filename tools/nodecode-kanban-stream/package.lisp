;;;; package.lisp --- this cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-kanban-stream
  (:nicknames #:kanban-stream)
  (:use #:cl)
  (:export #:start-cell #:stop-cell))
