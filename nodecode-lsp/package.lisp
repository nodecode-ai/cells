;;;; package.lisp --- the LSP package: the model's verbs and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed LSP so an eval form reads
;;;; (lsp:definition "src/a.rs" "parse"). It is meant to be used qualified
;;;; only: RESTART shadows its CL namesake (the model's verb).
;;;;
;;;; Every exported verb returns a STRING, and every refusal is LSP-ERROR,
;;;; which the eval snippet renders as "ERROR: LSP-ERROR: <detail>".

(defpackage #:nodecode-lsp
  (:use #:cl)
  (:nicknames #:lsp)
  (:shadow #:restart)
  (:export
   ;; model-facing vocabulary (see the primer in cell.lisp)
   #:diagnostics #:definition #:references #:hover #:symbols #:rename
   #:status #:restart #:request
   ;; the one condition
   #:lsp-error))

(in-package #:nodecode-lsp)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the files between read the section through it.
(nlk:define-peripheral lsp :not-running t)
