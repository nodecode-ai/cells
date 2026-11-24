;;;; package.lisp --- the ON-BRAND package: the linter, the store, the surface.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-ON-BRAND, and every
;;;; contribution is filed under that name downcased, which is this folder's
;;;; name. There is no nickname: the package name, the slash command and the
;;;; manual topic are the cell's own word, and no other cell may take it.

(defpackage #:nodecode-on-brand
  (:use #:cl)
  (:export
   ;; everything the model sees, callable through eval
   #:on-brand-error
   #:check #:rules #:learn #:voiceover
   #:use-brand #:status #:profiles #:active-brand
   ;; data, so a layer can reword it
   #:+primer+))

(in-package #:nodecode-on-brand)
