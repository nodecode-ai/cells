;;;; package.lisp --- the cell's package, its condition and its settings variable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-MEME-STUDIO, and every
;;;; contribution is filed under that name downcased, which is this folder's
;;;; name. DEFINE-PERIPHERAL gives the package its condition
;;;; MEME-STUDIO-ERROR, FAIL, the settings variable and SETTING, so the files
;;;; between this one and the declaration refuse in one voice.

(defpackage #:nodecode-meme-studio
  (:use #:cl)
  (:export #:suggest-templates #:search-templates #:get-template
           #:popular-templates #:create-meme #:meme-studio-error))

(in-package #:nodecode-meme-studio)

(nlk:define-peripheral meme-studio :not-running t)
