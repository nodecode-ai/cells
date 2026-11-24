;;;; package.lisp --- the CHMOD-REF package: the one condition, the settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the folder, so the loader finds START-CELL and
;;;; every contribution is filed under "nodecode-chmod-ref". The verbs
;;;; (EXPLAIN, SUMMARISE, PREVIEW) live in CELL.LISP; the bits and the words
;;;; in MODES.LISP, which imports nothing of Nodecode's.

(defpackage #:nodecode-chmod-ref
  (:use #:cl)
  (:export
   ;; the verbs, every one a string, or a list of them
   #:explain #:summarise #:preview
   ;; the cell's refusal (nothing running) and the engine's (not a mode)
   #:chmod-ref-error
   #:mode-error #:mode-error-message))

(in-package #:nodecode-chmod-ref)

(nlk:define-peripheral chmod-ref :not-running t)
