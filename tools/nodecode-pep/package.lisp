;;;; package.lisp --- the NODECODE-PEP package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed PEP so an eval form reads
;;;; (pep:reference "572"). Every failure the model sees is PEP-ERROR, whose
;;;; report is the whole detail and which the eval snippet renders as
;;;; "ERROR: PEP-ERROR: <detail>".

(defpackage #:nodecode-pep
  (:use #:cl)
  (:nicknames #:pep)
  (:export
   ;; the model-facing vocabulary (see the manual in cell.lisp)
   #:reference #:find-peps #:sections
   ;; the one condition
   #:pep-error))

(in-package #:nodecode-pep)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; html.lisp and pep.lisp read the section through it.
(nlk:define-peripheral pep :not-running t)
