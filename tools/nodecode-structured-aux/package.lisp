;;;; package.lisp --- the NODECODE-STRUCTURED-AUX package: the model-facing verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed STRUCTURED-AUX so an eval form reads
;;;; (structured-aux:approve "rm -rf build"). Every failure the model sees is
;;;; STRUCTURED-AUX-ERROR, which the eval snippet renders as "ERROR:
;;;; STRUCTURED-AUX-ERROR: <detail>"; the detail never carries the key.

(defpackage #:nodecode-structured-aux
  (:use #:cl)
  (:nicknames #:structured-aux)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:decide #:approve #:pick-tool #:compress
   ;; the one condition
   #:structured-aux-error))

(in-package #:nodecode-structured-aux)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; decide.lisp and compress.lisp read the section through it.
;;; The filter is what a refusal's text is cleaned of: every key this cell
;;; would send.
(nlk:define-peripheral structured-aux :not-running t :filter redact)
