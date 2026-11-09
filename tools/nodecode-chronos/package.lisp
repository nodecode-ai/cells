;;;; package.lisp --- the NODECODE-CHRONOS package: the checkpoints, the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; CHRONOS is a nickname so an eval form reads
;;;; (chronos:...). SETTING comes from the NLE:DEFINE-CELL declaration in
;;;; cell.lisp, which loads last. FAIL and CHRONOS-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL.

(defpackage #:nodecode-chronos
  (:use #:cl)
  (:nicknames #:chronos)
  (:export #:chronos-error))

(in-package #:nodecode-chronos)

(nlk:define-peripheral chronos :not-running t)
