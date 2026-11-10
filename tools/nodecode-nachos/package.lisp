;;;; package.lisp --- the NODECODE-NACHOS package: the store, the manifest and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed NACHOS so the manual and an eval form read
;;;; (nachos:...).

(defpackage #:nodecode-nachos
  (:use #:cl)
  (:nicknames #:nachos)
  (:export #:start-cell
           #:stop-cell
           #:running-settings
           #:nachos-error #:nachos-error-detail))

(in-package #:nodecode-nachos)

;;; NACHOS-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral nachos :not-running t)
