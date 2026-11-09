;;;; package.lisp --- the NODECODE-UNDERSTUDY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-understudy
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:audition #:latest-report #:run-ids
           #:running-settings
           #:understudy-error #:understudy-error-detail))

(in-package #:nodecode-understudy)

;;; UNDERSTUDY-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral understudy :not-running t)
