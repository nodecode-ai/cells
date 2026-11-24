;;;; package.lisp --- the NODECODE-RFC package: the tool and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it. It is meant to be used qualified only.

(defpackage #:nodecode-rfc
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:rfc-error))

(in-package #:nodecode-rfc)

;;; DEFINE-PERIPHERAL gives this package its condition RFC-ERROR, FAIL for a
;;; refusal in words, the settings variable *RFC* and RUNNING-SETTINGS.
;;; SETTING, which record.lisp reads, comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last.
(nlk:define-peripheral rfc :not-running t)
