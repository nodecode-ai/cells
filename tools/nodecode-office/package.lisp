;;;; package.lisp --- the OFFICE package: five tools, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed OFFICE so an eval form reads
;;;; (office:inspect-file ...). The five verbs are tools the model calls; a
;;;; person can call the same functions from eval. Every one answers a
;;;; string, and a refusal is OFFICE-ERROR, which the tool wrapper renders.

(defpackage #:nodecode-office
  (:use #:cl)
  (:nicknames #:office)
  (:export #:start-cell
           #:stop-cell
           #:inspect-file #:search-file #:replace-text #:tracked-edit-file #:receipts
           #:office-error #:office-error-detail))

(in-package #:nodecode-office)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and OFFICE-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of them, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral office :not-running t)
