;;;; package.lisp --- the NODECODE-OBOL package: invoices, one ledger, one error.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, nicknamed OBOL so an eval form reads
;;;; (obol:list-invoices). The nickname is the cell's own short word: no other
;;;; cell answers to it.

(defpackage #:nodecode-obol
  (:use #:cl)
  (:nicknames #:obol)
  (:export #:create-invoice #:send-invoice #:list-invoices #:get-invoice
           #:send-reminder #:void-invoice #:mark-paid #:obol-error))

(in-package #:nodecode-obol)

;;; OBOL-ERROR is the one condition every refusal signals; FAIL signals it with
;;; a detail. NOT-RUNNING gives this package *OBOL*, RUNNING-SETTINGS -- the
;;; refusal a verb opens with while the cell is not started. SETTING comes from
;;; the NLE:DEFINE-CELL declaration in cell.lisp, which loads last.
(nlk:define-peripheral obol :not-running t)
