;;;; package.lisp --- the CASH-DESK package: the ledger's verbs, its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CASHDESK, so an eval form reads
;;;; (cashdesk:status) or (cashdesk:import-file "invoices" "…/invoices.csv").
;;;; Every exported verb answers a STRING and every refusal is
;;;; CASH-DESK-ERROR, rendered by the eval snippet as
;;;; "ERROR: CASH-DESK-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `cash-desk' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :sender-name).

(defpackage #:nodecode-cash-desk
  (:use #:cl)
  (:nicknames #:cashdesk)
  (:export
   ;; reading the ledger
   #:status #:brief #:queue #:ledger
   ;; bringing data in
   #:import-file #:match-payments
   ;; writing to customers
   #:draft-reminders #:revise #:record-reply #:hold #:release #:approve #:send-draft
   ;; data, so a layer can reword it
   #:+stages+ #:+primer+
   ;; the one condition
   #:cash-desk-error))

(in-package #:nodecode-cash-desk)

(nlk:define-peripheral cash-desk :not-running t :lock "cash-desk" :recursive t)
