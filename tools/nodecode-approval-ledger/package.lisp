;;;; package.lisp --- the APPROVAL-LEDGER package: the verbs and the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed APPROVAL-LEDGER so an eval form reads
;;;; (approval-ledger:report). Every verb answers a string, a list of rows or
;;;; a count, and every refusal is APPROVAL-LEDGER-ERROR, rendered as
;;;; "ERROR: APPROVAL-LEDGER-ERROR: <detail>".
;;;;
;;;; The settings the cell runs on derive from the one `approval-ledger'
;;;; section in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :KEEP-ROWS).

(in-package #:cl-user)

(defpackage #:nodecode-approval-ledger
  (:use #:cl)
  (:nicknames #:approval-ledger)
  (:export
   ;; verbs
   #:record #:report #:recent #:rows #:clear
   ;; data, so a layer can reword it
   #:+manual+
   ;; the one condition
   #:approval-ledger-error))

(in-package #:nodecode-approval-ledger)

;; The condition, FAIL, *APPROVAL-LEDGER*, RUNNING-SETTINGS and DEFINE-VERB:
;; a verb called with nothing started refuses in words a model can act on.
(nlk:define-peripheral approval-ledger :not-running t)
