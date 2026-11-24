;;;; package.lisp --- the AUX-LEDGER package: the verbs, the task label, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed AUX-LEDGER so an eval form reads
;;;; (aux-ledger:report "7d providers"). Every exported verb answers a
;;;; STRING, and every refusal is AUX-LEDGER-ERROR, which the eval snippet
;;;; renders as "ERROR: AUX-LEDGER-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `aux_ledger' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :file).

(defpackage #:nodecode-aux-ledger
  (:use #:cl)
  (:nicknames #:aux-ledger)
  (:export
   ;; verbs, every one a string
   #:report #:clear
   ;; the label a caller puts on the side calls it makes
   #:*task* #:with-task
   ;; the one condition
   #:aux-ledger-error))

(in-package #:nodecode-aux-ledger)

(nlk:define-peripheral aux-ledger :not-running t :lock "aux-ledger")
