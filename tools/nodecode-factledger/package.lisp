;;;; package.lisp --- the FACTLEDGER package: the verbs, the words, the one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FACTLEDGER so an eval form reads (factledger:add-fact ...).
;;;; Every exported verb answers a value and every refusal is FACTLEDGER-ERROR,
;;;; rendered by the eval snippet as "ERROR: FACTLEDGER-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `factledger' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :path).

(defpackage #:nodecode-factledger
  (:use #:cl)
  (:nicknames #:factledger)
  (:export
   ;; the one condition
   #:factledger-error #:factledger-error-detail
   ;; the verbs, the ledger's own
   #:add-fact #:active-facts #:search-facts #:fact-history
   #:compile-view #:stale-facts #:export-facts #:check-ledger #:ledger-stats
   #:ledger-file
   ;; data, so a layer can reword it
   #:+primer+ #:+fields+ #:+confidences+))

(in-package #:nodecode-factledger)

(nlk:define-peripheral factledger :not-running t :lock "factledger")
