;;;; package.lisp --- the telemetry package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TELEMETRY so an eval form reads (telemetry:today).
;;;; The settings this cell runs on are not declared here: they derive from
;;;; the one `telemetry' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :mode), and *TELEMETRY* below is the variable they
;;;; land in.

(defpackage #:nodecode-telemetry
  (:use #:cl)
  (:nicknames #:telemetry)
  (:export
   ;; what a test or an eval form drives
   #:ledger-path #:rows #:read-ledger #:reset-ledger #:money
   #:breach #:breach-text #:spent-today #:spent-this-month
   #:record-frame #:observe-frame #:observe-tool #:observe-round
   #:run-stats #:run-budget #:report-spend #:stats-text #:budget-line
   #:mode #:enforcing-p))

(in-package #:nodecode-telemetry)

(nlk:define-peripheral telemetry :not-running t :lock "telemetry")
