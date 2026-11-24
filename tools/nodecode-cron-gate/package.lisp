;;;; package.lisp --- the CRON-GATE package: four verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CRON-GATE so an eval form reads
;;;; (cron-gate:check "heartbeat"). Every verb answers a string and every
;;;; refusal is CRON-GATE-ERROR, rendered by the eval snippet as
;;;; "ERROR: CRON-GATE-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `cron-gate' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :model).

(defpackage #:nodecode-cron-gate
  (:use #:cl)
  (:nicknames #:cron-gate)
  (:export
   ;; verbs, every one a string
   #:gate #:check #:gates #:forget
   ;; the one condition
   #:cron-gate-error))

(in-package #:nodecode-cron-gate)

(nlk:define-peripheral cron-gate :not-running t :lock "cron-gate")
