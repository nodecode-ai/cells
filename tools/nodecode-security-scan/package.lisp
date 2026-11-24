;;;; package.lisp --- the SECURITY-SCAN package: nine tools, one command, one condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed SECSCAN so an eval form reads (secscan:get-scan ...).
;;;; Every verb answers a STRING the model reads, and every refusal is the
;;;; cell's own SECURITY-SCAN-ERROR, rendered as "ERROR: SECURITY-SCAN-ERROR:
;;;; <detail>". The settings derive from the one `security_scan' section in
;;;; cell.lisp (NLE:DEFINE-CELL), read with (SETTING :max-chunk-lines).

(defpackage #:nodecode-security-scan
  (:use #:cl)
  (:nicknames #:secscan)
  (:export
   ;; the nine tools' bodies, and the command
   #:start-scan #:get-scan #:checkpoint #:submit-worker-result
   #:record-validations #:record-chains #:finalize-scan #:cancel-scan
   #:export-scan #:run-command
   ;; data, so a test or a layer can stand in
   #:*http*
   ;; the one condition
   #:security-scan-error))

(in-package #:nodecode-security-scan)

(nlk:define-peripheral security-scan :not-running t :lock "security-scan")
